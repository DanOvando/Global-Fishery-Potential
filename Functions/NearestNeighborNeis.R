
#' NearestNeighborNEIs
#'
#' \code(NearestNeighborNeis) assigns status to NEI fisheries based on nearest neighbors
#' @param BiomassData 
#' @param MsyData 
#' @param ProjData 
#' @param BaselineYear 
#' @param ResultFolder 
#' @param Spec_ISSCAAP 
#' @param CatchSharePrice 
#' @param CatchShareCost 
#' @param NumCPUs 
#' @param beta 
#'
#' @return
#' @export
NearestNeighborNeis <-
  function(BiomassData,
           MsyData,
           ProjData,
           BaselineYear,
           ResultFolder,
           Spec_ISSCAAP,
           CatchSharePrice,
           CatchShareCost,
           NumCPUs = 1,
           beta = 1.3)
  {
    
    ######################################################
    ##
    ## Function to, for each NEI stock, find comparable species level
    ## stocks and apply the median values of interest to the NEI stock
    ##
    ######################################################
    
    #Pull out NEI fisheries
    NEIs <-
      MsyData[MsyData$Dbase != 'RAM' &
                MsyData$RanCatchMSY == F &
                ((grepl("nei", MsyData$CommName, ignore.case = T)) |
                   (
                     grepl("nei", MsyData$CommName, ignore.case = T) &
                       (is.infinite(MsyData$BvBmsy) == T |
                          MsyData$BvBmsy == 999)
                   ) |
                   (
                     grepl("spp", MsyData$SciName) &
                       grepl("not identified", MsyData$SpeciesCatName) &
                       MsyData$Dbase == "FAO"
                   )), ]
    FinalYear <- NEIs %>%
      group_by(IdOrig) %>%
      summarize(MaxYear = max(Year, na.rm = T))
    
    FinalProjYear <- ProjData %>%
      group_by(IdOrig) %>%
      summarize(MaxYear = max(Year, na.rm = T))
    
    DropItProj <-
      FinalProjYear$IdOrig[FinalProjYear$MaxYear < BaselineYear]
    
    DropIt <- FinalYear$IdOrig[FinalYear$MaxYear < (BaselineYear)]
    
    # Prepare NEI data for nearest neighbot analysis --------------------------
    NEIs <- NEIs[(NEIs$IdOrig %in% DropIt) == F, ]
    
    NEIs$MarginalCost <- NA
    
    NEIs$Policy <- NA
    
    NEIs$Profits = NA
    
    HistoricNEIs <- NEIs
    
    HistoricNEIs$Policy <- 'Historic'
    
    OrigBaselineYear <- BaselineYear
    
    BaselineYear <- max(ProjData$Year, na.rm = T)
    
    Stocks <- (unique(NEIs$IdOrig))
    
    ExtendFAO <- T
    #   Rprof()
    
    FutureNEIs <- RepMat(subset(NEIs, Year == 2012), 2050 - 2012)
    
    YearVec <-
      sapply(2013:2050, function(x, reps)
        rep(x, reps), reps = length(Stocks)) %>% as.data.frame() %>% tidyr::gather()
    
    FutureNEIs$Year <- (YearVec$value)
    
    BaselineYear <- OrigBaselineYear
    
    Pols <- unique(ProjData$Policy)
    
    LongPols <- Pols
    
    Pols <- Pols[Pols != 'Historic']
    
    foo <- function(policy, x)
    {
      x$Policy <- policy
      return(x)
    }
    
    
    LongNeis <- lapply(Pols, foo, x = FutureNEIs) %>% bind_rows()
    
    NEIs <- rbind(HistoricNEIs, LongNeis)
    
    NEIs <- NEIs[order(NEIs$IdOrig, NEIs$Policy, NEIs$Year), ]
    
    SpeciesLevel <-
      ProjData[!(ProjData$IdOrig %in% unique(NEIs$IdOrig)) &
                 !(ProjData$IdOrig %in% DropItProj), ]
    
    allstocks <- SpeciesLevel %>%
      group_by(Year, Policy) %>%
      summarize(
        BvBmsy25 = quantile(BvBmsy, c(0.25), na.rm = T),
        FvFmsy75 = quantile(FvFmsy, c(0.75), na.rm = T),
        MedianR = median(g, na.rm = T),
        MedianK = median(k, na.rm = T),
        MedianPrice = median(Price, na.rm = T),
        MedianCost = median(MarginalCost, na.rm = T),
        JStocks = length(unique(IdOrig))
      )
    
    # Find comparison stocks --------------------------
    
    VarsToFill <- c("BvBmsy", "FvFmsy", "g", "k", "Price", "MarginalCost")
    
    NeiStats <-
      unique(NEIs[c("CommName", "SciName", "SpeciesCatName")]) # find unique combinations of nei stocks
    
    NeiStats$TaxonLevel <- NA
    
    NeiStats$TaxonLevel[grepl("spp", NeiStats$SciName) == T] <-
      "Genus"
    
    NeiStats$TaxonLevel[grepl("spp", NeiStats$SciName) == F] <-
      "Non-Genus"
    
    nei_stock <- unique(NeiStats$SciName)
    
    
    if (Sys.info()[1] != 'Windows')
    {
      tempNEIs <-
        (
          lapply(
            1:length(nei_stock),
            SnowNEIs2,
            nei_stock = nei_stock,
            NEIs = NEIs,
            SpeciesLevel = SpeciesLevel,
            NeiStats = NeiStats,
            Spec_ISSCAAP = Spec_ISSCAAP,
            VarsToFill = VarsToFill
          )
        )
    }
    if (Sys.info()[1] == 'Windows')
    {
      sfInit(parallel = TRUE, cpus = NumCPUs)
      
      sfExportAll()
      sfLibrary(dplyr)
      sfLibrary(stringr)
      
      tempNEIs <-
        sfClusterApplyLB(
          1:length(nei_stock),
          SnowNEIs2,
          nei_stock = nei_stock,
          NEIs = NEIs,
          SpeciesLevel = SpeciesLevel,
          NeiStats = NeiStats,
          Spec_ISSCAAP = Spec_ISSCAAP,
          VarsToFill = VarsToFill
        )
      sfStop()
      
    }
    show('Completed NEI Stats mclapply')
    
    NEIs <- bind_rows(tempNEIs, data.frame)
    
    show("Completed NEI ldply")
    
    NEIs <- NEIs %>%
      ungroup() %>%
      group_by(IdOrig) %>%
      mutate(NewMSY = (Catch / (BvBmsy * FvFmsy))[Policy == 'Historic' &
                                                    Year == 2012])
    
    NEIs$MSY <- NEIs$NewMSY
    
    NEIs$NewMSY <- NULL
    
    Where <- NEIs$Year > 2012
    
    NEIs$Catch[Where] <- (NEIs$MSY * (NEIs$BvBmsy * NEIs$FvFmsy))[Where]
    
    BOA <- NEIs$BvBmsyOpenAccess
    
    phi <- NEIs$phi
    
    FOA <- (((phi + 1) / phi) * (1 - BOA ^ phi / (phi + 1)))
    
    c_num <-  NEIs$Price * FOA * BOA * NEIs$MSY
    c_den = (NEIs$g * FOA) ^ beta
    
    cost = c_num / c_den
    
    cost <- cost
    
    NEIs$MarginalCost <- cost
    
    NEIs$MarginalCost[NEIs$Policy == 'CatchShare'] <-
      (NEIs$MarginalCost * CatchShareCost)[NEIs$Policy == 'CatchShare']
    
    NEIs$Bmsy <- NEIs$MSY / NEIs$g
    
    NEIs$Biomass <- NEIs$BvBmsy * NEIs$Bmsy
    
    NEIs$Profits <-
      NEIs$Price * NEIs$MSY * (NEIs$BvBmsy * NEIs$FvFmsy) - NEIs$MarginalCost *
      (NEIs$FvFmsy * NEIs$g) ^ beta
    
    Biomass <-
      NEIs[NEIs$Policy == 'Historic', colnames(NEIs) %in% colnames(BiomassData)]
    
    Biomass$BvBmsy <- log(Biomass$BvBmsy)
    
    return(list(ProjNeis = NEIs, BiomassNeis = Biomass))
  } # close function
