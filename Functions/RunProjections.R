######################################
#Run Projections --------------------------------------------------
# This code projects fished populations forward under different policy scenarios
######################################

RunProjection <-
  function(Data,
           BaselineYear,
           NumCPUs,
           StatusQuoPolicy,
           Policies = c(
             'StatusQuoOpenAccess',
             'Opt',
             'CatchShare',
             'StatusQuoFForever',
             'StatusQuoBForever',
             'Fmsy',
             'CloseDown'
           ),
           Discount = 0,
           CatchShareCost = 0.77,
           CatchSharePrice = 1.31)
  {
    Data$MarginalCost <- NA

    where_catch_share <- Data$CatchShare == 1 & Data$Year == 2012

    Data$Price[where_catch_share] <-
      Data$Price[where_catch_share] * CatchSharePrice

    Data$BvBmsy <-
      pmin(1 / Data$BtoKRatio, Data$BvBmsy) #Note capping projection data now

    Stocks <- unique(Data[Data$Year == BaselineYear, IdVar])

    Data <- Data[Data$Year <= BaselineYear, ]

    TempStockMatrix <-
      as.data.frame(matrix(NA, nrow = 0, ncol = dim(Data)[2] + 2))


    colnames(TempStockMatrix) <- c(colnames(Data), 'Policy', 'Profits')


    if (NumCPUs > 1)
    {
      if (Sys.info()[1] != 'Windows') {
        Projections <-
          (
            mclapply(
              1:(length(Stocks)),
              SnowProjections,
              mc.cores = NumCPUs,
              Data = Data,
              BaselineYear = BaselineYear,
              Stocks = Stocks,
              IdVar = IdVar,
              bvec = bvec,
              Discount = Discount,
              tol = tol,
              beta = beta,
              CatchSharePrice = CatchSharePrice,
              CatchShareCost = CatchShareCost,
              Policies = Policies,
              ProjectionTime = ProjectionTime,
              TempStockMatrix = TempStockMatrix,
              StatusQuoPolicy = StatusQuoPolicy,
              mc.cleanup = T
            )
          )
      }
      if (Sys.info()[1] == 'Windows')
      {
        sfInit(parallel = TRUE, cpus = NumCPUs)

        sfExportAll()
        sfLibrary(dplyr)

        Projections <-
          sfClusterApplyLB(
            1:(length(Stocks)),
            SnowProjections,
            Data = Data,
            BaselineYear = BaselineYear,
            Stocks = Stocks,
            IdVar = IdVar,
            bvec = bvec,
            Discount = Discount,
            tol = tol,
            beta = beta,
            CatchSharePrice = CatchSharePrice,
            CatchShareCost = CatchShareCost,
            Policies = Policies,
            ProjectionTime = ProjectionTime,
            TempStockMatrix = TempStockMatrix,
            StatusQuoPolicy = StatusQuoPolicy
          )
        sfStop()
      }

    }
    if (NumCPUs == 1)

    {
      Projections <- (
        lapply(
          1:(length(Stocks)),
          SnowProjections,
          Data = Data,
          BaselineYear = BaselineYear,
          Stocks = Stocks,
          IdVar = IdVar,
          bvec = bvec,
          Discount = Discount,
          tol = tol,
          beta = beta,
          CatchSharePrice = CatchSharePrice,
          CatchShareCost = CatchShareCost,
          Policies = Policies,
          ProjectionTime = ProjectionTime,
          TempStockMatrix = TempStockMatrix,
          StatusQuoPolicy = StatusQuoPolicy
        )
      )
    }


    PolicyStorage <-
      lapply(seq(along = Projections), function(i)
        Projections[[i]]$PolicyStorage)

    TempMat <-
      lapply(seq(along = Projections), function(i)
        Projections[[i]]$TempMat)

    TempStockMatrix <- bind_rows(TempMat)

    PolicyStorage <- bind_rows(PolicyStorage)


    Data$Policy <- NA


    Data$Profits <- NA

    Data$Policy[is.na(Data$Policy)] <- 'Historic'

    HistoricData <- Data$Policy == 'Historic'

    HistoricFData <-
      Data$Policy == 'Historic' &
      Data$HasRamFvFmsy == F &
      is.na(Data$FvFmsy) & Data$Dbase != 'RAM' #FIX THIS

    Data$FvFmsy[HistoricFData] <-
      (Data$Catch[HistoricFData] / Data$MSY[HistoricFData]) / Data$BvBmsy[HistoricFData]

    FOA <-
      ((Data$phi + 1) / Data$phi) * (1 - Data$BvBmsyOpenAccess ^ Data$phi / (Data$phi +
                                                                               1))

    c_num <-  Data$Price * FOA * Data$BvBmsyOpenAccess * Data$MSY

    c_den = (Data$g * FOA) ^ beta

    cost = (c_num / c_den)[HistoricData]

    Data$MarginalCost[HistoricData] <- cost

    Data$MarginalCost[HistoricData == T &
                        Data$CatchShare == 1] <-
      (Data$MarginalCost * CatchShareCost)[HistoricData == T &
                                             Data$CatchShare == 1]

    Data$Profits[HistoricData] = Data$Price[HistoricData] * Data$Catch[HistoricData] - Data$MarginalCost[HistoricData] *
      (Data$FvFmsy[HistoricData] * Data$g[HistoricData]) ^ beta

    DataPlus <- rbind((Data), (TempStockMatrix))

    return(list(DataPlus = DataPlus, PolicyStorage = PolicyStorage))

  }
