#' ggKobe
#'
#' \code{ggKobe} produces a kobe plot in the manner of Costello
#' et al 2016, using data frmo
#' @param dat dataframe containing required stuff
#' @param xvar thing you want plotted on the x axis
#' @param yvar thing you want plotted on the y axis
#'
#' @return a kobe plot ggplot
#' @export
#'
#' @examples
#' ggKobe(filter(ProjectionData, Year == 2012), xvar = 'BvBmsy', yvar = 'FvFmsy')
ggKobe <- function(dat, xvar = 'BvBmsy', yvar = 'FvFmsy') {
  dat <- ungroup(dat)
  
  dat[, yvar] <- pmin(4, as.matrix(dat[, yvar]))
  
  dat$is_ram <- dat$Dbase == 'RAM'
  
  dots <-
    eval(parse(
      text = paste(
        'list(~median(',
        xvar,
        ', na.rm = T),~median(',
        yvar,
        ', na.rm = T))',
        sep = ''
      )
    ))
  
  summary_dat <- dat %>%
    ungroup() %>%
    summarise_(.dots = setNames(dots, c('median_x', 'median_y'))) %>%
    mutate(is_ram = NA, MSY = NA)
  
  kobe <- dat %>%
    ggplot(aes_string(xvar, yvar)) + #general aesthetic
    stat_density_2d(
      aes(fill = ..density..),
      geom = 'tile',
      n = 100,
      alpha = 0.8,
      contour = F
    ) + #eggplot
    scale_fill_gradient2(
      guide = F,
      low = 'skyblue1',
      mid = 'white',
      high = 'khaki1',
      midpoint = 0.2
    ) + #set eggplot colors
    geom_hline(aes(yintercept = 1), linetype = 'longdash') +
    geom_vline(aes(xintercept = 1), linetype = 'longdash') +
    geom_point(aes_string(
      as.name(xvar),
      as.name(yvar),
      color = "is_ram",
      size = "MSY",
      alpha = ("MSY")
    )) + #plot points
    scale_color_manual(guide = F, values = c('grey', 'red')) +
    geom_point(
      data = summary_dat,
      aes(median_x, median_y),
      shape = 17,
      size = 4
    ) + #plot median
    # geom_point(
    #   data = filter(kobe_summary, fao_region_long == 'Global'),
    #   aes(x = msy_weighted_geom_mean_b, y = msy_weighted_geom_mean_f),
    #   shape = 15,
    #   size = 4
    # ) + #plot geometric summaries
    scale_size_continuous(guide = F) + #turn off legends
    scale_alpha_continuous(guide = F, range = c(0.5, 0.9)) +
    xlab(xvar) +
    ylab(yvar) +
    theme_classic() +
    theme(text = element_text(size = 16)) + #Extend the boundaries
    scale_x_continuous(
      limits = c(-1, 4),
      breaks = seq(-1, 4, by = 0.5),
      labels = c(
        seq(-1, 2, by = 0.5),
        expression(phantom(x) >= 2.5),
        seq(3, 4, by = 0.5)
      )
    ) +
    scale_y_continuous(
      limits = c(-1, 6),
      breaks = seq(-1, 6, by = 0.5),
      labels = c(
        seq(-1, 3.5, by = 0.5),
        expression(phantom(x) >= 4),
        seq(4.5, 6, by = 0.5)
      )
    ) +
    coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 4)) #Trim the boundaries
  
  return(kobe)
}