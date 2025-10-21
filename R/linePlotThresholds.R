#' takes the output of "validateScenarios()" and plots heat maps per variable
#'
#' @param valiData data to be plotted, as returned by ``validateScenarios()``
#'        and after filtering for one variable and one region.
#' @param scenData hand over additional scenario data to be plotted alongside
#'        the validation data. Will use the same variable and region, otherwise
#'        all available data.
#' @param refData hand over additional reference data to be plotted alongside
#'        the validation data. Will use the same variable and region, otherwise
#'        all available data.
#' @param xlim set limits for the x axis
#' @param interactive decide if an interactive ggploty object should be returned
#' @importFrom dplyr filter mutate group_by reframe %>%
#' @import ggplot2
#' @export

linePlotThresholds <- function(valiData,
                               scenData = NULL,
                               refData = NULL,
                               xlim = c(2010, 2030),
                               interactive = TRUE) {

  if (nrow(valiData) == 0) stop("Empty data frame given to plot function.")

  if (length(unique(valiData$variable)) > 1) {
    stop(paste("Multiple variables are present in validation data,
               filter data to contain only one before plotting.\n",
               paste(unique(valiData$variable), collapse = "\n")
               )
         )
  } else {
    var <- as.character(unique(valiData$variable))
    unit <- as.character(unique(valiData$unit))
  }

  if (length(unique(valiData$region)) > 1) {
    stop(paste("Multiple regions are present in validation data,
               filter data to contain only one before plotting.\n",
               paste(unique(valiData$region), collapse = "\n")
               )
    )
  } else {
    reg <- as.character(unique(valiData$region))
  }

  # it is possible to combine data from relative and absolute checks
  # care is advised when using data from multiple metrics to avoid overlaps
  if (nrow(unique(valiData[,c("metric", "ref_scenario")])) > 1) {
    warning("Multiple validation metrics detected, please be aware of potential
            data overlap.")
  }

  # convert relative to absolute thresholds
  d_background_rel <- valiData %>%
    filter(valiData$metric == "relative") %>%
    group_by(period) %>%  # thresholds only vary across periods
    reframe(
      min_red = ((1+min_red) * ref_value_min),
      min_yel = ((1+min_yel) * ref_value_min),
      max_yel = ((1+max_yel) * ref_value_max),
      max_red = ((1+max_red) * ref_value_max)
    )

  # convert difference to absolute thresholds
  d_background_dif <- valiData %>%
    filter(valiData$metric == "difference") %>%
    group_by(period) %>%
    reframe(
      min_red = (min_red + ref_value_min),
      min_yel = (min_yel + ref_value_min),
      max_yel = (max_yel + ref_value_max),
      max_red = (max_red + ref_value_max)
    )

  # same data structure for absolute thresholds
  d_background_abs <- valiData %>%
    filter(valiData$metric == "absolute") %>%
    group_by(period) %>%
    reframe(min_red, min_yel, max_yel, max_red)

  d_background <- rbind(d_background_rel, d_background_dif, d_background_abs)

  # remove duplicates
  d_background <- d_background[duplicated(d_background) == FALSE, ]

  # TODO: different type of plot if thresholds are only available for one period

  # plot thresholds as colored background areas
  p <- ggplot() +
    geom_ribbon(data = d_background, aes(x = period, ymin = min_yel, ymax = max_yel),
                fill = "#008450", alpha = 0.2, color = NA, inherit.aes = FALSE) +
    geom_ribbon(data = d_background, aes(x = period, ymin = max_yel, ymax = max_red),
                fill = "#EFB700", alpha = 0.2, color = NA, inherit.aes = FALSE) +
    geom_ribbon(data = d_background, aes(x = period, ymin = min_red, ymax = min_yel),
                fill = "#66ccee", alpha = 0.3, color = NA, inherit.aes = FALSE) +
    xlab("Period") +
    ylab(paste0(var, " [", unit, "]")) +
    theme_bw()

  # add scenario data as lines
  if (!is.null(scenData)) {
    d <- scenData %>%
      filter(variable == var, region == reg,
             period >= min(xlim) & period <= max(xlim))
    p <- p +
      geom_line(data = d,
                aes(x = period, y = value, color = model, linetype = scenario))
  }

  # add reference data as points
  if (!is.null(refData)) {
    h <- refData %>%
      filter(variable == var, region == reg,
             period >= min(xlim) & period <= max(xlim))
    p <- p +
      geom_point(data = h, aes(x = period, y = value, shape = model),
                 size = 1, color = "black")
  }

  # layout
  p <- p +
    ggtitle(paste0(var, " - ", reg)) +
    scale_x_continuous(limits = xlim)

  if (interactive) {
    plotly::ggplotly(p) #%>%
      #layout(legend = list(title=list(text='Model, Scenario')))
  } else {
    p
  }
}
