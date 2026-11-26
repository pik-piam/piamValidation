#' takes the output of "validateScenarios()" creates a line plot
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
#' @importFrom dplyr filter mutate group_by reframe bind_rows %>%
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

  # Prepare threshold background data, warning about multiple threshold sets
  # is already given above
  d_background <- getThresholdBands(valiData, warnMultiple = FALSE) %>%
    arrange(period)

  # plot thresholds as colored background areas or whiskers if only one period
  p <- ggplot()

  # helper for segmented threshold ribbons
  addThresholdBand <- function(p,
                               data,
                               lower,
                               upper,
                               fill,
                               alpha = 0.2) {

    d <- data %>%
      arrange(period)

    # keep only rows where both limits exist
    d$valid <- !is.na(d[[lower]]) &
      !is.na(d[[upper]])

    # identify contiguous valid segments
    d$group <- cumsum(
      d$valid != dplyr::lag(
        d$valid,
        default = d$valid[1]
      )
    )

    d_valid <- d %>%
      filter(valid)

    if (nrow(d_valid) == 0) {
      return(p)
    }

    group_sizes <- d_valid %>%
      dplyr::count(group, name = "n")

    multi_groups <- group_sizes %>%
      filter(n > 1) %>%
      pull(group)

    single_groups <- group_sizes %>%
      filter(n == 1) %>%
      pull(group)

    # regular ribbons
    if (length(multi_groups) > 0) {

      d_multi <- d_valid %>%
        filter(group %in% multi_groups)

      p <- p +
        geom_ribbon(
          data = d_multi,
          aes(
            x = period,
            ymin = .data[[lower]],
            ymax = .data[[upper]],
            group = group
          ),
          fill = fill,
          alpha = alpha,
          color = NA,
          inherit.aes = FALSE
        )
    }

    # single-period fallback
    if (length(single_groups) > 0) {

      d_single <- d_valid %>%
        filter(group %in% single_groups)

      p <- p +
        geom_linerange(
          data = d_single,
          aes(
            x = period,
            ymin = .data[[lower]],
            ymax = .data[[upper]]
          ),
          linewidth = 6,
          color = fill,
          alpha = alpha,
          inherit.aes = FALSE
        )
    }

    p
  }

  # build plot
  p <- ggplot()

  # green band
  p <- addThresholdBand(
    p = p,
    data = d_background,
    lower = "min_yel",
    upper = "max_yel",
    fill = "#008450",
    alpha = 0.20
  )

  # upper yellow band
  p <- addThresholdBand(
    p = p,
    data = d_background,
    lower = "max_yel",
    upper = "max_red",
    fill = "#EFB700",
    alpha = 0.20
  )

  # lower cyan band
  p <- addThresholdBand(
    p = p,
    data = d_background,
    lower = "min_red",
    upper = "min_yel",
    fill = "#66ccee",
    alpha = 0.30
  )

  p <- p +
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
    coord_cartesian(xlim = xlim)

  if (interactive) {
    plotly::ggplotly(p) #%>%
      #layout(legend = list(title=list(text='Model, Scenario')))
  } else {
    p
  }
}
