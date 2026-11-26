#' takes the output of "validateScenarios()" and computes threshold bands
#'
#' converts the thresholds attached to each data point by
#' ``validateScenarios()`` into absolute values per variable, unit, region and
#' period. The resulting bands can be plotted as colored background areas
#' behind scenario data, see ``linePlotThresholds()`` or the ``thresholds``
#' argument of ``mip::createLinePlots()``.
#'
#' For checks using the metrics "relative" and "difference", thresholds are
#' converted into absolute values using the reference values of the respective
#' check. Thresholds which are not defined in the config appear as -Inf/Inf.
#'
#' @param valiData data.frame as returned by ``validateScenarios()``
#' @param warnMultiple if TRUE, warn in case multiple sets of thresholds are
#'        found for the same variable, unit, region and period, e.g. from
#'        checks using multiple metrics or reference scenarios; plotting such
#'        bands can lead to overlaps
#' @return data.frame with columns variable, unit, region, period, min_red,
#'         min_yel, max_yel and max_red
#'
#' @importFrom dplyr filter group_by reframe bind_rows distinct arrange count %>%
#' @export
getThresholdBands <- function(valiData, warnMultiple = TRUE) {

  if (nrow(valiData) == 0) {
    stop("Empty data frame given to getThresholdBands().")
  }

  # thresholds of relative checks are converted using reference values
  bands_rel <- valiData %>%
    filter(metric == "relative") %>%
    group_by(.data$variable, .data$unit, .data$region, .data$period) %>%
    reframe(
      min_red = ((1 + .data$min_red) * .data$ref_value_min),
      min_yel = ((1 + .data$min_yel) * .data$ref_value_min),
      max_yel = ((1 + .data$max_yel) * .data$ref_value_max),
      max_red = ((1 + .data$max_red) * .data$ref_value_max)
    )

  # thresholds of difference checks are converted using reference values
  bands_dif <- valiData %>%
    filter(metric == "difference") %>%
    group_by(.data$variable, .data$unit, .data$region, .data$period) %>%
    reframe(
      min_red = .data$min_red + .data$ref_value_min,
      min_yel = .data$min_yel + .data$ref_value_min,
      max_yel = .data$max_yel + .data$ref_value_max,
      max_red = .data$max_red + .data$ref_value_max
    )

  # thresholds of absolute checks can be used directly
  bands_abs <- valiData %>%
    filter(metric == "absolute") %>%
    group_by(.data$variable, .data$unit, .data$region, .data$period) %>%
    reframe(
      .data$min_red,
      .data$min_yel,
      .data$max_yel,
      .data$max_red
    )

  bands <- bind_rows(
    bands_rel,
    bands_dif,
    bands_abs
  ) %>%
    distinct() %>%
    arrange(.data$variable, .data$region, .data$period)

  if (warnMultiple) {
    n_multiple <- bands %>%
      count(.data$variable, .data$unit, .data$region, .data$period) %>%
      filter(.data$n > 1) %>%
      nrow()
    if (n_multiple > 0) {
      warning(n_multiple, " combination(s) of variable, region and period ",
              "have more than one set of thresholds, e.g. from checks using ",
              "multiple metrics or reference scenarios. Plotted bands may ",
              "overlap.")
    }
  }

  return(bands)
}
