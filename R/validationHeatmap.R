#' takes the output of "validateScenarios()" and plots heat maps per variable
#'
#' @param valiData data to be plotted, as returned by ``validateScenarios()``
#'        (and ``appendTooltips()`` if interactive), plus optional filtering.
#'        Needs to have at least one dimension with only one unique element.
#' @param main_dim out of the 5-dim df, 1 dim has to contain only on element,
#'        this is the main dimension of the plot, default: variable
#' @param interactive return plots as interactive plotly plots by default
#' @param x_plot choose dimension to display on x-axis of plot, if any
#'        is NULL, arrangement is chosen automatically based on data dimensions
#' @param y_plot choose dimension to display on y-axis of plot
#' @param x_facet choose dimension to display on x-dim of facets
#' @param y_facet choose dimension to display on x-dim of facets
#'
#' @importFrom dplyr filter select mutate %>%
#' @import ggplot2
#' @importFrom ggthemes theme_tufte
#' @export

validationHeatmap <- function(valiData,
                              main_dim = "variable",
                              x_plot  = NULL, y_plot  = NULL,
                              x_facet = NULL, y_facet = NULL,
                              interactive = TRUE) {

  if (nrow(valiData) == 0) stop("Empty data frame given to plot function.")

  # setup ####
  df <- valiData
  plot_title <- paste0(df[1, main_dim])

  # prepare data
  df$period <- as.character(df$period)
  standard_dims <- c("model", "scenario", "variable", "region", "period")

  colors <-  c(green     = "#008450",
               yellow    = "#EFBB0F",
               red       = "#AA0014",
               cyan      = "#7BD5F3",
               blue      = "#4477AA",
               grey      = "#808080")

  # check arguments ####

  # check if valid name for main_dim is passed
  if (!main_dim %in% standard_dims) {
    stop("Please choose 'main_dim' from the standard dimensions: \n",
         "model, scenario, variable, region or period\n")
  }

  # check if data.frame has at least one dimension of only one element
  if (length(unique(df[, main_dim])) > 1) {
    cat("Data dimensions: \n")
    print(lengths(lapply(df[, standard_dims], unique)))
    stop(main_dim, " (main_dim) can only contain one unique element,
  Please filter data before plotting or select a different main_dim.\n")
  }

  # check validation categories, only one per plot allowed
  # TODO: ref_scenario is not checked, but important as "historical" signals a
  # different type of check. Problem: how to deal with "regular" scenario
  # comparison to multiple different scenarios
  # if (length(unique(df[, c("metric")])) > 1) {
  #   cat("Validation types: \n")
  #   print(unique(df[, c("metric")]))
  #   stop("Multiple categories of checks found in data, please filter the data
  # object to contain only one metric.\n")
  # }

  # check if an incomplete set of x/y_plot/facet arguments is passed
  null_args <- sum(sapply(list(x_plot, y_plot, x_facet, y_facet), is.null))
  if (null_args %in% c(1, 2, 3)) {
    stop("Please define either all 'plot' and 'facet' arguments or none.")
  }

  # arranging dimensions ####
  if (any(is.null(c(x_plot, y_plot, x_facet, y_facet)))) {
    # select dimensions except main_dim and how they should be plotted
    # length of each dim important to find the best arrangement of axis and facets
    # generally preferred, period and region as axis, scenario and model as facets
    # variable wherever there is space
    dim_length <- sort(lengths(
      lapply(df[, setdiff(standard_dims, main_dim)], unique)
      ))
    other_dims <- names(dim_length)

    # 3 possible ways to form 2 groups of 2 dimensions each
    # start by creating possible dimension products
    p <- as.data.frame(matrix(NA, 3, 2))
    p[1, ] <- c(dim_length[1]*dim_length[2], dim_length[3]*dim_length[4])
    p[2, ] <- c(dim_length[1]*dim_length[3], dim_length[2]*dim_length[4])
    p[3, ] <- c(dim_length[1]*dim_length[4], dim_length[2]*dim_length[3])

    # select combination that is closest to ideal plot layout ratio (x/y)
    ideal <- 2
    # determine ratio of bigger to smaller dim products,
    # V1 is the first product, V2 the second product
    p <- mutate(p, ratio = ifelse(V2 > V1, abs(V2/V1 - ideal), abs(V1/V2 - ideal)))
    # find idx of row closest to ideal
    ideal_idx <- which(p[, "ratio"] == min(p[, "ratio"]), arr.ind = TRUE)[1]

    # idx is found, but we don't know if the first or second product is the larger
    # one and thus should be on the y-axis and y-facet
    if (p[ideal_idx, "V1"] < p[ideal_idx, "V2"]) {
      # V1 always contains the the first other dim "other_dims[1]"
      # (dim_length and other_dims have same order of elements)
      # other element of V1 product has index "ideal_idx" + 1 by definition
      # region or period should be "plot" if possible
      y_plot  <- ifelse(other_dims[1] %in% c("period", "region"),
                        other_dims[1],
                        other_dims[ideal_idx + 1])
      y_facet <- ifelse(other_dims[1] %in% c("period", "region"),
                        other_dims[ideal_idx + 1],
                        other_dims[1])

      # remaining two dimensions are used for x axis and facet
      x_dims <- setdiff(c(2,3,4), ideal_idx + 1)
      x_plot <- ifelse(other_dims[x_dims[1]] %in% c("period", "region"),
                       other_dims[x_dims[1]],
                       other_dims[x_dims[2]])
      x_facet <- ifelse(other_dims[x_dims[1]] %in% c("period", "region"),
                        other_dims[x_dims[2]],
                        other_dims[x_dims[1]])
    } else {
      # same as "if", just switched x and y
      x_plot  <- ifelse(other_dims[1] %in% c("period", "region"),
                        other_dims[1],
                        other_dims[ideal_idx + 1])
      x_facet <- ifelse(other_dims[1] %in% c("period", "region"),
                        other_dims[ideal_idx + 1],
                        other_dims[1])

      y_dims <- setdiff(c(2,3,4), ideal_idx + 1)
      y_plot <- ifelse(other_dims[y_dims[1]] %in% c("period", "region"),
                       other_dims[y_dims[1]],
                       other_dims[y_dims[2]])
      y_facet <- ifelse(other_dims[y_dims[1]] %in% c("period", "region"),
                        other_dims[y_dims[2]],
                        other_dims[y_dims[1]])
    }
  }

  # plot ####
  p <- ggplot(df, aes(x = .data[[x_plot, ]],
                      y = .data[[y_plot, ]],
                      fill = check,
                      text = text)) +
    geom_tile(color = "white", linewidth = 0.0) +
    scale_fill_manual(values = colors, breaks = colors) +
    facet_grid(.data[[y_facet, ]] ~ .data[[x_facet, ]]) +
    labs(x = NULL, y = NULL, title = plot_title) +
    #theme_tufte(base_family = "Arial") +
    theme_minimal() +
    theme(panel.grid.major = element_blank()) +
    theme(axis.ticks = element_blank()) +     # remove ticks
    theme(axis.text  = element_text(size = 8)) +  # font size plot labels
    theme(strip.text = element_text(size = 8)) +  # font size facet labels
    # default labels for axis and facets, might need to be adjusted depending
    # on plot layout
    theme(axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    theme(axis.text.y  = element_text(angle =  0, vjust = 0.5, hjust = 1)) +
    theme(strip.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
    theme(strip.text.y = element_text(angle =  0, vjust = 0.5, hjust = 0)) +
    coord_equal() +
    theme(legend.position = "none")

  # create small gap to "World" data by creating white outline
  # -> in some cases this creates an area where tooltips do not work, disabled
  #if("World" %in% d$region) {
    #p <- p + geom_vline(xintercept = 1.5, linewidth = 0.6, color = "white")
  #}

  if (interactive) {
    # create interactive element
    fig <- plotly::ggplotly(p, tooltip = "text") %>%
    # avoid overlap of title and facet labels (plotly issue)
    plotly::layout(title = list(y = .95, xref = "plot"),
                   margin = list(l = 0, t = 150, r = 150))
    return(fig)
  } else {
    return(p)
  }

}
