#' Plot an Object of Class cs_distribution
#'
#' @description This function creates a generic clinical significance plot by
#'   plotting the patients' pre intervention value on the x-axis and the post
#'   intervention score on the y-axis.
#'
#' @param x An object of class `cs_distribution`
#' @param x_lab String, x axis label. Default is `"Pre"`.
#' @param y_lab String, x axis label. Default is `"Post"`.
#' @param color_lab String, color label (if colors are displayed). Default is
#'   `"Group"`
#' @param lower_limit Numeric, lower plotting limit. Defaults to 2% smaller than
#'   minimum instrument score
#' @param upper_limit Numeric, upper plotting limit. Defaults to 2% larger than
#'   maximum instrument score
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `category` (shows all categories at once)
#'   - `improved` (shows improved participants)
#'   - `unchanged` (shows unchanged participants)
#'   - `deteriorated` (shows deteriorated participants)
#' @param point_alpha Numeric, transparency adjustment for points. A value
#'   between 0 and 1 where 1 corresponds to not transparent at all and 0 to
#'   fully transparent.
#' @param trajectory_alpha Numeric, transparency adjustment for trajectories. A
#'   value between 0 and 1 where 1 corresponds to not transparent at all and 0
#'   to fully transparent.
#' @param overplotting Numeric, control amount of overplotting. Defaults to 0.02
#'   (i.e., 2% of range between lower and upper limit).
#' @param rci_fill String, a color (name or HEX code) for RCI fill
#' @param rci_alpha Numeric, controls the transparency of the RCI. This can be
#'   any value between 0 and 1, defaults to 0.1
#' @param ... Additional arguments
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_distribution(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     reliability = 0.80
#'   )
#'
#'
#' # Plot the results "as is"
#' plot(cs_results)
#'
#'
#' # Change the axis labels
#' plot(cs_results, x_lab = "Before Intervention", y_lab = "After Intervention")
#'
#'
#' # Show the individual categories
#' plot(cs_results, show = category)
#'
#'
#' # Show a specific
#' plot(cs_results, show = improved)
#'
#'
#' # Show groups as specified in the data
#' cs_results_grouped <- antidepressants |>
#'   cs_distribution(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     reliability = 0.80,
#'     group = condition
#'   )
#'
#' plot(cs_results_grouped)
#'
#'
#' # To avoid overplotting, generic ggplot2 code can be used to facet the plot
#' library(ggplot2)
#' plot(cs_results_grouped) +
#'   facet_wrap(~ group)
#'
#'
#' # Adjust the transparency of individual data points
#' plot(cs_results, point_alpha = 0.3)
#'
#'
#' # Adjust the fill and transparency of the "unchanged" (RCI) region
#' plot(cs_results, rci_fill = "firebrick", rci_alpha = 0.2)
#'
#'
#' # Control the overplotting
#' plot(cs_results, overplotting = 0.1)
#'
#'
#' # Or adjust the axis limits by hand
#' plot(cs_results, lower_limit = 0, upper_limit = 80)
plot.cs_distribution <- function(
  x,
  x_lab = NULL,
  y_lab = NULL,
  color_lab = "Group",
  lower_limit,
  upper_limit,
  show,
  point_alpha = 1,
  trajectory_alpha = 1,
  rci_fill = "grey10",
  rci_alpha = 0.1,
  overplotting = 0.02,
  ...
) {
  # Which plot should be plotted?
  cs_method <- x[["method"]]
  if (cs_method != "HLM") {
    which_plot <- "point"
  } else {
    which_plot <- "trajectory"
  }

  # Get augmented data for plotting
  if (which_plot == "point") {
    data <- cs_get_augmented_data(x) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  } else if (which_plot == "trajectory") {
    model_data <- cs_get_data(x, "model")
    categories <- cs_get_augmented_data(x)

    if (.has_group(model_data)) {
      by_ids <- dplyr::join_by(c("id", "group"))
    } else {
      by_ids <- dplyr::join_by("id")
    }

    data <- model_data |>
      dplyr::left_join(categories, by_ids) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  }

  # If lower and upper limit are not supplied, get them based on the data
  if (missing(lower_limit)) {
    lower_limit <- min(data[["pre"]], data[["post"]])
  }
  if (missing(upper_limit)) {
    upper_limit <- max(data[["pre"]], data[["post"]])
  }

  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, thus requiring expand = FALSE
  # in coord_cartesian()
  overplot_amount <- (upper_limit - lower_limit) * overplotting
  lower_limit <- lower_limit - overplot_amount
  upper_limit <- upper_limit + overplot_amount
  x_limits <- y_limits <- c(lower_limit, upper_limit)

  # Generate data for the RCI band
  if (cs_method != "HLM") {
    rci_data <- generate_plotting_band(
      x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  }

  # Default plot labels
  if (which_plot == "point" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Pre"
    y_lab <- "Post"
  } else if (which_plot == "trajectory" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Measurement"
    y_lab <- "Outcome Score"
  }

  # Create a list of geoms added to the plot
  if (cs_method != "HLM") {
    geom_list <- list(
      ggplot2::geom_ribbon(
        data = rci_data,
        ggplot2::aes(y = NULL, ymin = ymin, ymax = ymax),
        fill = rci_fill,
        alpha = rci_alpha
      ),
      ggplot2::geom_abline(color = "grey10"),
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_point(ggplot2::aes(color = group), alpha = point_alpha)
      } else {
        ggplot2::geom_point(
          ggplot2::aes(color = {{ show }}),
          alpha = point_alpha
        )
      }
    )
  } else if (cs_method == "HLM") {
    geom_list_trajectory <- list(
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_line(
          ggplot2::aes(color = group),
          alpha = trajectory_alpha,
          na.rm = TRUE
        )
      } else {
        ggplot2::geom_line(
          ggplot2::aes(color = {{ show }}),
          alpha = trajectory_alpha,
          na.rm = TRUE
        )
      }
    )
  }

  # Plot the whole thing
  if (which_plot == "point") {
    data |>
      ggplot2::ggplot(ggplot2::aes(pre, post)) +
      geom_list +
      ggplot2::coord_cartesian(
        xlim = x_limits,
        ylim = y_limits,
        expand = FALSE
      ) +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
  } else if (which_plot == "trajectory") {
    data |>
      ggplot2::ggplot(ggplot2::aes(time, outcome, group = id)) +
      geom_list_trajectory +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab) +
      ggplot2::facet_wrap(ggplot2::vars(category))
  }
}


#' Plot an Object of Class cs_statistical
#'
#' @description This function creates a generic clinical significance plot by
#'   plotting the patients' pre intervention value on the x-axis and the post
#'   intervention score on the y-axis.
#'
#' @inheritParams plot.cs_distribution
#' @param x An object of class `cs_statistical`
#' @param include_cutoff Logical, whether to include the population cutoff.
#'   Default is `TRUE`.
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `category` (shows all categories at once)
#'   - `clinical_pre` (shows participants with clinical scores pre intervention)
#'   - `functional_post` (shows participants with functional scores post
#'     intervention)
#'   - `unchanged` (shows unchanged participants)
#' @param point_alpha Numeric, transparency adjustment for points. A value
#'   between 0 and 1 where 1 corresponds to not transparent at all and 0 to
#'   fully transparent.
#'
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_statistical(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     m_functional = 15,
#'     sd_functional = 8,
#'     cutoff_type = "c"
#'   )
#'
#'
#' # Plot the results "as is"
#' plot(cs_results)
#'
#'
#' # Change the axis labels
#' plot(cs_results, x_lab = "Before Intervention", y_lab = "After Intervention")
#'
#'
#' # Show the individual categories
#' plot(cs_results, show = category)
#'
#'
#' # Show groups as specified in the data
#' cs_results_grouped <- antidepressants |>
#'   cs_statistical(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     m_functional = 15,
#'     sd_functional = 8,
#'     cutoff_type = "c",
#'     group = condition
#'   )
#'
#' plot(cs_results_grouped)
#'
#'
#' # To avoid overplotting, generic ggplot2 code can be used to facet the plot
#' library(ggplot2)
#' plot(cs_results_grouped) +
#'   facet_wrap(~ group)
#'
#'
#' # Adjust the transparency of individual data points
#' plot(cs_results, point_alpha = 0.3)
#'
#'
#' # Control the overplotting
#' plot(cs_results, overplotting = 0.1)
#'
#'
#' # Or adjust the axis limits by hand
#' plot(cs_results, lower_limit = 0, upper_limit = 80)
plot.cs_statistical <- function(
  x,
  x_lab = "Pre",
  y_lab = "Post",
  color_lab = "Group",
  include_cutoff = TRUE,
  lower_limit,
  upper_limit,
  show,
  point_alpha = 1,
  overplotting = 0.02,
  ...
) {
  cs_method <- x[["method"]]

  # Get data
  data <- cs_get_augmented_data(x) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
    )

  # Get the cutoff
  cs_cutoff <- cs_get_cutoff(x)[["value"]]

  # If lower and upper limit are not supplied, get them based on the data
  if (missing(lower_limit)) {
    lower_limit <- min(data[["pre"]], data[["post"]])
  }
  if (missing(upper_limit)) {
    upper_limit <- max(data[["pre"]], data[["post"]])
  }

  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, thus requiring expand = FALSE
  # in coord_cartesian()
  overplot_amount <- (upper_limit - lower_limit) * overplotting
  lower_limit <- lower_limit - overplot_amount
  upper_limit <- upper_limit + overplot_amount
  x_limits <- y_limits <- c(lower_limit, upper_limit)

  # Create a list of geoms that can be added to the plot
  geom_list <- list(
    ggplot2::geom_vline(xintercept = cs_cutoff, lty = "dashed"),
    ggplot2::geom_hline(yintercept = cs_cutoff, lty = "dashed"),
    ggplot2::geom_abline(color = "grey10"),
    if (.has_group(data) & missing(show)) {
      ggplot2::geom_point(ggplot2::aes(color = group), alpha = point_alpha)
    } else {
      ggplot2::geom_point(ggplot2::aes(color = {{ show }}), alpha = point_alpha)
    }
  )

  # Plot the whole thing
  data |>
    ggplot2::ggplot(ggplot2::aes(pre, post)) +
    geom_list +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
}


#' Plot an Object of Class cs_combined
#'
#' @description This function creates a generic clinical significance plot by
#'   plotting the patients' pre intervention value on the x-axis and the post
#'   intervention score on the y-axis.
#'
#' @param x An object of class `cs_distribution`
#' @param x_lab String, x axis label. Default is `"Pre"`.
#' @param y_lab String, x axis label. Default is `"Post"`.
#' @param color_lab String, color label (if colors are displayed). Default is
#'   `"Group"`
#' @param lower_limit Numeric, lower plotting limit. Defaults to 2% smaller
#'   than minimum instrument score
#' @param upper_limit Numeric, upper plotting limit. Defaults to 2% larger than
#'   maximum instrument score
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `category` (shows all categories at once)
#'   - `recovered` (shows recovered participants)
#'   - `improved` (shows improved participants)
#'   - `unchanged` (shows unchanged participants)
#'   - `deteriorated` (shows deteriorated participants)
#'   - `harmed` (shows harmed participants)
#' @param point_alpha Numeric, transparency adjustment for points. A value
#'   between 0 and 1 where 1 corresponds to not transparent at all and 0 to
#'   fully transparent.
#' @param trajectory_alpha Numeric, transparency adjustment for trajectories. A
#'   value between 0 and 1 where 1 corresponds to not transparent at all and 0
#'   to fully transparent.
#' @param overplotting Numeric, control amount of overplotting. Defaults to 0.02
#'   (i.e., 2% of range between lower and upper limit).
#' @param rci_fill String, a color (name or HEX code) for RCI fill
#' @param rci_alpha Numeric, controls the transparency of the RCI. This can be
#'   any value between 0 and 1, defaults to 0.1
#' @param ... Additional arguments
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_combined(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     reliability = 0.80,
#'     m_functional = 15,
#'     sd_functional = 8,
#'     cutoff_type = "c"
#'   )
#'
#'
#' # Plot the results "as is"
#' plot(cs_results)
#'
#'
#' # Change the axis labels
#' plot(cs_results, x_lab = "Before Intervention", y_lab = "After Intervention")
#'
#'
#' # Show the individual categories
#' plot(cs_results, show = category)
#'
#'
#' # Show a specific
#' plot(cs_results, show = recovered)
#'
#'
#' # Show groups as specified in the data
#' cs_results_grouped <- antidepressants |>
#'   cs_combined(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     reliability = 0.80,
#'     m_functional = 15,
#'     sd_functional = 8,
#'     cutoff_type = "c",
#'     group = condition
#'   )
#'
#' plot(cs_results_grouped)
#'
#'
#' # To avoid overplotting, generic ggplot2 code can be used to facet the plot
#' library(ggplot2)
#' plot(cs_results_grouped) +
#'   facet_wrap(~ group)
#'
#'
#' # Adjust the transparency of individual data points
#' plot(cs_results, point_alpha = 0.3)
#'
#'
#' # Adjust the fill and transparency of the "unchanged" (RCI) region
#' plot(cs_results, rci_fill = "firebrick", rci_alpha = 0.2)
#'
#'
#' # Control the overplotting
#' plot(cs_results, overplotting = 0.1)
#'
#'
#' # Or adjust the axis limits by hand
#' plot(cs_results, lower_limit = 0, upper_limit = 80)
plot.cs_combined <- function(
  x,
  x_lab = NULL,
  y_lab = NULL,
  color_lab = "Group",
  lower_limit,
  upper_limit,
  show,
  point_alpha = 1,
  trajectory_alpha = 1,
  rci_fill = "grey10",
  rci_alpha = 0.1,
  overplotting = 0.02,
  ...
) {
  # Which plot should be plotted?
  cs_method <- x[["method"]]
  if (cs_method != "HLM") {
    which_plot <- "point"
  } else {
    which_plot <- "trajectory"
  }

  # Get augmented data for plotting
  if (which_plot == "point") {
    data <- cs_get_augmented_data(x) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  } else if (which_plot == "trajectory") {
    model_data <- cs_get_data(x, "model")
    categories <- cs_get_augmented_data(x)

    # Join the data accordingly
    if (.has_group(model_data)) {
      by_ids <- dplyr::join_by(c("id", "group"))
    } else {
      by_ids <- dplyr::join_by("id")
    }

    data <- model_data |>
      dplyr::left_join(categories, by_ids) |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
      )
  }

  # Get the cutoff
  cs_cutoff <- cs_get_cutoff(x)[["value"]]

  # If lower and upper limit are not supplied, get them based on the data
  if (missing(lower_limit)) {
    lower_limit <- min(data[["pre"]], data[["post"]])
  }
  if (missing(upper_limit)) {
    upper_limit <- max(data[["pre"]], data[["post"]])
  }

  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, thus requiring expand = FALSE
  # in coord_cartesian()
  overplot_amount <- (upper_limit - lower_limit) * overplotting
  lower_limit <- lower_limit - overplot_amount
  upper_limit <- upper_limit + overplot_amount
  x_limits <- y_limits <- c(lower_limit, upper_limit)

  # Generate data for the RCI band
  if (cs_method != "HLM") {
    rci_data <- generate_plotting_band(
      x,
      lower_limit = lower_limit,
      upper_limit = upper_limit
    )
  }

  # Default plot labels
  if (which_plot == "point" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Pre"
    y_lab <- "Post"
  } else if (which_plot == "trajectory" & is.null(x_lab) & is.null(y_lab)) {
    x_lab <- "Measurement"
    y_lab <- "Outcome Score"
  }

  # Create a list of geoms added to the plot
  if (cs_method != "HLM") {
    geom_list <- list(
      ggplot2::geom_vline(xintercept = cs_cutoff, lty = "dashed"),
      ggplot2::geom_hline(yintercept = cs_cutoff, lty = "dashed"),
      ggplot2::geom_ribbon(
        data = rci_data,
        ggplot2::aes(y = NULL, ymin = ymin, ymax = ymax),
        fill = rci_fill,
        alpha = rci_alpha
      ),
      ggplot2::geom_abline(color = "grey10"),
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_point(ggplot2::aes(color = group), alpha = point_alpha)
      } else {
        ggplot2::geom_point(
          ggplot2::aes(color = {{ show }}),
          alpha = point_alpha
        )
      }
    )
  } else if (cs_method == "HLM") {
    geom_list_trajectory <- list(
      if (.has_group(data) & missing(show)) {
        ggplot2::geom_line(
          ggplot2::aes(color = group),
          alpha = trajectory_alpha,
          na.rm = TRUE
        )
      } else {
        ggplot2::geom_line(
          ggplot2::aes(color = {{ show }}),
          alpha = trajectory_alpha,
          na.rm = TRUE
        )
      }
    )
  }

  # Plot the whole thing
  if (which_plot == "point") {
    data |>
      ggplot2::ggplot(ggplot2::aes(pre, post)) +
      geom_list +
      ggplot2::coord_cartesian(
        xlim = x_limits,
        ylim = y_limits,
        expand = FALSE
      ) +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
  } else if (which_plot == "trajectory") {
    data |>
      ggplot2::ggplot(ggplot2::aes(time, outcome, group = id)) +
      geom_list_trajectory +
      ggplot2::labs(x = x_lab, y = y_lab, color = color_lab) +
      ggplot2::facet_wrap(ggplot2::vars(category))
  }
}


#' Plot an Object of Class cs_percentage
#'
#' @description This function creates a generic clinical significance plot by
#'   plotting the patients' pre intervention value on the x-axis and the post
#'   intervention score on the y-axis.
#'
#' @inheritParams plot.cs_distribution
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `improved` (shows improved participants)
#'   - `unchanged` (shows unchanged participants)
#'   - `deteriorated` (shows deteriorated participants)
#' @param point_alpha Numeric, transparency adjustment for points. A value
#'   between 0 and 1 where 1 corresponds to not transparent at all and 0 to
#'   fully transparent.
#' @param pct_fill String, a color (name or HEX code) for the percentage range
#'   fill
#' @param pct_alpha Numeric, controls the transparency of the percentage fill.
#'   This can be any value between 0 and 1, defaults to 0.1
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_percentage(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     pct_improvement = 0.4
#'   )
#'
#'
#' # Plot the results "as is"
#' plot(cs_results)
#'
#'
#' # Change the axis labels
#' plot(cs_results, x_lab = "Before Intervention", y_lab = "After Intervention")
#'
#'
#' # Show the individual categories
#' plot(cs_results, show = category)
#'
#'
#' # Show a specific category
#' plot(cs_results, show = improved)
#'
#'
#' # Show groups as specified in the data
#' cs_results_grouped <- antidepressants |>
#'   cs_percentage(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     pct_improvement = 0.4,
#'     group = condition
#'   )
#'
#' plot(cs_results_grouped)
#'
#'
#' # To avoid overplotting, generic ggplot2 code can be used to facet the plot
#' library(ggplot2)
#' plot(cs_results_grouped) +
#'   facet_wrap(~ group)
#'
#'
#' # Adjust the transparency of individual data points
#' plot(cs_results, point_alpha = 0.3)
#'
#'
#' # Adjust the fill and transparency of the "unchanged" (PCC) region
#' plot(cs_results, pct_fill = "firebrick", pct_alpha = 0.2)
#'
#'
#' # Control the overplotting
#' plot(cs_results, overplotting = 0.1)
#'
#'
#' # Or adjust the axis limits by hand
#' plot(cs_results, lower_limit = 0, upper_limit = 80)
plot.cs_percentage <- function(
  x,
  x_lab = "Pre",
  y_lab = "Post",
  color_lab = "Group",
  lower_limit,
  upper_limit,
  show,
  point_alpha = 1,
  pct_fill = "grey10",
  pct_alpha = 0.1,
  overplotting = 0.02,
  ...
) {
  # Get augmented data for plotting
  data <- cs_get_augmented_data(x) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
    )

  # If lower and upper limit are not supplied, get them based on the data
  if (missing(lower_limit)) {
    lower_limit <- min(data[["pre"]], data[["post"]])
  }
  if (missing(upper_limit)) {
    upper_limit <- max(data[["pre"]], data[["post"]])
  }

  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, thus requiring expand = FALSE
  # in coord_cartesian()
  overplot_amount <- (upper_limit - lower_limit) * overplotting
  lower_limit <- lower_limit - overplot_amount
  upper_limit <- upper_limit + overplot_amount
  x_limits <- y_limits <- c(lower_limit, upper_limit)

  # Generate data for the RCI band
  band_data <- generate_plotting_band(
    x,
    lower_limit = lower_limit,
    upper_limit = upper_limit
  )

  # Create a list of geoms added to the plot
  geom_list <- list(
    ggplot2::geom_ribbon(
      data = band_data,
      ggplot2::aes(y = NULL, ymin = ymin, ymax = ymax),
      fill = pct_fill,
      alpha = pct_alpha
    ),
    ggplot2::geom_abline(color = "grey10"),
    if (.has_group(data) & missing(show)) {
      ggplot2::geom_point(ggplot2::aes(color = group), alpha = point_alpha)
    } else {
      ggplot2::geom_point(ggplot2::aes(color = {{ show }}), alpha = point_alpha)
    }
  )

  # Plot the whole thing
  data |>
    ggplot2::ggplot(ggplot2::aes(pre, post)) +
    geom_list +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
}


#' Plot an Object of Class cs_anchor_individual
#'
#' @description This function creates a generic clinical significance plot by
#'   plotting the patients' pre intervention value on the x-axis and the post
#'   intervention score on the y-axis.
#'
#' @inheritParams plot.cs_distribution
#' @param show Unquoted category name. You have several options to color
#'   different features. Available are
#'   - `improved` (shows improved participants)
#'   - `unchanged` (shows unchanged participants)
#'   - `deteriorated` (shows deteriorated participants)
#' @param point_alpha Numeric, transparency adjustment for points. A value
#'   between 0 and 1 where 1 corresponds to not transparent at all and 0 to
#'   fully transparent.
#' @param mid_fill String, a color (name or HEX code) for the percentage range
#'   fill
#' @param mid_alpha Numeric, controls the transparency of the percentage fill.
#'   This can be any value between 0 and 1, defaults to 0.1
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     mid_improvement = 8
#'   )
#'
#'
#' # Plot the results "as is"
#' plot(cs_results)
#'
#'
#' # Change the axis labels
#' plot(cs_results, x_lab = "Before Intervention", y_lab = "After Intervention")
#'
#'
#' # Show the individual categories
#' plot(cs_results, show = category)
#'
#'
#' # Show a specific category
#' plot(cs_results, show = improved)
#'
#'
#' # Show groups as specified in the data
#' cs_results_grouped <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     pre = "Before",
#'     mom_di,
#'     mid_improvement = 8,
#'     group = condition
#'   )
#'
#' plot(cs_results_grouped)
#'
#'
#' # To avoid overplotting, generic ggplot2 code can be used to facet the plot
#' library(ggplot2)
#' plot(cs_results_grouped) +
#'   facet_wrap(~ group)
#'
#'
#' # Adjust the transparency of individual data points
#' plot(cs_results, point_alpha = 0.3)
#'
#'
#' # Adjust the fill and transparency of the "unchanged" (PCC) region
#' plot(cs_results, mid_fill = "firebrick", mid_alpha = 0.2)
#'
#'
#' # Control the overplotting
#' plot(cs_results, overplotting = 0.1)
#'
#'
#' # Or adjust the axis limits by hand
#' plot(cs_results, lower_limit = 0, upper_limit = 80)
plot.cs_anchor_individual <- function(
  x,
  x_lab = "Pre",
  y_lab = "Post",
  color_lab = "Group",
  lower_limit,
  upper_limit,
  show,
  point_alpha = 1,
  mid_fill = "grey10",
  mid_alpha = 0.1,
  overplotting = 0.02,
  ...
) {
  # Get augmented data for plotting
  data <- cs_get_augmented_data(x) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.logical), \(x) ifelse(x, "Yes", "No"))
    )

  # If lower and upper limit are not supplied, get them based on the data
  if (missing(lower_limit)) {
    lower_limit <- min(data[["pre"]], data[["post"]])
  }
  if (missing(upper_limit)) {
    upper_limit <- max(data[["pre"]], data[["post"]])
  }

  # Determine x and y limits for plotting. Overplotting is needed because we
  # want the ribbon to be at the edge of the plot, thus requiring expand = FALSE
  # in coord_cartesian()
  overplot_amount <- (upper_limit - lower_limit) * overplotting
  lower_limit <- lower_limit - overplot_amount
  upper_limit <- upper_limit + overplot_amount
  x_limits <- y_limits <- c(lower_limit, upper_limit)

  # Generate data for the RCI band
  band_data <- generate_plotting_band(
    x,
    lower_limit = lower_limit,
    upper_limit = upper_limit
  )

  # Create a list of geoms added to the plot
  geom_list <- list(
    ggplot2::geom_ribbon(
      data = band_data,
      ggplot2::aes(y = NULL, ymin = ymin, ymax = ymax),
      fill = mid_fill,
      alpha = mid_alpha
    ),
    ggplot2::geom_abline(color = "grey10"),
    if (.has_group(data) & missing(show)) {
      ggplot2::geom_point(ggplot2::aes(color = group), alpha = point_alpha)
    } else {
      ggplot2::geom_point(ggplot2::aes(color = {{ show }}), alpha = point_alpha)
    }
  )

  # Plot the whole thing
  data |>
    ggplot2::ggplot(ggplot2::aes(pre, post)) +
    geom_list +
    ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    ggplot2::labs(x = x_lab, y = y_lab, color = color_lab)
}


#' Plot an Object of Class cs_anchor_group_within
#'
#' @description This function creates a generic group level clinical
#'   significance plot by plotting the within group change with the associated
#'   uncertainty interval around the estimated change on the y-axis.
#'
#' @param x An object of class `cs_anchor_group_within`
#' @param x_lab String, x axis label. Defaults to `"Group"`
#' @param y_lab String, y axis label, defaults to `"Mean Intervention Effect
#'   (with 95%-CI)"`
#' @param ... Additional arguments
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     mid_improvement = 8,
#'     target = "group",
#'     group = condition
#'   )
#'
#'
#' # Plot the results "as is"
#' plot(cs_results)
#'
#'
#' # Change the axis labels
#' plot(cs_results, x_lab = "Condition", y_lab = "Treatment Effect")
plot.cs_anchor_group_within <- function(
  x,
  x_lab = "Group",
  y_lab = "Mean Intervention Effect\n(with 95%-CI)",
  ...
) {
  # Get augmented data for plotting
  data <- x[["anchor_results"]]
  mid_improvement <- x[["mid_improvement"]]
  direction <- x[["direction"]]

  threshold <- direction * mid_improvement

  geom_list <- list(
    ggplot2::geom_hline(yintercept = 0),
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed"),
    ggplot2::geom_point(shape = 15, size = 2),
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      width = 0.2
    ),
    ggplot2::expand_limits(y = 0, x = 0:2)
  )

  if (.has_group(data)) {
    data |>
      ggplot2::ggplot(ggplot2::aes(group, difference)) +
      geom_list +
      ggplot2::labs(x = x_lab, y = y_lab)
  } else {
    data |>
      ggplot2::ggplot(ggplot2::aes("", difference)) +
      geom_list +
      ggplot2::labs(x = x_lab, y = y_lab)
  }
}


#' Plot an Object of Class cs_anchor_group_between
#'
#' @description This function creates a generic group level clinical
#'   significance plot by plotting the between group change with the associated
#'   uncertainty interval around the estimated change on the y-axis.
#'
#' @param x An object of class `cs_anchor_group_between`
#' @param x_lab String, x axis label, defaults to `"Group"`
#' @param y_lab String, y axis label, defaults to `"Mean Intervention Effect
#'   (with 95%-CI)"`
#' @param ... Additional arguments
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     mid_improvement = 8,
#'     target = "group",
#'     group = condition,
#'     effect = "between",
#'     post = "After"
#'   )
#'
#'
#' # Plot the results "as is"
#' plot(cs_results)
#'
#'
#' # Change the axis labels
#' plot(cs_results, x_lab = "Condition", y_lab = "Treatment Effect")
plot.cs_anchor_group_between <- function(
  x,
  x_lab = "Group",
  y_lab = "Mean Intervention Effect\n(with 95%-CI)",
  ...
) {
  # Get augmented data for plotting
  data <- x[["anchor_results"]]
  mid_improvement <- x[["mid_improvement"]]
  direction <- x[["direction"]]

  threshold <- direction * mid_improvement

  geom_list <- list(
    ggplot2::geom_hline(yintercept = 0),
    ggplot2::geom_hline(yintercept = threshold, linetype = "dashed"),
    ggplot2::geom_point(shape = 15, size = 2),
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      width = 0.2
    ),
    ggplot2::expand_limits(y = 0, x = 0:2)
  )

  data |>
    ggplot2::ggplot(ggplot2::aes(comparison, difference)) +
    geom_list +
    ggplot2::labs(x = x_lab, y = y_lab)
}
