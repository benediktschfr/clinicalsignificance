#' Summarize Sensitivity Analysis Table
#'
#' @param summary_table The raw summary table from a sensitivity analysis
#' @return A summarized data frame containing Min, Max, and Difference
#' @noRd
.summarize_sensitivity_table <- function(summary_table) {
  has_group <- "group" %in% names(summary_table)

  if (has_group) {
    summary_table_agg <- summary_table |>
      dplyr::group_by(group, category)
  } else {
    summary_table_agg <- summary_table |>
      dplyr::group_by(category)
  }

  summary_table_agg <- summary_table_agg |>
    dplyr::summarise(
      Min = min(percent, na.rm = TRUE),
      Max = max(percent, na.rm = TRUE),
      Difference = max(percent, na.rm = TRUE) - min(percent, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Min = insight::format_percent(Min),
      Max = insight::format_percent(Max),
      Difference = insight::format_percent(Difference)
    ) |>
    dplyr::rename(Category = category)

  if (has_group) {
    summary_table_agg <- summary_table_agg |>
      dplyr::rename(Group = group)
  }

  summary_table_agg
}


#' Format a Summary Table for Printing
#'
#' @description
#' Internal helper function to prepare a summary table for output in `print()`
#' or `summary()` methods.
#'
#' - Columns containing "percent" in their name are formatted using `insight::format_percent()`.
#' - All column names are converted to Title Case using `snakecase::to_title_case()`.
#'
#' @param summary_table A `data.frame` or `tibble` to be formatted.
#' @param table_title String
#'
#' @return A formatted `tibble`.
#'
#' @noRd
.format_summary_table <- function(summary_table, table_title = NULL) {
  summary_table |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("percent"), \(a) insight::format_percent(a))
    ) |>
    dplyr::rename_with(snakecase::to_title_case) |>
    insight::export_table(title = table_title)
}

#' Print a Formatted Model Information Table as String
#'
#' @description
#' An internal helper that takes a named list of model attributes and prints them
#' to the console as a key-value table. Values are left-aligned for a clean,
#' table-like appearance.
#'
#' @param model_info A named `list` where names are the descriptions (keys) and
#'   elements are the values to be displayed.
#'
#' @return Invisibly `NULL`. This function is called for its side effect of
#'   printing to the console.
#'
#' @noRd
.format_model_info_string <- function(model_info) {
  # Add the colon to the names first
  names_with_colon <- paste0(names(model_info), ":")

  # Find the width of the longest name-with-colon
  width <- max(nchar(names_with_colon))

  # The format string now pads the combined "name:" string
  format_string <- paste0("%-", width, "s %s")

  formatted_lines <- purrr::map2_chr(
    names_with_colon,
    model_info,
    \(name, value) {
      sprintf(format_string, name, value)
    }
  )

  paste(formatted_lines, collapse = "\n")
}

#' Print Strings with a Standard Heading and Separation
#'
#' @description
#' A helper function that wraps `cat()` to produce a standardized output. It
#' first prints a "Clinical Significance Results" heading and then prints any
#' number of subsequent strings, each separated by a blank line.
#'
#' @param ... Character strings to be printed. Each argument will be separated
#'   by a blank line.
#'
#' @return Invisibly `NULL`. This function is called for its side effect of
#'   printing to the console.
#'
#' @noRd
.print_strings <- function(...) {
  heading <- "\n---- Clinical Significance Results ----"

  cat(heading, ..., sep = "\n\n")
}

#' Check if a data frame has a column named group
#'
#' @param data A data frame
#'
#' @return Boolean
#'
#' @noRd
.has_group <- function(data) {
  "group" %in% names(data)
}


#' Check if an Object is of Class cs_analysis
#'
#' @param data A data frame
#'
#' @return Boolean
#'
#' @noRd
.check_class <- function(x) {
  if (!inherits(x, "cs_analysis")) {
    cli::cli_abort(
      "The supplied object must be of class {.code cs_analysis} and not {.code {class(x)}}."
    )
  }
}


#' Calculate improvement or deterioration based on RCI scores
#'
#' This functions expects at least a preprocessed data frame with the column
#' `rci`
#'
#' @param data A preprocessed data frame
#' @param rci_cutoff A multiplier for the RCI that is used to calculate the
#'   cutoff
#' @param direction Which direction is better? 1 = higher, -1 = lower
#'
#' @return A tibble with columns `id`, `rci`, `improved`, `deteriorated`, and
#'   `unchanged`
#'
#' @noRd
.calc_improvement <- function(data, rci_cutoff = 1.96, direction = 1) {
  data |>
    dplyr::mutate(
      improved = ifelse(direction * rci > rci_cutoff, TRUE, FALSE),
      deteriorated = ifelse(direction * rci < -rci_cutoff, TRUE, FALSE),
      unchanged = !improved & !deteriorated
    ) |>
    dplyr::select(id, rci, improved:unchanged)
}


#' Calculate standard error of measurement
#'
#' @param sd_pre Pre measurement standard deviation
#' @param reliability Instrument's reliability
#'
#' @return A number
#'
#' @noRd
.calc_se_measurement <- function(sd_pre, reliability) {
  sd_pre * sqrt(1 - reliability)
}


#' Calculate standard error of differences
#'
#' @param se_measurement A standard error of measurements
#'
#' @return A number
#'
#' @noRd
.calc_s_diff <- function(se_measurement) {
  sqrt(2 * se_measurement^2)
}


#' Calculate standard error of predictions
#'
#' @param sd_pre Pre measurement standard deviation
#' @param reliability Instrument's reliability
#'
#' @return A number
#'
#' @noRd
.calc_se_prediction <- function(sd_pre, reliability) {
  sd_pre * sqrt(1 - reliability^2)
}


#' Calculate reliabilities like Hagemann & Arrindell
#'
#' @param sd A standard deviation. This may depend on the formula
#' @param se_measurment Standard error of measurement for a given instrument
#'
#' @return A number
#'
#' @noRd
.calc_reliability_ha <- function(sd, se_measurment) {
  (sd^2 - se_measurment^2) / sd^2
}
