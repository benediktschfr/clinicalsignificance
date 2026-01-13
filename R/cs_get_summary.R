#' Get A Summary Table From A cs_analysis Object
#'
#' Retrieve the summary table in a tidy tibble format. This is especially useful
#' to plot the results or conduct sensitivity analyses.
#'
#' @param x An object of class `cs_analysis`
#' @param ... Additional arguments passed to the respective method
#'
#'
#' @return A tibble with clinical significance categories
#'
#' @family get
#' @rdname summary_table
#' @export
#'
#' @references
#' - Hageman, W. J., & Arrindell, W. A. (1999). Establishing clinically significant change: increment of precision and the distinction between individual and group level analysis. Behaviour Research and Therapy, 37(12), 1169–1193. https://doi.org/10.1016/S0005-7967(99)00032-7
#'
#' @examples
#' anchor_results <- claus_2020 |>
#'   cs_anchor(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     mid_improvement = 8
#'   )
#'
#' cs_get_summary(anchor_results)
#'
#'
#' # Get summary table for a group level analysis
#' anchor_results_grouped <- claus_2020 |>
#'   cs_anchor_group(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     mid_improvement = 8,
#'   )
#'
#' cs_get_summary(anchor_results_grouped)
#'
#'
#' # Get group-wise summary table for the Hageman & Arrindell method
#' combined_results <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     reliability = 0.80,
#'     rci_method = "HA"
#'   )
#'
#' cs_get_summary(combined_results)
#' cs_get_summary(combined_results, which = "group")
cs_get_summary <- function(x, ...) {
  UseMethod("cs_get_summary")
}


#' Default Method To Get A Summary Table From A cs_analysis Object
#'
#' @param which Which level of summary table to return. This is only necessary
#'   for method `"HA"` since two summary tables are reported. Available are
#'   - `individual`, the default
#'   - `group`, group level results according to Hageman & Arrindell (1999)
#'
#' @rdname summary_table
#' @export
cs_get_summary.default <- function(x, which = c("individual", "group"), ...) {
  which_table <- rlang::arg_match(which)
  cs_method <- x[["method"]]

  # Needed to avoid an error
  if (is.null(cs_method)) {
    cs_method <- "Not HA"
  }

  if (cs_method != "HA" & which_table == "group") {
    abort("Group level results can only be exported for method HA.")
  }

  if (cs_method == "HA") {
    if (which_table != "group") {
      x[["summary_table"]][[1]]
    } else {
      x[["summary_table"]][[2]]
    }
  }
  if (cs_method != "HA" & inherits(x, "cs_combined")) {
    x[["summary_table"]][["individual_level_summary"]]
  } else {
    x[["summary_table"]]
  }
}


#' Method Get Summary Table From A cs_analysis Object With Within Group Effects
#'
#' @rdname summary_table
#' @export
cs_get_summary.cs_anchor_group_within <- function(x, ...) {
  x[["anchor_results"]]
}


#' Method Get Summary Table From A cs_analysis Object With Between Group Effects
#'
#' @rdname summary_table
#' @export
cs_get_summary.cs_anchor_group_between <- function(x, ...) {
  x[["anchor_results"]]
}
