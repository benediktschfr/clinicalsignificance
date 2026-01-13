#' Anchor-Based Analysis of Clinical Significance for Groups
#'
#' @description `cs_anchor_group()` evaluates clinical significance at the group
#'   level by comparing the magnitude of treatment effects against a Minimally
#'   Important Difference (MID).
#'
#'   This function supports two study designs:
#'   * **Within-Group**: Compares the change from pre- to post-intervention
#'       against the MID.
#'   * **Between-Group**: Compares the difference between two groups (e.g.,
#'       Intervention vs. Control) at the post-measurement against the MID.
#'
#'   The analysis can be performed using either a Bayesian (default) or a
#'   Frequentist framework.
#'
#' @section Computational details:
#'   For group-level analyses, the function calculates the effect size (mean or
#'   median difference) and its associated uncertainty interval (Confidence
#'   Interval, CI, or Credible Interval, CrI). The clinical significance is
#'   determined by the position of this interval relative to the MID threshold.
#'
#'   **Categories of Clinical Significance:**
#'   Based on the relation between the CI/CrI and the MID, the effect is
#'   classified into one of 5 categories:
#'   - **Statistically not significant**: The CI contains 0.
#'   - **Statistically significant but not clinically relevant**: The CI does
#'   not contain 0, but the entire interval is below the MID threshold.
#'   - **Not significantly less than the threshold**: The point estimate is
#'   below the MID, but the CI overlaps with the MID threshold.
#'   - **Probably clinically significant effect**: The point estimate exceeds
#'   the MID, but the lower limit of the CI is still below the MID.
#'   - **Large clinically significant effect**: The entire CI exceeds the MID
#'   threshold.
#'
#'   **Bayesian vs. Frequentist:**
#'   - If `bayesian = TRUE` (default), the median difference and a Credible
#'   Interval are calculated. The prior can be adjusted using `prior_scale`.
#'   - If `bayesian = FALSE`, a frequentist mean difference and Confidence
#'   Interval (t-distribution) are used.
#'
#' @inheritSection cs_distribution Data preparation
#'
#' @inheritParams cs_distribution
#' @param mid_improvement Numeric, change that indicates a clinically
#'   significant improvement.
#' @param mid_deterioration Numeric, change that indicates a clinically
#'   significant deterioration (optional). Mostly relevant for individual
#'   analyses, but kept for consistency.
#' @param effect String, specifies the design of the group comparison. Available are:
#'   - `"within"` (the default): Calculates the mean/median pre-post change
#'   within a group.
#'   - `"between"`: Calculates the mean/median difference between two groups at
#'   the `post` measurement. Requires the `group` argument.
#' @param bayesian Logical. If `TRUE` (default), calculates a Bayesian estimate
#'   (median) with a Credible Interval. If `FALSE`, calculates a Frequentist
#'   mean difference with a Confidence Interval.
#' @param prior_scale String or numeric, scales the Cauchy prior distribution
#'   for the Bayesian analysis. See `rscale` in [BayesFactor::ttestBF()] for
#'   details. Default is `"medium"`.
#' @param reference_group String, specifies the reference group for
#'   between-group comparisons (e.g., "Control" or "Placebo"). If not provided,
#'   the first level of the grouping factor is used.
#' @param ci_level Numeric, the confidence or credible interval level (e.g.,
#'   0.95 for 95%).
#'
#' @references Hespanhol, L., Vallio, C. S., Costa, L. M., & Saragiotto, B. T.
#'   (2019). Understanding and interpreting confidence and credible intervals
#'   around effect estimates. Brazilian Journal of Physical Therapy, 23(4),
#'   290–301. https://doi.org/10.1016/j.bjpt.2018.12.006
#'
#' @family main
#'
#' @return An S3 object of class `cs_anchor_group_within` or
#'   `cs_anchor_group_between`.
#' @export
#'
#' @examples
#' # 1. Within-Group Analysis (Bayesian Default)
#' #    Does the intervention group improve more than the MID of 8?
#' cs_results_within <- antidepressants |>
#'   cs_anchor_group(
#'     patient,
#'     measurement,
#'     mom_di,
#'     mid_improvement = 8,
#'     pre = "Before",
#'     post = "After"
#'   )
#'
#' summary(cs_results_within)
#' plot(cs_results_within)
#'
#'
#' # 2. Between-Group Analysis
#' #    Compare Intervention vs. Control at post-measurement
#' cs_results_between <- antidepressants |>
#'   cs_anchor_group(
#'     patient,
#'     measurement,
#'     mom_di,
#'     group = condition,
#'     effect = "between",
#'     post = "After",
#'     mid_improvement = 8
#'   )
#'
#' summary(cs_results_between)
#' plot(cs_results_between)
#'
#'
#' # 3. Frequentist Analysis (Standard t-test based CI)
#' cs_results_freq <- antidepressants |>
#'   cs_anchor_group(
#'     patient,
#'     measurement,
#'     mom_di,
#'     mid_improvement = 8,
#'     pre = "Before",
#'     post = "After",
#'     bayesian = FALSE
#'   )
cs_anchor_group <- function(
  data,
  id,
  time,
  outcome,
  group,
  pre,
  post,
  mid_improvement = NULL,
  mid_deterioration = NULL,
  better_is = c("lower", "higher"),
  effect = c("within", "between"),
  bayesian = TRUE,
  prior_scale = "medium",
  reference_group = NULL,
  ci_level = 0.95
) {
  cs_effect <- rlang::arg_match(effect)
  rlang::arg_match(better_is)

  if (missing(id)) {
    cli::cli_abort(
      "Argument {.arg id} is missing. A column containing patient-specific IDs must be supplied."
    )
  }
  if (missing(time)) {
    cli::cli_abort(
      "Argument {.arg time} is missing. A column identifying the individual measurements must be supplied."
    )
  }
  if (missing(outcome)) {
    cli::cli_abort(
      "Argument {.arg outcome} is missing. A column containing the outcome must be supplied."
    )
  }

  checkmate::assert_number(mid_improvement, lower = 0, finite = TRUE)
  checkmate::assert_number(ci_level, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_number(
    mid_deterioration,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )

  checkmate::assert_data_frame(data)
  checkmate::assert_logical(bayesian, len = 1)

  # 4. Logik-Checks für Design
  if (cs_effect == "between") {
    if (missing(group)) {
      cli::cli_abort(
        "Argument {.arg group} is missing. Necessary for between-group calculations."
      )
    }
    if (is.null(post)) {
      cli::cli_abort(
        "Argument {.arg post} is missing. Please specify the measurement timepoint for group comparison."
      )
    }
  }

  if (is.null(mid_deterioration)) {
    mid_deterioration <- mid_improvement
  }

  # Prepare the data
  if (cs_effect == "within") {
    datasets <- .prep_data(
      data = data,
      id = {{ id }},
      time = {{ time }},
      outcome = {{ outcome }},
      group = {{ group }},
      pre = {{ pre }},
      post = {{ post }}
    )
  } else {
    datasets <- data |>
      dplyr::select(
        id = {{ id }},
        time = {{ time }},
        outcome = {{ outcome }},
        group = {{ group }}
      )
  }

  # Prepend a class to enable method dispatch for RCI calculation
  prepend_classes <- c(
    "cs_anchor",
    paste("cs", "anchor", "group", cs_effect, sep = "_")
  )
  class(datasets) <- c(prepend_classes, class(datasets))

  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )

  # Get the direction of a beneficial intervention effect
  if (rlang::arg_match(better_is) == "lower") {
    direction <- -1
  } else {
    direction <- 1
  }

  # Check each participant's or group change relative to MID
  anchor_results <- calc_anchor(
    data = datasets,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    reference_group = reference_group,
    post = post,
    direction = direction,
    bayesian = bayesian,
    prior_scale = prior_scale,
    ci_level = ci_level
  )

  # Create the summary table for printing and exporting
  summary_table <- NULL
  if (cs_effect == "within") {
    class(datasets) <- "list"
  } else {
    class(datasets) <- c("tbl_df", "tbl", "data.frame")
  }

  # Put everything into a list
  output <- list(
    datasets = datasets,
    anchor_results = anchor_results,
    outcome = deparse(substitute(outcome)),
    n_obs = n_obs,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    direction = direction,
    bayesian = bayesian,
    prior_scale = prior_scale,
    summary_table = summary_table
  )

  # Return output
  class(output) <- c("cs_analysis", prepend_classes, class(output))
  output
}

#' Print Method for the Anchor-Based Approach for Groups (Within)
#'
#' @param x An object of class `cs_anchor_group_within`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_anchor_group(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     mid_improvement = 7,
#'   )
#'
#' cs_results
print.cs_anchor_group_within <- function(x, ...) {
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename(
      "CI-Level" = "ci",
      "[Lower" = "lower",
      "Upper]" = "upper",
      "Category" = "category"
    )

  if (.has_group(summary_table_formatted)) {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Group" = "group"
    )
  }

  if (!x[["bayesian"]]) {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Mean Difference" = "difference"
    )
  } else {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Median Difference" = "difference"
    )
  }

  summary_table <- .format_summary_table(summary_table_formatted)

  mid_improvement <- x[["mid_improvement"]]

  if (x[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (within groups)",
      "MID Improvement" = mid_improvement,
      "Better is" = direction
    )
  )

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}


#' Print Method for the Anchor-Based Approach for Groups (Between)
#'
#' @param x An object of class `cs_anchor_group_between`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_anchor_group(
#'     id,
#'     time,
#'     bdi,
#'     post = 4,
#'     mid_improvement = 7,
#'     group = treatment,
#'     effect = "between"
#'   )
#'
#' cs_results
print.cs_anchor_group_between <- function(x, ...) {
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename(
      "Group 1" = "reference",
      "Group 2" = "comparison",
      "CI-Level" = "ci",
      "[Lower" = "lower",
      "Upper]" = "upper",
      "Category" = "category",
      "n (1)" = "n_reference",
      "n (2)" = "n_comparison"
    )

  if (!x[["bayesian"]]) {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Mean Difference" = "difference"
    )
  } else {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Median Difference" = "difference"
    )
  }

  summary_table <- .format_summary_table(summary_table_formatted)

  mid_improvement <- x[["mid_improvement"]]

  if (x[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (between groups)",
      "MID (Improvement)" = mid_improvement,
      "Better is" = direction
    )
  )

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}


#' Summary Method for the Anchor-Based Approach for Groups (Within)
#'
#' @param object An object of class `cs_anchor_group_within`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_anchor_group(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     mid_improvement = 7,
#'   )
#'
#' summary(cs_results)
summary.cs_anchor_group_within <- function(object, ...) {
  summary_table_formatted <- object[["anchor_results"]] |>
    dplyr::rename(
      "Difference" = "difference",
      "CI-Level" = "ci",
      "[Lower" = "lower",
      "Upper]" = "upper",
      "Category" = "category"
    )

  # Get necessary information from object
  if (.has_group(summary_table_formatted)) {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Group" = "group"
    )
  }

  summary_table <- .format_summary_table(summary_table_formatted)

  mid_improvement <- object[["mid_improvement"]]
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  pct <- round(n_used / n_original, digits = 3) * 100
  direction <- object[["direction"]]

  if (object[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (between groups)",
      "MID Improvement" = mid_improvement,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent used" = insight::format_percent(n_used / n_original),
      "Better is" = direction,
      Outcome = outcome
    )
  )

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}


#' Summary Method for the Anchor-Based Approach for Groups (Between)
#'
#' @param object An object of class `cs_anchor_group_between`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- antidepressants |>
#'   cs_anchor_group(
#'     patient,
#'     measurement,
#'     post = "After",
#'     mom_di,
#'     mid_improvement = 7,
#'     effect = "between",
#'     group = condition
#'   )
#'
#' summary(cs_results)
summary.cs_anchor_group_between <- function(object, ...) {
  # Get necessary information from object
  summary_table_formatted <- object[["anchor_results"]] |>
    dplyr::rename(
      "Group 1" = "reference",
      "Group 2" = "comparison",
      "CI-Level" = "ci",
      "[Lower" = "lower",
      "Upper]" = "upper",
      "Category" = "category",
      "n (1)" = "n_reference",
      "n (2)" = "n_comparison"
    )

  if (!object[["bayesian"]]) {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Mean Difference" = "difference"
    )
  } else {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Median Difference" = "difference"
    )
  }

  summary_table <- .format_summary_table(summary_table_formatted)

  mid_improvement <- object[["mid_improvement"]]

  if (object[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (between groups)",
      "MID (Improvement)" = mid_improvement,
      "Better is" = direction,
      Outcome = outcome
    )
  )

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}
