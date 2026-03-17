#' Groupwise Anchor-Based Analysis of Clinical Significance
#'
#' @inheritParams cs_distribution
#' @param mid_improvement Numeric, change that indicates a clinically
#'   significant improvement
#' @param mid_deterioration Numeric, change that indicates a clinically
#'   significant deterioration (optional). If `mid_deterioration` is not
#'   provided, it will be assumed to be equal to `mid_improvement`
#' @param target String, whether an individual or group analysis should be
#'   calculated. Available are
#'   - `"individual"` (the default) for which every individual participant is
#'   evaluated
#'   - `"group"` for which only the group wise effect is evaluated
#' @param effect String, if `target = "group"`, specify which effect should be
#'   calculated. Available are
#'   - `"within"` (the default), which yields the mean pre-post intervention
#'   difference with associated confidence intervals
#'   - `"between"`, which estimates the group wise mean difference and
#'   confidence intervals between two or more groups specified with the `group`
#'   argument at the specified measurement supplied with the `post`- argument
#'   The reference group may be supplied with `reference_group`
#' @param bayesian Logical, only relevant if `target = "group"`. Indicates if a
#'   Bayesian estimate (i.e., the median) of group differences with a credible
#'   interval should be calculated (if set to `TRUE`, the default) or a
#'   frequentist mean difference with confidence interval (if set to `FALSE`)
#' @param prior_scale String or numeric, can be adjusted to change the Bayesian
#'   prior distribution. See the documentation for `rscale` in
#'   [BayesFactor::ttestBF()] for details.
#' @param reference_group Specify the reference group to which all subsequent
#'   groups are compared against if `target = "group"` and `effect = "within"`
#'   (optional). Otherwise, the first distinct group is chosen based on
#'   alphabetical, numerical or factor ordering.
#' @param ci_level Numeric, define the credible or confidence interval level.
#'   The default is 0.95 for a 95%-CI.
#'
#' @references Hespanhol, L., Vallio, C. S., Costa, L. M., & Saragiotto, B. T.
#'   (2019). Understanding and interpreting confidence and credible intervals
#'   around effect estimates. Brazilian Journal of Physical Therapy, 23(4),
#'   290–301. https://doi.org/10.1016/j.bjpt.2018.12.006
#'
#' @family main
#'
#' @return An S3 object of class `cs_analysis` and `cs_anchor`
#' @export
cs_anchor_group <- function(data, ...) {
  UseMethod("cs_anchor_group")
}

#' @export
#' @describeIn cs_anchor_group Default method for data frames
cs_anchor_group.default <- function(
  data,
  id,
  time,
  outcome,
  group = NULL,
  pre = NULL,
  post = NULL,
  mid_improvement = NULL,
  mid_deterioration = NULL,
  better_is = c("lower", "higher"),
  effect = c("within", "between"),
  bayesian = TRUE,
  prior_scale = "medium",
  reference_group = NULL,
  ci_level = 0.95,
  ...
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

  checkmate::assert_numeric(
    mid_improvement,
    lower = 0,
    finite = TRUE,
    min.len = 1
  )
  checkmate::assert_number(ci_level, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_numeric(
    mid_deterioration,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE,
    min.len = 1
  )

  checkmate::assert_data_frame(data)
  checkmate::assert_logical(bayesian, len = 1)

  # NSE-sichere Logik-Checks für Design
  if (cs_effect == "between") {
    if (
      rlang::quo_is_missing(rlang::enquo(group)) ||
        rlang::quo_is_null(rlang::enquo(group))
    ) {
      cli::cli_abort(
        "Argument {.arg group} is missing. Necessary for between-group calculations."
      )
    }
    if (
      rlang::quo_is_missing(rlang::enquo(post)) ||
        rlang::quo_is_null(rlang::enquo(post))
    ) {
      cli::cli_abort(
        "Argument {.arg post} is missing. Please specify the measurement timepoint for group comparison."
      )
    }
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

  # Get direction and outcome safely
  direction <- if (better_is[1] == "lower") -1 else 1
  outcome_name <- deparse(substitute(outcome))

  is_sensitivity <- length(mid_improvement) > 1 || length(mid_deterioration) > 1

  # >>> SENSITIVITÄTSANALYSE ODER STANDARD <<<
  if (is_sensitivity) {
    # NULL in NA_real_ umwandeln, damit expand_grid funktioniert
    mid_det_vec <- if (is.null(mid_deterioration)) {
      NA_real_
    } else {
      mid_deterioration
    }

    # Erstelle ein Grid aus allen Kombinationen
    results_list <- tidyr::expand_grid(
      mid_improvement = mid_improvement,
      mid_deterioration = mid_det_vec
    ) |>
      dplyr::mutate(
        # NA Werte (entstanden durch NULL) durch die symmetrischen improvement Werte ersetzen
        mid_deterioration = dplyr::if_else(
          is.na(mid_deterioration),
          mid_improvement,
          mid_deterioration
        )
      ) |>
      dplyr::mutate(
        models = purrr::pmap(
          list(mid_improvement, mid_deterioration),
          function(imp, det) {
            .core_anchor_group(
              datasets = datasets,
              mid_improvement = imp,
              mid_deterioration = det,
              reference_group = reference_group,
              post = post,
              direction = direction,
              bayesian = bayesian,
              prior_scale = prior_scale,
              ci_level = ci_level,
              cs_effect = cs_effect,
              outcome_name = outcome_name,
              prepend_classes = prepend_classes
            )
          }
        )
      )

    combined_results <- results_list |>
      dplyr::mutate(res = purrr::map(models, "anchor_results")) |>
      dplyr::select(-models) |>
      tidyr::unnest(res)

    n_obs <- results_list |>
      purrr::pluck("models", 1) |>
      purrr::pluck("n_obs")

    output <- list(
      anchor_results = combined_results,
      mid_improvement = mid_improvement,
      mid_deterioration = mid_deterioration,
      direction = direction,
      bayesian = bayesian,
      prior_scale = prior_scale,
      n_obs = n_obs,
      outcome = outcome_name
    )

    sensitivity_class <- paste(
      "cs",
      "anchor",
      "group",
      cs_effect,
      "sensitivity",
      sep = "_"
    )
    class(output) <- c("cs_analysis", sensitivity_class, "list")
    return(output)
  } else {
    if (is.null(mid_deterioration)) {
      mid_deterioration <- mid_improvement
    }

    return(.core_anchor_group(
      datasets = datasets,
      mid_improvement = mid_improvement,
      mid_deterioration = mid_deterioration,
      reference_group = reference_group,
      post = post,
      direction = direction,
      bayesian = bayesian,
      prior_scale = prior_scale,
      ci_level = ci_level,
      cs_effect = cs_effect,
      outcome_name = outcome_name,
      prepend_classes = prepend_classes
    ))
  }
}

.core_anchor_group <- function(
  datasets,
  mid_improvement,
  mid_deterioration,
  reference_group,
  post,
  direction,
  bayesian,
  prior_scale,
  ci_level,
  cs_effect,
  outcome_name,
  prepend_classes
) {
  # Count participants appropriately depending on design
  if (cs_effect == "within") {
    n_obs <- list(
      n_original = nrow(datasets[["wide"]]),
      n_used = nrow(datasets[["data"]])
    )
  } else {
    n_obs <- list(
      n_original = nrow(datasets),
      n_used = nrow(datasets)
    )
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

  if (cs_effect == "within") {
    class(datasets) <- setdiff(class(datasets), prepend_classes)
  } else {
    # SAFELY convert to a generic tibble
    datasets <- tibble::as_tibble(datasets)
  }

  # Put everything into a list
  output <- list(
    datasets = datasets,
    anchor_results = anchor_results,
    outcome = outcome_name,
    n_obs = n_obs,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    direction = direction,
    bayesian = bayesian,
    prior_scale = prior_scale,
    summary_table = NULL
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
  direction <- if (x[["direction"]] == -1) "Lower" else "Higher"

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (within groups)",
      "MID Improvement" = mid_improvement,
      "Better is" = direction
    )
  )

  .print_strings(model_info, summary_table)
}

#' Print Method for the Anchor-Based Approach Sensitivity for Groups (Within)
#'
#' @param x An object of class `cs_anchor_group_within_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
print.cs_anchor_group_within_sensitivity <- function(x, ...) {
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename(
      "MID Improvement" = "mid_improvement",
      "MID Deterioration" = "mid_deterioration",
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
  direction <- if (x[["direction"]] == -1) "Lower" else "Higher"

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (within groups) Sensitivity",
      "Better is" = direction
    )
  )

  .print_strings(model_info, summary_table)
}


#' Print Method for the Anchor-Based Approach for Groups (Between)
#'
#' @param x An object of class `cs_anchor_group_between`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
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
  direction <- if (x[["direction"]] == -1) "Lower" else "Higher"

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (between groups)",
      "MID (Improvement)" = mid_improvement,
      "Better is" = direction
    )
  )

  .print_strings(model_info, summary_table)
}

#' Print Method for the Anchor-Based Approach Sensitivity for Groups (Between)
#'
#' @param x An object of class `cs_anchor_group_between_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
print.cs_anchor_group_between_sensitivity <- function(x, ...) {
  summary_table_formatted <- x[["anchor_results"]] |>
    dplyr::rename(
      "MID Improvement" = "mid_improvement",
      "MID Deterioration" = "mid_deterioration",
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
  direction <- if (x[["direction"]] == -1) "Lower" else "Higher"

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (between groups) Sensitivity",
      "Better is" = direction
    )
  )

  .print_strings(model_info, summary_table)
}

#' Summary Method for the Anchor-Based Approach for Groups (Within)
#'
#' @param object An object of class `cs_anchor_group_within`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
summary.cs_anchor_group_within <- function(object, ...) {
  summary_table_formatted <- object[["anchor_results"]] |>
    dplyr::rename(
      "Difference" = "difference",
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

  summary_table <- .format_summary_table(summary_table_formatted)

  mid_improvement <- object[["mid_improvement"]]
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  direction <- if (object[["direction"]] == -1) "Lower" else "Higher"
  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (within groups)",
      "MID Improvement" = mid_improvement,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent used" = insight::format_percent(n_used / n_original),
      "Better is" = direction,
      Outcome = outcome
    )
  )

  .print_strings(model_info, summary_table)
}

#' Summary Method for the Anchor-Based Approach Sensitivity for Groups (Within)
#'
#' @param object An object of class `cs_anchor_group_within_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
summary.cs_anchor_group_within_sensitivity <- function(object, ...) {
  summary_table_formatted <- object[["anchor_results"]] |>
    dplyr::rename(
      "MID Improvement" = "mid_improvement",
      "MID Deterioration" = "mid_deterioration",
      "Difference" = "difference",
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

  if (!object[["bayesian"]]) {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Mean Difference" = "Difference"
    )
  } else {
    summary_table_formatted <- dplyr::rename(
      summary_table_formatted,
      "Median Difference" = "Difference"
    )
  }

  summary_table <- .format_summary_table(summary_table_formatted)

  format_range <- function(x) {
    if (is.null(x)) {
      return("---")
    }
    if (length(x) == 1) {
      return(as.character(round(x, 2)))
    }
    paste0(round(min(x), 2), " to ", round(max(x), 2))
  }

  mid_improvement <- format_range(object[["mid_improvement"]])
  mid_deterioration <- format_range(object[["mid_deterioration"]])
  if (is.null(object[["mid_deterioration"]])) {
    mid_deterioration <- paste0(mid_improvement, " (symmetric)")
  }

  direction <- if (object[["direction"]] == -1) "Lower" else "Higher"
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (within groups) Sensitivity",
      "Range MID Improvement" = mid_improvement,
      "Range MID Deterioration" = mid_deterioration,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent used" = insight::format_percent(n_used / n_original),
      "Better is" = direction,
      Outcome = outcome
    )
  )

  .print_strings(model_info, summary_table)
}


#' Summary Method for the Anchor-Based Approach for Groups (Between)
#'
#' @param object An object of class `cs_anchor_group_between`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
summary.cs_anchor_group_between <- function(object, ...) {
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
  direction <- if (object[["direction"]] == -1) "Lower" else "Higher"
  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (between groups)",
      "MID (Improvement)" = mid_improvement,
      "Better is" = direction,
      Outcome = outcome
    )
  )

  .print_strings(model_info, summary_table)
}

#' Summary Method for the Anchor-Based Approach Sensitivity for Groups (Between)
#'
#' @param object An object of class `cs_anchor_group_between_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
summary.cs_anchor_group_between_sensitivity <- function(object, ...) {
  summary_table_formatted <- object[["anchor_results"]] |>
    dplyr::rename(
      "MID Improvement" = "mid_improvement",
      "MID Deterioration" = "mid_deterioration",
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

  format_range <- function(x) {
    if (is.null(x)) {
      return("---")
    }
    if (length(x) == 1) {
      return(as.character(round(x, 2)))
    }
    paste0(round(min(x), 2), " to ", round(max(x), 2))
  }

  mid_improvement <- format_range(object[["mid_improvement"]])
  mid_deterioration <- format_range(object[["mid_deterioration"]])
  if (is.null(object[["mid_deterioration"]])) {
    mid_deterioration <- paste0(mid_improvement, " (symmetric)")
  }

  direction <- if (object[["direction"]] == -1) "Lower" else "Higher"
  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based (between groups) Sensitivity",
      "Range MID Improvement" = mid_improvement,
      "Range MID Deterioration" = mid_deterioration,
      "Better is" = direction,
      Outcome = outcome
    )
  )

  .print_strings(model_info, summary_table)
}
