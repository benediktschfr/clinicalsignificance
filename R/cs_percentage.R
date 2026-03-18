#' Percentage-Change Analysis of Clinical Significance
#'
#' @description `cs_percentage()` can be used to determine the clinical
#'   significance of intervention studies employing the percentage-change
#'   approach. For this, each individuals relative change compared to the pre
#'   intervention measurement and if this change exceeds a predefined change in
#'   percent points, this change is then deemed clinically significant.
#'
#' @section Computational details: Each participants change is calculated and
#'   then divided by the pre intervention score to estimate the individual's
#'   percent change. A percent change for an improvement as well as a
#'   deterioration can be provided separately and if `pct_deterioration` is not
#'   set, it will be assumed to be the same as `pct_improvement`.
#'
#' @section Categories: Each individual's change may then be categorized into
#'   one of the following three categories:
#'   - Improved, the change is greater than the predefined percent change in
#'   the beneficial direction
#'   - Unchanged, the change is within the predefined percent change
#'   - Deteriorated, the change is greater than the predefined percent change,
#'   but in the disadvantageous direction
#'
#'
#' @inheritSection cs_distribution Data preparation
#'
#'
#' @inheritParams cs_distribution
#' @param pct_improvement Numeric, percent change that indicates a clinically
#'   significant improvement
#' @param pct_deterioration Numeric, percent change that indicates a clinically
#'   significant deterioration (optional). If this is not set,
#'   `pct_deterioration` will be assumed to be equal to `pct_improvement`
#' @param ... Additional arguments passed to methods.
#'
#' @family main
#'
#'
#' @return An S3 object of class `cs_analysis` and `cs_percentage`
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = 0.3
#'   )
#'
#' cs_results
#' summary(cs_results)
#' plot(cs_results)
#'
#'
#' # You can set different thresholds for improvement and deterioration
#' cs_results_2 <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = 0.3,
#'     pct_deterioration = 0.2
#'   )
#'
#' cs_results_2
#' summary(cs_results_2)
#' plot(cs_results_2)
#'
#'
#' # You can group the analysis by providing a group column from the data
#' cs_results_grouped <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = 0.3,
#'     group = treatment
#'   )
#'
#' cs_results_grouped
#' summary(cs_results_grouped)
#' plot(cs_results_grouped)
#'
#'
#' # The analyses can be performed for positive outcomes as well, i.e., outcomes
#' # for which a higher value is beneficial
#' cs_results_who <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     who,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = 0.3,
#'     better_is = "higher"
#'   )
#'
#' cs_results_who
#' summary(cs_results_who)
#' plot(cs_results_who)
#' plot(cs_results_who, show = category)
cs_percentage <- function(data, ...) {
  UseMethod("cs_percentage")
}

#' @export
#' @describeIn cs_percentage Default method for data frames
cs_percentage.default <- function(
  data,
  id,
  time,
  outcome,
  group = NULL,
  pre = NULL,
  post = NULL,
  pct_improvement = NULL,
  pct_deterioration = NULL,
  better_is = c("lower", "higher"),
  ...
) {
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

  if (is.null(pct_improvement)) {
    cli::cli_abort(
      "Argument {.arg pct_improvement} is missing. A percentage change (between 0 and 1) must be supplied."
    )
  }

  if (is.numeric(pct_improvement) && any(pct_improvement > 1)) {
    first_invalid <- pct_improvement[pct_improvement > 1][1]
    cli::cli_abort(c(
      "{.arg pct_improvement} must be a probability between 0 and 1.",
      "i" = "Did you mean {.val {first_invalid / 100}} ({first_invalid}%)?"
    ))
  }

  checkmate::assert_numeric(
    pct_improvement,
    lower = 0,
    upper = 1,
    finite = TRUE,
    min.len = 1
  )

  checkmate::assert_numeric(
    pct_deterioration,
    lower = 0,
    upper = 1,
    finite = TRUE,
    null.ok = TRUE,
    min.len = 1
  )

  checkmate::assert_data_frame(data)

  # Prepare the data
  datasets <- .prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    group = {{ group }},
    pre = {{ pre }},
    post = {{ post }}
  )

  # Prepend a class
  class(datasets) <- c("cs_percentage", class(datasets))

  # Get direction
  direction <- if (better_is[1] == "lower") -1 else 1
  outcome_name <- deparse(substitute(outcome))

  is_sensitivity <- length(pct_improvement) > 1 || length(pct_deterioration) > 1

  # >>> SENSITIVITÄTSANALYSE ODER STANDARD <<<
  if (is_sensitivity) {
    # NULL in NA_real_ umwandeln, damit expand_grid funktioniert
    pct_det_vec <- if (is.null(pct_deterioration)) {
      NA_real_
    } else {
      pct_deterioration
    }

    # Erstelle ein Grid aus allen Kombinationen
    results_list <- tidyr::expand_grid(
      pct_improvement = pct_improvement,
      pct_deterioration = pct_det_vec
    ) |>
      dplyr::mutate(
        # Symmetrische Werte auffüllen, falls pct_deterioration = NA
        pct_deterioration = dplyr::if_else(
          is.na(pct_deterioration),
          pct_improvement,
          pct_deterioration
        )
      ) |>
      dplyr::mutate(
        models = purrr::pmap(
          list(pct_improvement, pct_deterioration),
          function(p_imp, p_det) {
            .core_percentage(
              datasets = datasets,
              pct_improvement = p_imp,
              pct_deterioration = p_det,
              direction = direction,
              outcome = outcome_name
            )
          }
        )
      )

    combined_tables <- results_list |>
      dplyr::mutate(tables = purrr::map(models, cs_get_summary)) |>
      dplyr::select(-models) |>
      tidyr::unnest(tables)

    n_obs <- results_list |>
      purrr::pluck("models", 1) |>
      purrr::pluck("n_obs")

    output <- list(
      summary_table = combined_tables,
      pct_improvement = pct_improvement,
      pct_deterioration = pct_deterioration,
      better_is = better_is[[1]],
      n_obs = n_obs,
      direction = direction,
      outcome = outcome_name
    )
    class(output) <- c("cs_analysis", "cs_percentage_sensitivity", "list")
    return(output)
  } else {
    if (is.null(pct_deterioration)) {
      pct_deterioration <- pct_improvement
    }

    return(.core_percentage(
      datasets = datasets,
      pct_improvement = pct_improvement,
      pct_deterioration = pct_deterioration,
      direction = direction,
      outcome = outcome_name
    ))
  }
}

.core_percentage <- function(
  datasets,
  pct_improvement,
  pct_deterioration,
  direction,
  outcome
) {
  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )

  # Determine RCI and check each participant's change relative to it
  pct_results <- calc_percentage(
    data = datasets[["data"]],
    pct_improvement = pct_improvement,
    pct_deterioration = pct_deterioration,
    direction = direction
  )

  # Create the summary table
  summary_table <- create_summary_table(
    x = pct_results,
    data = datasets
  )

  # SAFELY convert to a generic tibble
  pct_results <- tibble::as_tibble(pct_results)

  # Output list
  output <- list(
    datasets = datasets,
    pct_results = pct_results,
    outcome = outcome,
    n_obs = n_obs,
    pct_improvement = pct_improvement,
    pct_deterioration = pct_deterioration,
    direction = direction,
    summary_table = summary_table
  )

  # Return output
  class(output) <- c("cs_analysis", "cs_percentage", class(output))
  output
}

#' Print Method for the Percentange-Change Approach
#'
#' @param x An object of class `cs_percentage`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = 0.5
#'   )
#'
#' cs_results
print.cs_percentage <- function(x, ...) {
  summary_table <- .format_summary_table(x[["summary_table"]])
  pct_improvement <- insight::format_percent(x[["pct_improvement"]])
  pct_deterioration <- insight::format_percent(x[["pct_deterioration"]])

  if (x[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  model_info <- .format_model_info_string(
    list(
      Approach = "Percentage-based",
      "Percentage Improvement" = pct_improvement,
      "Percentage Deterioration" = pct_deterioration,
      "Better is" = direction
    )
  )

  .print_strings(
    model_info,
    summary_table
  )
}

#' Print Method for the Percentage-Change Approach Sensitivity
#'
#' @param x An object of class `cs_percentage_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
print.cs_percentage_sensitivity <- function(x, ...) {
  summary_table_agg <- .summarize_sensitivity_table(x[["summary_table"]])
  summary_table <- .format_summary_table(summary_table_agg)

  if (x[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  model_info <- .format_model_info_string(
    list(
      Approach = "Percentage-based Sensitivity",
      "Better is" = direction
    )
  )

  .print_strings(
    model_info,
    summary_table
  )
}

#' Summary Method for the Percentage-Change Approach
#'
#' @param object An object of class `cs_percentage`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = 0.5
#'   )
#'
#' summary(cs_results)
summary.cs_percentage <- function(object, ...) {
  # Get necessary information from object
  summary_table <- .format_summary_table(object[["summary_table"]])
  pct_improvement <- insight::format_percent(object[["pct_improvement"]])
  pct_deterioration <- insight::format_percent(object[["pct_deterioration"]])

  if (object[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  pct <- round(n_used / n_original, digits = 3) * 100
  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Percentage-based",
      "Percentage Improvement" = pct_improvement,
      "Percentage Deterioration" = pct_deterioration,
      "Better is" = direction,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent used" = insight::format_percent(n_used / n_original),
      Outcome = outcome
    )
  )

  .print_strings(
    model_info,
    summary_table
  )
}

#' Summary Method for the Percentage-Change Approach Sensitivity
#'
#' @param object An object of class `cs_percentage_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_percentage(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     pct_improvement = seq(0.3, 0.6, by = 0.1)
#'   )
#'
#' summary(cs_results)
summary.cs_percentage_sensitivity <- function(object, ...) {
  summary_table_agg <- .summarize_sensitivity_table(object[["summary_table"]])
  summary_table <- .format_summary_table(summary_table_agg)

  # Helper zur Formatierung der Ranges mit Format_Percent
  format_range <- function(x) {
    if (is.null(x)) {
      return("---")
    }
    if (length(x) == 1) {
      return(insight::format_percent(x))
    }
    paste0(
      insight::format_percent(min(x)),
      " to ",
      insight::format_percent(max(x))
    )
  }

  pct_improvement <- format_range(object[["pct_improvement"]])
  pct_deterioration <- format_range(object[["pct_deterioration"]])

  if (is.null(object[["pct_deterioration"]])) {
    pct_deterioration <- paste0(pct_improvement, " (symmetric)")
  }

  if (object[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Percentage-based Sensitivity",
      "Range Percentage Improvement" = pct_improvement,
      "Range Percentage Deterioration" = pct_deterioration,
      "Better is" = direction,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent used" = insight::format_percent(n_used / n_original),
      Outcome = outcome
    )
  )

  .print_strings(
    model_info,
    summary_table
  )
}
