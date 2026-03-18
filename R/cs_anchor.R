#' Anchor-Based Analysis of Clinical Significance for Individuals
#'
#' @description `cs_anchor()` estimates the clinical significance of
#'   intervention studies employing the anchor-based approach at the individual
#'   level.
#'
#'   This approach requires a predefined Minimally Important Difference (MID) for
#'   the used instrument, which corresponds to the smallest difference in scores
#'   that patients perceive as beneficial. The function categorizes each
#'   participant's change into one of three categories: improved, unchanged, or
#'   deteriorated.
#'
#' @section Computational details:
#'   The analysis compares the individual change of each participant (post -
#'   pre) against the specified MID thresholds.
#'
#'   A patient is categorized as:
#'   - **Improved**: The change indicates a benefit and its absolute magnitude is
#'   equal to or greater than `mid_improvement`.
#'   - **Deteriorated**: The change indicates a worsening and its absolute
#'   magnitude is equal to or greater than `mid_deterioration`.
#'   - **Unchanged**: The absolute magnitude of change is less than both the
#'   improvement and deterioration thresholds.
#'
#'   If `mid_deterioration` is not specified, it is assumed to be equal to
#'   `mid_improvement` (symmetrical MIDs).
#'
#' @inheritSection cs_distribution Data preparation
#'
#' @inheritParams cs_distribution
#' @param mid_improvement Numeric, the minimal change that indicates a clinically
#'   significant improvement.
#' @param mid_deterioration Numeric, the minimal change that indicates a
#'   clinically significant deterioration (optional). If `mid_deterioration` is
#'   not provided, it will be assumed to be equal to `mid_improvement`.
#' @param ... Additional arguments passed to methods.
#'
#' @family main
#'
#' @return An S3 object of class `cs_anchor_individual` and `cs_analysis`
#' @export
#'
#' @examples
#' # 1. Basic analysis with a symmetric MID of 8
#' cs_results <- antidepressants |>
#'   cs_anchor(
#'     id = patient,
#'     time = measurement,
#'     outcome = mom_di,
#'     mid_improvement = 8
#'   )
#'
#' summary(cs_results)
#' plot(cs_results)
#'
#' # 2. Analysis with distinct MIDs for improvement and deterioration
#' #    (e.g., improvement requires 8 points, but 5 points indicate worsening)
#' cs_results_asym <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     mid_improvement = 8,
#'     mid_deterioration = 5
#'   )
#'
#' summary(cs_results_asym)
#'
#' # 3. If "lower" scores are better (e.g., symptom severity), use better_is
#' #    (Default is "lower", but explicit definition is good practice)
#' cs_results_lower <- antidepressants |>
#'   cs_anchor(
#'     patient,
#'     measurement,
#'     mom_di,
#'     mid_improvement = 8,
#'     better_is = "lower"
#'   )
cs_anchor <- function(data, ...) {
  UseMethod("cs_anchor")
}

#' @export
#' @describeIn cs_anchor Default method for data frames
cs_anchor.default <- function(
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

  checkmate::assert_numeric(
    mid_improvement,
    lower = 0,
    finite = TRUE,
    min.len = 1
  )
  checkmate::assert_numeric(
    mid_deterioration,
    lower = 0,
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

  # Prepend a class to enable method dispatch
  prepend_classes <- c(
    "cs_anchor",
    paste("cs", "anchor", "individual", sep = "_")
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
        # FIX: NA Werte (entstanden durch NULL) durch die symmetrischen improvement Werte ersetzen
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
            # det ist hier jetzt immer ein valider numerischer Wert
            .core_anchor_individual(
              datasets = datasets,
              mid_improvement = imp,
              mid_deterioration = det,
              post = post,
              direction = direction,
              outcome = outcome_name,
              prepend_classes = prepend_classes
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
      mid_improvement = mid_improvement,
      mid_deterioration = mid_deterioration,
      better_is = better_is[[1]],
      n_obs = n_obs,
      direction = direction,
      outcome = outcome_name
    )
    class(output) <- c(
      "cs_analysis",
      "cs_anchor_individual_sensitivity",
      "list"
    )
    return(output)
  } else {
    # Wenn mid_deterioration NULL ist, wird es dem improvement gleichgesetzt
    if (is.null(mid_deterioration)) {
      mid_deterioration <- mid_improvement
    }

    return(.core_anchor_individual(
      datasets = datasets,
      mid_improvement = mid_improvement,
      mid_deterioration = mid_deterioration,
      post = post,
      direction = direction,
      outcome = outcome_name,
      prepend_classes = prepend_classes
    ))
  }
}

.core_anchor_individual <- function(
  datasets,
  mid_improvement,
  mid_deterioration,
  post,
  direction,
  outcome,
  prepend_classes
) {
  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )

  # Check each participant's change relative to MID
  anchor_results <- calc_anchor(
    data = datasets,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    post = post,
    direction = direction
  )

  # Create the summary table for printing and exporting
  summary_table <- create_summary_table(
    x = anchor_results,
    data = datasets
  )

  # SAFELY convert to a generic tibble instead of hard-overwriting classes
  anchor_results <- tibble::as_tibble(anchor_results)

  # Put everything into a list
  output <- list(
    datasets = datasets,
    anchor_results = anchor_results,
    outcome = outcome,
    n_obs = n_obs,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    direction = direction,
    summary_table = summary_table
  )

  # Return output
  class(output) <- c("cs_analysis", prepend_classes, class(output))
  output
}


#' Print Method for the Anchor-Based Approach for Individuals
#'
#' @param x An object of class `cs_anchor_individual`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#' cs_results
print.cs_anchor_individual <- function(x, ...) {
  summary_table <- .format_summary_table(x[["summary_table"]])
  mid_improvement <- x[["mid_improvement"]]
  mid_deterioration <- x[["mid_deterioration"]]

  if (x[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based",
      "MID Improvement" = mid_improvement,
      "MID Deterioration" = mid_deterioration,
      "Better is" = direction
    )
  )

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}

#' Print Method for the Anchor-Based Approach Sensitivity
#'
#' @param x An object of class `cs_anchor_individual_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
print.cs_anchor_individual_sensitivity <- function(x, ...) {
  has_group <- "group" %in% names(x[["summary_table"]])

  if (has_group) {
    summary_table_agg <- x[["summary_table"]] |>
      dplyr::group_by(group, category)
  } else {
    summary_table_agg <- x[["summary_table"]] |>
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

  summary_table <- .format_summary_table(summary_table_agg)

  if (x[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based Sensitivity",
      "Better is" = direction
    )
  )

  .print_strings(
    model_info,
    summary_table
  )
}


#' Summary Method for the Anchor-Based Approach
#'
#' @param object An object of class `cs_anchor_individual`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_anchor(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     mid_improvement = 7
#'   )
#'
#' summary(cs_results)
summary.cs_anchor_individual <- function(object, ...) {
  # Get necessary information from object
  summary_table <- .format_summary_table(object[["summary_table"]])
  mid_improvement <- object[["mid_improvement"]]
  mid_deterioration <- object[["mid_deterioration"]]
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]

  if (object[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Anchor-based",
      "MID Improvement" = mid_improvement,
      "MID Deterioration" = mid_deterioration,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent (used)" = insight::format_percent(n_used / n_original),
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

#' Summary Method for the Anchor-Based Approach Sensitivity
#'
#' @param object An object of class `cs_anchor_individual_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
summary.cs_anchor_individual_sensitivity <- function(object, ...) {
  has_group <- "group" %in% names(object[["summary_table"]])

  if (has_group) {
    summary_table_agg <- object[["summary_table"]] |>
      dplyr::group_by(group, category)
  } else {
    summary_table_agg <- object[["summary_table"]] |>
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

  summary_table <- .format_summary_table(summary_table_agg)

  # Helper zur Formatierung der Ranges
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

  # Spezifischer Fall: mid_deterioration wurde nicht übergeben (Symmetrie impliziert)
  if (is.null(object[["mid_deterioration"]])) {
    mid_deterioration <- paste0(mid_improvement, " (symmetric)")
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
      Approach = "Anchor-based Sensitivity",
      "Range MID Improvement" = mid_improvement,
      "Range MID Deterioration" = mid_deterioration,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent (used)" = insight::format_percent(n_used / n_original),
      "Better is" = direction,
      Outcome = outcome
    )
  )

  .print_strings(
    model_info,
    summary_table
  )
}
