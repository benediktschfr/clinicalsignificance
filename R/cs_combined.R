#' Combined Analysis of Clinical Significance
#'
#' @description `cs_combined()` can be used to determine the clinical
#'   significance of intervention studies employing the combination of the
#'   distribution-based and statistical approach. For this, it will be assumed
#'   that the functional (non-clinical population) and patient (clinical
#'   population) scores form two distinct distributions on a continuum.
#'   `cs_combined()` calculates a cutoff point between these two populations as
#'   well as a reliable change index (RCI) based on a provided instrument
#'   reliability estimate and counts, how many of those patients that showed a
#'   reliable change (that is likely to be not due to measurement error)
#'   switched from the clinical to the functional population during
#'   intervention. Several methods for calculating the cutoff and RCI are
#'   available.
#'
#' @inheritSection cs_statistical Computational details
#' @inheritSection cs_distribution Computational details
#'
#' @section Categories: Each individual's change can then be categorized into
#'   the following groups:
#' - Recovered, i.e., the individual showed a reliable change in the beneficial direction and changed from the clinical to the functional population
#' - Improved, i.e., the individual showed a reliable change in the beneficial direction but did not change populations
#' - Unchanged, i.e., the individual showed no reliable change
#' - Deteriorated, i.e., the individual showed a reliable change in the disadvantageous direction but did not change populations
#' - Harmed, i.e., the individual showed a reliable change in the disadvantageous direction and switched from the functional to the clinincal population
#'
#' @inheritSection cs_distribution Data preparation
#'
#' @inheritParams cs_distribution
#' @inheritParams cs_statistical
#' @inheritParams cs_anchor
#'
#' @family main
#'
#' @return An S3 object of class `cs_analysis` and `cs_combined`
#' @export
#'
#' @examples
# In this case, cutoff "a" is chosen by default
#' cs_results <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80
#'   )
#'
#' cs_results
#' summary(cs_results)
#' plot(cs_results)
#'
#'
#' # You can choose a different cutoff but must provide summary statistics for the
#' # functional population
#' cs_results_c <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     cutoff_type = "c"
#'   )
#'
#' cs_results_c
#' summary(cs_results_c)
#' plot(cs_results_c)
#'
#'
#' # You can group the analysis by providing a grouping variable in the data
#' cs_results_grouped <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     group = treatment,
#'     reliability = 0.80,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     cutoff_type = "c"
#'   )
#'
#' cs_results_grouped
#' summary(cs_results_grouped)
#' plot(cs_results_grouped)
cs_combined <- function(
  data,
  id,
  time,
  outcome,
  group = NULL,
  pre = NULL,
  post = NULL,
  mid_improvement = NULL,
  mid_deterioration = NULL,
  reliability = NULL,
  reliability_post = NULL,
  m_functional = NULL,
  sd_functional = NULL,
  better_is = c("lower", "higher"),
  rci_method = c("JT", "GLN", "HLL", "EN", "NK", "HA", "HLM"),
  cutoff_type = c("a", "b", "c"),
  significance_level = 0.05
) {
  cs_method <- rlang::arg_match(rci_method)
  cut_type <- rlang::arg_match(cutoff_type)
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

  checkmate::assert_data_frame(data)
  checkmate::assert_number(
    significance_level,
    lower = 0,
    upper = 1,
    finite = TRUE
  )

  checkmate::assert_number(
    mid_improvement,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_number(
    mid_deterioration,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_number(
    reliability,
    lower = 0,
    upper = 1,
    finite = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_number(
    reliability_post,
    lower = 0,
    upper = 1,
    finite = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_number(m_functional, finite = TRUE, null.ok = TRUE)
  checkmate::assert_number(
    sd_functional,
    lower = 0,
    finite = TRUE,
    null.ok = TRUE
  )

  if (is.null(mid_improvement) && cs_method != "HLM") {
    if (is.null(reliability)) {
      cli::cli_abort(
        "Argument {.arg reliability} is required when using distribution-based RCI methods (except HLM)."
      )
    }
  }

  if (cut_type %in% c("b", "c")) {
    if (is.null(m_functional) || is.null(sd_functional)) {
      cli::cli_abort(c(
        "Functional population statistics missing.",
        "x" = "For cutoff types {.val b} and {.val c}, mean and SD of the functional population are required.",
        "i" = "Please supply {.arg m_functional} and {.arg sd_functional}."
      ))
    }
  }

  # Prepare the data
  datasets <- .prep_data(
    data = data,
    id = {{ id }},
    time = {{ time }},
    outcome = {{ outcome }},
    group = {{ group }},
    pre = {{ pre }},
    post = {{ post }},
    method = cs_method
  )

  # Determine Classes & Method Name
  use_anchor <- !is.null(mid_improvement)

  if (use_anchor) {
    class(datasets) <- c("cs_anchor_individual", class(datasets))
    cs_method <- "CWB"

    # Defaults handling
    if (is.null(mid_deterioration)) mid_deterioration <- mid_improvement
  } else {
    # Distribution-Based Logic
    class(datasets) <- c(paste0("cs_", tolower(cs_method)), class(datasets))
  }

  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )

  # Summary Stats (Pre ist immer nötig)
  m_pre <- mean(datasets[["data"]][["pre"]], na.rm = TRUE)
  sd_pre <- stats::sd(datasets[["data"]][["pre"]], na.rm = TRUE)

  # Init Post Stats (Safety)
  m_post <- NULL
  sd_post <- NULL

  if (!is.null(datasets[["data"]][["post"]])) {
    m_post <- mean(datasets[["data"]][["post"]], na.rm = TRUE)
    sd_post <- stats::sd(datasets[["data"]][["post"]], na.rm = TRUE)
  }

  # Direction
  direction <- if (better_is[1] == "lower") -1 else 1

  # Critical Value
  if (cs_method != "HA") {
    critical_value <- stats::qnorm(1 - significance_level / 2)
  } else {
    critical_value <- stats::qnorm(1 - significance_level)
  }

  if (!use_anchor) {
    # Path A: Standard RCI (Distribution)
    rci_results <- calc_rci(
      data = datasets,
      m_pre = m_pre,
      m_post = m_post,
      sd_pre = sd_pre,
      sd_post = sd_post,
      reliability = reliability,
      reliability_post = reliability_post,
      direction = direction,
      critical_value = critical_value
    )
  } else {
    # Path B: Anchor Based
    rci_results <- calc_anchor(
      data = datasets,
      mid_improvement = mid_improvement,
      mid_deterioration = mid_deterioration,
      direction = direction
    )
  }

  # Cutoff (immer berechnet)
  cutoff_results <- calc_cutoff_from_data(
    data = datasets,
    m_clinical = m_pre,
    sd_clinical = sd_pre,
    m_functional = m_functional,
    sd_functional = sd_functional,
    m_post = m_post,
    sd_post = sd_post,
    reliability = reliability,
    type = cut_type,
    direction = direction,
    critical_value = critical_value
  )

  # Formatting Classes
  class(rci_results) <- c("cs_combined", "list")
  class(cutoff_results) <- "list"

  # Summary Table
  summary_table <- create_summary_table(
    x = rci_results,
    cutoff_results = cutoff_results,
    data = datasets,
    method = cs_method,
    r_dd = rci_results[["r_dd"]], # Kann NULL sein bei Anchor
    se_measurement = rci_results[["se_measurement"]], # Kann NULL sein bei Anchor
    cutoff = cutoff_results[["info"]][["value"]],
    sd_post = sd_post,
    direction = direction
  )

  # Output Construction
  output <- list(
    datasets = datasets,
    cutoff_results = cutoff_results,
    rci_results = rci_results,
    outcome = deparse(substitute(outcome)),
    n_obs = n_obs,
    method = cs_method,
    mid_improvement = mid_improvement,
    mid_deterioration = mid_deterioration,
    direction = direction,
    reliability = reliability,
    critical_value = critical_value,
    summary_table = summary_table
  )

  # Return
  class(output) <- c(
    "cs_analysis",
    "cs_combined",
    class(datasets),
    class(output)
  )
  output
}


#' Print Method for the Combined Approach
#'
#' @param x An object of class `cs_combined`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_combined(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#'
#' cs_results
print.cs_combined <- function(x, ...) {
  individual_summary_table <- .format_summary_table(x[["summary_table"]][[
    "individual_level_summary"
  ]])

  cs_method <- x[["method"]]

  if (cs_method == "HA") {
    group_summary_table <- x[["summary_table"]][["group_level_summary"]] |>
      .format_summary_table(table_title = "Group Level Summary")
  }

  if (x[["direction"]] == -1) {
    direction <- "Lower"
  } else {
    direction <- "Higher"
  }

  model_info <- .format_model_info_string(
    list(
      Approach = "Combined",
      "Method" = cs_method,
      "Better is" = direction
    )
  )

  # Print output
  if (cs_method != "HA") {
    .print_strings(
      model_info,
      individual_summary_table
    )
  } else {
    .print_strings(
      model_info,
      individual_summary_table,
      group_summary_table
    )
  }
}


#' Summary Method for the Combined Approach
#'
#' @param object An object of class `cs_combined`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_combined(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#'
#' summary(cs_results)
summary.cs_combined <- function(object, ...) {
  # browser()
  # Get necessary information from object
  summary_table <- .format_summary_table(
    object[["summary_table"]][[
      "individual_level_summary"
    ]],
    table_title = "-- Results"
  )

  rci_method <- object[["method"]]
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  cutoff_info <- cs_get_cutoff(object, with_descriptives = TRUE)
  cutoff_type <- cutoff_info[["type"]]
  cutoff_value <- round(cutoff_info[["value"]], 2)
  cutoff_descriptives <- cutoff_info[, 1:4] |>
    dplyr::rename(
      "M Clinical" = "m_clinical",
      "SD Clinical" = "sd_clinical",
      "M Functional" = "m_functional",
      "SD Functional" = "sd_functional"
    ) |>
    insight::export_table(missing = "---", title = "-- Cutoff Descriptives")
  mid_improvement <- object[["mid_improvement"]]
  mid_deterioration <- object[["mid_deterioration"]]

  if (rci_method == "HA") {
    group_summary_table <- .format_summary_table(
      object[["summary_table"]][["group_level_summary"]],
      table_title = "Group Level Results"
    )
  }

  outcome <- object[["outcome"]]

  model_info <- list(
    Approach = "Distribution-based",
    "RCI Method" = rci_method,
    "N (original)" = n_original,
    "N (used)" = n_used,
    "Percent used" = insight::format_percent(
      n_used / n_original
    ),
    Outcome = object[["outcome"]],
    "Cutoff Type" = cutoff_type,
    Cutoff = cutoff_value,
    Outcome = outcome
  )

  if (rci_method == "HLM") {
    additional_info <- list(
      Reliability = "----"
    )
  } else if (rci_method == "NK") {
    additional_info <- list(
      "Realiability Pre" = cs_get_reliability(object)[[1]],
      "Reliability Post" = cs_get_reliability(object)[[2]]
    )
  }
  if (rci_method == "CWB") {
    additional_info <- list(
      "MID (Improvement)" = mid_improvement,
      "MID (Deterioration)" = mid_deterioration
    )
  } else {
    additional_info <- list(
      Reliability = cs_get_reliability(object)[[1]]
    )
  }

  model_info <- .format_model_info_string(c(model_info, additional_info))

  # Print output
  .print_strings(
    model_info,
    cutoff_descriptives,
    summary_table
  )
}
