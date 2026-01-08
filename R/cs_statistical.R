#'Statistical Analysis of Clinical Significance
#'
#'@description `cs_statistical()` can be used to determine the clinical
#'  significance of intervention studies employing the statistical approach. For
#'  this, it will be assumed that the functional (non-clinical population) and
#'  patient (clinical population) scores form two distinct distributions on a
#'  continuum. `cs_statistical()` calculates a cutoff point between these two
#'  populations and counts, how many patients changed from the clinical to the
#'  functional population during intervention. Several methods for calculating
#'  this cutoff are available.
#'
#'@section Computational details: There are three available cutoff types, namely
#'  a, b, and c which can be used to "draw a line" or separate the functional
#'  and clinical population on a continuum. a as a cutoff is defined as the mean
#'  of the clinical population minus two times the standard deviation (SD) of
#'  the clinical population. b is defined as the mean of the functional
#'  population plus also two times the SD of the clinical population. This is
#'  true for "negative" outcomes, where a lower instrument score is desirable.
#'  For "positive" outcomes, where higher scores are beneficial, a is the mean
#'  of the clinical population plus 2 \eqn{\cdot} SD of the clinical population
#'  and b is mean of the functional population minus 2 \eqn{\cdot} SD of the
#'  clinical population. The summary statistics for the clinical population are
#'  estimated from the provided data at pre measurement.
#'
#'  c is defined as the midpoint between both populations based on their
#'  respective mean and SD. In order to calculate b and c, descriptive
#'  statistics for the functional population must be provided.
#'
#'@section Categories: Individual patients can be categorized into one of the
#'  following groups:
#'  - Improved, i.e., one changed from the clinical to the functional population
#'  - Unchanged, i.e., one can be seen as a member of the same population pre
#'  and post intervention
#'  - Deteriorated, i.e., one changed from the functional to the clinical
#'  population during intervention
#'
#'
#'@inheritSection cs_distribution Data preparation
#'
#'@inheritParams cs_distribution
#'@param m_functional Numeric, mean of functional population.
#'@param sd_functional Numeric, standard deviation of functional population
#'@param cutoff_method Cutoff method, Available are
#'    - `"JT"` (Jacobson & Truax, 1991, the default)
#'    - `"HA"` (Hageman & Arrindell, 1999)
#'@param cutoff_type Cutoff type. Available are `"a"`, `"b"`, and `"c"`.
#'  Defaults to `"a"` but `"c"` is usually recommended. For `"b"` and `"c"`,
#'  summary data from a functional population must be given with arguments
#'  `m_functional` and `sd_functional`.
#'
#'
#'@references
#'  - Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. Journal of Consulting and Clinical Psychology, 59(1), 12–19. https://doi.org/10.1037//0022-006X.59.1.12
#'  - Hageman, W. J., & Arrindell, W. A. (1999). Establishing clinically significant change: increment of precision and the distinction between individual and group level analysis. Behaviour Research and Therapy, 37(12), 1169–1193. https://doi.org/10.1016/S0005-7967(99)00032-7
#'
#'@family main
#'
#'@return An S3 object of class `cs_analysis` and `cs_statistical`
#'@export
#'
#' @examples
#' # By default, cutoff type "a" is used
#' cs_results <- claus_2020 |>
#'   cs_statistical(id, time, hamd, pre = 1, post = 4)
#'
#' cs_results
#' summary(cs_results)
#' plot(cs_results)
#'
#'
#' # You can choose a different cutoff type but need to provide additional
#' # population summary statistics for the functional population
#' cs_results_c <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
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
#' # You can use a different method to calculate the cutoff
#' cs_results_ha <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     reliability = 0.80,
#'     cutoff_type = "c",
#'     cutoff_method = "HA"
#'   )
#'
#' cs_results_ha
#' summary(cs_results_ha)
#' plot(cs_results_ha)
#'
#'
#' # And you can group the analysis by providing a grouping variable from the data
#' cs_results_grouped <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     cutoff_type = "c",
#'     group = treatment
#'   )
#'
#' cs_results_grouped
#' summary(cs_results_grouped)
#' plot(cs_results_grouped)
cs_statistical <- function(
  data,
  id,
  time,
  outcome,
  group = NULL,
  pre = NULL,
  post = NULL,
  m_functional = NULL,
  sd_functional = NULL,
  reliability = NULL,
  better_is = c("lower", "higher"),
  cutoff_method = c("JT", "HA"),
  cutoff_type = c("a", "b", "c"),
  significance_level = 0.05
) {
  cs_method <- rlang::arg_match(cutoff_method)
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

  # Optionale Parameter checken (dürfen NULL sein, aber WENN da, dann korrekt)
  checkmate::assert_number(m_functional, finite = TRUE, null.ok = TRUE)
  checkmate::assert_number(
    sd_functional,
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

  # Fall: Hageman & Arrindell (HA) braucht Reliability
  if (cs_method == "HA" && is.null(reliability)) {
    cli::cli_abort(
      "Argument {.arg reliability} is required when {.code cutoff_method = \"HA\"}."
    )
  }

  # Fall: Jacobson & Truax (JT) braucht keine Reliability -> Info an User
  if (cs_method == "JT" && !is.null(reliability)) {
    cli::cli_alert_info(
      "Argument {.arg reliability} is not used for the JT approach and will be ignored."
    )
  }

  # Fall: Cutoff b oder c braucht funktionale Populationsdaten
  if (cut_type %in% c("b", "c")) {
    if (is.null(m_functional) || is.null(sd_functional)) {
      cli::cli_abort(c(
        "Functional population statistics missing.",
        "x" = "For cutoff types {.val b} and {.val c}, mean and SD of the functional population are required.",
        "i" = "Please supply {.arg m_functional} and {.arg sd_functional}."
      ))
    }
  }

  # --- Ab hier: Datenvorbereitung & Berechnung ---

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

  # Prepend a class
  class(datasets) <- c(paste0("cs_", tolower(cs_method)), class(datasets))

  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )

  # Calculate relevant summary statistics
  m_pre <- mean(datasets[["data"]][["pre"]], na.rm = TRUE)
  sd_pre <- stats::sd(datasets[["data"]][["pre"]], na.rm = TRUE)

  # Initialisierung (Safety, falls JT gewählt wurde, damit Variablen existieren)
  m_post <- NULL
  sd_post <- NULL

  # HLL war in deinem Originalcode erwähnt, aber nicht in den Argumenten.
  # Ich lasse es drin, falls es intern genutzt wird.
  if (cs_method %in% c("HLL", "HA")) {
    m_post <- mean(datasets[["data"]][["post"]], na.rm = TRUE)
    sd_post <- stats::sd(datasets[["data"]][["post"]], na.rm = TRUE)
  }

  # Direction
  direction <- if (better_is[1] == "lower") -1 else 1

  # Critical Value
  # qnorm(0.975) für 2-sided 0.05 (JT), qnorm(0.95) für 1-sided (HA - falls das die Logik ist)
  if (cs_method != "HA") {
    critical_value <- stats::qnorm(1 - significance_level / 2)
  } else {
    critical_value <- stats::qnorm(1 - significance_level)
  }

  # Calculate cutoff
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

  # Summary table
  summary_table <- create_summary_table(
    x = cutoff_results,
    data = datasets,
    method = cs_method
  )

  class(cutoff_results) <- "list"

  # Output structure
  output <- list(
    datasets = datasets,
    cutoff_results = cutoff_results,
    outcome = deparse(substitute(outcome)),
    n_obs = n_obs,
    method = cs_method,
    reliability = reliability,
    critical_value = critical_value,
    summary_table = summary_table
  )

  # Return output
  class(output) <- c(
    "cs_analysis",
    "cs_statistical",
    class(datasets),
    class(output)
  )
  output
}


#' Print Method for the Statistical Approach
#'
#' @param x An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 7
#'   )
#'
#' cs_results
print.cs_statistical <- function(x, ...) {
  summary_table <- .format_summary_table(x[["summary_table"]])
  cs_method <- x[["method"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Statistical",
      Method = cs_method
    )
  )

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}


#' Summary Method for the Statistical Approach
#'
#' @param object An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 7
#'   )
#'
#' summary(cs_results)
summary.cs_statistical <- function(object, ...) {
  # Get necessary information from object
  summary_table <- .format_summary_table(
    object[["summary_table"]],
    table_title = "-- Results"
  )

  cs_method <- object[["method"]]
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

  outcome <- object[["outcome"]]

  model_info <- .format_model_info_string(
    list(
      Approach = "Statistical",
      Method = cs_method,
      "N (original)" = n_original,
      "N (used)" = n_used,
      "Percent used" = insight::format_percent(n_used / n_original),
      "Cutoff type" = cutoff_type,
      "Cutoff" = cutoff_value
    )
  )

  # Print output
  .print_strings(
    model_info,
    cutoff_descriptives,
    summary_table
  )
}
