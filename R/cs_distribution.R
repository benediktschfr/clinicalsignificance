#' Distribution-Based Analysis of Clinical Significance
#'
#' @description `cs_distribution()` can be used to determine the clinical
#'   significance of intervention studies employing the distribution-based
#'   approach. For this, the reliable change index is estimated from the
#'   provided data and a known reliability estimate which indicates, if an
#'   observed individual change is likely to be greater than the measurement
#'   error inherent for the used instrument. In this case, a reliable change is
#'   defined as clinically significant. Several methods for calculating this RCI
#'   can be chosen.
#'
#' @section Computational details: From the provided data, a region of change is
#'   calculated in which an individual change may likely be due to an inherent
#'   measurement of the used instrument. This concept is also known as the
#'   minimally detectable change (MDC).
#'
#'
#' @section Categories: Each individual's change may then be categorized into
#'   one of the following three categories:
#'   - Improved, the change is greater than the RCI in the beneficial direction
#'   - Unchanged, the change is within a region that may attributable to
#'   measurement error
#'   - Deteriorated, the change is greater than the RCI, but in the
#'   disadvantageous direction
#'
#'   Most of these methods are developed to deal with data containing two
#'   measurements per individual, i.e., a pre intervention and post intervention
#'   measurement. The Hierarchical Linear Modeling (`rci_method = "HLM"`) method
#'   can incorporate data for multiple measurements an can thus be used only
#'   for at least three measurements per participant.
#'
#' @section Data preparation: The data set must be tidy, which corresponds to a
#'   long data frame in general. It must contain a patient identifier which must
#'   be unique per patient. Also, a column containing the different measurements
#'   and the outcome must be supplied. Each participant-measurement combination
#'   must be unique, so for instance, the data must not contain two "After"
#'   measurements for the same patient.
#'
#'   Additionally, if the measurement column contains only two values, the first
#'   value based on alphabetical, numerical or factor ordering will be used as
#'   the `pre` measurement. For instance, if the column contains the
#'   measurements identifiers `"pre"` and `"post"` as strings, then `"post"`
#'   will be sorted before `"pre"` and thus be used as the `"pre"` measurement.
#'   The function will throw a warning but generally you may want to explicitly
#'   define the `"pre"` and `"post"` measurement with arguments `pre` and
#'   `post`. In case of more than two measurement identifiers, you have to
#'   define `pre` and `post` manually since the function does not know what your
#'   pre and post intervention measurements are.
#'
#'   If your data is grouped, you can specify the group by referencing the
#'   grouping variable (see examples below). The analysis is then run for every
#'   group to compare group differences.
#'
#' @param data A tidy data frame
#' @param id Participant ID
#' @param time Time variable
#' @param outcome Outcome variable
#' @param group Grouping variable (optional)
#' @param pre Pre measurement (only needed if the time variable contains more
#'   than two measurements)
#' @param post Post measurement (only needed if the time variable contains more
#'   than two measurements)
#' @param reliability The instrument's reliability estimate. If you selected the
#'   NK method, the here specified reliability will be the instrument's pre
#'   measurement reliability. Not needed for the HLM method.
#' @param reliability_post The instrument's reliability at post measurement
#'   (only needed for the NK method)
#' @param better_is Which direction means a better outcome for the used
#'   instrument? Available are
#'   - `"lower"` (lower outcome scores are desirable, the default) and
#'   - `"higher"` (higher outcome scores are desirable)
#' @param rci_method Clinical significance method. Available are
#'   - `"JT"` (Jacobson & Truax, 1991, the default)
#'   - `"GLN"` (Gulliksen, Lord, and Novick; Hsu, 1989, Hsu, 1995)
#'   - `"HLL"` (Hsu, Linn & Nord; Hsu, 1989)
#'   - `"EN"` (Edwards & Nunnally; Speer, 1992)
#'   - `"NK"` (Nunnally & Kotsch, 1983), requires a reliability estimate at post
#'   measurement. If this is not supplied, reliability and reliability_post are
#'   assumed to be equal
#'    - `"HA"` (Hageman & Arrindell, 1999)
#'    - `"HLM"` (Hierarchical Linear Modeling; Raudenbush & Bryk, 2002),
#'   requires at least three measurements per patient
#' @param significance_level Significance level alpha, defaults to `0.05`. If
#'   you choose the `"HA"` method, this value corresponds to the maximum risk of
#'   misclassification
#' @param ... Additional arguments passed to methods.
#'
#' @references
#' - Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. Journal of Consulting and Clinical Psychology, 59(1), 12–19. https://doi.org/10.1037//0022-006X.59.1.12
#' - Hsu, L. M. (1989). Reliable changes in psychotherapy: Taking into account regression toward the mean. Behavioral Assessment, 11(4), 459–467.
#' - Hsu, L. M. (1995). Regression toward the mean associated with measurement error and the identification of improvement and deterioration in psychotherapy. Journal of Consulting and Clinical Psychology, 63(1), 141–144. https://doi.org/10.1037//0022-006x.63.1.141
#' - Speer, D. C. (1992). Clinically significant change: Jacobson and Truax (1991) revisited. Journal of Consulting and Clinical Psychology, 60(3), 402–408. https://doi.org/10.1037/0022-006X.60.3.402
#' - Nunnally, J. C., & Kotsch, W. E. (1983). Studies of individual subjects: Logic and methods of analysis. British Journal of Clinical Psychology, 22(2), 83–93. https://doi.org/10.1111/j.2044-8260.1983.tb00582.x
#' - Hageman, W. J., & Arrindell, W. A. (1999). Establishing clinically significant change: increment of precision and the distinction between individual and group level analysis. Behaviour Research and Therapy, 37(12), 1169–1193. https://doi.org/10.1016/S0005-7967(99)00032-7
#' - Raudenbush, S. W., & Bryk, A. S. (2002). Hierarchical Linear Models - Applications and Data Analysis Methods (2nd ed.). Sage Publications.
#'
#' @family main
#'
#' @return An S3 object of class `cs_analysis` and `cs_distribution`
#' @export
#'
#' @examples
#' antidepressants |>
#'   cs_distribution(patient, measurement, mom_di, reliability = 0.80)
#'
#'
#' # Turn off the warning by providing the pre measurement time
#' cs_results <- antidepressants |>
#'   cs_distribution(
#'     patient,
#'     measurement,
#'     mom_di,
#'     pre = "Before",
#'     reliability = 0.80
#'   )
#'
#' summary(cs_results)
#' plot(cs_results)
#'
#'
#' # If you use data with more than two measurements, you always have to define a
#' # pre and post measurement
#' cs_results <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     hamd,
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
#' # Set the rci_method argument to change the RCI method
#' cs_results_ha <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80,
#'     rci_method = "HA"
#'   )
#'
#' cs_results_ha
#' summary(cs_results_ha)
#' plot(cs_results_ha)
#'
#'
#' # Group the analysis by providing a grouping variable
#' cs_results_grouped <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     hamd,
#'     pre = 1,
#'     post = 4,
#'     group = treatment,
#'     reliability = 0.80
#'   )
#'
#' cs_results_grouped
#' summary(cs_results_grouped)
#' plot(cs_results_grouped)
#'
#'
#' # Use more than two measurements
#' cs_results_hlm <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     hamd,
#'     rci_method = "HLM"
#'   )
#'
#' cs_results_hlm
#' summary(cs_results_hlm)
#' plot(cs_results_hlm)
cs_distribution <- function(data, ...) {
  UseMethod("cs_distribution")
}

#' @export
#' @describeIn cs_distribution Default method for data frames
cs_distribution.default <- function(
  data,
  id,
  time,
  outcome,
  group = NULL,
  pre = NULL,
  post = NULL,
  reliability = NULL,
  reliability_post = NULL,
  better_is = c("lower", "higher"),
  rci_method = c("JT", "GLN", "HLL", "EN", "NK", "HA", "HLM"),
  significance_level = 0.05,
  ...
) {
  cs_method <- rlang::arg_match(rci_method)
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

  # Check Reliability (Allow vector for sensitivity!)
  checkmate::assert_numeric(
    reliability,
    lower = 0,
    upper = 1,
    null.ok = TRUE,
    min.len = 1
  )

  # HLM braucht keine Reliability, daher Abbruch der Sensitivitätsprüfung
  if (cs_method == "HLM") {
    if (!is.null(reliability)) {
      cli::cli_alert_info(
        "Method {.val HLM} does not use reliability. Argument ignored."
      )
    }
    # Setze auf NULL für den Core-Aufruf
    reliability <- NULL
  } else {
    if (is.null(reliability)) {
      cli::cli_abort(
        "Argument {.arg reliability} is required for method {.val {cs_method}}."
      )
    }
  }

  # Special Case NK Method: reliability_post handling
  if (cs_method == "NK") {
    # Hier müsste man entscheiden: Variiert man auch reliability_post?
    # Fürs Erste: Wenn post NULL, nimm pre (auch wenn pre ein Vektor ist).
    if (is.null(reliability_post)) {
      reliability_post <- reliability
      cli::cli_alert_info(
        "Using {.arg reliability} for {.arg reliability_post} as well."
      )
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

  # Prepend a class
  class(datasets) <- c(paste0("cs_", tolower(cs_method)), class(datasets))

  if (length(reliability) > 1) {
    # >>> SENSITIVITÄTSANALYSE <<<

    # Check: NK Method mit Vektoren ist komplex.
    # Wenn reliability ein Vektor ist und post auch, müssen sie gleich lang sein.
    if (cs_method == "NK" && length(reliability_post) > 1) {
      if (length(reliability) != length(reliability_post)) {
        cli::cli_abort(
          "Lengths of {.arg reliability} and {.arg reliability_post} must match."
        )
      }
    } else if (cs_method == "NK" && length(reliability_post) == 1) {
      # Recycle reliability_post if scalar
      reliability_post <- rep(reliability_post, length(reliability))
    }

    # Loop over reliabilities
    results_list <- reliability |>
      tibble::as_tibble_col("reliability") |>
      dplyr::mutate(
        models = purrr::map2(
          reliability,
          if (is.null(reliability_post)) NA else reliability_post,
          function(r, r_post) {
            # Core aufrufen
            # Bei map2 mit NA für r_post aufpassen: NULL übergeben wenn nötig
            use_post <- if (is.na(r_post)) NULL else r_post

            .core_distribution(
              datasets = datasets,
              reliability = r,
              reliability_post = use_post,
              cs_method = cs_method,
              better_is = better_is,
              significance_level = significance_level,
              outcome = deparse(substitute(outcome))
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

    # Output Objekt bauen
    output <- list(
      summary_table = combined_tables,
      reliability = reliability,
      reliability_post = reliability_post,
      method = cs_method,
      better_is = better_is[[1]],
      n_obs = n_obs,
      significance_level = significance_level,
      outcome = deparse(substitute(outcome))
    )
    class(output) <- c("cs_analysis", "cs_distribution_sensitivity", "list")
    output
  } else {
    .core_distribution(
      datasets = datasets,
      reliability = reliability,
      reliability_post = reliability_post,
      cs_method = cs_method,
      better_is = better_is[[1]],
      significance_level = significance_level,
      outcome = deparse(substitute(outcome))
    )
  }
}

.core_distribution <- function(
  datasets,
  reliability,
  reliability_post,
  cs_method,
  better_is,
  significance_level,
  outcome
) {
  # Count participants
  n_obs <- list(
    n_original = nrow(datasets[["wide"]]),
    n_used = nrow(datasets[["data"]])
  )

  # Summary Stats (Pre ist immer nötig)
  m_pre <- mean(datasets[["data"]][["pre"]], na.rm = TRUE)
  sd_pre <- stats::sd(datasets[["data"]][["pre"]], na.rm = TRUE)

  # Init Post Stats
  m_post <- NULL
  sd_post <- NULL

  # Calculate Post Stats if available
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

  # Calculate RCI
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

  # Summary Table
  summary_table <- create_summary_table(
    x = rci_results,
    data = datasets
  )

  class(rci_results) <- "list"

  # Output Construction
  output <- list(
    datasets = datasets,
    rci_results = rci_results,
    outcome = outcome,
    method = cs_method,
    n_obs = n_obs,
    reliability = reliability,
    critical_value = critical_value,
    summary_table = summary_table
  )

  # Return
  class(output) <- c(
    "cs_analysis",
    "cs_distribution",
    class(datasets),
    class(output)
  )

  output
}


#' Print Method for the Distribution-Based Approach
#'
#' @param x An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#'
#' cs_results
print.cs_distribution <- function(x, ...) {
  model_info <- .format_model_info_string(
    list(
      Approach = "Distribution-based",
      "RCI Method" = x[["method"]]
    )
  )

  summary_table <- .format_summary_table(x[["summary_table"]])

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}

#' Print Method for the Distribution-Based Approach
#'
#' @param x An object of class `cs_distribution_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects
#' @export
print.cs_distribution_sensitivity <- function(x, ...) {
  model_info <- .format_model_info_string(
    list(
      Approach = "Distribution-based Sensitivity",
      "RCI Method" = x[["method"]]
    )
  )

  summary_table <- .format_summary_table(x[["summary_table"]])

  # Print output
  .print_strings(
    model_info,
    summary_table
  )
}


#' Summary Method for the Distribution-Based Approach
#'
#' @param object An object of class `cs_distribution`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(id, time, hamd, pre = 1, post = 4, reliability = 0.8)
#'
#' summary(cs_results)
summary.cs_distribution <- function(object, ...) {
  # browser()
  # Get necessary information from object
  summary_table <- .format_summary_table(object[["summary_table"]])
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  rci_method <- object[["method"]]

  model_info <- list(
    Approach = "Distribution-based",
    "RCI Method" = rci_method,
    "N (original)" = n_original,
    "N (used)" = n_used,
    "Percent used" = insight::format_percent(
      n_used / n_original
    ),
    Outcome = object[["outcome"]]
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
  } else {
    additional_info <- list(
      Reliability = cs_get_reliability(object)[[1]]
    )
  }

  model_info <- .format_model_info_string(c(model_info, additional_info))

  .print_strings(
    model_info,
    summary_table
  )
}

#' Summary Method for the Distribution-Based Approach Sensitivity
#'
#' @param object An object of class `cs_distribution_sensitivity`
#' @param ... Additional arguments
#'
#' @return No return value, called for side effects only
#' @export
#'
#' @examples
#' cs_results <- claus_2020 |>
#'   cs_distribution(
#'     id, time, bdi, reliability = seq(0.5, 0.9, by = 0.1), pre = 1, post = 4
#'   )
#'
#' summary(cs_results)
summary.cs_distribution_sensitivity <- function(object, ...) {
  # browser()
  # Get necessary information from object
  summary_table <- .format_summary_table(object[["summary_table"]])
  n_original <- cs_get_n(object, "original")[[1]]
  n_used <- cs_get_n(object, "used")[[1]]
  rci_method <- object[["method"]]

  model_info <- list(
    Approach = "Distribution-based Sensitivity",
    "RCI Method" = rci_method,
    "N (original)" = n_original,
    "N (used)" = n_used,
    "Percent used" = insight::format_percent(
      n_used / n_original
    ),
    Outcome = object[["outcome"]]
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
  } else {
    additional_info <- list(
      Reliability = paste0(
        min(object[["reliability"]]),
        " to ",
        max(object[["reliability"]])
      )
    )
  }

  model_info <- .format_model_info_string(c(model_info, additional_info))

  .print_strings(
    model_info,
    summary_table
  )
}
