#' Extract Augmented Data from a cs_analysis Object
#'
#' This function returns the patient-wise results, containing the considered pre
#' and post intervention value, its raw change as well as all other change
#' estimates calculated during the clinical significance analysis with the
#' individual's clinical significance category. This function is only useful for
#' individual level analyses because the group level analyses only yield group
#' level results.
#'
#' @param x A `cs_analysis` object
#' @param ... Additional arguments
#'
#' @family get
#'
#' @return A tibble with augmented data
#'
#' @rdname augmented_data
#' @export
#'
#' @examples
#' # Augmented data can be extracted for every individual approach
#' anchor_results <- claus_2020 |>
#'   cs_anchor(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     mid_improvement = 9
#'   )
#'
#'
#' distribution_results <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     reliability = 0.80
#'   )
#'
#'
#' distribution_results_hlm <- claus_2020 |>
#'   cs_distribution(
#'     id,
#'     time,
#'     bdi,
#'     rci_method = "HLM"
#'   )
#'
#'
#' statistical_results <- claus_2020 |>
#'   cs_statistical(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8
#'   )
#'
#'
#' combined_results <- claus_2020 |>
#'   cs_combined(
#'     id,
#'     time,
#'     bdi,
#'     pre = 1,
#'     post = 4,
#'     m_functional = 8,
#'     sd_functional = 8,
#'     reliability = 0.80
#'   )
#'
#'
#' cs_get_augmented_data(anchor_results)
#' cs_get_augmented_data(distribution_results)
#' cs_get_augmented_data(distribution_results_hlm)
#' cs_get_augmented_data(statistical_results)
#' cs_get_augmented_data(combined_results)
cs_get_augmented_data <- function(x, ...) {
  UseMethod("cs_get_augmented_data")
}


#' Default Augmented Data Method
#'
#' @rdname augmented_data
#' @export
cs_get_augmented_data.default <- function(x, ...) {
  cli::cli_abort(
    "Augmented data cannot be extracted for an object of class {.code {class(x)}}"
  )
}


#' Extract Augmented Data from a cs_distribution Object
#'
#' @rdname augmented_data
#' @export
cs_get_augmented_data.cs_distribution <- function(x, ...) {
  rci_categories <- x[["rci_results"]][["data"]]
  used_data <- x[["datasets"]][["data"]]

  used_data |>
    dplyr::left_join(rci_categories, dplyr::join_by("id")) |>
    dplyr::mutate(
      category = dplyr::case_when(
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated"
      ),
      category = factor(
        category,
        levels = c("Improved", "Unchanged", "Deteriorated")
      )
    )
}


#' Extract Augmented Data from a cs_statistical Object
#'
#' @rdname augmented_data
#' @export
cs_get_augmented_data.cs_statistical <- function(x, ...) {
  cutoff_categories <- x[["cutoff_results"]][["data"]]
  used_data <- x[["datasets"]][["data"]]
  cs_method <- x[["method"]]

  # Join data with cutoff results
  joined_data <- used_data |>
    dplyr::left_join(cutoff_categories, dplyr::join_by("id"))

  # Build categories based on cs_method
  if (cs_method != "HA") {
    joined_data |>
      dplyr::mutate(
        category = dplyr::case_when(
          clinical_pre & functional_post ~ "Improved",
          !clinical_pre & !functional_post ~ "Deteriorated",
          !(clinical_pre & functional_post) &
            !(!clinical_pre & !functional_post) ~ "Unchanged"
        ),
        category = factor(
          category,
          levels = c("Improved", "Unchanged", "Deteriorated")
        )
      )
  } else {
    joined_data |>
      dplyr::mutate(
        category = dplyr::case_when(
          functional_post ~ "Improved",
          clinical_post ~ "Deteriorated",
          !functional_post & !clinical_post ~ "Unchanged"
        ),
        category = factor(
          category,
          levels = c("Improved", "Unchanged", "Deteriorated")
        )
      )
  }
}


#' Extract Augmented Data from a cs_combined Object
#'
#' @rdname augmented_data
#' @export
cs_get_augmented_data.cs_combined <- function(x, ...) {
  cs_method <- x[["method"]]
  categories <- x[["summary_table"]][["categories"]]

  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  categories |>
    # dplyr::rename(rci_indiv = rci) |>
    dplyr::mutate(
      category = dplyr::case_when(
        recovered ~ "Recovered",
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated",
        harmed ~ "Harmed"
      ),
      category = factor(
        category,
        levels = c(
          "Recovered",
          "Improved",
          "Unchanged",
          "Deteriorated",
          "Harmed"
        )
      )
    )
}


#' Extract Augmented Data from a cs_percentage Object
#'
#' @rdname augmented_data
#' @export
cs_get_augmented_data.cs_percentage <- function(x, ...) {
  pct_categories <- x[["pct_results"]]
  used_data <- x[["datasets"]][["data"]]

  used_data |>
    dplyr::left_join(pct_categories, dplyr::join_by("id")) |>
    dplyr::mutate(
      category = dplyr::case_when(
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated"
      ),
      category = factor(
        category,
        levels = c("Improved", "Unchanged", "Deteriorated")
      )
    )
}


#' Extract Augmented Data from a cs_anchor_individual Object
#'
#' @rdname augmented_data
#' @export
cs_get_augmented_data.cs_anchor_individual <- function(x, ...) {
  anchor_categories <- x[["anchor_results"]][["data"]]
  used_data <- x[["datasets"]][["data"]]

  used_data |>
    dplyr::left_join(anchor_categories, dplyr::join_by("id")) |>
    dplyr::mutate(
      category = dplyr::case_when(
        improved ~ "Improved",
        unchanged ~ "Unchanged",
        deteriorated ~ "Deteriorated"
      ),
      category = factor(
        category,
        levels = c("Improved", "Unchanged", "Deteriorated")
      )
    )
}
