#' Generic For Creating Summary Tables
#'
#' This is an internal function used to create summary table for the different
#' clinical significance methods. This should not be used directly.
#'
#' @param x An object of class `cs_*`
#' @param ... Additional arguments passed to other methods
#'
#' @return A tibble
#'
#' @keywords internal
#' @export
create_summary_table <- function(x, ...) {
  UseMethod("create_summary_table")
}


#' Create Summary Table for Distribution-Based Approach
#'
#' @param x A results object that differs per approach:
#'   - RCI results for distribution-based and combined approach
#'   - Cutoff results for statistical approach
#'   - PCT results for percentage-change approach
#'   - Anchor results for the anchor-based approach
#' @param data The used data frame
#' @param ... Additional arguments
#'
#' @keywords internal
#' @export
create_summary_table.cs_distribution <- function(x, data, ...) {
  # Get the RCI results as well as the used data (needed if grouped results are
  # required)
  rci_results <- x[["data"]]
  used_data <- data[["data"]]

  # Check if data has a group column
  if (.has_group(used_data)) {
    group_var <- as.symbol("group")
  } else {
    group_var <- NULL
  }

  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(rci_results, dplyr::join_by("id"))

  # Count all cases per category and calculate relative amount (percentages)
  summary <- joined_data |>
    dplyr::summarise(
      dplyr::across(improved:unchanged, sum),
      .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = improved:unchanged,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = round(n / sum(n), digits = 4),
      category = tools::toTitleCase(category),
      category = factor(
        category,
        levels = c("Improved", "Unchanged", "Deteriorated")
      )
    )

  if (!.has_group(used_data)) {
    dplyr::arrange(summary, category)
  } else {
    dplyr::arrange(summary, group, category) |>
      dplyr::mutate(
        percent_by_group = round(n / sum(n), digits = 4),
        .by = tidyr::all_of(group_var)
      )
  }
}


#' Create Summary Table for Statistical Approach
#'
#' @param method Clinical significance method
#'
#' @keywords internal
#' @export
create_summary_table.cs_statistical <- function(x, data, method, ...) {
  # Get the cutoff results as well as the used data (needed if grouped results
  # are required)
  cutoff_results <- x[["data"]]
  used_data <- data[["data"]]

  # Check if data has a group column
  if (.has_group(used_data)) {
    group_var <- as.symbol("group")
  } else {
    group_var <- NULL
  }

  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(cutoff_results, dplyr::join_by("id"))

  # Count all cases per category and calculate relative amount (percentages)
  if (method != "HA") {
    categories <- joined_data |>
      dplyr::mutate(
        improved = ifelse(clinical_pre & functional_post, TRUE, FALSE),
        deteriorated = ifelse(!clinical_pre & !functional_post, TRUE, FALSE),
        unchanged = ifelse(!improved & !deteriorated, TRUE, FALSE)
      )
  } else {
    categories <- joined_data |>
      dplyr::rename(improved = functional_post, deteriorated = clinical_post) |>
      dplyr::mutate(unchanged = !improved & !deteriorated)
  }

  # Create summary table
  summary <- categories |>
    dplyr::summarise(
      dplyr::across(improved:unchanged, sum),
      .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = improved:unchanged,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = round(n / sum(n), digits = 4),
      category = tools::toTitleCase(category),
      category = factor(
        category,
        levels = c("Improved", "Unchanged", "Deteriorated")
      )
    )

  if (!.has_group(used_data)) {
    dplyr::arrange(summary, category)
  } else {
    dplyr::arrange(summary, group, category) |>
      dplyr::mutate(
        percent_by_group = round(n / sum(n), digits = 4),
        .by = tidyr::all_of(group_var)
      )
  }
}


#' Create Summary Table for Combined Approach
#'
#' @param cutoff_results Cutoff results object
#' @param r_dd r_dd
#' @param se_measurement se_measurement
#' @param cutoff Cutoff value
#' @param sd_post SD post intervention
#' @param direction Which direction is beneficial? 1 = higher, -1 = lower
#'
#' @keywords internal
#' @export
create_summary_table.cs_combined <- function(
  x,
  cutoff_results,
  data,
  method,
  r_dd,
  se_measurement,
  cutoff,
  sd_post,
  direction,
  ...
) {
  # Get all results as well as the used data (needed if grouped results are
  # required)
  rci_results <- x[["data"]]
  cutoff_results <- cutoff_results[["data"]]
  used_data <- data[["data"]]

  # Check if data has a group column
  if (.has_group(used_data)) {
    group_var <- as.symbol("group")
  } else {
    group_var <- NULL
  }

  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(cutoff_results, dplyr::join_by("id")) |>
    dplyr::left_join(rci_results, dplyr::join_by("id"))

  # Count all cases per category and calculate relative amount (percentages)
  categories <- joined_data |>
    dplyr::mutate(
      recovered = clinical_pre & functional_post & improved,
      improved = ifelse(recovered, FALSE, improved),
      harmed = !clinical_pre & !functional_post & deteriorated,
      deteriorated = ifelse(harmed, FALSE, deteriorated)
    ) |>
    dplyr::relocate(
      clinical_pre,
      functional_post,
      recovered,
      .before = improved
    ) |>
    dplyr::relocate(unchanged, .after = improved) |>
    dplyr::select(-c(clinical_pre, functional_post))

  # Create summary table
  summary <- categories |>
    dplyr::summarise(
      dplyr::across(recovered:harmed, sum),
      .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = recovered:harmed,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = round(n / sum(n), digits = 4),
      category = tools::toTitleCase(category),
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

  if (!.has_group(used_data)) {
    individual_level_summary <- dplyr::arrange(summary, category)
  } else {
    individual_level_summary <- dplyr::arrange(summary, group, category) |>
      dplyr::mutate(
        percent_by_group = round(n / sum(n), digits = 4),
        .by = tidyr::all_of(group_var)
      )
  }
  group_level_summary <- NA

  # Create group level summary table for HA method
  if (method == "HA") {
    reliability_post <- .calc_reliability_ha(sd_post, se_measurement)

    summary_statistics <- used_data |>
      dplyr::summarise(
        mean_change = mean(change),
        sd_change = sd(change),
        m_post = mean(post),
        sd_post = sd(post),
        .by = tidyr::all_of(group_var)
      )

    if (direction == 1) {
      group_level_pcts <- summary_statistics |>
        dplyr::mutate(
          z_changed = (0 - mean_change) / (sd_change * sqrt(r_dd)),
          changed = 1 - stats::pnorm(z_changed),
          z_functional = (cutoff - m_post) / (sd_post * sqrt(reliability_post)),
          functional = 1 - stats::pnorm(z_functional)
        )
    } else if (direction == -1) {
      group_level_pcts <- summary_statistics |>
        dplyr::mutate(
          z_changed = (0 - mean_change) / (sd_change * sqrt(r_dd)),
          changed = stats::pnorm(z_changed),
          z_functional = (cutoff - m_post) / (sd_post * sqrt(reliability_post)),
          functional = stats::pnorm(z_functional)
        )
    }

    group_level_summary <- group_level_pcts |>
      dplyr::select(-tidyr::matches(".*_.*")) |>
      tidyr::pivot_longer(
        cols = c(changed, functional),
        names_to = "category",
        values_to = "percent"
      ) |>
      dplyr::mutate(
        category = tools::toTitleCase(category)
      )
  }

  list(
    individual_level_summary = individual_level_summary,
    group_level_summary = group_level_summary,
    categories = categories
  )
}


#' Create Summary Table for Percentage-Change Approach
#'
#' @keywords internal
#' @export
create_summary_table.cs_percentage <- function(x, data, ...) {
  # Get the percentage results as well as the used data (needed if grouped
  # results are required)
  used_data <- data[["data"]]

  # Check if data has a group column
  if (.has_group(used_data)) {
    group_var <- as.symbol("group")
  } else {
    group_var <- NULL
  }

  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(x, dplyr::join_by("id"))

  # Count all cases per category and calculate relative amount (percentages)
  summary <- joined_data |>
    dplyr::summarise(
      dplyr::across(improved:unchanged, sum),
      .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = improved:unchanged,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = round(n / sum(n), digits = 4),
      category = tools::toTitleCase(category),
      category = factor(
        category,
        levels = c("Improved", "Unchanged", "Deteriorated")
      )
    )

  if (!.has_group(used_data)) {
    dplyr::arrange(summary, category)
  } else {
    dplyr::arrange(summary, group, category) |>
      dplyr::mutate(
        percent_by_group = round(n / sum(n), digits = 4),
        .by = tidyr::all_of(group_var)
      )
  }
}


#' Create Summary Table for Anchor-Based Approach
#'
#' @keywords internal
#' @export
create_summary_table.cs_anchor_individual <- function(x, data, ...) {
  # Get the percentage results as well as the used data (needed if grouped
  # results are required)
  anchor_results <- x[["data"]]
  used_data <- data[["data"]]

  # Check if data has a group column
  if (.has_group(used_data)) {
    group_var <- as.symbol("group")
  } else {
    group_var <- NULL
  }

  # Join used data with RCI results. This results in a data frame with one
  # participant per row and associated scores, change and RCI value as well as
  # the RCI category
  joined_data <- used_data |>
    dplyr::left_join(anchor_results, dplyr::join_by("id"))

  # Count all cases per category and calculate relative amount (percentages)
  summary <- joined_data |>
    dplyr::summarise(
      dplyr::across(improved:unchanged, sum),
      .by = tidyr::all_of(group_var)
    ) |>
    tidyr::pivot_longer(
      cols = improved:unchanged,
      names_to = "category",
      values_to = "n"
    ) |>
    dplyr::mutate(
      percent = round(n / sum(n), digits = 4),
      category = tools::toTitleCase(category),
      category = factor(
        category,
        levels = c("Improved", "Unchanged", "Deteriorated")
      )
    )

  if (!.has_group(used_data)) {
    dplyr::arrange(summary, category)
  } else {
    dplyr::arrange(summary, group, category) |>
      dplyr::mutate(
        percent_by_group = round(
          n / sum(n),
          digits = 4
        ),
        .by = tidyr::all_of(group_var)
      )
  }
}
