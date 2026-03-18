# Minimale Testdaten:
# ID 1: 100 -> 40 (60% Reduktion -> Verbesserung)
# ID 2: 100 -> 90 (10% Reduktion -> Keine Änderung bei 20% Cutoff)
# ID 3: 100 -> 150 (50% Erhöhung -> Verschlechterung)
test_data_pct <- tibble::tibble(
  id = rep(1:3, 2),
  time = rep(c(1, 2), each = 3),
  score = c(100, 100, 100, 40, 90, 150),
  group = "Treat"
)

test_that("cs_percentage input validation catches errors", {
  # 1. Missing Argument
  expect_error(
    cs_percentage(test_data_pct, id, time, score),
    "Argument `pct_improvement` is missing"
  )

  # 2. Der "Smart Check" für Werte > 1 (Dein neuer Code block)
  expect_error(
    cs_percentage(test_data_pct, id, time, score, pct_improvement = 20),
    "Did you mean .*0.2.*" # Prüft, ob der hilfreiche Hinweis kommt (Regex)
  )

  # 3. Typ-Check (String statt Zahl)
  expect_error(
    cs_percentage(test_data_pct, id, time, score, pct_improvement = "twenty"),
    "Must be of type 'numeric'"
  )

  # 4. Range Check (Negativ)
  expect_error(
    cs_percentage(test_data_pct, id, time, score, pct_improvement = -0.5),
    "Element 1 is not >= 0"
  )

  # 5. pct_deterioration Check
  expect_error(
    cs_percentage(
      test_data_pct,
      id,
      time,
      score,
      pct_improvement = 0.2,
      pct_deterioration = 50
    ),
    "Element 1 is not <= 1" # Hier greift der Standard checkmate Fehler, da wir dort keinen Custom Hint haben
  )
})


test_that("cs_percentage returns correct structure and defaults", {
  res <- cs_percentage(
    test_data_pct,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    pct_improvement = 0.3 # 30%
  )

  # Klassen
  expect_s3_class(res, "cs_analysis")
  expect_s3_class(res, "cs_percentage")

  # Namen
  expect_named(
    res,
    c(
      "datasets",
      "pct_results",
      "outcome",
      "n_obs",
      "pct_improvement",
      "pct_deterioration",
      "direction",
      "summary_table"
    )
  )

  # Default Logik: Deterioration sollte gleich Improvement sein
  expect_equal(res$pct_deterioration, 0.3)

  # Direction Check (Default "lower" -> -1)
  expect_equal(res$direction, -1)
})

test_that("cs_percentage print output is stable", {
  set.seed(123)
  res <- cs_percentage(
    test_data_pct,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    pct_improvement = 0.2
  )

  expect_snapshot(print(res))
  expect_snapshot(summary(res))
})

# --- NEUE TESTS FÜR ERWEITERTE FUNKTIONALITÄT ---

test_that("cs_percentage handles better_is = 'higher'", {
  res <- cs_percentage(
    test_data_pct,
    id,
    time,
    score,
    pct_improvement = 0.2,
    better_is = "higher"
  )
  expect_equal(res$direction, 1)
})

test_that("cs_percentage handles groups correctly", {
  test_data_grouped <- test_data_pct |>
    dplyr::mutate(group_var = rep(c("A", "B", "A"), 2))

  res <- cs_percentage(
    test_data_grouped,
    id,
    time,
    score,
    group = group_var,
    pct_improvement = 0.2
  )

  expect_true("group" %in% names(res$summary_table))
  expect_equal(nrow(res$summary_table), 6) # 2 Gruppen * 3 Kategorien
})

test_that("cs_percentage sensitivity analysis works and builds correct class", {
  res <- cs_percentage(
    test_data_pct,
    id,
    time,
    score,
    pct_improvement = c(0.2, 0.3, 0.4)
  )

  expect_s3_class(res, "cs_percentage_sensitivity")
  expect_s3_class(res, "cs_analysis")

  # Wir erwarten 3 Modelle * 3 Kategorien = 9 Zeilen in der internen Tabelle
  expect_equal(nrow(res$summary_table), 9)
})

test_that("cs_percentage sensitivity analysis recycles correctly with expand_grid", {
  res <- cs_percentage(
    test_data_pct,
    id,
    time,
    score,
    pct_improvement = c(0.2, 0.3),
    pct_deterioration = c(0.4, 0.5)
  )

  # Grid: 2 * 2 = 4 Modelle. 4 * 3 Kategorien = 12 Zeilen
  expect_equal(nrow(res$summary_table), 12)
})

test_that("cs_percentage_sensitivity print and summary outputs are stable", {
  set.seed(123)
  res <- cs_percentage(
    test_data_pct,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    pct_improvement = seq(0.1, 0.3, by = 0.1)
  )

  expect_snapshot(print(res))
  expect_snapshot(summary(res))
})
