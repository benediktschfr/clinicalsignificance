# Minimale Testdaten:
# ID 1: 100 -> 40 (60% Reduktion -> Verbesserung)
# ID 2: 100 -> 90 (10% Reduktion -> Keine Änderung bei 20% Cutoff)
# ID 3: 100 -> 150 (50% Erhöhung -> Verschlechterung)
test_data_pct <- tibble::tibble(
  id = rep(1:3, 2), # <--- Das hier hat gefehlt (Länge 6: 1,2,3,1,2,3)
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
  expect_equal(nrow(res$summary_table), 6) # Three rows for A, three for B
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

  # We expect 9 rows for summary table, three rows per pct_improvement value
  expect_equal(nrow(res$summary_table), 9)
})

test_that("cs_percentage sensitivity catches length mismatch", {
  expect_error(
    cs_percentage(
      test_data_pct,
      id,
      time,
      score,
      pct_improvement = c(0.2, 0.3),
      pct_deterioration = c(0.2, 0.3, 0.4)
    ),
    "Lengths of `pct_improvement` and `pct_deterioration` must match"
  )
})

test_that("cs_percentage sensitivity recycles pct_deterioration correctly", {
  res <- cs_percentage(
    test_data_pct,
    id,
    time,
    score,
    pct_improvement = c(0.2, 0.3, 0.4),
    pct_deterioration = 0.5 # Skalar sollte auf Länge 3 recycled werden
  )

  expect_equal(res$pct_deterioration, c(0.5, 0.5, 0.5))
})

test_that("cs_percentage_sensitivity print and summary outputs are stable", {
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
