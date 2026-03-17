# Minimale Testdaten für cs_anchor
test_data_anchor <- data.frame(
  id = rep(1:3, 2),
  time = rep(c(1, 2), each = 3),
  score = c(10, 20, 30, 15, 20, 40), # Change: +5, 0, +10
  group = "A"
)

test_that("cs_anchor works for basic improvement calculation", {
  # MID = 3.
  # Patient 1 (+5) -> Improved
  # Patient 2 (0) -> Unchanged
  # Patient 3 (+10) -> Improved

  res <- cs_anchor(
    data = test_data_anchor,
    id = id,
    time = time,
    outcome = score,
    mid_improvement = 3,
    better_is = "higher",
    pre = 1,
    post = 2
  )

  expect_s3_class(res, "cs_anchor_individual")
  expect_s3_class(res, "cs_analysis")

  # Check structure
  expect_equal(res$mid_improvement, 3)
  expect_equal(res$direction, 1) # 1 because better_is = "higher"

  # Check if result table exists and has correct rows
  expect_true("tbl_df" %in% class(res$anchor_results))
  expect_equal(nrow(res$anchor_results), 3)
})

test_that("cs_anchor handles 'better_is = lower' correctly", {
  df <- data.frame(
    id = c(1, 2),
    time = c("Pre", "Pre", "Post", "Post"),
    score = c(20, 20, 10, 25) # 1: -10 (Improved), 2: +5 (Deteriorated)
  )

  res <- cs_anchor(
    df,
    id,
    time,
    score,
    mid_improvement = 5,
    better_is = "lower",
    pre = "Pre",
    post = "Post"
  )

  expect_equal(res$direction, -1)
})

test_that("cs_anchor uses distinct mid_deterioration if provided", {
  df <- data.frame(
    id = 1,
    time = c(1, 2),
    score = c(10, 5) # Change -5
  )

  res <- cs_anchor(
    df,
    id,
    time,
    score,
    mid_improvement = 2,
    mid_deterioration = 10, # Large deterioration threshold
    better_is = "higher"
  )

  expect_equal(res$mid_deterioration, 10)
  expect_equal(res$mid_improvement, 2)
})

test_that("cs_anchor input validation triggers errors", {
  df <- data.frame(id = 1, time = 1, score = 1)

  # Missing ID
  expect_error(
    cs_anchor(df, time = time, outcome = score),
    "Argument `id` is missing"
  )

  # Missing Time
  expect_error(
    cs_anchor(df, id = id, outcome = score),
    "Argument `time` is missing"
  )

  # Invalid MID
  expect_error(
    cs_anchor(df, id, time, score, mid_improvement = "A"),
    "Must be of type 'numeric'"
  )
  expect_error(cs_anchor(df, id, time, score, mid_improvement = -5), ">= 0")
})

# --- NEUE TESTS FÜR SENSITIVITÄTSANALYSE (GRID) ---

test_that("cs_anchor sensitivity analysis creates a full grid", {
  res <- cs_anchor(
    test_data_anchor,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    mid_improvement = c(2, 4), # 2 Werte
    mid_deterioration = c(1, 3, 5), # 3 Werte
    better_is = "higher"
  )

  expect_s3_class(res, "cs_anchor_individual_sensitivity")
  expect_s3_class(res, "cs_analysis")

  # Grid sollte 2 * 3 = 6 Modelle haben
  # Jedes Modell produziert 3 Zeilen in der summary_table (Improved, Unchanged, Deteriorated)
  # 6 * 3 = 18 Zeilen insgesamt
  expect_equal(nrow(res$summary_table), 18)
})

test_that("cs_anchor sensitivity analysis handles symmetric default (NULL)", {
  res <- cs_anchor(
    test_data_anchor,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    mid_improvement = c(2, 4, 6), # 3 Werte, deterioration is NULL
    better_is = "higher"
  )

  expect_s3_class(res, "cs_anchor_individual_sensitivity")

  # Grid sollte 3 Modelle haben, da deterioration NULL war und symmetrisch angepasst wird.
  # 3 * 3 Kategorien = 9 Zeilen
  expect_equal(nrow(res$summary_table), 9)

  # Die originalen Inputs im Ausgabeobjekt sollten NULL für deterioration bleiben (für die Summary-Method-Logik)
  expect_null(res$mid_deterioration)
  expect_equal(res$mid_improvement, c(2, 4, 6))

  # Die tatsächlichen Werte in der internen Tabelle (Spalte mid_deterioration) sollten jedoch aufgefüllt sein (2, 4, 6)
  expect_equal(unique(res$summary_table$mid_deterioration), c(2, 4, 6))
})

test_that("cs_anchor_individual_sensitivity snapshots (Print/Summary)", {
  res_sens <- cs_anchor(
    test_data_anchor,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    mid_improvement = c(2, 5),
    better_is = "higher"
  )

  expect_snapshot(print(res_sens))
  expect_snapshot(summary(res_sens))
})
