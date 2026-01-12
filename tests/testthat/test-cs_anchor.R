test_that("cs_anchor works for basic improvement calculation", {
  # Mock Data: Improvement is better (higher scores are better here for simplicity)
  df <- data.frame(
    id = c(1, 2, 3),
    time = c(1, 1, 1, 2, 2, 2),
    score = c(10, 20, 30, 15, 20, 40), # 1: +5, 2: 0, 3: +10
    group = "A"
  )

  # MID = 3.
  # Patient 1 (+5) -> Improved
  # Patient 2 (0) -> Unchanged
  # Patient 3 (+10) -> Improved

  res <- cs_anchor(
    data = df,
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
  expect_true("tbl_df" %in% class(res$anchor_results$data))
  expect_equal(nrow(res$anchor_results$data), 3)
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
  # Hier würde man idealerweise prüfen, ob die Kategorien stimmen,
  # das hängt aber von deiner internen calc_anchor Logik ab.
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
    "Argument `id` is missing."
  )

  # Missing Time
  expect_error(
    cs_anchor(df, id = id, outcome = score),
    "Argument `time` is missing"
  )

  # Invalid MID
  expect_error(
    cs_anchor(df, id, time, score, mid_improvement = "A"),
    "Must be of type 'number'"
  )
  expect_error(cs_anchor(df, id, time, score, mid_improvement = -5), ">= 0")
})
