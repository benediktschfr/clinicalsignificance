test_that("cs_anchor_group works for within-group (default)", {
  # Mock Data
  df <- data.frame(
    id = rep(1:10, 2),
    time = rep(c(1, 2), each = 10),
    score = rnorm(20),
    group = rep(c("A", "B"), each = 5, times = 2)
  )

  res <- cs_anchor_group(
    df,
    id,
    time,
    score,
    mid_improvement = 0.5,
    pre = 1,
    post = 2
  )

  expect_s3_class(res, "cs_anchor_group_within")
  expect_equal(res$bayesian, TRUE)

  # Summary table handling
  # Bei "within" gibst du 'list' als dataset zurück laut deinem Code
  expect_type(res$datasets, "list")
})

test_that("cs_anchor_group works for between-group analysis", {
  df <- data.frame(
    id = rep(1:20, 2),
    time = rep(c("Pre", "Post"), each = 20),
    score = rnorm(40),
    treatment = rep(c("Control", "Intervention"), each = 10, times = 2)
  )

  res <- cs_anchor_group(
    df,
    id,
    time,
    score,
    group = treatment,
    mid_improvement = 0.5,
    effect = "between",
    post = "Post"
  )

  expect_s3_class(res, "cs_anchor_group_between")

  # Check Data preparation for between
  # Laut deinem Code: class(datasets) <- c("tbl_df", ...)
  expect_s3_class(res$datasets, "data.frame")
})

test_that("cs_anchor_group handles frequentist approach", {
  df <- data.frame(
    id = rep(1:5, 2),
    time = rep(1:2, each = 5),
    score = rnorm(10)
  )

  res <- cs_anchor_group(
    df,
    id,
    time,
    score,
    mid_improvement = 0.5,
    bayesian = FALSE,
    pre = 1,
    post = 2
  )

  expect_equal(res$bayesian, FALSE)
})

test_that("cs_anchor_group validation logic", {
  df <- data.frame(id = 1, time = 1, score = 1, grp = "A")

  # Between group requested but no group column supplied
  expect_error(
    cs_anchor_group(
      df,
      id,
      time,
      score,
      mid_improvement = 1,
      effect = "between",
      post = 1
    ),
    "Argument `group` is missing"
  )
})
