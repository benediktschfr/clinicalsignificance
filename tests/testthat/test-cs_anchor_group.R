# Deterministische Mock-Daten mit etwas Varianz (für t.test / ttestBF zwingend nötig)
test_data_within <- data.frame(
  id = rep(1:10, 2),
  time = rep(c(1, 2), each = 10),
  score = c(
    20,
    21,
    19,
    20,
    22,
    20,
    18,
    21,
    19,
    20, # Pre
    10,
    12,
    8,
    11,
    11,
    15,
    14,
    16,
    15,
    15 # Post (Grp A drops vary: -10, -9, -11, -9, -11)
  ),
  group = rep(c("A", "B"), each = 5, times = 2)
)

test_data_between <- data.frame(
  id = rep(1:20, 2),
  time = rep(c("Pre", "Post"), each = 20),
  score = c(
    rep(20, 20), # Pre
    10,
    11,
    9,
    10,
    12,
    10,
    11,
    9,
    10,
    8, # Post Intervention (drops ~10)
    18,
    19,
    17,
    18,
    20,
    18,
    19,
    17,
    18,
    16 # Post Control (drops ~2)
  ),
  treatment = rep(c("Intervention", "Control"), each = 10, times = 2)
)

# --- STANDARD TESTS ---

test_that("cs_anchor_group works for within-group (default)", {
  res <- cs_anchor_group(
    test_data_within,
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
  # Bei "within" ist das Datenset eine 'list'
  expect_type(res$datasets, "list")
})

test_that("cs_anchor_group works for between-group analysis", {
  res <- cs_anchor_group(
    test_data_between,
    id,
    time,
    score,
    group = treatment,
    mid_improvement = 0.5,
    effect = "between",
    post = "Post"
  )

  expect_s3_class(res, "cs_anchor_group_between")
  # Dataset sollte ein normales data.frame/tibble sein (nicht als Liste verschachtelt wie bei within)
  expect_s3_class(res$datasets, "data.frame")
})

test_that("cs_anchor_group handles frequentist approach", {
  res <- cs_anchor_group(
    test_data_within,
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

# --- NEUE TESTS FÜR SENSITIVITÄTSANALYSE (GRID) ---

test_that("cs_anchor_group sensitivity analysis creates a full grid (within)", {
  res <- cs_anchor_group(
    test_data_within,
    id,
    time,
    score,
    group = group,
    mid_improvement = c(2, 4), # 2 Werte
    mid_deterioration = c(1, 3, 5), # 3 Werte
    pre = 1,
    post = 2,
    effect = "within"
  )

  expect_s3_class(res, "cs_anchor_group_within_sensitivity")
  expect_s3_class(res, "cs_analysis")

  # Grid sollte 2 * 3 = 6 Modelle haben
  # 2 Gruppen ("A" und "B") = 2 Zeilen pro Modell
  # 6 * 2 = 12 Zeilen in den anchor_results
  expect_equal(nrow(res$anchor_results), 12)
})

test_that("cs_anchor_group sensitivity handles symmetric default (between)", {
  res <- cs_anchor_group(
    test_data_between,
    id,
    time,
    score,
    group = treatment,
    mid_improvement = c(2, 4, 6), # 3 Werte, deterioration is NULL
    effect = "between",
    post = "Post"
  )

  expect_s3_class(res, "cs_anchor_group_between_sensitivity")

  # Grid sollte 3 Modelle haben, da deterioration NULL war und symmetrisch angepasst wird.
  # 1 Comparison pro Modell = 3 Zeilen in den anchor_results
  expect_equal(nrow(res$anchor_results), 3)

  # Die originalen Inputs im Ausgabeobjekt sollten NULL für deterioration bleiben
  expect_null(res$mid_deterioration)
  expect_equal(res$mid_improvement, c(2, 4, 6))

  # In der internen Output-Tabelle wurden die NA durch Symmetrie (2, 4, 6) gefüllt
  expect_equal(unique(res$anchor_results$mid_deterioration), c(2, 4, 6))
})

# --- SNAPSHOT TESTS ---

test_that("cs_anchor_group snapshots (Print/Summary)", {
  set.seed(123)

  # Normale Ausgaben
  res_within <- cs_anchor_group(
    test_data_within,
    id,
    time,
    score,
    group = group,
    pre = 1,
    post = 2,
    mid_improvement = 5,
    effect = "within"
  )
  expect_snapshot(print(res_within))
  expect_snapshot(summary(res_within))

  res_between <- cs_anchor_group(
    test_data_between,
    id,
    time,
    score,
    group = treatment,
    post = "Post",
    mid_improvement = 5,
    effect = "between"
  )
  expect_snapshot(print(res_between))
  expect_snapshot(summary(res_between))

  # Sensitivitäts-Ausgaben (Within)
  res_sens_within <- cs_anchor_group(
    test_data_within,
    id,
    time,
    score,
    group = group,
    pre = 1,
    post = 2,
    mid_improvement = c(3, 5),
    effect = "within"
  )
  expect_snapshot(print(res_sens_within))
  expect_snapshot(summary(res_sens_within))

  # Sensitivitäts-Ausgaben (Between)
  res_sens_between <- cs_anchor_group(
    test_data_between,
    id,
    time,
    score,
    group = treatment,
    post = "Post",
    mid_improvement = c(3, 10),
    effect = "between"
  )
  expect_snapshot(print(res_sens_between))
  expect_snapshot(summary(res_sens_between))
})
