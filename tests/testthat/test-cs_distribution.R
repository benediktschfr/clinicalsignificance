# Deterministische Testdaten für stabile Snapshots
test_data_dist <- data.frame(
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
    15 # Post (einige starke, einige schwache Verbesserungen)
  ),
  group = rep(c("A", "B"), each = 5, times = 2)
)

# --- VALIDATION TESTS ---

test_that("cs_distribution input validation", {
  # Fehlende Argumente
  expect_error(
    cs_distribution(test_data_dist, time = time, outcome = score),
    "Argument `id` is missing"
  )

  # Reliability fehlt (für Standard-Methoden)
  expect_error(
    cs_distribution(test_data_dist, id, time, score, rci_method = "JT"),
    "Argument `reliability` is required"
  )
})

# --- STANDARD TESTS ---

test_that("cs_distribution works for standard calculation", {
  res <- cs_distribution(
    test_data_dist,
    id,
    time,
    score,
    reliability = 0.8
  )

  expect_s3_class(res, "cs_distribution")
  expect_s3_class(res, "cs_analysis")
  expect_equal(res$method, "JT")
})

# --- NEUE TESTS FÜR SENSITIVITÄTSANALYSE (GRID) ---

test_that("cs_distribution sensitivity analysis creates a full grid", {
  res <- cs_distribution(
    test_data_dist,
    id,
    time,
    score,
    reliability = c(0.7, 0.8, 0.9) # 3 Werte
  )

  expect_s3_class(res, "cs_distribution_sensitivity")
  expect_s3_class(res, "cs_analysis")

  # Grid sollte 3 Modelle haben.
  # Jedes Modell hat 3 Kategorien (Improved, Unchanged, Deteriorated).
  # 3 * 3 = 9 Zeilen in der internen summary_table
  expect_equal(nrow(res$summary_table), 9)
})

test_that("cs_distribution sensitivity analysis creates correct grid with groups", {
  res <- cs_distribution(
    test_data_dist,
    id,
    time,
    score,
    group = group,
    reliability = c(0.7, 0.8) # 2 Werte
  )

  expect_s3_class(res, "cs_distribution_sensitivity")

  # Grid sollte 2 Modelle haben.
  # Jedes Modell hat 2 Gruppen mit je 3 Kategorien = 6 Zeilen pro Modell.
  # 2 * 6 = 12 Zeilen in der internen summary_table
  expect_equal(nrow(res$summary_table), 12)
})

# --- SNAPSHOT TESTS ---

test_that("cs_distribution snapshots (Print/Summary)", {
  # Seed setzen für absolute Sicherheit (auch wenn hier meist kein MCMC läuft)
  set.seed(123)

  # Standard
  res_std <- cs_distribution(
    test_data_dist,
    id,
    time,
    score,
    reliability = 0.8,
    pre = 1,
    post = 2
  )
  expect_snapshot(print(res_std))
  expect_snapshot(summary(res_std))

  # Sensitivität (ohne Gruppe)
  res_sens <- cs_distribution(
    test_data_dist,
    id,
    time,
    score,
    reliability = c(0.7, 0.8, 0.9),
    pre = 1,
    post = 2
  )
  expect_snapshot(print(res_sens))
  expect_snapshot(summary(res_sens))

  # Sensitivität (mit Gruppe)
  res_sens_grp <- cs_distribution(
    test_data_dist,
    id,
    time,
    score,
    group = group,
    reliability = c(0.7, 0.9),
    pre = 1,
    post = 2
  )
  expect_snapshot(print(res_sens_grp))
  expect_snapshot(summary(res_sens_grp))
})
