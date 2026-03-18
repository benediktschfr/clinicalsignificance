# Testdaten Setup (erweitert für alle Kategorien: + / - / 0)
test_data_comb <- tibble::tribble(
  ~id , ~time , ~score , ~group  ,
    1 ,     1 ,     30 , "Treat" ,
    1 ,     2 ,     10 , "Treat" , # Improved / Recovered
    2 ,     1 ,     30 , "Treat" ,
    2 ,     2 ,     30 , "Treat" , # Unchanged
    3 ,     1 ,     10 , "Treat" ,
    3 ,     2 ,     30 , "Treat" # Deteriorated / Harmed
)

# --- VALIDATION TESTS ---

test_that("cs_combined validation: Reliability Logic", {
  # 1. Standard Fall (JT): Braucht Reliability
  expect_error(
    cs_combined(test_data_comb, id, time, score),
    "Argument `reliability` is required"
  )

  # 2. Anchor Fall (mid_improvement gesetzt): Braucht KEINE Reliability für den Change-Part
  # Wenn cutoff "a" (default) greift, wird auch für den Cutoff keine reliability/functional stats benötigt.
  expect_silent(
    capture.output(
      cs_combined(test_data_comb, id, time, score, mid_improvement = 5)
    )
  )
})

test_that("cs_combined validation: Cutoff Logic", {
  # Cutoff b/c brauchen Functional Stats
  expect_error(
    cs_combined(
      test_data_comb,
      id,
      time,
      score,
      reliability = 0.8,
      cutoff_type = "c"
    ),
    "Functional population statistics missing"
  )

  # Mit Stats geht es, liefert aber eine korrekte Info-Message bezüglich JT & Reliability
  expect_message(
    cs_combined(
      test_data_comb,
      id,
      time,
      score,
      reliability = 0.8,
      cutoff_type = "c",
      m_functional = 10,
      sd_functional = 2
    ),
    "is not used for the JT cutoff calculation"
  )
})

test_that("cs_combined: Logic Branching (Anchor vs Distribution)", {
  # Case A: Distribution based (JT)
  res_dist <- suppressMessages(cs_combined(
    test_data_comb,
    id,
    time,
    score,
    reliability = 0.8,
    rci_method = "JT"
  ))
  expect_equal(res_dist$method, "JT")
  expect_s3_class(res_dist$datasets, "cs_jt")

  # Case B: Anchor based (CWB)
  # Wenn mid_improvement gesetzt ist, wird rci_method ignoriert/überschrieben
  res_anchor <- cs_combined(
    test_data_comb,
    id,
    time,
    score,
    mid_improvement = 5,
    rci_method = "JT"
  )

  expect_equal(res_anchor$method, "CWB")
  expect_s3_class(res_anchor$datasets, "cs_anchor_individual")

  # Check, ob mid_deterioration default greift
  expect_equal(res_anchor$mid_deterioration, 5)
})


# --- NEUE TESTS FÜR SENSITIVITÄTSANALYSE (GRID) ---

test_that("cs_combined sensitivity analysis creates a full grid (Distribution)", {
  res <- suppressMessages(cs_combined(
    test_data_comb,
    id,
    time,
    score,
    reliability = c(0.7, 0.8), # 2 Werte
    m_functional = c(10, 15, 20), # 3 Werte
    sd_functional = 5, # 1 Wert
    cutoff_type = "c"
  ))

  expect_s3_class(res, "cs_combined_sensitivity")
  expect_s3_class(res, "cs_analysis")

  # Grid sollte 2 * 3 = 6 Modelle haben
  # combined Ansatz hat 5 Kategorien: Recovered, Improved, Unchanged, Deteriorated, Harmed
  # 6 Modelle * 5 Kategorien = 30 Zeilen in der summary_table
  expect_equal(nrow(res$summary_table), 30)
})

test_that("cs_combined sensitivity analysis handles symmetric default (Anchor)", {
  res <- cs_combined(
    test_data_comb,
    id,
    time,
    score,
    mid_improvement = c(4, 6), # 2 Werte, deterioration is NULL
    m_functional = c(10, 15), # 2 Werte
    sd_functional = 5,
    cutoff_type = "c"
  )

  expect_s3_class(res, "cs_combined_sensitivity")
  expect_equal(res$method, "CWB")

  # Grid: 2 * 2 = 4 Modelle
  # 4 * 5 Kategorien = 20 Zeilen
  expect_equal(nrow(res$summary_table), 20)

  # Die originalen Inputs im Ausgabeobjekt
  expect_null(res$mid_deterioration)
  expect_equal(res$mid_improvement, c(4, 6))

  # In der internen Output-Tabelle wurden die NA durch Symmetrie (4, 6) gefüllt
  expect_equal(unique(res$summary_table$mid_deterioration), c(4, 6))
})


# --- SNAPSHOT TESTS ---

test_that("cs_combined snapshots (Print/Summary)", {
  # Standard Distribution (JT)
  res_dist <- suppressMessages(cs_combined(
    test_data_comb,
    id,
    time,
    score,
    reliability = 0.8
  ))
  expect_snapshot(print(res_dist))
  expect_snapshot(summary(res_dist))

  # Standard Anchor (CWB)
  res_anchor <- cs_combined(
    test_data_comb,
    id,
    time,
    score,
    mid_improvement = 5
  )
  expect_snapshot(print(res_anchor))
  expect_snapshot(summary(res_anchor))

  # Sensitivität Distribution
  res_sens_dist <- suppressMessages(cs_combined(
    test_data_comb,
    id,
    time,
    score,
    reliability = c(0.7, 0.9),
    m_functional = 10,
    sd_functional = 5,
    cutoff_type = "c"
  ))
  expect_snapshot(print(res_sens_dist))
  expect_snapshot(summary(res_sens_dist))

  # Sensitivität Anchor
  res_sens_anchor <- cs_combined(
    test_data_comb,
    id,
    time,
    score,
    mid_improvement = c(3, 7),
    m_functional = 10,
    sd_functional = 5,
    cutoff_type = "c"
  )
  expect_snapshot(print(res_sens_anchor))
  expect_snapshot(summary(res_sens_anchor))
})
