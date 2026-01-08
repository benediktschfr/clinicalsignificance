# Testdaten Setup
test_data_comb <- tibble::tribble(
  ~id , ~time , ~score , ~group  ,
    1 ,     1 ,     30 , "Treat" ,
    1 ,     2 ,     10 , "Treat" , # Improved
    2 ,     1 ,     30 , "Treat" ,
    2 ,     2 ,     30 , "Treat" # Unchanged
)

test_that("cs_combined validation: Reliability Logic", {
  # 1. Standard Fall (JT): Braucht Reliability
  expect_error(
    cs_combined(test_data_comb, id, time, score),
    "Argument `reliability` is required"
  )

  # 3. Anchor Fall (mid_improvement gesetzt): Braucht KEINE Reliability für den Change-Part
  # Aber Achtung: calc_cutoff_from_data braucht evtl. reliability für Adjustment?
  # Falls deine cutoff Berechnung reliability braucht, würde der Fehler dort fliegen.
  # Aber cs_combined selbst sollte es durchlassen.
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

  # Mit Stats geht es
  expect_silent(
    capture.output(
      cs_combined(
        test_data_comb,
        id,
        time,
        score,
        reliability = 0.8,
        cutoff_type = "c",
        m_functional = 10,
        sd_functional = 2
      )
    )
  )
})

test_that("cs_combined: Logic Branching (Anchor vs Distribution)", {
  # Case A: Distribution based (JT)
  res_dist <- cs_combined(
    test_data_comb,
    id,
    time,
    score,
    reliability = 0.8,
    rci_method = "JT"
  )
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
  expect_s3_class(res_anchor$datasets, "cs_anchor_individual_within")

  # Check, ob mid_deterioration default greift
  expect_equal(res_anchor$mid_deterioration, 5)
})

test_that("cs_combined snapshots", {
  # Snapshot Standard
  res <- cs_combined(test_data_comb, id, time, score, reliability = 0.8)
  expect_snapshot(print(res))
  expect_snapshot(summary(res))
})
