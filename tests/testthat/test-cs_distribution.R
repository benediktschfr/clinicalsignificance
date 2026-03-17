# Standard Datensatz (2 Messzeitpunkte)
# ID 1: Verbesserung (30 -> 10)
# ID 2: Keine Änderung (30 -> 29)
test_data_dist <- tibble::tribble(
  ~id , ~time , ~score , ~group  ,
    1 ,     1 ,     30 , "Treat" ,
    1 ,     2 ,     10 , "Treat" ,
    2 ,     1 ,     30 , "Ctrl"  ,
    2 ,     2 ,     29 , "Ctrl"
)

# Datensatz für HLM (3 Messzeitpunkte nötig)
test_data_hlm <- tibble::tribble(
  ~id , ~time , ~score ,
    1 ,     1 ,     30 ,
    1 ,     2 ,     20 ,
    1 ,     3 ,     10 ,
    2 ,     1 ,     30 ,
    2 ,     2 ,     30 ,
    2 ,     3 ,     30
)


# --- Tests ---

test_that("cs_distribution input validation: Basics", {
  # 1. Fehlende Spalten
  expect_error(
    cs_distribution(test_data_dist, time = time, outcome = score),
    "Argument `id` is missing"
  )
  expect_error(
    cs_distribution(test_data_dist, id = id, outcome = score),
    "Argument `time` is missing"
  )

  # 2. Checkmate Typ-Checks
  expect_error(
    cs_distribution(test_data_dist, id, time, score, reliability = "high"),
    "Must be of type 'numeric'"
  )

  # 3. Range Checks
  expect_error(
    cs_distribution(test_data_dist, id, time, score, reliability = 1.1),
    "Element 1 is not <= 1"
  )
  expect_error(
    cs_distribution(test_data_dist, id, time, score, reliability = -0.1),
    "Element 1 is not >= 0"
  )
})

test_that("cs_distribution input validation: Reliability Logic", {
  # 1. Standard (JT): Reliability ist PFLICHT
  expect_error(
    cs_distribution(test_data_dist, id, time, score, rci_method = "JT"),
    "Argument `reliability` is required"
  )

  # 2. HLM: Reliability ist NICHT nötig
  # (Wir erwarten hier keinen Fehler bei den Argument Checks.
  # Ob die Berechnung klappt, hängt von den Daten ab, aber der Check muss passen.)
  expect_no_error(
    # capture.output verhindert Konsolenausgabe bei erfolgreichem Durchlauf
    capture.output(
      cs_distribution(test_data_hlm, id, time, score, rci_method = "HLM")
    )
  )
})

test_that("cs_distribution logic: Nunnally & Kotsch (NK)", {
  # Szenario 1: NK mit reliability_post -> Alles gut
  expect_silent(
    capture.output(
      cs_distribution(
        test_data_dist,
        id,
        time,
        score,
        rci_method = "NK",
        reliability = 0.8,
        reliability_post = 0.85
      )
    )
  )

  # Szenario 2: NK OHNE reliability_post -> Info Message & Fallback
  # Wir erwarten eine "cli alert info" Message
  expect_message(
    res_nk <- cs_distribution(
      test_data_dist,
      id,
      time,
      score,
      rci_method = "NK",
      reliability = 0.8
    ),
    "Using `reliability` for `reliability_post` as well"
  )

  # Prüfen, ob der Fallback intern funktioniert hat
  # Das datasets Objekt oder die interne Berechnung sollte nun reliability == reliability_post haben.
  # Da wir schwer in die 'calc_rci' reinschauen können von hier, prüfen wir, ob das Ergebnis existiert.
  expect_s3_class(res_nk, "cs_distribution")
  expect_equal(res_nk$method, "NK")
})

test_that("cs_distribution logic: HLM calculation", {
  # HLM braucht min. 3 Punkte. Wir nehmen den hlm_data satz.
  # Dies testet, ob der Dispatch zu cs_hlm funktioniert.

  res_hlm <- cs_distribution(test_data_hlm, id, time, score, rci_method = "HLM")

  expect_s3_class(res_hlm, "cs_distribution")
  expect_s3_class(res_hlm$datasets, "cs_hlm")
  expect_equal(res_hlm$method, "HLM")
})

test_that("cs_distribution logic: Higher is better", {
  # Daten umdrehen: Hohe Werte sind gut (z.B. Lebenszufriedenheit)
  # ID 1 steigt von 10 auf 30 -> Sollte "Improved" sein
  df_high <- tibble::tribble(
    ~id , ~time , ~val ,
      1 ,     1 ,   10 ,
      1 ,     2 ,   30 ,
      2 ,     1 ,    5 ,
      2 ,     2 ,   40 ,
      3 ,     1 ,   10 ,
      3 ,     2 ,   17
  )

  res <- cs_distribution(
    df_high,
    id,
    time,
    val,
    reliability = 0.8,
    better_is = "higher"
  )

  # Wir prüfen die Kategorien in der Summary Table
  category <- res$rci_results$data |>
    dplyr::filter(id == 1) |>
    dplyr::pull(improved)

  # Je nach Logik deiner `create_summary_table`
  expect_true(category)
})

test_that("cs_distribution returns correct structure", {
  res <- cs_distribution(test_data_dist, id, time, score, reliability = 0.8)

  # Klassen
  expect_s3_class(res, "cs_analysis")
  expect_s3_class(res, "cs_distribution")

  # Listen-Elemente
  expect_named(
    res,
    c(
      "datasets",
      "rci_results",
      "outcome",
      "method",
      "n_obs",
      "reliability",
      "critical_value",
      "summary_table"
    )
  )

  # Metadaten
  expect_equal(res$method, "JT")
  expect_equal(res$reliability, 0.8)
})

test_that("cs_distribution snapshots", {
  # 1. Standard JT
  res_jt <- cs_distribution(test_data_dist, id, time, score, reliability = 0.8)
  expect_snapshot(print(res_jt))
  expect_snapshot(summary(res_jt))

  # 2. Grouped Output
  res_grouped <- cs_distribution(
    test_data_dist,
    id,
    time,
    score,
    group = group,
    reliability = 0.8
  )
  expect_snapshot(print(res_grouped))
  expect_snapshot(summary(res_grouped))
})
