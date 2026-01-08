# Wir nutzen tribble für Lesbarkeit.
# Szenario: "Lower is better" (z.B. Depressions-Score)
# ID 1: Starke Verbesserung (30 -> 10)
# ID 2: Keine Veränderung (30 -> 30)
# ID 3: Verschlechterung (30 -> 40)
test_data_stat <- tibble::tribble(
  ~id , ~time , ~score , ~group  ,
    1 ,     1 ,     30 , "Treat" ,
    1 ,     2 ,     10 , "Treat" ,
    2 ,     1 ,     30 , "Ctrl"  ,
    2 ,     2 ,     30 , "Ctrl"  ,
    3 ,     1 ,     30 , "Treat" ,
    3 ,     2 ,     40 , "Treat"
)

# --- Tests ---

test_that("cs_statistical input validation catches missing/invalid arguments", {
  # 1. Pflicht-Spalten fehlen
  expect_error(
    cs_statistical(test_data_stat, time = time, outcome = score),
    "Argument `id` is missing"
  )
  expect_error(
    cs_statistical(test_data_stat, id = id, outcome = score),
    "Argument `time` is missing"
  )
  expect_error(
    cs_statistical(test_data_stat, id = id, time = time),
    "Argument `outcome` is missing"
  )

  # 2. Checkmate Checks (Typen & Ranges)
  expect_error(
    cs_statistical(test_data_stat, id, time, score, significance_level = 1.5),
    "Element 1 is not <= 1"
  )
  expect_error(
    cs_statistical(test_data_stat, id, time, score, sd_functional = -5),
    "Element 1 is not >= 0"
  )
})

test_that("cs_statistical enforces method-specific requirements (HA vs JT)", {
  # HA braucht zwingend Reliability
  expect_error(
    cs_statistical(test_data_stat, id, time, score, cutoff_method = "HA"),
    "Argument `reliability` is required"
  )

  # HA mit Reliability läuft durch
  expect_silent(
    capture.output(
      # Output unterdrücken
      cs_statistical(
        test_data_stat,
        id,
        time,
        score,
        cutoff_method = "HA",
        reliability = 0.8
      )
    )
  )

  # JT braucht KEINE Reliability -> Info Message
  expect_message(
    cs_statistical(
      test_data_stat,
      id,
      time,
      score,
      cutoff_method = "JT",
      reliability = 0.8
    ),
    "Argument `reliability` is not used"
  )
})

test_that("cs_statistical enforces cutoff-type requirements (a vs b/c)", {
  # Cutoff 'c' braucht funktionale Populationsdaten
  expect_error(
    cs_statistical(test_data_stat, id, time, score, cutoff_type = "c"),
    "Functional population statistics missing"
  )

  # Cutoff 'b' ebenso
  expect_error(
    cs_statistical(
      test_data_stat,
      id,
      time,
      score,
      cutoff_type = "b",
      m_functional = 10
    ),
    "Functional population statistics missing" # sd fehlt noch
  )

  # Mit Daten läuft es durch
  expect_no_error(
    res <- cs_statistical(
      test_data_stat,
      id,
      time,
      score,
      cutoff_type = "c",
      m_functional = 15,
      sd_functional = 5
    )
  )
})

test_that("cs_statistical returns correct structure (S3 Classes)", {
  res <- cs_statistical(test_data_stat, id, time, score, pre = 1, post = 2)

  # Klassen-Hierarchie prüfen
  expect_s3_class(res, "cs_analysis")
  expect_s3_class(res, "cs_statistical")
  # Spezifische Methoden-Klasse (wichtig für Dispatch)
  expect_s3_class(res$datasets, "cs_jt")

  # Struktur prüfen
  expect_named(
    res,
    c(
      "datasets",
      "cutoff_results",
      "outcome",
      "n_obs",
      "method",
      "reliability",
      "critical_value",
      "summary_table"
    )
  )

  # Metadaten Checks
  expect_equal(res$method, "JT")
  expect_equal(res$n_obs$n_original, 3)
})

test_that("cs_statistical handles 'better_is = higher' correctly", {
  # Daten umdrehen: Hohe Werte sind jetzt gut (z.B. Wohlbefinden)
  # ID 1: 10 -> 30 (Verbesserung)
  df_high <- tibble::tribble(
    ~id , ~time , ~val ,
      1 ,     1 ,   10 ,
      1 ,     2 ,   30
  )

  res <- cs_statistical(df_high, id, time, val, better_is = "higher")

  # Interner Direction-Marker sollte 1 sein
  # (Du musst wissen, wo dieser in deinem Output steckt, oft in cutoff_results oder datasets attributes)
  # Hier prüfen wir indirekt, ob es durchläuft und logisch erscheint.
  expect_no_error(summary(res))
})

test_that("cs_statistical works with grouped data", {
  res <- cs_statistical(
    test_data_stat,
    id,
    time,
    score,
    group = group,
    pre = 1,
    post = 2
  )

  # Das datasets Objekt sollte die Gruppe enthalten
  expect_true("group" %in% names(res$datasets$data))
  expect_s3_class(res, "cs_statistical")
})

test_that("cs_statistical snapshots (Print/Summary)", {
  # Wir fixieren Parameter, damit der Snapshot stabil bleibt
  res_jt <- cs_statistical(
    test_data_stat,
    id,
    time,
    score,
    pre = 1,
    post = 2
  )

  # Snapshot für Standard JT
  expect_snapshot(print(res_jt))
  expect_snapshot(summary(res_jt))

  # Snapshot für HA mit Cutoff c (komplexer Output)
  res_ha_c <- cs_statistical(
    test_data_stat,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    cutoff_method = "HA",
    cutoff_type = "c",
    reliability = 0.80,
    m_functional = 10,
    sd_functional = 2
  )

  expect_snapshot(print(res_ha_c))
  expect_snapshot(summary(res_ha_c))
})
