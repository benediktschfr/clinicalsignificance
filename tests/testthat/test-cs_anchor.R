test_data <- tibble::tibble(
  id = rep(1:5, each = 2),
  time = rep(c(1, 2), 5),
  score = c(
    20,
    10,
    20,
    18,
    20,
    20,
    20,
    25,
    20,
    30
  ),
  group = rep(c("Treat", "Ctrl"), length.out = 10)
)


test_that("cs_anchor input validation works", {
  # Missing Columns (Deine cli_abort Checks)
  expect_error(
    cs_anchor(test_data, time = time, outcome = score, mid_improvement = 2),
    "Argument `id` is missing", # Teilt des cli Fehlers matchen
    fixed = TRUE # Oder FALSE nutzen für Regex
  )

  expect_error(
    cs_anchor(test_data, id = id, outcome = score, mid_improvement = 2),
    "Argument `time` is missing"
  )

  # Numeric Checks (Deine checkmate asserts)
  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = "invalid"
    ),
    "Must be of type 'number'" # Standard checkmate Fehlermeldung
  )

  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = -5
    ),
    "Element 1 is not >= 0"
  )

  # CI Level Check
  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = 2,
      ci_level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})


test_that("cs_anchor input validation works", {
  # Missing Columns (Deine cli_abort Checks)
  expect_error(
    cs_anchor(test_data, time = time, outcome = score, mid_improvement = 2),
    "Argument `id` is missing", # Teilt des cli Fehlers matchen
    fixed = TRUE # Oder FALSE nutzen für Regex
  )

  expect_error(
    cs_anchor(test_data, id = id, outcome = score, mid_improvement = 2),
    "Argument `time` is missing"
  )

  # Numeric Checks (Deine checkmate asserts)
  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = "invalid"
    ),
    "Must be of type 'number'" # Standard checkmate Fehlermeldung
  )

  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = -5
    ),
    "Element 1 is not >= 0"
  )

  # CI Level Check
  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = 2,
      ci_level = 1.5
    ),
    "Element 1 is not <= 1"
  )
})


test_that("cs_anchor catches invalid design specifications", {
  # Between subjects bei individual target ist verboten
  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = 2,
      target = "individual",
      effect = "between",
      group = group,
      post = 2
    ),
    "Invalid design specification"
  )

  # Between subjects ohne Gruppe
  expect_error(
    cs_anchor(
      test_data,
      id = id,
      time = time,
      outcome = score,
      mid_improvement = 2,
      target = "group",
      effect = "between",
      post = 2
      # group fehlt hier
    ),
    "Argument `group` is missing"
  )
})


test_that("cs_anchor returns correct class and structure (Individual/Within)", {
  res <- cs_anchor(
    data = test_data,
    id = id,
    time = time,
    outcome = score,
    pre = 1,
    post = 2,
    mid_improvement = 5
  )

  # Prüfen der Klassen-Hierarchie
  expect_s3_class(res, "cs_analysis")
  expect_s3_class(res, "cs_anchor_individual_within")

  # Prüfen der Listen-Elemente
  expect_named(
    res,
    c(
      "datasets",
      "anchor_results",
      "outcome",
      "n_obs",
      "mid_improvement",
      "mid_deterioration",
      "direction",
      "bayesian",
      "prior_scale",
      "summary_table"
    )
  )

  # Logik-Check: Wurde mid_deterioration korrekt von improvement geerbt?
  expect_equal(res$mid_deterioration, 5)

  # Direction Check (Default ist "lower" -> -1? Hängt von deinem Default ab)
  # Angenommen better_is = "lower" ist default
  expect_equal(res$direction, -1)
})


test_that("cs_anchor print method looks correct", {
  res <- cs_anchor(
    test_data,
    id,
    time,
    score,
    pre = 1,
    post = 2,
    mid_improvement = 5
  )

  # Das testet, ob der Print-Output in der Konsole gleich bleibt
  expect_snapshot(print(res))

  # Das testet, ob die Summary-Methode gleich bleibt
  expect_snapshot(summary(res))
})
