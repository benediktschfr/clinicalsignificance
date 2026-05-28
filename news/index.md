# Changelog

## clinicalsignificance (development version)

- Change maintainer name and associated URLS.

## clinicalsignificance 3.0.0

CRAN release: 2025-11-27

### Breaking changes

- [`cs_get_summary()`](https://benediktclaus.github.io/clinicalsignificance/reference/summary_table.md)
  now returns raw numeric values instead of formatted character strings
  (e.g., `0.5` instead of `"50.0%"`). This facilitates downstream
  analyses and plotting (e.g., in sensitivity analyses). Formatting is
  now handled exclusively by the respective
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods.
- Refactored [`print()`](https://rdrr.io/r/base/print.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods to be
  lighter and more maintainable. The output no longer relies on the
  `cli` package, which is now used solely for messages and warnings.
  This is an internal change to improve code readability; calculation
  results remain unaffected.

### New features

- All main functions
  ([`cs_anchor()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_anchor.md),
  [`cs_percentage()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_percentage.md),
  [`cs_distribution()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_distribution.md),
  [`cs_statistical()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_statistical.md),
  [`cs_combined()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_combined.md))
  now calculate group-wise statistics if the `group` argument is
  provided.
- Added new datasets (`garden_club`, `trackmania`, `columbo`) to
  demonstrate various analysis methods.

### Minor improvements and fixes

- Fixed an issue where the summary table for combined approaches was not
  displayed correctly.
- All vignettes have been updated to reflect the latest changes.

## clinicalsignificance 2.1.0

CRAN release: 2024-12-02

- I included the JSS article <doi:10.18637/jss.v111.i01> as a reference.

## clinicalsignificance 2.0.0

CRAN release: 2023-11-16

### Breaking changes

- The former main function `clinical_significance()` is deprecated. The
  package now offers several different approaches to clinical
  significance analyses, namely:
  - [`cs_anchor()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_anchor.md)
    (Anchor-based approaches)
  - [`cs_percentage()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_percentage.md)(Percentage-change
    approaches)
  - [`cs_distribution()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_distribution.md)(Distribution-based
    approaches)
  - [`cs_statistical()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_statistical.md)(Statistical
    approaches)
  - [`cs_combined()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_combined.md)(Combined
    approaches)

The function `clinical_significance()` used the combined approach
(distribution-based and statistical). So the identical analysis can be
carried out with
[`cs_combined()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_combined.md).

- The function `check_cutoff()` is deprecated.

- When `method = "HLM`, participants with three or more data points will
  be used. Before, participants with at least three data points (i.e., 4
  or more) were used.

### New features

- You can now specify a significance level at which you want the
  analysis to be based on with the `significance_level` argument. The
  default is `significance_level = 0.05`. Note that for this is the Phi
  max level when you set `method = "HA"` as outlined in their article.

- The package now offers to combined approaches: The classic combination
  of statistical and distribution-based approaches but also the
  combination of statistical and anchor-based approaches

- We included the function
  [`cs_get_model()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_get_model.md)
  to retrieve the hierarchical linear model that is fit during the
  distribution-based approach if the HLM method is requested.

- The `hechler_2014` dataset was included to ensure reproducibility of
  the JSS article.

### Minor improvements and fixes

- Change the whole package to use R’s native pipe operator `|>`
- Remove unnecessary dependency on `magrittr`
- Include new vignettes replacing the old ones
- Display correct percentages in summary tables
- Update package vignettes

## clinicalsignificance 1.2.1

### Breaking changes

- Remove default theming from `check_cutoff()` as in other plot
  functions

### Minor improvements and fixes

- Improve documentation of datasets
- Include examples for
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method

## clinicalsignificance 1.2.0

CRAN release: 2022-12-08

### Breaking changes

- Remove default theming from
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to give the
  user more control over the overall appearance

### Minor improvements and fixes

- Further implementation of tidyeval principles since
  [`aes_()`](https://ggplot2.tidyverse.org/reference/aes_.html) was
  deprecated
- Small improvements in vignettes
- Better documentation for some functions

## clinicalsignificance 1.1.0

- Fix HLM method empirical bayes estimation
  ([\#2](https://github.com/benediktschfr/clinicalsignificance/issues/2)).
  This now relies on the lme4 package
- Consider the instrument direction for group level summaries in the HA
  method
  ([\#1](https://github.com/benediktschfr/clinicalsignificance/issues/1))
  which was not done in the initial release

## clinicalsignificance 1.0.1

- Added a `NEWS.md` file to track changes to the package.
- Added package URLs to DESCRIPTION, including one for bug reports
- New package logo
