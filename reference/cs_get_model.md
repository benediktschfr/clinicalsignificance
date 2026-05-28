# Get The HLM Model From A cs_analysis Object

With `cs_get_model()` you can extract the fitted HLM model for the
distribution-based approach. This is useful to either diagnose the model
further (beacuse all assumptions of HLMs apply in this case) or plot the
results differently.

## Usage

``` r
cs_get_model(x)
```

## Arguments

- x:

  A cs_analysis object

## Value

A model of class `lmerMod`

## See also

Extractor functions
[`cs_get_augmented_data()`](https://benediktclaus.github.io/clinicalsignificance/reference/augmented_data.md),
[`cs_get_data()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_get_data.md),
[`cs_get_n()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_get_n.md),
[`cs_get_reliability()`](https://benediktclaus.github.io/clinicalsignificance/reference/cs_get_reliability.md),
[`cs_get_summary()`](https://benediktclaus.github.io/clinicalsignificance/reference/summary_table.md)

## Examples

``` r
cs_results <- claus_2020 |>
  cs_distribution(id, time, bdi, rci_method = "HLM")

cs_get_model(cs_results)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: outcome ~ time + (time | id)
#>    Data: data
#> REML criterion at convergence: 1106.347
#> Random effects:
#>  Groups   Name        Std.Dev. Corr  
#>  id       (Intercept) 6.344          
#>           time        2.766    -0.08 
#>  Residual             5.316          
#> Number of obs: 160, groups:  id, 40
#> Fixed Effects:
#> (Intercept)         time  
#>      37.663       -3.398  
```
