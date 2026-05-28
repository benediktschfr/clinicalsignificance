# Summary Method for the Anchor-Based Approach for Groups (Within)

Summary Method for the Anchor-Based Approach for Groups (Within)

## Usage

``` r
# S3 method for class 'cs_anchor_group_within'
summary(object, ...)
```

## Arguments

- object:

  An object of class `cs_anchor_group_within`

- ...:

  Additional arguments

## Value

No return value, called for side effects only

## Examples

``` r
cs_results <- claus_2020 |>
  cs_anchor(
    id,
    time,
    bdi,
    pre = 1,
    post = 4,
    mid_improvement = 8,
    target = "group"
  )

summary(cs_results)
#> 
#> ---- Clinical Significance Results ----
#> 
#> Approach:        Anchor-based (between groups)
#> MID Improvement: 8
#> N (original):    43
#> N (used):        40
#> Percent used:    93.02%
#> Better is:       Lower
#> Outcome:         bdi
#> 
#> Difference |  Lower | Upper | Ci Level |  N |                               Category
#> ------------------------------------------------------------------------------------
#> -9.36      | -12.97 | -5.81 |     0.95 | 40 | Probably clinically significant effect
#> 
```
