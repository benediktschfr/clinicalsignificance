# cs_percentage print output is stable

    Code
      print(res)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                 Percentage-based
      Percentage Improvement:   20.00%
      Percentage Deterioration: 20.00%
      Better is:                Lower
      
      Category     | N | Percent
      --------------------------
      Improved     | 1 |  33.33%
      Unchanged    | 1 |  33.33%
      Deteriorated | 1 |  33.33%
      

---

    Code
      summary(res)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                 Percentage-based
      Percentage Improvement:   20.00%
      Percentage Deterioration: 20.00%
      Better is:                Lower
      N (original):             3
      N (used):                 3
      Percent used:             100.00%
      Outcome:                  score
      
      Category     | N | Percent
      --------------------------
      Improved     | 1 |  33.33%
      Unchanged    | 1 |  33.33%
      Deteriorated | 1 |  33.33%
      

# cs_percentage_sensitivity print and summary outputs are stable

    Code
      print(res)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Percentage-based Sensitivity
      Better is: Lower
      
      Pct Improvement | Pct Deterioration |     Category | N | Percent
      ----------------------------------------------------------------
      0.10            |              0.10 |     Improved | 2 |  66.67%
      0.10            |              0.10 |    Unchanged | 0 |   0.00%
      0.10            |              0.10 | Deteriorated | 1 |  33.33%
      0.20            |              0.20 |     Improved | 1 |  33.33%
      0.20            |              0.20 |    Unchanged | 1 |  33.33%
      0.20            |              0.20 | Deteriorated | 1 |  33.33%
      0.30            |              0.30 |     Improved | 1 |  33.33%
      0.30            |              0.30 |    Unchanged | 1 |  33.33%
      0.30            |              0.30 | Deteriorated | 1 |  33.33%
      

---

    Code
      summary(res)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                 Percentage-based Sensitivity
      Percentage Improvement:   10.00% to 30.00%
      Percentage Deterioration: 10.00% to 30.00%
      Better is:                Lower
      N (original):             3
      N (used):                 3
      Percent used:             100.00%
      Outcome:                  score
      
      Pct Improvement | Pct Deterioration |     Category | N | Percent
      ----------------------------------------------------------------
      0.10            |              0.10 |     Improved | 2 |  66.67%
      0.10            |              0.10 |    Unchanged | 0 |   0.00%
      0.10            |              0.10 | Deteriorated | 1 |  33.33%
      0.20            |              0.20 |     Improved | 1 |  33.33%
      0.20            |              0.20 |    Unchanged | 1 |  33.33%
      0.20            |              0.20 | Deteriorated | 1 |  33.33%
      0.30            |              0.30 |     Improved | 1 |  33.33%
      0.30            |              0.30 |    Unchanged | 1 |  33.33%
      0.30            |              0.30 | Deteriorated | 1 |  33.33%
      

