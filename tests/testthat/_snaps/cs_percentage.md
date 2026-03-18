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
      
      Category     |    Min |    Max | Difference
      -------------------------------------------
      Improved     | 33.33% | 66.67% |     33.34%
      Unchanged    |  0.00% | 33.33% |     33.33%
      Deteriorated | 33.33% | 33.33% |      0.00%
      

---

    Code
      summary(res)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                       Percentage-based Sensitivity
      Range Percentage Improvement:   10.00% to 30.00%
      Range Percentage Deterioration: 10.00% to 30.00% (symmetric)
      Better is:                      Lower
      N (original):                   3
      N (used):                       3
      Percent used:                   100.00%
      Outcome:                        score
      
      Category     |    Min |    Max | Difference
      -------------------------------------------
      Improved     | 33.33% | 66.67% |     33.34%
      Unchanged    |  0.00% | 33.33% |     33.33%
      Deteriorated | 33.33% | 33.33% |      0.00%
      

