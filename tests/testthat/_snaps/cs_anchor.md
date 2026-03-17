# cs_anchor_individual_sensitivity snapshots (Print/Summary)

    Code
      print(res_sens)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Anchor-based Sensitivity
      Better is: Higher
      
      Mid Improvement | Mid Deterioration |     Category | N | Percent
      ----------------------------------------------------------------
      2               |                 2 |     Improved | 2 |  66.67%
      2               |                 2 |    Unchanged | 1 |  33.33%
      2               |                 2 | Deteriorated | 0 |   0.00%
      5               |                 5 |     Improved | 2 |  66.67%
      5               |                 5 |    Unchanged | 1 |  33.33%
      5               |                 5 | Deteriorated | 0 |   0.00%
      

---

    Code
      summary(res_sens)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                Anchor-based Sensitivity
      Range MID Improvement:   2 to 5
      Range MID Deterioration: 2 to 5 (symmetric)
      N (original):            3
      N (used):                3
      Percent (used):          100.00%
      Better is:               Higher
      Outcome:                 score
      
      Mid Improvement | Mid Deterioration |     Category | N | Percent
      ----------------------------------------------------------------
      2               |                 2 |     Improved | 2 |  66.67%
      2               |                 2 |    Unchanged | 1 |  33.33%
      2               |                 2 | Deteriorated | 0 |   0.00%
      5               |                 5 |     Improved | 2 |  66.67%
      5               |                 5 |    Unchanged | 1 |  33.33%
      5               |                 5 | Deteriorated | 0 |   0.00%
      

