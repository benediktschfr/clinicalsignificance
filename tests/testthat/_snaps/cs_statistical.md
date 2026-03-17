# cs_statistical snapshots (Print/Summary)

    Code
      print(res_jt)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach: Statistical
      Method:   JT
      
      Category     | N | Percent
      --------------------------
      Improved     | 0 |   0.00%
      Unchanged    | 1 |  33.33%
      Deteriorated | 2 |  66.67%
      

---

    Code
      summary(res_jt)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Statistical
      Method:       JT
      N (original): 3
      N (used):     3
      Percent used: 100.00%
      Cutoff type:  a
      Cutoff:       30
      
      -- Cutoff Descriptives
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      30         |           0 |          --- |           ---
      
      
      -- Results
      
      Category     | N | Percent
      --------------------------
      Improved     | 0 |   0.00%
      Unchanged    | 1 |  33.33%
      Deteriorated | 2 |  66.67%
      

---

    Code
      print(res_ha_c)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach: Statistical
      Method:   HA
      
      Category     | N | Percent
      --------------------------
      Improved     |   |        
      Unchanged    |   |        
      Deteriorated |   |        
      

---

    Code
      summary(res_ha_c)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Statistical
      Method:       HA
      N (original): 3
      N (used):     3
      Percent used: 100.00%
      Cutoff type:  c_true
      Cutoff:       NaN
      
      -- Cutoff Descriptives
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      30         |           0 |           10 |             2
      
      
      -- Results
      
      Category     | N | Percent
      --------------------------
      Improved     |   |        
      Unchanged    |   |        
      Deteriorated |   |        
      

# cs_statistical_sensitivity snapshots (Print/Summary)

    Code
      print(res_sens)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach: Statistical Sensitivity
      Method:   JT
      
      M Functional | Sd Functional | Reliability |     Category | N | Percent
      -----------------------------------------------------------------------
      10           |             2 |             |     Improved | 0 |   0.00%
      10           |             2 |             |    Unchanged | 1 |  33.33%
      10           |             2 |             | Deteriorated | 2 |  66.67%
      10           |             5 |             |     Improved | 0 |   0.00%
      10           |             5 |             |    Unchanged | 1 |  33.33%
      10           |             5 |             | Deteriorated | 2 |  66.67%
      15           |             2 |             |     Improved | 0 |   0.00%
      15           |             2 |             |    Unchanged | 1 |  33.33%
      15           |             2 |             | Deteriorated | 2 |  66.67%
      15           |             5 |             |     Improved | 0 |   0.00%
      15           |             5 |             |    Unchanged | 1 |  33.33%
      15           |             5 |             | Deteriorated | 2 |  66.67%
      

---

    Code
      summary(res_sens)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:            Statistical Sensitivity
      Method:              JT
      N (original):        3
      N (used):            3
      Percent used:        100.00%
      Cutoff type:         c
      Range M Functional:  10 to 15
      Range SD Functional: 2 to 5
      Range Reliability:   ---
      
      -- Results
      
      M Functional | Sd Functional | Reliability |     Category | N | Percent
      -----------------------------------------------------------------------
      10           |             2 |             |     Improved | 0 |   0.00%
      10           |             2 |             |    Unchanged | 1 |  33.33%
      10           |             2 |             | Deteriorated | 2 |  66.67%
      10           |             5 |             |     Improved | 0 |   0.00%
      10           |             5 |             |    Unchanged | 1 |  33.33%
      10           |             5 |             | Deteriorated | 2 |  66.67%
      15           |             2 |             |     Improved | 0 |   0.00%
      15           |             2 |             |    Unchanged | 1 |  33.33%
      15           |             2 |             | Deteriorated | 2 |  66.67%
      15           |             5 |             |     Improved | 0 |   0.00%
      15           |             5 |             |    Unchanged | 1 |  33.33%
      15           |             5 |             | Deteriorated | 2 |  66.67%
      

