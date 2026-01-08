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
      

