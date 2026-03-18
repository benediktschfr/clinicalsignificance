# cs_distribution snapshots (Print/Summary)

    Code
      print(res_std)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:   Distribution-based
      RCI Method: JT
      
      Category     |  N | Percent
      ---------------------------
      Improved     | 10 | 100.00%
      Unchanged    |  0 |   0.00%
      Deteriorated |  0 |   0.00%
      

---

    Code
      summary(res_std)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Distribution-based
      RCI Method:   JT
      N (original): 10
      N (used):     10
      Percent used: 100.00%
      Outcome:      score
      Reliability:  0.8
      
      Category     |  N | Percent
      ---------------------------
      Improved     | 10 | 100.00%
      Unchanged    |  0 |   0.00%
      Deteriorated |  0 |   0.00%
      

---

    Code
      print(res_sens)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:   Distribution-based Sensitivity
      RCI Method: JT
      
      Category     |     Min |     Max | Difference
      ---------------------------------------------
      Improved     | 100.00% | 100.00% |      0.00%
      Unchanged    |   0.00% |   0.00% |      0.00%
      Deteriorated |   0.00% |   0.00% |      0.00%
      

---

    Code
      summary(res_sens)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Distribution-based Sensitivity
      RCI Method:   JT
      N (original): 10
      N (used):     10
      Percent used: 100.00%
      Outcome:      score
      Reliability:  0.7 to 0.9
      
      Category     |     Min |     Max | Difference
      ---------------------------------------------
      Improved     | 100.00% | 100.00% |      0.00%
      Unchanged    |   0.00% |   0.00% |      0.00%
      Deteriorated |   0.00% |   0.00% |      0.00%
      

---

    Code
      print(res_sens_grp)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:   Distribution-based Sensitivity
      RCI Method: JT
      
      Group |     Category |    Min |    Max | Difference
      ---------------------------------------------------
      A     |     Improved | 50.00% | 50.00% |      0.00%
      A     |    Unchanged |  0.00% |  0.00% |      0.00%
      A     | Deteriorated |  0.00% |  0.00% |      0.00%
      B     |     Improved | 50.00% | 50.00% |      0.00%
      B     |    Unchanged |  0.00% |  0.00% |      0.00%
      B     | Deteriorated |  0.00% |  0.00% |      0.00%
      

---

    Code
      summary(res_sens_grp)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Distribution-based Sensitivity
      RCI Method:   JT
      N (original): 10
      N (used):     10
      Percent used: 100.00%
      Outcome:      score
      Reliability:  0.7 to 0.9
      
      Group |     Category |    Min |    Max | Difference
      ---------------------------------------------------
      A     |     Improved | 50.00% | 50.00% |      0.00%
      A     |    Unchanged |  0.00% |  0.00% |      0.00%
      A     | Deteriorated |  0.00% |  0.00% |      0.00%
      B     |     Improved | 50.00% | 50.00% |      0.00%
      B     |    Unchanged |  0.00% |  0.00% |      0.00%
      B     | Deteriorated |  0.00% |  0.00% |      0.00%
      

