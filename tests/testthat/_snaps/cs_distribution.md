# cs_distribution snapshots

    Code
      print(res_jt)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:   Distribution-based
      RCI Method: JT
      
      Category     | N | Percent
      --------------------------
      Improved     | 2 | 100.00%
      Unchanged    | 0 |   0.00%
      Deteriorated | 0 |   0.00%
      

---

    Code
      summary(res_jt)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Distribution-based
      RCI Method:   JT
      N (original): 2
      N (used):     2
      Percent used: 100.00%
      Outcome:      score
      Reliability:  0.8
      
      Category     | N | Percent
      --------------------------
      Improved     | 2 | 100.00%
      Unchanged    | 0 |   0.00%
      Deteriorated | 0 |   0.00%
      

---

    Code
      print(res_grouped)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:   Distribution-based
      RCI Method: JT
      
      Group |     Category | N | Percent | Percent by Group
      -----------------------------------------------------
      Ctrl  |     Improved | 1 |  50.00% |          100.00%
      Ctrl  |    Unchanged | 0 |   0.00% |            0.00%
      Ctrl  | Deteriorated | 0 |   0.00% |            0.00%
      Treat |     Improved | 1 |  50.00% |          100.00%
      Treat |    Unchanged | 0 |   0.00% |            0.00%
      Treat | Deteriorated | 0 |   0.00% |            0.00%
      

---

    Code
      summary(res_grouped)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Distribution-based
      RCI Method:   JT
      N (original): 2
      N (used):     2
      Percent used: 100.00%
      Outcome:      score
      Reliability:  0.8
      
      Group |     Category | N | Percent | Percent by Group
      -----------------------------------------------------
      Ctrl  |     Improved | 1 |  50.00% |          100.00%
      Ctrl  |    Unchanged | 0 |   0.00% |            0.00%
      Ctrl  | Deteriorated | 0 |   0.00% |            0.00%
      Treat |     Improved | 1 |  50.00% |          100.00%
      Treat |    Unchanged | 0 |   0.00% |            0.00%
      Treat | Deteriorated | 0 |   0.00% |            0.00%
      

