# cs_combined snapshots (Print/Summary)

    Code
      print(res_dist)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Combined
      Method:    JT
      Better is: Lower
      
      Category     | N | Percent
      --------------------------
      Recovered    | 0 |   0.00%
      Improved     | 1 |  33.33%
      Unchanged    | 1 |  33.33%
      Deteriorated | 1 |  33.33%
      Harmed       | 0 |   0.00%
      

---

    Code
      summary(res_dist)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:     Combined
      RCI Method:   JT
      N (original): 3
      N (used):     3
      Percent used: 100.00%
      Cutoff Type:  a
      Cutoff:       0.24
      Outcome:      score
      Better is:    Lower
      Reliability:  0.8
      
      -- Cutoff Descriptives
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      23.33      |       11.55 |          --- |           ---
      
      
      -- Results
      
      Category     | N | Percent
      --------------------------
      Recovered    | 0 |   0.00%
      Improved     | 1 |  33.33%
      Unchanged    | 1 |  33.33%
      Deteriorated | 1 |  33.33%
      Harmed       | 0 |   0.00%
      

---

    Code
      print(res_anchor)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Combined
      Method:    CWB
      Better is: Lower
      
      Category     | N | Percent
      --------------------------
      Recovered    | 0 |   0.00%
      Improved     | 1 |  33.33%
      Unchanged    | 1 |  33.33%
      Deteriorated | 1 |  33.33%
      Harmed       | 0 |   0.00%
      

---

    Code
      summary(res_anchor)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:            Combined
      RCI Method:          CWB
      N (original):        3
      N (used):            3
      Percent used:        100.00%
      Cutoff Type:         a
      Cutoff:              0.24
      Outcome:             score
      Better is:           Lower
      MID (Improvement):   5
      MID (Deterioration): 5
      
      -- Cutoff Descriptives
      
      M Clinical | SD Clinical | M Functional | SD Functional
      -------------------------------------------------------
      23.33      |       11.55 |          --- |           ---
      
      
      -- Results
      
      Category     | N | Percent
      --------------------------
      Recovered    | 0 |   0.00%
      Improved     | 1 |  33.33%
      Unchanged    | 1 |  33.33%
      Deteriorated | 1 |  33.33%
      Harmed       | 0 |   0.00%
      

---

    Code
      print(res_sens_dist)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Combined Sensitivity
      Method:    JT
      Better is: Lower
      
      Category     |    Min |    Max | Difference
      -------------------------------------------
      Recovered    | 33.33% | 33.33% |      0.00%
      Improved     |  0.00% |  0.00% |      0.00%
      Unchanged    | 33.33% | 33.33% |      0.00%
      Deteriorated |  0.00% |  0.00% |      0.00%
      Harmed       | 33.33% | 33.33% |      0.00%
      

---

    Code
      summary(res_sens_dist)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:            Combined Sensitivity
      RCI Method:          JT
      N (original):        3
      N (used):            3
      Percent used:        100.00%
      Cutoff Type:         c
      Range M Functional:  10
      Range SD Functional: 5
      Outcome:             score
      Better is:           Lower
      Range Reliability:   0.7 to 0.9
      
      -- Results
      
      Category     |    Min |    Max | Difference
      -------------------------------------------
      Recovered    | 33.33% | 33.33% |      0.00%
      Improved     |  0.00% |  0.00% |      0.00%
      Unchanged    | 33.33% | 33.33% |      0.00%
      Deteriorated |  0.00% |  0.00% |      0.00%
      Harmed       | 33.33% | 33.33% |      0.00%
      

---

    Code
      print(res_sens_anchor)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Combined Sensitivity
      Method:    CWB
      Better is: Lower
      
      Category     |    Min |    Max | Difference
      -------------------------------------------
      Recovered    | 33.33% | 33.33% |      0.00%
      Improved     |  0.00% |  0.00% |      0.00%
      Unchanged    | 33.33% | 33.33% |      0.00%
      Deteriorated |  0.00% |  0.00% |      0.00%
      Harmed       | 33.33% | 33.33% |      0.00%
      

---

    Code
      summary(res_sens_anchor)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                Combined Sensitivity
      RCI Method:              CWB
      N (original):            3
      N (used):                3
      Percent used:            100.00%
      Cutoff Type:             c
      Range M Functional:      10
      Range SD Functional:     5
      Outcome:                 score
      Better is:               Lower
      Range MID Improvement:   3 to 7
      Range MID Deterioration: 3 to 7 (symmetric)
      
      -- Results
      
      Category     |    Min |    Max | Difference
      -------------------------------------------
      Recovered    | 33.33% | 33.33% |      0.00%
      Improved     |  0.00% |  0.00% |      0.00%
      Unchanged    | 33.33% | 33.33% |      0.00%
      Deteriorated |  0.00% |  0.00% |      0.00%
      Harmed       | 33.33% | 33.33% |      0.00%
      

