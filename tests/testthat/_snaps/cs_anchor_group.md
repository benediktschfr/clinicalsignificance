# cs_anchor_group snapshots (Print/Summary)

    Code
      print(res_within)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:        Anchor-based (within groups)
      MID Improvement: 5
      Better is:       Lower
      
      Group | Median Difference |  Lower | Upper | Ci Level | N |                                  Category
      -----------------------------------------------------------------------------------------------------
      A     |             -9.93 | -11.37 | -7.91 |     0.95 | 5 |       Large clinically significant effect
      B     |             -4.56 |  -5.37 | -3.64 |     0.95 | 5 | Not significantly less than the threshold
      

---

    Code
      summary(res_within)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:        Anchor-based (within groups)
      MID Improvement: 5
      N (original):    10
      N (used):        10
      Percent used:    100.00%
      Better is:       Lower
      Outcome:         score
      
      Group | Difference |  Lower | Upper | Ci Level | N |                                  Category
      ----------------------------------------------------------------------------------------------
      A     |      -9.93 | -11.37 | -7.91 |     0.95 | 5 |       Large clinically significant effect
      B     |      -4.56 |  -5.37 | -3.64 |     0.95 | 5 | Not significantly less than the threshold
      

---

    Code
      print(res_between)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:          Anchor-based (between groups)
      MID (Improvement): 5
      Better is:         Lower
      
      Group 1 |      Group 2 | Median Difference | Lower | Upper | Ci Level | N 1
      ---------------------------------------------------------------------------
      Control | Intervention |             -7.93 | -9.01 | -6.82 |     0.95 |  10
      
      Group 1 | N 2 |                            Category
      ---------------------------------------------------
      Control |  10 | Large clinically significant effect
      

---

    Code
      summary(res_between)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:          Anchor-based (between groups)
      MID (Improvement): 5
      Better is:         Lower
      Outcome:           score
      
      Group 1 |      Group 2 | Median Difference | Lower | Upper | Ci Level | N 1
      ---------------------------------------------------------------------------
      Control | Intervention |             -7.93 | -9.01 | -6.82 |     0.95 |  10
      
      Group 1 | N 2 |                            Category
      ---------------------------------------------------
      Control |  10 | Large clinically significant effect
      

---

    Code
      print(res_sens_within)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Anchor-based (within groups) Sensitivity
      Better is: Lower
      
      Mid Improvement | Mid Deterioration | Group | Median Difference |  Lower
      ------------------------------------------------------------------------
      3               |                 3 |     A |             -9.93 | -11.40
      3               |                 3 |     B |             -4.54 |  -5.31
      5               |                 5 |     A |             -9.90 | -11.32
      5               |                 5 |     B |             -4.55 |  -5.29
      
      Mid Improvement | Upper | Ci Level | N |                                  Category
      ----------------------------------------------------------------------------------
      3               | -7.89 |     0.95 | 5 |       Large clinically significant effect
      3               | -3.49 |     0.95 | 5 |       Large clinically significant effect
      5               | -7.93 |     0.95 | 5 |       Large clinically significant effect
      5               | -3.50 |     0.95 | 5 | Not significantly less than the threshold
      

---

    Code
      summary(res_sens_within)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                Anchor-based (within groups) Sensitivity
      Range MID Improvement:   3 to 5
      Range MID Deterioration: 3 to 5 (symmetric)
      N (original):            10
      N (used):                10
      Percent used:            100.00%
      Better is:               Lower
      Outcome:                 score
      
      Mid Improvement | Mid Deterioration | Group | Median Difference |  Lower
      ------------------------------------------------------------------------
      3               |                 3 |     A |             -9.93 | -11.40
      3               |                 3 |     B |             -4.54 |  -5.31
      5               |                 5 |     A |             -9.90 | -11.32
      5               |                 5 |     B |             -4.55 |  -5.29
      
      Mid Improvement | Upper | Ci Level | N |                                  Category
      ----------------------------------------------------------------------------------
      3               | -7.89 |     0.95 | 5 |       Large clinically significant effect
      3               | -3.49 |     0.95 | 5 |       Large clinically significant effect
      5               | -7.93 |     0.95 | 5 |       Large clinically significant effect
      5               | -3.50 |     0.95 | 5 | Not significantly less than the threshold
      

---

    Code
      print(res_sens_between)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Anchor-based (between groups) Sensitivity
      Better is: Lower
      
      Mid Improvement | Mid Deterioration | Group 1 |      Group 2
      ------------------------------------------------------------
      3               |                 3 | Control | Intervention
      10              |                10 | Control | Intervention
      
      Mid Improvement | Median Difference | Lower | Upper | Ci Level | N 1 | N 2 |                                              Category
      ----------------------------------------------------------------------------------------------------------------------------------
      3               |             -7.92 | -9.01 | -6.75 |     0.95 |  10 |  10 |                   Large clinically significant effect
      10              |             -7.92 | -9.01 | -6.74 |     0.95 |  10 |  10 | Statistically significant but not clinically relevant
      

---

    Code
      summary(res_sens_between)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:                Anchor-based (between groups) Sensitivity
      Range MID Improvement:   3 to 10
      Range MID Deterioration: 3 to 10 (symmetric)
      Better is:               Lower
      Outcome:                 score
      
      Mid Improvement | Mid Deterioration | Group 1 |      Group 2
      ------------------------------------------------------------
      3               |                 3 | Control | Intervention
      10              |                10 | Control | Intervention
      
      Mid Improvement | Median Difference | Lower | Upper | Ci Level | N 1 | N 2 |                                              Category
      ----------------------------------------------------------------------------------------------------------------------------------
      3               |             -7.92 | -9.01 | -6.75 |     0.95 |  10 |  10 |                   Large clinically significant effect
      10              |             -7.92 | -9.01 | -6.74 |     0.95 |  10 |  10 | Statistically significant but not clinically relevant
      

