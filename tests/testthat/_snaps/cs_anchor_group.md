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
      
      Group |                                  Category | Median Difference | Min Mid | Max Mid
      -----------------------------------------------------------------------------------------
      A     |       Large clinically significant effect |             -9.93 |       3 |       5
      B     |       Large clinically significant effect |             -4.54 |       3 |       3
      B     | Not significantly less than the threshold |             -4.55 |       5 |       5
      

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
      
      -- Results
      
      Group |                                  Category | Median Difference | Min Mid | Max Mid
      -----------------------------------------------------------------------------------------
      A     |       Large clinically significant effect |             -9.93 |       3 |       5
      B     |       Large clinically significant effect |             -4.54 |       3 |       3
      B     | Not significantly less than the threshold |             -4.55 |       5 |       5
      

---

    Code
      print(res_sens_between)
    Output
      
      ---- Clinical Significance Results ----
      
      Approach:  Anchor-based (between groups) Sensitivity
      Better is: Lower
      
      Group 1 |      Group 2 |                                              Category
      ------------------------------------------------------------------------------
      Control | Intervention |                   Large clinically significant effect
      Control | Intervention | Statistically significant but not clinically relevant
      
      Group 1 | Median Difference | Min Mid | Max Mid
      -----------------------------------------------
      Control |             -7.92 |       3 |       3
      Control |             -7.92 |      10 |      10
      

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
      
      -- Results
      
      Group 1 |      Group 2 |                                              Category
      ------------------------------------------------------------------------------
      Control | Intervention |                   Large clinically significant effect
      Control | Intervention | Statistically significant but not clinically relevant
      
      Group 1 | Median Difference | Min Mid | Max Mid
      -----------------------------------------------
      Control |             -7.92 |       3 |       3
      Control |             -7.92 |      10 |      10
      

