# here we are doing two things. First, we are changing the reference levels for role to measure different
# pairwise comparisons. Next, we are performing KW and Dunn tests for significance. 

#changing baseline
new_df <- within(model_df1, Q1 <- relevel(Q1, ref = "A resident"))

#### QUESTION 5
q5_1_new_baseline = analyzer(new_df, "Q1 + Q18", cleaner = cleaner_5, col=5, sub_col = 1)
q5_3_new_baseline = analyzer(new_df, "Q18 + Q1", cleaner = cleaner_5, col=5, sub_col = 3)
q5_4_new_baseline = analyzer(new_df, "Q18 + Q1", cleaner = cleaner_5, col=5, sub_col = 4)
q5_6_new_baseline = analyzer(new_df, "Q18 + Q17 + Q1", cleaner = cleaner_5, col=5, sub_col = 6)

# KW TESTS
kw_analyzer(new_df, cleaner=cleaner_5, col=5, sub_col=1)
kw_analyzer(new_df, cleaner=cleaner_5, col=5, sub_col=3)
kw_analyzer(new_df, cleaner=cleaner_5, col=5, sub_col=4)
kw_analyzer(new_df, cleaner=cleaner_5, col=5, sub_col=6)

### QUESTION 6
q6_1_new_baseline = analyzer(new_df, "Q1", cleaner = cleaner_6, col=6, sub_col = 1)
q6_4_new_baseline = analyzer(new_df, "Q1", cleaner = cleaner_6, col=6, sub_col = 4)
q6_5_new_baseline = analyzer(new_df, "Q1", cleaner = cleaner_6, col=6, sub_col = 5)

# KW TESTS
kw_analyzer(new_df, cleaner=cleaner_6, col=6, sub_col=1)
kw_analyzer(new_df, cleaner=cleaner_6, col=6, sub_col=4)
kw_analyzer(new_df, cleaner=cleaner_6, col=6, sub_col=5)

### QUESTION 8
q8_2_new_baseline = analyzer(new_df, "Q1 + Q17 + Q18", cleaner = cleaner_8, col=8, sub_col = 2)
q8_3_new_baseline = analyzer(new_df, "Q1 + Q18", cleaner = cleaner_8, col=8, sub_col = 3)

# KW TESTS
kw_analyzer(new_df, cleaner=cleaner_8, col=8, sub_col=2)
kw_analyzer(new_df, cleaner=cleaner_8, col=8, sub_col=3)

### QUESTION 10
q10_new_baseline = analyzer(new_df, "Q1", cleaner = cleaner_10, col=10)

# KW TESTS
kw_analyzer(new_df, cleaner=cleaner_10, col=10)

### QUESTION 11
q11_new_baseline = analyzer(new_df, "Q1 + Q18 + Q20", cleaner = cleaner_11, col=11)

# KW TESTS
kw_analyzer(new_df, cleaner=cleaner_11, col=11)

### QUESTION 13
q13_1_new_baseline = analyzer(new_df, "Q1 + Q17 + Q18 + Q20", cleaner = cleaner_13, col=13, sub_col=1)

# KW TESTS
kw_analyzer(new_df, cleaner=cleaner_13, col=13, sub_col=1)

# question 8

for (sub_q in 1:6){
  kw_analyzer(new_df, cleaner=cleaner_8, col=8, sub_col=sub_q)
}

# which is higher
sum(as.numeric(new_df[["Q1"]]) < as.numeric(new_df[["Q2"]]))
