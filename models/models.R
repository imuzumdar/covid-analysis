# packages to load
library(knitr)
library(lsmeans)
library(MASS)
library(rcompanion)
library(FSA)
library(sure) # model diagnostics

# crappy model
model = aov(Q11 ~ Q1, data=df3)
model = glm(formula = Q11 ~ Q1, family = binomial, data = df3)

#### logistic regression ####
model_data = df3

# setting other to unsure: data cleaning 
model_data[which(model_data$Q10 == "Other. Please specify."), "Q10"] = "Unsure"
model_data = model_data[-which(model_data$Q10 == ""),]
model_data$Q10 = droplevels(model_data$Q10)

# multinom Model
multinom_log_model = multinom(Q10 ~ Q18 + Q17 + Q16  + Q1, data=model_data)
output=summary(multinom_log_model)

# output tables
z <- output$coefficients/output$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2

Punsure <- rbind(output$coefficients[1,],output$standard.errors[1,],z[1,],p[1,])
rownames(unsure) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Punsure)


Pyes <- rbind(output$coefficients[2,],output$standard.errors[2,],z[2,],p[2,])
rownames(Pyes) <- c("Coefficient","Std. Errors","z stat","p value")
knitr::kable(Pyes)

# anova HAS REALLY BAD DIAGNOSTICS
#transform response
levels(model_data$Q10) = c("1", "2", "3")
model_data$Q10_cont = as.numeric(model_data$Q10)

anova_model = aov(Q10_cont ~ Q18 + Q17 + Q16  + Q1, data=model_data)
summary(anova_model)

anova_reduced_model = aov(Q10_cont ~ Q1, data=model_data)
summary(anova_reduced_model)

# pairwise comparisons
lsmeans(anova_reduced_model, pairwise ~ Q1, adjust="tukey")  
lsmeans(anova_reduced_model,pairwise ~ Q17,adjust="tukey") 

# Ordinal Logistic Regression
ordinal_model = polr(Q10 ~ Q18 + Q17 + Q16  + Q1, data = model_data, Hess=TRUE)
summary(ordinal_model)

# finding pseudo R-squared
nullmod = polr(Q10 ~ 1, data = model_data, Hess=TRUE)
1-logLik(ordinal_model)/logLik(nullmod)
anova(nullmod, ordinal_model, test = "Chisq")

# storing coefficients
(ctable_ordinal <- coef(summary(ordinal_model)))

#adding pvalues to table
p <- pnorm(abs(ctable_ordinal[, "t value"]), lower.tail = FALSE) * 2
(ctable_ordinal <- cbind(ctable_ordinal, "p value" = p))


#scheier-ray-hare test
srh_model = scheirerRayHare(Q10 ~ Q18 + Q1, data=model_data)

# kruskal-wallis
kruskal.test(Q10 ~ Q1, data=model_data)
PT = dunnTest(Q10 ~ Q1,data=model_data,method="bh")

################# Use Ordinal Logistic Regression from here since this works best

# missing demographic data
which(df3$Q17=="")
which(df3$Q1=="")
which(df3$Q16=="")
which(df3$Q18=="")
which(df3$Q20 == "")

missing_dem_info=unique(c(which(df3$Q17==""), which(df3$Q1==""), which(df3$Q16==""),
                       which(df3$Q18==""), which(df3$Q20 == "")))

# missing demographic data
model_df = df3[-missing_dem_info,]

# get rid of blank level
model_df$Q17 = droplevels(model_df$Q17)
model_df$Q16 = droplevels(model_df$Q16)
model_df$Q18 = droplevels(model_df$Q18)
model_df$Q20 = droplevels(model_df$Q20)

#### QUESTION 10
q10_df = model_df
q10_df = q10_df[-which(q10_df$Q10 == "Other. Please specify."),]
q10_df$Q10 = droplevels(q10_df$Q10)


# Ordinal Logistic Regression
q10_model = polr(Q10 ~ Q1, data = q10_df, Hess=TRUE)
summary(q10_model)

# finding pseudo R-squared
nullmod = polr(Q10 ~ 1, data = q10_df, Hess=TRUE)
1-logLik(q10_model)/logLik(nullmod)
anova(nullmod, q10_model, test = "Chisq")

# storing coefficients
ctable_q10 <- coef(summary(q10_model))

#adding pvalues to table
p <- pnorm(abs(ctable_q10[, "t value"]), lower.tail = FALSE) * 2 #pvalues
(ctable_q10 <- cbind(ctable_q10, "p value" = p))

# diagnostics for ordinal model Q10
autoplot(q10_model, what="qq")
#surrogate residuals plots
autoplot(resids(q10_model), what = "covariate", x = q10_df$Q1, xlab = "Q1")
brant(q10_model)

# considering race as a possible factor
model_df1 = model_df
other_races=unique(c(which(model_df1$Q20 == "Black or African American"),
                which(model_df1$Q20 == "Other. Please specify."),
                which(model_df1$Q20 == "White,Asian (Far East, Southeast Asia, or Indian subcontinent)"),
                which(model_df1$Q20 == "White,Black or African American"),
                which(model_df1$Q20 == "White,Other. Please specify.")))
race_pref_not_ans = unique(c(which(model_df1$Q20 == "Prefer not to answer"),
                             which(model_df1$Q20 == "Other. Please specify.,Prefer not to answer")))

model_df1$Q20[race_pref_not_ans] = "Prefer not to answer"
model_df1$Q20[other_races] = "Other. Please specify."
model_df1$Q20 = droplevels(model_df1$Q20)

# setting baselines for factors
levels(model_df1$Q17) = c("31-40","41-50","51-60","61 plus","30 or lower")
model_df1 <- within(model_df1, Q17 <- relevel(Q17, ref = "31-40"))
model_df1 <- within(model_df1, Q20 <- relevel(Q20, ref = "White"))

#### QUESTION 5
q5_1 = analyzer(model_df1, "Q1 + Q18", cleaner = cleaner_5, col=5, sub_col = 1)
q5_2 = analyzer(model_df1, "Q18", cleaner = cleaner_5, col=5, sub_col = 2)
q5_3 = analyzer(model_df1, "Q18 + Q1", cleaner = cleaner_5, col=5, sub_col = 3)
q5_4 = analyzer(model_df1, "Q18 + Q1", cleaner = cleaner_5, col=5, sub_col = 4)
q5_5 = analyzer(model_df1, "Q18", cleaner = cleaner_5, col=5, sub_col = 5)
q5_6 = analyzer(model_df1, "Q18 + Q17 + Q1", cleaner = cleaner_5, col=5, sub_col = 6)

### QUESTION 6
q6_1 = analyzer(model_df1, "Q1", cleaner = cleaner_6, col=6, sub_col = 1)
q6_2 = analyzer(model_df1, "Q17 + Q18", cleaner = cleaner_6, col=6, sub_col = 2)
q6_3 = analyzer(model_df1, "Q18 + Q20", cleaner = cleaner_6, col=6, sub_col = 3)
q6_4 = analyzer(model_df1, "Q1 + Q18", cleaner = cleaner_6, col=6, sub_col = 4)
q6_5 = analyzer(model_df1, "Q1", cleaner = cleaner_6, col=6, sub_col = 5)
q6_6 = analyzer(model_df1, "Q18", cleaner = cleaner_6, col=6, sub_col = 6)


### QUESTION 7
q7 = analyzer(model_df1, "Q16", cleaner = cleaner_7, col=7)

### QUESTION 8
q8_1 = analyzer(model_df1, "Q16 + Q17 + Q18", cleaner = cleaner_8, col=8, sub_col = 1)
q8_2 = analyzer(model_df1, "Q1 + Q17 + Q18", cleaner = cleaner_8, col=8, sub_col = 2)
q8_3 = analyzer(model_df1, "Q1 + Q18", cleaner = cleaner_8, col=8, sub_col = 3)
q8_4 = analyzer(model_df1, "Q18", cleaner = cleaner_8, col=8, sub_col = 4)
q8_5 = analyzer(model_df1, "Q17 + Q18", cleaner = cleaner_8, col=8, sub_col = 5)
q8_6 = analyzer(model_df1, "Q17 + Q18", cleaner = cleaner_8, col=8, sub_col = 6)


### QUESTION 10
q10 = analyzer(model_df1, "Q1", cleaner = cleaner_10, col=10)

### QUESTION 11
q11 = analyzer(model_df1, "Q1 + Q18 + Q20", cleaner = cleaner_11, col=11)

### QUESTION 12
q12 = analyzer(model_df1, "Q17 + Q18", cleaner = cleaner_12, col=12)

### QUESTION 13
# some brief data processing
levels(model_df1[["Q13_1"]]) = c(6,2,3,4,1,5)
model_df1 <- within(model_df1, Q13_1 <- reorder(Q13_1))

levels(model_df1[["Q13_2"]]) = c(6,2,3,4,1,5)
model_df1 <- within(model_df1, Q13_2 <- reorder(Q13_2))

levels(model_df1[["Q13_3"]]) = c(6,2,3,4,1,5)
model_df1 <- within(model_df1, Q13_3 <- reorder(Q13_3))

q13_1 = analyzer(model_df1, "Q1 + Q17 + Q18 + Q20", cleaner = cleaner_13, col=13, sub_col=1)
q13_2 = analyzer(model_df1, "Q17 + Q18 + Q20", cleaner = cleaner_13, col=13, sub_col=2)
q13_3 = analyzer(model_df1, "Q17 + Q18", cleaner = cleaner_13, col=13, sub_col=3)

### QUESTION 14
# some brief data processing
for (col in 1:6){
  column = paste("Q14", col, sep = "_")
  levels(model_df1[[column]]) = c(7,1,5,3,6,2,4)
}

# reordering factors to make them properly ordinal
# there is definitely a faster way to do this via the cleaning functions. I was pressed for them when
# completing this last part of the analysis but I encourage any feedback on how to do this quickly!
model_df1 <- within(model_df1, Q14_1 <- reorder(Q14_1))
model_df1 <- within(model_df1, Q14_1 <- reorder(Q14_1))
model_df1 <- within(model_df1, Q14_1 <- reorder(Q14_1))
model_df1 <- within(model_df1, Q14_1 <- reorder(Q14_1))
model_df1 <- within(model_df1, Q14_1 <- reorder(Q14_1))
model_df1 <- within(model_df1, Q14_1 <- reorder(Q14_1))

# models!
# MAY NEED TO REPEAT. WILL NEED ADDITIONAL CLEANING

# note that for some of these models, we exclude role as a factor, since they only apply to one level
# of role
q14_1 = analyzer(model_df1, "Q1", cleaner = cleaner_14, col=14, sub_col=1) #no significant predictors
q14_2 = analyzer(model_df1, "Q16", cleaner = cleaner_14, col=14, sub_col=2)
q14_3 = analyzer(model_df1, "Q1", cleaner = cleaner_14, col=14, sub_col=3) #no significant predictors
q14_4 = analyzer(model_df1, "Q17", cleaner = cleaner_14, col=14, sub_col=4)
q14_5 = analyzer(model_df1, "Q17", cleaner = cleaner_14, col=14, sub_col=5) #weird significant predictors
q14_6 = analyzer(model_df1, "Q1", cleaner = cleaner_14, col=14, sub_col=6)



# and that's a wrap... until part 2

