# friedman test (uses curve_df in exposure_charts.R file)
library(DescTools)
library(rcompanion)
library(PMCMRplus)

balance = c(curve_df$Q13_1, curve_df$Q13_2, curve_df$Q13_3)
balance = as.factor(balance)
time_horizon = factor(c(rep("present",320),rep("2 months",320),rep("6 months",320)),
                      levels = c("present","2 months","6 months"))

balance_df = data.frame(balance,time_horizon)

balance_df$id = factor(rep(1:320, 3))
balance_df$balance = as.numeric(balance_df$balance)
not_applicable_raters = unique(balance_df$id[which(balance_df$balance == 6)])

balance_df_na_raters_removed = balance_df[-which(balance_df$id %in% not_applicable_raters),]

#after removing non applicable people
balance_df_na_raters_removed$id = droplevels(balance_df_na_raters_removed$id)
friedman.test(balance ~ time_horizon|id, data = balance_df_na_raters_removed)
# SIGNIFICANT P VALUE


# finding effect size with Kendall's W Statistic
XT = xtabs(balance ~ time_horizon + id, data = balance_df_na_raters_removed)
head(XT)

KendallW(XT,correct=TRUE,test=TRUE)

#post hoc testing
order = order(as.numeric(balance_df_na_raters_removed$id))
ordered_balance_df_na_raters_removed = balance_df_na_raters_removed[order,]

#abs value
frdAllPairsConoverTest(y = balance_df_na_raters_removed$balance,
                            groups = balance_df_na_raters_removed$time_horizon,
                            blocks = balance_df_na_raters_removed$id,
                            p.adjust.method = "fdr")

# we can guess notions of stochastic dominance for this data
