# cleaning data
curve_df = read.xlsx2("covid_full_data_20200424.xlsx", sheetIndex = 2, header=TRUE)

levels(curve_df[["Q13_1"]]) = c("6","2","3","4","1","5")
curve_df <- within(curve_df, Q13_1 <- reorder(Q13_1))
levels(curve_df[["Q13_1"]])

levels(curve_df[["Q13_2"]]) = c("6","2","3","4","1","5")
curve_df <- within(curve_df, Q13_2 <- reorder(Q13_2))

levels(curve_df[["Q13_3"]]) = c("6","2","3","4","1","5")
curve_df <- within(curve_df, Q13_3 <- reorder(Q13_3))

for (i in 1:3) {
  col = paste("Q13",i,sep = "_")
  curve_df[[col]] = as.numeric(curve_df[[col]])
  not_applicable = which(curve_df[[col]] == 6)
  curve_df[[col]][not_applicable] = NA
}

q13_1mean = mean(curve_df$Q13_1[-which(is.na(curve_df$Q13_1))])
q13_2mean = mean(curve_df$Q13_2[-which(is.na(curve_df$Q13_2))])
q13_3mean = mean(curve_df$Q13_3[-which(is.na(curve_df$Q13_3))])

q13_1sd = sd(curve_df$Q13_1[-which(is.na(curve_df$Q13_1))])
q13_2sd = sd(curve_df$Q13_2[-which(is.na(curve_df$Q13_2))])
q13_3sd = sd(curve_df$Q13_3[-which(is.na(curve_df$Q13_3))])


ggplot(data.frame(x = c(0, 7.5)), aes(x = x)) +
  stat_function(fun = dnorm, args = list(q13_1mean, q13_1sd),
                colour = "red") +
  stat_function(fun = dnorm, args = list(q13_2mean, q13_2sd),
                colour = "blue", fill = "blue") + 
  stat_function(fun = dnorm, args = list(q13_3mean, q13_3sd),
                colour = "green")


# stacked bar chart
balance = c(curve_df$Q13_1, curve_df$Q13_2, curve_df$Q13_3)
balance = as.factor(balance)
time_horizon = factor(c(rep("present",320),rep("2 months",320),rep("6 months",320)),
                         levels = c("present","2 months","6 months"))

balance_df = data.frame(balance,time_horizon)
balance_df_no_na = balance_df[-which(balance_df$balance == 6),]
balance_df_no_na$balance = as.factor(balance_df_no_na$balance)
balance_df_no_na$balance = droplevels(balance_df_no_na$balance)

ggplot(balance_df_no_na, aes(x=balance, fill=time_horizon)) + 
  geom_bar(stat="count", position=position_dodge()) + labs(x="Q13 1-3") +
  scale_x_discrete(drop=FALSE)


