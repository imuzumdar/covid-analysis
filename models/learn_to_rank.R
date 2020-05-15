# performing learn to rank comparisons for models
library(PlackettLuce)
library(psychotree)
library(igraph)

# cleaning data for rank comparisons between govt agencies
rank_df = read.xlsx2("covid_full_data_20200424.xlsx", sheetIndex = 2, header=TRUE)
for (i in 1:6) {
  rank_df = cleaner_8(rank_df, i)
  rank_df[[paste("Q8",i,sep = "_")]] = as.numeric(rank_df[[paste("Q8",i,sep = "_")]])
}

ranks_q8 = rank_df[,c("Q8_1","Q8_2", "Q8_3", "Q8_4", "Q8_5", "Q8_6")]

ranks_q8[1:6,]

# calculating ranks using different methods
ranks <- t(apply(ranks_q8, 1, function(x) rank(-x) ))
colnames(ranks) <- paste(colnames(ranks), "_rank", sep="")

ranks_random_ties <- t(apply(ranks_q8, 1, function(x) rank(-x,ties.method= "random")))
colnames(ranks_random_ties) <- paste(colnames(ranks_random_ties), "_rank", sep="")

ranks_min_ties <- t(apply(ranks_q8, 1, function(x) rank(-x,ties.method= "min")))
colnames(ranks_min_ties) <- paste(colnames(ranks_min_ties), "_rank", sep="")

ranks_min_ties[1:5, ]
rank_table_q8 = plyr::count(ranks_min_ties)

# weighted learn to rank models
rank_model_q8 = PlackettLuce(rank_table_q8[,1:6], weights = rank_table_q8$freq)
coef(rank_model_q8, log = FALSE)
summary(rank_model_q8)

#quasi variances to compare models
qv_q8 <- qvcalc(rank_model_q8)
plot(qv_q8, ylab = "Trust of Agency (log)", main = "Trust of Agencies")

qv_q8_df = data.frame(qv_q8$qvframe)
qv_q8_df$agencies = c("CHP","UPMC","State Govt","CDC","Fed Govt","WHO")

# PLOTTING CONFIDENCE INTERVALS
pd <- position_dodge(0.78)

ggplot(qv_q8_df[-2,], aes(x=agencies, y = estimate, group = agencies)) +
  #draws the means
  geom_point(position=pd) + 
  # ggtitle("Trust of Agencies") + 
  labs(y="(log) Trust of Agencies", x = "agencies") +
  #draws the CI error bars
  geom_errorbar(data=qv_q8_df[-2,], aes(ymin=estimate-1.96*quasiSE, 
                                        ymax=estimate+1.96*quasiSE, 
                                color=agencies), width=.3, position=pd)


# EXPONENTIATING LOG ODDS
ggplot(qv_q8_df[-2,], aes(x=agencies, y = exp(estimate), group = agencies)) +
  #draws the means
  geom_point(position=pd) + 
  # ggtitle("Trust of Agencies") + 
  labs(y="Trust of Agencies", x = "agencies") +
  #draws the CI error bars
  geom_errorbar(data=qv_q8_df[-2,], aes(ymin=exp(estimate-1.96*quasiSE), 
                                        ymax=exp(estimate+1.96*quasiSE), 
                                        color=agencies), width=.3, position=pd)


### QUESTION 6 ######################################################################

# cleaning data for rank comparisons
rank_df_q5 = read.xlsx2("covid_full_data_20200424.xlsx", sheetIndex = 2, header=TRUE)
for (i in 1:6) {
  rank_df_q5 = cleaner_5(rank_df_q5, i)
  rank_df_q5[[paste("Q5",i,sep = "_")]] = as.numeric(rank_df_q5[[paste("Q5",i,sep = "_")]])
}

ranks_q5 = rank_df_q5[,c("Q5_1","Q5_2", "Q5_3", "Q5_4", "Q5_5", "Q5_6")]
ranks_q5[1:6,]

ranks_min_ties_q5 <- t(apply(ranks_q5, 1, function(x) rank(-x,ties.method= "min")))
colnames(ranks_min_ties_q5) <- paste(colnames(ranks_min_ties_q5), "_rank", sep="")

ranks_min_ties_q5[1:5, ]
rank_table_q5 = plyr::count(ranks_min_ties_q5)

#learn to rank models
rank_model_q5 = PlackettLuce(rank_table_q5[,1:6], weights = rank_table_q5$freq)
coef(rank_model_q5, log = FALSE)
summary(rank_model_q5)

#quasi variances to compare models
qv_q5 <- qvcalc(rank_model_q5)
plot(qv_q5, ylab = "Level of Concern (log)", main = "Level of Concern")

qv_q5_df = data.frame(qv_q5$qvframe)
qv_q5_df$people = c("1: self","2: family","3: faculty","4: trainee","5: staff","6: patients")

# PLOTTING CONFIDENCE INTERVALS
pd <- position_dodge(0.78)

ggplot(qv_q5_df, aes(x=people, y = estimate, group = people)) +
  #draws the means
  geom_point(position=pd) + 
  # ggtitle("Trust of Agencies") + 
  labs(y="(log) Level of Concern", x = "people") +
  #draws the CI error bars
  geom_errorbar(data=qv_q5_df, aes(ymin=estimate-1.96*quasiSE, 
                                        ymax=estimate+1.96*quasiSE, 
                                        color=people), width=.3, position=pd)


# graph visualizations for ranks
q5_adjacency = adjacency(ranks_min_ties_q5)
q5_net = graph_from_adjacency_matrix(q5_adjacency)
plot(q5_net, edge.arrow.size = .5, vertex.size = 30)

#plotting samples of graph
q5_adjacency_sample = adjacency(ranks_min_ties_q5[sample(1:319, size=50),])
q5_net_sample = graph_from_adjacency_matrix(q5_adjacency_sample)
plot(q5_net_sample, edge.arrow.size = .5, vertex.size = 30)

