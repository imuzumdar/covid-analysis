#### libraries to install ###

library(xlsx)
library(plyr)
library(dplyr)
library(ggplot2)
library(nnet)

### loading in data, with both headers as options ### 

# data with questions as headers
df3 = read.xlsx2("covid_full_data_20200424.xlsx", sheetIndex = 2, header=TRUE)

### EDA ###

faculty_counts = plyr::count(df2$Q1)
dept_by_faculty_counts = df2 %>% group_by(Q1, Q2) %>% tally()

#counts of specialties within pediatrics
specialities_within_ped = plyr::count(df2[which(df2$Q2 == "Pediatrics"), ]$Q3)

#finding others within ped
df2[which(df2$Q2 == "Pediatrics" & df2$Q3 == "Other"), "Q3_22_TEXT"] # no answers available

# demographic data ungrouped and grouped by faculty
gender_by_faculty_counts = df2 %>% group_by(Q1, Q18) %>% tally() 
gender_counts = plyr::count(df2$Q18)

race_by_faculty_counts = df2 %>% group_by(Q1, Q20) %>% tally() 
race_counts = plyr::count(df2$Q20)

latinx_by_faculty_counts = df2 %>% group_by(Q1, Q19) %>% tally() 
latinx_counts = plyr::count(df2$Q19)

img_by_faculty_counts = df2 %>% group_by(Q1, Q16) %>% tally() 
img_counts = plyr::count(df2$Q16)

age_by_faculty_counts = df2 %>% group_by(Q1, Q17) %>% tally() 
age_counts = plyr::count(df2$Q17)

director = plyr::count(df3$Q9)

### Bar Charts for attitudes (unstratified and stratified by role) ###
cols = colnames(df2)

# Q10
ggplot(df2, aes(x=Q10)) + geom_bar(stat="count", fill="steelblue")

# Q10 distribution stratified by role
print(ggplot(df3,  aes_string(x="Q10", fill="Q1")) + 
        geom_bar(stat="count", position=position_dodge()) + labs(x="Q10") + 
        scale_x_discrete(drop=FALSE))

# Q11
ggplot(df3, aes_string(x=factor(df3$Q11, levels(df3$Q11)[c(1, 5, 2, 4, 3)]))) + 
          geom_bar(stat="count", fill="steelblue") + labs(x="Q11")

# Q11 distribution stratified by role
print(ggplot(df3,  aes_string(x=factor(df3[["Q11"]], 
                                       levels(df3[["Q11"]])[c(1, 5, 2, 4, 3)]), 
                              fill="Q1")) + 
        geom_bar(stat="count", position=position_dodge()) + labs(x="Q11") + 
        scale_x_discrete(drop=FALSE))

# Q12
print(ggplot(df3, aes_string(x=factor(df3[["Q12"]], 
                              levels(df3[["Q12"]])[c(2, 6, 3, 5, 4, 1)]))) + 
        geom_bar(stat="count", fill="steelblue") + labs(x="Q12"))

# Q12 distribution stratified by role
print(ggplot(df3,  aes_string(x=factor(df3[["Q12"]], 
                                       levels(df3[["Q12"]])[c(2, 6, 3, 5, 4, 1)]), 
                                       fill="Q1")) + 
        geom_bar(stat="count", position=position_dodge()) + labs(x="Q12") + 
        scale_x_discrete(drop=FALSE))

# recode levels question 13
for (name in c("Q13_1", "Q13_2", "Q13_3")) {
  levels(df3[[name]]) = c("6", "2", "3", "4", "1", "5")
}

#Q13 plots
for (name in c("Q13_1", "Q13_2", "Q13_3")) {
  # Q13 answer distributions
  print(ggplot(df3, 
          aes_string(x=factor(df3[[name]], 
                              levels(df3[[name]])[c(5, 2:4, 6, 1)]))) + 
          geom_bar(stat="count", fill="steelblue") + labs(x=name) + 
          scale_x_discrete(drop=FALSE))
  
  # Q13 distribution stratified by role
  print(ggplot(df3,  aes_string(x=factor(df3[[name]], 
                                     levels(df3[[name]])[c(5, 2:4, 6, 1)]), fill="Q1")) + 
          geom_bar(stat="count", position=position_dodge()) + labs(x=name) +
          scale_x_discrete(drop=FALSE))
}

# plots for 14_1 though 14_6
lvl = c(2,6,4,7,3,5,1)
for (name in c("Q14_1", "Q14_2", "Q14_3","Q14_4", "Q14_5", "Q14_6")) {
  # Q14 distributions
  print(ggplot(df3, 
               aes_string(x=factor(df3[[name]], levels(df3[[name]])[lvl]))) + 
          geom_bar(stat="count", fill="steelblue") + labs(x=name) + 
          scale_x_discrete(drop=FALSE))
  
  # Q14 distribution stratified by role
  print(ggplot(df3,  aes_string(x=factor(df3[[name]], levels(df3[[name]])[lvl]), fill="Q1")) + 
          geom_bar(stat="count", position=position_dodge()) + labs(x=name) +
          scale_x_discrete(drop=FALSE))
}

# plots for question 5
for (name in c("Q5_1", "Q5_2", "Q5_3","Q5_4", "Q5_5", "Q5_6")) {
  # Q5 distribution
  print(ggplot(df3, aes_string(x=name)) + 
          geom_bar(stat="count", fill="steelblue") + labs(x=name) + 
          scale_x_discrete(drop=FALSE))
  
  # Q5 distribution stratified by role
  print(ggplot(df3,  aes_string(x=name, fill="Q1")) + 
          geom_bar(stat="count", position=position_dodge()) + labs(x=name) +
          scale_x_discrete(drop=FALSE))
}


# plots for question 6
for (name in c("Q6_1", "Q6_2", "Q6_3","Q6_4", "Q6_5", "Q6_6")) {
  # distribution to Q6
  print(ggplot(df3, aes_string(x=name)) + 
          geom_bar(stat="count", fill="steelblue") + labs(x=name) + 
          scale_x_discrete(drop=FALSE))
  
  #dist to Q6 stratified by role
  print(ggplot(df3,  aes_string(x=name, fill="Q1")) + 
          geom_bar(stat="count", position=position_dodge()) + labs(x=name) +
          scale_x_discrete(drop=FALSE))
}

# plot for Q7
ggplot(df3, aes_string(x=factor(df3[["Q7"]], levels(df3[["Q7"]])[c(2, 5, 3, 4, 1)]))) + 
        geom_bar(stat="count", fill="steelblue") + labs(x="Q7") + 
        scale_x_discrete(drop=FALSE)

# plot for Q7 stratified by role
ggplot(df3, aes_string(x=factor(df3[["Q7"]], levels(df3[["Q7"]])[c(2, 5, 3, 4, 1)]), 
                        fill="Q1")) + 
  geom_bar(stat="count", position=position_dodge()) + labs(x="Q7") +
  scale_x_discrete(drop=FALSE)

# plots for question 8
for (name in c("Q8_1", "Q8_2", "Q8_3","Q8_4", "Q8_5", "Q8_6")) {
  # print distribution of ans to Q8
  print(ggplot(df3, aes_string(x=name)) + 
          geom_bar(stat="count", fill="steelblue") + labs(x=name) + 
          scale_x_discrete(drop=FALSE))
  
  # print distributions of ans to Q8 stratified by role
  print(ggplot(df3,  aes_string(x=name, fill="Q1")) + 
          geom_bar(stat="count", position=position_dodge()) + labs(x=name) +
          scale_x_discrete(drop=FALSE))
}
