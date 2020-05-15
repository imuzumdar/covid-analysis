cleaner_5 <- function(data, sub_col) {
  new_data = data
  column = paste("Q5", sub_col, sep="_")
  zero_ans = which(new_data[[column]] == "")
  print(length(zero_ans))
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
    new_data[[column]] = droplevels(new_data[[column]])
  }
  levels(new_data[[column]]) = c("1","2","3","4","5")
  return(new_data)
}

cleaner_6 <- function(data, sub_col) {
  new_data = data
  column = paste("Q6", sub_col, sep="_")
  zero_ans = which(new_data[[column]] == "")
  print(length(zero_ans))
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
  }
  new_data[[column]] = droplevels(new_data[[column]])
  levels(new_data[[column]]) = c("1","2","3","4","5")
  return(new_data)
}

cleaner_7 <- function(data) {
  new_data = data
  zero_ans = which(new_data[["Q7"]] == "")
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
  }
  new_data[["Q7"]] = droplevels(new_data[["Q7"]])
  levels(new_data[["Q7"]]) = c(5,1,3,4,2)
  new_data <- within(new_data, Q7 <- reorder(Q7))
  return(new_data)
}


cleaner_8 <- function(data, sub_col) {
  new_data = data
  column = paste("Q8", sub_col, sep="_")
  zero_ans = which(new_data[[column]] == "")
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
  }
  new_data[[column]] = droplevels(new_data[[column]])
  levels(new_data[[column]]) = c("1","2","3","4","5")
  return(new_data)
}


cleaner_10 <- function(data) {
  new_data = data
  new_data = new_data[-which(new_data[["Q10"]] == "Other. Please specify."),]
  new_data[["Q10"]] = droplevels(new_data[["Q10"]])
  return(new_data)
}

cleaner_11 <- function(data) {
  new_data = data
  zero_ans = which(new_data[["Q11"]] == "")
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
  }
  new_data[["Q11"]] = droplevels(new_data[["Q11"]])
  levels(new_data[["Q11"]]) = c(5,3,1,2,4)
  new_data <- within(new_data, Q11 <- reorder(Q11))
  return(new_data)
}


cleaner_12 <- function(data) {
  new_data = data
  zero_ans = which(new_data[["Q12"]] == "")
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
  }
  new_data[["Q12"]] = droplevels(new_data[["Q12"]])
  levels(new_data[["Q12"]]) = c(5,3,1,2,4)
  new_data <- within(new_data, Q12 <- reorder(Q12))
  return(new_data)
}

cleaner_13 <- function(data, sub_col) {
  new_data = data
  column = paste("Q13", sub_col, sep="_")
  zero_ans = which(new_data[[column]] == "6")
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
  }
  new_data[[column]] = droplevels(new_data[[column]])
  return(new_data)
}

# LAST ONE
cleaner_14 <- function(data, sub_col) {
  new_data = data
  column = paste("Q14", sub_col, sep="_")
  zero_ans = which(new_data[[column]] == "7")
  if (length(zero_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-zero_ans,]
  }
  na_ans = which(new_data[[column]] == "6")
  if (length(na_ans) != 0) {
    print("CLEANING DATA")
    new_data = new_data[-na_ans,]
  }
  new_data[[column]] = droplevels(new_data[[column]])
  return(new_data)
}

