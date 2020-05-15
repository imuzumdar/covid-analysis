# old data code

### loading in data, with both headers as options ### 

# data with questions as headers
df = read.xlsx2("covid_data_cleaned_20200418.xlsx", sheetIndex = 1, startRow=2, header=TRUE)
nrows=as.integer(nrow(df))

#data with question numbers as headers
df2 = read.xlsx2("covid_data_cleaned_20200418.xlsx", sheetIndex = 1, header=TRUE)
df2 = df2[-1, ]

# PROPER data frame with question numbers as headers without questions as factors
df3 = df
colnames(df3) = colnames(df2)