kw_analyzer <- function(data, cleaner, col, sub_col=NULL){
  # clean data
  if (!is.null(sub_col)) {
    data = cleaner(data, sub_col)
    resp = paste("Q",col, sep="")
    resp = paste(resp, sub_col, sep = "_")
  }
  else {
    data = cleaner(data)
    resp = paste("Q",col, sep="")
  }
  
  kw_formula = paste(resp,"~","Q1")
  data[[resp]] = as.numeric(data[[resp]])
  print(kw_formula)
  # kruskal.test(x = data[["Q1"]], g = data[[resp]])
  
  print(kruskal.test(as.formula(kw_formula), data = data))
  print(dunnTest(as.formula(kw_formula),data=data, method="bh")) 
}
