analyzer <- function(data, formula_predictors, cleaner, col, sub_col=NULL){
  
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
  
  # constructing model formula
  formula_reg = paste(resp, "~", formula_predictors)
  null_formula = paste(resp, "~ 1")
  
  
  # Ordinal Logistic Regression
  model = polr(formula = formula_reg, data = data, Hess=TRUE)
  summary(model)
  
  # finding pseudo R-squared
  nullmod = polr(formula = null_formula, data = data, Hess=TRUE)
  print("PSEUDO-R^2")
  print(1-logLik(model)/logLik(nullmod))
  print("ANOVA")
  print(anova(nullmod, model, test = "Chisq"))
  
  # storing coefficients
  table <- coef(summary(model))
  
  #adding pvalues to table
  p <- pnorm(abs(table[, "t value"]), lower.tail = FALSE) * 2 #pvalues
  (table <- cbind(table, "p value" = p))
  
  # diagnostics for ordinal model
  print(autoplot(model, what="qq"))
  #surrogate residuals plots to assess distributions assumptions of ordinal classifier
  print(autoplot(resids(model), what = "covariate", x = data[["Q1"]], xlab = "Q1"))
  #brant test for proportional odds between levels in response
  print("BRANT TEST")
  print(brant(model))
  output = list("model"= model,"table"= table)
}
