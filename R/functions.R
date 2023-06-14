#function to quickly generate predictor names from our database
create_predictors <- function(condition, substance) {
  require(dplyr)
  cmax <- length(select(d, contains(condition) & contains(substance)))
  predictors <- c(paste0(condition, substance, ".SQ00", 1:cmax, "."))
  predictors
}

# function to fit robust linear regression 
# takes the subsetted data and returns the model
fit_robust_lm <- function(.data) {
  require(robustbase)
  model <- lmrob(EDI_SUM ~ ., data = .data, # EDI_SUM ~ . means "predict edi using all other vars in the data 
                 na.action = na.omit, 
                 fast.s.large.n = Inf, 
                 setting = "KS2014")
  model
}
