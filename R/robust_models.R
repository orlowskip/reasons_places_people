library(robustbase)
library(car)
library(dplyr)
d <- read.csv('./data/dane_poprawione.csv') # . is the home directory 
d <- select(d, -starts_with(c("OtherPlaces", "OtherReasons")))
# make a substet of the d data.frame with predictor (EDI_SUM) and predictors of choosing 
reasons_lsd <- subset(d, select = c("EDI_SUM", 
                                    "ReasonsLSD.SQ001.", 
                                    "ReasonsLSD.SQ002.", 
                                    "ReasonsLSD.SQ003.",
                                    "ReasonsLSD.SQ004.",
                                    "ReasonsLSD.SQ005.",
                                    "ReasonsLSD.SQ006.",
                                    "ReasonsLSD.SQ007.",
                                    "ReasonsLSD.SQ008."))
# same as above
places_lsd <- subset(d, select = c("EDI_SUM", 
                                   "PlacesLSD.SQ001.", 
                                   "PlacesLSD.SQ002.", 
                                   "PlacesLSD.SQ003.",
                                   "PlacesLSD.SQ004.",
                                   "PlacesLSD.SQ005.",
                                   "PlacesLSD.SQ006.",
                                   "PlacesLSD.SQ007."))
#function to quickly generate predictor names from our database
create_predictors <- function(condition, substance) {
  require(dplyr)
  cmax <- length(select(d, contains(condition) & contains(substance)))
  predictors <- c(paste0(condition, substance, ".SQ00", 1:cmax, "."))
  predictors
}
###################################
#creating sets containing predictors and predicted value
reasons_lsd <- subset(d, select = c("EDI_SUM", create_predictors('Reasons', 'LSD')))
places_lsd <- subset(d, select = c("EDI_SUM", create_predictors('Places', 'LSD')))
with_whom_lsd <- subset(d, select = c("EDI_SUM", create_predictors('WithWhom', 'LSD')))
reasons_mushrooms <- subset(d, select = c("EDI_SUM", create_predictors('Reasons', 'Mushrooms')))
places_mushrooms <- subset(d, select = c("EDI_SUM", create_predictors('Places', 'Mushrooms')))
with_whom_mushrooms <- subset(d, select = c("EDI_SUM", create_predictors('WithWhom', 'Mushrooms')))


# function to fit robust linear regression 
# takes the subsetted data and returns the model
fit_robust_lm <- function(.data) {
  model <- lmrob(EDI_SUM ~ ., data = .data, # EDI_SUM ~ . means "predict edi using all other vars in the data 
                 na.action = na.omit, 
                 fast.s.large.n = Inf, 
                 setting = "KS2014")
  model
}

# use of the function
reasons_lsd_model <- fit_robust_lm(reasons_lsd) 
places_lsd_model <- fit_robust_lm(places_lsd) 
with_whom_lsd_model <- fit_robust_lm(with_whom_lsd) 
reasons_mushrooms_model <- fit_robust_lm(reasons_mushrooms) 
places_mushrooms_model <- fit_robust_lm(places_mushrooms) 
with_whom_mushrooms_model <- fit_robust_lm(with_whom_mushrooms) 


# use with lapply wrapper
# takes a list of subsetted datasets and returns a list of models 
models <- lapply(list(reasons_lsd, places_lsd), fit_robust_lm)

# Anova() function from car package will return anova table from the model
Anova(reasons_lsd_model, type = "III") # type = chooses the type of sum of squares calculations, use type = "III" 
Anova(places_lsd, type = "III") 

# can use with lapply as well
anovas <- lapply(models, Anova, type = "III")

