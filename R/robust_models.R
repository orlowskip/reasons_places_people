library(robustbase)
library(car)
d <- read.csv('./data/dane_poprawione.csv') # . is the home directory 

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

# function to fit robust linear regression 
# takes the subsetted data and returns the model
fit_robust_lm <- function(.data) {
  model <- lmrob(EDI_SUM ~ ., data = .data, 
                 na.action = na.omit, 
                 fast.s.large.n = Inf, 
                 setting = "KS2014")
  model
}

# use of the function
reasons_lsd_model <- fit_robust_lm(reasons_lsd) 
  
places_lsd_model <- (places_lsd)

# use with lapply wrapper
# takes a list of subsetted datasets and returns a list of models 
models <- lapply(list(reasons_lsd, places_lsd), fit_robust_lm)

# Anova() function from car package will return anova table from the model
Anova(reasons_lsd_model, type = "III") # type = chooses the type of sum of squares calculations, use type = "III" 
Anova(places_lsd, type = "III") 
# can use with lapply as well

anovas <- lapply(models, Anova, type = "III")

