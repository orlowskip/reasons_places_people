library(car)
library(dplyr)
# load the functions used in the script
# create_predictors()
# and fit_robust_lm
source("./R/functions.R")
# read and clean the data 
d <- read.csv('./data/dane_poprawione.csv') # . is the home directory 
d <- select(d, -starts_with(c("OtherPlaces", "OtherReasons")))

# change levels order for WithWhom and Places
d <- d %>% mutate_at(vars(starts_with("WithWhom")), 
                     ~factor(.,
                             levels = c("wcale",
                                        "rzadko",
                                        "często",
                                        "najczęściej"))) %>%
  mutate_at(vars(starts_with("Reasons")), 
            ~factor(.,
                    levels = c("wcale",
                               "rzadko",
                               "często",
                               "najczęściej")))

# maybe we can use a for loop to make it cleaner
# conditions names
conditions <- c("Reasons", "Places", "WithWhom")
# substances names
substances <- c("LSD", "Mushrooms")
# empty list for filling with subsets
subsets <- list()
# empty vector for filling with names 
subset_names <- c()
# loop over the substances
for (i in substances) {
  # and over the conditions
  for (j in conditions) {
    # make a vector of names to name the list of subsets
    subset_names <- c(subset_names, paste(j, i, sep = "_"))
    # create a temp subset and coerce to a list
    temp_subset <- list(subset(d,
                          select = c("EDI_SUM",
                                     create_predictors(j, i))))
    # append subsets to a list
    subsets <- append(subsets,
                   temp_subset)
  }
} 
# name the subsets in the list
names(subsets) = subset_names
# and now we can use lapply wrapper to do the models in one line
# lapply is a wrapper that goes through every element in the list and apply the function
models <- lapply(subsets, fit_robust_lm)
# and then anovas tables
anovas <- lapply(models, Anova, type = "III")

