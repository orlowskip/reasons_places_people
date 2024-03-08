library(car)
library(dplyr)
library(bestNormalize)
# load the functions used in the script
# create_predictors()
# and fit_robust_lm
source("./R/functions.R")
# read and clean the data 
d <- read.csv('./data/dane_poprawione.csv', fileEncoding="UTF-8") # . is the home directory 
d <- subset(d, Plec != "inna")
d <- select(d, -starts_with(c("OtherPlaces", "OtherReasons")))



# change levels order for WithWhom and Places
# change levels order for WithWhom and Places
d <- d %>% mutate_at(vars(starts_with(c("WithWhom", 
                                        "Places"))), 
                     ~factor(.,
                             levels = c("wcale",
                                        "rzadko",
                                        "często",
                                        "najczęściej"))) %>%
  mutate(LifetimeLSD = yeojohnson(LifetimeLSD)$x.t,
         LifetimeMushrooms = yeojohnson(LifetimeMushrooms)$x.t)


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

# wybierz z datasetu dane doemograficzne (Wiek, płeć, wykształcenie)
for (i in 1:6) {
  subsets[[i]] <- subsets[[i]] %>%
    mutate(Age = d$Age,
           Gender = factor(d$Plec),
           Education = factor(d$Wyksztalcenie, levels = c("ponizej sredniego", "średnie" , "licencjackie", "magisterskie i wyzej")),
           Residence = factor(d$MiejsceZamieszkania, levels = c("Wieś","Miasto do 50 tys.","Miasto od 50 tys. do 150 tys.", "Miasto od 150 tys. do 500 tys.", "Miasto powyżej 500 tys.")),
           Finance = factor(d$SytuacjaMatarialna, levels = c("bardzo ?le", "źle","średnio","dobrze", "bardzo dobrze")),
           LifetimeLSD = d$LifetimeLSD,
           LifetimeMushrooms = d$LifetimeMushrooms
           )
}

for (i in 1:6) {
  if (i <= 3) {
    subsets[[i]] <- subsets[[i]] %>%
      select(Age, Gender, Education, Residence, Finance, LifetimeLSD, everything()) %>%
      select(-c(LifetimeMushrooms))
  }
  if (i > 3) {
    subsets[[i]] <- subsets[[i]] %>%
      select(Age, Gender,Education, Residence, Finance, LifetimeMushrooms, everything()) %>%
      select(-c(LifetimeLSD))
  }
  
}

# and now we can use lapply wrapper to do the models in one line
# lapply is a wrapper that goes through every element in the list and apply the function
models <- lapply(subsets, fit_robust_lm)
# and then anovas tables
anovas <- lapply(models, Anova, type = "III")

saveRDS(models, "models_with_use.rds")
saveRDS(anovas, "anovas_with_use.rds")
