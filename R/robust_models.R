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

#remove participants who have tried psychedelic substances except LSD and psilocybin mushrooms
d <- subset(d,EverAya == -1 &
               EverDMT == -1 &
               EverChanga == -1 &
               EverMeskalina == -1 &
               EverPochodne == -1 &
               EverPsyloSyn == -1)

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

# Reorder motivational predictors
new_order_LSD <- c("EDI_SUM", "ReasonsLSD.SQ001.", "ReasonsLSD.SQ002.", "ReasonsLSD.SQ006.", "ReasonsLSD.SQ008.", "ReasonsLSD.SQ003.", "ReasonsLSD.SQ004.","ReasonsLSD.SQ005.", "ReasonsLSD.SQ007." )
new_order_MSH <- c("EDI_SUM", "ReasonsMushrooms.SQ001.", "ReasonsMushrooms.SQ002.", "ReasonsMushrooms.SQ006.", "ReasonsMushrooms.SQ008.", "ReasonsMushrooms.SQ003.", "ReasonsMushrooms.SQ004.","ReasonsMushrooms.SQ005.", "ReasonsMushrooms.SQ007." )

subsets[1]$Reasons_LSD <- subsets[1]$Reasons_LSD[, new_order_LSD]
subsets[4]$Reasons_Mushrooms <- subsets[4]$Reasons_Mushrooms[, new_order_MSH]

# Select doemographic data (Age, gender, education) and lifetime use of the investigated substances 
for (i in 1:6) {
  subsets[[i]] <- subsets[[i]] %>%
    mutate(Age = d$Age,
           Gender = factor(d$Plec),
           Education = factor(d$Wyksztalcenie, levels = c("ponizej sredniego", "średnie" , "licencjackie", "magisterskie i wyzej")),
           Residence = factor(d$MiejsceZamieszkania, levels = c("Wieś","Miasto do 50 tys.","Miasto od 50 tys. do 150 tys.", "Miasto od 150 tys. do 500 tys.", "Miasto powyżej 500 tys.")),
           Finance = factor(d$SytuacjaMatarialna, levels = c("bardzo ?le", "źle","średnio","dobrze", "bardzo dobrze")),
           LifetimeLSD = d$LifetimeLSD,
           LifetimeMushrooms = d$LifetimeMushrooms)
}

for (i in 1:6) {
    subsets[[i]] <- subsets[[i]] %>%
      select(Age, Gender, Education, Residence, Finance, LifetimeLSD, LifetimeMushrooms, everything())
}

# drop specific factors levels (e.g. levels related with less than 20 responses)

#1 - Drop entire predictors
subsets[1]$Reasons_LSD <- subset(subsets[1]$Reasons_LSD, select = -ReasonsLSD.SQ007.)
subsets[2]$Places_LSD <- subset(subsets[2]$Places_LSD, select = -PlacesLSD.SQ006.)
subsets[3]$WithWhom_LSD <-subset(subsets[3]$WithWhom_LSD, select = -WithWhomLSD.SQ005.)
subsets[4]$Reasons_Mushrooms <- subset(subsets[4]$Reasons_Mushrooms, select = -ReasonsMushrooms.SQ007.)
subsets[5]$Places_Mushrooms <- subset(subsets[5]$Places_Mushrooms, select =  -PlacesMushrooms.SQ006.) # Mushrooms - Specialized Centre/Ceremony
subsets[6]$WithWhom_Mushrooms <- subset(subsets[6]$WithWhom_Mushrooms, select = -WithWhomMushrooms.SQ005.) # Mushrooms - Therapist/Guide

#2 - Drop the "Most often" level of a given predictor
subset_levels <- c("wcale","rzadko","często")
subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ004. <- factor(subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ004., levels = subset_levels)

#3 - Drop "Most often" and "Often" levels of a given predictor
subset_levels <- c("wcale","rzadko")
subsets[2]$Places_LSD$PlacesLSD.SQ003. <- factor(subsets[2]$Places_LSD$PlacesLSD.SQ003., levels = subset_levels)
subsets[2]$Places_LSD$PlacesLSD.SQ007. <- factor(subsets[2]$Places_LSD$PlacesLSD.SQ007., levels = subset_levels)
subsets[3]$WithWhom_LSD$WithWhomLSD.SQ006. <- factor(subsets[3]$WithWhom_LSD$WithWhomLSD.SQ006., levels = subset_levels) #LSD - Strangers
subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ003. <- factor(subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ003., levels = subset_levels) # Mushrooms - club party
subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ004. <- factor(subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ004., levels = subset_levels)
subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ007. <- factor(subsets[5]$Places_Mushrooms$PlacesMushrooms.SQ007., levels = subset_levels) # Mushrooms - Other
subsets[6]$WithWhom_Mushrooms$WithWhomMushrooms.SQ006. <- factor(subsets[6]$WithWhom_Mushrooms$WithWhomMushrooms.SQ006., levels = subset_levels) #Mushrooms - Strangers

# and now we can use lapply wrapper to do the models in one line
# lapply is a wrapper that goes through every element in the list and apply the function
models <- lapply(subsets, fit_robust_lm)
# and then anovas tables
anovas <- lapply(models, Anova, type = "III")

saveRDS(models, "models_subsample.rds")
saveRDS(anovas, "anovas_subsample.rds")