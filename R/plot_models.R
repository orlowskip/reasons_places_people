# Load required libraries
library(dplyr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

# Read in models and anovas objects from RDS files
models <- readRDS("models_with_use.rds")
anovas <- readRDS("anovas_with_use.rds")

# Plotting results
selected_predictors_R_LSD =  names(models$Reasons_LSD$coefficients)[grep(".SQ", names(models$Reasons_LSD$coefficients))]

p_Reasons_LSD = plot_model(models$Reasons_LSD,type="est",
                           terms = selected_predictors_R_LSD,
                           vline.color = "#C0C0C0", 
                           title = "Reasons LSD",
                           show.values = TRUE,
                           show.p = TRUE,
                           value.offset = .4,
                           value.size = 7,
                           dot.size = 3,
                           line.size = 1.2,
                           axis.title = "EDI",
                           axis.labels = c("Other: Yes",
                                           "Boredom: Yes",
                                           "Curiosity: Yes",
                                           "Recreational: Yes",
                                           'Cognitive: Yes',
                                           "Self-healing: Yes",
                                           "Spiritual: Yes",
                                           "Self-development: Yes"));

p_Reasons_LSD <- p_Reasons_LSD + theme_sjplot2(base_size = 25) + scale_y_continuous(limits=c(-160,160));p_Reasons_LSD

selected_predictors_R_MSH =  names(models$Reasons_Mushrooms$coefficients)[grep(".SQ", names(models$Reasons_Mushrooms$coefficients))]

p_Reasons_MSH = plot_model(models$Reasons_Mushrooms,type="est",
                           terms = selected_predictors_R_MSH,
                           vline.color = "#C0C0C0", 
                           title = "Reasons Mushrooms",
                           show.values = TRUE,
                           show.p = TRUE,
                           value.offset = .4,
                           value.size = 7,
                           dot.size = 3,
                           line.size = 1.2,
                           axis.title = "EDI",
                           axis.labels = c("Other: Yes",
                                           "Boredom: Yes",
                                           "Curiosity: Yes",
                                           "Recreational: Yes",
                                           'Cognitive: Yes',
                                           "Self-healing: Yes",
                                           "Spiritual: Yes",
                                           "Self-development: Yes"));

p_Reasons_MSH <- p_Reasons_MSH + theme_sjplot2(base_size = 25) + scale_y_continuous(limits=c(-160,160));p_Reasons_MSH

selected_predictors_P_LSD =  names(models$Places_LSD$coefficients)[grep(".SQ", names(models$Places_LSD$coefficients))]

p_Places_LSD = plot_model(models$Places_LSD,type="est",
                          terms = selected_predictors_P_LSD,
                          group.terms = c(1,1,1,2,2,2,3,3,4,4,4,5,5,5,6,7,7),
                          vline.color = "#C0C0C0",
                          title = "Enviroments - LSD",
                          show.values = TRUE,
                          show.p = TRUE,
                          value.offset = .4,
                          value.size = 7,
                          dot.size = 3,
                          line.size = 1.2,
                          axis.title = "EDI",
                          axis.labels = c("often","Other:   rarely",
                                          'Ceremony:   rarely',
                                          'most frequently','often', 'Nature:   rarely',
                                          'most frequently','often', 'Festival   rarely',
                                          'often', 'Club party:   rarely',
                                          'most frequently',"often", "Friend's house:   rarely",
                                          'most frequently','often', "Own house:   rarely"),
                          colors = c('#F05C3BFF','#075149FF', '#197EC0FF','#370335FF','#C80813FF','#1A9993FF','#8A4198FF'))

p_Places_LSD <- p_Places_LSD + theme_sjplot2(base_size = 25) + scale_y_continuous(limits=c(-160,160));p_Places_LSD


selected_predictors_P_MSH =  names(models$Places_Mushrooms$coefficients)[grep(".SQ", names(models$Places_Mushrooms$coefficients))]

p_Places_MSH = plot_model(models$Places_Mushrooms,type="est",
                          terms = selected_predictors_P_MSH,
                          group.terms = c(1,1,1,2,2,2,3,4,4,4,5,5,5,6,7),
                          vline.color = "#C0C0C0",
                          title = "Enviroments - Psilocybin Mushrooms",
                          show.values = TRUE,
                          show.p = TRUE,
                          value.offset = .4,
                          value.size = 7,
                          dot.size = 3,
                          line.size = 1.2,
                          axis.title = "EDI",
                          axis.labels = c("Other:   rarely",
                                          'Ceremony:   rarely',
                                          'most frequently','often', 'Nature:   rarely',
                                          'most frequently','often', 'Festival   rarely',
                                          'Club party:   rarely',
                                          'most frequently',"often", "Friend's house:   rarely",
                                          'most frequently','often', "Own house:   rarely"),
                          colors = c('#F05C3BFF','#075149FF', '#197EC0FF','#370335FF','#C80813FF','#1A9993FF','#8A4198FF'))

p_Places_MSH <- p_Places_MSH + theme_sjplot2(base_size = 25) + scale_y_continuous(limits=c(-160,160));p_Places_MSH


selected_predictors_Pe_LSD =  names(models$WithWhom_LSD$coefficients)[grep(".SQ", names(models$WithWhom_LSD$coefficients))]

p_People_LSD = plot_model(models$WithWhom_LSD,type="est",
                          terms = selected_predictors_Pe_LSD,
                          group.terms = c(1,1,1,2,2,2,3,3,3,4,4,4,5,6,6),
                          vline.color = "#C0C0C0",
                          title = "Social Context - LSD",
                          show.values = TRUE,
                          show.p = TRUE,
                          value.offset = .4,
                          value.size = 7,
                          dot.size = 3,
                          line.size = 1.2,
                          axis.title = "EDI",
                          axis.labels = c('often', "With others (yet unknown persons):   rarely",
                                          'With an experienced guide:   rarely',
                                          'most frequently','often', 'With friends   rarely',
                                          'most frequently','often', 'With close friends:   rarely',
                                          'most frequently','often', 'With partner:   rarely',
                                          'most frequently',"often", "Alone:   rarely"),
                          colors = c('#F05C3BFF','#075149FF', '#197EC0FF','#370335FF','#C80813FF','#1A9993FF'))

p_People_LSD <- p_People_LSD + theme_sjplot2(base_size = 25) + scale_y_continuous(limits=c(-160,160));p_People_LSD

selected_predictors_Pe_MSH =  names(models$WithWhom_Mushrooms$coefficients)[grep(".SQ", names(models$WithWhom_Mushrooms$coefficients))]

p_People_MSH = plot_model(models$WithWhom_Mushrooms,type="est",
                          terms = selected_predictors_Pe_MSH,
                          group.terms = c(1,1,1,2,2,2,3,3,3,4,4,4,5,6,6),
                          vline.color = "#C0C0C0",
                          title = "Social Context - Psilocybin Mushrooms",
                          show.values = TRUE,
                          show.p = TRUE,
                          value.offset = .4,
                          value.size = 7,
                          dot.size = 3,
                          line.size = 1.2,
                          axis.title = "EDI",
                          axis.labels = c('often', "With others (yet unknown persons):   rarely",
                                          'With an experienced guide:   rarely',
                                          'most frequently','often', 'With friends   rarely',
                                          'most frequently','often', 'With close friends:   rarely',
                                          'most frequently','often', 'With partner:   rarely',
                                          'most frequently',"often", "Alone:   rarely"),
                          colors = c('#F05C3BFF','#075149FF', '#197EC0FF','#370335FF','#C80813FF','#1A9993FF'));

p_People_MSH <- p_People_MSH + theme_sjplot2(base_size = 25) + scale_y_continuous(limits=c(-160,160));p_People_MSH

# Define the names of the plots to be saved
plot_names <- c("p_Reasons_LSD", "p_Reasons_MSH", "p_Places_LSD", "p_Places_MSH", "p_People_LSD", "p_People_MSH")

# Create a directory named 'Plots' if it does not already exist
if (!dir.exists("Plots")) {
  dir.create("Plots")
}

# Loop to save the plots
for (name in plot_names) {
  plot_object <- get(name) # Retrieve the plot object by the given name
  filename <- paste0("Plots/", name, ".png") # Construct the file name for the plot
  
  # Set the width and height based on the plot name
  if (grepl("Reasons", name)) {
    width <- 800
    height <- 600
  } else {
    width <- 800
    height <- 1000
  }
  
  # Save the plot as a PNG file with the specified width and height
  png(filename, width = width, height = height)
  print(plot_object)
  dev.off() # Close the graphics device
}