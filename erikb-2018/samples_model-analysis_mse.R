
### README ### 
#' This file runs an MSE analysis for the sample-based model using functions in `samples_model-basic_fxns`.
#' 
#' The model attempts to simulate a process of estimating quantity whereby the participant
#' has a noisy estimate of the magnitude of the number of dots shown and chooses an exact numerical estimate
#' based on the probability that this magnitude is greater or less than the magnitudes on previously seen trials.
#' In addition, it has a parameter for K 'bumpers' of rough magnitude -> number mappings which are assumed to align
#' with people's existing knowledge of certain magnitudes and prevent the model from wildly over or underestimating when
#' it sees a number which is higher or lower than any previous trials. 
#' 
#' The model uses data from `numberline/numexpt`


setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list=ls())

library(stats4)
library(tidyverse)
library(Matrix)

source('samples_model-fxns_basic.R')


#################
### FUNCTIONS ###
#################

# General function to calculate mean squared error of estimates
calculate.mse = function(data) {
  data = data %>%
    mutate(subj.sq.error = (answer - num_dots) ^ 2,
           model.sq.error = (model.answer - num_dots) ^ 2)
  
  return(data)
}

# plot theme (originally taken from `perceptual_analysis.R`)
individ_plot_theme_mse = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 20),
  axis.text.x = element_text(size = 20, hjust = 1), #, angle = 60, hjust = 0, vjust = 0.1
  # legend text
  legend.text = element_text(size = 24),
  # facet text
  strip.text = element_text(face = "bold", size = 28),
  # backgrounds, lines
  panel.background = element_blank(),
  #strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

plot.mse = function(data) {
  data %>% ggplot(aes(x = N)) +
    geom_line(aes(y = mse.subj, color = "subjects")) +
    geom_line(aes(y = mse.mod, color = "model")) +
    geom_point(aes(y = mse.mod, color = "model"), size = 2) +
    scale_color_manual(name = element_blank(), 
                       values = c("subjects" = "blue", "model" = "red")) +
    scale_y_continuous(limits = c(0, 4000)) +
    ggtitle("Model performance with increasing samples") +
    labs(x = "Samples", y = "Estimate error (MSE)") +
    individ_plot_theme_mse + # TODO consider making this a param in the function
    theme(legend.position = c(0.2, 0.2),
          legend.background = element_rect(color = "gray80", size = 0.5, linetype = "solid"))
  
}


################
### ANALYSIS ###
################

# MODEL GLOBALS
model.fxn = run.model.baseline # model to use for this analysis
p.bumper = 1.0 # set bumper probability manually for this analysis (don't use default)

# MSE GLOBALS
save.data = FALSE # whether to save the data afterwards
min.samples = 1 # lower bound on samples tested in MSE calculations
max.samples = 25 # upper bound on samples tested in MSE calculations

# NB: this can take a while!! (approx. 60 seconds per run, but this increases as N_SAMPLES increases)
mse.df = data.frame('N' = numeric(), 'p' = numeric(), 'mse.subj' = numeric(), 'mse.mod' = numeric())

for (n in seq(min.samples, max.samples)) {
  print(paste("Running model with N_SAMPLES = ", n, ", P_BUMPER = ", p.bumper))
  
  data = model.fxn(n.samples = n,
               p.bumper = p.bumper)
  
  data = calculate.mse(data)
  
  mse.df = rbind(mse.df, data.frame('N' = n, 'p' = p.bumper,
                                    'mse.subj' = mean(data$subj.sq.error),
                                    'mse.mod' = mean(data$model.sq.error[data$trial > n])))
}

# Graph MSE results
mse.plot = plot.mse(mse.df)
mse.plot

# optional: save output data
if (save.data) {
  save(mse.df, file = 'samples_model-mse.RData')
}
