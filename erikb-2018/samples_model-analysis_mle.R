### README ###
#' This file confirms the slope and cutoff fit across subject and model estimates.
#' 
#' It uses functions in `samples_model-fxns_basic` to read the data and
#' then uses more advanced functions in `samples_model-fxns_drift` to
#' fit lines to the model and human estimates


setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list=ls())

library(Rmisc) # needed for call to `multiplot`
# Fetch relevant model functions from samples_model
source('samples_model-fxns_basic.R')
# Fetch relevant functions for fitting lines to model data
source('samples_model-fxns_drift.R')




##########################
### ANALYSIS FUNCTIONS ###
##########################


plot.human.fit.scatter = function(subj.slopes) {
  ggplot(data = subj.slopes, aes(x = cutoff, y = slope)) +
    geom_point(size = 3, alpha = 0.5, color = "blue") +
    facet_wrap(~subj, ncol = 5, scales = "free") +
    ggtitle("") +
    labs(x = "Fitted cutoff", y = "Fitted slope") +
    theme(panel.background = element_blank(),
          strip.background = element_blank(),
          panel.grid = element_line(color = "gray"),
          axis.line = element_line(color = "black"),
          axis.title.y = element_text(face = "bold", size = 24),
          axis.title.x = element_text(face = "bold", size = 24),
          strip.text = element_text(face = "bold", size = 20))
}

################
### ANALYSIS ###
################

# Read subject data and extract relevant columns
data = read.data(DATA, TRIALS)

subj.data = data %>%
  select(subject, trial, num_dots, answer)
  #filter(subject %in% c(1,2)) # TODO remove this eventually


# Run multiple iterations of slope fitting
ITERS = 10
slope.fits = data.frame(subj = character(),
                        iter = numeric(),
                        cutoff = numeric(),
                        slope = numeric(),
                        se = numeric())


for (iter_index in seq(ITERS)) {
  print(paste("Fitting slopes: iteration ", iter_index))
  PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
  names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")
  
  PRIORS = list()
  PRIORS[[1]] = function(x){-dnorm(x, 2, 3.5, log = T)}
  PRIORS[[2]] = function(x){-dnorm(x, 0, 0.5, log = T)}
  PRIORS[[3]] = function(x){-dnorm(x, -1, 0.25, log = T)}
  
  # Fit static subject data
  bipower.fits.subj = data.frame(do.call(rbind, by(subj.data, subj.data$subject, brutefit)))
  print(paste("Failed bipower fits:", sum(bipower.fits.subj$logL == -9999)))
  slope.fits = rbind(slope.fits, data.frame(subj = bipower.fits.subj$subject,
                                            iter = iter_index,
                                            cutoff = 10^bipower.fits.subj$a,
                                            slope = 10^bipower.fits.subj$b,
                                            se = 10^bipower.fits.subj$s))
  
}



#############
### PLOTS ###
#############

plot.human.fit.scatter(slope.fits) # NB: save plot with size 1800x1500 for ideal proportions
  

