
### README ### 
#' This file runs the sample-based model using functions in `samples_model-basic_fxns`.
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

##########################
### ANALYSIS FUNCTIONS ###
##########################

# Graphing functions
my.log.breaks = function(lims){
  majors = seq(floor(log10(lims[1])), ceiling(log10(lims[2])), by = 1)
  minors = log10(unlist(lapply(majors[-1], function(x){seq(10 ^ (x - 1), 9 * 10 ^ (x - 1), by = 10 ^ (x - 1))})))
  return(list(majors, minors))
}

mylogx = function(lims){
  breaks = my.log.breaks(lims)
  scale_x_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

mylogy = function(lims){
  breaks = my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

individ_plot_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 32),
  axis.title.x = element_text(face = "bold", size = 32),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 16),
  axis.text.x = element_text(size = 14, angle = 90, hjust = 0, vjust = 0),
  # legend text
  legend.text = element_text(size = 24),
  # facet text
  strip.text = element_text(face = "bold", size = 28),
  strip.background = element_blank(),
  # backgrounds, lines
  panel.background = element_blank(),
  #strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

view.sample.data = function(data) {
  # Quick view of true numbers and estimates across trials
  sample.results = data %>%
    filter(subject == 1) %>% # set subject number here
    select(subject, trial, num_dots, model.answer) %>%
    arrange(trial)
  return(sample.results)
}

plot.sample.subjects = function(data, sample.subjects = c()) {
  sample.subject.plot = data %>%
    filter(subject %in% sample.subjects,
           trial > N_SAMPLES) %>%
    ggplot(aes(x = num_dots)) +
    geom_point(aes(y = answer, color = "subject"), alpha = 0.5) +
    geom_point(aes(y = model.answer, color = "model"), alpha = 0.5) +
    geom_abline() +
    #ggtitle(paste("Model estimates, calibration = ", N_SAMPLES, " samples, p = ", P_BUMPER)) +
    ggtitle(paste("Sample estimates")) +
    labs(x = "Number presented", y = "Number estimated") +
    scale_color_manual(name = "Estimates", 
                       values = c("subject" = "blue", "model" = "red")) +
    mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
    mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE)) + 
    individ_plot_theme + # TODO pass this in to the parent function
    facet_wrap(~subject, ncol = 3)
  #labeller = labeller(subject = c("3" = "Subject 3", "7" = "Subject 7", "9" = "Subject 9")))
  
  return(sample.subject.plot)
}

# TODO this can probably be consolidated with the above function
plot.all.subjects = function(data) {
  # Graph all subjects and model estimates side by side
  full.subject.plot = data %>%
    filter(trial > N_SAMPLES) %>%
    ggplot(aes(x = num_dots)) +
    geom_point(aes(y = answer, color = "subject"), alpha = 0.25, size = 0.5) +
    geom_point(aes(y = model.answer, color = "model"), alpha = 0.25, size = 0.5) +
    geom_abline() +
    #ggtitle(paste("Model estimates, calibration = ", N_SAMPLES, " samples, p = ", P_BUMPER)) +
    ggtitle(paste("Model and participant estimates")) +
    labs(x = "Number presented", y = "Number estimated") +
    scale_color_manual(name = "Estimates", 
                       values = c("subject" = "blue", "model" = "red")) +
    mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
    mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
    theme(axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 14),
          legend.position = "bottom") +
    facet_wrap(~subject, ncol = 6)
  
  return(full.subject.plot)
}


plot.subject.error = function(data) {
  # Understanding over- and under-estimation (line segments show difference between num.dots and estimate)
  est.error.plot = data %>% # single subject
    filter(subject == 1) %>%
    ggplot(aes(x = trial)) +
    geom_point(aes(y = log10(num_dots), color = "true number"), alpha = 0.5) +
    geom_point(aes(y = log10(model.answer), color = "model estimate"), alpha = 0.5) +
    geom_segment(aes(y = log10(num_dots), yend = log10(model.answer), xend = trial)) +
    ggtitle("Model estimates v. true numbers across trials") +
    labs(y = "Number of dots (log10)", x = "trial number") +
    scale_color_manual(name = "Estimates", 
                       values = c("true number" = "blue", "model estimate" = "red")) +
    theme(axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 20, face = "bold"),
          legend.position = "bottom")
  
  return(est.error.plot)
}

plot.quartile.estimates = function(data) {
  # Compare each quartile of model estimates to make sure it's not getting worse
  data = data %>%
    mutate(quartile = floor(trial / 75.25) + 1)
  
  quartile.plot = data %>% # all subjects side by side
    filter(trial > N_SAMPLES) %>%
    ggplot(aes(x = num_dots, y = model.answer, color = as.factor(quartile))) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_abline() +
    ggtitle(paste("Model estimates, calibration = ", N_SAMPLES, " samples, p = ", P_BUMPER)) +
    labs(x = "Number presented", y = "Number estimated") +
    mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
    mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
    theme(axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 14),
          legend.position = "bottom") +
    facet_wrap(~subject, ncol = 6)
  
  return(quartile.plot)
}



##################
### RUN MODELS ###
##################

print(paste("Running model with CoV: ", calculate.cov(PERCEIVED_DOTS_NOISE_SD)))
data.base = run.model.baseline()
data.iv = run.model.individ.memories()




#############
### PLOTS ###
#############

# View sample estimates for sanity check
sample.data.base = view.sample.data(data.base)
sample.data.iv = view.sample.data(data.iv)


# Graph three sample subjects and model estimates for a close look
sample.subjects = c(6, 11, 22)
sample.subj.plot.base = plot.sample.subjects(data.base, sample.subjects)
sample.subj.plot.iv = plot.sample.subjects(data.iv, sample.subjects)


# Graph all subjects
full.subj.plot.base = plot.all.subjects(data.base)
full.subj.plot.iv = plot.all.subjects(data.iv)


# View estimate errors for any large patterns
est.error.plot.base = plot.subject.error(data.base)
est.error.plot.iv = plot.subject.error(data.iv)

# View estimates by quartile to ensure no degradation
quartile.plot.base = plot.quartile.estimates(data.base)
quartile.plot.base = plot.quartile.estimates(data.iv)


### Print relevant plots/data ###
# sample.data
sample.subj.plot.base
full.subj.plot.iv
# est.error.plot
# quartile.plot






