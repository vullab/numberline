
rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/numberline/numexpt/R/")
library(stats4)
library(tidyverse)
library(Matrix)

### README ### 
#' This model attempts to simulate a process of estimating quantity whereby the participant
#' has a noisy estimate of the magnitude of the number of dots shown and chooses an exact numerical estimate
#' based on the probability that this magnitude is greater or less than the magnitudes on previously seen trials.
#' In addition, it has a parameter for K 'bumpers' of rough magnitude -> number mappings which are assumed to align
#' with people's existing knowledge of certain magnitudes and prevent the model from wildly over or underestimating when
#' it sees a number which is higher or lower than any previous trials. 
#' 
#' This model should:
#' 1. display the drift patterns characteristic of experiment participants, and 
#' 2. generate guesses that are broadly similar to the guesses participants make
#' 


###############
### GLOBALS ###
###############

# Globals for experiment setup and data
DATA = "../data/"
MIN_ESTIMATE = 1 # lowest number in the range of dots
MAX_ESTIMATE = 1000 # highest number in the range of dots
SUBJ = 17 # sample representative participant



#################
### FUNCTIONS ###
#################

# Data reading
read.data = function(filepath) {
  # Copied from fit.models.2014-06-02.R
  files = list.files(filepath)
  data = data.frame()
  subject = 1
  for (f in files) {
    q = read.csv2(paste(filepath, f, sep = ""), sep = ",", header = T, colClasses = "character")
    q$subject = subject
    data = rbind(data, q)
    subject = subject + 1
  }
  return(data)
}


# Data processing util
to.num <- function(x){as.numeric(as.character(x))}


# Sample previous trials
# NB: currently this just fetches the last N trials before the current one,
# but could easily be modified to e.g. sample at random
sample.previous.trials = function(trial.i, single.subject.data){
  prev.trials = single.subj.data %>%
    filter(trial %in% seq(trial.i - N_TRIALS_CALIBRATION, trial.i - 1))
  
  return(prev.trials)
}


# Sample "bumper" mappings by adding known magnitude -> number mappings to previous trials
# that are being used for likelihood calculation
sample.bumper.mappings = function(prev.trials) {
  prev.trials = prev.trials %>%
    add_row(subject = unique(prev.trials$subject),
            num.dots = BUMPER_SET,
            model.answer = BUMPER_SET,
            bumper.trial = TRUE)
  
  return(prev.trials)
}


# calculate log likelihood using magnitudes and number estimates from previous trials
calculate.loglik = function(prev.trials) {
  # matrix of number estimates initialized for use in calculation of likelihoods in loop below
  # NB: this matrix A doesn't need to be created every time we call this function but it doesn't slow it down much
  # so I'm keeping it here for compactness
  A = matrix(rep(1:MAX_ESTIMATE, each = N_TRIALS_CALIBRATION + K_BUMPERS), nrow = N_TRIALS_CALIBRATION + K_BUMPERS)
  
  # discretize possible answer estimates, set log likelihood at each value to be either p.mag.greater or 1 - p.mag.greater
  # use matrix with a row for each of the previous trials and a column for each candidate estimate value.
  # each row, col element in X will be the log likelihood that the magnitude on the current trial maps to the column value, given the magnitude 
  #   and corresponding estimate in that row's trial
  X = matrix(0, nrow = N_TRIALS_CALIBRATION + K_BUMPERS, ncol = MAX_ESTIMATE) # Initialize matrix for storing log likelihoods
  # matrix storing proability in each col that magnitude on this trial is < magnitude in trial indicated by that row
  P = matrix(rep(1 - prev.trials$p.mag.greater[1:(N_TRIALS_CALIBRATION + K_BUMPERS)], MAX_ESTIMATE), nrow = N_TRIALS_CALIBRATION + K_BUMPERS)
  # matrix storing numerical estimate generated for each previous trial
  B = matrix(rep(floor(prev.trials$model.answer[1:(N_TRIALS_CALIBRATION + K_BUMPERS)]), MAX_ESTIMATE), nrow = N_TRIALS_CALIBRATION + K_BUMPERS)
  X[A <= B] = log10(P[A <= B]) # NB: if we switch this to < and the next line to >=, it makes a big diff, not totally clear why
  X[A > B] = log10(1 - P[A > B])
  # across each previous trial that we've calculated a log likelihood vector for above, calculate aggregate likelihood for estimates
  new.loglik = data.frame('number.est' = seq(MIN_ESTIMATE:MAX_ESTIMATE), 'sum.loglik' = colSums(X))
  
  return(new.loglik)
}


# calculate MAP for a given posterior distribution over possible number estimates
posterior.map = function(posterior) {
  map = posterior$number.est[posterior$posterior.norm == max(posterior$posterior.norm)]
  return(map)
}

# generate a sample from the posterior distribution over possible number estimates
posterior.sample = function(posterior) {
  post.sample = with(posterior, sample(number.est, 1, prob = posterior.norm))
  return(post.sample)
}

# calculate mean of the posterior distribution over possible number estimates
posterior.mean = function(posterior) {
  post.mean = sum(posterior$number.est * posterior$posterior.norm)
  return(post.mean)
}

# calculate median of the posterior distribution over possible number estimates
# NB: median of the posterior is used for similar problems in Griffiths, Tenenbaum 2006
posterior.median = function(posterior) {
  estimate.med = posterior %>% 
    mutate(post_sum = cumsum(posterior.norm)) %>%
    filter(post_sum >= 0.5) %>%
    summarize(est = min(number.est)) %>%
    pull(est)
  
  return(estimate.med)
}



####################
### MODEL PARAMS ###
####################

PERCEIVED_DOTS_NOISE_SD = 0.01 # log noise of perceived magnitudes
PRIOR_EXP = -1 # exponential slope parameter for prior
N_TRIALS_CALIBRATION = 25 # number of previous trials to reference in calculating answer estimate
BUMPER_SET = c(1, 2, 3, 5, 10) # set bumpers manually
K_BUMPERS = length(BUMPER_SET) # number of bumpers based on manual set
# K_BUMPERS = 4 # set bumpers based on K param
# BUMPER_SET = 10 ^ seq(from = log10(MIN_ESTIMATE), to = log10(MAX_ESTIMATE), length.out = K_BUMPERS)




####################
### PROCESS DATA ###
####################

# Select actual participant data for numbers presented, add necessary columns
data = read.data(DATA)

single.subj.data = data %>%
  # process existing columns
  mutate(run = to.num(run),
         index = to.num(index),
         num.dots = to.num(num_dots),
         answer1 = to.num(answer1),
         answer2 = to.num(answer2),
         participant.answer = 10 ^ (log10(pmax(1, answer1)) / 2 + log10(pmax(1, answer2)) / 2)) %>% # blended average of participant answers for this array
  select(subject, run, index, num.dots, participant.answer) %>% # Note including answer only relevant for eventual comparison
  # add necessary columns for model below
  mutate(trial = ((run - 1) * max(index)) + index, # get true trial number (1-300) for each participant
         model.answer = 0,
         bumper.trial = FALSE) %>%
  # get single participant's data to simulate experiment and compare model to participant
  filter(subject == SUBJ)



#################
### RUN MODEL ###
#################

for (trial.i in seq(1, max(single.subj.data$trial))) { # iterate over all trials
# for (trial.i in seq(1, N_TRIALS_CALIBRATION + 1)) { # DEBUGGING iterate to X trials past calibrated point
  if (trial.i <= N_TRIALS_CALIBRATION) {
    # set first N_TRIALS_CALIBRATION values to be a sample from a distribution around the true number of dots
    estimate = round(10 ^ rnorm(1, log10(single.subj.data$num.dots[trial.i]), PERCEIVED_DOTS_NOISE_SD), digits = 0)
    # estimate = single.subj.data$num.dots[trial.i] # DEBUGGING use exact number for initial calibration
  } else {
    # fetch previous trials for magnitude comparison
    prev.trials = sample.previous.trials(trial.i, single.subj.data)
    # add in "bumpers" as if they were previous trials, then all logic below can treat them the same
    prev.trials = sample.bumper.mappings(prev.trials)
    
    # compare magnitude of trial.i to previous trial magnitudes
    # -> assumes both have a mean around the true number of dots, sd = PERCEIVED_DOTS_NOISE_SD
    curr.trial.mag.mean = single.subj.data$num.dots[single.subj.data$trial == trial.i]
    prev.trials = prev.trials %>%
      mutate(p.mag.greater = 1 - pnorm(0, mean = log10(curr.trial.mag.mean) - log10(num.dots), 
                                       sd = sqrt(2 * PERCEIVED_DOTS_NOISE_SD)))
    
    # calculate initial log likelihood for this trial based on p.mag.greater from previous trials
    loglik = calculate.loglik(prev.trials)
    # process log likelihood obtained above, calculate posterior
    posterior = loglik %>%
      mutate(prior = number.est ^ PRIOR_EXP,
             prior.norm = prior / sum(prior),
             sum.loglik = sum.loglik - max(sum.loglik),  #add a constant C to each value in new.loglik to make it more reasonable
             loglik.probability.raw = 10 ^ sum.loglik, # reconvert out of log space
             posterior.raw = loglik.probability.raw * prior.norm, # multiply loglik by the prior
             posterior.norm = posterior.raw / sum(posterior.raw)) # normalize
             
    
    # Generate an estimate from the posterior 
    # -> (functions above for mean, median, probability matched sample, or MAP)
    estimate = posterior.map(posterior) # choose function here
  }
  
  # add estimate to results
  single.subj.data$model.answer[single.subj.data$trial == trial.i] = estimate
}



################
### ANALYSIS ###
################

# Quick view estimates and true numbers across trials
results = single.subj.data %>%
  select(trial, num.dots, model.answer) %>%
  arrange(trial)
results


# validating model estimates
single.subj.data %>%
  filter(trial > N_TRIALS_CALIBRATION) %>%
  ggplot(aes(x = log10(num.dots))) +
  geom_point(aes(y = log10(participant.answer), color = "subject"), alpha = 0.5) +
  geom_point(aes(y = log10(model.answer), color = "model"), alpha = 0.5) +
  geom_abline() +
  geom_vline(xintercept = log10(BUMPER_SET), linetype = "dashed", alpha = 0.5) +
  ggtitle(paste0("Model estimates, calibration = ", N_TRIALS_CALIBRATION, " trial(s)")) +
  labs(x = "Number presented (log10)", y = "Number estimated (log10)") +
  scale_color_manual(name = "Estimates", 
                     values = c("subject" = "blue", "model" = "red")) +
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom")
  

# Understanding over- and under-estimation (line segments show difference between num.dots and estimate)
single.subj.data %>%
  #filter(trial > N_TRIALS_CALIBRATION) %>%
  ggplot(aes(x = trial)) +
  geom_point(aes(y = log10(num.dots), color = "true number"), alpha = 0.5) +
  geom_point(aes(y = log10(model.answer), color = "model estimate"), alpha = 0.5) +
  geom_segment(aes(y = log10(num.dots), yend = log10(model.answer), xend = trial)) +
  geom_hline(yintercept = log10(BUMPER_SET[BUMPER_SET <= 10]), linetype = "dashed") +
  ggtitle("Model estimates v. true numbers across trials") +
  labs(y = "Number of dots (log10)", x = "trial number") +
  scale_color_manual(name = "Estimates", 
                     values = c("true number" = "blue", "model estimate" = "red")) +
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom")



# validating log likelihood and posterior for individual estimates
posterior %>% 
  #filter(number.est <= 10) %>%
  ggplot(aes(x = number.est, y = posterior.norm)) + # plot posterior
  #ggplot(aes(x = number.est, y = loglik.probability.raw)) + # plot log likelihood
  geom_point() +
  geom_vline(xintercept = BUMPER_SET, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = estimate, color = "red", linetype = "dashed") +
  #scale_x_continuous(breaks = c(1:10), labels = c(1:10)) +
  labs(x = "magnitude estimate")

# to compare the above to the true number of dots on a given trial, run the below
single.subj.data[7,] # the plot above should be converging on something similar to this (be sure to use correct index)










