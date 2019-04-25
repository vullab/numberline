setwd("/Users/erikbrockbank/web/vullab/numberline/numexpt/R/")
rm(list=ls())

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
TRIALS = 300 # number of trials in the experiment (easier to set this as a global than compute it with e.g. max(trial))


#################
### FUNCTIONS ###
#################

# Data reading
read.data = function(filepath) {
  # read data
  files = list.files(filepath)
  data = data.frame()
  subject = 1
  for (f in files) {
    q = read.csv2(paste(filepath, f, sep = ""), sep = ",", header = T, colClasses = "character")
    q$subject = subject
    data = rbind(data, q)
    subject = subject + 1
  }
  
  # format existing columns
  to.num = function(x) {as.numeric(as.character(x))} # supporting util
  data$run = to.num(data$run)
  data$index = to.num(data$index)
  data$num_dots = to.num(data$num_dots)
  data$answer1 = to.num(data$answer1)
  data$answer2 = to.num(data$answer2)
  data$points1 = to.num(data$points1)
  data$points2 = to.num(data$points2)
  data$time = to.num(data$time)
  
  # add relevant new columns
  data$answer = 10 ^ (log10(pmax(1, data$answer1)) / 2 + log10(pmax(1, data$answer2)) / 2) # blended average of participant answers for this array
  data$trial = 0
  for (s in unique(data$subject)) {
    data$trial[data$subject == s] = 1:TRIALS
  }
  data$model.answer = 0
  data$bumper.trial = FALSE
  
  return(data)
}



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
  bumper_samples = c()
  for (bumper in BUMPER_SET) {
    bumper_samples = c(bumper_samples, round(10 ^ rnorm(1, log10(bumper), PERCEIVED_DOTS_NOISE_SD), digits = 0))
  }
  
  prev.trials = prev.trials %>%
    add_row(subject = unique(prev.trials$subject),
            num.dots = BUMPER_SET,
            #model.answer = BUMPER_SET, # DEBUGGING make the bumpers exact
            model.answer = bumper_samples,
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
  loglik = data.frame('number.est' = seq(MIN_ESTIMATE:MAX_ESTIMATE), 'sum.loglik' = colSums(X))
  
  # process log likelihood calculated above
  loglik = loglik %>%
    mutate(
      sum.loglik = sum.loglik - max(sum.loglik), #add a constant C to each value in loglike to make it more reasonable
      loglik.probability.raw = 10 ^ sum.loglik # reconvert out of log space
    )
  
  return(loglik)
}

# calculate posterior by taking in log likelihood data, assuming power law prior
# NB: this can be modified to make different assumptions about the prior
calculate.posterior = function(loglik) {
  posterior = loglik %>%
    mutate(prior = number.est ^ PRIOR_EXP, # assume power law probability of seeing a particular number
           prior.norm = prior / sum(prior), # normalize
           posterior.raw = loglik.probability.raw * prior.norm, # multiply loglik by the prior
           posterior.norm = posterior.raw / sum(posterior.raw)) # normalize
  
  return(posterior)
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

# this is a function for sampling according to p^l for some l
# posterior.sample is a special case of this function where l=1
# higher l will sample closer to MAP, lower l will be closer to mean
posterior.sample.pl = function(posterior) {
  post.sample = with(posterior, sample(number.est, 1, prob = (posterior.raw ^ SAMPLE_EXP)))
  return(post.sample)
}

# Graphing functions
my.log.breaks = function(lims){
  majors = seq(floor(log10(lims[1])), ceiling(log10(lims[2])), by = 1)
  minors = log10(unlist(lapply(majors[-1], function(x){seq(10^(x - 1), 9 * 10^(x - 1), by = 10^(x - 1))})))
  return(list(majors, minors))
}

mylogx = function(lims){
  breaks = my.log.breaks(lims)
  scale_x_log10(limits = lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks = breaks[[2]])
}

mylogy = function(lims){
  breaks <- my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10^breaks[[1]], 
                minor_breaks = breaks[[2]])
}



####################
### MODEL PARAMS ###
####################

# SUBJ = c(17) # sample representative participant(s)
SUBJ = seq(1:24) # sample representative participant(s)
PERCEIVED_DOTS_NOISE_SD = 0.01 # log noise of perceived magnitudes
PRIOR_EXP = -1 # exponential slope parameter for prior
N_TRIALS_CALIBRATION = 25 # number of previous trials to reference in calculating answer estimate
BUMPER_SET = c(1, 2, 3, 5, 10) # set bumpers manually
K_BUMPERS = length(BUMPER_SET) # number of bumpers based on manual set
#K_BUMPERS = 1 # set bumpers based on K param
#BUMPER_SET = 10 ^ seq(from = log10(MIN_ESTIMATE), to = log10(MAX_ESTIMATE), length.out = K_BUMPERS)
ESTIMATE_FXN = posterior.sample.pl
SAMPLE_EXP = 4 # exponent `l` to use when sampling from the posterior with probability p^l



#' Useful model params:
#' Sensible *underestimation*
#' -> ESTIMATE_FXN = posterior.sample.pl
#' -> N_TRIALS_CALIBRATION = 10
#' -> BUMPER_SET = c(1, 2, 3, 5, 10)
#' -> SAMPLE_EXP = 4
#' 
#' Sensible *mapping*, few samples
#' -> ESTIMATE_FXN = posterior.mean
#' -> N_TRIALS_CALIBRATION = 2
#' -> K_BUMPERS = 5 [equidistant]
#' TODO deal with consistent overestimation here
#' 
#' Sensible *drift*
#' The below produces a nice gradual loss of correlation similar to human data
#' The challenge is getting the correlations to a suitably high level to start
#' -> ESTIMATE_FXN = posterior.sample.pl
#' -> N_TRIALS_CALIBRATION = 25 (increasing this reduces the corr, decreasing makes it more spotty w/o increasing corr)
#' -> BUMPER_SET = c(1, 2, 3, 5, 10)
#' -> SAMPLE_EXP = 4
#' TODO fiddle with the bumpers: set them manually, etc. (since tweaking calibration trials doesn't help in either direction)
#' 




#################
### RUN MODEL ###
#################

data = read.data(DATA)

for (subj in SUBJ) {
  print(paste("Running with participant: ", subj))
  single.subj.data = data %>%
    # process existing columns
    mutate(participant.answer = answer,
           num.dots = num_dots) %>% 
    select(subject, trial, num.dots, participant.answer, model.answer, bumper.trial) %>%
    # get single participant's data to simulate experiment and compare model to participant
    filter(subject == subj)
  for (trial.i in seq(1, TRIALS)) { # iterate over all trials
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
      # calculate posterior using log likelihood calculated above
      posterior = calculate.posterior(loglik)
      
      # Generate an estimate from the posterior 
      # -> (functions above for mean, median, probability matched sample, or MAP)
      estimate = ESTIMATE_FXN(posterior) # all estimate functions above take in posterior
      # map = posterior.map(posterior)
      # print(paste("MAP Estimate: ", map, ", p^l sample: ", estimate))
    }
    
    # add estimate to single subject results
    single.subj.data$model.answer[single.subj.data$trial == trial.i] = estimate
  }
  # add estimates for this subject to original data
  data$model.answer[data$subject == subj] = single.subj.data$model.answer
  
}





################
### ANALYSIS ###
################

subject.data = SUBJ # set this variable to view individual subject data below

# Quick view of true numbers and estimates across trials
results = data %>%
  filter(subject == subject.data) %>%
  select(subject, trial, num_dots, model.answer) %>%
  arrange(trial)
results


# validating model estimates

data %>% # single subject
  filter(subject == subject.data,
         trial > N_TRIALS_CALIBRATION) %>%
  ggplot(aes(x = num_dots)) +
  geom_point(aes(y = answer, color = "subject"), alpha = 0.5) +
  geom_point(aes(y = model.answer, color = "model"), alpha = 0.5) +
  geom_abline() +
  geom_vline(xintercept = BUMPER_SET, linetype = "dashed", alpha = 0.5) +
  ggtitle(paste("Model estimates, calibration = ", N_TRIALS_CALIBRATION, " trial(s)")) +
  labs(x = "Number presented", y = "Number estimated") +
  scale_color_manual(name = "Estimates", 
                     values = c("subject" = "blue", "model" = "red")) +
  mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
  mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE)) + 
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom")


data %>% # all subjects side by side
  filter(trial > N_TRIALS_CALIBRATION) %>%
  ggplot(aes(x = num_dots)) +
  geom_point(aes(y = answer, color = "subject"), alpha = 0.25, size = 0.5) +
  geom_point(aes(y = model.answer, color = "model"), alpha = 0.25, size = 0.5) +
  geom_abline() +
  geom_vline(xintercept = BUMPER_SET, linetype = "dashed", alpha = 0.5) +
  ggtitle(paste("Model estimates, calibration = ", N_TRIALS_CALIBRATION, " trial(s)")) +
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



# Compare each third of model estimates to make sure it's not getting worse
data = data %>%
  mutate(quartile = floor(trial/75.25)+1)

data %>% # all subjects side by side
  filter(trial > N_TRIALS_CALIBRATION) %>%
  ggplot(aes(x = num_dots, y = model.answer, color = as.factor(quartile))) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_abline() +
  geom_vline(xintercept = BUMPER_SET, linetype = "dashed", alpha = 0.5) +
  ggtitle(paste("Model estimates, calibration = ", N_TRIALS_CALIBRATION, " trial(s)")) +
  labs(x = "Number presented", y = "Number estimated") +
  mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
  mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.position = "bottom") +
  facet_wrap(~subject, ncol = 6)
  

# Understanding over- and under-estimation (line segments show difference between num.dots and estimate)
data %>% # single subject
  filter(subject == subject.data) %>%
  ggplot(aes(x = trial)) +
  geom_point(aes(y = log10(num_dots), color = "true number"), alpha = 0.5) +
  geom_point(aes(y = log10(model.answer), color = "model estimate"), alpha = 0.5) +
  geom_segment(aes(y = log10(num_dots), yend = log10(model.answer), xend = trial)) +
  geom_hline(yintercept = log10(BUMPER_SET[BUMPER_SET <= 10]), linetype = "dashed") +
  ggtitle("Model estimates v. true numbers across trials") +
  labs(y = "Number of dots (log10)", x = "trial number") +
  scale_color_manual(name = "Estimates", 
                     values = c("true number" = "blue", "model estimate" = "red")) +
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom")



# validating log likelihood and posterior for individual estimates
posterior %>% # single subject, single trial
  #filter(number.est <= 10) %>%
  ggplot(aes(x = number.est, y = posterior.norm)) + # plot posterior
  #ggplot(aes(x = number.est, y = loglik.probability.raw)) + # plot log likelihood
  geom_point() +
  geom_vline(xintercept = BUMPER_SET, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = estimate, color = "red", linetype = "dashed") +
  labs(x = "magnitude estimate") +
  xlim(1, 10) # modify this as needed







#' Analysis TODO 
#' 
#' claim 1. if you get to sample (whether from the identity line or from your previous trials as long as they’re reasonably accurate), you can do the mapping —> with fewer samples than you would expect
#' 
#' claim 2
#' there’s a set of such models (or model conditions) that produce people’s underestimations
#' - we need a procedure that produces human curves in euquilibrium (check the last N to make sure it still looks okay, try doing more than 300): human like mapping should come out not from the average but from steady state
#' - “some combination of these samples and prior produces characteristic underestimation as samples drop and for the right L”
#' 
#' claim 3
#' there’s a very simple class of such models (add in noisy distribution around the identity line during posterior calculation, use not too many samples) that drifts








