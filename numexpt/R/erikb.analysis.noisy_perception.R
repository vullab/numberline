rm(list=ls())
setwd("/Users/erikbrockbank/web/vullab/numberline/numexpt/R/")
library(stats4)
library(tidyverse)
library(Matrix)

### README ### 
#' This model attempts to simulate a process of estimating quantity whereby the participant
#' has a noisy estimate of the magnitude of the number of dots shown and chooses an exact numerical estimate
#' based on the probability that this magnitude is greater or less than previously seen dots.
#' This model should:
#' 1. display the drift patterns characteristic of experiment participants, and 
#' 2. generate guesses that are broadly similar to the guesses participants make
#' 



#############################
### READ AND PROCESS DATA ###
#############################
# Read in and process experimental data to use for stimuli presentation and comparison to participant estimates.

# Copied from fit.models.2014-06-02.R
data.path = "../data/"
files <- list.files(data.path)
data <- data.frame()
subject <- 1
for(f in files){
  q = read.csv2(paste(data.path, f, sep = ""), sep = ",", header = T, colClasses = "character")
  q$subject = subject
  data <- rbind(data, q)
  subject <- subject + 1
}

to.num <- function(x){as.numeric(as.character(x))}

data$run <- to.num(data$run)
data$index <- to.num(data$index)
data$num_dots <- to.num(data$num_dots)
data$answer1 <- to.num(data$answer1) # participant's first guess answer
data$answer2 <- to.num(data$answer2) # participant's second guess answer
data$points1 <- to.num(data$points1) # participant's points earned for their first guess
data$points2 <- to.num(data$points2) # participant's points earned for their second guess
data$time <- to.num(data$time) # participant's response time
data$answer <- 10 ^ (log10(pmax(1, data$answer1)) / 2 + log10(pmax(1, data$answer2)) / 2) # blended average of participant answers for this array


# Select actual participant data for numbers presented, add necessary columns
data.model = data %>%
  select(subject, run, index, num_dots, answer) %>% # Note including answer only relevant for eventual comparison
  rename(participant.answer = answer) %>% # rename to avoid any confusion
  mutate(trial = ((run - 1) * max(index)) + index, # get true trial number (1-300) for each participant
         answer_estimate = 0) # initially set this to 0
# glimpse(data.model)

# Get single participant's data for easier initial modeling
single.subj = data.model %>%
  filter(subject == 17)
# glimpse(single.subj)

# Globals
PERCEIVED_DOTS_NOISE_SD = 0.01 # log noise: this may require some fiddling
ESTIMATE_CALIBRATION = 100 # number of previous trials to reference in calculating answer estimate
MAX_ESTIMATE = 1000 # highest number a participant will estimate
PRIOR_EXP = -1 # tweak this as needed
# matrix of number estimates initialized for use in calculation of likelihoods in loop below
A = matrix(rep(1:1000, each = ESTIMATE_CALIBRATION), nrow = ESTIMATE_CALIBRATION)

#for (trial.i in seq(1, ESTIMATE_CALIBRATION + 10)) { # iterate to one past calibrated point
for (trial.i in seq(1, max(single.subj$trial))) { # over all trials
  if (trial.i <= ESTIMATE_CALIBRATION) {
    # set first ESTIMATE_CALIBRATION values to be a sample from a distribution around the true number of dots
    # NB: samples in log space with low SD so that samples will be closer to real numbers at low n, farther at high n
    estimate = 10 ^ rnorm(1, log10(single.subj$num_dots[trial.i]), PERCEIVED_DOTS_NOISE_SD)
    # estimate = single.subj$num_dots[trial.i] # use exact number for debugging
  } else {
    # fetch previous trials for magnitude comparison
    comparison.trials = seq(trial.i - ESTIMATE_CALIBRATION, trial.i - 1)
    prev.vals = single.subj %>%
      filter(trial %in% comparison.trials)
    
    # compare magnitude of trial.i to previous trials
    curr.trial.mag.mean = single.subj$num_dots[single.subj$trial == trial.i]
    prev.vals = prev.vals %>%
      mutate(p.mag.greater = 1 - pnorm(0, mean = log10(curr.trial.mag.mean) - log10(num_dots), sd = sqrt(2 * PERCEIVED_DOTS_NOISE_SD)))

    # discretize possible answer estimates, set log likelihood at each value to be either p.mag.greater or 1 - p.mag.greater
    # use matrix with a row for each of the previous trials and a column for each candidate estimate value.
    # each row, col element in X is the log likelihood that the magnitude on the current trial maps to the column value, given the magnitude 
    #   and corresponding estimate in that row's trial
    X = matrix(0, nrow = ESTIMATE_CALIBRATION, ncol = MAX_ESTIMATE) # Initialize matrix for storing log likelihoods
    # matrix storing proability in each col that magnitude on this trial is < magnitude in trial indicated by that row
    P = matrix(rep(1 - prev.vals$p.mag.greater[1:ESTIMATE_CALIBRATION], MAX_ESTIMATE), nrow = ESTIMATE_CALIBRATION)
    # matrix storing numerical estimate generated for each previous trial
    B = matrix(rep(floor(prev.vals$answer_estimate[1:ESTIMATE_CALIBRATION]), 1000), nrow = ESTIMATE_CALIBRATION)
    X[A < B] = log10(P[A < B])
    X[A >= B] = log10(1 - P[A >= B])
    # across each previous trial that we've calculated a log likelihood vector for above, calculate aggregate likelihood for estimates
    new.loglik = data.frame('number.est' = seq(1:MAX_ESTIMATE), 'sum.loglik' = colSums(X))

    # process log likelihoods obtained above (add constant factor, re-convert to probability space, and normalize)
    new.loglik = new.loglik %>%
      mutate(prior = number.est ^ PRIOR_EXP,
             prior.norm = prior / sum(prior),
             sum.loglik = sum.loglik - max(sum.loglik),  #add a constant C to each value in new.loglik to make it more reasonable
             loglik.probability.raw = 10 ^ sum.loglik, # reconvert out of log space
             posterior.raw = loglik.probability.raw * prior.norm, # multiply loglik by the prior
             posterior.norm = posterior.raw / sum(posterior.raw)) # normalize
             
    
    # Three different ways of generating a number estimate are each included below (sample, posterior mean, posterior median)
    # 1. sample estimate from the posterior
    # estimate = with(new.loglik, sample(number.est, 1, prob = posterior.norm))
    # 2. use the posterior mean as our estimate
    # estimate = sum(new.loglik$number.est * new.loglik$posterior.norm)
    # 3. use the median as our estimate (following Griffiths, Tenenbaum 2006)
    estimate = new.loglik %>% 
      mutate(post_sum = cumsum(posterior.norm)) %>% 
      filter(post_sum >= 0.5) %>% 
      summarize(est = min(number.est)) %>% 
      pull(est)
  }
  
  # add estimate to single.subj results
  single.subj$answer_estimate[single.subj$trial == trial.i] = estimate

}

# Quick view estimates and true numbers across trials
results = single.subj %>%
  select(trial, num_dots, answer_estimate) %>%
  arrange(trial)
results


# validating discretized log likelihood answer.range matrix
dim(answer.range) # should be 100 trials by 1000 possible number estimates
answer.range[1,2] # log likelihood that current trial estimate is equal to 2, given magnitude of trial 1
prev.vals$answer_estimate[2] # cutoff for log likelihood vals, trial 2
floor(prev.vals$answer_estimate[2])
answer.range[2, floor(prev.vals$answer_estimate[2])] # this number and the next should be very different
answer.range[2, floor(prev.vals$answer_estimate[2]) + 1]

# validating new.loglik (use for seq above up to ESTIMATE_CALIBRATION + 10)
new.loglik %>% 
  filter(number.est %in% seq(1:1000)) %>% 
  ggplot(aes(x = number.est, y = posterior.norm)) + 
  geom_jitter() +
  #ggtitle("Log likelihood for a trial with true number 107") +
  labs(x = "magnitude estimate", y = "log likelihood")
single.subj[11,] # this plot above should be converging on something similar to this

# validating model answers
single.subj %>%
  filter(trial > ESTIMATE_CALIBRATION) %>%
  ggplot(aes(x = log10(num_dots))) +
  geom_jitter(aes(y = log10(participant.answer)), color = 'blue', alpha = 0.5) +
  geom_jitter(aes(y = log10(answer_estimate)),  color = 'red', alpha = 0.5) +
  geom_abline() +
  ggtitle(paste0("Model estimates, calibration = ", ESTIMATE_CALIBRATION)) +
  labs(x = "Number presented (log10)", y = "Number estimated (log10)") +
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"))
  
# Understanding over- and under-estimation (line segments show difference between num_dots and estimate)
single.subj %>%
  #filter(trial > ESTIMATE_CALIBRATION) %>%
  ggplot(aes(x = trial)) +
  geom_point(aes(y = log10(num_dots)), color = 'blue', alpha = 0.5) +
  geom_point(aes(y = log10(answer_estimate)), color = 'red', alpha = 0.5) +
  geom_segment(aes(y = log10(num_dots), yend = log10(answer_estimate), xend = trial)) +
  ggtitle("Disparity between model estimates (red) and true numbers (blue) by trial") +
  labs(y = "Number of dots (log10)", x = "trial number") +
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"))




