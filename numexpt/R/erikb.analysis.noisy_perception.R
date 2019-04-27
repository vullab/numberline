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
#SUBJ = c(17) # sample representative participant(s)
SUBJ = seq(1:24) # sample representative participant(s)


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



# Sample previous trials based on exponential decay function with slope TRIALS_EXP
sample.previous.trials = function(trial.i, single.subject.data, n.trials){
  # set of number mappings from which to sample bumpers
  trial_set = data.frame("sub_index" = seq(1, trial.i - 1)) # number of trials *back* from current trial to sample
  trial_set$sub_index.scaled = trial_set$sub_index ^ TRIALS_EXP # TODO this is exponential but linearly decreasing in log sapce
  trial_set$ptrial_sub = trial_set$sub_index.scaled / sum(trial_set$sub_index.scaled)
  prev.trial.sub.indices = sort(with(trial_set, sample(sub_index, n.trials, prob = ptrial_sub, replace = F)))
  
  # get only trials m indices prior to trial.i for m in prev.trial.sub.indices
  prev.trials = single.subj.data %>%
    filter(trial %in% (trial.i - prev.trial.sub.indices))
  
  return(prev.trials)
}


# Sample "bumper" mappings by adding known magnitude -> number mappings to previous trials
# that are being used for likelihood calculation
sample.bumper.mappings = function(subj, prev.trials, k_bumpers, bumper.min, bumper.max) {
  if (k_bumpers > 0) {
    # set of number mappings from which to sample bumpers
    world_vals = data.frame("num" = seq(bumper.min, bumper.max))
    world_vals$num.scaled = world_vals$num ^ BUMPER_EXP # TODO this is exponential but linearly decreasing in log sapce
    world_vals$pnum = world_vals$num.scaled / sum(world_vals$num.scaled)
    bumper_set = with(world_vals, sample(num, k_bumpers, prob = pnum, replace = F))
    
    bumper_samples = c() # sampled vaue of bumpers
    for (bumper in bumper_set) {
      bumper_samples = c(bumper_samples, round(10 ^ rnorm(1, log10(bumper), PERCEIVED_DOTS_NOISE_SD), digits = 0))
    }
    
    prev.trials = prev.trials %>%
      add_row(subject = subj, # this gets passed in so we don't rely on previous trials if there are none
              num.dots = bumper_set,
              #model.answer = BUMPER_SET, # DEBUGGING make the bumpers exact
              model.answer = bumper_samples,
              bumper.trial = TRUE)
  }
  
  return(prev.trials)
}


# calculate log likelihood using magnitudes and number estimates from previous trials
calculate.loglik = function(prev.trials) {
  # matrix of number estimates initialized for use in calculation of likelihoods in loop below
  # NB: this matrix A doesn't need to be created every time we call this function but it doesn't slow it down much
  # so I'm keeping it here for compactness
  A = matrix(rep(1:MAX_ESTIMATE, each = N_SAMPLES), nrow = N_SAMPLES)
  
  # discretize possible answer estimates, set log likelihood at each value to be either p.mag.greater or 1 - p.mag.greater
  # use matrix with a row for each of the previous trials and a column for each candidate estimate value.
  # each row, col element in X will be the log likelihood that the magnitude on the current trial maps to the column value, given the magnitude 
  #   and corresponding estimate in that row's trial
  X = matrix(0, nrow = N_SAMPLES, ncol = MAX_ESTIMATE) # Initialize matrix for storing log likelihoods
  # matrix storing proability in each col that magnitude on this trial is < magnitude in trial indicated by that row
  P = matrix(rep(1 - prev.trials$p.mag.greater[1:(N_SAMPLES)], MAX_ESTIMATE), nrow = N_SAMPLES)
  # matrix storing numerical estimate generated for each previous trial
  B = matrix(rep(floor(prev.trials$model.answer[1:(N_SAMPLES)]), MAX_ESTIMATE), nrow = N_SAMPLES)
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
  post.sample = with(posterior, sample(number.est, 1, prob = (posterior.raw ^ POSTERIOR_SAMPLE_EXP)))
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

PERCEIVED_DOTS_NOISE_SD = 0.01 # log noise of perceived magnitudes

PRIOR_EXP = -1 # exponential slope parameter for prior

# params for sample from posterior
ESTIMATE_FXN = posterior.sample.pl
POSTERIOR_SAMPLE_EXP = 4 # exponent `l` to use when sampling from the posterior with probability p^l

# params for sample of previous trials
TRIALS_EXP = -4 # explonential slope parameter for sampling previous trials

# params for sample of real world "bumpers"
BUMPER_EXP = -4 # exponential slope parameter for sampling "bumper" values

# hyperparams: number of samples and probability that samples are derived from real world bumpers
N_SAMPLES = 25 # total number of samples to take from previous trials and "bumper" distribution
P_BUMPER = 0.1 # probability p that a given sample comes from previous trials or real world "bumper" distribution






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
    # TODO handle first m trials < N_SAMPLES
    if (trial.i <= N_SAMPLES) {
      # set first N_SAMPLES values to be a sample from a distribution around the true number of dots
      estimate = round(10 ^ rnorm(1, log10(single.subj.data$num.dots[trial.i]), PERCEIVED_DOTS_NOISE_SD), digits = 0)
      # estimate = single.subj.data$num.dots[trial.i] # DEBUGGING use exact number for initial calibration
    } else {
      # get set of previous trials and real world bumpers to sample
      sample.cats = rbinom(N_SAMPLES, 1, P_BUMPER)
      num.bumpers = sum(sample.cats) # TODO this fails if we end up with 0 bumpers, find out why
      num.trials = N_SAMPLES - num.bumpers

      # fetch previous trials for magnitude comparison
      prev.trials = sample.previous.trials(trial.i, single.subj.data, num.trials)
      # add in "bumpers" as if they were previous trials, then all logic below can treat them the same
      prev.trials = sample.bumper.mappings(subj, prev.trials, num.bumpers, MIN_ESTIMATE, MAX_ESTIMATE)
      
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

subject.data = SUBJ[1] # set this variable to view individual subject data below

# Quick view of true numbers and estimates across trials
results = data %>%
  filter(subject == subject.data) %>%
  select(subject, trial, num_dots, model.answer) %>%
  arrange(trial)
results


# validating model estimates

data %>% # single subject
  filter(subject == subject.data,
         trial > N_SAMPLES) %>%
  ggplot(aes(x = num_dots)) +
  geom_point(aes(y = answer, color = "subject"), alpha = 0.5) +
  geom_point(aes(y = model.answer, color = "model"), alpha = 0.5) +
  geom_abline() +
  #geom_vline(xintercept = BUMPER_SET, linetype = "dashed", alpha = 0.5) +
  ggtitle(paste("Model estimates, calibration = ", N_SAMPLES, " samples, p = ", P_BUMPER)) +
  labs(x = "Number presented", y = "Number estimated") +
  scale_color_manual(name = "Estimates", 
                     values = c("subject" = "blue", "model" = "red")) +
  mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
  mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE)) + 
  theme(axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "bottom")


data %>% # all subjects side by side
  filter(trial > N_SAMPLES) %>%
  ggplot(aes(x = num_dots)) +
  geom_point(aes(y = answer, color = "subject"), alpha = 0.25, size = 0.5) +
  geom_point(aes(y = model.answer, color = "model"), alpha = 0.25, size = 0.5) +
  geom_abline() +
  ggtitle(paste("Model estimates, calibration = ", N_SAMPLES, " samples, p = ", P_BUMPER)) +
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
  

# Understanding over- and under-estimation (line segments show difference between num.dots and estimate)
data %>% # single subject
  filter(subject == subject.data) %>%
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



# validating log likelihood and posterior for individual estimates
posterior %>% # single subject, single trial
  #filter(number.est <= 10) %>%
  ggplot(aes(x = number.est, y = posterior.norm)) + # plot posterior
  #ggplot(aes(x = number.est, y = loglik.probability.raw)) + # plot log likelihood
  geom_point() +
  #geom_vline(xintercept = BUMPER_SET, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = estimate, color = "red", linetype = "dashed") +
  labs(x = "magnitude estimate") +
  xlim(1, 10) # modify this as needed




# TODO
#' cleanup: remove globals from functions (only call functions with globals in main model?)



#' NOTES 
#' single subj
#' n=10,p=.2: noisy underestimate everywhere
#' n=10,p=.4: less noisy, less underestimate
#' n=10,p=.6: less noisy, more characteristic underestimation
#' n=10,p=.8: noisy again, maybe too characteristic underestimation
#' 
#' n=20,p=.1: under at low numbers, over at high: doesn't look like people or very good mapping
#' n=20,p=.2: close to identity line
#' n=20,p=.4: close to identity line, more noise at high number (over and under)
#' n=20,p=.6: close to identity line, more noise at high numbers (similar to p=.4)
#' n=20,p=.8: little closter to identity line at slightly higher numbers
#' 
#' Summary: when N is high (e.g. 20+), it's only interesting if P is low (otherwise we just have many bumpers)
#' 
#' all subj: the above doesn't seem to generalize super well to all subjects
#' n=15,p=.4: pretty good underestimation (lower Ns are pretty scattered, higher Ps are very noisy at high numbers)
#' 

