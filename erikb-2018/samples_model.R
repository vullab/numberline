setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
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
#' The model uses data from `numberline/numexpt`


###############
### GLOBALS ###
###############

# Globals for experiment setup and data
DATA = "../numexpt/data/"
MIN_ESTIMATE = 1 # lowest number in the range of dots
MAX_ESTIMATE = 1000 # highest number in the range of dots
TRIALS = 300 # number of trials in the experiment (easier to set this as a global than compute it with e.g. max(trial))


#################
### FUNCTIONS ###
#################

# Data reading
read.data = function(filepath, n.trials) {
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
    data$trial[data$subject == s] = 1:n.trials
  }
  data$model.answer = 0
  data$bumper.trial = FALSE
  
  return(data)
}


get.samples = function(trial.i, subj, single.subj.data, n.trials, n.samples, p.bumper, min.est, max.est, 
                       perceived.mag.sd, trials.exp, bumper.exp, mapping.vals = NULL) {
  # get set of previous trials and real world bumpers to sample
  sample.cats = rbinom(n.samples, 1, p.bumper)
  num.bumpers = sum(sample.cats)
  num.trials = n.samples - num.bumpers
  #num.trials = 0 # DEBUGGING keep things easy
  # fetch previous trials for magnitude comparison
  prev.trials = sample.previous.trials(trial.i, single.subj.data, num.trials, trials.exp)
  # add in "bumpers" as if they were previous trials (note we're providing a fixed set from which to sample here)
  prev.trials = sample.bumper.mappings(subj, prev.trials, num.bumpers, min.est, max.est, bumper.exp, perceived.mag.sd, mapping.vals)
  
  return(prev.trials)
}


# Sample previous trials based on exponential decay function with slope trials.exp
sample.previous.trials = function(trial.i, single.subject.data, n.trials, trials.exp) {
  if (n.trials > 0) {
    # set of number mappings from which to sample bumpers
    trial_set = data.frame("sub_index" = seq(1, trial.i - 1)) # number of trials *back* from current trial to sample
    trial_set$sub_index.scaled = trial_set$sub_index ^ trials.exp
    trial_set$ptrial_sub = trial_set$sub_index.scaled / sum(trial_set$sub_index.scaled)
    prev.trial.sub.indices = sort(with(trial_set, sample(sub_index, n.trials, prob = ptrial_sub, replace = F)))
    
    # get only trials m indices prior to trial.i for m in prev.trial.sub.indices
    prev.trials = single.subject.data %>%
      filter(trial %in% (trial.i - prev.trial.sub.indices))
  } else {
    prev.trials = single.subject.data[0,] # return formatting of single subject data but with no previous trials
  }
  
  return(prev.trials)
}

get.bumper.set = function(bumper.min, bumper.max, bumper.exp, samples, with.replace) {
  world_vals = data.frame("num" = seq(bumper.min, bumper.max))
  world_vals$num.scaled = world_vals$num ^ bumper.exp
  world_vals$pnum = world_vals$num.scaled / sum(world_vals$num.scaled)
  
  # bumper_set = with(world_vals, sample(num, samples, prob = pnum, replace = with.replace))
  bumper_set = sample_n(tbl = world_vals, size = samples, replace = with.replace, weight = world_vals$pnum)
  
  return(bumper_set)
}

# Sample "bumper" mappings by adding known magnitude -> number mappings to previous trials
# that are being used for likelihood calculation
sample.bumper.mappings = function(subj, prev.trials, k_bumpers, bumper.min, bumper.max, bumper.exp, mag.sd, bumper.set.fixed = NULL) {
  if (k_bumpers > 0) {
    bumper_samples = data.frame(num_dots = numeric(), estimate = numeric()) # sampled value of bumpers for mapping
    if (is.null(bumper.set.fixed)) {
      # fetch set of sampled bumper number mappings: get sample of true numbers, create noisy mapping
      bumper_set = get.bumper.set(bumper.min, bumper.max, bumper.exp, k_bumpers, with.replace = F)
      for (bumper in bumper_set) {
        bumper_samples = rbind(bumper_samples, data.frame(num_dots = bumper,
                                                          estimate = round(10 ^ rnorm(1, log10(bumper), mag.sd), digits = 0)))
      }
      # print("Bumpers:")
      # print(bumper_samples)
    } else {
      # use already supplied set of number mappings: maximize uniqueness
      if (length(unique(bumper.set.fixed$num_dots)) <= k_bumpers) {
        bumper_samples = bumper.set.fixed
      } else {
        # int.sample = sample(unique(bumper.set.fixed$num_dots), k_bumpers, replace = F)
        # for (bumper in int.sample) {
        #   bumper_subset = bumper.set.fixed[bumper.set.fixed$num_dots == bumper,]
        #   bumper_samples = rbind(bumper_samples, data.frame(num_dots = bumper,
        #                                                     estimate = sample(bumper_subset$estimate, 1, replace = F)))
        # }
        bumper_samples = sample_n(bumper.set.fixed, k_bumpers, replace = F, weight = bumper.set.fixed$prob)
      }
      # bumper_samples = sample_n(bumper.set.fixed, k_bumpers, replace = F) # assume equal probability for all known mappings
    }

    prev.trials = prev.trials %>%
      add_row(subject = subj, # this gets passed in so we don't rely on previous trials if there are none
              num.dots = bumper_samples$num_dots,
              #model.answer = BUMPER_SET, # DEBUGGING make the bumpers exact
              model.answer = bumper_samples$estimate,
              bumper.trial = TRUE)
  }
  
  return(prev.trials)
}


# calculate log likelihood using magnitudes and number estimates from previous trials
calculate.loglik = function(prev.trials, n.samples, min.est, max.est) {
  # matrix of number estimates initialized for use in calculation of likelihoods in loop below
  # NB: this matrix A doesn't need to be created every time we call this function but it doesn't slow it down much
  # so I'm keeping it here for compactness
  A = matrix(rep(min.est:max.est, each = n.samples), nrow = n.samples)
  
  # discretize possible answer estimates, set log likelihood at each value to be either p.mag.greater or 1 - p.mag.greater
  # use matrix with a row for each of the previous trials and a column for each candidate estimate value.
  # each row, col element in X will be the log likelihood that the magnitude on the current trial maps to the column value, given the magnitude 
  #   and corresponding estimate in that row's trial
  X = matrix(0, nrow = n.samples, ncol = max.est) # Initialize matrix for storing log likelihoods
  # matrix storing proability in each col that magnitude on this trial is < magnitude in trial indicated by that row
  P = matrix(rep(1 - prev.trials$p.mag.greater[1:(n.samples)], max.est), nrow = n.samples)
  # matrix storing numerical estimate generated for each previous trial
  B = matrix(rep(floor(prev.trials$model.answer[1:(n.samples)]), max.est), nrow = n.samples)
  X[A <= B] = log10(P[A <= B]) # NB: if we switch this to < and the next line to >=, it makes a big diff, not totally clear why
  X[A > B] = log10(1 - P[A > B])
  # across each previous trial that we've calculated a log likelihood vector for above, calculate aggregate likelihood for estimates
  loglik = data.frame('number.est' = seq(min.est:max.est), 'sum.loglik' = colSums(X))
  
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
calculate.posterior = function(loglik, prior.exp) {
  posterior = loglik %>%
    mutate(prior = number.est ^ prior.exp, # assume power law probability of seeing a particular number
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
posterior.sample.pl = function(posterior, sample.exp) {
  post.sample = with(posterior, sample(number.est, 1, prob = (posterior.raw ^ sample.exp)))
  return(post.sample)
}

calculate.mse = function(data) {
  data = data %>%
    mutate(subj.sq.error = (answer - num_dots) ^ 2,
           model.sq.error = (model.answer - num_dots) ^ 2)
  
  return(data)
}

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
  # backgrounds, lines
  panel.background = element_blank(),
  #strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

# plot theme from perceptual_analysis.R
individ_plot_theme_v2 = theme(
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


run.model.baseline = function(data.path,
                              n.trials,
                              n.samples, 
                              p.bumper, 
                              min.est, 
                              max.est,
                              perceived.mag.sd,
                              est.fxn,
                              trials.exp,
                              bumper.exp,
                              prior.exp,
                              post.exp) {
  
  data = read.data(data.path, n.trials)
  subjects = unique(data$subject)
  # subjects = c(6) # DEBUGGING

  for (subj in subjects) {
    print(paste("Running with participant: ", subj))
    print(paste("Bumper probability: ", p.bumper))
    print(paste("Samples: ", n.samples))
    
    # reformat data to be consistent with util functions above
    single.subj.data = data %>%
      mutate(participant.answer = answer,
             num.dots = num_dots) %>% 
      select(subject, trial, num.dots, participant.answer, model.answer, bumper.trial) %>%
      filter(subject == subj)
    
    for (trial.i in seq(1, max(data$trial[data$subject == subj]))) {
      if (trial.i <= n.samples) {
        # set first N_SAMPLES values to be a sample from a distribution around the true number of dots
        estimate = round(10 ^ rnorm(1, log10(single.subj.data$num.dots[trial.i]), perceived.mag.sd), digits = 0)
        # estimate = single.subj.data$num.dots[trial.i] # DEBUGGING use exact number for initial calibration
      } else {
        # get set of previous trials and real world bumpers to sample
        sample.cats = rbinom(n.samples, 1, p.bumper)
        num.bumpers = sum(sample.cats) # TODO this fails if we end up with 0 bumpers, find out why
        num.trials = n.samples - num.bumpers
        # fetch previous trials for magnitude comparison
        prev.trials = sample.previous.trials(trial.i, single.subj.data, num.trials, trials.exp)
        # add in "bumpers" as if they were previous trials, then all logic below can treat them the same
        prev.trials = sample.bumper.mappings(subj, prev.trials, num.bumpers, min.est, max.est, bumper.exp, perceived.mag.sd)
        
        # compare magnitude of trial.i to previous trial magnitudes
        # -> assumes both have a mean around the true number of dots, sd = PERCEIVED_DOTS_NOISE_SD
        curr.trial.mag.mean = single.subj.data$num.dots[single.subj.data$trial == trial.i]
        prev.trials = prev.trials %>%
          mutate(p.mag.greater = 1 - pnorm(0, mean = log10(curr.trial.mag.mean) - log10(num.dots), 
                                           sd = sqrt(2 * perceived.mag.sd)))
        
        # calculate initial log likelihood for this trial based on p.mag.greater from previous trials
        loglik = calculate.loglik(prev.trials, n.samples, min.est, max.est)
        # calculate posterior using log likelihood calculated above
        posterior = calculate.posterior(loglik, prior.exp)

        # Generate an estimate from the posterior 
        # -> (functions above for mean, median, probability matched sample, or MAP)
        estimate = est.fxn(posterior, post.exp) # all estimate functions above take in posterior
      }
      
      # add estimate to single subject results
      single.subj.data$model.answer[single.subj.data$trial == trial.i] = estimate
    }
    # add estimates for this subject to original data
    data$model.answer[data$subject == subj] = single.subj.data$model.answer
    
  }
  
  return(data)
}



run.model.individ.samples = function(data.path,
                                     n.trials,
                                     min.est, 
                                     max.est,
                                     perceived.mag.sd,
                                     est.fxn,
                                     trials.exp,
                                     bumper.exp,
                                     prior.exp,
                                     post.exp) {
  data = read.data(data.path, n.trials)
  subjects = unique(data$subject)
  
  for (subj in subjects) {
    # set unique sample, bumper probability values for this participant
    n.samples = round(rnorm(1, mean = 20, sd = 5)) # normal sample for number of trials sampled
    if (n.samples < 1) {n.samples = 1}
    p.bumper = runif(1) # uniform sample of bumper probability
    
    # start model
    print(paste("Running with participant: ", subj))
    print(paste("Bumper probability: ", p.bumper))
    print(paste("Samples: ", n.samples))
    
    # reformat data to be consistent with util functions above 
    single.subj.data = data %>%
      mutate(participant.answer = answer,
             num.dots = num_dots) %>% 
      select(subject, trial, num.dots, participant.answer, model.answer, bumper.trial) %>%
      filter(subject == subj)
    for (trial.i in seq(1, max(data$trial[data$subject == subj]))) {
      if (trial.i <= n.samples) {
        # set first N_SAMPLES values to be a sample from a distribution around the true number of dots
        estimate = round(10 ^ rnorm(1, log10(single.subj.data$num.dots[trial.i]), perceived.mag.sd), digits = 0)
        # estimate = single.subj.data$num.dots[trial.i] # DEBUGGING use exact number for initial calibration
      } else {
        # get set of previous trials and real world bumpers to sample
        sample.cats = rbinom(n.samples, 1, p.bumper)
        num.bumpers = sum(sample.cats) # TODO this fails if we end up with 0 bumpers, find out why
        num.trials = n.samples - num.bumpers
        # fetch previous trials for magnitude comparison
        prev.trials = sample.previous.trials(trial.i, single.subj.data, num.trials, trials.exp)
        # add in "bumpers" as if they were previous trials, then all logic below can treat them the same
        prev.trials = sample.bumper.mappings(subj, prev.trials, num.bumpers, min.est, max.est, bumper.exp, perceived.mag.sd)
        
        # compare magnitude of trial.i to previous trial magnitudes
        # -> assumes both have a mean around the true number of dots, sd = PERCEIVED_DOTS_NOISE_SD
        curr.trial.mag.mean = single.subj.data$num.dots[single.subj.data$trial == trial.i]
        prev.trials = prev.trials %>%
          mutate(p.mag.greater = 1 - pnorm(0, mean = log10(curr.trial.mag.mean) - log10(num.dots), 
                                           sd = sqrt(2 * perceived.mag.sd)))
        
        # calculate initial log likelihood for this trial based on p.mag.greater from previous trials
        loglik = calculate.loglik(prev.trials, n.samples, min.est, max.est)
        # calculate posterior using log likelihood calculated above
        posterior = calculate.posterior(loglik, prior.exp)
        
        # Generate an estimate from the posterior 
        # -> (functions above for mean, median, probability matched sample, or MAP)
        estimate = est.fxn(posterior, post.exp) # all estimate functions above take in posterior
      }
      
      # add estimate to single subject results
      single.subj.data$model.answer[single.subj.data$trial == trial.i] = estimate
    }
    # add estimates for this subject to original data
    data$model.answer[data$subject == subj] = single.subj.data$model.answer
    
  }
  
  return(data)
}


# DEBUGGING
data = read.data(DATA, TRIALS)
subj = 6
mems = 100
current.trial = 50
single.subj.data = data %>%
  mutate(participant.answer = answer,
         num.dots = num_dots) %>% 
  select(subject, trial, num.dots, participant.answer, model.answer, bumper.trial) %>%
  filter(subject == subj)
mapping.set = get.bumper.set(MIN_ESTIMATE, MAX_ESTIMATE, BUMPER_EXP, mems, with.replace = F)
sort(unique(mapping.set$num)) # unique values in initial memory bank
mapping.vals = data.frame(num_dots = numeric(), estimate = numeric(), prob = numeric())
for (bumper in mapping.set$num) {
  mapping.vals = rbind(mapping.vals, data.frame(num_dots = bumper,
                                                estimate = round(10 ^ rnorm(1, log10(bumper), PERCEIVED_DOTS_NOISE_SD), digits = 0),
                                                prob = mapping.set$pnum[mapping.set$num == bumper]))
}
sum(mapping.vals$prob) # TODO should we re-normalize?
mapping.vals$prob = mapping.vals$prob / sum(mapping.vals$prob)
prev.estimates = get.samples(current.trial, single.subj.data, TRIALS, N_SAMPLES, P_BUMPER, 
                             MIN_ESTIMATE, MAX_ESTIMATE, PERCEIVED_DOTS_NOISE_SD, TRIALS_EXP, BUMPER_EXP, mapping.vals)
sort(unique(prev.estimates$num.dots)) # unique values sampled for a given trial






run.model.individ.memories = function(data.path,
                                      n.trials,
                                      n.samples, 
                                      p.bumper, 
                                      min.est, 
                                      max.est,
                                      perceived.mag.sd,
                                      est.fxn,
                                      trials.exp,
                                      bumper.exp,
                                      prior.exp,
                                      post.exp,
                                      n.memories) {
  
  data = read.data(data.path, n.trials)
  subjects = unique(data$subject)
  #subjects = c(6, 11, 22) # DEBUGGING
  #mapping.dict = list() # DEBUGGING keep track of participants' mapping sets

  for (subj in subjects) {
    print(paste("Running with participant: ", subj))
    print(paste("Bumper probability: ", p.bumper))
    print(paste("Samples: ", n.samples))
    print(paste("Memories: ", n.memories))
    
    # reformat data to be consistent with util functions above
    single.subj.data = data %>%
      mutate(participant.answer = answer,
             num.dots = num_dots) %>% 
      select(subject, trial, num.dots, participant.answer, model.answer, bumper.trial) %>%
      filter(subject == subj)
    
    # before running trials, determine participant's mapping "memories" to sample from
    # mapping.set = sort(get.bumper.set(min.est, max.est, bumper.exp, n.memories, with.replace = T))
    # mapping.vals = data.frame(num_dots = numeric(), estimate = numeric())
    # for (bumper in mapping.set) {
    #   mapping.vals = rbind(mapping.vals, data.frame(num_dots = bumper,
    #                                                 estimate = round(10 ^ rnorm(1, log10(bumper), perceived.mag.sd), digits = 0)))
    # }
    mapping.set = get.bumper.set(MIN_ESTIMATE, MAX_ESTIMATE, BUMPER_EXP, n.memories, with.replace = F)
    mapping.vals = data.frame(num_dots = numeric(), estimate = numeric(), prob = numeric())
    for (bumper in mapping.set$num) {
      mapping.vals = rbind(mapping.vals, data.frame(num_dots = bumper,
                                                    estimate = round(10 ^ rnorm(1, log10(bumper), PERCEIVED_DOTS_NOISE_SD), digits = 0),
                                                    prob = mapping.set$pnum[mapping.set$num == bumper]))
    }
    mapping.vals$prob = mapping.vals$prob / sum(mapping.vals$prob) # re-normalize probabilities
    print("Bumpers in memory:")
    print(sort(unique(mapping.vals$num_dots)))
    
    # DEBUGGING see how similar participants' mappings end up being
    # for (elem in mapping.set) {
    #   if (!is.null(mapping.dict[[as.character(elem)]])) {
    #     mapping.dict[[as.character(elem)]] = mapping.dict[[as.character(elem)]] + 1
    #   } else {
    #     mapping.dict[[as.character(elem)]] = 1
    #   }
    # }

    for (trial.i in seq(1, max(data$trial[data$subject == subj]))) {
      if (trial.i <= n.samples) {
        # set first N_SAMPLES values to be a sample from a distribution around the true number of dots
        estimate = round(10 ^ rnorm(1, log10(single.subj.data$num.dots[trial.i]), perceived.mag.sd), digits = 0)
        # estimate = single.subj.data$num.dots[trial.i] # DEBUGGING use exact number for initial calibration
        # num.bumpers = n.samples
      } else {
        # get set of previous trials and real world bumpers to sample
      #   sample.cats = rbinom(n.samples, 1, p.bumper)
      #   num.bumpers = sum(sample.cats) # TODO this fails if we end up with 0 bumpers, find out why
      # # }
      # 
      #   num.trials = n.samples - num.bumpers
      #   # fetch previous trials for magnitude comparison
      #   prev.trials = sample.previous.trials(trial.i, single.subj.data, num.trials, trials.exp)
      #   # add in "bumpers" as if they were previous trials (note we're providing a fixed set from which to sample here)
      #   prev.trials = sample.bumper.mappings(subj, prev.trials, num.bumpers, min.est, max.est, bumper.exp, perceived.mag.sd, mapping.vals)
      #   
        prev.trials = get.samples(trial.i, subj, single.subj.data, n.trials, n.samples, p.bumper, 
                                  min.est, max.est, perceived.mag.sd, trials.exp, bumper.exp, mapping.vals)
        # print("Trial:")
        # print(trial.i)
        # print("Bumpers:")
        # print(sort(unique(prev.trials$num.dots[prev.trials$bumper.trial == TRUE])))
        # compare magnitude of trial.i to previous trial magnitudes
        # -> assumes both have a mean around the true number of dots, sd = PERCEIVED_DOTS_NOISE_SD
        curr.trial.mag.mean = single.subj.data$num.dots[single.subj.data$trial == trial.i]
        prev.trials = prev.trials %>%
          mutate(p.mag.greater = 1 - pnorm(0, mean = log10(curr.trial.mag.mean) - log10(num.dots), 
                                           sd = sqrt(2 * perceived.mag.sd)))
        
        # calculate initial log likelihood for this trial based on p.mag.greater from previous trials
        loglik = calculate.loglik(prev.trials, n.samples, min.est, max.est)
        # calculate posterior using log likelihood calculated above
        posterior = calculate.posterior(loglik, prior.exp)
        
        # Generate an estimate from the posterior 
        # -> (functions above for mean, median, probability matched sample, or MAP)
        estimate = est.fxn(posterior, post.exp) # all estimate functions above take in posterior
      }
      
      # add estimate to single subject results
      single.subj.data$model.answer[single.subj.data$trial == trial.i] = estimate
    }
    # add estimates for this subject to original data
    data$model.answer[data$subject == subj] = single.subj.data$model.answer
    
  }
  
  #print("Individual mappings:")
  #print(mapping.dict)
  
  return(data)
}



####################
### MODEL PARAMS ###
####################

PERCEIVED_DOTS_NOISE_SD = 0.1 # log noise of perceived magnitudes

PRIOR_EXP = -1 # exponential slope parameter for prior

# params for sample from posterior
ESTIMATE_FXN = posterior.sample.pl
POSTERIOR_SAMPLE_EXP = 4 # exponent `l` to use when sampling from the posterior with probability p^l

# params for sample of previous trials
TRIALS_EXP = -4 # explonential slope parameter for sampling previous trials

# params for sample of real world "bumpers"
BUMPER_EXP = -4 # exponential slope parameter for sampling "bumper" values

# hyperparams: number of samples and probability that samples are derived from real world bumpers
N_SAMPLES = 20 # total number of samples to take from previous trials and "bumper" distribution
P_BUMPER = 0.6 # probability p that a given sample comes from previous trials or real world "bumper" distribution


#################
### RUN MODEL ###
#################

# data = run.model.baseline(data.path = DATA,
#                           n.trials = TRIALS,
#                           n.samples = N_SAMPLES,
#                           p.bumper = P_BUMPER,
#                           min.est = MIN_ESTIMATE,
#                           max.est = MAX_ESTIMATE,
#                           perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
#                           est.fxn = ESTIMATE_FXN,
#                           trials.exp = TRIALS_EXP,
#                           bumper.exp = BUMPER_EXP,
#                           prior.exp = PRIOR_EXP,
#                           post.exp = POSTERIOR_SAMPLE_EXP)

# data = run.model.individ.samples(data.path = DATA,
#                                  n.trials = TRIALS,
#                                  min.est = MIN_ESTIMATE, 
#                                  max.est = MAX_ESTIMATE,
#                                  perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
#                                  est.fxn = ESTIMATE_FXN,
#                                  trials.exp = TRIALS_EXP,
#                                  bumper.exp = BUMPER_EXP,
#                                  prior.exp = PRIOR_EXP,
#                                  post.exp = POSTERIOR_SAMPLE_EXP)

data = run.model.individ.memories(data.path = DATA,
                                  n.trials = TRIALS,
                                  n.samples = 20,
                                  p.bumper = 0.6,
                                  min.est = MIN_ESTIMATE,
                                  max.est = MAX_ESTIMATE,
                                  perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
                                  est.fxn = ESTIMATE_FXN,
                                  trials.exp = TRIALS_EXP,
                                  bumper.exp = BUMPER_EXP,
                                  prior.exp = PRIOR_EXP,
                                  post.exp = POSTERIOR_SAMPLE_EXP,
                                  n.memories = 50)

# Calculating CoV
x = 1 # arbitrary
samples = 10000 # arbitrary
magnitude = 10 ^ rnorm(samples, log10(x), sd = PERCEIVED_DOTS_NOISE_SD)
sd(magnitude)/x # approx. CoV



################
### ANALYSIS ###
################

subject.data = 1 # set this variable to view individual subject data below
# Quick view of true numbers and estimates across trials
results = data %>%
  filter(subject == subject.data) %>%
  select(subject, trial, num_dots, model.answer) %>%
  arrange(trial)
results

# Graph three sample subjects and model estimates for a close look
sample.subjects = c(6, 11, 22)
data %>%
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
  individ_plot_theme +
  facet_wrap(~subject, ncol = 3)


# Graph all subjects and model estimates side by side
data %>% # all subjects side by side
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


# Compare each quartile of model estimates to make sure it's not getting worse
data = data %>%
  mutate(quartile = floor(trial / 75.25) + 1)

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
  



### MSE ANALYSIS ###
# NB: this can take a while!! (approx. 60 seconds per run, but this increases as N_SAMPLES increases)
mse.df = data.frame('N' = numeric(), 'p' = numeric(), 'mse.subj' = numeric(), 'mse.mod' = numeric())

MSE = TRUE

if (MSE) {
  P_BUMPER = 0.9 # set P_BUMPER manually for this analysis
  for (n in seq(1, 25)) {
    N_SAMPLES = n
    print(paste("Fitting model with N_SAMPLES = ", N_SAMPLES, ", P_BUMPER = ", P_BUMPER))
    
    data = run.model.baseline(data.path = DATA,
                              n.trials = TRIALS,
                              n.samples = N_SAMPLES, 
                              p.bumper = P_BUMPER, 
                              min.est = MIN_ESTIMATE, 
                              max.est = MAX_ESTIMATE,
                              perceived.mag.sd = PERCEIVED_DOTS_NOISE_SD,
                              est.fxn = ESTIMATE_FXN,
                              trials.exp = TRIALS_EXP,
                              bumper.exp = BUMPER_EXP,
                              prior.exp = PRIOR_EXP,
                              post.exp = POSTERIOR_SAMPLE_EXP)
    
    data = calculate.mse(data)
    
    mse.df = rbind(mse.df, data.frame('N' = N_SAMPLES, 'p' = P_BUMPER,
                                      'mse.subj' = mean(data$subj.sq.error),
                                      'mse.mod' = mean(data$model.sq.error[data$trial > N_SAMPLES])))
    
    
  }
}

if (MSE) {
  mse.df %>% ggplot(aes(x = N)) +
    geom_line(aes(y = mse.subj, color = "subjects")) +
    geom_line(aes(y = mse.mod, color = "model")) +
    geom_point(aes(y = mse.mod, color = "model"), size = 2) +
    scale_color_manual(name = element_blank(), 
                       values = c("subjects" = "blue", "model" = "red")) +
    scale_y_continuous(limits = c(0, 4000)) +
    ggtitle("Model performance with increasing samples") +
    labs(x = "Samples", y = "Estimate error (MSE)") +
    individ_plot_theme_v2 +
    theme(legend.position = c(0.2, 0.2),
          legend.background = element_rect(color = "gray80", size = 0.5, linetype = "solid"))
}

save(mse.df, file = 'mse.RData')




