### README ###
#' This file does an analysis of the reliability of slope and cutoff fits across 
#' subject estimates and model estimates with different numbers of trials per fit.
#' We're trying to understand e.g. whether the model fits are more noisy than human fits
#' for fitting in blocks of 10, 20, ..., 100 trials
#' 
#' It takes the output from the model in `samples_model-fxns_basic` and 
#' then uses more advanced functions in `samples_model-fxns_drift` to
#' fit lines to the model and human estimates.
#' 


setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list = ls())

library(viridis)

# Fetch relevant model functions from samples_model
source('samples_model-fxns_basic.R')
# Fetch relevant functions for fittig lines to model data
source('samples_model-fxns_drift.R')


# Functions ====================================================================
drift_theme = theme(
  # titles
  plot.title = element_text(face = "bold", size = 32),
  axis.title.y = element_text(face = "bold", size = 16),
  axis.title.x = element_text(face = "bold", size = 16),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14, hjust = 1), #, angle = 60, hjust = 0, vjust = 0.1
  # legend text
  legend.text = element_text(size = 14),
  # facet text
  strip.text = element_text(face = "bold", size = 28),
  # backgrounds, lines
  panel.background = element_blank(),
  legend.key = element_rect(colour = "transparent", fill = "transparent"),

  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)



# Run models ===================================================================
data.base = run.model.baseline()
data.iv = run.model.individ.memories(n.memories = 10)

# Select relevant data
subj.data = data.base %>%
  select(subject, trial, num_dots, answer)

model.base = data.base %>%
  mutate(answer = model.answer) %>% # align column names to match participant data
  select(subject, trial, num_dots, answer)

model.iv = data.iv %>%
  mutate(answer = model.answer) %>% # align column names to match participant data
  select(subject, trial, num_dots, answer)


# Fit slopes ===================================================================

# each element here is the number of blocks, *not size* of blocks
BLOCK_CHECKS = c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 25, 30)
# Initialize params (mean a, var a, mean b, var b, mean s, var s)
PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2) 
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")

# Initialize priors
PRIORS = list()
PRIORS[[1]] = function(x){-dnorm(x, 1.5, 0.1, log = T)} #
PRIORS[[2]] = function(x){-dnorm(x, -0.2, 0.1, log = T)} #
PRIORS[[3]] = function(x){-dnorm(x, -1, 0.1, log = T)} #



# Analysis: within-subject variance of fitted slopes across blocks =============

blocksize = 30
fitsBlock.subj = fit.slopes(c(blocksize), subj.data)
fitsBlock.model.base = fit.slopes(c(blocksize), model.base)
fitsBlock.model.iv = fit.slopes(c(blocksize), model.iv)

combined_slopes = data.frame(source = character(),
                            block = numeric(),
                            subject = numeric(),
                            slope_est = numeric())

for (block_idx in seq(1:length(fitsBlock.subj))) {
  subj_slopes = fitsBlock.subj[[block_idx]]
  combined_slopes = rbind(combined_slopes, data.frame(
    source = "subjects",
    block = block_idx,
    subject = subj_slopes$subject,
    slope_est = subj_slopes$b
  ))
  
  model_base_slopes = fitsBlock.model.base[[block_idx]]
  combined_slopes = rbind(combined_slopes, data.frame(
    source = "model_baseline",
    block = block_idx,
    subject = model_base_slopes$subject,
    slope_est = model_base_slopes$b
  ))
  
  model_iv_slopes = fitsBlock.model.iv[[block_idx]]
  combined_slopes = rbind(combined_slopes, data.frame(
    source = "model_iv",
    block = block_idx,
    subject = model_iv_slopes$subject,
    slope_est = model_iv_slopes$b
  ))
}

combined_slope_summary = combined_slopes %>%
  group_by(source, subject) %>%
  summarize(slope_mean = mean(slope_est),
            slope_sd = sd(slope_est))

# Plots ========================================================================
# Each point here is an individual subject's SD of fitted slopes across 30 trial blocks
combined_slope_summary %>%
  ggplot(aes(x = source, y = slope_sd, color = source)) +
  geom_jitter(alpha = 0.75, size = 3, width = 0.25) +
  drift_theme +
  # ylim(0.25, 0.8) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "Within-subject SD of fitted slopes (across blocks)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("subjects" = "subjects", 
                                 "model_baseline" = "model (baseline)", 
                                 "model_iv" = "model (individual variability)"))


# Each point here is an individual subject's mean of fitted slopes across 30 trial blocks
combined_slope_summary %>%
  ggplot(aes(x = source, y = slope_mean, color = source)) +
  geom_jitter(alpha = 0.75, size = 3, width = 0.25) +
  drift_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(y = "Within-subject mean of fitted slopes (across blocks)") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("subjects" = "subjects", 
                                 "model_baseline" = "model (baseline)", 
                                 "model_iv" = "model (individual variability)"))




# Analysis: slope fits and error by block size =================================

slope.fits.df = data.frame(
  source = character(),
  num_blocks = numeric(),
  trials = numeric(),
  mean_slope_se = numeric()
)


for (block_number in BLOCK_CHECKS) {
  print(paste("######## BLOCKS: ", block_number, " ########")) # time estimate:
  # Fit slopes. NB: this can take ~10s each
  fitsBlock.subj = fit.slopes(c(block_number), subj.data)
  fitsBlock.model.base = fit.slopes(c(block_number), model.base)
  fitsBlock.model.iv = fit.slopes(c(block_number), model.iv)
  
  # Summarize fits 
  # TODO this is almost certainly a slow way to do this...
  slope.means = c()
  slope.se.means = c()
  error.means = c()
  error.se.means = c()
  for (block in fitsBlock.subj) {
    slope.means = append(slope.means, 10^mean(block$b))
    slope.se.means = append(slope.se.means, mean(block$se.b))
    error.means = append(error.means, 10^mean(block$s))
    error.se.means = append(error.se.means, mean(block$se.s))
  }
  slope.fits.df = rbind(slope.fits.df, data.frame(
    source = "subjects",
    num_blocks = block_number,
    trials = TRIALS / block_number,
    mean_slope = mean(slope.means),
    mean_slope_se = mean(slope.se.means),
    mean_error = mean(error.means),
    mean_error_se = mean(error.se.means)
  ))
  
  slope.means = c()
  slope.se.means = c()
  error.means = c()
  error.se.means = c()
  for (block in fitsBlock.model.base) {
    slope.means = append(slope.means, 10^mean(block$b))
    slope.se.means = append(slope.se.means, mean(block$se.b))
    error.means = append(error.means, 10^mean(block$s))
    error.se.means = append(error.se.means, mean(block$se.s))
  }
  slope.fits.df = rbind(slope.fits.df, data.frame(
    source = "model_baseline",
    num_blocks = block_number,
    trials = TRIALS / block_number,
    mean_slope = mean(slope.means),
    mean_slope_se = mean(slope.se.means),
    mean_error = mean(error.means),
    mean_error_se = mean(error.se.means)
  ))
  
  slope.means = c()
  slope.se.means = c()
  error.means = c()
  error.se.means = c()
  for (block in fitsBlock.model.iv) {
    slope.means = append(slope.means, 10^mean(block$b))
    slope.se.means = append(slope.se.means, mean(block$se.b))
    error.means = append(error.means, 10^mean(block$s))
    error.se.means = append(error.se.means, mean(block$se.s))
  }
  slope.fits.df = rbind(slope.fits.df, data.frame(
    source = "model_iv",
    num_blocks = block_number,
    trials = TRIALS / block_number,
    mean_slope = mean(slope.means),
    mean_slope_se = mean(slope.se.means),
    mean_error = mean(error.means),
    mean_error_se = mean(error.se.means)
  ))
}

slope.fits.df


# Plots ========================================================================

# Look at average of fitted slopes (see how stable they are)
slope.plot = slope.fits.df %>%
  ggplot(aes(x = trials, y = mean_slope, color = source)) +
  geom_line() +
  geom_point() +
  labs(x = "Trials per block", y = "Mean of fitted slopes") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("subjects" = "subjects", 
                                 "model_baseline" = "model (baseline)", 
                                 "model_iv" = "model (individual variability)")) +
  drift_theme


# Look at mean Std. Err. of fitted slopes
slope.se.plot = slope.fits.df %>%
  ggplot(aes(x = trials, y = mean_slope_se, color = source)) +
  geom_line() +
  geom_point() +
  labs(x = "Trials per block", y = "Mean of fitted slope Std. Err.") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("subjects" = "subjects", 
                                 "model_baseline" = "model (baseline)", 
                                 "model_iv" = "model (individual variability)")) +
  drift_theme

# Look at mean of MLE error
mle.error.plot = slope.fits.df %>%
  ggplot(aes(x = trials, y = mean_error, color = source)) +
  geom_line() +
  geom_point() +
  labs(x = "Trials per block", y = "Mean of MLE error") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("subjects" = "subjects", 
                                 "model_baseline" = "model (baseline)", 
                                 "model_iv" = "model (individual variability)")) +
  drift_theme

# Look at mean of MLE error Std. Err.
mle.error.se.plot = slope.fits.df %>%
  ggplot(aes(x = trials, y = mean_error_se, color = source)) +
  geom_line() +
  geom_point() +
  labs(x = "Trials per block", y = "Mean of MLE error Std. Err.") +
  scale_color_viridis(discrete = T,
                      name = element_blank(),
                      labels = c("subjects" = "subjects", 
                                 "model_baseline" = "model (baseline)", 
                                 "model_iv" = "model (individual variability)")) +
  drift_theme



Rmisc::multiplot(slope.plot, 
                 slope.se.plot, 
                 mle.error.plot, 
                 mle.error.se.plot,
                 cols = 2)
  







