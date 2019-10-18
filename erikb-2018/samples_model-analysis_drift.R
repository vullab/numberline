
### README ###
#' This file does an analysis of the *calibration drift* across subject estimates 
#' and model estimates for different versions of the model.
#' 
#' It takes the output from the model in `samples_model-fxns_basic` and 
#' then uses more advanced functions in `samples_model-fxns_drift` to
#' fit lines to the model and human estimates, for easy analysis of model drift 
#' under various parameters and comparison between model and participant calibration drift.
#' 


setwd("/Users/erikbrockbank/web/vullab/numberline/erikb-2018/")
rm(list=ls())

library(viridis)

# Fetch relevant model functions from samples_model
source('samples_model-fxns_basic.R')
# Fetch relevant functions for fittig lines to model data
source('samples_model-fxns_drift.R')


##########################
### ANALYSIS FUNCTIONS ###
##########################

# Graphing functions
my.log.breaks = function(lims){
  majors = seq(floor(log10(lims[1])), ceiling(log10(lims[2])), by = 1)
  minors = log10(unlist(lapply(majors[-1], function(x){seq(10 ^ (x - 1), 9 * 10 ^ (x - 1), by = 10 ^ (x - 1))})))
  return(list(majors, minors))
}

mylogx = function(lims) {
  breaks = my.log.breaks(lims)
  scale_x_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

mylogy = function(lims) {
  breaks = my.log.breaks(lims)
  scale_y_log10(limits = lims, 
                breaks = 10 ^ breaks[[1]], 
                minor_breaks = breaks[[2]])
}

drift_theme = theme(
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


plot.all.subjects = function(data) {
  # Plot participant/model estimates
  data %>%
    ggplot(aes(x = num_dots, y = answer)) +
    geom_point(alpha = 0.25, color = "red", size = 0.75) +
    geom_abline() +
    mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
    mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE + 200)) +
    xlab("number presented") +
    ylab("number reported") +
    ggtitle("Number estimates for each participant") +
    theme(axis.title = element_text(size = 16, face = "bold"),
          title = element_text(size = 18, face = "bold")) +
    facet_wrap(~subject, ncol = 6)
  
}

plot.corr.matrix = function(data) {
  # Plot slope correlations by trial block
  data %>%
    ggplot(aes(x = as.factor(block1), y = as.factor(block2), fill = slope.corr)) +
    geom_tile() +
    scale_fill_gradient2(low = "white", mid = "white", high = "red", midpoint = 0.3, limits = c(-0.5, 1)) +
    xlab("") + ylab("") +
    ggtitle("Trial block slope correlations") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme(axis.ticks = element_blank(),
          axis.text = element_text(size = 16, face = "bold"),
          axis.text.y = element_text(angle = 90),
          legend.title = element_blank(),
          legend.text = element_text(size = 12, face = "bold"),
          title = element_text(size = 18, face = "bold"),
          panel.grid = element_blank())
  
}

plot.drift.single = function(data) {
  data %>%
    ggplot(aes(x = trial.dist, y = mean.cor)) +
    geom_point(color = "red") +
    geom_errorbar(aes(ymin = mean.cor - se.cor,
                      ymax = mean.cor + se.cor),
                  width = 5,
                  color = "red") +
    labs(x = "Distance (trials)",
         y = "Slope correlation") +
    drift_theme
}

plot.drift.composite = function(data.subj, data.model.base, data.model.iv) {
  # Plot slope correlations by trial distance for model and human subjects
  ggplot() +
    # subject data
    geom_point(data = data.subj,
               aes(x = trial.dist,
                   y = mean.cor,
                   color = "subjects")) +
    geom_errorbar(data = data.subj,
                  aes(x = trial.dist,
                      ymin = mean.cor - se.cor,
                      ymax = mean.cor + se.cor,
                      color = "subjects"),
                  width = 5) +
    # baseline model data
    geom_point(data = data.model.base,
               aes(x = trial.dist,
                   y = mean.cor,
                   color = "model.base")) +
    geom_errorbar(data = data.model.base,
                  aes(x = trial.dist,
                      ymin = mean.cor - se.cor,
                      ymax = mean.cor + se.cor,
                      color = "model.base"),
                  width = 5) +
  # individual variability model
  geom_point(data = data.model.iv,
             aes(x = trial.dist,
                 y = mean.cor,
                 color = "model.iv")) +
    geom_errorbar(data = data.model.iv,
                  aes(x = trial.dist,
                      ymin = mean.cor - se.cor,
                      ymax = mean.cor + se.cor,
                      color = "model.iv"),
                  width = 5) +
    #xlim(0, 255) + # NB: Tweak this as needed
    #ylim(-0.2, 1) + # NB: Tweak this as needed
    labs(x = "Distance (trials)", y = "Slope correlation") +
    #ggtitle("Drift in estimate calibration") +
    scale_color_manual(name = element_blank(), 
                       labels = c("subjects" = "subjects", "model.base" = "model (baseline)", "model.iv" = "model (individual variability)"),
                       values = c("subjects" = "red", "model.base" = "blue", "model.iv" = "darkseagreen")) +
    # theme(legend.position = c(0.2, 0.1),
    #       legend.background = element_rect(color = "gray80", size = 0.5, linetype = "solid")) +
    drift_theme
}


plot.drift.ribbon = function(corr.data) {
  corr.data %>%
    ggplot(aes(x = trial.dist, y = corr.mean, color = source)) +
    geom_ribbon(stat = "identity", aes(ymin = corr.min, ymax = corr.max)) +
    labs(x = "Distance (trials)", y = "Slope correlation") +
    scale_color_viridis(discrete = T,
                        labels = c("subjects" = "subjects", "model.base" = "model (baseline)", "model.iv" = "model (individual variability)")) +
    drift_theme
}



###############
### GLOBALS ###
###############

save.data = FALSE # toggle to save data

# Initialize params (mean a, var a, mean b, var b, mean s, var s)
PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2) 
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")

# Initialize priors
PRIORS = list()
PRIORS[[1]] = function(x){-dnorm(x, 1.5, 0.1, log = T)} #
PRIORS[[2]] = function(x){-dnorm(x, -0.2, 0.1, log = T)} #
PRIORS[[3]] = function(x){-dnorm(x, -1, 0.1, log = T)} #


MODEL_RUNS = 10


  


################
### ANALYSIS ###
################

corr.data = data.frame(
  source = character(),
  model.run = numeric(),
  trial.dist = numeric(),
  mean.cor = numeric(),
  se.cor = numeric()
)

for (x in seq(1:MODEL_RUNS)) {
  print(paste("######## CYCLE: ", x, " ########")) # approx. 5 mins / cycle
  # Run models
  data.base = run.model.baseline()
  data.iv = run.model.individ.memories(n.memories = 10)
  
  # Select relevant data for subjects, base model, individual variability model
  subj.data = data.base %>%
    select(subject, trial, num_dots, answer)
  
  model.base = data.base %>%
    mutate(answer = model.answer) %>% # align column names to match participant data
    select(subject, trial, num_dots, answer)
  
  model.iv = data.iv %>%
    mutate(answer = model.answer) %>% # align column names to match participant data
    select(subject, trial, num_dots, answer)
  
  ### Subject drift ###
  # Fit slopes, NB: this can take ~10s
  fitsBlock.subj = fit.slopes(c(BLOCKSIZE), subj.data)
  # Get matrix of fitted slope correlations
  cor.matrix.subj = get.cor.matrix(fitsBlock.subj)
  # Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
  slope.cor.df.subj = get.cor.df(cor.matrix.subj)
  # Process slope correlations by trial distance to get mean, se across participants
  cor.means.df.blocks.subj = get.distance.cors(slope.cor.df.subj)
  
  ### Model drift: baseline ###
  # Fit slopes, NB: this can take ~10s
  fitsBlock.model.base = fit.slopes(c(BLOCKSIZE), model.base)
  # Get matrix of fitted slope correlations
  cor.matrix.model.base = get.cor.matrix(fitsBlock.model.base)
  # Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
  slope.cor.df.model.base = get.cor.df(cor.matrix.model.base)
  # Process slope correlations by trial distance to get mean, se across participants
  cor.means.df.blocks.model.base = get.distance.cors(slope.cor.df.model.base)
  
  
  ### Model drift: individual variability ###
  # Fit slopes, NB: this can take ~10s
  fitsBlock.model.iv = fit.slopes(c(BLOCKSIZE), model.iv)
  # Get matrix of fitted slope correlations
  cor.matrix.model.iv = get.cor.matrix(fitsBlock.model.iv)
  # Format correlation matrix as data frame to plot slope correlations by trial block in analysis section below
  slope.cor.df.model.iv = get.cor.df(cor.matrix.model.iv)
  # Process slope correlations by trial distance to get mean, se across participants
  cor.means.df.blocks.model.iv = get.distance.cors(slope.cor.df.model.iv)
  
  
  # Aggregate data from above
  corr.data = rbind(corr.data, data.frame(
    source = "subjects",
    model.run = x,
    trial.dist = cor.means.df.blocks.subj$trial.dist,
    mean.cor = cor.means.df.blocks.subj$mean.cor,
    se.cor = cor.means.df.blocks.subj$se.cor
  ))
  
  corr.data = rbind(corr.data, data.frame(
    source = "model.base",
    model.run = x,
    trial.dist = cor.means.df.blocks.model.base$trial.dist,
    mean.cor = cor.means.df.blocks.model.base$mean.cor,
    se.cor = cor.means.df.blocks.model.base$se.cor
  ))
  
  corr.data = rbind(corr.data, data.frame(
    source = "model.iv",
    model.run = x,
    trial.dist = cor.means.df.blocks.model.iv$trial.dist,
    mean.cor = cor.means.df.blocks.model.iv$mean.cor,
    se.cor = cor.means.df.blocks.model.iv$se.cor
  ))
  
}



corr.data.summary = corr.data %>%
  group_by(source, trial.dist) %>%
  summarize(corr.mean = mean(mean.cor),
            corr.min = min(mean.cor),
            corr.max = max(mean.cor))

plot.drift.ribbon(corr.data.summary)




plot.drift.composite(cor.means.df.blocks.subj,
                     cor.means.df.blocks.model.base,
                     cor.means.df.blocks.model.iv)


#############
### PLOTS ###
#############

# Plot individual estimates by subjects, models
est.plot.subj = plot.all.subjects(subj.data)
est.plot.model.base = plot.all.subjects(model.base)
est.plot.model.iv = plot.all.subjects(model.iv)

# Plot correlation matrix for subject estimates, models
matrix.subj = plot.corr.matrix(slope.cor.df.subj)
matrix.model.base = plot.corr.matrix(slope.cor.df.model.base)
matrix.model.iv = plot.corr.matrix(slope.cor.df.model.iv)


# Plot drift over trial distance *individually* for subject estimates, models
drift.plot.subj = plot.drift.single(cor.means.df.blocks.subj)
drift.plot.model.base = plot.drift.single(cor.means.df.blocks.model.base)
drift.plot.model.iv = plot.drift.single(cor.means.df.blocks.model.iv)

# Plot drift for human subjects alongside model runs
drift.plot.composite = plot.drift.composite(cor.means.df.blocks.subj,
                                            cor.means.df.blocks.model.base,
                                            cor.means.df.blocks.model.iv)


# Display plots: subjects 
# NB: can print any of the plots above for model runs as well
est.plot.subj
matrix.subj
drift.plot.subj
drift.plot.composite


if (save.data) {
  save(cor.means.df.blocks.subj, cor.means.df.blocks.model.base, cor.means.df.blocks.model.iv,
       file = 'samples_model-drift.RData')
}