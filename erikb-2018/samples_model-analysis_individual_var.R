
### README ###
#' This file does an analysis of the *individual variability* across subject estimates 
#' and model estimates for different versions of the model.
#' 
#' It takes the output from the model in `samples_model-fxns_basic` and 
#' then uses more advanced functions in `samples_model-fxns_drift` to
#' fit lines to the model and human estimates, for easy analysis variability
#' in bilinear fit parameters across subject and model participants
#' 


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
  plot.title = element_text(face = "bold", size = 28),
  axis.title.y = element_text(face = "bold", size = 24),
  axis.title.x = element_text(face = "bold", size = 24),
  legend.title = element_text(face = "bold", size = 16),
  # axis text
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14, angle = 90, hjust = 0, vjust = 0),
  # legend text
  legend.text = element_text(size = 14),
  # facet text
  strip.text = element_text(face = "bold", size = 20),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  
  panel.grid = element_line(color = "gray"),
  axis.line = element_line(color = "black"),
  # positioning
  legend.position = "bottom"
)

plot.estimates.all = function(data, graph.title) {
  data %>%
    ggplot(aes(x = num_dots, y = answer)) +
    geom_point(alpha = 0.25, color = "red", size = 0.75) +
    geom_abline() +
    mylogx(c(MIN_ESTIMATE, MAX_ESTIMATE)) +
    mylogy(c(MIN_ESTIMATE, MAX_ESTIMATE + 200)) +
    xlab("Number presented") +
    ylab("Number reported") +
    ggtitle(graph.title) +
    theme(axis.title = element_text(size = 16, face = "bold"),
          title = element_text(size = 18, face = "bold")) +
    facet_wrap(~subject, ncol = 6)
}

plot.model.comparison.histogram = function(model.slopes.low, model.slopes.high, subj.slopes) {
  ggplot() +
    # subject slopes
    geom_histogram(data = model.slopes.low, aes(x = slope.trans, color = "model.low", fill = "model.low"),
                   alpha = 0.5,
                   binwidth = 0.05) +
    # high var slopes
    geom_histogram(data = model.slopes.high, aes(x = slope.trans, color = "model.high", fill = "model.high"),
                   alpha = 0.5,
                   binwidth = 0.05) +
    geom_vline(xintercept = mean(subj.slopes$slope.trans), color = "black", linetype = "dashed", size = 1) +
    scale_color_manual(name = element_blank(), 
                       labels = c("model.low" = "low variability model", 
                                  "model.high" = "high variability model"),
                       values = c("model.low" = "red", "model.high" = "blue")) +
    scale_fill_manual(name = element_blank(), 
                      labels = c("model.low" = "low variability model",
                                 "model.high" = "high variability model"),
                      values = c("model.low" = "red", "model.high" = "blue")) +
    ggtitle("Comparing model slope variability") +
    labs(x = "Fitted bilinear slope") +
    individ_plot_theme
}

plot.model.human.comparison.histogram = function(model.slopes, subj.slopes, plot.title) {
  ggplot() +
    # subject slopes
    geom_histogram(data = subj.slopes, aes(x = slope.trans, color = "subjects", fill = "subjects"),
                   alpha = 0.5,
                   binwidth = 0.05) +
    # high var slopes
    geom_histogram(data = model.slopes, aes(x = slope.trans, color = "model", fill = "model"),
                   alpha = 0.5,
                   binwidth = 0.05) +
    scale_color_manual(name = element_blank(), 
                       labels = c("subjects" = "subjects", 
                                  "model" = "model"),
                       values = c("subjects" = "red", "model" = "blue")) +
    scale_fill_manual(name = element_blank(), 
                      labels = c("subjects" = "subjects", 
                                 "model" = "model"),
                      values = c("subjects" = "red", "model" = "blue")) +
    ggtitle(plot.title) +
    labs(x = "Fitted bilinear slope") +
    individ_plot_theme
}

plot.model.comparison.scatter = function(model.slopes.low, model.slopes.high, subj.slopes) {
  # TODO add dot with vertical and horizontal error bars for human averages
  ggplot() +
    geom_point(data = model.slopes.low, aes(x = cutoff.trans, y = slope.trans, color = "model.low"),
               size = 3) +
    geom_errorbar(data = model.slopes.low, aes(x = cutoff.trans,
                                                   ymin = slope.trans - slope.error.trans, 
                                                   ymax = slope.trans + slope.error.trans,
                                                   color = "model.low"),
                  width = 0) +
    geom_point(data = model.slopes.high, aes(x = cutoff.trans, y = slope.trans, color = "model.high"),
               size = 3) +
    geom_errorbar(data = model.slopes.high, aes(x = cutoff.trans,
                                                    ymin = slope.trans - slope.error.trans, 
                                                    ymax = slope.trans + slope.error.trans,
                                                    color = "model.high"),
                  width = 0) +
    geom_hline(yintercept = 1.0, linetype = "dashed") +
    scale_color_manual(name = "Data source",
                       values = c("model.low" = "red", "model.high" = "blue"),
                       labels = c("model.low" = "low variability model", "model.high" = "high variability model")) +
    # ylim(0, 1.5) +
    # xlim(0, 30) +
    ggtitle("Comparing model fits") +
    labs(x = "Fitted bilinear cutoff", y = "Fitted slope above cutoff") +
    individ_plot_theme
}

plot.model.human.comparison.scatter = function(model.slopes, subj.slopes, plot.title) {
  ggplot() +
    geom_point(data = subj.slopes, aes(x = cutoff.trans, y = slope.trans, color = "subjects"),
               size = 3) +
    geom_errorbar(data = subj.slopes, aes(x = cutoff.trans, color = "subjects",
                                                ymin = slope.trans - slope.error.trans, 
                                                ymax = slope.trans + slope.error.trans),
                  width = 0) +
    geom_point(data = model.slopes, aes(x = cutoff.trans, y = slope.trans, color = "model"),
               size = 3) +
    geom_errorbar(data = model.slopes, aes(x = cutoff.trans,
                                                   ymin = slope.trans - slope.error.trans, 
                                                   ymax = slope.trans + slope.error.trans,
                                                   color = "model"),
                  width = 0) +
    geom_hline(yintercept = 1.0, linetype = "dashed") +
    scale_color_manual(name = "Data source",
                       values = c("subjects" = "red", "model" = "blue"),
                       labels = c("subjects" = "subjects", "model" = "model")) +
    ylim(0, 1.5) +
    xlim(0, 30) +
    ggtitle(plot.title) +
    labs(x = "Fitted bilinear cutoff", y = "Fitted slope above cutoff") +
    individ_plot_theme
}


##################
### RUN MODELS ###
##################


data.model.high.var = run.model.individ.memories(n.memories = 10)

data.model.low.var = run.model.individ.memories(n.memories = 1000)


subj.data = data.model.high.var %>% # can use either here
  select(subject, trial, num_dots, answer)
  
model.data.high.var = data.model.high.var %>%
  select(subject, trial, num_dots, model.answer) %>%
  mutate(answer = model.answer) # align column names to match participant data

model.data.low.var = data.model.low.var %>%
  select(subject, trial, num_dots, model.answer) %>%
  mutate(answer = model.answer) # align column names to match participant data




################
### ANALYSIS ###
################

PARAMS = c(0.7, 1.5, -0.5, 0.2, -0.7, 0.2)
names(PARAMS) = c("ma", "sa", "mb", "sb", "ms", "ss")

PRIORS = list()
PRIORS[[1]] = function(x){-dnorm(x, 2, 3.5, log = T)}
PRIORS[[2]] = function(x){-dnorm(x, 0, 0.5, log = T)}
PRIORS[[3]] = function(x){-dnorm(x, -1, 0.25, log = T)}

# Fit static subject data
bipower.fits.subj = data.frame(do.call(rbind, by(subj.data, subj.data$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.subj$logL == -9999)))
bipower.fits.subj

# Fit model data, high variability
bipower.fits.mod.high = data.frame(do.call(rbind, by(model.data.high.var, model.data.high.var$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.mod.high$logL == -9999)))
bipower.fits.mod.high

# Fit model data, low variability
bipower.fits.mod.low = data.frame(do.call(rbind, by(model.data.low.var, model.data.low.var$subject, brutefit)))
print(paste("Failed bipower fits:", sum(bipower.fits.mod.low$logL == -9999)))
bipower.fits.mod.low

# Add columns transforming fits to non-log space
bipower.fits.subj = bipower.fits.subj %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)

bipower.fits.mod.high = bipower.fits.mod.high %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)

bipower.fits.mod.low = bipower.fits.mod.low %>%
  mutate(cutoff.trans = 10^a,
         slope.trans = 10^b,
         slope.error.trans = 10^s)


# Get predictions based on model fits
predictions = data.frame()
for (s in unique(subj.data$subject)) {
  stims = seq(1, 300, by = 1)
  # subject params, static
  biparams.subj = bipower.fits.subj[bipower.fits.subj$subject == s,]
  bipred.subj = (map.bipower(stims, biparams.subj$a, biparams.subj$b))
  # model params, high var
  biparams.mod.high = bipower.fits.mod.high[bipower.fits.mod.high$subject == s,]
  bipred.mod.high = (map.bipower(stims, biparams.mod.high$a, biparams.mod.high$b))
  # model params, low variability
  biparams.mod.low = bipower.fits.mod.low[bipower.fits.mod.low$subject == s,]
  bipred.mod.low = (map.bipower(stims, biparams.mod.low$a, biparams.mod.low$b))
  predictions = rbind(predictions, 
                      data.frame(subject = s, 
                                 num_dots = stims,
                                 bipred.subj = bipred.subj,
                                 bipred.mod.high = bipred.mod.high,
                                 bipred.mod.low = bipred.mod.low))
}




#############
### PLOTS ###
#############

# View raw (summary) data for slope fits across models, human subjects
summary(bipower.fits.mod.low$slope.trans)
sd(bipower.fits.mod.low$slope.trans)
summary(bipower.fits.mod.high$slope.trans)
sd(bipower.fits.mod.high$slope.trans)
summary(bipower.fits.subj$slope.trans)
sd(bipower.fits.subj$slope.trans)



### Sanity check ###

# plot estimates by subject for human, model data (sanity check)
plot.estimates.all(subj.data, "Estimates from subjects")
plot.estimates.all(model.data.high.var, "Estimates from high variability model")
plot.estimates.all(model.data.low.var, "Estimates from low variability model")


### Comparing models ###

# Histogram of model fits compared against each other
plot.model.comparison.histogram(bipower.fits.mod.low, bipower.fits.mod.high, bipower.fits.subj)
# Scatterplot of model fits (slope and intercept) compared against each other
plot.model.comparison.scatter(bipower.fits.mod.low, bipower.fits.mod.high, bipower.fits.subj)


### Comparing model and human fits ###

# Histogram of *high* variability model fits and human data
plot.model.human.comparison.histogram(bipower.fits.mod.high, bipower.fits.subj, "High variability model and human slope distributions")
# Histogram of *low* variability model fits and human data (may not be necessary)
plot.model.human.comparison.histogram(bipower.fits.mod.low, bipower.fits.subj, "Low variability model and human slope distributions")

# Scatterplot of *high* variability model fits (slope and intercept) and human data (may not be necessary)
plot.model.human.comparison.scatter(bipower.fits.mod.high, bipower.fits.subj, "High variability model and human cutoff and slope values")
# Scatterplot of *low* variability model fits (slope and intercept) and human data (may not be necessary)
plot.model.human.comparison.scatter(bipower.fits.mod.low, bipower.fits.subj, "Low variability model and human cutoff and slope values")
