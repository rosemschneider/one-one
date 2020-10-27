#one-to-one simulation

# SETUP ----
rm(list = ls())
# setwd("/Users/erikbrockbank/web/one-one/")

source("Study 1/Analysis/0-clean.R") # data cleaning script, produces cleaned data
# Load cleaned data - 2 dfs
# load(here::here("Study 1/Data/one-one_cleaned.RData")) #study 1 data
load("Study 1/Data/one-one_cleaned.RData") #study 1 data

# load packages ----
library(tidyverse)
library(magrittr)
library(car)
library(lme4)
library(broom.mixed)
library(tidylog)
library(emmeans)
library(stats4)


# # Custom global variables
cp.sub.palette <- c("#1ECCE3", "#FF7C00")
#global theme set
theme_set(theme_bw() + theme(text = element_text(size=9),
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0)),
                             panel.grid = element_blank()))

# Data manipulations ----

##create age group and scaled/centered age variable; also create capped and centered highest_count
all.data %<>%
  filter(Task == "Parallel" | Task == "Orthogonal") %>% #remove extra rows from non one-one tasks
  mutate(Age = as.numeric(as.character(Age)),
         age.group.floor = factor(floor(Age)),
         age.c = as.vector(scale(Age, center = TRUE, scale = TRUE)),
         highest_count.cap = ifelse(highest_count >= 60, 60, highest_count),
         highest_count.c = as.vector(scale(highest_count.cap, center = TRUE, scale = TRUE)),
         Task_item = as.numeric(as.character(Task_item)),
         Response = as.numeric(as.character(Response)))


## make an error df
error.df <- all.data %>%
  filter(Correct == 0)%>% ##only interested in incorrect responses
  mutate(abs.error = abs(Task_item - Response))

# ...response distribution ----
## function for generating response plots
make_dist_plot_default <- function(df, task, numbers, title) { #numbers should be a vector
  p <- df %>%
    filter(Task == task,
           Task_item %in% numbers)%>%
    group_by(CP_subset, Task_item, Response)%>%
    ggplot(aes(x = Response, fill= CP_subset)) + # Plot user data
    # ggplot(aes(x = approximate_estimate_med, fill= CP_subset)) + # Plot model simulation
    geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
    geom_histogram(color = 'black', binwidth = 1) +
    theme_bw(base_size = 18) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          panel.grid = element_blank()) +
    scale_fill_manual(values = cp.sub.palette) +
    facet_grid(CP_subset ~ Task_item) +
    # scale_x_continuous(breaks = seq(1, 15, 1)) +
    labs(x = 'Number of items given', y = 'Frequency',
         # title = paste0(as.character(task), " Task"))
         title = title)
  print(p)
}


parallel_dist_plot  <- make_dist_plot_default(all.data, "Parallel", c(3, 4, 6, 8, 10), "Parallel Task")
orthogonal_dist_plot <- make_dist_plot_default(all.data, "Orthogonal", c(3, 4, 6, 8, 10), "Orthogonal Task")




# Estimate ~ N(cov(0.64), sigma)  based on Wagner et al. 2018
# Or fit one CoV; this might be silly since we know that's not necessarily appropriate here...


#' Simulation goals:
#' 1. H0 is that people are approximating across the board
#' -> Try to show that the approximation process doesn't accurately capture CP knowers 3-10
#' -> Additionally, show taht this *does* capture Subset knowers 3-10 pretty well
#' 2. H1 is that people are doing some exact matching and they become approximators
#' at higher magnitudes
#' -> Show that this "approximation waterfall" fits the CP knower data better


# Basic approximation function: generates integer estimate by drawing from
# normal distribution centered at `number` with
get_approximate_estimate = function(number, CoV) {
  est = rnorm(1, number, number * 10^CoV)
  if (is.na(est)) {
    print("NA aaaaagh: ")
    #print(number)
    print(CoV)
  }
  max(round(est, 0), 1) # don't allow returning 0 as an option
}




# 1. Manual CoV analysis =======================================================


glimpse(all.data)
subjects = unique(all.data$SID)
trials = as.numeric(unique(all.data$Trial_number))
task = "Parallel"
task_item = sort(unique(all.data$Task_item))
# baseline
low_cov = log10(0.16)
med_cov = log10(0.32)
high_cov = log10(0.64)



# Generate simulated matching data
simulation_data = all.data %>%
  filter(Task == "Parallel",
         CP_subset == "CP") %>%
  rowwise() %>%
  mutate(approximate_estimate_low = get_approximate_estimate(Task_item, low_cov),
         approximate_estimate_med = get_approximate_estimate(Task_item, med_cov),
         approximate_estimate_high = get_approximate_estimate(Task_item, high_cov))%>%
  pivot_longer(cols = c(approximate_estimate_low, approximate_estimate_med,
                        approximate_estimate_high), 
               names_to = "approximation_level", 
               values_to = "approximation_response")%>%
  mutate(COV_level = ifelse(approximation_level == "approximate_estimate_low", "0.16", 
                            ifelse(approximation_level == "approximate_estimate_med", "0.32", "0.64")))

##plot this
make_dist_plot <- function(df, task, numbers, title) { #numbers should be a vector
  p <- df %>%
    filter(Task == task,
           Task_item %in% numbers)%>%
    group_by(CP_subset, Task_item, Response)%>%
    ggplot(aes(x = approximation_response, fill= COV_level)) + # Plot model simulation
    geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
    geom_histogram(color = 'black', binwidth = 1) +
    theme_bw(base_size = 18) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          panel.grid = element_blank()) +
    langcog::scale_fill_solarized("COV levels") +
    facet_grid(COV_level ~ Task_item) +
    # scale_x_continuous(breaks = seq(1, 15, 1)) +
    labs(x = 'Number of items given', y = 'Frequency',
         # title = paste0(as.character(task), " Task"))
         title = title)
  print(p)
}


# Plot results
simulated_plot <- make_dist_plot(simulation_data, "Parallel", task_item, "Simulated data: Low, med., high COV")
# simulated_plot_med <- make_dist_plot(simulation_data, "Parallel", task_item, paste("Model (CoV = ", 10^med_cov,")"))
# simulated_plot_high <- make_dist_plot(simulation_data, "Parallel", task_item, paste("Model (CoV = ", 10^high_cov,")"))




# 2. Fitted CoV analysis =======================================================

# Fit CoV instead of using values manually generated above
# Then generate model data with fitted CoV (but use process above)

# log likelihood function
# Returns (log) probability of sampling the *difference* between
# the subject's response and the generated response (via approximation)
# from a normal distribution centered at 0 with sd = s
loglik = function(task_item, subj_resp, approx_fxn, cov_val, s) {
  sum(
    pmax(-6, dnorm(log10(subj_resp) - log10(approx_fxn(task_item, cov_val)), 0, s, log = T))
  )
}

# fit function
# `tmp` is data
brutefit = function(data, usefx) {
  nLL = function(cov_fitted, s) {
    -loglik(data$Task_item, data$Response, usefx, cov_fitted, 10^s) +
      priors[[1]](cov_fitted) +
      priors[[2]](s)
  }
  
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL,
                           start = list(cov_fitted = -0.8, # 10^-0.8 = .15ish
                                        # errors range between about log10(5) - log10(10) and log10(15) - log10(10)
                                        s = -1))), 
        TRUE) 
    iter = iter + 1
    
    if (!is.null(fit)) {
      # TODO what's up with this 0.5??
      fits = c(-0.5*fit@m2logL, length(data$Task_item), fit@coef[,"Estimate"])
    } else {
      if (iter > 500) {
        fits = c(-9999, 0, 0, 0)
      }
    }
  }
  names(fits) = c("logL", "n", "fitted_cov", "s")
  return(fits)
}

priors = list()
priors[[1]] = function(x) {-dnorm(x, -0.8, 0.2, log = T)} # priors for (log) cov value
priors[[2]] = function(x) {-dnorm(x, -0.1, 0.01, log = T)} # priors for s value


brutefit(simulation_data, get_approximate_estimate)
# TODO this seems to allow positively insane cov values;
# our approximation function sometimes produces NAs because
# the fitted CoV values are in the 100s, 1000s, ...
# Does this mean the data isn't easy to fit or we did something wrong?
# If we don't include priors, the fitted values are insane for both CoV and s...


#' Notes
#' may need to handle possibility of exact matching on 3 (even for approximators) because of PI
#' For CP knowers, may have stable label for 3 so similar to above concern
