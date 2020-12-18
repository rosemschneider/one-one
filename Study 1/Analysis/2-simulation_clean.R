# One-to-one data simulation
# Erik Brockbank and Rose Schneider

# SETUP ----
rm(list = ls())

source("Study 1/Analysis/0-clean.R") # data cleaning script, produces cleaned data
# Load cleaned data
load("Study 1/Data/one-one_cleaned.RData") #study 1 data

# ...load packages ----
library(tidyverse)
library(magrittr)
library(tidylog)
library(stats4)
library(psych) # used for logit and logistic transformations


# # Custom global variables
cp.sub.palette <- c("#1ECCE3", "#FF7C00")
#global theme set
theme_set(theme_bw() + theme(text = element_text(size=12),
                             axis.title=element_text(size=11),
                             strip.text = element_text(margin=margin(2,0,2,0)),
                             panel.grid = element_blank()))

# Data manipulations ----
all.data %<>%
  filter(Task == "Parallel" | Task == "Orthogonal") %>% #remove extra rows from non one-one tasks
  mutate(Age = as.numeric(as.character(Age)),
         Task_item = as.numeric(as.character(Task_item)),
         Response = as.numeric(as.character(Response)))

# data-specific variables
GIVE_ALL_MAX = 15
TASK_ITEMS = sort(unique(all.data$Task_item))


# # 0.response distribution ----
# ## function for generating response plots
# make_dist_plot_default <- function(df, task, numbers, title) { #numbers should be a vector
#   p <- df %>%
#     filter(Task == task,
#            Task_item %in% numbers)%>%
#     group_by(CP_subset, Task_item, Response)%>%
#     ggplot(aes(x = Response, fill= CP_subset)) + # Plot user data
#     # ggplot(aes(x = approximate_estimate_med, fill= CP_subset)) + # Plot model simulation
#     geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
#     geom_histogram(color = 'black', binwidth = 1) +
#     theme_bw(base_size = 18) +
#     theme(legend.position = "none",
#           axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#           panel.grid = element_blank()) +
#     scale_fill_manual(values = cp.sub.palette) +
#     facet_grid(CP_subset ~ Task_item) +
#     # scale_x_continuous(breaks = seq(1, 15, 1)) +
#     labs(x = 'Number of items given', y = 'Frequency',
#          # title = paste0(as.character(task), " Task"))
#          title = title)
#   print(p)
# }
# 
# 
# parallel_dist_plot  <- make_dist_plot_default(all.data, "Parallel", c(3, 4, 6, 8, 10), "Parallel Task")
# orthogonal_dist_plot <- make_dist_plot_default(all.data, "Orthogonal", c(3, 4, 6, 8, 10), "Orthogonal Task")

# 1. Basic approximation function =======================================================
# Basic approximation function: generates integer estimate by drawing from
# normal distribution centered at number with sd = number * CoV
get_approximate_estimate = function(number, CoV) {
  # sample from normal distribution: return 0 if sample < 0, 15 if sample > 15
  min(max(round(rnorm(1, number, number * CoV), 0), 1), GIVE_ALL_MAX)
}

# 2. Fitted CoV analysis: Subset ===============================================
# This fits a CoV to subset knower data
# Then generates model data with fitted CoV


# log likelihood function: find MLE for CoV
# Returns probability of sampling the subject's response 
# from a normal distribution centered at the prompted value, with sd = prompted value * cov
# NB we fit a value for cov_val in log space to avoid a negative sd in pnorm calls below
loglik_approx = function(task_item, subj_resp, cov_val) {
  sum(
    ifelse(subj_resp == GIVE_ALL_MAX, 
           # subject response was maximum: return probability of value >= 15
           log(
             (1 - pnorm(subj_resp - 0.5, mean = task_item, sd = exp(cov_val) * task_item)) / 
               # normalize by probability of response > 0
               (1 - pnorm(0, mean = task_item, sd = exp(cov_val) * task_item))),
           # subject response was < maximum
           log(
             (pnorm(subj_resp + 0.5, mean = task_item, sd = exp(cov_val) * task_item) -
                pnorm(subj_resp - 0.5, mean = task_item, sd = exp(cov_val) * task_item)) /
               # normalize by probability of response > 0
               (1 - pnorm(0, mean = task_item, sd = exp(cov_val) * task_item))))
  )
}


# fit function
mle_fit_approx = function(data, fit_params) {
  nLL = function(cov_fitted) {
    -loglik_approx(data$Task_item, data$Response, cov_fitted) +
      priors[[1]](cov_fitted)
  }
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL,
                           start = list(cov_fitted = log(0.2)))), # fitting log value here: exp(-1.6) =~ 0.2
        TRUE) 
    iter = iter + 1
    
    if (!is.null(fit)) {
      # m2logL is deviance (-2x LL)
      fits = c(-0.5*fit@m2logL, length(data$Task_item), fit@coef[,"Estimate"])
    } else {
      if (iter > 500) {
        fits = c(-9999, 0, 0)
      }
    }
  }
  names(fits) = fit_params
  return(fits)
}



## Analysis
fit_params_subset_approx = c("logL", "n", "cov_fitted")
priors = list()
priors[[1]] =  function(x) {-dnorm(x, log(0.3), 0.1, log = T)} # priors for cov value in log space
# priors[[1]] = function(x){0} # NB: without prior, this fits really high CoV (~.7)

# Pull out data to fit
subset_data = all.data %>%
  filter(CP_subset == "Subset",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10))

# Do MLE fit for CoV
subset_vars_approx = mle_fit_approx(subset_data, fit_params_subset_approx)
subset_vars_approx
exp(subset_vars_approx['cov_fitted']) #fits a COV of around .56

# large scale simulation
SAMPLE_N = 10000
obs = length(subset_data$Task_item)
subset_simulation_data_approx = data.frame(
  Task_item = rep(unique(subset_data$Task_item), SAMPLE_N)
)
subset_simulation_data_approx = subset_simulation_data_approx %>%
  rowwise() %>%
  mutate(Response = get_approximate_estimate(Task_item, 
                                                   exp(subset_vars_approx['cov_fitted'])))

scale_factor = obs / (SAMPLE_N * length(unique(subset_data$Task_item)))

### Plot these simulations overlaid with actual data
ggplot(subset_data, aes(x = Response)) + 
  geom_vline(aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(aes(y = ..count.., fill = "Subset-knower"), binwidth = 1, color = 'black', 
                 alpha = .9) + #CP-data first
  geom_histogram(data = subset_simulation_data_approx, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black', 
                 binwidth = 1, alpha = .5) + #simulation - approx next
  scale_x_continuous(breaks= seq(1, 15, 1)) +
  facet_grid(~Task_item) +
  scale_y_continuous(breaks = seq(0, 40, by = 10)) +
  ylim(c(0, 40)) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "top") +
  scale_fill_manual(name = "Data type", 
                    values = c('Simulated' = "#827f7d", 'Subset-knower' = "#FF7C00")) +
  labs(y = "Frequency", 
       fill = "legend") 

# 3. Fitted CoV analysis: Subset with "give-all" percent =======================
# Fit CoV instead of using values manually generated,
# along with percent of people who are doing "give-all".
# Then generate model data with fitted CoV and give-all percent

# log likelihood function: find MLE for CoV and give-all percent
# logistic(give_all_log_odds) percent of the time, returns maximum value
# (1-logistic(give_all_log_odds)) percent of the time, returns probability of sampling the subject's response 
# from a normal distribution centered at the prompted value, with sd = prompted value * cov
# TODO clean this up, move logic to separate functions, etc.
loglik_give_all = function(task_item, subj_resp, cov_val, give_all_log_odds) {
  sum(
    log(
      # match_pct of the time, subject's value is the maximum
      (logistic(give_all_log_odds) * (
        subj_resp == GIVE_ALL_MAX)) +
        # (1 - match_pct) of the time, subject's value is approximation
        (1 - logistic(give_all_log_odds)) * (
          # subject response was < maximum
          (pnorm(subj_resp + 0.5, mean = task_item, sd = exp(cov_val) * task_item) -
             pnorm(subj_resp - 0.5, mean = task_item, sd = exp(cov_val) * task_item)) /
            (1 - pnorm(0, mean = task_item, sd = exp(cov_val) * task_item))
        )
    )  
  )
}

# fit function
mle_fit_give_all = function(data, fit_params) {
  nLL = function(cov_fitted, give_all_log_odds_fitted) {
    -loglik_give_all(data$Task_item, data$Response, cov_fitted, give_all_log_odds_fitted) +
      priors[[1]](cov_fitted) +
      priors[[2]](give_all_log_odds_fitted)
  }
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL,
                           start = list(cov_fitted = log(0.2),
                                        give_all_log_odds_fitted = logit(0.25)))), # convert starting probability to log odds
        TRUE) 
    iter = iter + 1
    
    if (!is.null(fit)) {
      # m2logL is deviance (-2x LL)
      fits = c(-0.5*fit@m2logL, length(data$Task_item), fit@coef[,"Estimate"])
    } else {
      if (iter > 1000) {
        fits = c(-9999, length(data$Task_item), 0, 0)
      }
    }
  }
  names(fits) = fit_params
  return(fits)
}


# Approximation function: generates integer estimate by drawing from
# normal distribution centered at number with sd = number * CoV or
# by simply giving the maximum value give_all_percent of the time
get_mixture_give_all_estimate = function(number, CoV, give_all_percent) {
  if (rbinom(1, 1, give_all_percent)) {
    # give_all_percent of the time, return maximum value
    return(GIVE_ALL_MAX)
  } else {
    # sample from normal distribution: return 0 if sample < 0, 15 if sample > 15
    return(
      min(max(round(rnorm(1, number, number * CoV), 0), 1), GIVE_ALL_MAX)
    )
  }
}



# Fit CoV and give-all percent 
fit_params_give_all = c("logL", "n", "cov_fitted", "give_all_log_odds_fitted")
priors = list()
priors[[1]] = function(x) {-dnorm(x, log(0.2), 0.1, log = T)} # priors for cov value in log space
# priors[[1]] = function(x){0}
priors[[2]] =  function(x) {-dnorm(logistic(x), 0.25, 0.1, log = T)} # priors for give-all pct log odds
# priors[[2]] = function(x){0}

# MLE fit for CoV and give-all percent
# TODO fit this as a percent of subjects rather than responses
subset_vars_give_all = mle_fit_give_all(subset_data, fit_params_give_all)
subset_vars_give_all
exp(subset_vars_give_all['cov_fitted'])
logistic(subset_vars_give_all['give_all_log_odds_fitted'])



# Model comparison with regular approx with subset
k_approx = 1
BIC_approx_sub = -2 * subset_vars_approx['logL'] +
  k_approx * log(length(subset_data$SID))
BIC_approx_sub

k_give_all = 2
BIC_give_all_sub = -2 * subset_vars_give_all['logL'] +
  k_give_all * log(length(subset_data$SID))
BIC_give_all_sub

BIC_give_all_sub < BIC_approx_sub # give-all is a slightly better fit
# than approx. Not super compelling, but interesting

# Simulate responses based on fitted CoV and give-all percent
SAMPLE_N = 10000
obs = length(subset_data$Task_item)
subset_simulation_data_give_all = data.frame(
  Task_item = rep(unique(subset_data$Task_item), SAMPLE_N)
)
subset_simulation_data_give_all = subset_simulation_data_give_all %>%
  rowwise() %>%
  mutate(Response = get_mixture_give_all_estimate(Task_item, 
                                                        exp(subset_vars_give_all['cov_fitted']), 
                                                        logistic(subset_vars_give_all['give_all_log_odds_fitted'])))
# sanity check
table(subset_simulation_data_give_all$Response)

scale_factor = obs / (SAMPLE_N * length(unique(subset_data$Task_item)))

# #Overlaid plots of simulated and real subset-knower data for BOTH approximate AND Give-All
###get the relevant data frames together and add a "type" for pretty plots
#approximation simulation
subset_sim_data_approx <- subset_simulation_data_approx %>%
  mutate(type = "Approximation")
#give-all simulation
subset_sim_data_giveall <- subset_simulation_data_give_all %>%
  mutate(type = "Approximation + Give All")
#subset actual data down for give-all dummy
subset_data_dummy_give_all <- subset_data %>%
  mutate(type = "Approximation + Give All")%>%
  select(Task_item, Response, type)
#subset actual data down for approximation dummy
subset_data_dummy_approximation <- subset_data %>%
  mutate(type = "Approximation")%>%
  select(Task_item, Response, type)


## Plotting both data simulations with real data
ggplot(subset_data_dummy_approximation, aes(x = Response)) + 
  geom_vline(aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(aes(y = ..count.., fill = "Subset-knower"), binwidth = 1, color = 'black', 
                 alpha = .9) + #Subset-data first
  geom_histogram(data = subset_sim_data_approx, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black', 
                 binwidth = 1, alpha = .5) + #simulation - approx next
  geom_vline(data = subset_data_dummy_give_all, aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(data = subset_data_dummy_give_all, aes(y = ..count.., fill = 'Subset-knower'), color = 'black', 
                 binwidth = 1, alpha = .9) + #now subset-data for give-all
  geom_histogram(data = subset_sim_data_giveall, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black',
                 binwidth = 1, alpha = .5) + # now simulation data for give-all
  scale_x_continuous(breaks= seq(1, 15, 1)) +
  scale_y_continuous(breaks = seq(0, 40, by = 10)) +
  ylim(c(0, 40)) +
  facet_grid(type~Task_item) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "top") +
  scale_fill_manual(name = "Data type", 
                    values = c('Subset-knower' = "#FF7C00", 'Simulated' = "#827f7d")) +
  labs(y = "Frequency", 
       fill = "legend") + 
  guides(fill = guide_legend(reverse = TRUE))

ggsave('Study 1/Analysis/Figures/subset_simulation_data.png', width = 8, height = 5)

# 5. Fitted CoV analysis: CP ===================================================
# Fit CoV to CP knower data
# Then generate model data with fitted CoV

## Analysis
fit_params_cp_approx = c("logL", "n", "cov_fitted")
priors = list()
# priors[[1]] =  function(x) {-dnorm(x, subset_vars_approx['cov_fitted'], 0.25, log = T)} # priors for cov value in log space
priors[[1]] =  function(x) {-dnorm(x, 0.1, 0.25, log = T)} # priors for cov value in log space
# priors[[1]] = function(x){0} # NB: without prior, this fits really high CoV (~.7)

# Pull out data to fit
cp_data = all.data %>%
  filter(CP_subset == "CP",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10))

# Do MLE fit for CoV
cp_vars_approx = mle_fit_approx(cp_data, fit_params_cp_approx)
cp_vars_approx
exp(cp_vars_approx['cov_fitted'])


k_approx = 1
BIC_approx = -2 * cp_vars_approx['logL'] +
  k_approx * log(length(cp_data$SID))
BIC_approx 

# compare to approx fit for subset
BIC_approx_sub = -2 * subset_vars_approx['logL'] +
  k_approx * log(length(subset_data$SID))
BIC_approx_sub

BIC_approx_sub < BIC_approx ## FALSE; we have a better fit to CP-knower parallel data


# large scale simulation
SAMPLE_N = 10000
obs = length(cp_data$Task_item)
cp_simulation_data_approx = data.frame(
  Task_item = rep(unique(cp_data$Task_item), SAMPLE_N)
)
cp_simulation_data_approx = cp_simulation_data_approx %>%
  rowwise() %>%
  mutate(Response = get_approximate_estimate(Task_item, 
                                                   exp(cp_vars_approx['cov_fitted'])))

scale_factor = obs / (SAMPLE_N * length(unique(cp_data$Task_item)))


##new plot with simulations overlaid with real data 
ggplot(cp_data, aes(x = Response)) + 
  geom_vline(aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(aes(y = ..count.., fill = "CP-knower"), binwidth = 1, color = 'black', 
                 alpha = .9) + #CP-data first
  geom_histogram(data = cp_simulation_data_approx, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black', 
                 binwidth = 1, alpha = .5) + #simulation - approx next
  scale_x_continuous(breaks= seq(1, 15, 1)) +
  scale_y_continuous(breaks = seq(0, 40, by = 10)) +
  ylim(c(0, 40)) +
  facet_grid(~Task_item) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "top") +
  scale_fill_manual(name = "Data type", 
                    values = c('Simulated' = "#827f7d", 'CP-knower' = "#1ECCE3")) +
  labs(y = "Frequency", 
       fill = "legend") 

# 6. Fitted CoV analysis: CP with exact match percent ==========================
# Fit CoV instead of using values manually generated,
# along with percent of people who are doing "exact match".
# Then generate model data with fitted CoV and exact match percent


# log likelihood function: find MLE for CoV and exact match percent
# logistic(match_log_odds) percent of the time, returns the exact value
# (1-logistic(match_log_odds)) percent of the time, returns probability of sampling the subject's response 
# from a normal distribution centered at the prompted value, with sd = prompted value * cov
# TODO clean this up, move logic to separate functions, etc.
loglik_exact_match = function(task_item, subj_resp, cov_val, match_log_odds) {
  sum(
    log(
      # match_pct of the time, subject's value is basically spot on
      (logistic(match_log_odds) * (
        subj_resp == task_item)) +
        # (1 - match_pct) of the time, subject's value is approximation
        (1 - logistic(match_log_odds)) * (
          # this logic copied from loglik_approx above
          ifelse(subj_resp == GIVE_ALL_MAX, 
                 # subject response was maximum: return probability of value >= 15
                 (1 - pnorm(subj_resp - 0.5, mean = task_item, sd = exp(cov_val) * task_item)) /
                   # normalize by probability of response > 0
                   (1 - pnorm(0, mean = task_item, sd = exp(cov_val) * task_item)),
                 # subject response was < maximum
                 (pnorm(subj_resp + 0.5, mean = task_item, sd = exp(cov_val) * task_item) -
                    pnorm(subj_resp - 0.5, mean = task_item, sd = exp(cov_val) * task_item)) /
                   (1 - pnorm(0, mean = task_item, sd = exp(cov_val) * task_item))
          )
        )
    )
  )
}

# fit function
mle_fit_exact_match = function(data, fit_params) {
  nLL = function(cov_fitted, match_log_odds_fitted) {
    -loglik_exact_match(data$Task_item, data$Response, cov_fitted, match_log_odds_fitted) +
      priors[[1]](cov_fitted) +
      priors[[2]](match_log_odds_fitted)
  }
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL,
                           start = list(cov_fitted = log(0.2),
                                        match_log_odds_fitted = logit(0.1)))), # convert starting probability to log odds
        TRUE) 
    iter = iter + 1
    
    if (!is.null(fit)) {
      # m2logL is deviance (-2x LL)
      fits = c(-0.5*fit@m2logL, length(data$Task_item), fit@coef[,"Estimate"])
    } else {
      if (iter > 1000) {
        fits = c(-9999, length(data$Task_item), 0, 0)
      }
    }
  }
  names(fits) = fit_params
  return(fits)
}


# Approximation function: generates integer estimate by drawing from
# normal distribution centered at number with sd = number * CoV
# or by simply returning the exact value match_percent of the time
get_mixture_exact_match_estimate = function(number, CoV, match_percent) {
  if (rbinom(1, 1, match_percent)) {
    # match_percent of the time, return exact match
    return(number)
  } else {
    # sample from normal distribution: return 0 if sample < 0, 15 if sample > 15
    return(
      min(max(round(rnorm(1, number, number * CoV), 0), 1), GIVE_ALL_MAX)
    )
  }
}



# Fit CoV and match percent 
fit_params_cp_exact_match = c("logL", "n", "cov_fitted", "match_log_odds_fitted")
priors = list()
priors[[1]] = function(x) {-dnorm(x, log(0.2), 0.1, log = T)} # priors for cov value in log space
# priors[[1]] = function(x){0}
priors[[2]] =  function(x) {-dnorm(logistic(x), 0.1, 0.25, log = T)} # priors for match pct log odds
# priors[[2]] = function(x){0}

# MLE fit for CoV and exact match percent
# TODO fit this as a percent of subjects rather than responses
cp_vars_exact_match = mle_fit_exact_match(cp_data, fit_params_cp_exact_match)
cp_vars_exact_match
exp(cp_vars_exact_match['cov_fitted'])
logistic(cp_vars_exact_match['match_log_odds_fitted'])

# Compare BICs 
# "In general, BIC penalizes models with more parameters more than AICc does"
# So this is good, we should use BIC to be conservative
# TODO put this somewhere useful
k_exact = 2
BIC_exact_match = -2 * cp_vars_exact_match['logL'] +
  k_exact * log(length(cp_data$SID))
BIC_exact_match

k_approx = 1
BIC_approx = -2 * cp_vars_approx['logL'] +
  k_approx * log(length(cp_data$SID))
BIC_approx 

BIC_exact_match < BIC_approx # If TRUE, yay!




# Simulate responses based on fitted CoV and match percent
SAMPLE_N = 10000
obs = length(cp_data$Task_item)
cp_simulation_data_exact_match = data.frame(
  Task_item = rep(unique(cp_data$Task_item), SAMPLE_N)
)
cp_simulation_data_exact_match = cp_simulation_data_exact_match %>%
  rowwise() %>%
  mutate(Response = get_mixture_exact_match_estimate(Task_item, 
                                                           exp(cp_vars_exact_match['cov_fitted']), 
                                                           logistic(cp_vars_exact_match['match_log_odds_fitted'])))
# sanity check
table(cp_simulation_data_exact_match$Response)

scale_factor = obs / (SAMPLE_N * length(unique(cp_data$Task_item)))

# Overlaid plots of simulated and real CP-knower data 
## For approximation only and approximation + exact match
#rename and plot all together
##adding a type for nice faceting
cp_exact_data <- cp_simulation_data_exact_match %>%
  mutate(type = "Approximation + Exact match")

cp_data_abb <- cp_data %>%
  mutate(type = "Approximation + Exact match")%>%
  dplyr::select(Task_item, Response, type)

cp_sim_data <- cp_simulation_data_approx %>%
  mutate(type = "Approximation")

cp_data_dummy <- cp_data %>%
  mutate(type = "Approximation")%>%
  dplyr::select(Task_item, Response, type)

## CP-knower data
ggplot(cp_data_dummy, aes(x = Response)) + 
  geom_vline(aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(aes(y = ..count.., fill = "CP-knower"), binwidth = 1, color = 'black', 
                 alpha = .9) + #CP-data first
  geom_histogram(data = cp_sim_data, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black', 
                 binwidth = 1, alpha = .5) + #simulation - approx next
  geom_vline(data = cp_data_abb, aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(data = cp_data_abb, aes(y = ..count.., fill = 'CP-knower'), color = 'black', 
                 binwidth = 1, alpha = .9) + #now CP-data for exact
  geom_histogram(data = cp_exact_data, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black', 
                 binwidth = 1, alpha = .5) + # now simulation data for exact
  scale_x_continuous(breaks= seq(1, 15, 1)) +
  scale_y_continuous(breaks = seq(0, 40, by = 10)) +
  ylim(c(0, 40)) +
  facet_grid(type~Task_item) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "top") +
  scale_fill_manual(name = "Data type", 
                    values = c('Simulated' = "#827f7d", 'CP-knower' = "#1ECCE3")) +
  labs(y = "Frequency", 
       fill = "legend") 

ggsave('Study 1/Analysis/Figures/CP_simulation_data.png', width = 8, height = 5)


# 7. Fitted CoV analysis: Subset with exact match percent ======================
# Fit CoV instead of using values manually generated,
# along with percent of people who are doing "exact match".
# Then generate model data with fitted CoV and exact match percent

# Fit CoV and match percent 
fit_params_subset_exact_match = c("logL", "n", "cov_fitted", "match_log_odds_fitted")
priors = list()
priors[[1]] = function(x) {-dnorm(x, log(0.2), 0.1, log = T)} # priors for cov value in log space
# priors[[1]] = function(x){0}
priors[[2]] =  function(x) {-dnorm(logistic(x), 0.1, 0.25, log = T)} # priors for match pct log odds
# priors[[2]] = function(x){0}

# MLE fit for CoV and exact match percent
subset_vars_exact_match = mle_fit_exact_match(subset_data, fit_params_subset_exact_match)
subset_vars_exact_match
exp(subset_vars_exact_match['cov_fitted'])
logistic(subset_vars_exact_match['match_log_odds_fitted'])


# Compare BICs 
# "In general, BIC penalizes models with more parameters more than AICc does"
# So this is good, we should use BIC to be conservative
# TODO put this somewhere useful
k_exact = 2
BIC_exact_match = -2 * subset_vars_exact_match['logL'] +
  k_exact * log(length(subset_data$SID))
BIC_exact_match

k_approx = 1
BIC_approx = -2 * subset_vars_approx['logL'] +
  k_approx * log(length(subset_data$SID))
BIC_approx 

BIC_exact_match < BIC_approx # If FALSE, yay!

# 8. Fitted CoV analysis: CP approximation, orthogonal data ====================
## Analysis
fit_params_cp_approx_orth = c("logL", "n", "cov_fitted")
priors = list()
# priors[[1]] =  function(x) {-dnorm(x, subset_vars_approx['cov_fitted'], 0.25, log = T)} # priors for cov value in log space
priors[[1]] =  function(x) {-dnorm(x, 0.1, 0.25, log = T)} # priors for cov value in log space
# priors[[1]] = function(x){0} # NB: without prior, this fits really high CoV (~.7)

# Pull out data to fit
cp_data_orth = all.data %>%
  filter(CP_subset == "CP",
         Task == "Orthogonal",
         Task_item %in% c(6, 8, 10))

# Do MLE fit for CoV
cp_vars_approx_orth = mle_fit_approx(cp_data_orth, fit_params_cp_approx_orth)
cp_vars_approx_orth
exp(cp_vars_approx_orth['cov_fitted'])


# large scale simulation
SAMPLE_N = 10000
obs = length(cp_data_orth$Task_item)
cp_simulation_data_approx_orth = data.frame(
  Task_item = rep(unique(cp_data_orth$Task_item), SAMPLE_N)
)
cp_simulation_data_approx_orth = cp_simulation_data_approx_orth %>%
  rowwise() %>%
  mutate(Response = get_approximate_estimate(Task_item, 
                                             exp(cp_vars_approx_orth['cov_fitted'])))

scale_factor = obs / (SAMPLE_N * length(unique(cp_data_orth$Task_item)))
# cp_simulation_data_approx_orth %>%
#   ggplot(aes(x = simulation_est)) + # Plot model simulation
#   geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
#   geom_histogram(aes(y = ..count.. * scale_factor),color = 'black', binwidth = 1) +
#   theme_bw(base_size = 18) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#         panel.grid = element_blank()) +
#   facet_grid(~Task_item) +
#   ylim(c(0, 65)) +
#   labs(x = 'Number of items given', y = 'Frequency',
#        title = paste0("Simulated CP data, orthogonal; (fitted) CoV=", round(exp(cp_vars_approx_orth['cov_fitted']), 2)))


## Plot with real data overlaid
ggplot(cp_data_orth, aes(x = Response)) + 
  geom_vline(aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(aes(y = ..count.., fill = "CP-knower"), binwidth = 1, color = 'black', 
                 alpha = .9) + #CP-data first
  geom_histogram(data = cp_simulation_data_approx_orth, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black', 
                 binwidth = 1, alpha = .5) + #simulation - approx next
  # geom_vline(data = cp_data_abb, aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  # geom_histogram(data = cp_data_abb, aes(y = ..count.., fill = 'CP-knower'), color = 'black', 
  #                binwidth = 1, alpha = .9) + #now CP-data for exact
  # geom_histogram(data = cp_exact_data, aes(y = ..count.. * scale_factor, fill = 'Simulated'), color = 'black', 
  #                binwidth = 1, alpha = .5) + # now simulation data for exact
  scale_x_continuous(breaks= seq(1, 15, 1)) +
  facet_grid(~Task_item) +
  scale_y_continuous(breaks = seq(0, 30, by = 10), limits = seq(0, 30, by = 10), labels = seq(0, 30, by = 10)) +
  ylim(c(0, 30)) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "top") +
  scale_fill_manual(name = "Data type", 
                    values = c('Simulated' = "#827f7d", 'CP-knower' = "#1ECCE3")) +
  labs(y = "Frequency", 
       fill = "legend") 



# compare fits
k_approx = 1
BIC_approx = -2 * cp_vars_approx['logL'] +
  k_approx * log(length(cp_data$SID))
BIC_approx 

BIC_approx_orth = -2 * cp_vars_approx_orth['logL'] +
  k_approx * log(length(cp_data_orth$SID))
BIC_approx_orth 

BIC_approx_orth < BIC_approx

