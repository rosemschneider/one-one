# one-to-one simulation

# SETUP ----
rm(list = ls())

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
library(psych) # used for logit and logistic transformations


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

GIVE_ALL_MAX = 15
TASK_ITEMS = sort(unique(all.data$Task_item))

# Basic approximation function: generates integer estimate by drawing from
# normal distribution centered at number with sd = number * CoV
get_approximate_estimate = function(number, CoV) {
  # sample from normal distribution: return 0 if sample < 0, 15 if sample > 15
  min(max(round(rnorm(1, number, number * CoV), 0), 0), GIVE_ALL_MAX)
}


# 1. Manual CoV analysis =======================================================



# baseline values (globals)
LOW_COV_SIM = 0.1
MED_COV_SIM = 0.25
HIGH_COV_SIM = 0.5



# Generate simulated matching data
approximation_sim = all.data %>%
  filter(Task == "Parallel",
         CP_subset == "CP") %>% # this doesn't actually matter, just need to match a condition
  rowwise() %>%
  mutate(approximate_estimate_low = get_approximate_estimate(Task_item, LOW_COV_SIM),
         approximate_estimate_med = get_approximate_estimate(Task_item, MED_COV_SIM),
         approximate_estimate_high = get_approximate_estimate(Task_item, HIGH_COV_SIM))%>%
  pivot_longer(cols = c(approximate_estimate_low, approximate_estimate_med,
                        approximate_estimate_high), 
               names_to = "approximation_level", 
               values_to = "approximation_response")%>%
  mutate(COV_level = ifelse(approximation_level == "approximate_estimate_low", "0.1", 
                            ifelse(approximation_level == "approximate_estimate_med", "0.25", "0.5")))
# sanity check
table(approximation_sim$approximation_response) 


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
    # xlim(c(1, 15)) +
    labs(x = 'Number of items given', y = 'Frequency',
         # title = paste0(as.character(task), " Task"))
         title = title)
  print(p)
}

# Plot results
make_dist_plot(approximation_sim, "Parallel", TASK_ITEMS, "Simulated approx. data: Low, med., high COV")



# 1.5. Modify simulations above to include exact match
# Globals for CP simulation
COV_CP = 0.3 # CoV to use for this population
EXACT_MATCH_PROP = 0.25 # Proportion *of subjects* using exact match

# Simulate the data
approximation_sim_exact_match = all.data %>%
  filter(Task == "Parallel",
         CP_subset == "CP") %>%
  group_by(SID) %>%
  mutate(category = ifelse(rbinom(1, 1, EXACT_MATCH_PROP),
                           "exact", "approximate")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(simulation_est = ifelse(
    category == "exact", Task_item,
    get_approximate_estimate(Task_item, COV_CP)
  ))
# sanity check
table(approximation_sim_exact_match$simulation_est) 

# Plot
approximation_sim_exact_match %>%
  ggplot(aes(x = simulation_est)) + # Plot model simulation
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  facet_grid(~Task_item) +
  ylim(c(0, 65)) + # TODO set a fixed height for this plot and empirical plots
  labs(x = 'Number of items given', y = 'Frequency',
       title = paste0("Simulated CP data, CoV=", round(COV_CP, 2), ", Match pct ", EXACT_MATCH_PROP))
  

# Globals for Subset simulation
COV_SUB = 0.3
# Pull out data we want
subset_data = all.data %>%
  filter(CP_subset == "Subset",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10)) %>%
  mutate(is_max = Response == GIVE_ALL_MAX)
# What percent *of subjects* are maximizing? (best estimate)
give_all_prop = subset_data %>%
  #group_by(Task_item) %>%
  summarize(participants = n(),
            maximizers = sum(is_max),
            max_pct = maximizers / participants) %>%
  select(max_pct)

# Simulate the data
approximation_sim_give_all = all.data %>%
  filter(Task == "Parallel",
         CP_subset == "Subset") %>%
  group_by(SID) %>%
  mutate(category = ifelse(rbinom(1, 1, give_all_prop$max_pct),
                           "give-all", "approximate")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(simulation_est = ifelse(
    category == "give-all", GIVE_ALL_MAX,
    get_approximate_estimate(Task_item, COV_SUB)
  ))

table(approximation_sim_give_all$category) # sanity check
table(approximation_sim_give_all$simulation_est) # sanity check

# Plot
approximation_sim_give_all %>%
  ggplot(aes(x = simulation_est)) + # Plot model simulation
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  facet_grid(~Task_item) +
  ylim(c(0, 65)) +
  labs(x = 'Number of items given', y = 'Frequency',
       title = paste0("Simulated Subset data, CoV=", round(COV_SUB, 2), ", Give-all pct ", round(give_all_prop, 2)))



# 2. Fitted CoV analysis: Subset ===============================================
# Fit CoV to subset knower data
# Then generate model data with fitted CoV


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
         # compare to fitting 3, 4 as well
         # Task_item %in% c(3, 4, 6, 8, 10))

# Do MLE fit for CoV
subset_vars_approx = mle_fit_approx(subset_data, fit_params_subset_approx)
subset_vars_approx
exp(subset_vars_approx['cov_fitted'])

# simulate
# TODO these simulations seem highly variable, maybe we do 1000s of runs?
# subset_data = subset_data %>%
#   rowwise() %>%
#   mutate(simulation_est = get_approximate_estimate(Task_item, 
#                                                    exp(subset_vars['cov_fitted'])))
# # sanity check
# table(subset_data$simulation_est)
# # plot simulated data
# subset_data %>%
#   ggplot(aes(x = simulation_est)) + # Plot model simulation
#   geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
#   geom_histogram(color = 'black', binwidth = 1) +
#   theme_bw(base_size = 18) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#         panel.grid = element_blank()) +
#   facet_grid(~Task_item) +
#   ylim(c(0, 65)) +
#   labs(x = 'Number of items given', y = 'Frequency',
#        title = paste0("Simulated Subset data, (fitted) CoV=", round(exp(subset_vars['cov_fitted']), 2)))
# 


# large scale simulation
SAMPLE_N = 10000
obs = length(subset_data$Task_item)
subset_simulation_data_approx = data.frame(
  Task_item = rep(unique(subset_data$Task_item), SAMPLE_N)
)
subset_simulation_data_approx = subset_simulation_data_approx %>%
  rowwise() %>%
  mutate(simulation_est = get_approximate_estimate(Task_item, 
                                                   exp(subset_vars_approx['cov_fitted'])))

scale_factor = obs / (SAMPLE_N * length(unique(subset_data$Task_item)))

###old plot of simulations only
subset_simulation_data_approx %>%
  ggplot(aes(x = simulation_est)) + # Plot model simulation
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(aes(y = ..count.. * scale_factor),color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  facet_grid(~Task_item) +
  ylim(c(0, 65)) +
  labs(x = 'Number of items given', y = 'Frequency',
       title = paste0("Simulated Subset data, (fitted) CoV=", round(exp(subset_vars_approx['cov_fitted']), 2)))



# 2.5 Fitted CoV analysis with log normal: Subset ==============================

# NB: this seems not to work because it's not able to exponentiate (log) CoV
# during the fitting process


# log likelihood function: find MLE for CoV
# Returns probability of sampling the subject's response 
# from a log normal distribution centered at the prompted value, with sd = log(cov)
# loglik_subset_lnorm = function(task_item, subj_resp, cov_val) {
#   sum(
#     ifelse(subj_resp == GIVE_ALL_MAX, 
#            # subject response was maximum: return probability of value >= 15
#            log(
#              1 - plnorm(subj_resp, meanlog = log(task_item), sdlog = cov_val)
#            ),
#            # subject response was < maximum
#            log(
#              plnorm(subj_resp + 0.5, meanlog = log(task_item), sdlog = cov_val, log.p = T) -
#                plnorm(subj_resp - 0.5, meanlog = log(task_item), sdlog = cov_val, log.p = T)
#            ))
#   )
# }
# 
# 
# # fit function
# mle_fit_subset_lnorm = function(data, fit_params) {
#   nLL = function(cov_fitted) {
#     -loglik_subset_lnorm(data$Task_item, data$Response, cov_fitted) +
#       priors_lnorm[[1]](cov_fitted)
#   }
#   iter = 0
#   fits = NULL
#   fit = NULL
#   while (is.null(fits)) {
#     try(fit <- summary(mle(nLL,
#                            # start = list(cov_fitted = 0.2))),
#                            start = list(cov_fitted = log(0.2)))), # log CoV
#         TRUE) 
#     iter = iter + 1
#     
#     if (!is.null(fit)) {
#       # m2logL is deviance (-2x LL)
#       fits = c(-0.5*fit@m2logL, length(data$Task_item), fit@coef[,"Estimate"])
#     } else {
#       if (iter > 500) {
#         fits = c(-9999, 0, 0)
#       }
#     }
#   }
#   names(fits) = fit_params
#   return(fits)
# }



## Analysis
# fit_params_subset_lnorm = c("logL", "n", "cov_fitted")
# priors_lnorm = list()
# priors_lnorm[[1]] =  function(x) {-dlnorm(x, 0.2, 0.1, log = T)} # priors for cov value
# priors_lnorm[[1]] =  function(x) {-dlnorm(x, log(0.2), 0.1, log = T)} # priors for cov value in log space
# priors_lnorm[[1]] = function(x){0}

# Pull out data to fit
# subset_data = all.data %>%
#   filter(CP_subset == "Subset",
#          Task == "Parallel",
#          Task_item %in% c(6, 8, 10))
# compare to fitting 3, 4 as well
# Task_item %in% c(3, 4, 6, 8, 10))

# Do MLE fit for CoV from log normal
# subset_vars_lnorm = mle_fit_subset_lnorm(subset_data, fit_params_subset_lnorm)
# subset_vars_lnorm


#' Options
#' 1. complicated mixture model, different things happening at different numbers (15, CoV)
#' -  may want to do log normal anyway to avoid these things
#' 2. shape of dist. isn't Gaussian and has heavier tails: t distribution
#' - error distributed according to t
#' or do log t
#' hist(pmin(15, pmax(0, round(exp(rt(1000, 100)*0.2 + log(3)), 0))), breaks = seq(-0.5, 15.5, by=1))
#' 


# 3. Fitted CoV analysis: CP ===================================================
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
         # compare to fitting 3, 4 as well
         # Task_item %in% c(3, 4, 6, 8, 10))

# Do MLE fit for CoV
cp_vars_approx = mle_fit_approx(cp_data, fit_params_cp_approx)
cp_vars_approx
exp(cp_vars_approx['cov_fitted'])


# simulate
# TODO these simulations seem highly variable, maybe we do 1000s of runs?
# cp_data = subset_data %>%
#   rowwise() %>%
#   mutate(simulation_est = get_approximate_estimate(Task_item,
#                                                    exp(cp_vars['cov_fitted'])))

# sanity check
# table(cp_data$simulation_est)
# plot simulated data
# cp_data %>%
#   ggplot(aes(x = simulation_est)) + # Plot model simulation
#   geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
#   geom_histogram(color = 'black', binwidth = 1) +
#   theme_bw(base_size = 18) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#         panel.grid = element_blank()) +
#   facet_grid(~Task_item) +
#   ylim(c(0, 65)) +
#   labs(x = 'Number of items given', y = 'Frequency',
#        title = paste0("Simulated Subset data, (fitted) CoV=", round(exp(cp_vars['cov_fitted']), 2)))



# large scale simulation
SAMPLE_N = 10000
obs = length(cp_data$Task_item)
cp_simulation_data_approx = data.frame(
  Task_item = rep(unique(cp_data$Task_item), SAMPLE_N)
)
cp_simulation_data_approx = cp_simulation_data_approx %>%
  rowwise() %>%
  mutate(simulation_est = get_approximate_estimate(Task_item, 
                                                   exp(cp_vars_approx['cov_fitted'])))

scale_factor = obs / (SAMPLE_N * length(unique(cp_data$Task_item)))
cp_simulation_data_approx %>%
  ggplot(aes(x = simulation_est)) + # Plot model simulation
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(aes(y = ..count.. * scale_factor),color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  facet_grid(~Task_item) +
  ylim(c(0, 65)) +
  labs(x = 'Number of items given', y = 'Frequency',
       title = paste0("Simulated CP data, (fitted) CoV=", round(exp(cp_vars_approx['cov_fitted']), 2)))


##new plot with simulations overlaid with real data -- first do approx data only
cp_sim_data <- cp_simulation_data_approx %>%
  dplyr::rename('Response' = 'simulation_est')




# 4. Fitted CoV analysis: CP with exact match percent ==========================
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
      min(max(round(rnorm(1, number, number * CoV), 0), 0), GIVE_ALL_MAX)
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

# Pull out data to fit
cp_data = all.data %>%
  filter(CP_subset == "CP",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10))
         # check fit with all values
         # Task_item %in% c(3, 4, 6, 8, 10))

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
  mutate(simulation_est = get_mixture_exact_match_estimate(Task_item, 
                                                           exp(cp_vars_exact_match['cov_fitted']), 
                                                           logistic(cp_vars_exact_match['match_log_odds_fitted'])))
# sanity check
table(cp_simulation_data_exact_match$simulation_est)

scale_factor = obs / (SAMPLE_N * length(unique(cp_data$Task_item)))

# 4.1 Overlaid plots of simulated and real CP-knower data ==========================
#rename and plot all together
##adding a type for nice faceting
cp_exact_data <- cp_simulation_data_exact_match %>%
  dplyr::rename('Response' = 'simulation_est') %>%
  mutate(type = "Approximation + Exact match")

cp_data_abb <- cp_data %>%
  mutate(type = "Approximation + Exact match")%>%
  dplyr::select(Task_item, Response, type)

cp_sim_data <- cp_sim_data %>%
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
  facet_grid(type~Task_item) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "right") +
  scale_fill_manual(name = "Data type", 
                    values = c('Simulated' = "#827f7d", 'CP-knower' = "#1ECCE3")) +
  labs(y = "Frequency", 
       fill = "legend") 

ggsave('Study 1/Analysis/Figures/CP_simulation_data.png', width = 8)
  
###old plot - nice new shiny plot is above
# cp_simulation_data_exact_match %>%
#   ggplot(aes(x = simulation_est)) + # Plot model simulation
#   geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
#   geom_histogram(aes(y = ..count.. * scale_factor), color = 'black', binwidth = 1) +
#   theme_bw(base_size = 18) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#         panel.grid = element_blank()) +
#   facet_grid(~Task_item) +
#   ylim(c(0, 65)) +
#   labs(x = 'Number of items given', y = 'Frequency',
#        title = paste0("Simulated CP data, (fitted) CoV=", 
#                       round(exp(cp_vars_exact_match['cov_fitted']), 2),
#                       ", (fitted) match pct.=", 
#                       round(logistic(cp_vars_exact_match['match_log_odds_fitted']), 2)))



# 5. Fitted CoV analysis: Subset with "give-all" percent =======================
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
      min(max(round(rnorm(1, number, number * CoV), 0), 0), GIVE_ALL_MAX)
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

# Pull out data to fit
subset_data = all.data %>%
  filter(CP_subset == "Subset",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10))
# check fit with all values
# Task_item %in% c(3, 4, 6, 8, 10))

# MLE fit for CoV and give-all percent
# TODO fit this as a percent of subjects rather than responses
subset_vars_give_all = mle_fit_give_all(subset_data, fit_params_give_all)
subset_vars_give_all
exp(subset_vars_give_all['cov_fitted'])
logistic(subset_vars_give_all['give_all_log_odds_fitted'])


# Simulate responses based on fitted CoV and give-all percent
SAMPLE_N = 10000
obs = length(subset_data$Task_item)
subset_simulation_data_give_all = data.frame(
  Task_item = rep(unique(subset_data$Task_item), SAMPLE_N)
)
subset_simulation_data_give_all = subset_simulation_data_give_all %>%
  rowwise() %>%
  mutate(simulation_est = get_mixture_give_all_estimate(Task_item, 
                                                        exp(subset_vars_give_all['cov_fitted']), 
                                                        logistic(subset_vars_give_all['give_all_log_odds_fitted'])))
# sanity check
table(subset_simulation_data_give_all$simulation_est)

scale_factor = obs / (SAMPLE_N * length(unique(subset_data$Task_item)))

# 5.1 Overlaid plots of simulated and real subset-knower data =======================
###get the relevant data frames together and add a "type" for pretty plots
#approximation simulation
subset_sim_data_approx <- subset_simulation_data_approx %>%
  dplyr::rename('Response' = 'simulation_est')%>%
  mutate(type = "Approximation")
#give-all simulation
subset_sim_data_giveall <- subset_simulation_data_give_all %>%
  dplyr::rename("Response" = "simulation_est")%>%
  mutate(type = "Approximation + Give All")
#subset actual data down for give-all dummy
subset_data_dummy_give_all <- subset_data %>%
  mutate(type = "Approximation + Give All")%>%
  select(Task_item, Response, type)
#subset actual data down for approximation dummy
subset_data_dummy_approximation <- subset_data %>%
  mutate(type = "Approximation")%>%
  select(Task_item, Response, type)


## subset-knower data
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
  facet_grid(type~Task_item) +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "right") +
  scale_fill_manual(name = "Data type", 
                    values = c('Subset-knower' = "#FF7C00", 'Simulated' = "#827f7d")) +
  labs(y = "Frequency", 
       fill = "legend") + 
  guides(fill = guide_legend(reverse = TRUE))

ggsave('Study 1/Analysis/Figures/subset_simulation_data.png', width = 8)




##this is the old plotting code!
# subset_simulation_data_give_all %>%
#   ggplot(aes(x = simulation_est)) + # Plot model simulation
#   geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
#   geom_histogram(aes(y = ..count.. * scale_factor), color = 'black', binwidth = 1) +
#   theme_bw(base_size = 18) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#         panel.grid = element_blank()) +
#   facet_grid(~Task_item) +
#   ylim(c(0, 65)) +
#   labs(x = 'Number of items given', y = 'Frequency',
#        title = paste0("Simulated Subset data, (fitted) CoV=", 
#                       round(exp(subset_vars_give_all['cov_fitted']), 2),
#                       ", (fitted) give-all pct.=", 
#                       round(logistic(subset_vars_give_all['give_all_log_odds_fitted']), 2)))




# 6. Fitted CoV analysis: CP with "exact match" percent by subject =============
# Fit CoV and exact match percent, but by subject instead of treating trials
# as independent


get_estimate_aggregate = function(subj_data, cov_val) {
  return_prob = 1
  for (trial in subj_data$Trial_number) {
    task_item = subj_data$Task_item[subj_data$Trial_number == trial]
    subj_resp = subj_data$Response[subj_data$Trial_number == trial]
    return_prob = return_prob * (
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
  }
  # print(return_prob)
  return(return_prob)
}

# log likelihood function: MLE for CoV and exact match percent
# logistic(match_log_odds) percent of the time, returns the exact value 
# for *all of a subject's trials* 
# (1-logistic(match_log_odds)) percent of the time, returns probability of sampling the subject's responses 
# from a normal distribution centered at the prompted value, with sd = prompted value * cov
# TODO clean this up, move logic to separate functions, etc.
loglik_exact_match_subj = function(data, cov_val, match_log_odds) {
  ll_sum = 0
  for (subj in unique(data$SID)) {
    # print(ll_sum)
    subj_data = data %>%
      filter(SID == subj)
    ll_sum = ll_sum +
      log(
        # match_pct of the time, subject's values are all accurate
        (logistic(match_log_odds) * (
          sum(subj_data$Correct) == length(subj_data$Correct))) +
        # (1 - match_pct) of the time, subject's values are approximation
        (1 - logistic(match_log_odds)) * (
          get_estimate_aggregate(subj_data, cov_val)
        )
      )
    # print(ll_sum)
  }
  return(ll_sum)
}

# fit function
mle_fit_exact_match_subj = function(data, fit_params) {
  nLL = function(cov_fitted, match_log_odds_fitted) {
    -loglik_exact_match_subj(data, cov_fitted, match_log_odds_fitted) +
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
get_mixture_exact_match_subj_estimate = function(number_set, CoV, match_percent) {
  if (rbinom(1, 1, match_percent)) {
    # match_percent of the time, return exact match
    return(number_set)
  } else {
    # sample from normal distribution: return 0 if sample < 0, 15 if sample > 15
    estimates = c()
    for (num in number_set) {
      estimates = c(estimates, 
                    min(max(round(rnorm(1, num, num * CoV), 0), 0), GIVE_ALL_MAX))
    }
    return(estimates)
  }
}



# Fit CoV and match percent 
fit_params_cp_exact_match_subject = c("logL", "n", "cov_fitted", "match_log_odds_fitted")
priors = list()
priors[[1]] = function(x) {-dnorm(x, log(0.2), 0.1, log = T)} # priors for cov value in log space
# priors[[1]] = function(x){0}
priors[[2]] =  function(x) {-dnorm(logistic(x), 0.1, 0.25, log = T)} # priors for match pct log odds
# priors[[2]] = function(x){0}

# Pull out data to fit
cp_data = all.data %>%
  filter(CP_subset == "CP",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10)) %>%
  select(SID, Trial_number, Task_item, Response, Correct) # NB: this is mostly for debugging
# check fit with all values
# Task_item %in% c(3, 4, 6, 8, 10))

length(unique(cp_data$SID)) # 70 participants
cp_data %>% # only 9 got all 3 correct (13%)
  group_by(SID) %>%
  summarize(correct_tot = sum(Correct)) %>%
  filter(correct_tot == 3)

# MLE fit for CoV and exact match percent
options("tidylog.display" = list())
cp_vars_exact_match_subj = mle_fit_exact_match_subj(cp_data, fit_params_cp_exact_match_subject)
cp_vars_exact_match_subj
exp(cp_vars_exact_match_subj['cov_fitted'])
logistic(cp_vars_exact_match_subj['match_log_odds_fitted'])



# Simulate responses based on fitted CoV and match percent
SAMPLE_N = 10000
obs = length(cp_data$Task_item)
cp_simulation_data_exact_match_subj = data.frame(
  SID = rep(seq(1, SAMPLE_N), length(unique(cp_data$Task_item)))
)
cp_simulation_data_exact_match_subj = cp_simulation_data_exact_match_subj %>%
  group_by(SID) %>%
  mutate(Task_item = unique(cp_data$Task_item))

cp_simulation_data_exact_match_subj = cp_simulation_data_exact_match_subj %>%
  group_by(SID) %>%
  mutate(simulation_est = get_mixture_exact_match_subj_estimate(Task_item, 
                                                                exp(cp_vars_exact_match_subj['cov_fitted']),
                                                                logistic(cp_vars_exact_match_subj['match_log_odds_fitted'])))
# sanity check
table(cp_simulation_data_exact_match_subj$simulation_est)

scale_factor = obs / (SAMPLE_N * length(unique(cp_data$Task_item)))
cp_simulation_data_exact_match_subj %>%
  ggplot(aes(x = simulation_est)) + # Plot model simulation
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(aes(y = ..count.. * scale_factor), color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  facet_grid(~Task_item) +
  ylim(c(0, 65)) +
  labs(x = 'Number of items given', y = 'Frequency',
       title = paste0("Simulated CP data, (fitted) CoV=",
                      round(exp(cp_vars_exact_match_subj['cov_fitted']), 2),
                      ", (fitted) match pct.=",
                      round(logistic(cp_vars_exact_match_subj['match_log_odds_fitted']), 2)))

k_exact_subj = 2
BIC_exact_subj = -2 * cp_vars_exact_match_subj['logL'] +
  k_exact_subj * log(length(cp_data$SID))

# Compare
BIC_exact_subj
BIC_exact_match 
BIC_approx 




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

# Pull out data to fit
subset_data = all.data %>%
  filter(CP_subset == "Subset",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10))
# check fit with all values
# Task_item %in% c(3, 4, 6, 8, 10))

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
# compare to fitting 3, 4 as well
# Task_item %in% c(3, 4, 6, 8, 10))

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
  mutate(simulation_est = get_approximate_estimate(Task_item, 
                                                   exp(cp_vars_approx_orth['cov_fitted'])))

scale_factor = obs / (SAMPLE_N * length(unique(cp_data_orth$Task_item)))
cp_simulation_data_approx_orth %>%
  ggplot(aes(x = simulation_est)) + # Plot model simulation
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(aes(y = ..count.. * scale_factor),color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  facet_grid(~Task_item) +
  ylim(c(0, 65)) +
  labs(x = 'Number of items given', y = 'Frequency',
       title = paste0("Simulated CP data, orthogonal; (fitted) CoV=", round(exp(cp_vars_approx_orth['cov_fitted']), 2)))


