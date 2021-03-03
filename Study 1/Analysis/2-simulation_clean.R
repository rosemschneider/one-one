# One-to-one data simulation
# Erik Brockbank and Rose Schneider


#
# SETUP ========================================================================
#
rm(list = ls())
source("Study 1/Analysis/0-clean.R") # data cleaning script, produces cleaned data
# Load cleaned data
load("Study 1/Data/one-one_cleaned.RData") # study 1 data
library(stats4)
library(psych) # used for logit and logistic transformations



#
# GLOBAL VARIABLES =============================================================
#
GIVE_ALL_MAX = 15 # maximum value participants can provide
SAMPLE_N = 10000 # number of sample "participants" to use in simulation data

theme_set(theme_bw() + theme(text = element_text(size = 12),
                             axis.title=element_text(size = 11),
                             strip.text = element_text(margin=margin(2, 0, 2, 0)),
                             panel.grid = element_blank()))

#
# ANALYSIS FUNCTIONS ===========================================================
#

# Utility function to calculate BIC 
# uses k parameters, n observations, log likelihood ll
get_BIC = function(ll, k, n) {
  (k * log(n)) - (2 * ll)
}


# Simulation functions ----

# Approximation function: generates integer estimate by drawing from
# normal distribution centered at number with sd = number * CoV
get_approximate_estimate = function(number, CoV) {
  # sample from normal distribution: return 0 if sample < 0, 15 if sample > 15
  min(max(round(rnorm(1, number, number * CoV), 0), 1), GIVE_ALL_MAX)
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

# Approximation function: generates integer estimate by drawing from
# normal distribution centered at number with sd = number * CoV or
# by simply returning the exact value match_percent of the time
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


# Log likelihood functions ----

# log likelihood function for calculating fitted CoV
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

# log likelihood function for calculating fitted CoV and "give-all percent"
# logistic(give_all_log_odds) percent of the time, returns maximum value ("give all")
# (1-logistic(give_all_log_odds)) percent of the time, returns probability of sampling the subject's response 
# from a normal distribution centered at the prompted value, with sd = prompted value * cov ("approximation")
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

# log likelihood function for calculating fitted CoV and "exact match percent"
# logistic(match_log_odds) percent of the time, returns the exact value ("exact match")
# (1-logistic(match_log_odds)) percent of the time, returns probability of sampling the subject's response 
# from a normal distribution centered at the prompted value, with sd = prompted value * cov ("approximation")
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


# MLE functions ----

# MLE function for fitting a CoV to participant data
mle_fit_approx = function(data, starting_vals, priors) {
  nLL = function(cov_fitted) {
    -loglik_approx(data$Task_item, data$Response, cov_fitted) +
      priors[[1]](cov_fitted)
  }
  iter = 0
  fits = NULL
  fit = NULL
  while (is.null(fits)) {
    try(fit <- summary(mle(nLL,
                           start = starting_vals)),
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
  names(fits) = c("logL", "n", "cov_fitted")
  return(fits)
}


# MLE function for fitting a CoV and "give-all percent" to participant data
mle_fit_give_all = function(data, starting_vals, priors) {
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
                           start = starting_vals)), 
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
  names(fits) = c("logL", "n", "cov_fitted", "give_all_log_odds_fitted")
  return(fits)
}


# MLE function for fitting a CoV and "exact match percent" to participant data
mle_fit_exact_match = function(data, starting_vals, priors) {
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
                           start = starting_vals)),
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
  names(fits) = c("logL", "n", "cov_fitted", "match_log_odds_fitted")
  return(fits)
}


#
# DATA PROCESSING ==============================================================
#
all.data %<>%
  # remove extra rows from non one-one tasks
  filter(Task == "Parallel" | Task == "Orthogonal") %>%
  mutate(Age = as.numeric(as.character(Age)),
         Task_item = as.numeric(as.character(Task_item)),
         Response = as.numeric(as.character(Response)))

subset_data = all.data %>%
  filter(CP_subset == "Subset",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10))

cp_data = all.data %>%
  filter(CP_subset == "CP",
         Task == "Parallel",
         Task_item %in% c(6, 8, 10))


#
# ANALYSIS: Subset, approximation ==============================================
#

# This fits a CoV to subset knower data then generates simulated data with fitted CoV
# Initial vals and priors
subset_approximation_init = list(cov_fitted = log(0.3))
subset_approximation_priors = list(function(x) {-dnorm(x, log(0.3), 0.1, log = T)})

# MLE fit for CoV
subset_vars_approx = mle_fit_approx(subset_data, 
                                    subset_approximation_init, 
                                    subset_approximation_priors)
# Sanity check fitted values
subset_vars_approx
exp(subset_vars_approx['cov_fitted']) # fits a COV of around .56

# Simulate responses based on fitted CoV
subset_simulation_data_approx = data.frame(
  Task_item = rep(unique(subset_data$Task_item), SAMPLE_N)
)
subset_simulation_data_approx = subset_simulation_data_approx %>%
  rowwise() %>%
  mutate(Response = get_approximate_estimate(Task_item, 
                                                   exp(subset_vars_approx['cov_fitted'])))


# PLOT: Subset, approximation ----

scale_factor = length(subset_data$Task_item) / 
  (SAMPLE_N * length(unique(subset_data$Task_item)))

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


#
# ANALYSIS: Subset, "give-all" =================================================
#

# This fits a CoV and "give-all percent" to subset knower data then generates 
# simulated data with fitted CoV and give-all percent

# Initial vals and priors
# NB: uses same initial CoV vals and priors as subset fitting above
subset_give_all_init = list(cov_fitted = log(0.3),
                            give_all_log_odds_fitted = logit(0.25))
subset_give_all_priors = list(function(x) {-dnorm(x, log(0.3), 0.1, log = T)}, # priors for cov value in log space
                              function(x) {-dnorm(logistic(x), 0.25, 0.1, log = T)}) # priors for give-all pct log odds

# MLE fit for CoV and give-all percent
subset_vars_give_all = mle_fit_give_all(subset_data, 
                                        subset_give_all_init, 
                                        subset_give_all_priors)
# Sanity check fitted values
subset_vars_give_all
exp(subset_vars_give_all['cov_fitted'])
logistic(subset_vars_give_all['give_all_log_odds_fitted'])

# Simulate responses based on fitted CoV and give-all percent
subset_simulation_data_give_all = data.frame(
  Task_item = rep(unique(subset_data$Task_item), SAMPLE_N)
)
subset_simulation_data_give_all = subset_simulation_data_give_all %>%
  rowwise() %>%
  mutate(Response = get_mixture_give_all_estimate(Task_item, 
                                                  exp(subset_vars_give_all['cov_fitted']), 
                                                  logistic(subset_vars_give_all['give_all_log_odds_fitted'])))

# PLOT: Subset, model comparison ----

# Overlaid plots of simulated and real subset-knower data for BOTH approximate AND Give-All
# Get the relevant data frames together and add a "type" for pretty plots
# approximation simulation
subset_sim_data_approx <- subset_simulation_data_approx %>%
  mutate(type = "Approximation")
# give-all simulation
subset_sim_data_giveall <- subset_simulation_data_give_all %>%
  mutate(type = "Approximation + Give All")
# subset actual data down for give-all dummy
subset_data_dummy_give_all <- subset_data %>%
  mutate(type = "Approximation + Give All")%>%
  select(Task_item, Response, type)
# subset actual data down for approximation dummy
subset_data_dummy_approximation <- subset_data %>%
  mutate(type = "Approximation")%>%
  select(Task_item, Response, type)

# Plot both data simulations with real data
scale_factor = length(subset_data$Task_item) / 
  (SAMPLE_N * length(unique(subset_data$Task_item)))

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


# MODEL COMPARISON: Subset approximation, give-all ----

# Compare BIC for subset approximation model and subset "give-all" model
BIC_subset_approx = get_BIC(subset_vars_approx['logL'],
                            k = 1, n = length(subset_data$SID))
BIC_subset_give_all = get_BIC(subset_vars_give_all['logL'],
                              k = 2, n = length(subset_data$SID))

# give-all is a slightly better fit than approx.
# even after correcting for add'l params
BIC_subset_approx
BIC_subset_give_all
BIC_subset_give_all < BIC_subset_approx 


#
# ANALYSIS: CP, approximation ==================================================
#

# This fits a CoV to CP knower data then generates simulated data with fitted CoV

# Initial vals and priors
# NB: uses same initial vals and priors as subset fitting above
cp_approximation_init = list(cov_fitted = log(0.3))
cp_approximation_priors = list(function(x) {-dnorm(x, log(0.3), 0.1, log = T)})

# MLE fit for CoV
cp_vars_approx = mle_fit_approx(cp_data, cp_approximation_init, cp_approximation_priors)
# Sanity check fitted values
cp_vars_approx
exp(cp_vars_approx['cov_fitted'])

# Simulate responses based on fitted CoV
cp_simulation_data_approx = data.frame(
  Task_item = rep(unique(cp_data$Task_item), SAMPLE_N)
)
cp_simulation_data_approx = cp_simulation_data_approx %>%
  rowwise() %>%
  mutate(Response = get_approximate_estimate(Task_item, 
                                             exp(cp_vars_approx['cov_fitted'])))


# PLOT: CP, approximation ----

scale_factor = length(cp_data$Task_item) / 
  (SAMPLE_N * length(unique(cp_data$Task_item)))

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


#
# ANALYSIS: CP, "exact-match" ==================================================
#

# This fits a CoV and "exact match percent" to CP knower data then generates 
# simulated data with fitted CoV and exact match percent

# Initial vals and priors
# NB: uses same initial CoV vals and priors as subset fitting above
cp_exact_match_init = list(cov_fitted = log(0.3),
                           match_log_odds_fitted = logit(0.1))
cp_exact_match_priors = list(function(x) {-dnorm(x, log(0.3), 0.1, log = T)}, # priors for cov value in log space
                             function(x) {-dnorm(logistic(x), 0.1, 0.25, log = T)}) # priors for match pct log odds

# MLE fit for CoV and exact match percent
cp_vars_exact_match = mle_fit_exact_match(cp_data, cp_exact_match_init, cp_exact_match_priors)
# Sanity check fitted values
cp_vars_exact_match
exp(cp_vars_exact_match['cov_fitted'])
logistic(cp_vars_exact_match['match_log_odds_fitted'])

# Simulate responses based on fitted CoV and exact match percent
cp_simulation_data_exact_match = data.frame(
  Task_item = rep(unique(cp_data$Task_item), SAMPLE_N)
)
cp_simulation_data_exact_match = cp_simulation_data_exact_match %>%
  rowwise() %>%
  mutate(Response = get_mixture_exact_match_estimate(Task_item, 
                                                     exp(cp_vars_exact_match['cov_fitted']), 
                                                     logistic(cp_vars_exact_match['match_log_odds_fitted'])))

# PLOT: CP, model comparison ----

# Overlaid plots of simulated and real CP-knower data 
# For approximation only and approximation + exact match 
# Rename and plot all together, adding a type for nice faceting
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

# Plot both data simulations with real data
scale_factor = length(cp_data$Task_item) / 
  (SAMPLE_N * length(unique(cp_data$Task_item)))

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


# MODEL COMPARISON: CP approximation, exact match ----

# Compare BIC for CP approximation model and CP "exact match" model
BIC_cp_approx = get_BIC(cp_vars_approx['logL'],
                        k = 1, n = length(cp_data$SID))
BIC_cp_exact_match = get_BIC(cp_vars_exact_match['logL'],
                             k = 2, n = length(cp_data$SID))

# exact match is a slightly better fit than approx.
# even after correcting for add'l params
BIC_cp_approx
BIC_cp_exact_match
BIC_cp_exact_match < BIC_cp_approx 



#
# ANALYSIS: Subset, "exact-match" ==============================================
#

# This fits a CoV and "exact match percent" to *subset* knower data then 
# generates simulated data with fitted CoV and exact match percent. This way we 
# can see whether subset knowers get any improvement from the exact match model

# Initial vals and priors
# NB: uses same initial vals and priors as CP exact match fitting above
subset_exact_match_init = list(cov_fitted = log(0.3),
                               match_log_odds_fitted = logit(0.1))
subset_exact_match_priors = list(function(x) {-dnorm(x, log(0.3), 0.1, log = T)}, # priors for cov value in log space
                                 function(x) {-dnorm(logistic(x), 0.1, 0.25, log = T)}) # priors for match pct log odds

# MLE fit for CoV and exact match percent
subset_vars_exact_match = mle_fit_exact_match(subset_data, 
                                              subset_exact_match_init, 
                                              subset_exact_match_priors)
# Sanity check fitted values
subset_vars_exact_match
exp(subset_vars_exact_match['cov_fitted'])
logistic(subset_vars_exact_match['match_log_odds_fitted'])


# MODEL COMPARISON: Subset approximation, exact match ----

# Compare BIC for subset "exact match" model to earlier subset approximation model
BIC_subset_exact_match = get_BIC(subset_vars_exact_match['logL'],
                                 k = 2, n = length(subset_data$SID))

# approximation model is a better fit than exact match applied to subset data
# after correcting for add'l params (they're basically equivalent)
BIC_subset_approx
BIC_subset_exact_match
BIC_subset_approx < BIC_subset_exact_match


#
# APPENDIX ANALYSIS: CP orthogonal =============================================
#

# This fits approximation and exact match models to the CP knowers orthogonal
# task data, to show that CP knowers behave like subset knowers in the
# orthogonal task

# Pull out data to fit
cp_data_orth = all.data %>%
  filter(CP_subset == "CP",
         Task == "Orthogonal",
         Task_item %in% c(6, 8, 10))

# Initial vals and priors: approximation model
# NB: uses same initial vals and priors as CP approximation fitting above
cp_orth_approximation_init = list(cov_fitted = log(0.3))
cp_orth_approximation_priors = list(function(x) {-dnorm(x, log(0.3), 0.1, log = T)})

# MLE fit for CoV
cp_vars_approx_orth = mle_fit_approx(cp_data_orth, 
                                     cp_orth_approximation_init,
                                     cp_orth_approximation_priors)
# Sanity check fitted values
cp_vars_approx_orth
exp(cp_vars_approx_orth['cov_fitted'])


# Initial vals and priors: exact match model
# NB: uses same initial CoV vals and priors as CP exact match fitting above
cp_orth_exact_match_init = list(cov_fitted = log(0.3),
                                match_log_odds_fitted = logit(0.1))
cp_orth_exact_match_priors = list(function(x) {-dnorm(x, log(0.3), 0.1, log = T)}, # priors for cov value in log space
                                  function(x) {-dnorm(logistic(x), 0.1, 0.25, log = T)}) # priors for match pct log odds

# MLE fit for CoV and exact match percent
cp_vars_exact_match_orth = mle_fit_exact_match(cp_data_orth, 
                                               cp_orth_exact_match_init, 
                                               cp_orth_exact_match_priors)
# Sanity check fitted values
cp_vars_exact_match_orth
exp(cp_vars_exact_match_orth['cov_fitted'])
logistic(cp_vars_exact_match_orth['match_log_odds_fitted'])


# Compare BIC for approximation and exact match model with CP knower orthogonal data
BIC_cp_orth_approx = get_BIC(cp_vars_approx_orth['logL'],
                             k = 1, n = length(cp_data_orth$SID))
BIC_cp_orth_exact_match = get_BIC(cp_vars_exact_match_orth['logL'],
                                  k = 2, n = length(cp_data_orth$SID))

# approximation is a slightly better fit than exact match after correcting
# for add'l params (they're basically identical)
BIC_cp_orth_approx
BIC_cp_orth_exact_match

