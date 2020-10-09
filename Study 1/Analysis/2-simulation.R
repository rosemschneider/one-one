#one-to-one simulation

# SETUP ----
rm(list = ls())
setwd("/Users/erikbrockbank/web/one-one/")

source("Study 1/Analysis/0-clean.R") # data cleaning script, produces cleaned data
# Load cleaned data - 2 dfs
# load(here::here("Study 1/Data/one-one_cleaned.RData")) #study 1 data
load("Study 1/Data/one-one_cleaned.RData") #study 1 data

# load packages ----
library(tidyverse)
library(magrittr)
library(car)
library(lme4)
library(ggpubr)
library(broom)
library(broom.mixed)
library(tidylog)
library(lmerTest)
library(emmeans)
library(patchwork)

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

# ...response distribution: Parallel ----
parallel_dist_plot = all.data %>%
  filter(Task == "Parallel")%>%
  filter(Task_item > 2)%>%
  group_by(CP_subset, Task_item, Response)%>%
  # summarise(n = n()) %>%
  ggplot(aes(x = Response, fill = CP_subset)) +
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  scale_fill_manual(values = cp.sub.palette) +
  facet_grid(CP_subset ~ Task_item) +
  scale_x_continuous(breaks = seq(1, 15, 1)) +
  labs(x = 'Number of items given', y = 'Frequency',
       title = 'a) Parallel trials')
parallel_dist_plot

# ...response distribution: Orthogonal ----
orth_dist_plot = all.data %>%
  filter(Task == "Orthogonal")%>%
  filter(Task_item > 2)%>%
  group_by(CP_subset, Task_item, Response)%>%
  # summarise(n = n()) %>%
  ggplot(aes(x = Response, fill = CP_subset)) +
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(binwidth = 1, color = 'black') +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        panel.grid = element_blank()) +
  scale_fill_manual(values = cp.sub.palette) +
  facet_grid(CP_subset ~ Task_item) +
  scale_x_continuous(breaks = seq(1, 15, 1)) +
  labs(x = 'Number of items given', y = 'Frequency',
       title = 'b) Orthogonal trials')
orth_dist_plot


# Estimate ~ N(cov(0.64), sigma)  based on Wagner et al. 2018
# Or fit one CoV; this might be silly since we know that's not necessarily appropriate here...


#' Simulation goals:
#' 1. H0 is that people are approximating across the board
#' -> Try to show that the approximation process doesn't accurately capture CP knowers 3-10
#' -> Additionally, show taht this *does* capture Subset knowers 3-10 pretty well
#' 2. H1 is that people are doing some exact matching and they become approximators
#' at higher magnitudes
#' -> Show that this "approximation waterfall" fits the CP knower data better

glimpse(all.data)
subjects = unique(all.data$SID)
trials = unique(all.data$Trial_number)
task = "Parallel"
task_item = unique(all.data$Task_item)


simulation_data = data.frame(
  SID = character(),
  Trial_number = numeric(), # 1-5
  Task = character(), # "Parallel", filter all.data to match this
  Task_item = numeric(), # {3, 4, 6, 8, 10}
  Response = numeric(),
  CP_subset = character() # {"CP", "Subset"}
)


CoV = 0.64 # sd(estimates) / mean(estimates)
number = 3
# Here's basic approximation
round(rnorm(1, number, number * CoV), 0) # TODO confirm that this is legit?
# TODO may need to handle possibility of exact matching on 3 (even for approximators) because of PI
# For CP knowers, may have stable label for 3 so similar to above concern


# With above, can try first just simulating and seeing how it looks
# Then fit a CoV with function above and see how things look





