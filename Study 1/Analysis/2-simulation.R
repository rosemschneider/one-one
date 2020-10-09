#one-to-one simulation

# SETUP ----
rm(list = ls())
source("Study 1/Analysis/0-clean.R") # data cleaning script, produces cleaned data 
# Load cleaned data - 2 dfs
load(here::here("Study 1/Data/one-one_cleaned.RData")) #study 1 data

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
all.data %>%
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

# ...response distribution: Orthogonal ----
all.data %>%
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


