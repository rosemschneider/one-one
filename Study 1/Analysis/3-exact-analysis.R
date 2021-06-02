## One-to-one correspondence: Exact matching and baseline data
## Analyses for paper
## 3.2.21
## Rose M. Schneider

## Setup
rm(list =ls())
library(tidyverse)
library(tidylog)
library(lme4)

## Read in the data 
## Experiment 1 (one-one baseline)
source("Study 1/Analysis/0-clean.R")

#Custom plotting
#global theme set
theme_set(theme_bw() + theme(text = element_text(size=11),
                             axis.title=element_text(size=10),
                             strip.text = element_text(margin=margin(2,0,2,0)),
                             panel.grid = element_blank()))


##create age group and scaled/centered age variable; also create capped and centered highest_count
all.data <- read.csv("Study 1/Data/one-one_cleaned.csv")

exp1.data <- all.data %>%
  filter(Task == "Parallel" | Task == "Orthogonal") %>% #remove extra rows from non one-one tasks
  mutate(Age = as.numeric(as.character(Age)),
         age.group.floor = factor(floor(Age)),
         age.c = as.vector(scale(Age, center = TRUE, scale = TRUE)),
         highest_count.cap = ifelse(highest_count >= 60, 60, highest_count),
         highest_count.c = as.vector(scale(highest_count.cap, center = TRUE, scale = TRUE)),
         Task_item = as.numeric(as.character(Task_item)),
         Response = as.numeric(as.character(Response)))
## Experiment 2a (exact matching data)
exp2.data <- read.csv("Study 1/Data/one-one_priming_data.csv")

## Quick cleaning of priming data 
exp2.data <- exp2.data %>%
  filter(Exclude != 1, 
         Condition == "EXPLICIT", Task == "Set-matching", 
         Trial_number != "Training", 
         Task_item != "6")%>%
  droplevels() %>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         Age = as.numeric(as.character(Age)), 
         Correct = as.numeric(as.character(Correct)))%>%
  filter(Response <= 15) %>%#get rid of the crazy response
  mutate(CP_subset = ifelse(Knower_level == "CP", "CP", "Subset"))

## Create data frame ====
## Create data frame with both set-matching tasks
exp1.data <- exp1.data %>%
  filter(Task == "Parallel")%>%
  dplyr::select(SID, Age, Task, Task_item, Response, Correct, CP_subset)%>%
  mutate(Condition = "Study 1")

exp2.data.abb <- exp2.data %>%
  dplyr::select(SID, Age, Task, Task_item, Response, Correct, CP_subset)%>%
  mutate(Condition = "Study 2")

## Combine both of these dfs together
combined.data <- bind_rows(exp1.data, exp2.data.abb)%>%
  mutate(Numerosity = ifelse(Task_item > 5, "Large", "Small"))

## Ombinbus tests: Accuracy =====
## Combined CP- and subset-knower data 
combined.base <- glmer(Correct ~ Task_item + Age + (1|SID), 
                       family = 'binomial', 
                       data = combined.data)
combined.cond <- glmer(Correct ~ Condition + Task_item + Age + (1|SID), 
                       family = 'binomial', 
                       data = combined.data)
anova(combined.base, combined.cond, test = 'LRT')

## Visualization ====
combined.data %>%
  mutate(Task_item = factor(Task_item)) %>%
  group_by(Condition, Task_item, CP_subset, Numerosity) %>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, color =  Condition, group=interaction(Numerosity, Condition))) + 
  geom_point(size = 2) + 
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper),
                 width = .1) +
  geom_line() +
  theme(panel.grid = element_blank()) + 
  scale_color_brewer(palette = "Paired") +
  facet_grid(~CP_subset) +
  labs(y = "Mean accuracy", x = "Set size", 
       color = "")
ggsave("Study 1/Analysis/Figures/exactBaseline_accuracy.png", width = 5.8, height = 2.5)

## Counting attempts in exact ====
exp2.data <- exp2.data %>%
  mutate(Counting.Number.language = ifelse(Counting.Number.language == "", 0, 1))

exp2.data %>% 
  group_by(CP_subset, Counting.Number.language)%>%
  summarise(n = n())%>%
  group_by(CP_subset)%>%
  mutate(total = sum(n))
