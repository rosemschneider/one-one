##One-one exact - analysis
##Rose M. Schneider

rm(list = ls())

source("Study 2 (exact number)/Analysis/0-cleaning.R")
## load libraries
library(tidyverse)
library(stringr)
library(langcog)
library(here)

# # Custom global variables
cp.sub.palette <- c("#1ECCE3", "#FF7C00")

## Data ----
all.data <- read.csv(here("Study 2 (exact number)/Data/one-one_exact_cleaned.csv"))

## save aggregate data
write.csv(all.data, "Study 2 (exact number)/Data/agg_data_exp3.csv")

## demographics
all.data %>%
  distinct(SID, Age, CP_subset)%>%
  group_by(CP_subset)%>%
  summarise(n = n(),
            mean_age = mean(Age, na.rm = TRUE), 
            sd_age = sd(Age, na.rm = TRUE))

## sex 
all.data %>%
  distinct(SID, Age, CP_subset, Sex)%>%
  group_by(CP_subset, Sex)%>%
  summarise(n = n())

## Numerosity 
all.data <- all.data %>%
  mutate(Numerosity = ifelse(Task_item > 4, "Large", "Small"))

## Accuracy ----
## descriptives
all.data %>%
  group_by(CP_subset, Numerosity)%>%
  summarise(n = n(),
            mean = mean(Correct, na.rm = TRUE), 
            sd = sd(Correct, na.rm = TRUE))

mean(all.data$Age)

## ...visualization of accuracy ----
all.data %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4", "8", "10")))%>%
  group_by(Numerosity, Task_item, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group = interaction(Numerosity, CP_subset))) +
  geom_point(size = 2) +
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper),
                 width = .1) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "right") +
  labs(x = "Set size", y = "Mean accuracy") +
  scale_colour_manual(values = cp.sub.palette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color= "Knower Level")
ggsave("Study 2 (exact number)/Analysis/Figures/CP_Subset_accuracy_Exact.png", width = 5, height = 3.5)

## ...distribution of responses: parallel
all.data %>%
  mutate(CP_subset = factor(CP_subset, levels = c("Subset","CP")))%>%
  group_by(CP_subset, Task_item, Response)%>%
  # summarise(n = n()) %>%
  ggplot(aes(x = Response, fill = CP_subset)) +
  geom_vline(aes(xintercept = Task_item), linetype = "dashed") +
  geom_histogram(color = 'black', binwidth = 1) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        panel.grid = element_blank()) +
  scale_fill_manual(values = rev(cp.sub.palette)) +
  facet_grid(CP_subset~Task_item) +
  scale_x_continuous(breaks = seq(1, 15, 1)) +
  labs(x = 'Number of items given', y = 'Frequency')

## Error ----
error.df <- all.data %>%
  filter(Correct == 0)

## descriptives
error.df %>%
  group_by(CP_subset, Numerosity)%>%
  summarise(n = n(),
            mean = mean(abs.error, na.rm = TRUE), 
            sd = sd(abs.error, na.rm = TRUE))

## ...visualization of absolute error ----
error.df %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4", "8", "10")))%>%
  group_by(Numerosity, Task_item, CP_subset)%>%
  langcog::multi_boot_standard("abs.error", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group = interaction(Numerosity, CP_subset))) +
  geom_point(size = 2) +
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper),
                 width = .1) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "right") +
  labs(x = "Set size", y = "Mean absolute error") +
  scale_colour_manual(values = cp.sub.palette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color= "Knower Level")

# How many kids counted 
all.data %>%
  mutate(counted = ifelse(Counting.Number.language != "", "Counted", "NA"))%>%
  filter(counted == "Counted")%>%
  distinct(SID, CP_subset)

