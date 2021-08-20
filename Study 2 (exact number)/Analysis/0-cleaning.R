##One-one exact number condition (Study 2)
##0 - cleaning script
##Rose M. Schneider

rm(list = ls())

## load libraries
library(tidyverse)
library(stringr)
library(here)

## Read in the data -----
data.raw <- read.csv(here("Study 2 (exact number)/Data/one-one_exact.csv"))%>%
  filter(SID != "CopyPasteMe", 
         Condition == "EXPLICIT")

## How many kids pre-exclusions?\
data.raw %>%
  distinct(SID, Age)

## What kids were excluded and why?
data.raw %>% 
  filter(Exclude == 1)%>%
  distinct(SID, Age, Exclude_reason)%>%
  group_by(Exclude_reason)%>%
  summarise(n = n())%>%
  mutate(total.n = sum(n))

## filter out excluded kids
data.raw <- data.raw %>%
  filter(Exclude != 1)

## Get the highest count data
hc.data <- data.raw %>%
  filter(Task_item == "Highest_count")%>%
  select(SID, Response)%>%
  dplyr::rename("highest_count" = "Response")%>%
  mutate(highest_count = as.numeric(as.character(highest_count)))

## Get set-matching data only
all.data <- data.raw %>%
  filter(Task == "Set-matching")%>%
  mutate(Age = as.numeric(as.character(Age)), 
         Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         Correct = as.numeric(as.character(Correct)), 
         CP_subset = ifelse(Knower_level == "CP", "CP", "Subset"))%>%
  filter(!is.na(Task_item),
         Task_item > 2, 
         Task_item != 6, 
         is.na(Exclude.Trial))%>%
  left_join(hc.data, by = "SID")

## Do a sanity check on correct
sanity.check <- all.data %>%
  mutate(check.correct = ifelse((Response == Task_item & Correct == 0), 1, 0), 
         check.incorrect = ifelse((Response != Task_item & Correct == 1), 1, 0))%>%
  filter(check.correct == 1 | 
           check.incorrect == 1) #length is 0, we're good

##finally, add an absolute error
all.data <- all.data %>%
  mutate(abs.error = abs(Task_item-Response))

##save the data
write.csv(all.data, here("Study 2 (exact number)/Data/one-one_exact_cleaned.csv"))

  
