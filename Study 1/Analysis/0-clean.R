#Data cleaning script for one-to-one set-matching baseline study
#rose schneider

#setup
rm(list = ls())
library(tidyverse)
library(magrittr)
library(tidylog)
library(stringr)

setwd("/Users/erikbrockbank/web/one-one/")

#filtering function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Read in data ----
# data.raw <- read.csv(here::here('Study 1/Data/one-one_data.csv'))%>%
data.raw <- read.csv('Study 1/Data/one-one_data.csv')%>%
  filter(SID != "CopyPasteMe")

#change factor level names for prettier graphs
#change some factor to numeric types
data.raw %<>%
  mutate(Condition = ifelse(Condition == "IDENTICAL", "Identical", "Non-identical"),
         Correct = as.numeric(as.character(Correct)),
         Counting.Number.language = ifelse(is.na(Counting.Number.language), NA,
                                           ifelse(Counting.Number.language == "0", 0, 1)))

# Validation ----
#make sure that responses are correct for all relevant items
#set-matching
set.match.validate <- data.raw %>%
  filter(Task == "Parallel" | Task == "Orthogonal")%>%
  droplevels()%>%
  mutate(Task_item = as.numeric(as.character(Task_item)),
         Response = as.numeric(as.character(Response)),
         check.correct = ifelse(Response == Task_item, 1, 0))%>%
  filter(Correct != check.correct)

if(length(set.match.validate$SID) == 0) {
  print("Data validated, no issues")
} else {
  print("Issues in data")
}

# Data manipulations: Counting prof., numerosity, CP_subset, and highest count ----
##Counting proficiency
#fix the name of the task for kids who weren't run on two trials of 10
count.prof <- data.raw %>%
  filter(Task == "How Many")%>%
  mutate(Task_item = ifelse(Task_item == "Score", "10 - Score", as.character(Task_item)))

#compute mean counting
ms.count <- count.prof %>%
  filter(Task_item == "10 - Score" |
           Task_item == "8 - Score")%>%
  distinct(SID, Task_item, Response)%>%
  group_by(SID)%>%
  summarise(count_proficiency = mean(as.numeric(as.character(Response, na.rm = TRUE))))%>%
  dplyr::select(SID, count_proficiency)

##add to all.data
data.raw <- left_join(data.raw, ms.count, by = "SID")%>%
  mutate(count_proficiency = as.numeric(as.character(count_proficiency)))

##Highest count
hc.lookup <- data.raw %>%
  filter(Task_item == "Highest_count")%>%
  dplyr::select(SID, Response)%>%
  dplyr::rename(highest_count = Response)

##There are several children who will not count out loud, exclude them from analyses with Highest Count
data.raw <- left_join(data.raw, hc.lookup, by = "SID")

#change to numeric
data.raw %<>%
  mutate(highest_count = ifelse(is.na(highest_count), NA, as.numeric(as.character(highest_count))))

#Numerosity: Small (<5) or large, for set-matching
data.raw %<>%
  mutate(Numerosity = factor(ifelse(((Task == "Parallel" | Task == "Orthogonal") &
                                as.numeric(as.character(Task_item)) < 5), "Small",
                             ifelse(((Task == "Parallel" | Task == "Orthogonal") &
                                       as.numeric(as.character(Task_item)) > 5), "Large", NA))))

#CP/subset
data.raw %<>%
  mutate(CP_subset = factor(ifelse(Knower_level == "CP", "CP", "Subset")))

# Exclusions ----
##How many kids before exclusions?
data.raw %>%
  distinct(SID, Age)%>%
  group_by()%>%
  summarise(n = n())

#Table of exclusions
data.raw %>%
  filter(Exclude == 1)%>%
  distinct(SID, Exclude_reason)%>%
  group_by(Exclude_reason)%>%
  summarise(n = n())%>%
  group_by()%>%
  mutate(total = sum(n))

#Exclude these participants
data.raw %<>%
  filter(Exclude != 1)

#post-hoc exclusions: We did not put a criteria on passing/failing training trials
#this attempts to identify children who are failing to understand the purpose of the task
#how many kids failed at least one trial in Parallel
failed.trials <- data.raw %>%
  filter(Task == "Parallel" | Task == "Orthogonal",
         Trial_number == "Training")%>%
  distinct(SID, Task, Task_item, Correct)%>%
  filter(Correct == 0)

#first look at the kids who failed both on parallel, as these are likely to not understand the task
failed.trials %>%
  filter(Task == "Parallel")%>%
  group_by(SID)%>%
  summarise(n = n())%>%
  filter(n == 2)

#one-one_48 - gives max for every single trial, failed training trials even with feedback, should be excluded
#one-one_62 - should not be excluded - failed both Parallel training (15 for both), but seemed to get it (matched for 3 and 4)
#one-one_92 - looked a little confused on training trials at the start, but seemed to get it
#one-one_112 - marginal, failed both parallel (15 for both), some variability after that, succeeded on orthogonal training, but failed on 3 and 4; should not be excluded, succeeded on Orthogonal training
#one-one_120 - marginal, seems to be performing randomly, succeeded on only one training trial
#one-one_124 - should not be excluded, failed on first two parallel, but then seemed to recover, succeeds on orthogonal training

##MANUAL EXCLUSION for complete failure on set-matching
data.raw %<>%
  filter(SID != 'one-one_48')

#Finally, exclude training trials
data.raw %<>%
  filter(Trial_number != "Training")

# Save and export ----
##rename
all.data <- data.raw

save(all.data, file="Study 1/Data/one-one_cleaned.RData")

write.csv(all.data, file="Study 1/Data/one-one_cleaned.csv")
