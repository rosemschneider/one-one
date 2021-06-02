#Data cleaning script for one-to-one set-matching control study
#rose schneider

#setup
rm(list = ls())
library(tidyverse)
library(magrittr)
library(tidylog)
library(stringr)

#filtering function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Read in data ----
data.raw <- read.csv('Study 2.1/Data/1-1_Control data - Data.csv')%>%
  filter(SID != "CopyPasteMe", 
         SID != "Pilot")%>%
  droplevels()%>%
  mutate(Age = as.numeric(as.character(Age)),
         Agegroup = cut(Age, breaks = c(3, 3.5, 4, 4.5, 5.1)))%>%
  mutate(CP_subset = ifelse(Knower_level == "CP", "CP", "Subset"),
         CP_subset = factor(CP_subset))

#change factor level names for prettier graphs
#change some factor to numeric types
data.raw %<>%
  mutate(Correct = as.numeric(as.character(Correct)), 
         Counting.Number.language = ifelse(is.na(Counting.Number.language), NA, 
                                           ifelse(Counting.Number.language == "0", 0, 1)))

# Validation ----
#make sure that responses are correct for all relevant items
#set-matching
set.match.validate <- data.raw %>%
  filter(Task == "Set-matching")%>%
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
  mutate(Numerosity = factor(ifelse((Task == "Set-matching" & as.numeric(as.character(Task_item)) < 5), "Small", 
                                    ifelse((Task == "Set-matching" & as.numeric(as.character(Task_item)) > 5), "Large", NA))))


# Exclusions ----
#How many kids pre-exclusions?
data.raw %<>%
  filter(Exclude_reason != "Pilot")

data.raw %>%
  distinct(SID, Age)%>%
  summarise(n = n())


#Table of exclusions
data.raw %>%
  filter(Exclude == 1)%>%
  distinct(SID, Exclude_reason)%>%
  group_by(Exclude_reason)%>%
  summarise(n = n())%>%
  group_by()%>%
  mutate(sum = sum(n))

#Exclude these participants
data.raw %<>%
  filter(Exclude != 1)

#Finally, exclude training trials
data.raw %<>%
  filter(Trial_number != "Training" | 
           Task_item == "IDK first?" |
           Task_item == "Memory_check_answer"|  
           Task_item == "Neutral-Final Question")

# Get final answer and memory check data 

# get counting check data 
counting.check.df <- data.raw %>%
  filter(Task != "Give_N",
         Trial_number == "5" | 
           Task_item == "IDK first?" |
           Task_item == "Memory_check_answer"|  
           Task_item == "Neutral-Final Question")

# Save and export ----
##rename 
all.data.study2 <- data.raw 

save(all.data.study2, file="Study 2.1/Data/one-one_control_cleaned.RData")
save(counting.check.df, file = "Study 2.1/Data/one-one_control_check_cleaned.RData")

write.csv(all.data.study2, file="Study 2.1/Data/one-one_control_cleaned.csv")

