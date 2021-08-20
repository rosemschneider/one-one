## One-to-one correspondence: Sharing (Study 3)
## Data cleaning
## Rose M. Schneider

## Set-up
rm(list = ls()) 
library(tidyverse) 
library(tidylog)

## Read in data ----
### Study 1 (sharing)
sharing.data <- read.csv("Study 3 (sharing)/Data/one-one_sharing.csv") #read in the data
str(sharing.data) 
summary(sharing.data) 

## Data Cleaning ----
sharing.data <- sharing.data %>% 
  filter(SID != "CopyPasteMe", #removing the template "copypasteme" rows
         Condition == "SHARING", #there's matching and sharing in this dataset, so we're just looking at sharing
         !is.na(Age), # remove any values that have NA for age
         Task == "Set-matching", # remove other tasks
         Exclude_reason != "Pilot", 
         Trial_number != "Training")%>% # I don't want to keep data that was collected for the pilot or training trials
  mutate(Sex = ifelse(Sex == "Male", "M", as.character(Sex)))%>% # this is fixing an issue with coding sex
  droplevels() #droplevels removes unused factor levels


## Demographics, exclusions ----
### How many kids before exclusions?
sharing.data %>%
  distinct(SID, Age)%>% 
  summarise(n = n()) 

### Why are kids excluded?
sharing.data %>%
  distinct(SID, Age, Exclude, Exclude_reason)%>%
  filter(Exclude == 1)%>%
  group_by(Exclude_reason)%>% 
  summarise(n = n())%>%
  mutate(total.n = sum(n)) 

### Exclude these kids
sharing.data <- sharing.data %>%
  filter(Exclude != 1)%>% 
  mutate(Age = as.numeric(as.character(Age)),
         CP_subset = ifelse(Knower_level == "CP", "CP", "Subset")) 

### How many kids post-exclusion? Mean age, etc.
sharing.data %>% 
  distinct(SID, Age)%>%
  summarise(n = n(), 
            mean_age = mean(Age, na.rm = TRUE), 
            sd_age = sd(Age, na.rm = TRUE))

## By sex
sharing.data %>%
  distinct(SID, CP_subset, Sex)%>%
  group_by(CP_subset, Sex)%>%
  summarise(n = n())

## CP_subset
sharing.data %>%
  distinct(SID, Age, CP_subset)%>%
  group_by(CP_subset)%>%
  summarise(n = n(),
            mean = mean(Age, na.rm =TRUE), 
            sd = sd(Age, na.rm = TRUE))

## add numerosity
sharing.data <- sharing.data %>%
  mutate(Task_item = as.numeric(as.character(Task_item)),  
          Numerosity = ifelse(Task_item > 4, "Large", "Small")) 

## Counting ----
### How often did kids count??
sharing.data %>%
  mutate(Counting.Number.language = as.numeric(as.character(Counting.Number.language)))%>%
  filter(Task == "Set-matching", 
         Trial_number != "Training")%>%
  group_by(CP_subset, Counting.Number.language)%>%
  summarise(n = n())%>%
  group_by(CP_subset)%>%
  mutate(total.n = sum(n))

## Save cleaned data ----
write.csv(sharing.data, "Study 3 (sharing)/Data/one-one_sharing_cleaned.csv")# to sharing folder



