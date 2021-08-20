##One-to-one counting control 

# SETUP ----
source("Study 4 (counting control)/Analysis/0-clean.R") # data cleaning script, produces cleaned data 
# Load cleaned data - 2 dfs
rm(list = ls())
load("Study 4 (counting control)/Data/one-one_control_cleaned.RData") #study 2 all data
load("Study 4 (counting control)/Data/one-one_control_check_cleaned.RData") #study 2 check data

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

#to-do: color palettes
# # Custom global variables
cp.sub.palette <- c("#1ECCE3")
#global theme set
theme_set(theme_bw())

# ... Descriptives ----
all.data.study2 %>%
  distinct(SID, Age)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(n, mean, sd, median, min, max)

#sex
all.data.study2 %>%
  distinct(SID, Sex)%>%
  group_by(Sex)%>%
  summarise(n = n())

# Accuracy ----
all.data.study2 %>%
  filter(Task == "Set-matching", 
         Trial_number !="Training")%>%
  group_by(Numerosity)%>%
  summarise(mean = mean(Correct, na.rm = TRUE), 
            sd = sd(Correct, na.rm = TRUE))

# Memory check ---

##make a df of kids and final responses 
idk.first <- counting.check.df %>%
  filter(Task_item == "Neutral-Final Question")%>%
  mutate(IDK.first = ifelse(str_detect(Response, "IDK", negate = FALSE), 1, 0))%>%
  dplyr::select(SID, IDK.first, Response)%>%
  dplyr::rename(Final_q = Response)

##get the last trial for each kid
last.trial <- counting.check.df %>%
  filter(Trial_number == "5")%>%
  dplyr::select(SID, Task_item, Response)%>%
  dplyr::rename(Num_given = Response)

##now get memory check 
memory.check <- counting.check.df %>%
  filter(Task_item == "Memory_check_answer")%>%
  dplyr::select(SID, Response)%>%
  dplyr::rename("Memory_check" = Response)

##bind all this together
full.check <- left_join(last.trial, idk.first, by = "SID")
full.check <- left_join(full.check, memory.check, by = "SID")

##how many children passed memory check 
full.check %<>%
  mutate(Task_item = as.character(Task_item), 
         Memory_check = as.character(Memory_check))%>%
  mutate(memory.check.correct = ifelse(Memory_check == Task_item, 1, 0))

#binomial test for significance
binom.test(sum(full.check$memory.check.correct), 
           28, p = .05)

mean(full.check$memory.check.correct)
sum(full.check$memory.check.correct)

## how many said IDK first
sum(full.check$IDK.first)

## of children who did not give IDK response first, how many were correct
numeric.responses <- full.check %>%
  filter(IDK.first != 1)%>%
  mutate(Final_q = as.character(Final_q), 
         correct.verbal.response = ifelse(Final_q == Task_item, 1, 0), 
         delta = abs(as.numeric(Task_item) - as.numeric(Final_q)), 
         delta.given = abs(as.numeric(Final_q) - as.numeric(as.character(Num_given))))

sum(numeric.responses$correct.verbal.response)

#how many kids said the same number of items that they gave?
numeric.responses %>%
  filter(delta.given == 0)

numeric.responses %<>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Num_given = as.numeric(as.character(Num_given)), 
         Final_q = as.numeric(as.character(Final_q)))

## Now let's add the kids who first said IDK, and then gave a numeric response
full.check %<>% 
  mutate(Final_q_with_IDK = ifelse(IDK.first == 1, str_remove(Final_q, "IDK,"), as.character(Final_q)))

numeric.with.idk <- full.check %>%
  filter(Final_q_with_IDK != "IDK", 
         Final_q_with_IDK != " no guess")%>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Num_given = as.numeric(as.character(Num_given)), 
         Final_q_with_IDK = as.numeric(as.character(Final_q_with_IDK)))

tmp <- numeric.with.idk[order(numeric.with.idk$Task_item, 
                              numeric.with.idk$Num_given, 
                              numeric.with.idk$Final_q_with_IDK),]%>%
  mutate(SID.numeric = 1:length(numeric.with.idk$SID), 
         IDK.first = factor(IDK.first, labels = c("Numeric response first", 
                                                  "Non-numeric response")))

# plot of distance between num_given, set size, and num_said
tmp %>% 
  mutate(Num_given = as.numeric(as.character(Num_given)), 
         Task_item = as.numeric(as.character(Task_item)), 
         Final_q = as.numeric(as.character(Final_q)))%>%
  ggplot(aes(x=as.factor(SID.numeric))) +
  geom_linerange(aes(ymin=Final_q_with_IDK, ymax=Num_given), size=.5) +
  geom_linerange(aes(ymin=Task_item, ymax=Num_given), size=.5) +
  geom_linerange(aes(ymin=Num_given, ymax=Final_q), size=.5) +
  geom_point(aes(y=Num_given, fill="Number of items given", shape = "Number of items given"), 
             alpha = .7, size=3, stroke = 0) +
  geom_point(aes(y=Final_q_with_IDK, fill="Verbal response", shape = "Verbal response"), 
             alpha = .7, size=3, stroke = 0) +
  geom_point(aes(y=Task_item, fill="Set size", shape = "Set size"), size=2, stroke = 0) +
  scale_fill_manual(name = "", 
                     breaks = c("Set size", "Number of items given", 
                                "Verbal response"),
                     values = c("Set size"="#fa8620", "Number of items given"="#02619c", 
                                "Verbal response"="#6ac1f7"),
                     guide = "legend") +
  scale_shape_manual(name = "",
                     breaks = c("Set size", "Number of items given", 
                                "Verbal response"),
                     values = c("Set size"=21, "Number of items given"=24, "Verbal response"=21),
                     guide = "legend") +
  scale_y_continuous(breaks = seq(1, 20, 1)) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom", panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(), 
        panel.grid = element_blank()) + 
  facet_grid(~IDK.first, scale= "free_x") +
  labs(x = "Each line = individual participant", 
       y = "Response/Set size")
ggsave("Study 4 (counting control)/Analysis/Figures/numeric_distance.png", width = 5.5, height = 4)


## of children who did not give IDK first responses, how far off were they?
mean(numeric.responses$delta)
sd(numeric.responses$delta)

## of children who did not give IDK first responses, how far off were their responses from the number given?
mean(numeric.responses$delta.given)
sd(numeric.responses$delta.given)

## separate numerical group by correct matching, see if there are differences in numerical response
numeric.responses %<>%
  mutate(correct.given = ifelse(Num_given == Task_item, 1, 0))

numeric.responses %>% 
  group_by(correct.given)%>%
  summarise(n = n(),
            mean.delta.verbal = mean(delta), 
            mean.delta.given = mean(delta.given))

##now look at kids who said IDK first for # correct and mean number off from correct response
full.check %>%
  filter(IDK.first == 1)%>%
  mutate(Num_given = as.numeric(as.character(Num_given)), 
         Task_item = as.numeric(Task_item),
         delta.given = abs(Num_given - Task_item),
         correct.given = ifelse(Num_given == Task_item, 1, 0))%>%
  group_by(correct.given)%>%
  summarise(n = n(),
            mean = mean(delta.given, na.rm = TRUE))

## Of the kids who said IDK first but then gave a numeric response, how many gave correct response
tmp <- full.check %>% 
  filter(IDK.first == 1)%>%
  mutate(IDK.final = ifelse(Final_q == "IDK", 1, 0), 
         correct.given = ifelse(Task_item == as.numeric(as.character(Final_q_with_IDK)), 1, 0))%>%
  group_by(IDK.final, correct.given)%>%
  summarise(n = n())

## group IDKs and numeric together and look at accuracy 
all.data.study2 <- left_join(all.data.study2, idk.first, by = "SID")

acc.ms <- all.data.study2 %>%
  group_by(SID, IDK.first)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#finally, t-test for accuracy
t.test(subset(acc.ms, IDK.first == 1)$mean, 
       subset(acc.ms, IDK.first == 0)$mean, var.equal = TRUE)

## How often did answers match responses? ====
answer.check <- full.check %>%
  filter(Final_q_with_IDK != "IDK")%>%
  mutate(Num_given = as.numeric(as.character(Num_given)),
         Task_item = as.numeric(as.character(Task_item)),
          Final_q_with_IDK = ifelse(Final_q_with_IDK == "no guess", NA, as.numeric(as.character(Final_q_with_IDK))), 
          answer_response_correct = ifelse(Final_q_with_IDK == Num_given, 1, 0))

## How far off are kids in responses ====
acc.ms <- answer.check %>%
  mutate(delta_target = abs(Final_q_with_IDK - Task_item), 
         delta_response = abs(Final_q_with_IDK - Num_given))

## How far off are they from the target
mean(acc.ms$delta_target, na.rm = TRUE)
sd(acc.ms$delta_target, na.rm = TRUE)

## How far off are they from the response
mean(acc.ms$delta_response, na.rm = TRUE)
sd(acc.ms$delta_response, na.rm = TRUE)

## Omnibus test looking at accuracy between baseline and this control ====
## read in the data from exp 1
exp1.data <- read.csv("Study 1/Data/one-one_cleaned.csv")%>%
  filter(Task == "Parallel", 
         CP_subset == "CP")%>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         Task_item.c = as.vector(scale(Task_item, scale = TRUE, center = TRUE)), 
         age.c = as.vector(scale(Age, scale = TRUE, center = TRUE)), 
         Experiment = "Experiment 1")%>%
  select(SID, Age, Task, Knower_level, Task_item, Response, Correct, Task_item.c, age.c, Experiment)

exp2.model <- all.data.study2%>%
  filter(Task == "Set-matching", 
         !is.na(Trial_number))%>%
  droplevels()%>%
  mutate(Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)), 
         Task_item.c = as.vector(scale(Task_item, scale = TRUE, center = TRUE)), 
         age.c = as.vector(scale(Age, scale = TRUE, center = TRUE)), 
         Experiment = "Experiment 2b")%>%
  select(SID, Age, Task, Knower_level, Task_item, Response, Correct, Task_item.c, age.c, Experiment)

#now put these dudes into one df
omni.df <- bind_rows(exp1.data, exp2.model)

## glmer predicting accuracy to test for effect of experiment
## base
omni.base <- glmer(Correct ~ Task_item.c + age.c + (1|SID), 
                   family = 'binomial', 
                   data = omni.df)
omni.exp <- glmer(Correct ~ Experiment + Task_item.c + age.c + (1|SID), 
                   family = 'binomial', 
                   data = omni.df)
anova(omni.base, omni.exp, test = 'lrt')

