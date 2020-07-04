#One-to-one baseline: Set-matching, analysis
## Info about study and analysis plan here

# SETUP ----
source("0-clean.R") # data cleaning script, produces cleaned data 
# Load cleaned data - 2 dfs
rm(list = ls())
load("../Data/one-one_cleaned.RData") #study 1


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
cp.sub.palette <- c("#1ECCE3", "#FF7C00")
#global theme set
theme_set(theme_bw() + theme(text = element_text(size=9), 
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0)), 
                             panel.grid = element_blank()))

# Data manipulations ----

##create age group and scaled/centered age variable; also create capped and centered highest_count
all.data %<>%
  filter(Task == "Parallel" | Task == "Orthogonal") %>% #remove extra rows
  mutate(Age = as.numeric(as.character(Age)),
         age.group.floor = factor(floor(Age)), 
         age.c = as.vector(scale(Age, center = TRUE, scale = TRUE)), 
         highest_count.cap = ifelse(highest_count >= 60, 60, highest_count), 
         highest_count.c = as.vector(scale(highest_count.cap, center = TRUE, scale = TRUE)), 
         Task_item = as.numeric(as.character(Task_item)), 
         Response = as.numeric(as.character(Response)))
  

## make an error df
error.df <- all.data %>%
  filter(Correct == 0)%>%
  mutate(abs.error = abs(Task_item - Response))

# ... study 1 descriptives ----
#overall
all.data %>%
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

#by CP/subset level
all.data %>%
  distinct(SID, CP_subset, Age)%>%
  group_by(CP_subset)%>%
  summarise_at('Age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(CP_subset, n, mean, sd, median, min, max)

# knower level
all.data %>%
  distinct(SID, Knower_level)%>%
  group_by(Knower_level)%>%
  summarise(n= n())

#sex
all.data %>%
  distinct(SID, Sex)%>%
  group_by(Sex)%>%
  summarise(n = n())

# Accuracy ----

# ... Descriptives: overall accuracy grouped by CP_knower status, task, numerosity ----
all.data %>%
  group_by(CP_subset, Task, Numerosity)%>%
  summarise_at('Correct', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T)))%>%
  dplyr::select(CP_subset, Task, Numerosity, mean, sd)

# ...visualization ----
all.data %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4",  
                                                  "6", "8", "10")))%>%
  group_by(Numerosity, Task, Task_item, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group = interaction(Numerosity, CP_subset))) +
  geom_point(size = 2) + 
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 width = .1) +
  theme_bw(base_size = 15) + 
  facet_grid(~factor(Task, levels = c("Parallel", "Orthogonal")), scale = "free_x") +
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.title = element_blank(), 
    legend.position = "right") +
  labs(x = "Set size", y = "Mean accuracy") +
  scale_colour_manual(values = cp.sub.palette) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(color= "Knower Level")
ggsave("Figures/mean_accuracy.png", width = 5.8, height = 3.5)

## visualization of just parallel for talks 

all.data %>%
  filter(Task == "Parallel")%>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4",  
                                                  "6", "8", "10")))%>%
  group_by(Numerosity, Task_item, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group = interaction(Numerosity, CP_subset))) +
  geom_point(size = 2) + 
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 width = .1) +
  theme_bw(base_size = 15) + 
  theme( 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.title = element_blank(), 
    legend.position = "right") +
  labs(x = "Set size", y = "Mean accuracy") +
  scale_colour_manual(values = cp.sub.palette) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(color= "Knower Level")
ggsave("Figures/mean_accuracy_parallel.png", width = 4, height = 3.5)

# ...response distribution: Parallel ----
para <- all.data %>%
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

ggsave("Figures/dist_response_parallel.png", width = 14)

# ...response distribution: Orthogonal ----
orth <- all.data %>%
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

ggsave("Figures/dist_response_orthogonal.png", width = 14)

library(patchwork)
para / orth
ggsave("Figures/dist_response_both.png", width = 14, 
       height = 10)

# ...accuracy by condition ----
all.data %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4", 
                                                  "6", "8", "10")))%>%
  group_by(Numerosity, Task, Task_item, Condition, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group = interaction(Numerosity, CP_subset))) +
  geom_point(size = 2) + 
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 width = .1) +
  theme_bw(base_size = 15) + 
  facet_grid(Condition ~ factor(Task, levels = c("Parallel", "Orthogonal")), scale = "free_x") +
  theme(legend.position = "right", 
        panel.grid = element_blank(), 
        legend.title = element_blank()) +
  labs(x = "Set size", y = "Mean performance") +
  scale_colour_manual(values = cp.sub.palette)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  labs(color = "Knower Level")

ggsave("Figures/mean_accuracy_condition.png", width = 6, height = 4.2)

# ...Overall accuracy by CP-knower status ----
#make model df
model.df <- all.data %>%
  mutate(Task_item.c = as.vector(scale(Task_item, center = TRUE, scale=TRUE)),
         count_proficiency.c = as.vector(scale(count_proficiency, center = TRUE, scale = TRUE)),
         CP_subset = factor(CP_subset, levels = c("Subset", "CP")))

# Do CP-knowers perform better overall?
#create a base model that includes numerosity and task
overall.acc.base <- glmer(Correct ~ Task_item.c + Task + age.c + (1|SID), 
                          family = "binomial", data = model.df)

#add CP_subset-knower status
overall.acc.kl <- glmer(Correct ~ CP_subset + Task_item.c + Task + age.c + (1|SID), 
                        family = "binomial", data = model.df)

#compare
anova(overall.acc.kl, overall.acc.base, test = 'lrt')

#now add interaction
overall.acc.kl.int <- glmer(Correct ~ CP_subset*Task_item.c + Task + age.c + (1|SID), 
                            family = "binomial", data = model.df)

#compare
anova(overall.acc.base, overall.acc.kl, overall.acc.kl.int, test = 'lrt')
#is the interaction significant?
car::Anova(overall.acc.kl.int)#nope - interaction doesn't significantly improve model fit; p = .27
tidy(overall.acc.kl, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

### FOLLOW UP TEST FOR 3 WAY INTERACTION BETWEEN SET SIZE, ORIENTATION, AND CP ###
## FIRST, test for interaction between task and set size
#base model with interaction between numerosity and task
three_way.base <- glmer(Correct ~ Task_item.c*Task + age.c + (1|SID), 
                        family = "binomial", data = model.df, 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
#compare to overall.acc
anova(overall.acc.base, three_way.base, test = 'lrt') # this interaction does not improve the fit of the model; p = .32

### Next, test for interaction between orientation, cp, and set size
## main effects
three_way.base.full <- glmer(Correct ~  CP_subset + Task + Task_item.c + age.c + (1|SID), 
                             family = "binomial", data = model.df, 
                             control=glmerControl(optimizer="bobyqa",
                                                  optCtrl=list(maxfun=2e4)))

three_way.2int <- glmer(Correct ~  CP_subset*Task + Task_item.c + age.c + (1|SID), 
                        family = "binomial", data = model.df, 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
anova(three_way.base.full, three_way.2int, test= 'lrt') #there is a significant interaction with task and CP, p = .006 

##full 3-way interaction between task, cp, and set size
three_way.full <- glmer(Correct ~  CP_subset*Task*Task_item.c + age.c + (1|SID), 
                        family = "binomial", data = model.df, 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
anova(three_way.base.full, three_way.full, test= 'lrt') # significant interaction between set size, task, and cp knower status, p = .0005
car::Anova(three_way.full)
summary(three_way.full)
# ... FOLLOW up with just parallel for talks stats ----
parallel.acc.kl <- glmer(Correct ~ CP_subset + Task_item.c + age.c + (1|SID), 
                        family = "binomial", data = subset(model.df, Task == "Parallel"))
summary(parallel.acc.kl)

# ...Followup analysis: Are CP-knowers significantly more accurate than subset-knowers on Orthogonal task? ----
#build the base model
orth.base <- glmer(Correct ~ Task_item.c + age.c + (1|SID), 
                   family= "binomial", data = subset(model.df, Task == "Orthogonal"), 
                   control=glmerControl(optimizer="bobyqa",
                                        optCtrl=list(maxfun=2e4)))

#main effect of KL
orth.kl <- glmer(Correct ~ CP_subset + Task_item.c + age.c + (1|SID), 
                 family= "binomial", data = subset(model.df, Task == "Orthogonal"), 
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e4)))
anova(orth.base, orth.kl, test= 'lrt') #yes, p = .004

#what about an interaction
orth.kl.int <- glmer(Correct ~ CP_subset*Task_item.c + age.c + (1|SID), 
                     family= "binomial", data = subset(model.df, Task == "Orthogonal"), 
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e4)))
car::Anova(orth.kl.int) #yes, significant interaction
tidy(orth.kl.int, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

# ...accuracy by item identity ----
#make base model without condition term 
condition.acc.base <- glmer(Correct ~ Condition + CP_subset + Task_item.c + Task + age.c + (1|SID), 
                            family = "binomial", data = model.df)
#does adding condition improve fit of base?
anova(overall.acc.kl, condition.acc.base, test = 'LRT')#no main effect of condition
summary(condition.acc.base)

##test for two-way interaction
condition.acc.2int <- glmer(Correct ~ CP_subset*Condition + Task_item.c + Task + age.c + (1|SID), 
                            family = "binomial", data = model.df, 
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e4)))
anova(overall.acc.kl, condition.acc.base, condition.acc.2int, test = 'lrt')
#test for interaction
car::Anova(condition.acc.2int) #but there is a significant interaction
tidy(condition.acc.2int, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

#multiple comparisons for parallel condition
## No difference between CP and Subset knowers when items are identical (p = .11)
## No difference for subset knowers on identical vs non-identical (p = .23)
## CP-knowers better on non-identical than subset knowers' identical (p = .0001)
## CP-knowers better on identical than subset knowers' non-identical (p < .0001)
## No difference for CP-knowers between identical and non-identical (p = .09)
## CP-knowers better on non-identical than subset knowers (p < .0001)
condition.group <- glmer(Correct ~ CP_subset*Condition + Numerosity  + Task +
                         age.group.floor + (1|SID), 
                       data=model.df, family = 'binomial', 
                       control=glmerControl(optimizer="bobyqa",
                                            optCtrl=list(maxfun=2e4)))
emmeans::emmeans(condition.group, list(pairwise ~ CP_subset*Condition), adjust = 'tukey') 

# ...follow up with main effect of CP-knowers on identical ----
follow.ident.base <- glmer(Correct ~ Task_item.c + 
                        Task + age.c + (1|SID), 
                      data = subset(model.df, Condition == "Identical"), 
                      family = 'binomial')
follow.ident <- glmer(Correct ~ CP_subset + Task_item.c + 
                        Task + age.c + (1|SID), 
                      data = subset(model.df, Condition == "Identical"), 
                      family = 'binomial')
anova(follow.ident.base, follow.ident, test = 'lrt')
summary(follow.ident)

follow.noident.base <- glmer(Correct ~ Task_item.c + 
                             Task + age.c + (1|SID), 
                           data = subset(model.df, Condition == "Non-identical"), 
                           family = 'binomial')
follow.noident <- glmer(Correct ~ CP_subset + Task_item.c + 
                        Task + age.c + (1|SID), 
                      data = subset(model.df, Condition == "Non-identical"), 
                      family = 'binomial')
anova(follow.noident.base, follow.noident, test = 'lrt')
summary(follow.noident)

all.data %>%
  filter(Task == "Parallel" | 
           Task == "Orthogonal")%>%
  group_by(CP_subset, Condition)%>%
  summarise(mean = mean(Correct, na.rm = TRUE), 
            sd = sd(Correct, na.rm = TRUE))

# ...follow up: comparing variances ----
##make a variance df
var.df <- all.data %>%
  filter(Task == "Parallel"|
           Task == "Orthogonal")
#CP-knowers' small ~ task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "CP" & 
                                            Task_item == 3))
#CP-knowers' small ~ task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "CP" & 
                                            Task_item == 4))
#CP-knowers' large ~ Task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "CP" & 
                                            Task_item == 6))
#CP-knowers' large ~ Task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "CP" & 
                                            Task_item == 8))
#CP-knowers' large ~ Task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "CP" & 
                                            Task_item == 10))
#CP-knowers' small ~ task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "Subset" & 
                                            Task_item == 3))
#CP-knowers' small ~ task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "Subset" & 
                                            Task_item == 4))
#CP-knowers' large ~ Task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "Subset" & 
                                            Task_item == 6))
#CP-knowers' large ~ Task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "Subset" & 
                                            Task_item == 8))
#CP-knowers' large ~ Task
leveneTest(Response ~ Task, data = subset(var.df, 
                                          CP_subset == "Subset" & 
                                            Task_item == 10))


# ... follow-up: what is the modal response for each set size and kl? ----
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

var.df %>%
  group_by(Task, CP_subset, Task_item)%>%
  summarise(mode = getmode(Response))

# Counting proficiency ----
## visualization of counting proficiency score by KL
all.data %>%
  filter(!is.na(count_proficiency))%>%
  distinct(SID, CP_subset, count_proficiency)%>%
  mutate(count_proficiency.group = cut(count_proficiency, seq(0, 3, 1), include.lowest = TRUE), 
         count_proficiency.group = factor(count_proficiency.group, labels = 
                                            c("Random counters", 
                                              "Minimal counters", 
                                              "Proficient counters")))%>%
  group_by(CP_subset, count_proficiency.group)%>%
  summarise(n = n()) %>%
  ggplot(aes(x = CP_subset, y = n, fill = count_proficiency.group)) + 
  geom_bar(stat = 'identity')

##Does counting proficiency predict accuracy? Pre-registered w/ only subset knowers --- 
count.prof.df <- model.df %>%
  filter(!is.na(count_proficiency.c))%>%
  mutate(count_proficiency.group = cut(count_proficiency, seq(0, 3, 1), include.lowest = TRUE), 
         count_proficiency.group = factor(count_proficiency.group, labels = 
                                            c("Random counters", 
                                              "Minimal counters", 
                                              "Proficient counters")))

count.prof.df %>%
  distinct(SID, CP_subset, count_proficiency.group)%>%
  group_by(CP_subset, count_proficiency.group)%>%
  summarise(n = n())
  

count.prof.base <- glmer(Correct ~ Task_item.c + Task + age.c + (1|SID), 
                         family = 'binomial', 
                         data = subset(count.prof.df, CP_subset == "Subset"), 
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e4)))
count.prof.count <- glmer(Correct ~ count_proficiency.group + Task_item.c + Task + age.c + (1|SID), 
                         family = 'binomial', 
                         data = subset(count.prof.df, CP_subset == "Subset"), 
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e4)))
count.prof.int <- glmer(Correct ~ count_proficiency.group*Task_item.c + Task + age.c + (1|SID), 
                         family = 'binomial', 
                         data = subset(count.prof.df, CP_subset == "Subset"), 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
anova(count.prof.base, count.prof.count, count.prof.int, test = 'lrt') #no significant effect of counting proficiency

#now try with CP knowers
count.prof.base <- glmer(Correct ~ Task_item.c + Task + age.c + (1|SID), 
                         family = 'binomial', 
                         data = subset(count.prof.df, CP_subset == "CP"), 
                         control=glmerControl(optimizer="bobyqa",
                                              optCtrl=list(maxfun=2e4)))
count.prof.count <- glmer(Correct ~ count_proficiency.group + Task_item.c + Task + age.c + (1|SID), 
                          family = 'binomial', 
                          data = subset(count.prof.df, CP_subset == "CP"), 
                          control=glmerControl(optimizer="bobyqa",
                                               optCtrl=list(maxfun=2e4)))
count.prof.int <- glmer(Correct ~ count_proficiency.group*Task_item.c + Task + age.c + (1|SID), 
                        family = 'binomial', 
                        data = subset(count.prof.df, CP_subset == "CP"), 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
anova(count.prof.base, count.prof.count, count.prof.int, test = 'lrt') #no significant effect of counting proficiency


# Highest count ----
#Do children with higher verbal counts have better set-matching accuracy?
## subset knowers
hc.df <- model.df %>%
  filter(!is.na(highest_count.c))

subset.hc.base <- glmer(Correct ~ Task_item.c + Task + age.c + (1|SID), 
                     family = 'binomial', 
                     data = subset(hc.df, CP_subset == "Subset"), 
                     control=glmerControl(optimizer="bobyqa",
                                          optCtrl=list(maxfun=2e4)))
subset.hc.hc <- glmer(Correct ~ highest_count.c + Task_item.c + Task + age.c + (1|SID), 
                        family = 'binomial', 
                        data = subset(hc.df, CP_subset == "Subset"), 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
anova(subset.hc.base, subset.hc.hc, test = 'lrt') #nope

#now cp- knowers
cp.hc.base <- glmer(Correct ~ Task_item.c + Task + age.c + (1|SID), 
                        family = 'binomial', 
                        data = subset(hc.df, CP_subset == "CP"), 
                        control=glmerControl(optimizer="bobyqa",
                                             optCtrl=list(maxfun=2e4)))
cp.hc.hc <- glmer(Correct ~ highest_count.c + Task_item.c + Task + age.c + (1|SID), 
                      family = 'binomial', 
                      data = subset(hc.df, CP_subset == "CP"), 
                      control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e4)))
anova(cp.hc.base, cp.hc.hc, test = 'lrt')

# Counting attempts ----
## Do children who attempt to count do better?


# Error ----
# ... Descriptives: overall absolute error grouped by CP knower status, task, and numerosity ----
error.df %>%
  group_by(CP_subset, Task, Numerosity)%>%
  summarise_at('abs.error', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T)))%>%
  dplyr::select(CP_subset, Task, Numerosity, mean, sd)

# ...visualization: overall error ----
error.df %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4", 
                                                  "6", "8", "10")))%>%
  group_by(Numerosity, Task, Task_item, CP_subset)%>%
  langcog::multi_boot_standard("abs.error", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group = interaction(Numerosity, CP_subset))) +
  geom_point(size = 2) + 
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 width = .1) +
  theme_bw(base_size = 15) + 
  facet_grid(~factor(Task, levels = c("Parallel", "Orthogonal")), scale = "free_x") +
  theme(legend.position = "right", 
        panel.grid = element_blank(), 
        legend.title = element_blank()) +
  labs(x = "Set size", y = "Mean absolute error") +
  scale_colour_manual(values = cp.sub.palette) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  labs(color = "Knower Level")

ggsave("Figures/mean_abs_error.png", width = 5.8, height = 3.5)

# ... visualization: Error by condition ----
error.df %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4", "5", 
                                                  "6", "7", "8", "9", "10")))%>%
  group_by(Numerosity, Task, Task_item, CP_subset, Condition)%>%
  langcog::multi_boot_standard("abs.error", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = CP_subset, group = interaction(Numerosity, CP_subset))) +
  geom_point(size = 2) + 
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 width = .1) +
  theme_bw(base_size = 15) + 
  facet_grid(factor(Task, levels = c("Parallel", "Orthogonal")) ~ Condition, scale = "free_x") +
  theme(legend.position = "right", 
        panel.grid = element_blank(), 
        legend.title = element_blank()) +
  labs(x = "Set size", y = "Mean absolute error") +
  scale_colour_manual(values = cp.sub.palette) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  labs(color = "Knower Level")

# ...models: Do CP-knowers have overall lower error? ----
error.model.df <- error.df %>%
  mutate(Task_item.c = as.vector(scale(Task_item, center = TRUE, scale=TRUE)),
         count_proficiency.c = as.vector(scale(count_proficiency, center = TRUE, scale = TRUE)),
         CP_subset = factor(CP_subset, levels = c("Subset", "CP")))

#base model
overall.error.base <- lmer(abs.error ~ Task_item.c + Task + age.c + (1|SID), 
                           data = error.model.df)

#add kl
overall.error.kl <- lmer(abs.error ~ CP_subset + Task_item.c + Task + age.c + (1|SID), 
                         data = error.model.df)
#compare - does CP knower status explain variance above age?
anova(overall.error.base, overall.error.kl, test = 'lrt') # yes, p = .002

#add interaction
overall.error.int <- lmer(abs.error ~ CP_subset*Task_item.c + Task + age.c + (1|SID), 
                          data = error.model.df)
anova(overall.error.base, overall.error.kl, overall.error.int, test = 'lrt')
tidy(overall.error.kl, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

#multiple comparisons
error.group <- lmer(abs.error ~ CP_subset + Numerosity + Task + 
                           age.group.floor + (1|SID), 
                         data=error.model.df)
emmeans::emmeans(error.group, list(pairwise ~ CP_subset*Numerosity*Task), adjust = 'tukey') 

# ... follow up: is there a three-way interaction with orientation? ----
follow.3way.error.base <- lmer(abs.error ~ CP_subset + Task + Task_item.c + age.c + 
                                 (1|SID), 
                               data = error.model.df)
follow.3way.error.2int <- lmer(abs.error ~ CP_subset *Task + Task_item.c + age.c + 
                                 (1|SID), 
                               data = error.model.df)
follow.3way.error.3int <- lmer(abs.error ~ CP_subset *Task *Task_item.c + age.c + 
                                 (1|SID), 
                               data = error.model.df)
anova(follow.3way.error.base, follow.3way.error.2int, follow.3way.error.3int, 
      test = 'lrt') #nope, no 3-way interaction; overall lower error across the board for cp-knowers

##Does error differ as a function of condition?
#base model
overall.error.cond.base <- lmer(abs.error ~ CP_subset + Task_item.c + Task + age.c + (1|SID), 
                                data = error.model.df)

#add kl
overall.error.cond.kl <- lmer(abs.error ~ Condition + CP_subset + Task_item.c + Task + age.c + (1|SID), 
                              data = error.model.df)
#compare
anova(overall.error.cond.base, overall.error.cond.kl, test = 'lrt') #no main effect

#add interaction
overall.error.cond.int <- lmer(abs.error ~ Condition*CP_subset+ Task_item.c + Task + age.c + (1|SID), 
                               data = error.model.df)

car::Anova(overall.error.cond.int) #but a significant interaction, p = .001
tidy(overall.error.cond.int, conf.int=T) %>% #coefficients, cis, and p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

#multiple comparisons
error.group <- lmer(abs.error ~ CP_subset + Numerosity + Condition + 
                      age.group.floor + (1|SID), 
                    data=subset(error.model.df, Task == "Parallel"))
emmeans::emmeans(error.group, list(pairwise ~ CP_subset*Numerosity*Condition), adjust = 'tukey') 

# COV ----
#for each participant
#hardcode n as 5, no participant has more or less

#this data frame is approximating a single COV for each subject
cov.df <- all.data %>%
  dplyr::select(SID, Age, CP_subset, Task, Task_item, Response, Numerosity)%>%
  mutate(cov = sqrt((Task_item - Response)^2)/Task_item)

# ...descriptives: summary of COV by task, CP-knower status
cov.df %>%
  distinct(SID, CP_subset, Task, cov)%>%
  group_by(CP_subset, Task)%>%
  summarise_at('cov', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(n, mean, sd, median, min, max)


#correlation for CP across tasks
cor.test(subset(cov.df, CP_subset == "CP" & Task == "Parallel")$cov, 
         subset(cov.df, CP_subset == "CP" & Task == "Orthogonal")$cov)

#correlation for subset across tasks
cor.test(subset(cov.df, CP_subset == "Subset" & Task == "Parallel")$cov, 
         subset(cov.df, CP_subset == "Subset" & Task == "Orthogonal")$cov)

ggplot(cov.df, aes(x = Task_item, y = cov, color = CP_subset, group = CP_subset)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_grid(~Task)

cov.df %>%
  group_by(Task, Task_item, CP_subset)%>%
  summarise(mean_COV = mean(cov, na.rm = TRUE)) %>% #not sure why there are NAs right now - fix
  ggplot(aes(x = Task_item, y = mean_COV, color = CP_subset, group = CP_subset)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(~Task)


