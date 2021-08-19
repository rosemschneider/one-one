## One-one sharing (Study 4)
## Rose M. Schneider

## Run the cleaning script
source("Study 4/Analysis/0-clean.R")

## setup
rm(list = ls())
library(tidyverse)
library(langcog)
library(lme4)
library(tidylog)

## custom palette - sharing and matching
## For paper
exp.1.pal <- c('#2559ba', '#bf5d06')
# exp.1.talk.pal <- c("#c96406", "#185ea3")

## Custom theme set
theme_set(theme_bw() + theme(text = element_text(size=11),
                             axis.title=element_text(size=10),
                             strip.text = element_text(margin=margin(2,0,2,0)),
                             panel.grid = element_blank()))

## Read in the data ====
sharing.data <- read.csv("Study 4/Data/one-one_sharing_cleaned.csv")%>%
  mutate(Condition = "Study 3", 
         Response = ifelse(Response == 17, 15, as.numeric(Response)))%>% #one miscoded item
  filter(Task_item > 2) #all training trials should be removed, but just in case

baseline.data <- read.csv("Study 1/Data/one-one_cleaned.csv")%>% ## we need to read in the baseline data as well
  mutate(Condition = "Study 1")

## Sharing ----
### ...descriptive analyses ----
### Accuracy by numerosity 
sharing.data %>%
  group_by(CP_subset, Numerosity)%>%
  summarise(n = n(), 
            mean = mean(Correct, na.rm = TRUE), 
            sd = sd(Correct, na.rm = TRUE))

### Visualization of accuracy in both conditions
# first we need to create a data frame that has both sharing and matching
sharing.limited <- sharing.data %>%
  select(SID, Age, CP_subset, Task_item, Response, Correct, Numerosity, Condition)

baseline.limited <- baseline.data %>%
  filter(Task == "Parallel")%>%
  select(SID, Age, CP_subset, Task_item, Response, Correct, Numerosity, Condition)%>%
  mutate(Response = as.numeric(as.character(Response)))

full.df <- bind_rows(sharing.limited, baseline.limited)

## Accuracy visualization
full.df %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4",
                                                  "6", "8", "10")))%>%
  group_by(Numerosity, Condition, Task_item, CP_subset)%>%
  langcog::multi_boot_standard("Correct", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = Condition, group = interaction(Numerosity, Condition))) +
  geom_point(size = 2) +
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper),
                 width = .1) +
  facet_grid(~CP_subset, scale = "free_x") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "right") +
  labs(x = "Set size", y = "Mean accuracy") +
  scale_color_manual(values = exp.1.pal) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "right", 
        # legend.key.size = unit(.5,'cm'), 
        # legend.margin = unit(-0.6, 'cm'))
        )+
  labs(color= "Condition")
ggsave("Study 4/Analysis/Figures/accuracy_sharingMatching.png", width = 5.8, height = 2.5)

## Response distribution visualization
ggplot(baseline.limited, aes(x = Response)) +
  geom_vline(aes(xintercept = Task_item), linetype = "dashed", color = 'black') +
  geom_histogram(aes(y = ..density.., fill = "Study 1"), binwidth = 1, color = 'black', 
                 size = .3, alpha = .6) + 
  geom_histogram(data = sharing.limited, aes(y = ..density.., fill = 'Study 3'), color = 'black', 
                  binwidth = 1,  size = .3, alpha = .7) + #Matching next
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(), 
        legend.title = element_blank()) +
  scale_fill_manual(name = "Condition", 
                     values = c('Study 1' = "#2559ba", 'Study 3' = "#bf5d06")) +
  facet_grid(CP_subset ~ Task_item) +
  scale_x_continuous(breaks = c(1, 5, 10, 15)) +
  labs(x = 'Number of items given', y = 'Density')
ggsave("Study 4/Analysis/Figures/sharingMatching_distribution.png", width = 7, height = 3.5)


## ... accuracy analysis ----
full.df <- full.df %>%
  mutate(Task_item.c = as.vector(scale(Task_item, center = TRUE, scale=TRUE)),
         age.c = as.vector(scale(Age, center = TRUE, scale=TRUE))) # center and scale continuous variables

correct.baseline <- glmer(Correct ~ Task_item.c + CP_subset + age.c + (1|SID) + (1 | Task_item.c), 
                          family = "binomial", 
                          data = full.df)
correct.condition <- glmer(Correct ~ Condition + Task_item.c + CP_subset + age.c + (1|SID) + (1 | Task_item.c), 
                          family = "binomial", 
                          data = full.df)
correct.int <- glmer(Correct ~ Condition*Task_item.c + CP_subset + age.c + (1|SID) + (1 | Task_item.c), 
                           family = "binomial", 
                           data = full.df)
##test for significance
anova(correct.baseline, correct.condition, correct.int, test = 'lrt') # there is an effect of condition, but no interaction
summary(correct.condition) #children are less accurate on sharing relative to matching, p =

## ... Does sharing interact with KL? ---
kl.baseline <- correct.baseline <- glmer(Correct ~ Condition + CP_subset +Task_item.c + age.c + (1|SID)+ (1 | Task_item.c), 
                                         family = "binomial", 
                                         data = full.df)
kl.int <- correct.baseline <- glmer(Correct ~ Condition*CP_subset +Task_item.c + age.c + (1|SID)+ (1 | Task_item.c), 
                                         family = "binomial", 
                                         data = full.df)
anova(kl.baseline, kl.int, test = 'lrt') # nope!

## T-tests for follow-up
ms.accuracy <- full.df %>%
  group_by(SID, CP_subset, Numerosity, Condition)%>%
  summarise(mean = mean(Correct, na.rm = TRUE))

#CP-knowers - small numerosity
t.test(subset(ms.accuracy, CP_subset == "CP" & Numerosity == "Small" & Condition == "Study 3")$mean, 
       subset(ms.accuracy, CP_subset == "CP" & Numerosity == "Small" & Condition == "Study 1")$mean, var.equal = TRUE)
#CP-knowers - large numerosity
t.test(subset(ms.accuracy, CP_subset == "CP" & Numerosity == "Large" & Condition == "Study 3")$mean, 
       subset(ms.accuracy, CP_subset == "CP" & Numerosity == "Large" & Condition == "Study 1")$mean, var.equal = TRUE)

#subset-knowers - small numerosity
t.test(subset(ms.accuracy, CP_subset == "Subset" & Numerosity == "Small" & Condition == "Study 3")$mean, 
       subset(ms.accuracy, CP_subset == "Subset" & Numerosity == "Small" & Condition == "Study 1")$mean, var.equal = TRUE)
#CP-knowers - large numerosity
t.test(subset(ms.accuracy, CP_subset == "Subset" & Numerosity == "Large" & Condition == "Study 3")$mean, 
       subset(ms.accuracy, CP_subset == "Subset" & Numerosity == "Large" & Condition == "Study 1")$mean, var.equal = TRUE)

## Sharing analysis only: Accuracy between CP and subset ----
sharing.base <- glmer(Correct ~ Task_item.c + age.c + (1|SID)+ (1 | Task_item.c), 
                          family = 'binomial', data = subset(full.df, Condition == "Study 3"))
sharing.kl <- glmer(Correct ~ CP_subset + Task_item.c + age.c + (1|SID)+ (1 | Task_item.c), 
                    family = 'binomial', data = subset(full.df, Condition == "Study 3"))
anova(sharing.base, sharing.kl, test = 'lrt')
summary(sharing.kl)

## ... error analysis ----
error.df <- full.df %>%
  filter(Correct == 0)%>% #get only incorrect
  mutate(abs.error = abs(Task_item - Response))

## Visualize
error.df %>%
  mutate(Task_item = factor(Task_item, levels = c("3", "4",
                                                  "6", "8", "10")))%>%
  group_by(Numerosity, Condition, Task_item, CP_subset)%>%
  langcog::multi_boot_standard("abs.error", na.rm = TRUE)%>%
  ggplot(aes(x = Task_item, y = mean, colour = Condition, group = interaction(Numerosity, Condition))) +
  geom_point(size = 2) +
  geom_line() +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper),
                 width = .1) +
  facet_grid(~CP_subset, scale = "free_x") +
  theme_bw(base_size =10)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = c(.37, .77), 
        legend.key.size = unit(.4,'cm'), 
        legend.margin = unit(-0.6, 'cm'), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) +
  labs(x = "Set size", y = "Mean absolute error") +
  scale_color_manual(values = exp.1.pal) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color= "Condition")

### Does error differ by condition??
error.baseline <- lmer(abs.error ~ Task_item.c + CP_subset + age.c + (1|SID)+ (1 | Task_item.c), 
                       data = error.df)
error.condition <- lmer(abs.error ~ Condition + Task_item.c + CP_subset + age.c + (1|SID)+ (1 | Task_item.c), 
                        data = error.df)
error.int <- lmer(abs.error ~ Condition*Task_item.c + CP_subset + age.c + (1|SID)+ (1 | Task_item.c), 
     data = error.df)
anova(error.baseline, error.condition, error.int, test = 'lrt')
summary(error.int) # I actually think that this interaction is an artifact of design??
summary(error.condition)

error.condition <- lmer(abs.error ~ Condition + CP_subset + Task_item.c + age.c + (1|SID)+ (1 | Task_item.c), 
                        data = error.df)

## get those p values
coefs <- data.frame(coef(summary(error.condition)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

## T-tests for follow-up
ms.error <- error.df %>%
  group_by(SID, CP_subset, Numerosity, Condition)%>%
  summarise(mean = mean(abs.error, na.rm = TRUE))

#CP-knowers - small numerosity
t.test(subset(ms.error, CP_subset == "CP" & Numerosity == "Small" & Condition == "Study 3")$mean, 
       subset(ms.error, CP_subset == "CP" & Numerosity == "Small" & Condition == "Study 1")$mean, var.equal = TRUE)
#CP-knowers - large numerosity
t.test(subset(ms.error, CP_subset == "CP" & Numerosity == "Large" & Condition == "Study 3")$mean, 
       subset(ms.error, CP_subset == "CP" & Numerosity == "Large" & Condition == "Study 1")$mean, var.equal = TRUE)

#subset-knowers - small numerosity
t.test(subset(ms.error, CP_subset == "Subset" & Numerosity == "Small" & Condition == "Study 3")$mean, 
       subset(ms.error, CP_subset == "Subset" & Numerosity == "Small" & Condition == "Study 1")$mean, var.equal = TRUE)
#CP-knowers - large numerosity
t.test(subset(ms.error, CP_subset == "Subset" & Numerosity == "Large" & Condition == "Study 3")$mean, 
       subset(ms.error, CP_subset == "Subset" & Numerosity == "Large" & Condition == "Study 1")$mean, var.equal = TRUE)



