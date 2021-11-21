#Last update：2021/11/21 13:50

#import libraires
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

raw_dat <- read.csv("../data/Exp1_data.csv")

#Check age and sex
#age
dat_age <- raw_dat %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))
print(dat_age)

dat_sex <- raw_dat %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())
print(dat_sex)

###################
# Create bar plot #
###################

#Sd stands for participants mean
dat_participant_mean <- raw_dat %>%
  dplyr::select(-P_ID, -Sex, -Age) %>%
  tidyr::gather(key = Questions, value = Value,
                -ID) %>% 
  tidyr::separate(col = Questions, into = c("Question", "Condition")) %>%
  tidyr::drop_na() %>%
  dplyr::group_by(ID, Condition) %>%
  dplyr::summarise(Average = mean(Value))
dat_participant_mean$ID = as.factor(dat_participant_mean$ID)
dat_participant_mean$Condition = factor(dat_participant_mean$Condition)


#prepare bar plot
dat_participant_graph <- dat_participant_mean %>%
  dplyr::group_by(Condition) %>%
  dplyr::summarise(Mean = mean(Average), SD = sd(Average))

#create bar plot
ggplot()+theme_set(theme_classic(base_size = 12,base_family="Hiragino Mincho Pro W3"))

g<-ggplot(dat_participant_graph, aes(y=Mean, x=Condition))
g <- g + geom_bar(position = "dodge", stat = "identity")
g <- g + geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2, position = position_dodge(.9))
g <- g + scale_y_continuous(name = "Mean aptness rating", breaks = seq(0, 6, length=7), limits = c(0, 6))
g <- g + scale_x_discrete(name = "Number of topic-vehicle shared features",
                          labels = c("S1" = "One shared feature", "S2" = "Two shared features"),
                          limits=c("S1", "S2"))
plot(g)

###################
# Analayze result #
###################

#prepare data for mixed effect model
dat_lmer <- raw_dat %>% 
  dplyr::select(-P_ID, -Sex, -Age) %>%
  tidyr::gather(key = Questions, value = Value,
                     -ID) %>% 
  tidyr::separate(col = Questions, into = c("Question", "Condition")) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(Value = Value,
                Condition = as.factor(Condition),
                Question = as.factor(Question),
                ID = as.factor(ID))

#prepare deviation coding
dat_lmer$Condition.dev = dat_lmer$Condition
contrasts(dat_lmer$Condition.dev) = contr.sum(2)/2
print(contrasts(dat_lmer$Condition.dev))

#analyze results
res_lmer_dat_dev <- lmer(formula = Value ~ Condition.dev + (1 + Condition.dev|ID) + (1 + Condition.dev|Question),
                         data = dat_lmer)

#because contrasts is based on S1: 0.5, S2: -0.5,
#the more the feature become, the more aptness increase
summary(res_lmer_dat_dev)
