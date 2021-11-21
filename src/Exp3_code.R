#Last update: 2021/11/21 15:04

library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

raw_dat <- read.csv("../data/Exp3_data.csv")

#check age and sex
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
# create bar plot #
###################

#prepare data for bar plot
dat_participant_mean <- raw_dat %>%
  dplyr::select(-P_ID, -Sex, -Age) %>%
  tidyr::gather(key = Questions, value = Value,
                -ID) %>% 
  tidyr::drop_na() %>%
  tidyr::separate(col = Questions, into = c("Question", "FeatureCondition", "Type", "Dummy"), sep="[_.]") %>%
  dplyr::mutate(Value = Value,
                FeatureCondition = as.factor(FeatureCondition),
                Question = as.factor(Question),
                Type = as.factor(Type),
                ID = as.factor(ID),
                Dummy = as.factor(Dummy)) %>%
  dplyr::group_by(ID, FeatureCondition, Type) %>%
  dplyr::summarise(Average = mean(Value))

#concatenate SV1A and SV1B(counterbalanced list)
dat_participant_mean$FCComb = dat_participant_mean$FeatureCondition
dat_participant_mean$FCComb = factor(dat_participant_mean$FCComb, levels = c("S2", "SV1A", "SV1B", "V2"),
                                     labels=c("S2", "SV1", "SV1", "V2"))

dat_participant_mean$FCCombRev = dat_participant_mean$FCComb
dat_participant_mean$FCCombRev = factor(dat_participant_mean$FCComb, levels = c("S2", "SV1", "V2"),
                                     labels=c("S2", "ST1", "T2"))

dat_participant_graph <- dat_participant_mean %>%
  dplyr::group_by(FCCombRev, Type) %>%
  dplyr::summarise(Mean = mean(Average), SD = sd(Average))

#change names of lagbels
dat_participant_graph <- dat_participant_graph %>% 
  dplyr::mutate(MetaphorType = as.factor(case_when(Type == "M" ~ "Metaphor",
                                                   Type == "S" ~ "Simile")),
                TypeOfFeature = as.factor(case_when(FCCombRev == "S2" ~ "Two topic-vehicle shared",
                                                    FCCombRev == "ST1" ~ "One topic-vehicle shared\n and one topic unique",
                                                    FCCombRev == "T2" ~ "Two topic unique"))
  )

#create bar plot
ggplot()+theme_set(theme_classic(base_size = 12,base_family="Hiragino Mincho Pro W3"))
g<-ggplot(dat_participant_graph, aes(y=Mean, x=TypeOfFeature, fill=MetaphorType))
g <- g + geom_bar(position = "dodge", stat = "identity")
g <- g + geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2, position = position_dodge(.9))
g <- g + scale_y_continuous(name = "Mean aptness rating", breaks = seq(0, 6, length=7), limits = c(0, 6))
g <- g + scale_x_discrete(name = "Type of topic-attributed features")
g <- g + labs(fill = "Metaphor type")
plot(g)

##################
# analyze result #
##################

#prepare data for mixed effect model
dat_lmer <- raw_dat %>% 
  dplyr::select(-P_ID, -Sex, -Age) %>%
  tidyr::gather(key = Questions, value = Value,
                -ID) %>% 
  tidyr::drop_na() %>%
  tidyr::separate(col = Questions, into = c("Question", "FeatureCondition", "Type", "Dummy"), sep="[_.]") %>%
  dplyr::mutate(Value = Value,
                FeatureCondition = as.factor(FeatureCondition),
                Question = as.factor(Question),
                Type = as.factor(Type),
                ID = as.factor(ID),
                Dummy = as.factor(Dummy))

#concatenate SV1A and SV1B(counterbalanced list)
dat_lmer$FCComb = dat_lmer$FeatureCondition
dat_lmer$FCComb = factor(dat_lmer$FCComb, levels = c("S2", "SV1A", "SV1B", "V2"),
                                     labels=c("S2", "ST1", "ST1", "T2"))

#prepare contrasts for deviation coding
dat_lmer$ffcomb.dev = dat_lmer$FCComb
contrasts(dat_lmer$ffcomb.dev) = contr.sum(3)/2

dat_lmer$type.dev = dat_lmer$Type
contrasts(dat_lmer$type.dev) = contr.sum(2)/2

res_lmer_dat_dev <- lmer(formula = Value ~ ffcomb.dev*type.dev + (1 + ffcomb.dev|ID) + (1 + ffcomb.dev + type.dev|Question),
                         data = dat_lmer)
summary(res_lmer_dat_dev)

