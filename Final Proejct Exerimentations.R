library(snakecase)
library(lmerTest)
library(sjPlot)
library(MASS)
library(car)
library(ggplot2)
library(viridis)
library(hrbrthemes)

set.seed(123)

survey_data = read.csv("responses_transformed.csv")


# Mixed model

survey_data_exp = survey_data[survey_data$experimental == 0, ]
survey_data_ctr = survey_data[survey_data$experimental == 1, ]
survey_data_ctr_shuffle  = survey_data_ctr
survey_data_ctr_shuffle$word2_binary = sample(survey_data_ctr_shuffle$word2_binary, replace =
                                                FALSE)

survey_data = rbind(survey_data_exp, survey_data_ctr_shuffle)

survey_data_chi_set = survey_data[survey_data$experimental == 0,]

#Pearson’s Chi-squared test
data_table = table(survey_data$word1_binary, survey_data$word2_binary)
chisq.test(data_table)

# Lmer test
expModel = lmer(word2_binary ~ word1_binary*experimental + (1|p), data=survey_data)
summary(expModel)

Anova(expModel)

plot_model(expModel, type = "pred", terms = c("word1_binary", "experimental"))

stem_length_dform = length(survey_data_exp[survey_data_exp["word1_binary"] ==
                                             0])
stem_length_bareform = length(survey_data_exp[survey_data_exp["word1_binary"] ==
                                                1])
dimin_length_dform = length(survey_data_exp[survey_data_exp["word2_binary"] ==
                                              0])
dimin_length_bareform = length(survey_data_exp[survey_data_exp["word2_binary"] ==
                                                 1])
stem_length_dform
stem_length_bareform
dimin_length_dform
dimin_length_bareform