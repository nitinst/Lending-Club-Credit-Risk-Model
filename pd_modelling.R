library(stats)
library(broom)
library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)
library(lubridate)
library(caret)
library(ggplot2)
library(data.table)

train_data_trnsfrmd <- train_data_trnsfrmd %>%
  select(-contains(pull(IV_df %>% filter(IV_value < 0.01) %>% select(WoE_col))))
test_data_trnsfrmd <- test_data_trnsfrmd %>%
  select(-contains(pull(IV_df %>% filter(IV_value < 0.01) %>% select(WoE_col))))

glimpse(train_data_trnsfrmd)
train_data_trnsfrmd <- train_data_trnsfrmd %>% mutate_if(is.character,as.factor)
summary(train_data_trnsfrmd)

model <- glm(good_bad ~.,family=binomial(link='logit'),data=train_data_trnsfrmd)
summary(model)

source('validation.R')
source('score_card.R')