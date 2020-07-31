library(stats)
library(broom)
# source('data_preparation.R')

train_data_trnsfrmd <- train_data_trnsfrmd %>%
  select(-contains(pull(IV_df %>% filter(IV_value < 0.02) %>% select(WoE_col))))
test_data_trnsfrmd <- test_data_trnsfrmd %>%
  select(-contains(pull(IV_df %>% filter(IV_value < 0.02) %>% select(WoE_col))))

glimpse(train_data_trnsfrmd)
train_data_trnsfrmd <- train_data_trnsfrmd %>% mutate_if(is.character,as.factor)
summary(train_data_trnsfrmd)

model <- glm(good_bad ~.,family=binomial(link='logit'),data=train_data_trnsfrmd)

# model2 <- lm(good_bad ~.,family=binomial(link='logit'),data=train_data_trnsfrmd)
# model <- model2

summary(model)

# drop these: open_acc_trnsfrmd, pub_rec_trnsfrmd, total_acc_trnsfrmd, delinq_2yrs_trnsfrmd
# train_data_trnsfrmd <- train_data_trnsfrmd %>%
#   select(-c(open_acc_trnsfrmd, pub_rec_trnsfrmd, total_acc_trnsfrmd, delinq_2yrs_trnsfrmd))
# test_data_trnsfrmd <- test_data_trnsfrmd %>%
#   select(-c(open_acc_trnsfrmd, pub_rec_trnsfrmd, total_acc_trnsfrmd, delinq_2yrs_trnsfrmd))
# model <- glm(good_bad ~.,family=binomial(link='logit'),data=train_data_trnsfrmd)
# ### validation


test <- dummy.coef(model)
ref_var <- list()
for (var in test){
  ref_var <- append(ref_var, as.character(names(var[1])))
}
# 
# significant_vars <- p_values_df %>%
#   filter(values <= 0.005)
# 
# train_data_reduced <- train_data_trnsfrmd %>%
#   mutate_all(paste0())

library(ROCR)
predicted <- plogis(predict(model, test_data_trnsfrmd))  # predicted
library(InformationValue)
optCutOff <- optimalCutoff(test_data_trnsfrmd$good_bad, predicted)[1] + 0.2
misClassError(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
plotROC(test_data_trnsfrmd$good_bad, predicted)
AUROC_value <- AUROC(test_data_trnsfrmd$good_bad, predicted)
# Concordance(test_data_trnsfrmd$good_bad, predicted)
sensitivity(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
specificity(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
confusionMatrix(test_data_trnsfrmd$good_bad, predicted, threshold = optCutOff)
gini_coef <- 2 * AUROC_value - 1

ks_plot(test_data_trnsfrmd$good_bad, predicted)
ks_stat(test_data_trnsfrmd$good_bad, predicted)

######### score card preparation ########
library(gtsummary)
tbl_regression(model, exponentiate = TRUE)

p_values_df <- stack(coef(summary(model))[,4])
coeff_df <- stack(coefficients(model))
col_names <- colnames(train_data_trnsfrmd)

model1 <- model

score_coeff <- score_card_df %>%
  filter(regression_coeff != 0) %>%
  select(col, score_preliminary)

cat_names <- pull(score_coeff, col)
score_coeff <- pull(score_coeff, score_preliminary)
names(score_coeff) <- cat_names
  
model1$coefficients <- score_coeff
train_data_trnsfrmd <- train_data_trnsfrmd %>%
  mutate(credit_score = predict(model1, train_data_trnsfrmd))

test_data_trnsfrmd <- test_data_trnsfrmd %>%
  mutate(credit_score = predict(model1, test_data_trnsfrmd))

# coeff_df1 <- coeff_df %>%
#   spread(ind, values) %>%
#   mutate_at(vars(%in% col_names), funs (.== 'True'))

