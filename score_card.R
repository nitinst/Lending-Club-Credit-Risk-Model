test <- dummy.coef(model)
score_card_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(score_card_df) <- c('col+category', 'col', 'category', 'regression_coeff')
for (i in seq_along(test)){
  for (j in seq_along(test[[i]])){
    col_category <- paste0(names(test[i]), names(test[[i]][j]))
    correspndng_col <- names(test[i])
    correspndng_category <- names(test[[i]][j])
    reg_coeff <- test[[i]][[j]]
    score_card_df[nrow(score_card_df) + 1,] = list(col_category, correspndng_col, correspndng_category, reg_coeff)
  }
}

######### score card preparation ########
# library(gtsummary)
# tbl_regression(model, exponentiate = TRUE)

p_values_df <- stack(coef(summary(model))[,4])
coeff_df <- stack(coefficients(model))
col_names <- colnames(train_data_trnsfrmd)

score_card_df %>%
  group_by(col) %>%
  summarise(max(regression_coeff))

max_sum_coef <- sum(pull(score_card_df %>%
                           group_by(col) %>%
                           summarise(regression_coeff = max(regression_coeff)) %>%
                           select(regression_coeff)))

min_sum_coef <- sum(pull(score_card_df %>%
                           group_by(col) %>%
                           summarise(regression_coeff = min(regression_coeff)) %>%
                           select(regression_coeff)))

max_score <- 850
min_score <- 300
score_card_df <- score_card_df %>%
  mutate(score_calc = regression_coeff * (max_score - min_score) / (max_sum_coef - min_sum_coef)) %>%
  mutate(score_calc = ifelse(category == '(Intercept)', 
                             (regression_coeff - min_sum_coef)/(max_sum_coef - min_sum_coef) * 
                               (max_score - min_score) + min_score, score_calc)) %>%
  mutate(score_preliminary = round(score_calc))

max_sum_score_prelimnry <- sum(pull(score_card_df %>%
                                      group_by(col) %>%
                                      summarise(score_preliminary = max(score_preliminary)) %>%
                                      select(score_preliminary)))

min_sum_score_prelimnry <- sum(pull(score_card_df %>%
                                      group_by(col) %>%
                                      summarise(score_preliminary = min(score_preliminary)) %>%
                                      select(score_preliminary)))


######### score card preparation ########
# library(gtsummary)
# tbl_regression(model, exponentiate = TRUE)

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


score_card_final <- score_card_df %>%
  select(col, category, score_preliminary)

colnames(score_card_final) <- c('Feature', 'Category', 'Credit Score')
