
calculate_weight_of_evidence_discrete <- function(train_data, WoE_col, target_col) {
  df_input <- train_data
  df1 <- df_input %>%
    select_at(c(WoE_col, target_col))
  df_n_obs <- df1 %>% group_by_at(WoE_col) %>% summarise(n_obs = n())
  
  df_good_prop <- df1 %>% group_by_at(WoE_col) %>% summarise(prop_good = mean(get(target_col)))
  # eval(as.name(target_col)))
  df_final <- df_n_obs %>%
    left_join(df_good_prop)
  
  df_final$prop_n_obs <- df_final$n_obs / sum(df_final$n_obs)
  df_final$n_good <- df_final$prop_good * df_final$n_obs
  df_final$n_bad <- (1-df_final$prop_good) * df_final$n_obs
  df_final$prop_n_good <- df_final$n_good / sum(df_final$n_good)
  df_final$prop_n_bad <- df_final$n_bad / sum(df_final$n_bad)
  df_final$WoE <- log(df_final$prop_n_good / df_final$prop_n_bad)
  df_final <- df_final %>%
    arrange(WoE)
  df_final <- df_final %>%
    mutate(diff_prop_good = prop_good - lag(prop_good, default = first(prop_good)),
           diff_WoE = WoE - lag(WoE, default = first(WoE)))
  df_final$WoE <- replace_na(df_final$WoE, 0)
  df_final$WoE <- ifelse(is.finite(df_final$WoE), df_final$WoE, 0)
  df_final <- df_final %>%
    mutate(IV = sum((prop_n_good - prop_n_bad) * WoE))
  print(paste0(WoE_col, " IV is: ", distinct(df_final, IV)))
  df_final <- df_final %>%
    arrange(WoE) %>%
    mutate(prop_obs = n_obs / sum(n_obs))
}

plot_woe_discrete <- function(plot_data, x_axis, x_label, y_scale, angl) {
  ggplot(data = plot_data, aes(x = reorder(get(x_axis), WoE))) + 
    geom_line(aes(y = WoE, group = 1), color = 'blue') + 
    geom_point(aes(y = WoE, group = 1), color = 'blue') + 
    geom_bar(aes(y = prop_obs / y_scale, group = 1), stat="identity", size=.1, color = 'red', fill = "#69b3a2", alpha=.4) +
    theme(
      axis.title.y = element_text(color = 'blue', size=13),
      axis.title.y.right = element_text(color = 'red', size=13)
    ) + 
    theme(axis.text.x = element_text(angle = angl, vjust = 0.5, hjust=1)) +
    scale_y_continuous(sec.axis = sec_axis(~.*y_scale, name = "Observation proportion")) + 
    scale_colour_manual(values = c("blue", "red")) + 
    labs(y = "Weight of evidence", x = x_label)
}

IV_df <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(IV_df) <- c('WoE_col', 'IV_value')
## grade
grade_woe <- calculate_weight_of_evidence_discrete(train_data, "grade", "good_bad")
IV_df[nrow(IV_df) + 1,] <- list("grade", distinct(grade_woe, IV))
plot_woe_discrete(grade_woe, 'grade', 'Grade', 1, 0)
train_data$grade_trnsfrmd <- train_data$grade

## home_ownership
distinct(train_data, home_ownership)
home_ownership_woe <- calculate_weight_of_evidence_discrete(train_data, "home_ownership", "good_bad")
IV_df[nrow(IV_df) + 1,] <- list("home_ownership", distinct(home_ownership_woe, IV))
IV_df[nrow(IV_df) + 1,] <- list("grade", distinct(home_ownership_woe, IV))
plot_woe_discrete(home_ownership_woe, 'home_ownership', 'Home Ownership', 5, 0)
combine_home_ownership <- c("RENT", "OTHER", "NONE", "ANY")
train_data$home_ownership_trnsfrmd <- ifelse(train_data$home_ownership %in% combine_home_ownership, "RENT_OTHER_NONE_ANY", train_data$home_ownership)


state_woe <- calculate_weight_of_evidence_discrete(train_data, "addr_state", "good_bad")
IV_df[nrow(IV_df) + 1,] <- list("addr_state", distinct(state_woe, IV))
plot_woe_discrete(state_woe %>% slice(2:(n()-1)), 'addr_state', 'State', 0.2, 0)

state1 <- c("NV", "NE", "IA", "ME")
state2 <- c("HI", "FL", "AL", "LA")
state3 <- c("NY")
state4 <- c("NC", "MD", "NM", "VA", "NJ", "UT", "MO")
state5 <- c("CA")
state6 <- c("AZ", "AR", "MI")
state7 <- c("OK", "TN", "PA")
state8 <- c("DE", "MA", "RI", "KY", "MN", "OH", "SD", "IN", "OR", "GA", "WA", "ID", "WI", "MT")
state9 <- c("TX")
state10 <- c("VT", "CT", "IL", "AK", "SC", "CO", "KS", "MS")
state11 <- c("NH", "WV", "WY")
state12 <- c("DC")

train_data$addr_state_trnsfrmd <- ifelse(train_data$addr_state %in% state1, "state_list_1",
                                    ifelse(train_data$addr_state %in% state2, "state_list_2",
                                           ifelse(train_data$addr_state %in% state3, "state_list_3",
                                                  ifelse(train_data$addr_state %in% state4, "state_list_4",
                                                         ifelse(train_data$addr_state %in% state5, "state_list_5",
                                                                ifelse(train_data$addr_state %in% state6, "state_list_6",
                                                                       ifelse(train_data$addr_state %in% state7, "state_list_7",
                                                                              ifelse(train_data$addr_state %in% state8, "state_list_8",
                                                                                     ifelse(train_data$addr_state %in% state9, "state_list_9",
                                                                                            ifelse(train_data$addr_state %in% state10, "state_list_10",
                                                                                                   ifelse(train_data$addr_state %in% state11, "state_list_11",
                                                                                                          ifelse(train_data$addr_state %in% state12, "state_list_12",NaN))))))))))))


distinct(loan_data, verification_status)
verification_status_woe <- calculate_weight_of_evidence_discrete(train_data, "verification_status", "good_bad")
IV_df[nrow(IV_df) + 1,] <- list("verification_status", distinct(verification_status_woe, IV))
plot_woe_discrete(verification_status_woe, 'verification_status', 'Verification Status', 5, 0)
train_data$verification_status_trnsfrmd <- ifelse(train_data$verification_status %in% c("Not Verified", "Source Verified"),
                                                  "Not or Source Verified",
                                                  train_data$verification_status)


distinct(loan_data, purpose)
purpose_woe <- calculate_weight_of_evidence_discrete(train_data, "purpose", "good_bad")
IV_df[nrow(IV_df) + 1,] <- list("purpose", distinct(purpose_woe, IV))
plot_woe_discrete(purpose_woe, 'purpose', 'Purpose', 2, 0)

purpose1 <- c("small_business", "educational", "renewable_energy", "moving")
purpose2 <- c("other", "medical", "house")
purpose3 <- c("debt_consolidation", "vacation", "home_improvement")
purpose4 <- c("wedding", "car", "major_purchase", "credit_card")
train_data$purpose_trnsfrmd <- ifelse(train_data$purpose %in% purpose1, "purpose_list_1",
                                      ifelse(train_data$purpose %in% purpose2, "purpose_list_2",
                                             ifelse(train_data$purpose %in% purpose3, "purpose_list_3",
                                                    ifelse(train_data$purpose %in% purpose4, "purpose_list_4", NaN))))

init_list_status_woe <- calculate_weight_of_evidence_discrete(train_data, "initial_list_status", "good_bad")
IV_df[nrow(IV_df) + 1,] <- list("initial_list_status", distinct(init_list_status_woe, IV))
plot_woe_discrete(init_list_status_woe, 'initial_list_status', 'Initial List Status', 2, 0)
train_data$initial_list_status_trnsfrmd <- train_data$initial_list_status

calculate_weight_of_evidence_continuous <- function(train_data, WoE_col, target_col) {
  df_input <- train_data
  df1 <- df_input %>%
    select_at(c(WoE_col, target_col))
  df_n_obs <- df1 %>% group_by_at(WoE_col) %>% summarise(n_obs = n())
  
  df_good_prop <- df1 %>% group_by_at(WoE_col) %>% summarise(prop_good = mean(get(target_col)))
  # eval(as.name(target_col)))
  df_final <- df_n_obs %>%
    left_join(df_good_prop)
  
  df_final$prop_n_obs <- df_final$n_obs / sum(df_final$n_obs)
  df_final$n_good <- df_final$prop_good * df_final$n_obs
  df_final$n_bad <- (1-df_final$prop_good) * df_final$n_obs
  df_final$prop_n_good <- df_final$n_good / sum(df_final$n_good)
  df_final$prop_n_bad <- df_final$n_bad / sum(df_final$n_bad)
  df_final$WoE <- log(df_final$prop_n_good / df_final$prop_n_bad)
  df_final <- df_final %>%
    arrange(WoE)
  df_final <- df_final %>%
    mutate(diff_prop_good = prop_good - lag(prop_good, default = first(prop_good)),
           diff_WoE = WoE - lag(WoE, default = first(WoE)))
  df_final$WoE <- replace_na(df_final$WoE, 0)
  df_final$WoE <- ifelse(is.finite(df_final$WoE), df_final$WoE, 0)
  df_final <- df_final %>%
           mutate(IV = sum((prop_n_good - prop_n_bad) * WoE))
  print(paste0(WoE_col, " IV is: ", distinct(df_final, IV)))
  df_final <- df_final %>%
    arrange_at(WoE_col) %>%
    mutate(prop_obs = n_obs / sum(n_obs))
}

plot_woe_continuous <- function(plot_data, x_axis, x_label, y_scale) {
  ggplot(data = plot_data, aes(x = get(x_axis), WoE)) + 
    geom_line(aes(y = WoE, group = 1), color = 'blue') + 
    geom_point(aes(y = WoE, group = 1), color = 'blue') + 
    geom_bar(aes(y = prop_obs / y_scale, group = 1), stat="identity", size=.1, color = 'red', fill = "#69b3a2", alpha=.4) +
    theme(
      axis.title.y = element_text(color = 'blue', size=13),
      axis.title.y.right = element_text(color = 'red', size=13)
    ) + 
    scale_x_continuous(breaks = seq(min(plot_data %>% select_at(x_axis)), max(plot_data %>% select_at(x_axis)), by = 1)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*y_scale, name = "Observation proportion")) + 
    scale_colour_manual(values = c("blue", "red")) + 
    labs(y = "Weight of evidence", x = x_label)
}

distinct(loan_data, term_int)
term_int_woe <- calculate_weight_of_evidence_continuous(train_data, 'term_int', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("term_int", distinct(term_int_woe, IV))
plot_woe_continuous(term_int_woe, 'term_int', 'Loan Term', 2)
train_data$term_int_trnsfrmd <- ifelse(train_data$term_int == 36, 'term_36',
                                       ifelse(train_data$term_int == 60, 'term_60', NaN))


distinct(loan_data, emp_length_int)
emp_length_int_woe <- calculate_weight_of_evidence_continuous(train_data, 'emp_length_int', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("emp_length_int", distinct(emp_length_int_woe, IV))
plot_woe_continuous(emp_length_int_woe %>% arrange(emp_length_int), 'emp_length_int', 'Employment Length', 3)
train_data$emp_length_int_trnsfrmd <- ifelse(train_data$emp_length_int %in% c(0), 'emp_length_0',
                                             ifelse(train_data$emp_length_int %in% c(1,2,3,4), 'emp_length_1_4',
                                                    ifelse(train_data$emp_length_int %in% c(5,6), 'emp_length_5_6',
                                                           ifelse(train_data$emp_length_int %in% c(7,8,9), 'emp_length_7_9',
                                                                  'emp_length_10'))))

distinct(train_data, mths_since_issue_d)
mths_since_issue_d_woe <- calculate_weight_of_evidence_continuous(train_data, 'mths_since_issue_d', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("mths_since_issue_d", distinct(mths_since_issue_d_woe, IV))
plot_woe_continuous(mths_since_issue_d_woe, 'mths_since_issue_d', 'Months Since Issue Date', 0.1)
train_data$mths_since_issue_d_trnsfrmd <- ifelse(train_data$mths_since_issue_d <= 55, paste0('mths_since_issue_date=',str(train_data$mths_since_issue_d)),
                                                 ifelse(train_data$mths_since_issue_d %in% c(56,57,58,59,60,61,62,63), 'mths_since_issue_date>=56<=63',
                                                        ifelse(train_data$mths_since_issue_d %in% seq(64,84), 'mths_since_issue_date>63<=84', 
                                                               ifelse(train_data$mths_since_issue_d > 84, 'mths_since_issue_data>84', NaN))))

train_data <- train_data %>%
  arrange(int_rate)
train_data$int_rate_factor <- cut(train_data$int_rate, 50)
distinct(train_data, int_rate_factor)
int_rate_factor_woe <- calculate_weight_of_evidence_continuous(train_data, 'int_rate_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("int_rate_factor", distinct(int_rate_factor_woe, IV))
plot_woe_discrete(int_rate_factor_woe, 'int_rate_factor', 'Interest rate factor', 0.02, 90)
train_data$int_rate_factor_trnsfrmd <- ifelse(train_data$int_rate <= 9.548, 'int_rate<9.548',
                                       ifelse(train_data$int_rate <= 12.025, '9.548<int_rate<=12.025',
                                              ifelse(train_data$int_rate <= 15.74, '12.025<int_rate<=15.74',
                                                     ifelse(train_data$int_rate <= 20.281, '15.74<int_rate<20.281',
                                                            ifelse(train_data$int_rate >20.281, 'int_rate>20.281',NaN)))))
distinct(train_data, int_rate_factor_trnsfrmd)

distinct(train_data, funded_amnt)
train_data$funded_amnt_factor <- cut(train_data$funded_amnt, 50)
funded_amnt_factor_woe <- calculate_weight_of_evidence_continuous(train_data, 'funded_amnt_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("funded_amnt_factor", distinct(funded_amnt_factor_woe, IV))
plot_woe_discrete(funded_amnt_factor_woe, 'funded_amnt_factor', 'Funded amount factor', 0.02, 90)
train_data$funded_amnt_factor_trnsfrmd <- ifelse(train_data$funded_amnt <= 1000, 'funded_amnt:<5K',
                                                 ifelse(train_data$funded_amnt <= 10000, 'funded_amnt:5K-10K',
                                                        ifelse(train_data$funded_amnt <= 20000, 'funded_amnt:10K:20K',
                                                               ifelse(train_data$funded_amnt <= 30000, 'funded_amnt:20K:30:',
                                                                      ifelse(train_data$funded_amnt <= 50000, 'funded_amnt:30K:50K',
                                                                             ifelse(train_data$funded_amnt <= 70000, 'funded_amnt:50K:70K',
                                                                                    ifelse(train_data$funded_amnt <= 100000, 'funded_amnt:70K:100K',
                                                                                           ifelse(train_data$funded_amnt > 100000, 'funded_amnt:>100K',
                                                                                                  NaN))))))))

# continuous variable: mths_since_earliest_cr_line
distinct(train_data, mths_since_earliest_cr_line)
train_data <- train_data %>%
  arrange(mths_since_earliest_cr_line)
train_data$mths_since_earliest_cr_line_factor <- cut(train_data$mths_since_earliest_cr_line, 50)
distinct(train_data, mths_since_earliest_cr_line_factor)
mths_since_earliest_cr_line_woe <- calculate_weight_of_evidence_continuous(train_data, 'mths_since_earliest_cr_line_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("mths_since_earliest_cr_line", distinct(mths_since_earliest_cr_line_woe, IV))
plot_woe_discrete(mths_since_earliest_cr_line_woe, 'mths_since_earliest_cr_line_factor', 'Months since earliest credit line', 0.1, 90)
train_data$mths_since_earliest_cr_line_factor_trnsfrmd <- ifelse(train_data$mths_since_earliest_cr_line <= 140, 'mths_since_earliest_cr_line<=140',
                                                          ifelse(train_data$mths_since_earliest_cr_line <= 246, '140<mths_since_earliest_cr_line<=246',
                                                                 ifelse(train_data$mths_since_earliest_cr_line <= 270, '246<mths_since_earliest_cr_line<=270',
                                                                        ifelse(train_data$mths_since_earliest_cr_line <= 293, '270<mths_since_earliest_cr_line<=293',
                                                                               ifelse(train_data$mths_since_earliest_cr_line <= 398, '293<mths_since_earliest_cr_line<=398',
                                                                                      ifelse(train_data$mths_since_earliest_cr_line >398, 'mths_since_earliest_cr_line>398',
                                                                                             NaN))))))

distinct(train_data, delinq_2yrs)
delinq_2yrs_woe <- calculate_weight_of_evidence_continuous(train_data, 'delinq_2yrs', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("delinq_2yrs", distinct(delinq_2yrs_woe, IV))
plot_woe_continuous(delinq_2yrs_woe, 'delinq_2yrs', 'Deliquency in last 2 years', 2)
train_data$delinq_2yrs_trnsfrmd <- ifelse(train_data$delinq_2yrs == 0, 'delinq_2yrs=0',
                                          ifelse(train_data$delinq_2yrs == 1, 'delinq_2yrs=1',
                                                 ifelse(train_data$delinq_2yrs == 2, 'delinq_2yrs=2',
                                                        ifelse(train_data$delinq_2yrs > 2, 'delinq_2yrs>2',
                                                               NaN))))

distinct(train_data, inq_last_6mths)
inq_last_6mths_woe <- calculate_weight_of_evidence_continuous(train_data, 'inq_last_6mths', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("inq_last_mths", distinct(inq_last_6mths_woe, IV))
plot_woe_continuous(inq_last_6mths_woe, 'inq_last_6mths', 'Inq last 6 months', 1)
train_data$inq_last_6mths_trnsfrmd <- ifelse(train_data$inq_last_6mths == 0, 'inq_last_6mths=0',
                                             ifelse(train_data$inq_last_6mths == 1, 'inq_last_6mths=1',
                                                    ifelse(train_data$inq_last_6mths == 2, 'inq_last_6mths=2',
                                                           ifelse(train_data$inq_last_6mths == 3, 'inq_last_6mths=3',
                                                                  ifelse(train_data$inq_last_6mths > 3, 'inq_last_6mths>3',
                                                                         NaN)))))

distinct(train_data, open_acc)
open_acc_woe <- calculate_weight_of_evidence_continuous(train_data, 'open_acc', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("open_acc", distinct(open_acc_woe, IV))
plot_woe_continuous(open_acc_woe, 'open_acc', 'Open Account', 0.1)
plot_woe_continuous(open_acc_woe %>% filter(open_acc>3, open_acc<20), 'open_acc', 'Open Account', 2)
train_data$open_acc_trnsfrmd <- ifelse(train_data$open_acc %in% c(0,1,2,3,seq(21,max(train_data$open_acc))), 'open_acc=0,1,2,3,21-max',
                                       ifelse(train_data$open_acc %in% c(4,5,6,7), 'open_acc=4,5,6,7',
                                              ifelse(train_data$open_acc %in% c(8), 'open_acc=8',
                                                     ifelse(train_data$open_acc %in% c(9,10,11,12), 'open_acc=9,10,11,12',
                                                            ifelse(train_data$open_acc %in% c(13,14,15,16), 'open_acc=13,14,15,16',
                                                                   ifelse(train_data$open_acc %in% c(17), 'open_acc=17',
                                                                          ifelse(train_data$open_acc %in% c(18,19,20), 'open_acc=18,19,20',
                                                                                 NaN)))))))

distinct(train_data, pub_rec)
pub_rec_woe <- calculate_weight_of_evidence_continuous(train_data, 'pub_rec', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("pub_rec", distinct(pub_rec_woe, IV))
plot_woe_continuous(pub_rec_woe, 'pub_rec', 'Public Records', 1)
train_data$pub_rec_trnsfrmd <- ifelse(train_data$pub_rec == 0, 'pub_rec=0',
                                      ifelse(train_data$pub_rec == 1, 'pub_rec=1',
                                             ifelse(train_data$pub_rec == 2, 'pub_rec=2',
                                                    ifelse(train_data$pub_rec >2, 'pub_rec>=3',
                                                           NaN))))

distinct(train_data, total_acc)
train_data <- train_data %>%
  arrange(total_acc)
train_data$total_acc_factor <- cut(train_data$total_acc, 50)
distinct(train_data, total_acc_factor)
total_acc_woe <- calculate_weight_of_evidence_continuous(train_data, 'total_acc_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("total_acc_factor", distinct(total_acc_woe, IV))
plot_woe_discrete(total_acc_woe, 'total_acc_factor', 'Total account factor', 0.1, 90)


train_data$total_acc_factor_trnsfrmd <- ifelse(train_data$total_acc <= 10, 'total_acc<=10',
                                        ifelse(train_data$total_acc <= 25, 'total_acc<=25',
                                               ifelse(train_data$total_acc <= 50, 'total_acc<=50',
                                                      ifelse(train_data$total_acc > 50, 'total_acc>50',
                                                             NaN))))

distinct(train_data, acc_now_delinq)
acc_now_delinq_woe <- calculate_weight_of_evidence_continuous(train_data, 'acc_now_delinq', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("acc_now_delinq", distinct(acc_now_delinq_woe, IV))
plot_woe_continuous(acc_now_delinq_woe, 'acc_now_delinq', 'Account now delinquent', 1.0)
train_data$acc_now_delinq_trnsfrmd <- ifelse(train_data$acc_now_delinq == 0, 'acc_now_delinq=0',
                                             ifelse(train_data$acc_now_delinq >= 1, 'acc_now_delinq>=1',
                                                    NaN))

distinct(train_data, total_rev_hi_lim)
train_data <- train_data %>% 
  arrange(total_rev_hi_lim)
train_data$total_rev_hi_lim_factor <- cut(train_data$total_rev_hi_lim, 1000)
distinct(train_data, total_rev_hi_lim_factor)
total_rev_hi_lim_woe <- calculate_weight_of_evidence_continuous(train_data, 'total_rev_hi_lim_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("total_rev_hi_lim_factor", distinct(total_rev_hi_lim_woe, IV))
plot_woe_discrete(total_rev_hi_lim_woe, 'total_rev_hi_lim_factor', 'Total revolving hi limit factor', 1, 90)
train_data$total_rev_hi_lim_factor_trnsfrmd <- ifelse(train_data$total_rev_hi_lim <= 5000, 'total_rev_hi_lim<=5K',
                                               ifelse(train_data$total_rev_hi_lim <= 10000, 'total_rev_hi_lim:5K-10K',
                                                      ifelse(train_data$total_rev_hi_lim <= 20000, 'total_rev_hi_lim:10K-20K',
                                                             ifelse(train_data$total_rev_hi_lim <= 30000, 'total_rev_hi_lim:20K-30K',
                                                                    ifelse(train_data$total_rev_hi_lim <= 40000, 'total_rev_hi_lim:30K-40K',
                                                                           ifelse(train_data$total_rev_hi_lim <= 50000, 'total_rev_hi_lim:40K-50K',
                                                                                  ifelse(train_data$total_rev_hi_lim <= 100000, 'total_rev_hi_lim:50K-100L',
                                                                                         ifelse(train_data$total_rev_hi_lim > 100000, 'total_rev_hi_lim>100K',
                                                                                                NaN))))))))
distinct(train_data, total_rev_hi_lim_factor_trnsfrmd)

distinct(train_data, installment)
train_data <- train_data %>% arrange(installment)
train_data$installment_factor <- cut(train_data$installment, 100)
distinct(train_data, installment_factor)
installment_woe <- calculate_weight_of_evidence_continuous(train_data, 'installment_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("installment_factor", distinct(installment_woe, IV))
plot_woe_discrete(installment_woe, 'installment_factor', 'Installment Factor', 0.05, 90)
train_data$installment_factor_trnsfrmd <- ifelse(train_data$installment <= 30, 'installment<=30',
                                          ifelse(train_data$installment <= 100, 'installment:30-100',
                                                 ifelse(train_data$installment <= 200, 'installment:100-200',
                                                        ifelse(train_data$installment <= 300, 'installment:200-300',
                                                               ifelse(train_data$installment <= 400, 'installment:300-400',
                                                                      ifelse(train_data$installment <= 500, 'installment:400-500',
                                                                             ifelse(train_data$installment <= 600, 'installment:500:600',
                                                                                    ifelse(train_data$installment <= 700, 'installment:600-700',
                                                                                           ifelse(train_data$installment <= 800, 'installment:700-800',
                                                                                                  ifelse(train_data$installment > 800, 'installment>800',
                                                                                                         NaN))))))))))

distinct(train_data, annual_inc)
train_data <- train_data %>% arrange(annual_inc)
train_data_temp <- train_data %>%
  filter(annual_inc <= 140000)
train_data_temp$annual_inc_factor <- cut(train_data_temp$annual_inc, 50)
annual_inc_woe <- calculate_weight_of_evidence_continuous(train_data_temp, 'annual_inc_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("annual_inc_factor", distinct(annual_inc_woe, IV))
plot_woe_discrete(annual_inc_woe, 'annual_inc_factor', 'Annual Income Factor', 0.1, 90)
train_data$annual_inc_factor_trnsfrmd <- ifelse(train_data$annual_inc <= 20000, 'annual_inc<=20K',
                                         ifelse(train_data$annual_inc <= 30000, 'annual_inc:20K-30K',
                                                ifelse(train_data$annual_inc <= 40000, 'annual_inc:30K-40K',
                                                       ifelse(train_data$annual_inc <= 50000, 'annual_inc:40K-50K',
                                                              ifelse(train_data$annual_inc <= 60000, 'annual_inc:50K-60K',
                                                                     ifelse(train_data$annual_inc <= 70000, 'annual_inc:60K-70K',
                                                                            ifelse(train_data$annual_inc <= 80000, 'annual_inc:70K-80K',
                                                                                   ifelse(train_data$annual_inc <= 90000, 'annual_inc:80K-90K',
                                                                                          ifelse(train_data$annual_inc <= 100000, 'annual_inc:90K-100K',
                                                                                                 ifelse(train_data$annual_inc <= 120000, 'annual_inc:100K-120K',
                                                                                                        ifelse(train_data$annual_inc <= 140000, 'annual_inc:120K-140K',
                                                                                                               ifelse(train_data$annual_inc > 140000, 'annual_inc:>140K',
                                                                                                                      NaN))))))))))))
distinct(train_data, mths_since_last_delinq)
train_data <- train_data %>%
  arrange(mths_since_last_delinq)
train_data$mths_since_last_delinq_factor <- cut(train_data$mths_since_last_delinq, 50)
distinct(train_data, mths_since_last_delinq_factor)
mths_since_last_delinq_woe <- calculate_weight_of_evidence_continuous(train_data, 'mths_since_last_delinq_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("mths_since_last_delinq_factor", distinct(mths_since_last_delinq_woe, IV))
plot_woe_discrete(mths_since_last_delinq_woe, 'mths_since_last_delinq_factor', 'Months since last delinquency factor', 0.5, 90)
train_data$mths_since_last_delinq_factor_trnsfrmd <- ifelse(train_data$mths_since_last_delinq <= 3, 'mths_since_last_delinq<=3',
                                                     ifelse(train_data$mths_since_last_delinq <= 7, 'mths_since_last_delinq:4-7',
                                                            ifelse(train_data$mths_since_last_delinq <= 40, 'mths_since_last_delinq:7-40',
                                                                   ifelse(train_data$mths_since_last_delinq <= 80, 'mths_since_last_delinq:40-80',
                                                                          ifelse(train_data$mths_since_last_delinq > 80, 'mths_since_last_delinq>80',
                                                                                 ifelse(is.na(train_data$mths_since_last_delinq), 'Missing',
                                                                                        NaN))))))
distinct(train_data, dti)
train_data <- train_data %>%
  arrange(dti)
train_data_temp <- train_data %>%
  filter(dti <= 35)
train_data_temp$dti_factor <- cut(train_data_temp$dti, 20) # try with 100 and 50 cuts
train_data <- train_data %>%
  left_join(train_data_temp %>% select(id, dti_factor))
distinct(train_data_temp, dti_factor)
dti_woe <- calculate_weight_of_evidence_continuous(train_data_temp, 'dti_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("dti_factor", distinct(dti_woe, IV))
plot_woe_discrete(dti_woe, 'dti_factor', 'Debt to Income', 0.1, 90)
train_data$dti_factor_trnsfrmd <- ifelse(train_data$dti <= 1, 'dti:<1',
                                         ifelse(train_data$dti <= 2, 'dti:1-2',
                                                ifelse(train_data$dti <= 4, 'dti:2-4',
                                                       ifelse(train_data$dti <= 6, 'dti:4-6',
                                                              ifelse(train_data$dti <= 8, 'dti:6-8',
                                                                     ifelse(train_data$dti <= 10, 'dti:8-10',
                                                                            ifelse(train_data$dti <= 15, 'dti:10-15',
                                                                                   ifelse(train_data$dti <= 20, 'dti:15-20',
                                                                                          ifelse(train_data$dti <= 25, 'dti:20-25',
                                                                                                 ifelse(train_data$dti <= 30, 'dti:25-30',
                                                                                                        ifelse(train_data$dti <= 35, 'dti:30-35',
                                                                                                               ifelse(train_data$dti >35, 'dti:>35',
                                                                                                                      NaN))))))))))))
distinct(train_data, dti_factor_trnsfrmd)


distinct(train_data, mths_since_last_record)
train_data <- train_data %>%
  arrange(mths_since_last_record)
train_data$mths_since_last_record_factor <- cut(train_data$mths_since_last_record, 20)
mths_since_last_record_woe <- calculate_weight_of_evidence_continuous(train_data, 'mths_since_last_record_factor', 'good_bad')
IV_df[nrow(IV_df) + 1,] <- list("mths_since_last_record_factor", distinct(mths_since_last_record_woe, IV))
plot_woe_discrete(mths_since_last_record_woe, 'mths_since_last_record_factor', 'Months since last record factor', 0.1, 90)
train_data$mths_since_last_record_factor_trnsfrmd <- ifelse(train_data$mths_since_last_record <= 2, 'mths_since_last_record<=2',
                                                     ifelse(train_data$mths_since_last_record <= 30, 'mths_since_last_record:3-30',
                                                            ifelse(train_data$mths_since_last_record <= 65, 'mths_since_last_record:31-65',
                                                                   ifelse(train_data$mths_since_last_record <= 80, 'mths_since_last_record:66-80',
                                                                          ifelse(train_data$mths_since_last_record > 80, 'mths_since_last_record>80',
                                                                                 ifelse(is.na(train_data$mths_since_last_record), 'Missing',
                                                                                        NaN))))))

train_data <- train_data %>%
  mutate(mths_since_last_record_factor_trnsfrmd = replace_na(mths_since_last_record_factor_trnsfrmd, 'Missing'))
train_data <- train_data %>%
  mutate(mths_since_last_delinq_factor_trnsfrmd = replace_na(mths_since_last_delinq_factor_trnsfrmd, 'Missing'))

train_data_trnsfrmd <- train_data %>%
  select(ends_with(c('good_bad', 'trnsfrmd')))
