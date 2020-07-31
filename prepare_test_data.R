test_data$grade_trnsfrmd <- test_data$grade

test_data$home_ownership_trnsfrmd <- ifelse(test_data$home_ownership %in% combine_home_ownership, "RENT_OTHER_NONE_ANY", test_data$home_ownership)
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
test_data$addr_state_trnsfrmd <- ifelse(test_data$addr_state %in% state1, "state_list_1",
                                         ifelse(test_data$addr_state %in% state2, "state_list_2",
                                                ifelse(test_data$addr_state %in% state3, "state_list_3",
                                                       ifelse(test_data$addr_state %in% state4, "state_list_4",
                                                              ifelse(test_data$addr_state %in% state5, "state_list_5",
                                                                     ifelse(test_data$addr_state %in% state6, "state_list_6",
                                                                            ifelse(test_data$addr_state %in% state7, "state_list_7",
                                                                                   ifelse(test_data$addr_state %in% state8, "state_list_8",
                                                                                          ifelse(test_data$addr_state %in% state9, "state_list_9",
                                                                                                 ifelse(test_data$addr_state %in% state10, "state_list_10",
                                                                                                        ifelse(test_data$addr_state %in% state11, "state_list_11",
                                                                                                               ifelse(test_data$addr_state %in% state12, "state_list_12",NaN))))))))))))


test_data$verification_status_trnsfrmd <- ifelse(test_data$verification_status %in% c("Not Verified", "Source Verified"),
                                                  "Not or Source Verified",
                                                  test_data$verification_status)

purpose1 <- c("small_business", "educational", "renewable_energy", "moving")
purpose2 <- c("other", "medical", "house")
purpose3 <- c("debt_consolidation", "vacation", "home_improvement")
purpose4 <- c("wedding", "car", "major_purchase", "credit_card")
test_data$purpose_trnsfrmd <- ifelse(test_data$purpose %in% purpose1, "purpose_list_1",
                                      ifelse(test_data$purpose %in% purpose2, "purpose_list_2",
                                             ifelse(test_data$purpose %in% purpose3, "purpose_list_3",
                                                    ifelse(test_data$purpose %in% purpose4, "purpose_list_4", NaN))))

test_data$initial_list_status_trnsfrmd <- test_data$initial_list_status

test_data$term_int_trnsfrmd <- ifelse(test_data$term_int == 36, 'term_36',
                                       ifelse(test_data$term_int == 60, 'term_60', NaN))

test_data$emp_length_int_trnsfrmd <- ifelse(test_data$emp_length_int %in% c(0), 'emp_length_0',
                                             ifelse(test_data$emp_length_int %in% c(1,2,3,4), 'emp_length_1_4',
                                                    ifelse(test_data$emp_length_int %in% c(5,6), 'emp_length_5_6',
                                                           ifelse(test_data$emp_length_int %in% c(7,8,9), 'emp_length_7_9',
                                                                  'emp_length_10'))))

test_data$mths_since_issue_d_trnsfrmd <- ifelse(test_data$mths_since_issue_d <= 55, paste0('mths_since_issue_date=',str(test_data$mths_since_issue_d)),
                                                 ifelse(test_data$mths_since_issue_d %in% c(56,57,58,59,60,61,62,63), 'mths_since_issue_date>=56<=63',
                                                        ifelse(test_data$mths_since_issue_d %in% seq(64,84), 'mths_since_issue_date>63<=84', 
                                                               ifelse(test_data$mths_since_issue_d > 84, 'mths_since_issue_data>84', NaN))))

test_data$int_rate_factor_trnsfrmd <- ifelse(test_data$int_rate <= 9.548, 'int_rate<9.548',
                                              ifelse(test_data$int_rate <= 12.025, '9.548<int_rate<=12.025',
                                                     ifelse(test_data$int_rate <= 15.74, '12.025<int_rate<=15.74',
                                                            ifelse(test_data$int_rate <= 20.281, '15.74<int_rate<20.281',
                                                                   ifelse(test_data$int_rate >20.281, 'int_rate>20.281',NaN)))))

test_data$funded_amnt_factor_trnsfrmd <- ifelse(test_data$funded_amnt <= 1000, 'funded_amnt:<5K',
                                                 ifelse(test_data$funded_amnt <= 10000, 'funded_amnt:5K-10K',
                                                        ifelse(test_data$funded_amnt <= 20000, 'funded_amnt:10K:20K',
                                                               ifelse(test_data$funded_amnt <= 30000, 'funded_amnt:20K:30:',
                                                                      ifelse(test_data$funded_amnt <= 50000, 'funded_amnt:30K:50K',
                                                                             ifelse(test_data$funded_amnt <= 70000, 'funded_amnt:50K:70K',
                                                                                    ifelse(test_data$funded_amnt <= 100000, 'funded_amnt:70K:100K',
                                                                                           ifelse(test_data$funded_amnt > 100000, 'funded_amnt:>100K',
                                                                                                  NaN))))))))

test_data$mths_since_earliest_cr_line_factor_trnsfrmd <- ifelse(test_data$mths_since_earliest_cr_line <= 140, 'mths_since_earliest_cr_line<=140',
                                                                 ifelse(test_data$mths_since_earliest_cr_line <= 246, '140<mths_since_earliest_cr_line<=246',
                                                                        ifelse(test_data$mths_since_earliest_cr_line <= 270, '246<mths_since_earliest_cr_line<=270',
                                                                               ifelse(test_data$mths_since_earliest_cr_line <= 293, '270<mths_since_earliest_cr_line<=293',
                                                                                      ifelse(test_data$mths_since_earliest_cr_line <= 398, '293<mths_since_earliest_cr_line<=398',
                                                                                             ifelse(test_data$mths_since_earliest_cr_line >398, 'mths_since_earliest_cr_line>398',
                                                                                                    NaN))))))

test_data$delinq_2yrs_trnsfrmd <- ifelse(test_data$delinq_2yrs == 0, 'delinq_2yrs=0',
                                          ifelse(test_data$delinq_2yrs == 1, 'delinq_2yrs=1',
                                                 ifelse(test_data$delinq_2yrs == 2, 'delinq_2yrs=2',
                                                        ifelse(test_data$delinq_2yrs > 2, 'delinq_2yrs>2',
                                                               NaN))))

test_data$inq_last_6mths_trnsfrmd <- ifelse(test_data$inq_last_6mths == 0, 'inq_last_6mths=0',
                                             ifelse(test_data$inq_last_6mths == 1, 'inq_last_6mths=1',
                                                    ifelse(test_data$inq_last_6mths == 2, 'inq_last_6mths=2',
                                                           ifelse(test_data$inq_last_6mths == 3, 'inq_last_6mths=3',
                                                                  ifelse(test_data$inq_last_6mths > 3, 'inq_last_6mths>3',
                                                                         NaN)))))

test_data$open_acc_trnsfrmd <- ifelse(test_data$open_acc %in% c(0,1,2,3,seq(21,max(test_data$open_acc))), 'open_acc=0,1,2,3,21-max',
                                       ifelse(test_data$open_acc %in% c(4,5,6,7), 'open_acc=4,5,6,7',
                                              ifelse(test_data$open_acc %in% c(8), 'open_acc=8',
                                                     ifelse(test_data$open_acc %in% c(9,10,11,12), 'open_acc=9,10,11,12',
                                                            ifelse(test_data$open_acc %in% c(13,14,15,16), 'open_acc=13,14,15,16',
                                                                   ifelse(test_data$open_acc %in% c(17), 'open_acc=17',
                                                                          ifelse(test_data$open_acc %in% c(18,19,20), 'open_acc=18,19,20',
                                                                                 NaN)))))))

test_data$pub_rec_trnsfrmd <- ifelse(test_data$pub_rec == 0, 'pub_rec=0',
                                      ifelse(test_data$pub_rec == 1, 'pub_rec=1',
                                             ifelse(test_data$pub_rec == 2, 'pub_rec=2',
                                                    ifelse(test_data$pub_rec >2, 'pub_rec>=3',
                                                           NaN))))

test_data$total_acc_factor_trnsfrmd <- ifelse(test_data$total_acc <= 10, 'total_acc<=10',
                                               ifelse(test_data$total_acc <= 25, 'total_acc<=25',
                                                      ifelse(test_data$total_acc <= 50, 'total_acc<=50',
                                                             ifelse(test_data$total_acc > 50, 'total_acc>50',
                                                                    NaN))))

test_data$acc_now_delinq_trnsfrmd <- ifelse(test_data$acc_now_delinq == 0, 'acc_now_delinq=0',
                                             ifelse(test_data$acc_now_delinq >= 1, 'acc_now_delinq>=1',
                                                    NaN))

test_data$total_rev_hi_lim_factor_trnsfrmd <- ifelse(test_data$total_rev_hi_lim <= 5000, 'total_rev_hi_lim<=5K',
                                                      ifelse(test_data$total_rev_hi_lim <= 10000, 'total_rev_hi_lim:5K-10K',
                                                             ifelse(test_data$total_rev_hi_lim <= 20000, 'total_rev_hi_lim:10K-20K',
                                                                    ifelse(test_data$total_rev_hi_lim <= 30000, 'total_rev_hi_lim:20K-30K',
                                                                           ifelse(test_data$total_rev_hi_lim <= 40000, 'total_rev_hi_lim:30K-40K',
                                                                                  ifelse(test_data$total_rev_hi_lim <= 50000, 'total_rev_hi_lim:40K-50K',
                                                                                         ifelse(test_data$total_rev_hi_lim <= 100000, 'total_rev_hi_lim:50K-100L',
                                                                                                ifelse(test_data$total_rev_hi_lim > 100000, 'total_rev_hi_lim>100K',
                                                                                                       NaN))))))))


test_data$installment_factor_trnsfrmd <- ifelse(test_data$installment <= 30, 'installment<=30',
                                                 ifelse(test_data$installment <= 100, 'installment:30-100',
                                                        ifelse(test_data$installment <= 200, 'installment:100-200',
                                                               ifelse(test_data$installment <= 300, 'installment:200-300',
                                                                      ifelse(test_data$installment <= 400, 'installment:300-400',
                                                                             ifelse(test_data$installment <= 500, 'installment:400-500',
                                                                                    ifelse(test_data$installment <= 600, 'installment:500:600',
                                                                                           ifelse(test_data$installment <= 700, 'installment:600-700',
                                                                                                  ifelse(test_data$installment <= 800, 'installment:700-800',
                                                                                                         ifelse(test_data$installment > 800, 'installment>800',
                                                                                                                NaN))))))))))

test_data$annual_inc_factor_trnsfrmd <- ifelse(test_data$annual_inc <= 20000, 'annual_inc<=20K',
                                                ifelse(test_data$annual_inc <= 30000, 'annual_inc:20K-30K',
                                                       ifelse(test_data$annual_inc <= 40000, 'annual_inc:30K-40K',
                                                              ifelse(test_data$annual_inc <= 50000, 'annual_inc:40K-50K',
                                                                     ifelse(test_data$annual_inc <= 60000, 'annual_inc:50K-60K',
                                                                            ifelse(test_data$annual_inc <= 70000, 'annual_inc:60K-70K',
                                                                                   ifelse(test_data$annual_inc <= 80000, 'annual_inc:70K-80K',
                                                                                          ifelse(test_data$annual_inc <= 90000, 'annual_inc:80K-90K',
                                                                                                 ifelse(test_data$annual_inc <= 100000, 'annual_inc:90K-100K',
                                                                                                        ifelse(test_data$annual_inc <= 120000, 'annual_inc:100K-120K',
                                                                                                               ifelse(test_data$annual_inc <= 140000, 'annual_inc:120K-140K',
                                                                                                                      ifelse(test_data$annual_inc > 140000, 'annual_inc:>140K',
                                                                                                                             NaN))))))))))))
test_data$mths_since_last_delinq_factor_trnsfrmd <- ifelse(test_data$mths_since_last_delinq <= 3, 'mths_since_last_delinq<=3',
                                                            ifelse(test_data$mths_since_last_delinq <= 7, 'mths_since_last_delinq:4-7',
                                                                   ifelse(test_data$mths_since_last_delinq <= 40, 'mths_since_last_delinq:7-40',
                                                                          ifelse(test_data$mths_since_last_delinq <= 80, 'mths_since_last_delinq:40-80',
                                                                                 ifelse(test_data$mths_since_last_delinq > 80, 'mths_since_last_delinq>80',
                                                                                        ifelse(is.na(test_data$mths_since_last_delinq), 'Missing',
                                                                                               NaN))))))
test_data$dti_factor_trnsfrmd <- ifelse(test_data$dti <= 1, 'dti:<1',
                                         ifelse(test_data$dti <= 2, 'dti:1-2',
                                                ifelse(test_data$dti <= 4, 'dti:2-4',
                                                       ifelse(test_data$dti <= 6, 'dti:4-6',
                                                              ifelse(test_data$dti <= 8, 'dti:6-8',
                                                                     ifelse(test_data$dti <= 10, 'dti:8-10',
                                                                            ifelse(test_data$dti <= 15, 'dti:10-15',
                                                                                   ifelse(test_data$dti <= 20, 'dti:15-20',
                                                                                          ifelse(test_data$dti <= 25, 'dti:20-25',
                                                                                                 ifelse(test_data$dti <= 30, 'dti:25-30',
                                                                                                        ifelse(test_data$dti <= 35, 'dti:30-35',
                                                                                                               ifelse(test_data$dti >35, 'dti:>35',
                                                                                                                      NaN))))))))))))

test_data$mths_since_last_record_factor_trnsfrmd <- ifelse(test_data$mths_since_last_record <= 2, 'mths_since_last_record<=2',
                                                            ifelse(test_data$mths_since_last_record <= 30, 'mths_since_last_record:3-30',
                                                                   ifelse(test_data$mths_since_last_record <= 65, 'mths_since_last_record:31-65',
                                                                          ifelse(test_data$mths_since_last_record <= 80, 'mths_since_last_record:66-80',
                                                                                 ifelse(test_data$mths_since_last_record > 80, 'mths_since_last_record>80',
                                                                                        ifelse(is.na(test_data$mths_since_last_record), 'Missing',
                                                                                               NaN))))))

test_data <- test_data %>%
  mutate(mths_since_last_record_factor_trnsfrmd = replace_na(mths_since_last_record_factor_trnsfrmd, 'Missing'))
test_data <- test_data %>%
  mutate(mths_since_last_delinq_factor_trnsfrmd = replace_na(mths_since_last_delinq_factor_trnsfrmd, 'Missing'))

test_data_trnsfrmd <- test_data %>%
  select(ends_with(c('good_bad', 'trnsfrmd')))
