new_data <- fread("lc_2016_2017.csv")
sapply(new_data, class)

# General Preprocessing
# preprocessing continuous variables
# display unique values of a column
distinct(new_data, emp_length)
distinct(new_data, term)

distinct(new_data, emp_length)
# remove columns with all NA's
new_data <- new_data %>%
  select_if(~sum(!is.na(.)) > 0)

# convert emp_length column to integers, replace "< 1 year" with 0 and replace NA's with 0
new_data <- new_data %>% 
  mutate(emp_length_int = gsub("< 1 year", "0", emp_length))

new_data$emp_length_int <- map_chr(new_data$emp_length_int, numextract) %>%
  as.double() %>%
  replace_na(0)


distinct(new_data, emp_length_int)

# convert term_int column to integers
new_data <- new_data %>%
  mutate(term_int = ifelse(term == '36 months', 36, 60))
distinct(new_data, term_int)

new_data1 <- copy(new_data)

# convert mths_since_earliest_cr_line and mths_since_earliest_cr_line, to date and calculate 
# months upto 01-12-2017
new_data$mths_since_earliest_cr_line <- interval(parse_date_time(new_data$earliest_cr_line, "by"), 
                                                  as.POSIXct("2017-12-01"))%/% months(1)
summary(new_data$mths_since_earliest_cr_line)

new_data$mths_since_earliest_cr_line <- ifelse(new_data$mths_since_earliest_cr_line < 0, 
                                                max(new_data$mths_since_earliest_cr_line, na.rm = TRUE), 
                                                new_data$mths_since_earliest_cr_line)


new_data$mths_since_issue_d <- interval(parse_date_time(new_data$issue_d, "by"), 
                                         as.POSIXct("2017-12-01"))%/% months(1)
summary(new_data$mths_since_issue_d)

# preprocessing discrete variables
# since we do not need to create dummy variables for linear regression, we simply note down the discrete variable names
# grade, sub_grade, home_ownership, verification_status, loan_status, purpose, addr_state, initial_list_status


# check for missing values and clean
data_summary = summary(new_data)
sapply(new_data, function(y) sum(length(which(is.na(y)))))

# 'Total revolving high credit/ credit limit', so it makes sense that the missing values are equal to funded_amnt.
new_data$total_rev_hi_lim <- ifelse(is.na(new_data$total_rev_hi_lim), 
                                     new_data$funded_amnt, new_data$total_rev_hi_lim)

# missing annual income is replace by mean annual income
new_data$annual_inc <- ifelse(is.na(new_data$annual_inc), 
                               mean(new_data$annual_inc, na.rm = TRUE), new_data$annual_inc)

# for the rest fill Na with 0
new_data$mths_since_earliest_cr_line <- ifelse(is.na(new_data$mths_since_earliest_cr_line), 
                                                0, new_data$mths_since_earliest_cr_line)

new_data$acc_now_delinq <- ifelse(is.na(new_data$acc_now_delinq), 0, new_data$acc_now_delinq)
new_data$total_acc <- ifelse(is.na(new_data$total_acc), 0, new_data$total_acc)
new_data$pub_rec <- ifelse(is.na(new_data$pub_rec), 0, new_data$pub_rec)
new_data$open_acc <- ifelse(is.na(new_data$open_acc), 0, new_data$open_acc)
new_data$inq_last_6mths <- ifelse(is.na(new_data$inq_last_6mths), 0, new_data$inq_last_6mths)
new_data$delinq_2yrs <- ifelse(is.na(new_data$delinq_2yrs), 0, new_data$delinq_2yrs)
new_data$emp_length_int <- ifelse(is.na(new_data$emp_length_int), 0, new_data$emp_length_int)

# remove unwanted columns
keep_columns <- c("id", "grade",                            
                  "home_ownership", "loan_status", "addr_state",
                  "verification_status", "purpose",
                  "initial_list_status", "term_int",
                  "emp_length_int", "mths_since_issue_d",
                  "int_rate", "funded_amnt",
                  "mths_since_earliest_cr_line", "delinq_2yrs",                     
                  "inq_last_6mths", "open_acc",
                  "pub_rec", "total_acc",
                  "acc_now_delinq", "total_rev_hi_lim",
                  "installment", "annual_inc",
                  "mths_since_last_delinq", "dti",
                  "mths_since_last_record")
new_data <- new_data %>%
  select(keep_columns)

#### PD model
# Data preparation
# Dependent variable, good/bad (default) definition
# check each loan_status frequency
distinct(new_data, loan_status)
new_data %>%
  group_by(loan_status) %>%
  summarise(n_distinct(id)/nrow(new_data))

# create good_bad (default or not) column from loan_status and observe the frequency of good/bad news
bad_status <- c('Charged Off', 'Default',
                'Does not meet the credit policy. Status:Charged Off',
                'Late (31-120 days)')
new_data$good_bad <- ifelse(new_data$loan_status %in% bad_status, 0, 1)
new_data %>% group_by(good_bad) %>% summarise(n_distinct(id)/nrow(new_data))