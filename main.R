# In this project we will model lending club data available at: 
# <https://www.kaggle.com/husainsb/lendingclub-issued-loans#lc_loan.csv>
# We start with a data between 2007-2015. We will first analyse features, 
# calculate Weight of Evidence and Information Value. 
# Based on that, we will use Generalised logistic regression to calculate probability of default, 
# and finally use coefficients of logistic regression to generate a credit score card.
# 
# We will also validate the performance of our predictions. After that, we will predict EAD and LGD.
# 
# Next, we use the data for 2016-2017 and treat it as a new data that needs to be integrated with our 
# model. However, before we can do that we have to quantify how similar/dissimilar new data is 
# from existing data. We will do this via Population Stability Index(PSI).



library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)
library(lubridate)
library(caret)
library(ggplot2)
library(data.table)

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

# setwd('D:\\ASUS\\Credit_Risk_Modelling\\Credit-Risk-Model')
setwd('C:\\Asus WebStorage\\nitin.7785@gmail.com\\MySyncFolder\\Credit_Risk_Modelling\\Credit-Risk-Model')
loan_data <- fread("lc_loan.csv")
coltypes <- sapply(loan_data, class)

# General Preprocessing
# preprocessing continuous variables
# display unique values of a column
distinct(loan_data, emp_length)
distinct(loan_data, term)

# distinct(loan_data, emp_length)
# remove columns with all NA's
loan_data <- loan_data %>%
  select_if(~sum(!is.na(.)) > 0)

# convert emp_length column to integers, replace "< 1 year" with 0 and replace NA's with 0
loan_data <- loan_data %>% 
  mutate(emp_length_int = gsub("< 1 year", "0", emp_length))

loan_data$emp_length_int <- map_chr(loan_data$emp_length_int, numextract) %>%
  as.double() %>%
replace_na(0)


distinct(loan_data, emp_length_int)

# convert term_int column to integers
loan_data <- loan_data %>%
  mutate(term_int = ifelse(term == '36 months', 36, 60))
distinct(loan_data, term_int)

loan_data1 <- copy(loan_data)

# convert mths_since_earliest_cr_line and mths_since_earliest_cr_line, to date and calculate 
# months upto 01-12-2017
loan_data$mths_since_earliest_cr_line <- interval(parse_date_time(loan_data$earliest_cr_line, "by"), 
                                                   as.POSIXct("2017-12-01"))%/% months(1)
summary(loan_data$mths_since_earliest_cr_line)

loan_data$mths_since_earliest_cr_line <- ifelse(loan_data$mths_since_earliest_cr_line < 0, 
                                               max(loan_data$mths_since_earliest_cr_line, na.rm = TRUE), 
                                               loan_data$mths_since_earliest_cr_line)


loan_data$mths_since_issue_d <- interval(parse_date_time(loan_data$issue_d, "by"), 
                                         as.POSIXct("2017-12-01"))%/% months(1)
summary(loan_data$mths_since_issue_d)

# preprocessing discrete variables
# since we do not need to create dummy variables for linear regression, we simply note down the discrete variable names
# grade, sub_grade, home_ownership, verification_status, loan_status, purpose, addr_state, initial_list_status


# check for missing values and clean
data_summary = summary(loan_data)
sapply(loan_data, function(y) sum(length(which(is.na(y)))))

# 'Total revolving high credit/ credit limit', so it makes sense that the missing values are equal to funded_amnt.
loan_data$total_rev_hi_lim <- ifelse(is.na(loan_data$total_rev_hi_lim), 
                                     loan_data$funded_amnt, loan_data$total_rev_hi_lim)

# missing annual income is replace by mean annual income
loan_data$annual_inc <- ifelse(is.na(loan_data$annual_inc), 
                               mean(loan_data$annual_inc, na.rm = TRUE), loan_data$annual_inc)

# for the rest fill Na with 0
loan_data$mths_since_earliest_cr_line <- ifelse(is.na(loan_data$mths_since_earliest_cr_line), 
                                                0, loan_data$mths_since_earliest_cr_line)

loan_data$acc_now_delinq <- ifelse(is.na(loan_data$acc_now_delinq), 0, loan_data$acc_now_delinq)
loan_data$total_acc <- ifelse(is.na(loan_data$total_acc), 0, loan_data$total_acc)
loan_data$pub_rec <- ifelse(is.na(loan_data$pub_rec), 0, loan_data$pub_rec)
loan_data$open_acc <- ifelse(is.na(loan_data$open_acc), 0, loan_data$open_acc)
loan_data$inq_last_6mths <- ifelse(is.na(loan_data$inq_last_6mths), 0, loan_data$inq_last_6mths)
loan_data$delinq_2yrs <- ifelse(is.na(loan_data$delinq_2yrs), 0, loan_data$delinq_2yrs)
loan_data$emp_length_int <- ifelse(is.na(loan_data$emp_length_int), 0, loan_data$emp_length_int)

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
loan_data <- loan_data %>%
  select(keep_columns)

#### PD model
# Data preparation
# Dependent variable, good/bad (default) definition
# check each loan_status frequency
distinct(loan_data, loan_status)
loan_data %>%
  group_by(loan_status) %>%
  summarise(n_distinct(id)/nrow(loan_data))

# create good_bad (default or not) column from loan_status and observe the frequency of good/bad loans
bad_status <- c('Charged Off', 'Default',
                'Does not meet the credit policy. Status:Charged Off',
                'Late (31-120 days)')
loan_data$good_bad <- ifelse(loan_data$loan_status %in% bad_status, 0, 1)
loan_data %>% group_by(good_bad) %>% summarise(n_distinct(id)/nrow(loan_data))

# splitting dataset into training and testing
train_index <- createDataPartition(loan_data$good_bad,p=0.9,list=FALSE)
train_data <- loan_data[train_index,]
test_data <- loan_data[-train_index,]

source('prepare_train_data.R')
source('prepare_test_data.R')
source('prepare_new_data.R')
rm(list= ls()[!(ls() %in% c('loan_data','train_data', 'train_data_trnsfrmd', 'test_data', 'test_data_trnsfrmd', 'new_data', 'new_data_trnsfrmd', 'IV_df'))])

source('pd_modelling.R')
