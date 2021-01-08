---
title: "R Notebook- Project Stat 652"
author: "Maitry Shah"
date: "March 1,2020"
output:
  html_document: 
    keep_md: yes
  html_notebook: default
  always_allow_html: yes
  pdf_document: default
---

```r
library(pacman)
p_load(mdsr, tidyverse, Amelia, rpart,ggplot,ggplot2)
```

```
## Warning: package 'ggplot' is not available (for R version 3.6.1)
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE,
## logical.return = TRUE, : there is no package called 'ggplot'
```

```
## Warning in p_load(mdsr, tidyverse, Amelia, rpart, ggplot, ggplot2): Failed to install/load:
## ggplot
```

```r
library(caret)
library(lattice)
library(ggplot2)
library(GGally)
library(dplyr)
library(C50)
library(tictoc) 
```
\newpage
Introduction:
#Read CVS file Accepted Data
loan<- read.csv("accepted_2007_to_2018Q4.csv")
head(loan)
loan1<-loan

#step-1

library(lubridate)
loan1$issue_d <- myd(loan1$issue_d, truncated = 1L)
date1<- as.Date("2012-01-01")
date2<- as.Date("2014-12-31")

loan2 <- subset(loan1,issue_d>=date1 & issue_d<=date2)
dim(loan2)
#Save subset using saveRDS function
saveRDS(loan2,file = "subsetloan.rds")
--------------Didn't run everytime due to take it so long-----------------
\newpage

```r
loan2<-readRDS("subsetloan.rds")
```
#step-2

```r
str(loan2)
```

```
## 'data.frame':	423810 obs. of  151 variables:
##  $ id                                        : Factor w/ 2260701 levels "1000007","100001133",..: 1184934 1194932 1193160 1190869 1190103 1193498 1191153 1184912 1192016 1192795 ...
##  $ member_id                                 : logi  NA NA NA NA NA NA ...
##  $ loan_amnt                                 : num  10400 15000 9600 7650 12800 ...
##  $ funded_amnt                               : num  10400 15000 9600 7650 12800 ...
##  $ funded_amnt_inv                           : num  10400 15000 9600 7650 12800 ...
##  $ term                                      : Factor w/ 3 levels ""," 36 months",..: 2 3 2 2 3 3 2 2 2 2 ...
##  $ int_rate                                  : num  6.99 12.39 13.66 13.66 17.14 ...
##  $ installment                               : num  321 337 327 260 319 ...
##  $ grade                                     : Factor w/ 8 levels "","A","B","C",..: 2 4 4 4 5 5 4 4 3 5 ...
##  $ sub_grade                                 : Factor w/ 36 levels "","A1","A2","A3",..: 4 12 14 14 20 17 14 15 11 21 ...
##  $ emp_title                                 : Factor w/ 512695 levels "","\tCFO","\tMultimedia Supervisor",..: 480607 268356 11167 465963 409613 353538 125916 462012 275239 387553 ...
##  $ emp_length                                : Factor w/ 12 levels "","< 1 year",..: 11 4 4 2 4 9 4 4 2 4 ...
##  $ home_ownership                            : Factor w/ 7 levels "","ANY","MORTGAGE",..: 3 7 7 7 3 7 3 7 3 7 ...
##  $ annual_inc                                : num  58000 78000 69000 50000 125000 63800 75000 72000 89000 60000 ...
##  $ verification_status                       : Factor w/ 4 levels "","Not Verified",..: 2 3 3 3 4 3 4 3 3 3 ...
##  $ issue_d                                   : Date, format: "2014-12-01" "2014-12-01" ...
##  $ loan_status                               : Factor w/ 10 levels "","Charged Off",..: 2 7 7 2 3 7 7 2 7 2 ...
##  $ pymnt_plan                                : Factor w/ 3 levels "","n","y": 2 2 2 2 2 2 2 2 2 2 ...
##  $ url                                       : Factor w/ 2260669 levels "","https://lendingclub.com/browse/loanDetail.action?loan_id=1000007",..: 1184935 1194933 1193161 1190870 1190104 1193499 1191154 1184913 1192017 1192796 ...
##  $ desc                                      : Factor w/ 124502 levels "","\t Loan for purchase of grand piano. Piano will further diversify an already profitable business. Monthly budge"| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ purpose                                   : Factor w/ 15 levels "","car","credit_card",..: 3 4 4 4 2 3 4 3 6 7 ...
##  $ title                                     : Factor w/ 63156 levels "","\tcredit_card",..: 15513 18670 18670 18670 7279 15513 18670 15513 31769 31507 ...
##  $ zip_code                                  : Factor w/ 957 levels "","007xx","008xx",..: 895 227 72 816 911 633 138 940 534 323 ...
##  $ addr_state                                : Factor w/ 52 levels "","AK","AL","AR",..: 6 47 33 5 6 26 36 49 25 11 ...
##  $ dti                                       : num  14.92 12.03 25.81 34.81 8.31 ...
##  $ delinq_2yrs                               : num  0 0 0 0 1 0 0 1 0 0 ...
##  $ earliest_cr_line                          : Factor w/ 755 levels "","Apr-1934",..: 729 106 610 114 679 115 305 677 680 303 ...
##  $ fico_range_low                            : num  710 750 680 685 665 685 675 665 685 680 ...
##  $ fico_range_high                           : num  714 754 684 689 669 689 679 669 689 684 ...
##  $ inq_last_6mths                            : num  2 0 0 1 0 0 0 0 1 0 ...
##  $ mths_since_last_delinq                    : num  42 NA NA NA 17 60 46 1 55 48 ...
##  $ mths_since_last_record                    : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ open_acc                                  : num  17 6 12 11 8 10 7 14 9 11 ...
##  $ pub_rec                                   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ revol_bal                                 : num  6133 138008 16388 16822 5753 ...
##  $ revol_util                                : num  31.6 29 59.4 91.9 100.9 ...
##  $ total_acc                                 : num  36 17 44 20 13 35 31 23 32 19 ...
##  $ initial_list_status                       : Factor w/ 3 levels "","f","w": 3 3 2 2 3 3 2 2 2 2 ...
##  $ out_prncp                                 : num  0 0 0 0 2969 ...
##  $ out_prncp_inv                             : num  0 0 0 0 2969 ...
##  $ total_pymnt                               : num  6612 17392 9973 2282 15994 ...
##  $ total_pymnt_inv                           : num  6612 17392 9973 2282 15994 ...
##  $ total_rec_prncp                           : num  5218 15000 9600 704 9831 ...
##  $ total_rec_int                             : num  873 2392 373 340 6163 ...
##  $ total_rec_late_fee                        : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ recoveries                                : num  521 0 0 1238 0 ...
##  $ collection_recovery_fee                   : num  93.8 0 0 222.8 0 ...
##  $ last_pymnt_d                              : Factor w/ 137 levels "","Apr-2008",..: 21 79 9 20 93 102 102 44 78 90 ...
##  $ last_pymnt_amnt                           : num  321.1 12017.8 9338.6 17.7 319.1 ...
##  $ next_pymnt_d                              : Factor w/ 107 levels "","Apr-2008",..: 1 1 1 1 10 1 1 1 1 1 ...
##  $ last_credit_pull_d                        : Factor w/ 142 levels "","Apr-2009",..: 46 48 60 128 96 11 118 128 96 128 ...
##  $ last_fico_range_high                      : num  564 704 679 559 664 529 719 499 789 664 ...
##  $ last_fico_range_low                       : num  560 700 675 555 660 525 715 0 785 660 ...
##  $ collections_12_mths_ex_med                : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ mths_since_last_major_derog               : num  59 NA NA NA 36 74 51 NA NA NA ...
##  $ policy_code                               : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ application_type                          : Factor w/ 3 levels "","Individual",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ annual_inc_joint                          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ dti_joint                                 : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ verification_status_joint                 : Factor w/ 4 levels "","Not Verified",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ acc_now_delinq                            : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ tot_coll_amt                              : num  0 0 0 0 0 0 0 0 0 900 ...
##  $ tot_cur_bal                               : num  162110 149140 38566 64426 261815 ...
##  $ open_acc_6m                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ open_act_il                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ open_il_12m                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ open_il_24m                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ mths_since_rcnt_il                        : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ total_bal_il                              : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ il_util                                   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ open_rv_12m                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ open_rv_24m                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_bal_bc                                : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ all_util                                  : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ total_rev_hi_lim                          : num  19400 184500 27600 18300 5700 ...
##  $ inq_fi                                    : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ total_cu_tl                               : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ inq_last_12m                              : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ acc_open_past_24mths                      : num  7 5 8 6 2 4 2 6 6 7 ...
##  $ avg_cur_bal                               : num  9536 29828 3214 5857 32727 ...
##  $ bc_open_to_buy                            : num  7599 9525 6494 332 0 ...
##  $ bc_util                                   : num  41.5 4.7 69.2 93.2 103.2 ...
##  $ chargeoff_within_12_mths                  : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ delinq_amnt                               : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ mo_sin_old_il_acct                        : num  76 103 183 137 16 135 93 132 158 191 ...
##  $ mo_sin_old_rev_tl_op                      : num  290 244 265 148 170 136 167 194 148 122 ...
##  $ mo_sin_rcnt_rev_tl_op                     : num  1 1 23 8 21 7 21 15 24 2 ...
##  $ mo_sin_rcnt_tl                            : num  1 1 3 8 16 7 10 12 6 2 ...
##  $ mort_acc                                  : num  1 0 0 0 5 0 2 6 5 0 ...
##  $ mths_since_recent_bc                      : num  5 47 24 17 21 7 27 23 24 6 ...
##  $ mths_since_recent_bc_dlq                  : num  42 NA NA NA 17 60 46 NA NA NA ...
##  $ mths_since_recent_inq                     : num  1 NA 17 3 1 7 NA 16 2 1 ...
##  $ mths_since_recent_revol_delinq            : num  42 NA NA NA 17 60 46 1 NA NA ...
##  $ num_accts_ever_120_pd                     : num  4 0 0 0 1 1 2 0 0 0 ...
##  $ num_actv_bc_tl                            : num  6 1 4 1 3 3 1 3 3 3 ...
##  $ num_actv_rev_tl                           : num  9 4 7 4 5 4 3 5 4 8 ...
##  $ num_bc_sats                               : num  7 1 5 1 3 3 1 7 3 3 ...
##  $ num_bc_tl                                 : num  18 2 16 4 5 12 7 9 6 6 ...
##  $ num_il_tl                                 : num  2 8 17 12 1 16 4 4 17 6 ...
##   [list output truncated]
```

```r
##missmap(loan2) 
sapply(loan2, function(x) sum(is.na(x)))
```

```
##                                         id 
##                                          0 
##                                  member_id 
##                                     423810 
##                                  loan_amnt 
##                                          0 
##                                funded_amnt 
##                                          0 
##                            funded_amnt_inv 
##                                          0 
##                                       term 
##                                          0 
##                                   int_rate 
##                                          0 
##                                installment 
##                                          0 
##                                      grade 
##                                          0 
##                                  sub_grade 
##                                          0 
##                                  emp_title 
##                                          0 
##                                 emp_length 
##                                          0 
##                             home_ownership 
##                                          0 
##                                 annual_inc 
##                                          0 
##                        verification_status 
##                                          0 
##                                    issue_d 
##                                          0 
##                                loan_status 
##                                          0 
##                                 pymnt_plan 
##                                          0 
##                                        url 
##                                          0 
##                                       desc 
##                                          0 
##                                    purpose 
##                                          0 
##                                      title 
##                                          0 
##                                   zip_code 
##                                          0 
##                                 addr_state 
##                                          0 
##                                        dti 
##                                          0 
##                                delinq_2yrs 
##                                          0 
##                           earliest_cr_line 
##                                          0 
##                             fico_range_low 
##                                          0 
##                            fico_range_high 
##                                          0 
##                             inq_last_6mths 
##                                          0 
##                     mths_since_last_delinq 
##                                     223454 
##                     mths_since_last_record 
##                                     364812 
##                                   open_acc 
##                                          0 
##                                    pub_rec 
##                                          0 
##                                  revol_bal 
##                                          0 
##                                 revol_util 
##                                        250 
##                                  total_acc 
##                                          0 
##                        initial_list_status 
##                                          0 
##                                  out_prncp 
##                                          0 
##                              out_prncp_inv 
##                                          0 
##                                total_pymnt 
##                                          0 
##                            total_pymnt_inv 
##                                          0 
##                            total_rec_prncp 
##                                          0 
##                              total_rec_int 
##                                          0 
##                         total_rec_late_fee 
##                                          0 
##                                 recoveries 
##                                          0 
##                    collection_recovery_fee 
##                                          0 
##                               last_pymnt_d 
##                                          0 
##                            last_pymnt_amnt 
##                                          0 
##                               next_pymnt_d 
##                                          0 
##                         last_credit_pull_d 
##                                          0 
##                       last_fico_range_high 
##                                          0 
##                        last_fico_range_low 
##                                          0 
##                 collections_12_mths_ex_med 
##                                          0 
##                mths_since_last_major_derog 
##                                     324816 
##                                policy_code 
##                                          0 
##                           application_type 
##                                          0 
##                           annual_inc_joint 
##                                     423810 
##                                  dti_joint 
##                                     423810 
##                  verification_status_joint 
##                                          0 
##                             acc_now_delinq 
##                                          0 
##                               tot_coll_amt 
##                                      27741 
##                                tot_cur_bal 
##                                      27741 
##                                open_acc_6m 
##                                     423810 
##                                open_act_il 
##                                     423810 
##                                open_il_12m 
##                                     423810 
##                                open_il_24m 
##                                     423810 
##                         mths_since_rcnt_il 
##                                     423810 
##                               total_bal_il 
##                                     423810 
##                                    il_util 
##                                     423810 
##                                open_rv_12m 
##                                     423810 
##                                open_rv_24m 
##                                     423810 
##                                 max_bal_bc 
##                                     423810 
##                                   all_util 
##                                     423810 
##                           total_rev_hi_lim 
##                                      27741 
##                                     inq_fi 
##                                     423810 
##                                total_cu_tl 
##                                     423810 
##                               inq_last_12m 
##                                     423810 
##                       acc_open_past_24mths 
##                                       7495 
##                                avg_cur_bal 
##                                      27753 
##                             bc_open_to_buy 
##                                      11470 
##                                    bc_util 
##                                      11723 
##                   chargeoff_within_12_mths 
##                                          0 
##                                delinq_amnt 
##                                          0 
##                         mo_sin_old_il_acct 
##                                      41043 
##                       mo_sin_old_rev_tl_op 
##                                      27742 
##                      mo_sin_rcnt_rev_tl_op 
##                                      27742 
##                             mo_sin_rcnt_tl 
##                                      27741 
##                                   mort_acc 
##                                       7495 
##                       mths_since_recent_bc 
##                                      11074 
##                   mths_since_recent_bc_dlq 
##                                     324778 
##                      mths_since_recent_inq 
##                                      49560 
##             mths_since_recent_revol_delinq 
##                                     284597 
##                      num_accts_ever_120_pd 
##                                      27741 
##                             num_actv_bc_tl 
##                                      27741 
##                            num_actv_rev_tl 
##                                      27741 
##                                num_bc_sats 
##                                      16055 
##                                  num_bc_tl 
##                                      27741 
##                                  num_il_tl 
##                                      27741 
##                              num_op_rev_tl 
##                                      27741 
##                              num_rev_accts 
##                                      27741 
##                        num_rev_tl_bal_gt_0 
##                                      27741 
##                                   num_sats 
##                                      16055 
##                           num_tl_120dpd_2m 
##                                      35857 
##                               num_tl_30dpd 
##                                      27741 
##                         num_tl_90g_dpd_24m 
##                                      27741 
##                         num_tl_op_past_12m 
##                                      27741 
##                             pct_tl_nvr_dlq 
##                                      27894 
##                           percent_bc_gt_75 
##                                      11585 
##                       pub_rec_bankruptcies 
##                                          0 
##                                  tax_liens 
##                                          0 
##                            tot_hi_cred_lim 
##                                      27741 
##                          total_bal_ex_mort 
##                                       7495 
##                             total_bc_limit 
##                                       7495 
##                 total_il_high_credit_limit 
##                                      27741 
##                            revol_bal_joint 
##                                     423810 
##                     sec_app_fico_range_low 
##                                     423810 
##                    sec_app_fico_range_high 
##                                     423810 
##                   sec_app_earliest_cr_line 
##                                          0 
##                     sec_app_inq_last_6mths 
##                                     423810 
##                           sec_app_mort_acc 
##                                     423810 
##                           sec_app_open_acc 
##                                     423810 
##                         sec_app_revol_util 
##                                     423810 
##                        sec_app_open_act_il 
##                                     423810 
##                      sec_app_num_rev_accts 
##                                     423810 
##           sec_app_chargeoff_within_12_mths 
##                                     423810 
##         sec_app_collections_12_mths_ex_med 
##                                     423810 
##        sec_app_mths_since_last_major_derog 
##                                     423810 
##                              hardship_flag 
##                                          0 
##                              hardship_type 
##                                          0 
##                            hardship_reason 
##                                          0 
##                            hardship_status 
##                                          0 
##                              deferral_term 
##                                     423227 
##                            hardship_amount 
##                                     423227 
##                        hardship_start_date 
##                                          0 
##                          hardship_end_date 
##                                          0 
##                    payment_plan_start_date 
##                                          0 
##                            hardship_length 
##                                     423227 
##                               hardship_dpd 
##                                     423227 
##                       hardship_loan_status 
##                                          0 
## orig_projected_additional_accrued_interest 
##                                     423342 
##             hardship_payoff_balance_amount 
##                                     423227 
##               hardship_last_payment_amount 
##                                     423227 
##                        disbursement_method 
##                                          0 
##                       debt_settlement_flag 
##                                          0 
##                  debt_settlement_flag_date 
##                                          0 
##                          settlement_status 
##                                          0 
##                            settlement_date 
##                                          0 
##                          settlement_amount 
##                                     417521 
##                      settlement_percentage 
##                                     417521 
##                            settlement_term 
##                                     417521
```

```r
sapply(loan2, function(x) length(unique(x)))
```

```
##                                         id 
##                                     423810 
##                                  member_id 
##                                          1 
##                                  loan_amnt 
##                                       1339 
##                                funded_amnt 
##                                       1339 
##                            funded_amnt_inv 
##                                       1723 
##                                       term 
##                                          2 
##                                   int_rate 
##                                        188 
##                                installment 
##                                      49362 
##                                      grade 
##                                          7 
##                                  sub_grade 
##                                         35 
##                                  emp_title 
##                                     182442 
##                                 emp_length 
##                                         12 
##                             home_ownership 
##                                          6 
##                                 annual_inc 
##                                      29318 
##                        verification_status 
##                                          3 
##                                    issue_d 
##                                         36 
##                                loan_status 
##                                          7 
##                                 pymnt_plan 
##                                          2 
##                                        url 
##                                     423810 
##                                       desc 
##                                      95494 
##                                    purpose 
##                                         13 
##                                      title 
##                                      45960 
##                                   zip_code 
##                                        875 
##                                 addr_state 
##                                         50 
##                                        dti 
##                                       3997 
##                                delinq_2yrs 
##                                         24 
##                           earliest_cr_line 
##                                        660 
##                             fico_range_low 
##                                         38 
##                            fico_range_high 
##                                         38 
##                             inq_last_6mths 
##                                          9 
##                     mths_since_last_delinq 
##                                        146 
##                     mths_since_last_record 
##                                        123 
##                                   open_acc 
##                                         62 
##                                    pub_rec 
##                                         26 
##                                  revol_bal 
##                                      56275 
##                                 revol_util 
##                                       1173 
##                                  total_acc 
##                                        111 
##                        initial_list_status 
##                                          2 
##                                  out_prncp 
##                                       9493 
##                              out_prncp_inv 
##                                       9647 
##                                total_pymnt 
##                                     403488 
##                            total_pymnt_inv 
##                                     342872 
##                            total_rec_prncp 
##                                      76009 
##                              total_rec_int 
##                                     274709 
##                         total_rec_late_fee 
##                                       7172 
##                                 recoveries 
##                                      48370 
##                    collection_recovery_fee 
##                                      49371 
##                               last_pymnt_d 
##                                         87 
##                            last_pymnt_amnt 
##                                     255676 
##                               next_pymnt_d 
##                                          3 
##                         last_credit_pull_d 
##                                         88 
##                       last_fico_range_high 
##                                         72 
##                        last_fico_range_low 
##                                         71 
##                 collections_12_mths_ex_med 
##                                          9 
##                mths_since_last_major_derog 
##                                        153 
##                                policy_code 
##                                          1 
##                           application_type 
##                                          1 
##                           annual_inc_joint 
##                                          1 
##                                  dti_joint 
##                                          1 
##                  verification_status_joint 
##                                          1 
##                             acc_now_delinq 
##                                          6 
##                               tot_coll_amt 
##                                       6322 
##                                tot_cur_bal 
##                                     220706 
##                                open_acc_6m 
##                                          1 
##                                open_act_il 
##                                          1 
##                                open_il_12m 
##                                          1 
##                                open_il_24m 
##                                          1 
##                         mths_since_rcnt_il 
##                                          1 
##                               total_bal_il 
##                                          1 
##                                    il_util 
##                                          1 
##                                open_rv_12m 
##                                          1 
##                                open_rv_24m 
##                                          1 
##                                 max_bal_bc 
##                                          1 
##                                   all_util 
##                                          1 
##                           total_rev_hi_lim 
##                                      14613 
##                                     inq_fi 
##                                          1 
##                                total_cu_tl 
##                                          1 
##                               inq_last_12m 
##                                          1 
##                       acc_open_past_24mths 
##                                         40 
##                                avg_cur_bal 
##                                      55982 
##                             bc_open_to_buy 
##                                      47225 
##                                    bc_util 
##                                       1295 
##                   chargeoff_within_12_mths 
##                                          8 
##                                delinq_amnt 
##                                        726 
##                         mo_sin_old_il_acct 
##                                        464 
##                       mo_sin_old_rev_tl_op 
##                                        692 
##                      mo_sin_rcnt_rev_tl_op 
##                                        216 
##                             mo_sin_rcnt_tl 
##                                        144 
##                                   mort_acc 
##                                         37 
##                       mths_since_recent_bc 
##                                        403 
##                   mths_since_recent_bc_dlq 
##                                        141 
##                      mths_since_recent_inq 
##                                         27 
##             mths_since_recent_revol_delinq 
##                                        149 
##                      num_accts_ever_120_pd 
##                                         31 
##                             num_actv_bc_tl 
##                                         29 
##                            num_actv_rev_tl 
##                                         38 
##                                num_bc_sats 
##                                         37 
##                                  num_bc_tl 
##                                         57 
##                                  num_il_tl 
##                                         86 
##                              num_op_rev_tl 
##                                         57 
##                              num_rev_accts 
##                                         86 
##                        num_rev_tl_bal_gt_0 
##                                         37 
##                                   num_sats 
##                                         63 
##                           num_tl_120dpd_2m 
##                                          4 
##                               num_tl_30dpd 
##                                          6 
##                         num_tl_90g_dpd_24m 
##                                         23 
##                         num_tl_op_past_12m 
##                                         27 
##                             pct_tl_nvr_dlq 
##                                        481 
##                           percent_bc_gt_75 
##                                        181 
##                       pub_rec_bankruptcies 
##                                         10 
##                                  tax_liens 
##                                         24 
##                            tot_hi_cred_lim 
##                                     225961 
##                          total_bal_ex_mort 
##                                     117516 
##                             total_bc_limit 
##                                      12191 
##                 total_il_high_credit_limit 
##                                     104847 
##                            revol_bal_joint 
##                                          1 
##                     sec_app_fico_range_low 
##                                          1 
##                    sec_app_fico_range_high 
##                                          1 
##                   sec_app_earliest_cr_line 
##                                          1 
##                     sec_app_inq_last_6mths 
##                                          1 
##                           sec_app_mort_acc 
##                                          1 
##                           sec_app_open_acc 
##                                          1 
##                         sec_app_revol_util 
##                                          1 
##                        sec_app_open_act_il 
##                                          1 
##                      sec_app_num_rev_accts 
##                                          1 
##           sec_app_chargeoff_within_12_mths 
##                                          1 
##         sec_app_collections_12_mths_ex_med 
##                                          1 
##        sec_app_mths_since_last_major_derog 
##                                          1 
##                              hardship_flag 
##                                          2 
##                              hardship_type 
##                                          2 
##                            hardship_reason 
##                                         10 
##                            hardship_status 
##                                          4 
##                              deferral_term 
##                                          2 
##                            hardship_amount 
##                                        572 
##                        hardship_start_date 
##                                         26 
##                          hardship_end_date 
##                                         28 
##                    payment_plan_start_date 
##                                         27 
##                            hardship_length 
##                                          2 
##                               hardship_dpd 
##                                         33 
##                       hardship_loan_status 
##                                          5 
## orig_projected_additional_accrued_interest 
##                                        460 
##             hardship_payoff_balance_amount 
##                                        583 
##               hardship_last_payment_amount 
##                                        575 
##                        disbursement_method 
##                                          1 
##                       debt_settlement_flag 
##                                          2 
##                  debt_settlement_flag_date 
##                                         71 
##                          settlement_status 
##                                          4 
##                            settlement_date 
##                                         76 
##                          settlement_amount 
##                                       5348 
##                      settlement_percentage 
##                                       1146 
##                            settlement_term 
##                                         28
```


```r
loan2 %>% count()
```

```
## # A tibble: 1 x 1
##        n
##    <int>
## 1 423810
```

```r
loan2 %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))/10000))
```

```
##   id member_id loan_amnt funded_amnt funded_amnt_inv term int_rate
## 1  0    42.381         0           0               0    0        0
##   installment grade sub_grade emp_title emp_length home_ownership
## 1           0     0         0         0          0              0
##   annual_inc verification_status issue_d loan_status pymnt_plan url desc
## 1          0                   0       0           0          0   0    0
##   purpose title zip_code addr_state dti delinq_2yrs earliest_cr_line
## 1       0     0        0          0   0           0                0
##   fico_range_low fico_range_high inq_last_6mths mths_since_last_delinq
## 1              0               0              0                22.3454
##   mths_since_last_record open_acc pub_rec revol_bal revol_util total_acc
## 1                36.4812        0       0         0      0.025         0
##   initial_list_status out_prncp out_prncp_inv total_pymnt total_pymnt_inv
## 1                   0         0             0           0               0
##   total_rec_prncp total_rec_int total_rec_late_fee recoveries
## 1               0             0                  0          0
##   collection_recovery_fee last_pymnt_d last_pymnt_amnt next_pymnt_d
## 1                       0            0               0            0
##   last_credit_pull_d last_fico_range_high last_fico_range_low
## 1                  0                    0                   0
##   collections_12_mths_ex_med mths_since_last_major_derog policy_code
## 1                          0                     32.4816           0
##   application_type annual_inc_joint dti_joint verification_status_joint
## 1                0           42.381    42.381                         0
##   acc_now_delinq tot_coll_amt tot_cur_bal open_acc_6m open_act_il
## 1              0       2.7741      2.7741      42.381      42.381
##   open_il_12m open_il_24m mths_since_rcnt_il total_bal_il il_util
## 1      42.381      42.381             42.381       42.381  42.381
##   open_rv_12m open_rv_24m max_bal_bc all_util total_rev_hi_lim inq_fi
## 1      42.381      42.381     42.381   42.381           2.7741 42.381
##   total_cu_tl inq_last_12m acc_open_past_24mths avg_cur_bal bc_open_to_buy
## 1      42.381       42.381               0.7495      2.7753          1.147
##   bc_util chargeoff_within_12_mths delinq_amnt mo_sin_old_il_acct
## 1  1.1723                        0           0             4.1043
##   mo_sin_old_rev_tl_op mo_sin_rcnt_rev_tl_op mo_sin_rcnt_tl mort_acc
## 1               2.7742                2.7742         2.7741   0.7495
##   mths_since_recent_bc mths_since_recent_bc_dlq mths_since_recent_inq
## 1               1.1074                  32.4778                 4.956
##   mths_since_recent_revol_delinq num_accts_ever_120_pd num_actv_bc_tl
## 1                        28.4597                2.7741         2.7741
##   num_actv_rev_tl num_bc_sats num_bc_tl num_il_tl num_op_rev_tl
## 1          2.7741      1.6055    2.7741    2.7741        2.7741
##   num_rev_accts num_rev_tl_bal_gt_0 num_sats num_tl_120dpd_2m num_tl_30dpd
## 1        2.7741              2.7741   1.6055           3.5857       2.7741
##   num_tl_90g_dpd_24m num_tl_op_past_12m pct_tl_nvr_dlq percent_bc_gt_75
## 1             2.7741             2.7741         2.7894           1.1585
##   pub_rec_bankruptcies tax_liens tot_hi_cred_lim total_bal_ex_mort
## 1                    0         0          2.7741            0.7495
##   total_bc_limit total_il_high_credit_limit revol_bal_joint
## 1         0.7495                     2.7741          42.381
##   sec_app_fico_range_low sec_app_fico_range_high sec_app_earliest_cr_line
## 1                 42.381                  42.381                        0
##   sec_app_inq_last_6mths sec_app_mort_acc sec_app_open_acc
## 1                 42.381           42.381           42.381
##   sec_app_revol_util sec_app_open_act_il sec_app_num_rev_accts
## 1             42.381              42.381                42.381
##   sec_app_chargeoff_within_12_mths sec_app_collections_12_mths_ex_med
## 1                           42.381                             42.381
##   sec_app_mths_since_last_major_derog hardship_flag hardship_type
## 1                              42.381             0             0
##   hardship_reason hardship_status deferral_term hardship_amount
## 1               0               0       42.3227         42.3227
##   hardship_start_date hardship_end_date payment_plan_start_date
## 1                   0                 0                       0
##   hardship_length hardship_dpd hardship_loan_status
## 1         42.3227      42.3227                    0
##   orig_projected_additional_accrued_interest
## 1                                    42.3342
##   hardship_payoff_balance_amount hardship_last_payment_amount
## 1                        42.3227                      42.3227
##   disbursement_method debt_settlement_flag debt_settlement_flag_date
## 1                   0                    0                         0
##   settlement_status settlement_date settlement_amount
## 1                 0               0           41.7521
##   settlement_percentage settlement_term
## 1               41.7521         41.7521
```



```r
loan3 <- loan2[, colSums(is.na(loan2))==0] 

loan3 %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))/10000))
```

```
##   id loan_amnt funded_amnt funded_amnt_inv term int_rate installment grade
## 1  0         0           0               0    0        0           0     0
##   sub_grade emp_title emp_length home_ownership annual_inc
## 1         0         0          0              0          0
##   verification_status issue_d loan_status pymnt_plan url desc purpose
## 1                   0       0           0          0   0    0       0
##   title zip_code addr_state dti delinq_2yrs earliest_cr_line
## 1     0        0          0   0           0                0
##   fico_range_low fico_range_high inq_last_6mths open_acc pub_rec revol_bal
## 1              0               0              0        0       0         0
##   total_acc initial_list_status out_prncp out_prncp_inv total_pymnt
## 1         0                   0         0             0           0
##   total_pymnt_inv total_rec_prncp total_rec_int total_rec_late_fee
## 1               0               0             0                  0
##   recoveries collection_recovery_fee last_pymnt_d last_pymnt_amnt
## 1          0                       0            0               0
##   next_pymnt_d last_credit_pull_d last_fico_range_high last_fico_range_low
## 1            0                  0                    0                   0
##   collections_12_mths_ex_med policy_code application_type
## 1                          0           0                0
##   verification_status_joint acc_now_delinq chargeoff_within_12_mths
## 1                         0              0                        0
##   delinq_amnt pub_rec_bankruptcies tax_liens sec_app_earliest_cr_line
## 1           0                    0         0                        0
##   hardship_flag hardship_type hardship_reason hardship_status
## 1             0             0               0               0
##   hardship_start_date hardship_end_date payment_plan_start_date
## 1                   0                 0                       0
##   hardship_loan_status disbursement_method debt_settlement_flag
## 1                    0                   0                    0
##   debt_settlement_flag_date settlement_status settlement_date
## 1                         0                 0               0
```


```r
sapply(loan3, function(x) length(unique(x)))
```

```
##                         id                  loan_amnt 
##                     423810                       1339 
##                funded_amnt            funded_amnt_inv 
##                       1339                       1723 
##                       term                   int_rate 
##                          2                        188 
##                installment                      grade 
##                      49362                          7 
##                  sub_grade                  emp_title 
##                         35                     182442 
##                 emp_length             home_ownership 
##                         12                          6 
##                 annual_inc        verification_status 
##                      29318                          3 
##                    issue_d                loan_status 
##                         36                          7 
##                 pymnt_plan                        url 
##                          2                     423810 
##                       desc                    purpose 
##                      95494                         13 
##                      title                   zip_code 
##                      45960                        875 
##                 addr_state                        dti 
##                         50                       3997 
##                delinq_2yrs           earliest_cr_line 
##                         24                        660 
##             fico_range_low            fico_range_high 
##                         38                         38 
##             inq_last_6mths                   open_acc 
##                          9                         62 
##                    pub_rec                  revol_bal 
##                         26                      56275 
##                  total_acc        initial_list_status 
##                        111                          2 
##                  out_prncp              out_prncp_inv 
##                       9493                       9647 
##                total_pymnt            total_pymnt_inv 
##                     403488                     342872 
##            total_rec_prncp              total_rec_int 
##                      76009                     274709 
##         total_rec_late_fee                 recoveries 
##                       7172                      48370 
##    collection_recovery_fee               last_pymnt_d 
##                      49371                         87 
##            last_pymnt_amnt               next_pymnt_d 
##                     255676                          3 
##         last_credit_pull_d       last_fico_range_high 
##                         88                         72 
##        last_fico_range_low collections_12_mths_ex_med 
##                         71                          9 
##                policy_code           application_type 
##                          1                          1 
##  verification_status_joint             acc_now_delinq 
##                          1                          6 
##   chargeoff_within_12_mths                delinq_amnt 
##                          8                        726 
##       pub_rec_bankruptcies                  tax_liens 
##                         10                         24 
##   sec_app_earliest_cr_line              hardship_flag 
##                          1                          2 
##              hardship_type            hardship_reason 
##                          2                         10 
##            hardship_status        hardship_start_date 
##                          4                         26 
##          hardship_end_date    payment_plan_start_date 
##                         28                         27 
##       hardship_loan_status        disbursement_method 
##                          5                          1 
##       debt_settlement_flag  debt_settlement_flag_date 
##                          2                         71 
##          settlement_status            settlement_date 
##                          4                         76
```

```r
loan3 <- loan3 %>% select(-id,-desc,-emp_title,-zip_code ,-application_type, -sec_app_earliest_cr_line, -disbursement_method, -url, -title, -policy_code) %>%
  na.omit()

dim(loan3)
```

```
## [1] 423810     62
```


```r
loan3$loan_status <- as.character(loan3$loan_status)
loan3 <- loan3 [((loan3$loan_status=="Charged Off") | (loan3$loan_status=="Fully Paid")),]
table(loan3$loan_status)
```

```
## 
## Charged Off  Fully Paid 
##       70829      340444
```

```r
table(loan3$loan_status, loan3$grade)
```

```
##              
##                           A      B      C      D      E      F      G
##   Charged Off      0   3567  13701  21065  17052  10057   4241   1146
##   Fully Paid       0  60888 108858  91442  50145  20420   7186   1505
```

```r
head(loan3)
```

```
##         loan_amnt funded_amnt funded_amnt_inv       term int_rate
## 1117061     10400       10400           10400  36 months     6.99
## 1117062     15000       15000           15000  60 months    12.39
## 1117063      9600        9600            9600  36 months    13.66
## 1117064      7650        7650            7650  36 months    13.66
## 1117066     21425       21425           21425  60 months    15.59
## 1117067     17000       17000           17000  36 months    13.66
##         installment grade sub_grade emp_length home_ownership annual_inc
## 1117061      321.08     A        A3    8 years       MORTGAGE      58000
## 1117062      336.64     C        C1  10+ years           RENT      78000
## 1117063      326.53     C        C3  10+ years           RENT      69000
## 1117064      260.20     C        C3   < 1 year           RENT      50000
## 1117066      516.36     D        D1    6 years           RENT      63800
## 1117067      578.22     C        C3  10+ years       MORTGAGE      75000
##         verification_status    issue_d loan_status pymnt_plan
## 1117061        Not Verified 2014-12-01 Charged Off          n
## 1117062     Source Verified 2014-12-01  Fully Paid          n
## 1117063     Source Verified 2014-12-01  Fully Paid          n
## 1117064     Source Verified 2014-12-01 Charged Off          n
## 1117066     Source Verified 2014-12-01  Fully Paid          n
## 1117067            Verified 2014-12-01  Fully Paid          n
##                    purpose addr_state   dti delinq_2yrs earliest_cr_line
## 1117061        credit_card         CA 14.92           0         Sep-1989
## 1117062 debt_consolidation         VA 12.03           0         Aug-1994
## 1117063 debt_consolidation         NJ 25.81           0         Nov-1992
## 1117064 debt_consolidation         AZ 34.81           0         Aug-2002
## 1117066        credit_card         MO 18.49           0         Aug-2003
## 1117067 debt_consolidation         NY 23.63           0         Jan-2001
##         fico_range_low fico_range_high inq_last_6mths open_acc pub_rec
## 1117061            710             714              2       17       0
## 1117062            750             754              0        6       0
## 1117063            680             684              0       12       0
## 1117064            685             689              1       11       0
## 1117066            685             689              0       10       0
## 1117067            675             679              0        7       0
##         revol_bal total_acc initial_list_status out_prncp out_prncp_inv
## 1117061      6133        36                   w         0             0
## 1117062    138008        17                   w         0             0
## 1117063     16388        44                   f         0             0
## 1117064     16822        20                   f         0             0
## 1117066     16374        35                   w         0             0
## 1117067      5063        31                   f         0             0
##         total_pymnt total_pymnt_inv total_rec_prncp total_rec_int
## 1117061     6611.69         6611.69         5217.75        872.67
## 1117062    17392.37        17392.37        15000.00       2392.37
## 1117063     9973.43         9973.43         9600.00        373.43
## 1117064     2281.98         2281.98          704.38        339.61
## 1117066    25512.20        25512.20        21425.00       4087.20
## 1117067    19562.31        19562.31        17000.00       2562.31
##         total_rec_late_fee recoveries collection_recovery_fee last_pymnt_d
## 1117061                  0     521.27                 93.8286     Aug-2016
## 1117062                  0       0.00                  0.0000     Jun-2016
## 1117063                  0       0.00                  0.0000     Apr-2015
## 1117064                  0    1237.99                222.8382     Aug-2015
## 1117066                  0       0.00                  0.0000     May-2016
## 1117067                  0       0.00                  0.0000     May-2016
##         last_pymnt_amnt next_pymnt_d last_credit_pull_d
## 1117061          321.08                        Feb-2017
## 1117062        12017.81                        Feb-2019
## 1117063         9338.58                        Jan-2019
## 1117064           17.70                        Oct-2016
## 1117066        17813.19                        Apr-2018
## 1117067        10888.01                        Nov-2018
##         last_fico_range_high last_fico_range_low
## 1117061                  564                 560
## 1117062                  704                 700
## 1117063                  679                 675
## 1117064                  559                 555
## 1117066                  529                 525
## 1117067                  719                 715
##         collections_12_mths_ex_med verification_status_joint
## 1117061                          0                          
## 1117062                          0                          
## 1117063                          0                          
## 1117064                          0                          
## 1117066                          0                          
## 1117067                          0                          
##         acc_now_delinq chargeoff_within_12_mths delinq_amnt
## 1117061              0                        0           0
## 1117062              0                        0           0
## 1117063              0                        0           0
## 1117064              0                        0           0
## 1117066              0                        0           0
## 1117067              0                        0           0
##         pub_rec_bankruptcies tax_liens hardship_flag hardship_type
## 1117061                    0         0             N              
## 1117062                    0         0             N              
## 1117063                    0         0             N              
## 1117064                    0         0             N              
## 1117066                    0         0             N              
## 1117067                    0         0             N              
##         hardship_reason hardship_status hardship_start_date
## 1117061                                                    
## 1117062                                                    
## 1117063                                                    
## 1117064                                                    
## 1117066                                                    
## 1117067                                                    
##         hardship_end_date payment_plan_start_date hardship_loan_status
## 1117061                                                               
## 1117062                                                               
## 1117063                                                               
## 1117064                                                               
## 1117066                                                               
## 1117067                                                               
##         debt_settlement_flag debt_settlement_flag_date settlement_status
## 1117061                    N                                            
## 1117062                    N                                            
## 1117063                    N                                            
## 1117064                    N                                            
## 1117066                    N                                            
## 1117067                    N                                            
##         settlement_date
## 1117061                
## 1117062                
## 1117063                
## 1117064                
## 1117066                
## 1117067
```

```r
ggplot(loan3, aes(x = int_rate)) + geom_histogram(aes(fill = grade)) + facet_wrap(~loan_status, ncol = 1)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
table(loan3$loan_status, loan3$term)
```

```
##              
##                       36 months  60 months
##   Charged Off      0      40596      30233
##   Fully Paid       0     265866      74578
```

```r
loan3$loan_status <- as.factor(loan3$loan_status)
```

```r
index = createDataPartition(y = loan3$loan_status, p = 0.90)[[1]]
loan3.sample <- loan3[-index,]
ggplot(loan3.sample, aes(x = loan_amnt, y = int_rate)) + geom_point(aes(color = term))
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
loan4<-loan3
```


```r
loan4$bad_loan <- ifelse(loan3$loan_status %in% c("Charged Off", "Default", "Late (16-30 days)", "Late (31-120 days)", "Does not meet the credit policy. Status:Charged Off", "In Grace Period"), 1, 
                             ifelse(loan3$loan_status == "Fully Paid", 0, "Other"))
```


```r
ggplot(loan4, aes(bad_loan, group= verification_status_joint)) + geom_bar(aes(y= ..prop.., fill = factor(..x..)), stat="count")+  facet_grid(~verification_status_joint) + labs(title = "The proportion of bad loan by verification status")
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
ggplot(loan3, aes(x= as.factor(purpose) , y=loan_amnt)) + geom_boxplot(aes(fill = grade))+ 
  labs(x= "Purpose", y= "Loan Amount")
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
library(rpart.plot)
library(rpart)
library(rattle)
indx<-createDataPartition(loan3$loan_status,p=0.75,list = FALSE)
loan3_test <- loan3[-indx,]
loan3_train <- loan3[indx,]

loans.rpart.0 <- rpart(loan_status ~ ., data = loan3_train)
loans.rpart.1 <- rpart(loan_status ~ . , data = loan3_train, 
                      control=rpart.control(minsplit=10, minbucket = 3, cp=0.001))
fancyRpartPlot(loans.rpart.1)
```

```
## Warning: labs do not fit even at cex 0.15, there may be some overplotting
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
predictions <- (predict(loans.rpart.0, loan3_train,type = "class"))
confusionMatrix(predictions, loan3_train$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off       46426        532
##   Fully Paid         6696     254801
##                                          
##                Accuracy : 0.9766         
##                  95% CI : (0.976, 0.9771)
##     No Information Rate : 0.8278         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9139         
##                                          
##  Mcnemar's Test P-Value : < 2.2e-16      
##                                          
##             Sensitivity : 0.8740         
##             Specificity : 0.9979         
##          Pos Pred Value : 0.9887         
##          Neg Pred Value : 0.9744         
##              Prevalence : 0.1722         
##          Detection Rate : 0.1505         
##    Detection Prevalence : 0.1522         
##       Balanced Accuracy : 0.9359         
##                                          
##        'Positive' Class : Charged Off    
## 
```

```r
library(ROCR)
library(ROSE)
roc.curve(loan3_test$loan_status, predict(loans.rpart.0, loan3_test, type = "prob")[,1], plot = TRUE)
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```
## Area under the curve (AUC): 0.942
```


```r
library(trelliscopejs)
library(gapminder)

qplot(purpose, int_rate, data = loan3) + ylim(range.default(loan3$int_rate)) +
  theme_bw() + facet_trelliscope(~ addr_state,nrow = 2, ncol = 7, width = 300,as_plotly = TRUE,path = "Shah_Maitry_stat652_project.pdf" )
```

```
## using data from the first layer
```

```
## writing panels       [>-------------------------]   5% 3/55 eta:11swriting panels       [=>------------------------]   7% 4/55 eta: 9swriting panels       [=>------------------------]   9% 5/55 eta: 7swriting panels       [==>-----------------------]  11% 6/55 eta: 6swriting panels       [==>-----------------------]  13% 7/55 eta: 6swriting panels       [===>----------------------]  15% 8/55 eta: 6swriting panels       [===>----------------------]  16% 9/55 eta: 5swriting panels       [====>--------------------]  18% 10/55 eta: 5swriting panels       [====>--------------------]  20% 11/55 eta: 4swriting panels       [====>--------------------]  22% 12/55 eta: 4swriting panels       [=====>-------------------]  24% 13/55 eta: 4swriting panels       [=====>-------------------]  25% 14/55 eta: 4swriting panels       [======>------------------]  27% 15/55 eta: 4swriting panels       [======>------------------]  29% 16/55 eta: 3swriting panels       [=======>-----------------]  31% 17/55 eta: 3swriting panels       [=======>-----------------]  33% 18/55 eta: 3swriting panels       [========>----------------]  35% 19/55 eta: 3swriting panels       [========>----------------]  36% 20/55 eta: 3swriting panels       [=========>---------------]  38% 21/55 eta: 3swriting panels       [=========>---------------]  40% 22/55 eta: 2swriting panels       [=========>---------------]  42% 23/55 eta: 2swriting panels       [==========>--------------]  44% 24/55 eta: 2swriting panels       [==========>--------------]  45% 25/55 eta: 2swriting panels       [===========>-------------]  47% 26/55 eta: 2swriting panels       [===========>-------------]  49% 27/55 eta: 2swriting panels       [============>------------]  51% 28/55 eta: 2swriting panels       [============>------------]  53% 29/55 eta: 2swriting panels       [=============>-----------]  55% 30/55 eta: 2swriting panels       [=============>-----------]  56% 31/55 eta: 2swriting panels       [==============>----------]  58% 32/55 eta: 2swriting panels       [==============>----------]  60% 33/55 eta: 1swriting panels       [==============>----------]  62% 34/55 eta: 1swriting panels       [===============>---------]  64% 35/55 eta: 1swriting panels       [===============>---------]  65% 36/55 eta: 1swriting panels       [================>--------]  67% 37/55 eta: 1swriting panels       [================>--------]  69% 38/55 eta: 1swriting panels       [=================>-------]  71% 39/55 eta: 1swriting panels       [=================>-------]  73% 40/55 eta: 1swriting panels       [==================>------]  75% 41/55 eta: 1swriting panels       [==================>------]  76% 42/55 eta: 1swriting panels       [===================>-----]  78% 43/55 eta: 1swriting panels       [===================>-----]  80% 44/55 eta: 1swriting panels       [===================>-----]  82% 45/55 eta: 1swriting panels       [====================>----]  84% 46/55 eta: 1swriting panels       [====================>----]  85% 47/55 eta: 0swriting panels       [=====================>---]  87% 48/55 eta: 0swriting panels       [=====================>---]  89% 49/55 eta: 0swriting panels       [======================>--]  91% 50/55 eta: 0sbuilding display obj [======================>--]  93% 51/55 eta: 0swriting cognostics   [=======================>-]  95% 52/55 eta: 0swriting thumbnail    [=======================>-]  96% 53/55 eta: 0sPhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
## writing display list [========================>]  98% 54/55 eta: 0swriting app config   [=========================] 100% 55/55 eta: 0s                                                                   
```

<!--html_preserve--><div id="htmlwidget-783dab44689483069313" style="width:900px;height:550px;" class="trelliscopejs_widget html-widget"></div>
<script type="application/json" data-for="htmlwidget-783dab44689483069313">{"x":{"id":"caa4d9ac","config_info":"'Shah_Maitry_stat652_project.pdf/appfiles/config.jsonp'","self_contained":false,"latest_display":{"name":"by_addr_state","group":"common"},"spa":false,"in_knitr":true,"in_shiny":false,"in_notebook":false},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
sampledf<-loan3[sample(nrow(loan3), 1000), ]
index<-createDataPartition(sampledf$loan_status,p=0.75,list = FALSE)
sampledf_test <- sampledf[-index,]
sampledf_train <- sampledf[index,]
```

#checking accuracy and kappa using CART and KNN model Package: caret train fucntion

```r
library(e1071)
#10-fold cross validation
control<-trainControl(method = "cv",number = 10)
metric<- "Accuracy"

#nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(loan_status~., data=sampledf, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(loan_status~., data=sampledf, method="knn", metric=metric, trControl=control)

set.seed(7)
fit.svm <- train(loan_status~., data=sampledf, method="svmRadial", metric=metric, trControl=control)
```

```
## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.

## Warning in .local(x, ...): Variable(s) `' constant. Cannot scale data.
```

```r
# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn,svm=fit.svm))
summary(results)
```

```
## 
## Call:
## summary.resamples(object = results)
## 
## Models: cart, knn, svm 
## Number of resamples: 10 
## 
## Accuracy 
##           Min.   1st Qu.    Median      Mean  3rd Qu.     Max. NA's
## cart 0.9108911 0.9600000 0.9698990 0.9660685 0.980098 0.990000    0
## knn  0.9100000 0.9201980 0.9346970 0.9330174 0.940294 0.970000    0
## svm  0.9400000 0.9549242 0.9701485 0.9669687 0.980000 0.980198    0
## 
## Kappa 
##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## cart 0.5936522 0.8470344 0.8876168 0.8641926 0.9246586 0.9637155    0
## knn  0.5664740 0.6327604 0.7007465 0.6982527 0.7323926 0.8792271    0
## svm  0.7368421 0.8081884 0.8793270 0.8613214 0.9216301 0.9217661    0
```

```r
dotplot(results)
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# estimate skill of KNN on the validation dataset
predictions <- predict(fit.knn, sampledf)
confusionMatrix(predictions, sampledf$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off         106          0
##   Fully Paid           52        842
##                                           
##                Accuracy : 0.948           
##                  95% CI : (0.9324, 0.9609)
##     No Information Rate : 0.842           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7744          
##                                           
##  Mcnemar's Test P-Value : 1.522e-12       
##                                           
##             Sensitivity : 0.6709          
##             Specificity : 1.0000          
##          Pos Pred Value : 1.0000          
##          Neg Pred Value : 0.9418          
##              Prevalence : 0.1580          
##          Detection Rate : 0.1060          
##    Detection Prevalence : 0.1060          
##       Balanced Accuracy : 0.8354          
##                                           
##        'Positive' Class : Charged Off     
## 
```

```r
# estimate skill of CART on the validation dataset
predictions <- predict(fit.cart, sampledf)
confusionMatrix(predictions, sampledf$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off         145          3
##   Fully Paid           13        839
##                                           
##                Accuracy : 0.984           
##                  95% CI : (0.9741, 0.9908)
##     No Information Rate : 0.842           
##     P-Value [Acc > NIR] : < 2e-16         
##                                           
##                   Kappa : 0.9383          
##                                           
##  Mcnemar's Test P-Value : 0.02445         
##                                           
##             Sensitivity : 0.9177          
##             Specificity : 0.9964          
##          Pos Pred Value : 0.9797          
##          Neg Pred Value : 0.9847          
##              Prevalence : 0.1580          
##          Detection Rate : 0.1450          
##    Detection Prevalence : 0.1480          
##       Balanced Accuracy : 0.9571          
##                                           
##        'Positive' Class : Charged Off     
## 
```

```r
# estimate skill of SVM on the validation dataset
predictions <- predict(fit.svm, sampledf)
confusionMatrix(predictions, sampledf$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off         133          0
##   Fully Paid           25        842
##                                           
##                Accuracy : 0.975           
##                  95% CI : (0.9633, 0.9838)
##     No Information Rate : 0.842           
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.8996          
##                                           
##  Mcnemar's Test P-Value : 1.587e-06       
##                                           
##             Sensitivity : 0.8418          
##             Specificity : 1.0000          
##          Pos Pred Value : 1.0000          
##          Neg Pred Value : 0.9712          
##              Prevalence : 0.1580          
##          Detection Rate : 0.1330          
##    Detection Prevalence : 0.1330          
##       Balanced Accuracy : 0.9209          
##                                           
##        'Positive' Class : Charged Off     
## 
```
#ROC for CART mdoel

```r
library(ROCR)

#trainning

p <- predict(fit.cart, newdata=sampledf_train, type="prob")

pr <- prediction(p[,2], sampledf_train$loan_status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
```

```
## [1] 0.9752154
```
#ROC for KNN mdoel

```r
library(ROCR)

#trainning

p <- predict(fit.knn, newdata=sampledf_train, type="prob")

pr <- prediction(p[,2], sampledf_train$loan_status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

```r
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
```

```
## [1] 0.9935246
```

```r
library(ROCR)

#trainning

p <- predict(fit.cart, newdata=sampledf_train, type="prob")

pr <- prediction(p[,2], sampledf_train$loan_status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
```

```
## [1] 0.9752154
```


#C5.0 Decision Tree

```r
## Boosting ----

library(C50)
sampledf <- rbind(sampledf_train,sampledf_test)

# fixing empty character level names

levels(sampledf$term)[1] = "missing"
levels(sampledf$sub_grade)[1] = "missing"
levels(sampledf$grade)[1] = "missing"
levels(sampledf$debt_settlement_flag)[1] = "missing"
levels(sampledf$home_ownership)[1] = "missing"

index<-createDataPartition(sampledf$loan_status,p=0.75,list = FALSE)
sampledf_test <- sampledf[-index,]
sampledf_train <- sampledf[index,]

  tic()
m_c50_bst <- C5.0(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf_train, trials = 5, rule = FALSE)
toc()
```

```
## 0.093 sec elapsed
```

```r
  summary(m_c50_bst)
```

```
## 
## Call:
## C5.0.formula(formula = loan_status ~ loan_amnt + funded_amnt
##  + tax_liens + debt_settlement_flag, data = sampledf_train, trials =
##  5, rule = FALSE)
## 
## 
## C5.0 [Release 2.07 GPL Edition]  	Mon Mar 16 11:19:06 2020
## -------------------------------
## 
## Class specified by attribute `outcome'
## 
## Read 751 cases (39 attributes) from undefined.data
## 
## -----  Trial 0:  -----
## 
## Decision tree:
## 
## recoveries > 0: Charged Off (98)
## recoveries <= 0:
## :...last_fico_range_high > 639: Fully Paid (521/1)
##     last_fico_range_high <= 639:
##     :...last_pymnt_amnt > 720.9: Fully Paid (78)
##         last_pymnt_amnt <= 720.9:
##         :...term in {missing,36 months}: Fully Paid (37/9)
##             term = 60 months:
##             :...total_rec_prncp <= 16200: Charged Off (12/1)
##                 total_rec_prncp > 16200: Fully Paid (5)
## 
## -----  Trial 1:  -----
## 
## Decision tree:
## 
## last_fico_range_high > 674: Fully Paid (330.9/2.3)
## last_fico_range_high <= 674:
## :...last_pymnt_amnt > 1200: Fully Paid (86.7/1.5)
##     last_pymnt_amnt <= 1200:
##     :...sub_grade in {missing,A3,F3,F4,F5,G2,G4,G5}: Charged Off (0)
##         sub_grade in {A1,A5,B2,C1,D5,E1,E5}: Fully Paid (56/15.1)
##         sub_grade in {A2,A4,B1,B3,B4,B5,C2,C3,C4,C5,D1,D2,D3,D4,E2,E3,E4,F1,F2,
##         :             G1,G3}:
##         :...total_rec_prncp <= 13013.46: Charged Off (263.1/27.9)
##             total_rec_prncp > 13013.46: Fully Paid (14.3/3.8)
## 
## -----  Trial 2:  -----
## 
## Decision tree:
## 
## recoveries > 0: Charged Off (126.9)
## recoveries <= 0:
## :...total_pymnt <= 1319.08: Charged Off (27.6/0.6)
##     total_pymnt > 1319.08:
##     :...last_fico_range_high > 639:
##         :...dti <= 3.16: Charged Off (18.7/5.2)
##         :   dti > 3.16: Fully Paid (345.2)
##         last_fico_range_high <= 639:
##         :...last_pymnt_amnt > 720.9: Fully Paid (52.4)
##             last_pymnt_amnt <= 720.9:
##             :...installment <= 188.66: Fully Paid (26.6)
##                 installment > 188.66: Charged Off (153.6/45)
## 
## -----  Trial 3:  -----
## 
## Decision tree:
## 
## recoveries > 0: Charged Off (97.5)
## recoveries <= 0:
## :...total_rec_prncp <= 3750.33: Charged Off (91.4/35.4)
##     total_rec_prncp > 3750.33:
##     :...last_fico_range_high > 629: Fully Paid (312.6)
##         last_fico_range_high <= 629:
##         :...sub_grade in {missing,A1,A3,A5,B1,B2,B4,B5,C1,C2,C3,D1,D3,D4,D5,E1,
##             :             E3,E4,E5,F1,F2,F3,F4,F5,G2,G3,G4,
##             :             G5}: Fully Paid (152.4/3.3)
##             sub_grade in {A2,A4,B3,C4,C5,D2,E2,G1}: Charged Off (97/41.9)
## 
## -----  Trial 4:  -----
## 
## Decision tree:
## 
## recoveries > 0: Charged Off (77.3)
## recoveries <= 0:
## :...last_fico_range_high > 584: Fully Paid (493.8/27)
##     last_fico_range_high <= 584:
##     :...last_pymnt_amnt <= 720.9: Charged Off (126.9/53.4)
##         last_pymnt_amnt > 720.9: Fully Paid (50)
## 
## 
## Evaluation on training data (751 cases):
## 
## Trial	    Decision Tree   
## -----	  ----------------  
## 	  Size      Errors  
## 
##    0	     6   11( 1.5%)
##    1	     5   67( 8.9%)
##    2	     7   34( 4.5%)
##    3	     5   57( 7.6%)
##    4	     4   21( 2.8%)
## boost	          6( 0.8%)   <<
## 
## 
## 	   (a)   (b)    <-classified as
## 	  ----  ----
## 	   119          (a): class Charged Off
## 	     6   626    (b): class Fully Paid
## 
## 
## 	Attribute usage:
## 
## 	100.00%	recoveries
## 	100.00%	last_fico_range_high
## 	 96.94%	total_rec_prncp
## 	 86.95%	total_pymnt
## 	 69.37%	dti
## 	 41.54%	last_pymnt_amnt
## 	 33.42%	sub_grade
## 	  7.19%	term
## 	  6.92%	installment
## 
## 
## Time: 0.0 secs
```

```r
credit_pred <- predict(m_c50_bst, sampledf_train)
confusionMatrix(data=credit_pred, sampledf_train$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off         119          6
##   Fully Paid            0        626
##                                           
##                Accuracy : 0.992           
##                  95% CI : (0.9827, 0.9971)
##     No Information Rate : 0.8415          
##     P-Value [Acc > NIR] : < 2e-16         
##                                           
##                   Kappa : 0.9706          
##                                           
##  Mcnemar's Test P-Value : 0.04123         
##                                           
##             Sensitivity : 1.0000          
##             Specificity : 0.9905          
##          Pos Pred Value : 0.9520          
##          Neg Pred Value : 1.0000          
##              Prevalence : 0.1585          
##          Detection Rate : 0.1585          
##    Detection Prevalence : 0.1664          
##       Balanced Accuracy : 0.9953          
##                                           
##        'Positive' Class : Charged Off     
## 
```

```r
## Using AdaBoost.M1
library(adabag)

# create a Adaboost.M1 model
tic()
set.seed(300)
m_adaboost <- boosting(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf_train)
toc()
```

```
## 38.337 sec elapsed
```

```r
p_adaboost <- predict(m_adaboost, sampledf_train)
head(p_adaboost$class)
```

```
## [1] "Fully Paid"  "Fully Paid"  "Charged Off" "Fully Paid"  "Fully Paid" 
## [6] "Charged Off"
```

```r
p_adaboost$confusion
```

```
##                Observed Class
## Predicted Class Charged Off Fully Paid
##     Charged Off         119          0
##     Fully Paid            0        632
```

```r
# create and evaluate an Adaboost.M1 model using 10-fold-CV
tic()
set.seed(300)
adaboost_cv <- boosting.cv(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf_train)
```

```
## i:  1 Mon Mar 16 11:20:22 2020 
## i:  2 Mon Mar 16 11:20:58 2020 
## i:  3 Mon Mar 16 11:21:36 2020 
## i:  4 Mon Mar 16 11:22:12 2020 
## i:  5 Mon Mar 16 11:22:52 2020 
## i:  6 Mon Mar 16 11:23:28 2020 
## i:  7 Mon Mar 16 11:24:05 2020 
## i:  8 Mon Mar 16 11:24:41 2020 
## i:  9 Mon Mar 16 11:25:18 2020 
## i:  10 Mon Mar 16 11:25:55 2020
```

```r
toc()
```

```
## 370.047 sec elapsed
```

```r
adaboost_cv$confusion
```

```
##                Observed Class
## Predicted Class Charged Off Fully Paid
##     Charged Off         108          0
##     Fully Paid           11        632
```

```r
# calculate kappa
library(vcd)
Kappa(adaboost_cv$confusion)
```

```
##             value     ASE    z Pr(>|z|)
## Unweighted 0.9429 0.01705 55.3        0
## Weighted   0.9429 0.01705 55.3        0
```

## Random Forests 

```r
# random forest with default settings
library(randomForest)

tic()
set.seed(300)
rf <- randomForest(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf_train)
toc()
```

```
## 0.45 sec elapsed
```

```r
rf
```

```
## 
## Call:
##  randomForest(formula = loan_status ~ loan_amnt + funded_amnt +      funded_amnt_inv + term + int_rate + installment + grade +      sub_grade + home_ownership + annual_inc + dti + delinq_2yrs +      fico_range_low + fico_range_high + inq_last_6mths + open_acc +      pub_rec + revol_bal + total_acc + out_prncp + out_prncp_inv +      total_pymnt + total_pymnt_inv + total_rec_prncp + total_rec_int +      total_rec_late_fee + recoveries + collection_recovery_fee +      last_pymnt_amnt + last_fico_range_high + last_fico_range_low +      collections_12_mths_ex_med + acc_now_delinq + chargeoff_within_12_mths +      delinq_amnt + pub_rec_bankruptcies + tax_liens + debt_settlement_flag,      data = sampledf_train) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 6
## 
##         OOB estimate of  error rate: 2.13%
## Confusion matrix:
##             Charged Off Fully Paid class.error
## Charged Off         103         16   0.1344538
## Fully Paid            0        632   0.0000000
```

```r
credit_pred <- predict(rf, sampledf_train)
confusionMatrix(data=credit_pred, sampledf_train$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off         119          0
##   Fully Paid            0        632
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9951, 1)
##     No Information Rate : 0.8415     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.0000     
##             Specificity : 1.0000     
##          Pos Pred Value : 1.0000     
##          Neg Pred Value : 1.0000     
##              Prevalence : 0.1585     
##          Detection Rate : 0.1585     
##    Detection Prevalence : 0.1585     
##       Balanced Accuracy : 1.0000     
##                                      
##        'Positive' Class : Charged Off
## 
```

```r
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

tic()
set.seed(300)
m_rf <- train(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf_train, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
toc()
```

```
## 348.207 sec elapsed
```

```r
m_rf
```

```
## Random Forest 
## 
## 751 samples
##  38 predictor
##   2 classes: 'Charged Off', 'Fully Paid' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 676, 676, 675, 677, 675, 676, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9306594  0.6748557
##    4    0.9733994  0.8896317
##    8    0.9756661  0.8996204
##   16    0.9804523  0.9202352
## 
## Kappa was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 16.
```

```r
credit_pred <- predict(m_rf, sampledf)
confusionMatrix(data=credit_pred, sampledf$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off         153          0
##   Fully Paid            5        842
##                                           
##                Accuracy : 0.995           
##                  95% CI : (0.9884, 0.9984)
##     No Information Rate : 0.842           
##     P-Value [Acc > NIR] : < 2e-16         
##                                           
##                   Kappa : 0.981           
##                                           
##  Mcnemar's Test P-Value : 0.07364         
##                                           
##             Sensitivity : 0.9684          
##             Specificity : 1.0000          
##          Pos Pred Value : 1.0000          
##          Neg Pred Value : 0.9941          
##              Prevalence : 0.1580          
##          Detection Rate : 0.1530          
##    Detection Prevalence : 0.1530          
##       Balanced Accuracy : 0.9842          
##                                           
##        'Positive' Class : Charged Off     
## 
```



```r
# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20),
                        .winnow = "FALSE")

tic()
set.seed(300)
m_c50 <- train(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf, method = "C5.0",
                metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
```

```
## Warning: 'trials' should be <= 9 for this object. Predictions generated
## using 9 trials
```

```
## Warning in Ops.factor(x$winnow): '!' not meaningful for factors
```

```r
toc()
```

```
## 37.568 sec elapsed
```

```r
m_c50
```

```
## C5.0 
## 
## 1000 samples
##   38 predictor
##    2 classes: 'Charged Off', 'Fully Paid' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 10 times) 
## Summary of sample sizes: 900, 899, 900, 901, 900, 900, ... 
## Resampling results across tuning parameters:
## 
##   trials  Accuracy   Kappa    
##   10      0.9855995  0.9431174
##   20      0.9875006  0.9505132
## 
## Tuning parameter 'model' was held constant at a value of tree
## 
## Tuning parameter 'winnow' was held constant at a value of FALSE
## Kappa was used to select the optimal model using the largest value.
## The final values used for the model were trials = 20, model = tree
##  and winnow = FALSE.
```

```r
credit_pred <- predict(m_c50, sampledf)
confusionMatrix(data=credit_pred, sampledf$loan_status)
```

```
## Confusion Matrix and Statistics
## 
##              Reference
## Prediction    Charged Off Fully Paid
##   Charged Off         158          0
##   Fully Paid            0        842
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9963, 1)
##     No Information Rate : 0.842      
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
##                                      
##             Sensitivity : 1.000      
##             Specificity : 1.000      
##          Pos Pred Value : 1.000      
##          Neg Pred Value : 1.000      
##              Prevalence : 0.158      
##          Detection Rate : 0.158      
##    Detection Prevalence : 0.158      
##       Balanced Accuracy : 1.000      
##                                      
##        'Positive' Class : Charged Off
## 
```

```r
## Random Forests ----
library(ranger)

tic()
set.seed(300)
m_rf_ranger <- ranger(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf, num.threads = 8)
toc()
```

```
## 0.072 sec elapsed
```

```r
m_rf_ranger
```

```
## Ranger result
## 
## Call:
##  ranger(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv +      term + int_rate + installment + grade + sub_grade + home_ownership +      annual_inc + dti + delinq_2yrs + fico_range_low + fico_range_high +      inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc +      out_prncp + out_prncp_inv + total_pymnt + total_pymnt_inv +      total_rec_prncp + total_rec_int + total_rec_late_fee + recoveries +      collection_recovery_fee + last_pymnt_amnt + last_fico_range_high +      last_fico_range_low + collections_12_mths_ex_med + acc_now_delinq +      chargeoff_within_12_mths + delinq_amnt + pub_rec_bankruptcies +      tax_liens + debt_settlement_flag, data = sampledf, num.threads = 8) 
## 
## Type:                             Classification 
## Number of trees:                  500 
## Sample size:                      1000 
## Number of independent variables:  38 
## Mtry:                             6 
## Target node size:                 1 
## Variable importance mode:         none 
## Splitrule:                        gini 
## OOB prediction error:             1.80 %
```

```r
m_rf_ranger$confusion.matrix
```

```
##              predicted
## true          Charged Off Fully Paid
##   Charged Off         140         18
##   Fully Paid            0        842
```




#logistic Regression

```r
lmodel <- glm(loan_status  ~ int_rate+ grade + loan_amnt + annual_inc,
                     family = "binomial", data = sampledf_train)
summary(lmodel)
```

```
## 
## Call:
## glm(formula = loan_status ~ int_rate + grade + loan_amnt + annual_inc, 
##     family = "binomial", data = sampledf_train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.6084   0.3208   0.4585   0.6443   1.0704  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)   
## (Intercept)  1.662e+00  7.849e-01   2.118  0.03417 * 
## int_rate     9.654e-02  8.505e-02   1.135  0.25633   
## gradeB      -7.737e-01  5.677e-01  -1.363  0.17295   
## gradeC      -1.710e+00  7.412e-01  -2.307  0.02103 * 
## gradeD      -2.545e+00  9.764e-01  -2.606  0.00916 **
## gradeE      -2.733e+00  1.227e+00  -2.227  0.02592 * 
## gradeF      -3.235e+00  1.538e+00  -2.103  0.03545 * 
## gradeG      -3.133e+00  1.786e+00  -1.754  0.07944 . 
## loan_amnt   -4.518e-05  1.534e-05  -2.946  0.00321 **
## annual_inc   1.347e-05  4.156e-06   3.241  0.00119 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 656.52  on 750  degrees of freedom
## Residual deviance: 602.55  on 741  degrees of freedom
## AIC: 622.55
## 
## Number of Fisher Scoring iterations: 5
```

```r
predictions_s <- predict(lmodel, newdata = sampledf_train,
                              type = "response")                

range(predictions_s) 
```

```
## [1] 0.5639182 0.9996092
```

```r
pred_cutoff_20 <- ifelse(predictions_s > 0.55,1,0)

conf_20 <- table(sampledf_train$loan_status,pred_cutoff_20)
conf_20
```

```
##              pred_cutoff_20
##                 1
##   Charged Off 119
##   Fully Paid  632
```

```r
accuracy_20 <- sum(diag(conf_20))/sum(conf_20)
accuracy_20
```

```
## [1] 0.1584554
```

```r
library(ROCR)
pred_train_small <- predict(lmodel,type = "response")

predROC_small <- prediction(pred_train_small, sampledf_train$loan_status)

perfROC_small <- performance(predROC_small,"tpr","fpr")

plot(perfROC_small, colorize = TRUE,
     print.cutoffs.at = seq(0,1,0.1),text.adj = c(-0.2,1.7))
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-29-1.png)<!-- -->

```r
AUC_small <- as.numeric(performance(predROC_small , "auc")@y.values)
AUC_small
```

```
## [1] 0.7045128
```

```r
library(Boruta)
 set.seed(1)
Boruta.loan <- Boruta(loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + int_rate + 
    installment + grade + sub_grade + home_ownership + 
    annual_inc +  
     dti + delinq_2yrs +  
    fico_range_low + fico_range_high + inq_last_6mths + open_acc + 
    pub_rec + revol_bal + total_acc +  out_prncp + 
    out_prncp_inv + total_pymnt + total_pymnt_inv + total_rec_prncp + 
    total_rec_int + total_rec_late_fee + recoveries + collection_recovery_fee + 
    last_pymnt_amnt + 
    last_fico_range_high + last_fico_range_low + collections_12_mths_ex_med + 
    acc_now_delinq + chargeoff_within_12_mths + 
    delinq_amnt + pub_rec_bankruptcies + tax_liens  + 
    debt_settlement_flag, data = sampledf, doTrace = 2, ntree = 500)
```

```
##  1. run of importance source...
```

```
##  2. run of importance source...
```

```
##  3. run of importance source...
```

```
##  4. run of importance source...
```

```
##  5. run of importance source...
```

```
##  6. run of importance source...
```

```
##  7. run of importance source...
```

```
##  8. run of importance source...
```

```
##  9. run of importance source...
```

```
##  10. run of importance source...
```

```
##  11. run of importance source...
```

```
##  12. run of importance source...
```

```
## After 12 iterations, +3.8 secs:
```

```
##  confirmed 16 attributes: collection_recovery_fee, debt_settlement_flag, funded_amnt, funded_amnt_inv, installment and 11 more;
```

```
##  rejected 11 attributes: acc_now_delinq, chargeoff_within_12_mths, collections_12_mths_ex_med, delinq_amnt, home_ownership and 6 more;
```

```
##  still have 11 attributes left.
```

```
##  13. run of importance source...
```

```
##  14. run of importance source...
```

```
##  15. run of importance source...
```

```
##  16. run of importance source...
```

```
## After 16 iterations, +4.8 secs:
```

```
##  confirmed 2 attributes: int_rate, total_rec_late_fee;
```

```
##  rejected 3 attributes: delinq_2yrs, dti, inq_last_6mths;
```

```
##  still have 6 attributes left.
```

```
##  17. run of importance source...
```

```
##  18. run of importance source...
```

```
##  19. run of importance source...
```

```
##  20. run of importance source...
```

```
##  21. run of importance source...
```

```
##  22. run of importance source...
```

```
##  23. run of importance source...
```

```
##  24. run of importance source...
```

```
##  25. run of importance source...
```

```
##  26. run of importance source...
```

```
##  27. run of importance source...
```

```
## After 27 iterations, +7.1 secs:
```

```
##  confirmed 1 attribute: grade;
```

```
##  rejected 1 attribute: total_acc;
```

```
##  still have 4 attributes left.
```

```
##  28. run of importance source...
```

```
##  29. run of importance source...
```

```
##  30. run of importance source...
```

```
##  31. run of importance source...
```

```
##  32. run of importance source...
```

```
##  33. run of importance source...
```

```
##  34. run of importance source...
```

```
##  35. run of importance source...
```

```
##  36. run of importance source...
```

```
##  37. run of importance source...
```

```
##  38. run of importance source...
```

```
##  39. run of importance source...
```

```
##  40. run of importance source...
```

```
##  41. run of importance source...
```

```
##  42. run of importance source...
```

```
##  43. run of importance source...
```

```
##  44. run of importance source...
```

```
##  45. run of importance source...
```

```
##  46. run of importance source...
```

```
##  47. run of importance source...
```

```
##  48. run of importance source...
```

```
##  49. run of importance source...
```

```
##  50. run of importance source...
```

```
##  51. run of importance source...
```

```
##  52. run of importance source...
```

```
##  53. run of importance source...
```

```
##  54. run of importance source...
```

```
##  55. run of importance source...
```

```
##  56. run of importance source...
```

```
##  57. run of importance source...
```

```
##  58. run of importance source...
```

```
##  59. run of importance source...
```

```
##  60. run of importance source...
```

```
##  61. run of importance source...
```

```
##  62. run of importance source...
```

```
## After 62 iterations, +15 secs:
```

```
##  confirmed 1 attribute: fico_range_low;
```

```
##  still have 3 attributes left.
```

```
##  63. run of importance source...
```

```
##  64. run of importance source...
```

```
##  65. run of importance source...
```

```
##  66. run of importance source...
```

```
##  67. run of importance source...
```

```
## After 67 iterations, +16 secs:
```

```
##  confirmed 1 attribute: annual_inc;
```

```
##  still have 2 attributes left.
```

```
##  68. run of importance source...
```

```
##  69. run of importance source...
```

```
##  70. run of importance source...
```

```
##  71. run of importance source...
```

```
##  72. run of importance source...
```

```
##  73. run of importance source...
```

```
##  74. run of importance source...
```

```
##  75. run of importance source...
```

```
##  76. run of importance source...
```

```
##  77. run of importance source...
```

```
##  78. run of importance source...
```

```
##  79. run of importance source...
```

```
##  80. run of importance source...
```

```
##  81. run of importance source...
```

```
##  82. run of importance source...
```

```
##  83. run of importance source...
```

```
## After 83 iterations, +19 secs:
```

```
##  confirmed 1 attribute: revol_bal;
```

```
##  still have 1 attribute left.
```

```
##  84. run of importance source...
```

```
##  85. run of importance source...
```

```
##  86. run of importance source...
```

```
##  87. run of importance source...
```

```
##  88. run of importance source...
```

```
##  89. run of importance source...
```

```
##  90. run of importance source...
```

```
##  91. run of importance source...
```

```
##  92. run of importance source...
```

```
##  93. run of importance source...
```

```
##  94. run of importance source...
```

```
##  95. run of importance source...
```

```
##  96. run of importance source...
```

```
##  97. run of importance source...
```

```
## After 97 iterations, +23 secs:
```

```
##  confirmed 1 attribute: fico_range_high;
```

```
##  no more attributes left.
```

```r
 set.seed(1)
Boruta.Short <- Boruta(loan_status ~ ., data = sampledf_test, maxRuns = 12)
TentativeRoughFix(Boruta.Short)
```

```
## Boruta performed 11 iterations in 1.314296 secs.
## Tentatives roughfixed over the last 11 iterations.
##  58 attributes confirmed important: acc_now_delinq, addr_state,
## annual_inc, chargeoff_within_12_mths, collection_recovery_fee and
## 53 more;
##  3 attributes confirmed unimportant: last_credit_pull_d,
## last_pymnt_amnt, recoveries;
```

```r
getConfirmedFormula(Boruta.loan)
```

```
## loan_status ~ loan_amnt + funded_amnt + funded_amnt_inv + term + 
##     int_rate + installment + grade + sub_grade + annual_inc + 
##     fico_range_low + fico_range_high + revol_bal + total_pymnt + 
##     total_pymnt_inv + total_rec_prncp + total_rec_int + total_rec_late_fee + 
##     recoveries + collection_recovery_fee + last_pymnt_amnt + 
##     last_fico_range_high + last_fico_range_low + debt_settlement_flag
## <environment: 0x7fd54cd2a010>
```

```r
attStats(Boruta.loan)
```

```
##                                meanImp   medianImp     minImp     maxImp
## loan_amnt                   9.28100350  9.31585529  7.9824794 10.4968385
## funded_amnt                 9.22310863  9.20634210  7.8374669 10.2503526
## funded_amnt_inv             9.41800447  9.45924048  8.1527873 10.8638717
## term                        5.32430095  5.31515495  3.8805731  6.8224943
## int_rate                    4.03428885  4.02523606  1.9904432  5.9933305
## installment                 9.54784522  9.49833025  8.4207393 11.4774596
## grade                       2.83266479  2.82196958  1.0234192  4.5898233
## sub_grade                   3.78065195  3.81558714  0.9502242  5.9724139
## home_ownership             -0.05853374 -0.04659159 -2.2660738  0.9408724
## annual_inc                  2.90051882  2.94645847  0.2475566  4.7987969
## dti                         0.87187431  1.13180172 -0.7001173  2.0046863
## delinq_2yrs                 0.17244002  0.14487600 -1.3010831  2.0984745
## fico_range_low              2.73322593  2.73701870  1.0021882  4.2781356
## fico_range_high             2.67795269  2.55974079  0.4873397  5.1123806
## inq_last_6mths             -0.26836659 -0.16294449 -2.5744495  1.3860723
## open_acc                    0.20149633  0.07686808 -1.6418629  1.6318750
## pub_rec                    -0.47413688 -0.30136897 -1.7123045  0.7884895
## revol_bal                   2.79191537  2.80732990  0.7009434  5.0351557
## total_acc                   0.94884815  0.79161582 -1.5752063  2.7143855
## out_prncp                   0.00000000  0.00000000  0.0000000  0.0000000
## out_prncp_inv               0.00000000  0.00000000  0.0000000  0.0000000
## total_pymnt                10.03016362 10.02911641  8.6350150 11.1530475
## total_pymnt_inv            10.03794489  9.95759904  8.6761704 11.6607786
## total_rec_prncp            15.73515573 15.72528249 13.8937090 17.5651497
## total_rec_int               7.69515149  7.69385765  6.5724230  9.3917173
## total_rec_late_fee          3.76790298  3.75136416  1.8407408  5.4795168
## recoveries                 26.83726098 26.92717060 23.1610490 28.9889102
## collection_recovery_fee    19.75900744 19.78335058 18.1808361 21.2761162
## last_pymnt_amnt            14.38338659 14.36378779 12.7393685 15.7598815
## last_fico_range_high       15.04217421 15.00517714 13.0362467 16.9648696
## last_fico_range_low        15.02551111 15.00459375 13.5882086 16.4574158
## collections_12_mths_ex_med -0.08341679  0.00000000 -1.0010015  1.0010015
## acc_now_delinq              0.00000000  0.00000000  0.0000000  0.0000000
## chargeoff_within_12_mths   -0.28161284  0.00000000 -1.3488827  0.0000000
## delinq_amnt                 0.00000000  0.00000000  0.0000000  0.0000000
## pub_rec_bankruptcies       -0.41619657 -0.36782448 -1.9186928  1.2046941
## tax_liens                   0.45193618  0.56888634 -1.4476344  1.6622696
## debt_settlement_flag       11.96402675 11.95368777 10.2046906 13.3303804
##                              normHits  decision
## loan_amnt                  1.00000000 Confirmed
## funded_amnt                1.00000000 Confirmed
## funded_amnt_inv            1.00000000 Confirmed
## term                       0.98969072 Confirmed
## int_rate                   0.94845361 Confirmed
## installment                1.00000000 Confirmed
## grade                      0.76288660 Confirmed
## sub_grade                  0.95876289 Confirmed
## home_ownership             0.00000000  Rejected
## annual_inc                 0.75257732 Confirmed
## dti                        0.01030928  Rejected
## delinq_2yrs                0.01030928  Rejected
## fico_range_low             0.73195876 Confirmed
## fico_range_high            0.68041237 Confirmed
## inq_last_6mths             0.01030928  Rejected
## open_acc                   0.00000000  Rejected
## pub_rec                    0.00000000  Rejected
## revol_bal                  0.71134021 Confirmed
## total_acc                  0.04123711  Rejected
## out_prncp                  0.00000000  Rejected
## out_prncp_inv              0.00000000  Rejected
## total_pymnt                1.00000000 Confirmed
## total_pymnt_inv            1.00000000 Confirmed
## total_rec_prncp            1.00000000 Confirmed
## total_rec_int              1.00000000 Confirmed
## total_rec_late_fee         0.93814433 Confirmed
## recoveries                 1.00000000 Confirmed
## collection_recovery_fee    1.00000000 Confirmed
## last_pymnt_amnt            1.00000000 Confirmed
## last_fico_range_high       1.00000000 Confirmed
## last_fico_range_low        1.00000000 Confirmed
## collections_12_mths_ex_med 0.00000000  Rejected
## acc_now_delinq             0.00000000  Rejected
## chargeoff_within_12_mths   0.00000000  Rejected
## delinq_amnt                0.00000000  Rejected
## pub_rec_bankruptcies       0.00000000  Rejected
## tax_liens                  0.00000000  Rejected
## debt_settlement_flag       1.00000000 Confirmed
```

```r
plot(Boruta.loan,las = 2, cex.axis = 0.7)
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

```r
plotImpHistory(Boruta.loan)
```

![](Shah_Maitry_stat652_project_files/figure-html/unnamed-chunk-31-2.png)<!-- -->
\newpage
Conclusion

```r
tbl<-matrix(c("97.87%","0.92","0.94","92.9%","0.688","0.8898","97.6%","0.9055","0.9614","99.7%","0.988","","98.5%","0.94","",
              "83.75%","0.694","","97.4%","0.897",""),ncol = 3,byrow = TRUE)
colnames(tbl)<-c("Accuracy","Kappa","AUC")
rownames(tbl)<-c("Regression Tree","KNN","CART","Random Forest","C5.0","Logistic Regression","SVM")
tbl<-as.table(tbl)
tbl
```

```
##                     Accuracy Kappa  AUC   
## Regression Tree     97.87%   0.92   0.94  
## KNN                 92.9%    0.688  0.8898
## CART                97.6%    0.9055 0.9614
## Random Forest       99.7%    0.988        
## C5.0                98.5%    0.94         
## Logistic Regression 83.75%   0.694        
## SVM                 97.4%    0.897
```

```r
#Summary Table
```



