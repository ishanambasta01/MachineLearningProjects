#Gramener Case Study by:
#Arif
#Pragya
#Ishan Ambasta
#Ashay
#
#
# Set working directory

if  (!("ggplot2" %in% rownames(installed.packages()))){
  install.packages("ggplot2")
}
if (!("dplyr" %in% rownames(installed.packages()))){
  install.packages("dplyr")
}
if (!("tidyr" %in% rownames(installed.packages()))){
  install.packages("tidyr")
}
if (!("stringr" %in% rownames(installed.packages()))){
  install.packages("stringr")
}
if (!("lubridate" %in% rownames(installed.packages()))){
  install.packages("lubridate")
}
if (!("chron" %in% rownames(installed.packages()))){
  install.packages("chron")
}
if (!("gridExtra" %in% rownames(installed.packages()))){
  install.packages("gridExtra")
}


if (!("reshape2" %in% rownames(installed.packages()))){
  install.packages("reshape2")
}


library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(chron)
library(gridExtra)
library(reshape2)


color.theme = c("#e3a711", "#93b856", "#c56055","#55a0d3")
theme_title <- theme(plot.title = element_text(size = 20, face = "bold"))
bar_chart_text <- geom_text(stat='Identity', aes(label=value),position=position_dodge(width=0.9),vjust=-0.25,hjust=0.25)
percentile <- seq(0.1,1,0.01)

#------------------------------------------------ Business Goal ------------------------------------------------
#-- Analyse the data to find loan applicant's behaviour.
#-- Find key driver attributes or variables which will be used to predict whether loan applicant will pay the loan or
#-- will become defaulter. Analysis should be carefully so we would not wrongly identify good customer as defaulter. 
#-- This will cause companay as business loss. 


#------------------------------------------------ Data understading -----------------------------------------
#-- Data Source Type: csv file.
#-- Description:  This file contains loan information.
#-- It has key attributes about loan like 
#-- 1. Applicant details like Annual Income, Home ownership, Employment Lenght, Employment Title, geographics.
#-- 2. Loan Details e.g Loan amount, Loan Purpose, Funded amount, interest rate, duration, grade, sub grade, Amount Remaining, Loan Status etc.
#-- 3. Loan Application details like Issued date, Application Type, etc.
#-- 4. Other Details about loan payment and collection. E.g. Payment Received, Collection Fees, Next payment date and etc. 
#-- 5. Other columns are there but mostly having missing values. We will take care of this while preparing data. 
#-- 6. For this analysis we assume that loan amount is in $.
#-- Some Insights
#-- 1. Total 39717 loan details are available
#-- 2. There are three status for loan
#--    Charged Off  : 5627  Rows
#--    Current      : 1140  Rows
#--    Fully Paid   : 32950 Rows
#-- 3. Only 14% of all loans, are with "Charged off" status.
#-- 5. Only two options are available for loan tern. "36 Months" and "60 Months". Lender are mostly interested in short term 
#--    loans. Almost 73% loans are given for 36 month term.
#-- 6. Based on three plot. We can say Average amount offered in 36 month loan in around $7.7k approx and 
#--    this amount is increased in 60 months duration. And average amount offered is $14k approx.
#-- 7. Based on comparison of loan status in two terms. Percentage of loan charged of is high, if loan is lent for long term
#-- 8. pub_rec_bankruptcies is not having significance on loan status. 
#-- 8. As per summary of data and some other initial univariate analysis. We have removed column with no significance.
#--    Other column with NA values and 0 values.  
#--       acc_now_delinq	,acc_open_past_24mths	,all_util	,annual_inc_joint	,application_type	,avg_cur_bal	,bc_open_to_buy	
#--       ,bc_util	,chargeoff_within_12_mths	,collection_recovery_fee	,collections_12_mths_ex_med	,delinq_amnt	
#--       ,desc	,dti_joint	,earliest_cr_line	,emp_title	,fico_range_high	,fico_range_low	,id	,il_util	,initial_list_status	
#--       ,inq_fi	,inq_last_12m	,last_fico_range_high	,last_fico_range_low	,max_bal_bc	,mo_sin_old_il_acct	,mo_sin_old_rev_tl_op	
#--       ,mo_sin_rcnt_rev_tl_op	,mo_sin_rcnt_tl	,mort_acc	,mths_since_last_delinq	,mths_since_last_major_derog	
#--       ,mths_since_last_record	,mths_since_rcnt_il	,mths_since_recent_bc	,mths_since_recent_bc_dlq	,mths_since_recent_inq	
#--       ,mths_since_recent_revol_delinq	,next_pymnt_d	,num_accts_ever_120_pd	,num_actv_bc_tl	,num_actv_rev_tl	,num_bc_sats	
#--       ,num_bc_tl	,num_il_tl	,num_op_rev_tl	,num_rev_accts	,num_rev_tl_bal_gt_0	,num_sats	,num_tl_120dpd_2m	,num_tl_30dpd	
#--       ,num_tl_90g_dpd_24m	,num_tl_op_past_12m	,open_acc_6m	,open_il_12m	,open_il_24m	,open_il_6m	,open_rv_12m	,open_rv_24m	
#--       ,pct_tl_nvr_dlq	,percent_bc_gt_75	,policy_code	,pymnt_plan	,recoveries	,tax_liens	,title	,tot_coll_amt	,tot_cur_bal	
#--       ,tot_hi_cred_lim	,total_bal_ex_mort	,total_bal_il	,total_bc_limit	,total_cu_tl	,total_il_high_credit_limit	
#--       ,total_rev_hi_lim,url	,verified_status_joint	,zip_code


loan <- read.csv(file = 'loan.csv',sep = ',', header = T, stringsAsFactors=F)
View(loan)
dim(loan)
summary(loan)
summary(factor(loan$term))

bx_plt_1 <- ggplot(loan,aes(x=factor(term),y=loan$loan_amnt))+geom_boxplot()
bx_plt_2 <- ggplot(loan,aes(x=factor(term),y=loan$funded_amnt))+geom_boxplot()
bx_plt_3 <- ggplot(loan,aes(x=factor(term),y=loan$funded_amnt_inv))+geom_boxplot()
grid.arrange(bx_plt_1,bx_plt_2,bx_plt_3,nrow=3,ncol=1)


bar_plot_2 <- ggplot(loan,aes(x = factor(term)))+geom_bar(stat = 'count',aes(fill=factor(loan$loan_status)))
bar_plot_3 <- ggplot(loan,aes(x = factor(term)))+geom_bar(stat = 'count',aes(fill=factor(loan$loan_status)),position = "fill")
bar_plot_3 <- bar_plot_3 + scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))
bar_plot_3

bar_plot_4 <- ggplot(loan,aes(x = factor(loan$pub_rec_bankruptcies)))+geom_bar(stat = 'count',aes(fill=factor(loan$loan_status)),position = "fill")
bar_plot_4 <- bar_plot_4 + scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))
bar_plot_4

bar_plot_5 <- ggplot(loan,aes(x = factor(loan_status)))+geom_bar(stat = 'count',aes(fill=factor(pub_rec_bankruptcies)),position = "fill")
bar_plot_5 <- bar_plot_5 + scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))
bar_plot_5

summary(factor(loan$loan_status))
summary(factor(loan$next_pymnt_d))
summary(factor(loan$last_credit_pull_d))
summary(factor(loan$application_type))
summary(factor(loan$last_pymnt_d))
summary(factor(loan$zip_code))
summary(factor(loan$pymnt_plan))
loan$emp_title
factor(loan$title)
summary(loan$collection_recovery_fee)
#-- collection_recovery_fee is not significant.Because it does not have any data for none "Charged Off" loan. 
ggplot(loan, aes(x=loan_status,y=collection_recovery_fee))+geom_boxplot()
#-- recoveries is not significant.Because it does not have any data for none "Charged Off" loan. 
ggplot(loan, aes(x=loan_status,y=recoveries))+geom_boxplot()
View(loan)



#------------------------------------------------ Data Cleaning and Data Preparing -----------------------------------------

#-- Per per data understanding we found lot's of columns are having NA values and some them having either 0 or blank
#-- Remove those columns. From 111 columns, we have only considered 36 for Analysis.
loan_clean <- loan[, -which(names(loan) %in% c("mths_since_last_delinq","mths_since_last_record","collections_12_mths_ex_med","mths_since_last_major_derog","policy_code","annual_inc_joint","dti_joint","verification_status_joint","acc_now_delinq","tot_coll_amt","tot_cur_bal","open_acc_6m","open_il_6m","open_il_6m","open_il_12m","open_il_24m","mths_since_rcnt_il","total_bal_il","il_util","open_rv_12m","open_rv_24m","max_bal_bc"	,"all_util"	,"total_rev_hi_lim"	,"inq_fi"	,"total_cu_tl"	,"inq_last_12m"	,"acc_open_past_24mths"	,"avg_cur_bal"	,"bc_open_to_buy"	,"bc_util"	,"chargeoff_within_12_mths"	,"delinq_amnt"	,"mo_sin_old_il_acct"	,"mo_sin_old_rev_tl_op"	,"mo_sin_rcnt_rev_tl_op"	,"mo_sin_rcnt_tl"	,"mort_acc"	,"mths_since_recent_bc"	,"mths_since_recent_bc_dlq"	,"mths_since_recent_inq"	,"mths_since_recent_revol_delinq"	,"num_accts_ever_120_pd"	,"num_actv_bc_tl"	,"num_actv_rev_tl"	,"num_bc_sats"	,"num_bc_tl"	,"num_il_tl"	,"num_op_rev_tl"	,"num_rev_accts"	,"num_rev_tl_bal_gt_0"	,"num_sats"	,"num_tl_120dpd_2m"	,"num_tl_30dpd"	,"num_tl_90g_dpd_24m"	,"num_tl_op_past_12m"	,"pct_tl_nvr_dlq"	,"percent_bc_gt_75"	,"tax_liens"	,"tot_hi_cred_lim"	,"total_bal_ex_mort"	,"total_bc_limit"	,"total_il_high_credit_limit","next_pymnt_d","application_type","initial_list_status","desc","earliest_cr_line","id","zip_code","url","emp_title","title","collection_recovery_fee","pymnt_plan","recoveries"))]
dim(loan_clean)
summary(loan_clean)

#------ NA value treatment.
#-- There are 697 cells with NA Values after removing columns. Based on Summary we can say.
#-- NA value is in pub_rec_bankruptcies columns.
#-- No need to convert NA values. 
sum(is.na(loan_clean))
summary(loan_clean)

View(loan_clean)

#-- Check for Duplicate record.
#-- No Duplicate Record found.
which(duplicated(loan_clean$member_id))

#------ Data Preparation 
#-- Convert Percentage to Numeric
loan_clean$int_rate <- as.numeric(str_replace(loan_clean$int_rate,"%",""))
loan_clean$revol_util <- as.numeric(str_replace(loan_clean$revol_util,"%",""))


#-- Convert dates into correct date format.
loan_clean$issue_d <- (parse_date_time(loan_clean$issue_d,"b-y"))
loan_clean$last_credit_pull_d <- (parse_date_time(loan_clean$last_credit_pull_d,"b-y"))
loan_clean$last_pymnt_d <- (parse_date_time(loan_clean$last_pymnt_d,"b-y"))

#-- Convert appropriate columns to factor.
loan_clean$term <-  factor(str_trim(loan_clean$term))
loan_clean$grade <- factor(loan_clean$grade)
loan_clean$purpose <- factor(loan_clean$purpose)
loan_clean$addr_state <- factor(loan_clean$addr_state)
loan_clean$grade <- (factor(loan_clean$grade))
loan_clean$sub_grade <- (factor(loan_clean$sub_grade))
loan_clean$emp_length <- (factor(loan_clean$emp_length))
loan_clean$home_ownership <- (factor(loan_clean$home_ownership))
loan_clean$verification_status<-(factor(loan_clean$verification_status))
loan_clean$loan_status <- (factor(loan_clean$loan_status))


#-- Check for the Identical Columns
#-- No identical column found 
sum(loan_clean$total_pymnt)-sum(loan_clean$total_pymnt_inv)
sum(loan_clean$total_pymnt)-(sum(loan_clean$total_rec_int)+sum(loan_clean$total_rec_prncp))
loan_clean[loan_clean$total_pymnt-(loan_clean$total_rec_prncp+loan_clean$total_rec_int)==0,]

#-- Remove non-useful data from the data set. 
#-- Since we are interested in "Charged Off" and "Fully Paid" data. 
#-- Data will "Current" as Loan status are excluded from the dataset. 
loan_clean <- loan_clean[!loan_clean$loan_status %in% c("Current"),]

View(loan_clean)
summary(loan_clean)

#--------------------------------------------------------- EDA ---------------------------------------------------------
#------ Univariate Analysis

#-- 1.  "loan_amnt"
#-- Based on Graphs there is no significant Loan amount on the status of loan.
#-- We can conclude same for funded_amnt, funded_amnt_inv as well. Since they are dependent on loan_amnt
p1 <- ggplot(loan_clean,aes(x=loan_status,y=loan_amnt))+geom_boxplot()
p1 <- p1 + labs(x = "Loan Status", y = "Loan Amount",title="Loam Amount Distribution by Loan Status")+theme_title

p2 <- ggplot(loan_clean[loan_clean$loan_status=="Charged Off",],aes(x=loan_amnt))+geom_histogram(binwidth =  3000)
p2 <- p2 + labs(x = "Loan Amount", y = "Number of Loan(s)",title="Loan Amount Distribution for Charged Off")+theme_title

p3 <- ggplot(loan_clean[loan_clean$loan_status=="Fully Paid",],aes(x=loan_amnt))+geom_histogram(stat='bin', binwidth = 3000)
p3 <- p3 + labs(x = "Loan Amount", y = "Number of Loan(s)",title="Loan Amount Distribution for Fully Paid")+theme_title

  
grid.arrange(p1,p2,p3,nrow=3,ncol=1)

#-- 2.  "term"
#-- Based on Portion of Loan status for different term. 
#-- In Graph p2, we can see for 60 months of term % of charged off is higher than 36 month term
#-- But as per Graph p3, when we further dig into Charged Off loan status.
#-- Almost 60% of loan are charged off when lent for 36 months term

p1 <- ggplot(loan_clean,aes(term))+geom_bar(stat = 'count')
p1 <- p1 + labs(x = "Loan Term", y = "Number of Loan(s)",title="Number of loans By Loan Term")+theme_title

p2 <- ggplot(loan_clean,aes(term,fill=loan_status))
p2 <- p2+geom_bar(stat = 'count',position ="fill") +scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))    #position_dodge(width=0.8))
p2 <- p2 + labs(x = "Loan Term", y = "Number of Loan(s)",title="Number of loans by Loan Term and Loan Status")+theme_title

p3 <- ggplot(loan_clean,aes(loan_status,fill=term))
p3 <- p3+geom_bar(stat = 'count',position ="fill") +scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))    #position_dodge(width=0.8))
p3 <- p3 + labs(x = "Loan Term", y = "Number of Loan(s)",title="Number of loans by Loan Status and Loan Term")+theme_title


grid.arrange(p1,p2,p3,nrow=3,ncol=1)

#-- 3.  int_rate
#-- Add Segment for Interest rate
#-- 0-5 ->  Less Than 5
#-- 6-10 -> Between 6 to 10
#-- 11-15 -> Between 11 to 15
#-- 16-20 -> Between 16 to 20
#-- 21-25 -> Between 21 to 25
#-- >25 -> Greater than 25


int_seg <- function (x){
  segment <- ""
  if ( x >=0.00 && x <5.00){
    
    segment= "less Than 5"
  }else if ( x >=5.00 && x <=10.00){
    segment= "Between 5 to 10"
  }else if ( x >10.00 && x <=15.00){
    segment= "Between 11 to 15"
  }else if ( x >15.00 && x <=20.00){
    segment= "Between 16 to 20"
  }else if ( x >20.00 && x <=25.00){
    segment= "Between 21 to 25"
  }else if ( x >25.00 ){
    segment= "Greater Than 25"
  }
  return (segment)
}

loan_clean$int_segment <- sapply(loan_clean$int_rate, FUN =  int_seg)
summary(loan_clean$int_rate)

quantile(loan_clean$int_rate,percentile)
p1 <- ggplot(loan_clean,aes(x=loan_status,y=int_rate))+geom_boxplot()
p1 

p2 <- ggplot (loan_clean[loan_clean$loan_status=="Charged Off",], aes(x=int_rate)) + geom_histogram(binwidth = 1)
p2 <- p2+labs(x = "Int. Rate", y = "Number of Loan(s)",title="Distribution Of Loan by Int. Rate for Charged Off ")+theme_title
p3 <- ggplot (loan_clean[loan_clean$loan_status=="Fully Paid",], aes(x=int_rate)) + geom_histogram(binwidth = 1)
p3 <- p3 + labs(x = "Int. Rate", y = "Number of Loan(s)",title="Distribution Of Loan by Int. Rate for Fully Paid")+theme_title

grid.arrange(p1,p2,p3,nrow=3,ncol=1)

#-- Based on Histogram. It seems there is some significance of interest rate on the status of loan. 
#-- To understand it better, below is some analysis. 
#-- Showing % of loan Charged off with change in interest rate.
#-- Based on the Bar graph and by.int.segment data frame. 
#-- 80% of loans are charged off when lent between 11 to 20% of interest rate. 

byintsegment <- group_by(loan_clean,loan_status,int_segment)
by.int.segment <- as.data.frame(summarise(byintsegment,loans = n()))
ggplot(by.int.segment,aes(x=loan_status,y=loans,fill=int_segment))+geom_bar(stat='Identity',position='fill')+scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))

perc <- function(x){
  total_loan <- sum(by.int.segment[by.int.segment$loan_status==x["loan_status"],c("loans")])
  return((as.numeric(x["loans"])/as.numeric(total_loan))*100)
}

by.int.segment$percent<- apply(by.int.segment,1,perc)




#-- 4.  installment
#-- Installlment does not have major significance on Loan status.
p1 <- ggplot(loan_clean,aes(x=installment))+geom_histogram()
p1 <- p1+labs(x = "Installment", y = "Number of Loan(s)",title="Distribution (OverAll)")+theme_title

p2 <- ggplot(loan_clean[loan_clean$loan_status=="Charged Off",],aes(x=installment))+geom_histogram()
p2 <- p2+labs(x = "Installment", y = "Number of Loan(s)",title="Distribution (Charged Off)")+theme_title

p3 <- ggplot(loan_clean[loan_clean$loan_status=="Fully Paid",],aes(x=installment))+geom_histogram()
p3 <- p3 +labs(x = "Installment", y = "Number of Loan(s)",title="Distribution (Fully Paid)")+theme_title

grid.arrange(p1,p2,p3,nrow=3,ncol=1)


#-- 5.  Grade
#-- From the normal graph. Can't see any significance on Grade on Loann Status.
#-- When we plot bar graph with % of loan for each loan status. 
#-- We can see as loan grade moves from A to G percentage of loan getting charged off are increasing.

p1 <- ggplot(loan_clean,aes(x=grade,fill=loan_status))+geom_bar(stat = 'count')
p1 <- p1+labs(x = "Grade", y = "Number of Loan(s)",title="Loans by Grades")+theme_title


p3 <- ggplot(loan_clean,aes(x=grade,fill=loan_status))+geom_bar(stat = 'count',position='fill')
p3 <- p3+labs(x = "Grade", y = "Number of Loan(s)",title="Loans by Grade by % of Loan Status")+theme_title

grid.arrange(p1,p3,nrow=2, ncol=1)

#-- More understand by Percentage table as below. 
#-- Based on Graph p1
#-- Number of Charged off loans are high B,C,D 
#-- Based on Percentage as per by.grade data frame
#-- Almost 70% loans are charged off when lent to applicant with LC assigned grade as B,C,D

bygrade <- group_by(loan_clean,loan_status,grade)
by.grade <- as.data.frame(summarise(bygrade,loans = n()))
by.grade
p1 <- ggplot(by.grade[by.grade$loan_status=='Charged Off',],aes(x=grade,y=loans))+geom_bar(stat = 'Identity')
p1 <- p1 + geom_text(stat = 'Identity', aes(label=loans),vjust=-0.50)
p1
by.grade$percentage <- apply(by.grade,1,FUN = perc)


#-- 6.  emp_length
#-- Based on below graphs, we can conclude.
#-- 1. As Employment length is increasing, number of loans taken by people is decreasing. Due to increase in salary and good pay.
#-- 2. But after 10 years, there is hike in loans, might be because of plannig for new car, home, mortgage loan etc and more responsibility.
#-- 3. Since there is hike in loan for 10+ years of experience and hence almost 22% of loans are charged off. 
#-- 4. For employment length of 2 to 5 year, it's high risk since 38.4 % of loans are charged off out of all charged off loans.  

summary(loan_clean$emp_length)

p1 <- ggplot(loan_clean,aes(x=emp_length))+geom_bar(stat = 'count')
p1 <- p1+labs(x = "Employment Length", y = "Number of Loan(s)",title="All Loans by Employment Length")+theme_title
p1 <- p1 + geom_text(stat='count',aes(label=..count..,vjust=-.50))


p2 <- ggplot(loan_clean[loan_clean$loan_status=="Charged Off",],aes(x=emp_length))+geom_bar(stat = 'count')
p2 <- p2+labs(x = "Employment Length", y = "Number of Loan(s)",title="Charged Off Loans by Employment Length")+theme_title
p2 <- p2 + geom_text(stat='count',aes(label=..count..,vjust=-.50))


grid.arrange(p1,p2,nrow=2,ncol=1)

byemplength <- group_by(loan_clean,emp_length)
by.emp.length <- as.data.frame(summarise(byemplength,loans = n()))
total.loan <- sum(by.emp.length$loans)
by.emp.length$percentage <- (by.emp.length[,c('loans')]/total.loan)*100
by.emp.length



#-- 7.  home_ownership
#-- Proportion of Charged Off loan is high, If home owneship of borrower is Rent or Mortgage, as compare to others.
#-- But as per graph p3. For Charged off loans and Fully Paid loans,  Proportion of different home_ownership is quite same.
#-- But Still it can be used as driving factor, we can investigate it's relationship with other variables.
summary(loan_clean$home_ownership)

p1 <- ggplot(loan_clean,aes(x=home_ownership))+geom_bar(stat='count')
p1 <- p1 + labs(x = "Home Ownership", y = "Number of Loan(s)",title="")+theme_title

p2 <- ggplot(loan_clean,aes(x=home_ownership,fill=loan_status))+geom_bar(stat='count')
p2 <- p2 + labs(x = "Home Ownership", y = "Number of Loan(s)",title="")+theme_title

p3 <- ggplot(loan_clean,aes(x=loan_status,fill=home_ownership))+geom_bar(stat='count',position='fill')
p3 <- p3 + labs(x = "Home Ownership", y = "Number of Loan(s)",title="")+theme_title+scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))


grid.arrange(p1,p2,p3,nrow=3,ncol=1)

#-- 8.  annual_inc 
#-- Removed outlier for annual income.
#-- After removing outlier, when we analyse impact of Annual Salary on loan status. 


income_seg <- function (x){
  segment <- ""
  if ( x<=30000){
    
    segment= "less Than 30k"
  }else if ( x >30000.00 && x <=60000.00){
    segment= "Between 30k to 60k"
  }else if ( x >60000.00 && x <=90000.00){
    segment= "Between 60k to 90k"
  }else if ( x >90000.00 && x <=120000.00){
    segment= "Between 90k to 120k"
  }else if ( x >120000.00){
    segment= "Greater Than 120k"
  }
  return (segment)
}




summary(loan_clean$annual_inc)
quantile(loan_clean$annual_inc,percentile)
plot(quantile(loan_clean$annual_inc,percentile))
loan_clean$inc_segment <- sapply(loan_clean$annual_inc,FUN = income_seg)
loan_clean$inc_segment <- factor(loan_clean$inc_segment )
b <- boxplot(loan_clean$annual_inc)
b$out
loan_clean_removed_outlier <- loan_clean[!loan_clean$annual_inc %in% b$out,]
quantile(loan_clean_removed_outlier$annual_inc,percentile)


#-- Conclusion
#-- Applicant earning between 30k to 90k are applying for loans
#-- For annual income between 30k to 90k, 70% of loans are charged off



summary(loan_clean_removed_outlier$inc_segment)
summary(loan_clean_removed_outlier$annual_inc)


p2 <- ggplot(loan_clean_removed_outlier[loan_clean_removed_outlier$loan_status=='Charged Off',],aes(x=annual_inc))+geom_histogram(binwidth = 10000)
p2 <- p2 + labs(x = "Income", y = "Number of Loan(s)",title="Charged off Loans Distribution By Income")+theme_title


p3 <- ggplot(loan_clean_removed_outlier,aes(x=inc_segment))+geom_bar(stat = 'count')
p3 <- p3+geom_text(stat = 'count',aes(label=..count..,vjust=-.50))
p3 <- p3 + labs(x = "Income Segment", y = "Number of Loan(s)",title="All Loans By Income Segment")+theme_title

p4 <- ggplot(loan_clean_removed_outlier[loan_clean_removed_outlier$loan_status=='Charged Off',],aes(x=inc_segment))+geom_bar(stat = 'count')
p4 <- p4+geom_text(stat = 'count',aes(label=..count..,vjust=-.50))
p4 <- p4 + labs(x = "Income Segment", y = "Number of Loan(s)",title="Charged Off Loans By Income Segment")+theme_title

grid.arrange(p2,p3,p4,nrow=3,ncol=1)

byincsegment <- group_by(loan_clean_removed_outlier[loan_clean_removed_outlier$loan_status=='Charged Off',],inc_segment)
by.inc.segment <- as.data.frame(summarise(byincsegment,loans = n()))
total.loan <- sum(by.inc.segment$loans)
by.inc.segment$percent <- (by.inc.segment$loans/total.loan)*100


#-- 9.  Verification Status
#-- Based on Graph p2, we can conclude that Verification status does not have major impact on Loan status.
#-- Since for each status of verification both loan status is having same proportion, there is no individual significance of Verification status. 
#-- But my be we can use this variable in combination with other driver variable to infer some outcomes.

p1 <- ggplot(loan_clean,aes(x=verification_status))+geom_bar(stat='count')
p1 <- p1 + labs(x = "Verification Status", y = "Number of Loan(s)",title="Loan count by Verification Status")+theme_title
p1

p2 <- ggplot(loan_clean,aes(x=verification_status,fill=loan_status))+geom_bar(stat='count',position = 'fill')
p2 <- p2 + labs(x = "Verification Status", y = "Number of Loan(s)",title="Loan count by Verification Status by % of Loan Status")+theme_title
p2 <- p2 + scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))
p2

p3 <- ggplot(loan_clean,aes(x=loan_status,fill=verification_status))+geom_bar(stat='count',position = 'fill')
p3 <- p3 + labs(x = "Loan Status", y = "Number of Loan(s)",title="Loan count by Loan Status by % of Verification Status")+theme_title
p3 <- p3 + scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))
p3

grid.arrange(p1,p2,p3,nrow=3,ncol=1)

#-- 10. Purpose
#-- Based on graph p1 most of the loans are borrowed for debt_consolidation and small_business
#-- Based on Graph p2 most of the loan borrowed for debt_consolication and small_business are most likely to get Charged off.
#-- Based on by.loan.purpose data frame we can say from over all charged off loans.
#-- 52% loan are lent for debt_consolication and small_business.

p1 <- ggplot(loan_clean,aes(x=purpose))+geom_bar(stat='count')
p1 <- p1 + labs(x = "Verification Status", y = "Number of Loan(s)",title="All Loan count by Purpose")+theme_title
p1 <- p1 + geom_text(stat = 'count', aes(label=..count..,vjust=-0.50))
p1
p2 <- ggplot(loan_clean[loan_clean$loan_status=='Charged Off',],aes(x=purpose))+geom_bar(stat='count')
p2 <- p2 + labs(x = "Verification Status", y = "Number of Loan(s)",title="Charged off Loan count by Purpose")+theme_title
p2 <- p2 + geom_text(stat = 'count', aes(label=..count..,vjust=-0.50))
p2

grid.arrange(p1,p2,nrow=2,ncol=1)

by.loan.purpose <- group_by(loan_clean[loan_clean$loan_status=='Charged Off',],purpose)
by.loan.purpose <- as.data.frame(summarise(by.loan.purpose,loans=n()))
total.loan <- sum(by.loan.purpose$loans)
by.loan.purpose$percent <- (by.loan.purpose$loans/total.loan)*100
by.loan.purpose





#-- 11. addr_state
#-- Based on Graph p2: We can see for CA, FL and NY Charged Off loans are very high. 
#-- It's around 37% of all Charged of loans,
#-- Using by.loan.state table, we can say CA, FL and NY have 37% of over all of charged off loans.

p1 <- ggplot(loan_clean,aes(x=addr_state))+geom_bar(stat='count')
p1 <- p1 + labs(x = "Address State", y = "Number of Loan(s)",title="All Loans by State")+theme_title
p1 <- p1 + geom_text(stat = 'count', aes(label=..count..),vjust=-0.50)
p1


p2 <- ggplot(loan_clean[loan_clean$loan_status=="Charged Off",] ,aes(x=addr_state))+geom_bar(stat='count')
p2 <- p2 + labs(x = "Address State", y = "Number of Loan(s)",title="Charged Off Loans by State")+theme_title
p2 <- p2 + geom_text(stat = 'count', aes(label=..count..),vjust=-0.50)
p2

grid.arrange(p1,p2,nrow=2,ncol=1)

by.loan.state <- group_by(loan_clean[loan_clean$loan_status=="Charged Off",],loan_status,addr_state)
by.loan.state <- as.data.frame(summarise(by.loan.state,loans=n()))
by.loan.state$percent <- (by.loan.state$loans/total.loan)*100
arrange(by.loan.state,desc(percent))

p3 <- ggplot(by.loan.state,aes(x=addr_state,y=loans))+geom_bar(stat='Identity')
p3


#-- 12. delinq_2yrs
#-- As per histogram no significance.
summary(loan_clean$delinq_2yrs)
ggplot(loan_clean,aes(x=delinq_2yrs))+geom_histogram(binwidth = 1)
ggplot(loan_clean[loan_clean$loan_status=="Charged Off",],aes(x=delinq_2yrs))+geom_histogram(binwidth = 1)

#--------------- Other Columns are Not Significant---------------------------


#--------------------------------------------- Bivariate Analysis------------------------------------------------
#-- Using findings and conclusion in Univariate analysis.
#-- We will do Bivariate analysis.
#-- We will Only consider variables found significant in Univariate Analysis.

#-- Below are the variables found significant in Univariable analysis.
#-- 1. term:        60% of over all charged off loans are lent for 36 months duration. 
#-- 2. int_rate:    80% of over all charged off loans are lent between 11 to 20% of interest rate. 
#-- 3. grade:       70% of over all charged off loans are lent to B,C,D grade applicant.
#-- 4. emp_length:  22% of over all charged off loans are lent to applicant having 10+ years of emp_length. 
#--                 38% of over all charged off loans are lent to applicant having 2 to 5 years of emp_length
#-- 5. annual_inc:  70% of over all charged off loans are lent to applicant having annaul income between 30 to 90k
#-- 6. purpose:     51% of over all charged off loans are give for debt_consolidation purpose.
#-- 7. Address:     37% of over all charged off loans are given to applicant in CA, FL, NY.

loan_charged_off <-  loan_clean[loan_clean$loan_status=='Charged Off',]

#---- 1.  term -> int_rate
#-- Approx. 88%% of loans, lent for 60 months term, are Charged Off if they are lent at interest rate between 11 to 20

p2 <- ggplot(loan_charged_off,aes(x=term,fill=int_segment))+geom_bar(stat = 'count',position = 'fill')+scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))
p2 <- p2 + labs(x = "Term", y = "Number of Loan(s)",title="Loans by Loan Term by % of Interest Rate Segment")+theme_title
p2
#---- 2.  term -> grade
#-- No significance.
p1 <- ggplot(loan_charged_off,aes(x=grade))+geom_bar(stat = 'count')
p2 <- ggplot(loan_charged_off,aes(x=term,fill=grade))+geom_bar(stat = 'count',position = 'fill')
grid.arrange(p1,p2,nrow=2,ncol=1)

#---- 3.  term -> grade
#-- No significance.

p1 <- ggplot(loan_charged_off,aes(x=emp_length))+geom_bar(stat = 'count')
p2 <- ggplot(loan_charged_off,aes(x=term,fill=emp_length))+geom_bar(stat = 'count')
grid.arrange(p1,p2,nrow=2,ncol=1)

#---- 4.  term -> annual_inc
#-- No significance.
ggplot(loan_charged_off,aes(x=term , fill= inc_segment))+geom_bar(stat='count',position='fill')



#---- 5.  term -> purpose
#-- No significance.
ggplot(loan_charged_off,aes(x=term , fill= purpose))+geom_bar(stat='count',position='fill')

#---- 6.  term -> state
#-- No significance.
ggplot(loan_charged_off,aes(x=term , fill= addr_state))+geom_bar(stat='count',position='fill')

#---- 7.  int_rate -> grade
#-- Approx. On Average 45% of loans, lent between 11 to 15% rate of interest , are Charged Off if they are lent to applicant of Grade B OR C
#-- Approx. On Average 42% of loans, lent between 16 to 20% rate of interest , are Charged Off if they are lent to applicant of Grade D OR E

perc_1 <- function(x){
  total_loan <- sum(int.segment.ana[int.segment.ana$int_segment==x["int_segment"],c("loans")])
  return((as.numeric(x["loans"])/as.numeric(total_loan))*100)
}

ggplot(loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),],aes(x=int_segment,fill=grade))+
  geom_bar(stat = 'count',position = 'fill')+
  labs(x = "Interest Segment", y = "Proportion of Loan",title="Loans by Interest Segment by % of Grade")+theme_title+scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))

int.segment.ana <-loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),]
int.segment.ana <- group_by(int.segment.ana,int_segment,grade)
int.segment.ana <- as.data.frame(summarise(int.segment.ana,loans = n()))
int.segment.ana$percent <- apply(int.segment.ana,1,FUN=perc_1)
int.segment.ana


#---- 8.  int_rate -> emp_length
#-- No significance.
ggplot(loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),],aes(x=emp_length))+
  geom_bar(stat = 'count')

ggplot(loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),],aes(x=int_segment,fill=emp_length))+
  geom_bar(stat = 'count',position = 'fill')


#---- 9.  int_rate -> annual_income

ggplot(loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),],aes(x=inc_segment))+
  geom_bar(stat = 'count')
ggplot(loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),],aes(x=int_segment,fill=inc_segment))+
  geom_bar(stat = 'count',position = 'fill')+scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))


#---- 10.  int_rate -> purpose
#-- Approx. 47% of loans, lent between 11 to 15% rate of interest , are Charged Off if purpose is debt_consolidation.
#-- Approx. 56% of loans, lent between 16 to 20% rate of interest , are Charged Off if purpose is debt_consolidation.

int.segment.ana <-loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),]
int.segment.ana <- group_by(int.segment.ana,int_segment,purpose)
int.segment.ana <- as.data.frame(summarise(int.segment.ana,loans = n()))
int.segment.ana$percent <- apply(int.segment.ana,1,FUN=perc_1)
arrange(int.segment.ana,int_segment,desc(percent))


p1 <- ggplot(int.segment.ana[int.segment.ana$int_segment=='Between 11 to 15',],aes(x=purpose,y=percent))+geom_bar(stat = 'Identity')
p1 <- p1 +labs(x = "Purpose", y = "Percentage of Loan",title="Loans by Purpose for Interest Rate Between 11 to 15")+theme_title

p2 <- ggplot(int.segment.ana[int.segment.ana$int_segment=='Between 16 to 20',],aes(x=purpose,y=percent))+geom_bar(stat = 'Identity')
p2 <- p2 +labs(x = "Purpose", y = "Percentage of Loan",title="Loans by Purpose for Interest Rate Between 16 to 20")+theme_title

grid.arrange(p1,p2,nrow=2,ncol=1)

#---- 11.  int_rate -> addr_state
#-- No Significance

int.segment.ana <-loan_charged_off[loan_charged_off$int_segment%in% c ("Between 11 to 15","Between 16 to 20"),]
int.segment.ana <- group_by(int.segment.ana,int_segment,addr_state)
int.segment.ana <- as.data.frame(summarise(int.segment.ana,loans = n()))
int.segment.ana$percent <- apply(int.segment.ana,1,FUN=perc_1)
arrange(int.segment.ana,int_segment,desc(percent))

#---- 12.  grade -> emp_length
#-- No Significance

ggplot(loan_charged_off[loan_charged_off$grade%in% c ("B","C","D"),],aes(x=grade))+
  geom_bar(stat = 'count')
ggplot(loan_charged_off[loan_charged_off$grade%in% c ("B","C","D"),],aes(x=grade,fill=emp_length))+
  geom_bar(stat = 'count',position = 'fill')+scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))


#---- 13.  grade -> annual_inc
#-- Approx. On an Average 48% of loans, lent to applicant in grade (B,C,D), are Charged Off if there annual income is'Between 30k to 60k'.
#-- Approx. On an Average 23% of loans, lent to applicant in grade (B,C,D), are Charged Off if there annual income is'Between 60k to 90k'.

perc_2 <- function(x){
  total_loan <- sum(grade.ana[grade.ana$grade==x["grade"],c("loans")])
  return((as.numeric(x["loans"])/as.numeric(total_loan))*100)
}

grade.ana <- loan_charged_off[loan_charged_off$grade%in% c ("B","C","D"),]
grade.ana <- group_by(grade.ana,grade,inc_segment)
grade.ana <- as.data.frame(summarise(grade.ana,loans = n()))
grade.ana$percent <- apply(grade.ana,1,FUN=perc_2)
grade.ana

p1 <- ggplot(grade.ana,aes(x=grade,y=loans, fill=inc_segment))+geom_bar(stat='Identity',position = 'fill')+scale_y_continuous(labels = scales::percent(c(seq (0,1,0.25))))
p1 <- p1 + labs(x = "Applicant Grade", y = "Loan Count",title="Loans by Applicant Grade by % of Income Segment")+theme_title
p1




#---- 14.  grade -> purpose
#-- Approx. 48% of loans, lent to applicant in grade ( B,C,D), are Charged Off if there purpose of loan is 'debt_consolidation'

grade.ana <-loan_charged_off[loan_charged_off$grade%in% c ("B","C","D"),]
grade.ana <- group_by(grade.ana,purpose)
grade.ana <- as.data.frame(summarise(grade.ana,loans = n()))
total.loan <- sum(grade.ana$loans)
grade.ana$percent <- (grade.ana$loans/total.loan)*100
arrange(grade.ana,desc(percent))


p1 <- ggplot(grade.ana,aes(x=purpose,y=percent))+geom_bar(stat = 'Identity')
p1 <- p1+labs(x = "Purpose", y = "Percentage Of Loan",title="Loans by Purpose for Applicant in Grade (B,C,D)")+theme_title
p1 <- p1 + geom_text(stat = 'Identity',aes(label=round(percent,2),vjust=-0.50))
p1



#---- 15.  grade -> addr_state
#-- No Significance

grade.ana <-loan_charged_off[loan_charged_off$grade%in% c ("B","C","D"),]
grade.ana <- group_by(grade.ana,addr_state)
grade.ana <- as.data.frame(summarise(grade.ana,loans = n()))
total.loan <- sum(grade.ana$loans)
grade.ana$percent <- (grade.ana$loans/total.loan)*100
arrange(grade.ana,desc(percent))

#---- 16.  emp_length -> annual_inc
#-- Approx. 50% of loans, lent to applicant having employment length between 2 to 5 years, are Charged Off if there Annual income is between Between 30k to 60k.
#-- Approx. 22% of loans, lent to applicant having employment length between 2 to 5 years, are Charged Off if there Annual income is between Between 60k to 90k.

#-- This case changes if Employment length is 10+
#-- Approx. 10% decrease respect to above case, if lent to applicant having Annual income is between Between 30k to 60k.
#-- Approx. 10% Increase respect to above case, if lent to applicant having Annual income is between Between 60k to 90k.

emp_length.ana <-loan_charged_off[loan_charged_off$emp_length%in% c ("2 years","3 years","4 years","5 years"),]
emp_length.ana <- group_by(emp_length.ana,inc_segment)
emp_length.ana <- as.data.frame(summarise(emp_length.ana,loans = n()))
total.loan <- sum(emp_length.ana$loans)
emp_length.ana$percent <- (emp_length.ana$loans/total.loan)*100
arrange(emp_length.ana,desc(percent))


p1 <- ggplot(emp_length.ana,aes(x=inc_segment,y=percent))+geom_bar(stat = 'Identity')
p1 <- p1+labs(x = "Term", y = "Percentage Of Loan",title="Loans by Income segment for Applicant between 2 to 5 year Employment Length")+theme_title
p1 <- p1 + geom_text(stat = 'Identity',aes(label=round(percent,2),vjust=-0.50))
p1

emp_length.ana <-loan_charged_off[loan_charged_off$emp_length%in% c ("10+ years"),]
emp_length.ana <- group_by(emp_length.ana,inc_segment)
emp_length.ana <- as.data.frame(summarise(emp_length.ana,loans = n()))
total.loan <- sum(emp_length.ana$loans)
emp_length.ana$percent <- (emp_length.ana$loans/total.loan)*100
arrange(emp_length.ana,desc(percent))


#---- 17.  emp_length -> purpose
#-- Approx. 48% of loans, lent to applicant having employment length between 2 to 5 years, are Charged Off if purpose is debt_consolidation.
#-- For Employment Length 10+ Years, it's increases by 5%.
#-- Approx. 53% of loans, lent to applicant having employment length more than 10+ years, are Charged Off if purpose is debt_consolidation.

emp_length.ana <-loan_charged_off[loan_charged_off$emp_length%in% c ("2 years","3 years","4 years","5 years"),]
emp_length.ana <- group_by(emp_length.ana,purpose)
emp_length.ana <- as.data.frame(summarise(emp_length.ana,loans = n()))
total.loan <- sum(emp_length.ana$loans)
emp_length.ana$percent <- (emp_length.ana$loans/total.loan)*100
arrange(emp_length.ana,desc(percent))


p1 <- ggplot(emp_length.ana,aes(x=purpose,y=percent))+geom_bar(stat = 'Identity')
p1 <- p1+labs(x = "Purpose", y = "Percentage Of Loan",title="Loans by Purpose for Applicant between 2 to 5 year Employment Length")+theme_title
p1 <- p1 + geom_text(stat = 'Identity',aes(label=round(percent,2),vjust=-0.50))
p1



emp_length.ana <-loan_charged_off[loan_charged_off$emp_length%in% c ("10+ years"),]
emp_length.ana <- group_by(emp_length.ana,purpose)
emp_length.ana <- as.data.frame(summarise(emp_length.ana,loans = n()))
total.loan <- sum(emp_length.ana$loans)
emp_length.ana$percent <- (emp_length.ana$loans/total.loan)*100
arrange(emp_length.ana,desc(percent))


p2 <- ggplot(emp_length.ana,aes(x=purpose,y=percent))+geom_bar(stat = 'Identity')
p2 <- p2+labs(x = "Purpose", y = "Percentage Of Loan",title="Loans by Purpose for Applicant Greater 10+ years")+theme_title
p2 <- p2 + geom_text(stat = 'Identity',aes(label=round(percent,2),vjust=-0.50))
p2

grid.arrange(p1,p2,nrow=2,ncol=1)

#---- 17.  emp_length -> addr_state
#-- No Significance
emp_length.ana <-loan_charged_off[loan_charged_off$emp_length%in% c ("2 years","3 years","4 years","5 years"),]
emp_length.ana <- group_by(emp_length.ana,addr_state)
emp_length.ana <- as.data.frame(summarise(emp_length.ana,loans = n()))
total.loan <- sum(emp_length.ana$loans)
emp_length.ana$percent <- (emp_length.ana$loans/total.loan)*100
arrange(emp_length.ana,desc(percent))

emp_length.ana <-loan_charged_off[loan_charged_off$emp_length%in% c ("10+ years"),]
emp_length.ana <- group_by(emp_length.ana,addr_state)
emp_length.ana <- as.data.frame(summarise(emp_length.ana,loans = n()))
total.loan <- sum(emp_length.ana$loans)
emp_length.ana$percent <- (emp_length.ana$loans/total.loan)*100
arrange(emp_length.ana,desc(percent))


#---- 18.  annual_inc -> purpose
#-- Approx. 52% of loans, lent to applicant having annual income between 30 to 90k, are Charged Off if purpose is debt_consolidation.

annual_inc.ana <-loan_charged_off[loan_charged_off$inc_segment%in% c ("Between 30k to 60k","Between 60k to 90k"),]
annual_inc.ana <- group_by(annual_inc.ana,purpose)
annual_inc.ana <- as.data.frame(summarise(annual_inc.ana,loans = n()))
total.loan <- sum(annual_inc.ana$loans)
annual_inc.ana$percent <- (annual_inc.ana$loans/total.loan)*100
arrange(annual_inc.ana,desc(percent))


p1 <- ggplot(annual_inc.ana,aes(x=purpose,y=percent))+geom_bar(stat = 'Identity')
p1 <- p1+labs(x = "Purpose", y = "Percentage Of Loan",title="Loans by Purpose for Applicant With Income Between 30k and 90k")+theme_title
p1 <- p1 + geom_text(stat = 'Identity',aes(label=round(percent,2),vjust=-0.50))
p1



#---- 18.  annual_inc -> addr_state
#-- No Significance
annual_inc.ana <-loan_charged_off[loan_charged_off$inc_segment%in% c ("Between 30k to 60k","Between 60k to 90k"),]
annual_inc.ana <- group_by(annual_inc.ana,addr_state)
annual_inc.ana <- as.data.frame(summarise(annual_inc.ana,loans = n()))
total.loan <- sum(annual_inc.ana$loans)
annual_inc.ana$percent <- (annual_inc.ana$loans/total.loan)*100
arrange(annual_inc.ana,desc(percent))


#-- Below are the variables found significant in Bivariate analysis.

#-- term -> int_rate 			
#-- Approx. 88%% of loans, lent for 60 months term, are Charged Off if they are lent at interest rate between 11 to 20

#-- int_rate -> grade 			
#-- Approx. On Average 45% of loans, lent between 11 to 15% rate of interest , are Charged Off if they are lent to appplicant of Grade B OR C
#-- Approx. On Average 42% of loans, lent between 16 to 20% rate of interest , are Charged Off if they are lent to appplicant of Grade D OR E

#-- int_rate -> purpose			
#-- Approx. 47% of loans, lent between 11 to 15% rate of interest , are Charged Off if purpose is debt_consolidation.
#-- Approx. 56% of loans, lent between 16 to 20% rate of interest , are Charged Off if purpose is debt_consolidation.

#-- grade -> annual_inc			
#-- Approx. On an Average 48% of loans, lent to applicant in grade (B,C,D), are Charged Off if there annual income is'Between 30k to 60k'.
#-- Approx. On an Average 23% of loans, lent to applicant in grade (B,C,D), are Charged Off if there annual income is'Between 60k to 90k'.


#-- grade -> purpose			
#--	Approx. 48% of loans, lent to applicant in grade ( B,C,D), are Charged Off if there purpose of loan is 'debt_consolidation'

#-- emp_length -> annual_inc 	
#--	Approx. 50% of loans, lent to applicant having employment length between 2 to 5 years, are Charged Off if there Annual income is between Between 30k to 60k.
#-- Approx. 22% of loans, lent to applicant having employment length between 2 to 5 years, are Charged Off if there Annual income is between Between 60k to 90k.

#-- This case changes if Employment length is 10+
#-- Approx. 10% decrease respect to above case, if lent to applicant having Annual income is between Between 30k to 60k.
#-- Approx. 10% Increase respect to above case, if lent to applicant having Annual income is between Between 60k to 90k.


#-- emp_length -> purpose 		
#--	Approx. 48% of loans, lent to applicant having employment length between 2 to 5 years, are Charged Off if purpose is debt_consolidation.

#-- For Employment Length 10+ Years, it's increases by 5%.
#-- Approx. 53% of loans, lent to applicant having employment length more than 10+ years, are Charged Off if purpose is debt_consolidation.

#-- annual_inc -> purpose 		
#--	Approx. 52% of loans, lent to applicant having annual income between 30 to 90k, are Charged Off if purpose is debt_consolidation.





#----------------------------------------Final Conclusion----------------------------------------------

#-- 1.  Long term loan with high interest rate are most likely to get charged off.
#-- 2.  44% loan on an average, get Charged off if lent between 11 to 20 Interest rate, to applicant having Grade B,C,D or E. 
#-- 3.  Applicant, with annual income between 30k to 60k and Grade in B,c or D, are likely to be defaulter
#-- 4.  50% of time Applicant, having annual income of 30k to 60k and with employment length of 2 to 5 years, are likely to get defaulter.
#-- 5.  Most common finding is related to purpose of loan, if Applicant is taking loan for "debt_consolidation" 50% chances that it will get defaulter.
