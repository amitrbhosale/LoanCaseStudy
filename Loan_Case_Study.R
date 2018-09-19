#Install and load required packages
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("ggthemes")
library("ggthemes")
library(ggplot2)
library(gridExtra)
library(stringr)
library("lubridate")
library(stringr)

#Set the working directory
#setwd("D:/upgrad/Loan Case Study")

#Load the data from csv to 'loan' data frame
loan <- read.csv("loan.csv",stringsAsFactors = FALSE)
str(loan)

#Total Null values in the dataset - 2262491
sum(is.na(loan))

#Total number of records - 39717
nrow(loan)

#Check if loan ids are unique in the dataset
length(unique(loan$id))

#Removing the columns which has no data. (Check with code. shows error and need to run multiple times)

#This will give us Column wise NA Value Count 
sapply(colnames(loan), function(x) length(which(is.na(loan[,x]))))

#There are 54 columns having NA values throughout
sum(sapply(colnames(loan), function(x) length(which(is.na(loan[,x])))) == nrow(loan))

#Checking blank values
sapply(colnames(loan),function(x) length(which(loan[,x]=="")))

#Removing Column Having All NA Values
loan<-loan[!sapply(loan, function(x) all(is.na(x)))]
ncol(loan) #Returns 57 columns now
sum(is.na(loan)) #118646

#Removing Column Having All Constant Values
vapply(loan, function(x) length(unique(x)) > 1, logical(1L))
temp <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

a <- colnames(loan)
b <- colnames(temp)
setdiff(a,b) # Returns column names which are not available in the loan dataset

#Remove the temp dataframe and other variables once the data is verified
rm(list = c("temp","b","a"))

#Removed the columns with constant values accoss the dataset.
loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]
ncol(loan) # Returns 51 columns now

#Remove  % from int_rate and convert to numeric
loan$int_rate <- str_remove_all(loan$int_rate,"\\%")
loan$int_rate <- as.numeric(loan$int_rate)

#Convert columns to Factor
loan$term <- as.factor(loan$term)
loan$grade <- as.factor(loan$grade)
loan$sub_grade <- as.factor(loan$sub_grade)
loan$home_ownership <- as.factor(loan$home_ownership)
loan$verification_status <- as.factor(loan$verification_status)
loan$loan_status <- as.factor(loan$loan_status)
loan$purpose <- as.factor(loan$purpose)

#Remove insignificant columns
loan$title <- NULL
loan$desc <- NULL
loan$emp_title <- NULL

ncol(loan)

#Removing Inconsistency in Date Field
#Convert to valid date for issue_d
loan$issue_d <- paste0("01-",loan$issue_d)
loan$issue_d <- as.Date(loan$issue_d,format = "%d-%b-%y")

#Convert to valid date for earliest_cr_line
loan$earliest_cr_line <- paste0("01-",loan$earliest_cr_line)
loan$earliest_cr_line <- as.Date(loan$earliest_cr_line,format = "%d-%b-%y")

#Convert to valid date for last_pymnt_d
loan$last_pymnt_d <- paste0("01-",loan$last_pymnt_d)
loan$last_pymnt_d <- as.Date(loan$last_pymnt_d,format = "%d-%b-%y")

#Convert to valid date for last_credit_pull_d
loan$last_credit_pull_d <- paste0("01-",loan$last_credit_pull_d)
loan$last_credit_pull_d <- as.Date(loan$last_credit_pull_d,format = "%d-%b-%y")

#Removing % symbol and converting interest to numeric type
loan$int_rate <- gsub("%","",loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)

#Removing % symbol from revol_util and converting interest to numeric type
loan$revol_util <- gsub("%","",loan$revol_util)
loan$revol_util <- as.numeric(loan$revol_util)

#Deriving Interest bucket attribute
loan$Interest_Bucket[loan$int_rate>0 & loan$int_rate<=15] <- "Low"
loan$Interest_Bucket[loan$int_rate>15 & loan$int_rate<=20] <- "Medium"
loan$Interest_Bucket[loan$int_rate>20] <- "High"

#Deriving open_acc_bucket attribute
loan$open_acc<- as.integer(loan$open_acc)
loan$open_acc_bucket[loan$open_acc>=00 & loan$open_acc<=15] <- "0 - 15"
loan$open_acc_bucket[loan$open_acc >= 16 & loan$open_acc <= 30] <- "16 - 30"
loan$open_acc_bucket[loan$open_acc >= 31 & loan$open_acc <= 45] <- "31 - 45"
loan$open_acc_bucket[loan$open_acc >= 46] <- "46+"

#Deriving revol_util_bucket attribute
loan$revol_util <- as.integer(loan$revol_util)
loan$revol_util_bucket[loan$revol_util>=0 & loan$revol_util<=40] <- "0-40"
loan$revol_util_bucket[loan$revol_util>=41 & loan$revol_util<=80] <- "41-80"
loan$revol_util_bucket[loan$revol_util>=81] <- "81+"

#Deriving dti_bucket attribute
loan$dti <- as.integer(loan$dti)
loan$dti_bucket[loan$dti>=0 & loan$dti<=10] <- "0-10"
loan$dti_bucket[loan$dti>=11 & loan$dti<=20] <- "11-20"
loan$dti_bucket[loan$dti>=21 & loan$dti<=30] <- "21-30"
loan$dti_bucket[loan$dti>=31] <- "30+"

#Deriving annual_inc_bucket attribute
loan$annual_inc <- as.integer(loan$annual_inc)
loan$annual_inc_bucket[loan$annual_inc>=0 & loan$annual_inc<=30999] <- "0-30k"
loan$annual_inc_bucket[loan$annual_inc>=31000 & loan$annual_inc<=60999] <- "31-60k"
loan$annual_inc_bucket[loan$annual_inc>=61000 & loan$annual_inc<=90999] <- "61-90k"
loan$annual_inc_bucket[loan$annual_inc>=91000] <- "90k+"

#Income per month derived attribute
loan$income_per_month <- loan$annual_inc/12

#Convert term to numeric
loan$term <- str_replace_all(loan$term, " months","")
loan$term <- as.numeric(loan$term)

#Convert to valid date for earliest_cr_line
loan$earliest_cr_line <- as.Date(loan$earliest_cr_line,format = "%d-%b-%y")

for (i in 1:nrow(loan))
{
  if(loan$earliest_cr_line[i] > Sys.Date())
  {
    loan$earliest_cr_line[i] <- as.Date(format(loan$earliest_cr_line[i],"19%y-%m-%d"),"%Y-%m-%d")
    
  }
}

#Last_payment_Credit_pull_date derived attribute
for (i in 1:nrow(loan)) {
  if(!is.na(loan$last_pymnt_d[i])){
  loan$Last_payment_Credit_pull_date[i] <- loan$last_pymnt_d[i]
  }else{
    loan$Last_payment_Credit_pull_date[i] <- loan$last_credit_pull_d[i]
  }
}

loan$Last_payment_Credit_pull_date <- as.Date(loan$Last_payment_Credit_pull_date,origin="1970-01-01")

#Removing Outliers 
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
loan$int_rate <- remove_outliers(loan$int_rate)

#Calculating the FICO Score (Our Assumption as mentioned here below)

#Max FICO Score = 850

#                       Percentage	      Score(Percentage /100 * Max FICO Score)
#FICO Score (Total)	        100	                        850
#Payment History	           35	                        297.5
#Amounts Owed	               30	                        255
#Length of Credit History	   15	                        127.5
#New Credit	                 10	                         85
#Credit Mix 
# or Credit Utilization	     10	                         85

#Calculating difference between earliest crdit line date znd last payment date
sd <- as.POSIXlt(loan$earliest_cr_line)
ed <- as.POSIXlt(loan$Last_payment_Credit_pull_date)
loan$length_of_history <- 12 * (ed$year - sd$year) + (ed$mon - sd$mon)

#Calculating length_of_history_points attribute
for (i in 1:nrow(loan)) {
  loan$length_of_history_points[i] <- (loan$length_of_history[i] * 127.5)/max(loan$length_of_history)
}

#Calculating revol_util_points (utilization of credit points) attribute
for (i in 1:nrow(loan)) {
  loan$revol_util_points[i] <- 85 - ((85*loan$revol_util[i])/100)
}
loan$revol_util_points[which(is.na(loan$revol_util_points))] <- 0

#Calculating new_credit_points attribute
for (i in 1:nrow(loan)) {
  loan$new_credit_points[i] <- 85 - ((85*loan$inq_last_6mths[i])/8)
}

#Calculating payment_points attribute
for (i in 1:nrow(loan)) {
  loan$payment_points[i] <- (297.5*((loan$total_pymnt[i]-loan$total_rec_late_fee[i])/(loan$installment[i]*loan$term[i]))*100)/100
}

#Calculating amount_owed_points attribute
for (i in 1:nrow(loan)) {
  loan$amount_owned_points[i] <- 255 - ((255*((loan$out_prncp[i])/(loan$installment[i]*loan$term[i]))*100)/100)
}

#Total FICO score
for (i in 1:nrow(loan)) {
  loan$fico_score[i] <- loan$payment_points[i] + loan$amount_owned_points[i] + loan$length_of_history_points[i] + loan$new_credit_points[i] + loan$revol_util_points[i]
}
loan$fico_score <- round(loan$fico_score,digits = 0)

for (i in 1: nrow(loan))
{
  if(loan$fico_score[i]>=300 & loan$fico_score[i]<=579 )
  { loan$fico_rating[i] <- "Very Poor" }
  else
    {
      if(loan$fico_score[i]>=580 & loan$fico_score[i]<=669 )
      { loan$fico_rating[i] <- "Fair"}
      else{
        if(loan$fico_score[i]>=670 & loan$fico_score[i]<=739 )
        { loan$fico_rating[i] <- "Good"}
        else{
          if(loan$fico_score[i]>=740 & loan$fico_score[i]<=799 )
          { loan$fico_rating[i] <- "Very Good"}
          else{
            if(loan$fico_score[i]>=800 & loan$fico_score[i]<=850 )
            { loan$fico_rating[i] <- "Exceptional"}
            else{
              loan$fico_rating[i] <- "NA"
            }
          }
        }
        }
    }
}
loan$fico_rating <- as.factor(loan$fico_rating)  

#All Requests Bar Graph 
#(Graph 1)
All_requests <- ggplot(loan,aes(x=loan_status,fill=loan_status)) + geom_bar()
All_requests <- All_requests + geom_text(stat='count', aes(label=..count..), vjust=-1) + xlab("Loan Status")+ylab("Count")+ggtitle("Loan Status Vs Count ")+theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Loan Status") +theme_economist()
All_requests

#Graphs based on different groupings
Open_acc_graph <- ggplot(loan,aes(x=open_acc_bucket,fill=loan_status))+geom_bar() + xlab("Open Accounts Range")+ylab("Count")+ggtitle("Open Accounts Range Vs Count ")+theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Loan Status") +theme_economist()
revol_util_graph <-ggplot(loan,aes(x=revol_util_bucket,fill=loan_status))+geom_bar() + xlab("Revolving Utilization Range")+ylab("Count")+ggtitle("Revolving Utilization Range Vs Count ")+theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Loan Status") +theme_economist()
dti_graph <- ggplot(loan,aes(x=dti_bucket,fill=loan_status))+geom_bar() + xlab("DTI Range")+ylab("Count")+ggtitle("DTI Range Vs Count ")+theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Loan Status") +theme_economist()
annual_inc_graph <- ggplot(loan,aes(x=annual_inc_bucket,fill=loan_status))+geom_bar() + xlab("Annual Income Range")+ylab("Count")+ggtitle("Annual Income Range Vs Count ")+theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Loan Status") +theme_economist()

#As per below graphs, none of the provided attributes give direct insights about loan defaulters
#(Graph 2)
grid.arrange(Open_acc_graph,revol_util_graph,dti_graph,annual_inc_graph,ncol=2)

#Group Vs interest rate(mean) line graph
grade_int <- aggregate(loan$int_rate,by=list(loan$grade),mean)
names(grade_int) <- c("Grade","Average_Interest_Rate")
grade_int_graph <- ggplot(grade_int,aes(x=Grade,y=Average_Interest_Rate ,group = 1)) + geom_point() + geom_line()
grade_int_graph <- grade_int_graph + ggtitle("Grade Vs Average Interest Rate") + xlab("Grade")+ylab("Average Interest Rate")+ theme(plot.title = element_text(hjust = 0.5)) + theme_economist()
grade_int_graph

#Bar graph which shows grade count for each fico_rating status
loan$fico_rating <- factor(loan$fico_rating, levels = c("Exceptional", "Very Good",  "Good", "Fair","Very Poor","NA"))
fico_rating_grade <- ggplot(loan,aes(x=fico_rating,fill=grade)) + geom_bar(position = "fill")
fico_rating_grade <- fico_rating_grade + ggtitle("Fico Rating Vs Grade Allocated") + xlab("FICO Rating")+ylab("Range")+ theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Grade") +theme_economist()
fico_rating_grade

#From below graph it is noted that the grades are based on the interest rate and and higher the fico range lower will be the interest.
#Graph 3
grid.arrange(grade_int_graph,fico_rating_grade,ncol=2)

#Status vs Fico Score box plot - Highligh : clearly shows lower FICO score users have likely to be defaulted.
Fico_score_graph <- ggplot(loan,aes(x=loan_status,y=loan$fico_score)) + geom_boxplot()
Fico_score_graph <- Fico_score_graph + ggtitle("Fico Score Range for each Loan Status") + xlab("Loan Status")+ylab("FICO Score")+ theme(plot.title = element_text(hjust = 0.5)) +theme_economist() 
Fico_score_graph

#Loan_status count for each Fico_rating bar graph
Fico_Loan_Status_bar <- ggplot(loan,aes(x=loan_status,fill=loan_status)) + geom_bar()
Fico_Loan_Status_bar <- Fico_Loan_Status_bar + facet_grid(~fico_rating)
Fico_Loan_Status_bar <- Fico_Loan_Status_bar + geom_text(stat='count', aes(label=..count..), vjust=-0.5)
Fico_Loan_Status_bar <- Fico_Loan_Status_bar + ggtitle("Loan Status wise count in each FICO Rating") + xlab("Loan Status")+ylab("Count")+ theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Loan Status") +theme_economist()
Fico_Loan_Status_bar

#Fico_rating Vs status Bar graph

Fico_Status_Requests <- ggplot(loan,aes(x=fico_rating)) + geom_bar()
Fico_Status_Requests <- Fico_Status_Requests + ggtitle("No of Loan Requests in each FICO Rating")
Fico_Status_Requests <- Fico_Status_Requests + geom_text(stat='count', aes(label=..count..), vjust=-0.5) + xlab("FICO Rating")+ylab("Count")+ theme(plot.title = element_text(hjust = 0.5)) +theme_economist()
Fico_Status_Requests <- Fico_Status_Requests + ggtitle("FICO Rating wise Requests")
Fico_Status_Requests

#Main Graph (Graph 4)
grid.arrange(Fico_score_graph,Fico_Status_Requests,ncol=2)

#Graph 5
Fico_Status_bar <- ggplot(loan,aes(x=fico_rating,fill=loan_status)) + geom_bar(position = "fill")+scale_y_continuous(labels = scales::percent, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))
Fico_Status_bar <- Fico_Status_bar + ggtitle("Loan Status Percentage for each FICO Rating") + xlab("FICO Rating")+ylab("Percentage")+ theme(plot.title = element_text(hjust = 0.5))+ labs(fill="Loan Status") +theme_economist()
Fico_Status_bar

#Subset for Charged off, full paid and charged off- fully paid comibined
Charged_Off <- subset(loan,loan$loan_status=="Charged Off")
fully_paid<- subset(loan,loan$loan_status=="Fully Paid")
Defaulter_And_Paid <- subset(loan,loan$loan_status=="Charged Off" | loan$loan_status=="Fully Paid")

#Loan Status Vs Int_rate box plot
ggplot(loan,aes(x=loan_status, y = int_rate)) +geom_boxplot() + xlab("Loan Status") +ylab("Interest Rate") + ggtitle("Loan Status Vs Interest Rate (Removed Outliers)") + theme_economist()
loan$monthsdf <- interval(loan$issue_d,loan$last_pymnt_d)

#Purpose Vs Interest rate Box plot
ggplot(loan,aes(x=purpose,y=int_rate)) + geom_boxplot()

#Plot 1
#No. of Loan Requests vs Status
p1 <- ggplot(loan,aes(x=loan_status)) +geom_bar()+ theme_economist() + xlab("Loan Status") + ylab("Count") + ggtitle("No. of Loan requests by Status") +geom_text(stat="count",aes(label=..count..),vjust =0)
p1

#Plot 2
#Relation between the grade and average loan interest.
p2 <- ggplot(grade_int,aes(x=Grade,y=Average_Interest_Rate,group = 1)) +geom_point() + geom_line() +theme_economist() + ggtitle("Average Interest Rate by Grade")
p2

#Plot 3
#Relation between the Fico Score and the grade
p3 <- ggplot(loan,aes(x=fico_rating,fill=grade)) + geom_bar(position = "fill") + theme_economist() + xlab("FICO Rating") + ggtitle("FICO Rating vs Grade")
p3

