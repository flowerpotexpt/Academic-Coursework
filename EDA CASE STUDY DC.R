#Load Libraries
library(lubridate)
library(ggplot2)
library(dplyr)


#Read Data
loan <- read.csv("loan.csv")
View(loan)

#data cleaning


#remove column with duplicate values
loan <- loan[, !apply(loan , 2 , function(x)
  length(unique(x)) == 1)]



#remove URL
loan$url <- NULL

#remove the column with na values and more than 65% with 0


loan <- loan[, colSums(is.na(loan)) != nrow(loan)]


loan <- loan[, -which(colMeans(loan == 0 | is.na(loan)) > 0.65)]



#rounding off the funded_amt_inv, annual_inc
loan$funded_amnt_inv <- round(loan$funded_amnt_inv)
loan$installment <- round(loan$installment)
loan$annual_inc <- round(loan$annual_inc)
loan$total_pymnt <- round(loan$total_pymnt)
loan$total_pymnt_inv <- round(loan$total_pymnt_inv)
loan$total_rec_prncp <- round(loan$total_rec_prncp)
loan$last_pymnt_amnt <- round(loan$last_pymnt_amnt)


#removing the months from the term
loan$term <- substr(loan$term, 2, 3)

#removing percent symbol from int_rate and revol_util
loan$int_rate <- as.character(loan$int_rate)
loan$int_rate <-
  as.numeric(substr(loan$int_rate, 0, nchar(loan$int_rate) - 1))

loan$revol_util <- as.character(loan$revol_util)
loan$revol_util <-
  as.numeric(substr(loan$revol_util, 0, nchar(loan$revol_util) - 1))

#remove na values
for (i in 1:ncol(loan)) {
  loan[is.na(loan[, i]), i] <- 0
}

#derived metrics-

#now we will group int_rate variable as below:

loan$int_rate_grp <- 
  ifelse(
    loan$int_rate < 10,"Low",
    ifelse(
      loan$int_rate >= 10 &
        loan$int_rate <= 18,"Medium",
      ifelse(
        loan$int_rate > 18,"High",
        "NA"
      )
    )
  )

#grouping of emp_length varaible as below

str(loan$emp_length)

table(loan$emp_length)

loan$emp_length <- trimws(tolower(loan$emp_length))

loan$emp_len_grp <-
  ifelse(
    loan$emp_length %in% c("< 1 year", "1 year", "2 years", "3 years", "4 years"),
    "Junior",
    ifelse(
      loan$emp_length %in% c("5 years", "6 years", "7 years", "8 years") ,
      "Mid-Level",
      ifelse(
        loan$emp_length %in% c("9 years", "10+ years"),
        "Senior",
        "NA"
      )
    )
  )

#creating income categories


loan$inc_cat <- ifelse(loan$annual_inc > 59000, "High Income", "Low Income")



#univariate analysis starts here 
#on home_ownership column
ggplot(loan, aes(x = loan_status)) + geom_bar() + geom_text(stat = 'count', aes(label = ..count..),vjust = -1.5)



#home ownership with loan statuses, Charged off is high with Mortgage,Rent
ggplot(loan, aes(x = loan$loan_status)) + geom_bar(stat = "count") + facet_wrap(~ loan$home_ownership)


#univariate analysis done to plot % contribution of loan_status = charged_off & then all the loan statuses 
#In first plot, it is a comparison between 2 categorical variables & in the second plot, the comparison is done between 2 
#categorical & one continuous varaible


home_ownership_co <- filter(loan, loan_status == "Charged Off")
View(home_ownership_co)
CO <- prop.table(table(home_ownership_co$loan_status,home_ownership_co$home_ownership), 1)*100

View(CO)

CO <- as.data.frame(CO, stringsAsFactors = TRUE)
CO <- na.omit(CO, round(CO$Freq))

View(CO)


barplot(CO$Freq, names.arg = CO$Var2)

--------------------------------------------------------------------------------------------
  
  home_ownership <- data_frame(loan$loan_status,loan$home_ownership)
View(home_ownership)
ALL <-prop.table(table(home_ownership), 2)*100



View(ALL)

ALL <- as.data.frame(ALL)

class(ALL)

C1 <- ggplot() + geom_bar(aes(y=ALL$Freq, x = ALL$loan.home_ownership, fill = ALL$loan.loan_status), data = ALL, stat = "identity")

C2 <- C1 + geom_text(data = ALL, aes( x = ALL$loan.home_ownership, y = ALL$Freq, label = ALL$Freq), size = 4)

C2


#Plot loan status for every state
# it can be inferred from the plot that the state CA is the most tricky state when it comes to approving / rejecting loan requests.
# Reason being - the highest number of Fully paid as well the highest number of Charged-off loans are from the same state!
# IL, NY, TX are the states where the decision to approving loans is being taken most correctly.

ggplot(loan, aes(x = addr_state,fill=loan_status)) + geom_bar(stat = "count", position = position_dodge())


# to plot loan status as per Purpose.
# it can be seen that debt consolidation is a tricky category. Most Fully paid loans & most charged_off happens 
# in this category of loan purpose
ggplot(loan, aes(x = loan$purpose, fill= loan$loan_status)) + geom_bar(position = position_dodge())


## Boxplot to show inquiries in last 6 months has relationship with chargeoff
ggplot(loan, aes(x = loan$loan_status, y = loan$inq_last_6mths)) + geom_boxplot(fill = "red3", colour = "green4")


#Outliers check
quantile(loan$annual_inc)

# we will subset the data to remove outliers as AVERAGE annual income is being considered for drawing interference.
# else the numbers that we draw will be very different

loan_temp <- subset(loan, loan$annual_inc <= 82300)

# Finding relationship with the annual income average
# it can be seen that the average annual income of lesser than 47589 generally get waived off
# So it can be assumed that loan requests with this range of income has historically been a reason for financial loss 

summary(loan_temp$annual_inc)
avg_income <- loan_temp %>% group_by(loan_status) %>% summarise(avg_income = mean(annual_inc))
ggplot(avg_income, aes(x = loan_status, y = avg_income, label = round(avg_income,0))) + geom_bar(stat = "identity", fill = "deepskyblue") + geom_text(size = 5, vjust = 2) #+ theme_economist_white()


# Finding relationship with DTI
summary(loan$dti)

#not much difference between the Mean & median of the DTI values that we have with us. 
#hence, we can safely take the avg of dti without removing the outliers in this case. Although there is a signifacnt difference between 3rd quartile & max
#ASSUMPTION - according to the dti definition, we have understood that a lower value of DTI is a good indicator for the analysis. 
#However, the higher the DTI, it means by default a lower capability to repay the loan. Hence, we replaced all smaller values to 10.


replace(loan$dti, loan$dti > 0 & loan$dti < 10, 10)

avg_dti <- loan %>% group_by(loan_status) %>% summarise(avg_dti = mean(dti))
ggplot(avg_dti, aes(x = loan_status, y = avg_dti, label = round(avg_dti,2))) + geom_bar(stat = "identity", fill = "violetred3") + geom_text(size = 5, vjust = 2)


#Finding relationship with term and Interest rate
#The current loan portfolio doesn't have any loan with 36 month term. 60 month term is more prone to default
#as compared to 36 month
avg_int_rate <- loan %>% group_by(term) %>% summarise(avg_int_rate = mean(int_rate))
ggplot(avg_int_rate, aes(x = term, y = avg_int_rate, label = round(avg_int_rate, 2))) + geom_bar( stat = "identity", fill = "orchid4") + geom_text(size = 5, vjust = 2)


#Bivariate analysis starts from here 

#interestingly enough, the higher income category people have higher rates of charged off both for 36 month & 60 month term
inc_int_term_agg <- aggregate(int_rate ~ loan_status + inc_cat + term, loan, mean)
View(inc_int_term_agg)
ggplot(inc_int_term_agg, aes(x = term, y = int_rate, fill = inc_cat, label = round(int_rate,2))) + geom_bar(stat = "identity", position = "dodge") + facet_grid(loan_status~.)


#Finding relationship with average Utilization of revolving credit
#Individuals with high utilization of revolving credit are prone to default. Current portfolio average
#revolving credit utiliation is very close to the Charge-off average. Company may choose to offer loan restructring
#to individuls with high ratio. In future lending may be capped at 50% utilization of revolving credit.

summary(loan$revol_util)

avg_revol_util <- loan %>% group_by(loan_status) %>% summarise(avg_revol_util = mean(revol_util))
View(avg_revol_util)
ggplot(avg_revol_util, aes(x = loan_status, y = avg_revol_util, label = round(avg_revol_util, 2))) + geom_bar(stat = "identity", fill = "springgreen4") +geom_text(size = 3, vjust = 2)

#CO Correlation of continuous variables
#revolving line utilization rate & int_rate are highly co -related while there is the most negatively related variable to 
#this is inquiries in last 6 months 
#the number of open credit lines is positively co-related to DTI to the highest degree (this makes sense as the higher the values of DTI, higher debt on the bearer's name. 
#more Credit lines are required to waive off the debt amount)
home_ownership_co <- filter(loan, loan_status == "Charged Off")
home_ownership_fp <- filter(loan, loan_status == "Fully Paid")
home_ownership_curr <- filter(loan, loan_status == "Current")
View(home_ownership_co)
View(home_ownership_fp)
View(home_ownership_curr)





install.packages("GGally")
library(GGally)
View(home_ownership_co)
write_xlsx(home_ownership_co,"home_ownership_co.xlsx")
CO_data <- select(home_ownership_co, 7, 14, 23, 25, 26, 28)
head(CO_data)
cor(CO_data)
ggcorr(CO_data)

##Coorelation between loan amount and funded amount
#there is a positive co-relation between loan_amount & total amount committed by investors for that loan at that point in time.
#which means %increase in funded_amount will tends to %increase in loan_amount.



plot(
  
  loan$loan_amnt,
  
  loan$funded_amnt_inv,
  
  main = "correlation between loan_amnt and funded_amnt_inv",
  
  xlab = "loan_amnt ",
  
  ylab = "funded_amnt_inv ",
  
  pch = 20
  
)





