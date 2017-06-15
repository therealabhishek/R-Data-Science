
# Reading File

hr<- read.csv('HR_comma_sep.csv',header = TRUE,stringsAsFactors = FALSE)

# Reorder

hr<- hr[,c(1,2,3,4,5,6,8,9,10,7)]
View(hr)

# Structure

str(hr)

# Summary

summary(hr)

# Changing integer data to numeric data types

# number_project
hr$number_project<- as.numeric(hr$number_project)


# average_montly_hours
hr$average_montly_hours<- as.numeric(hr$average_montly_hours)

# time_spend_company
hr$time_spend_company<- as.numeric(hr$time_spend_company)

# Work_accident
hr$Work_accident<- as.numeric(hr$Work_accident)

# promotion_last_5years
hr$promotion_last_5years<- as.numeric(hr$promotion_last_5years)

# left
hr$left<- as.numeric(hr$left)

# Check str

str(hr)

# Changing categorical variables to numeric variables

# Sales

unique(hr$sales)

hr$sales<- factor(hr$sales,
                  levels = c("sales","accounting","hr","technical","support","management","IT","product_mng","marketing","RandD"),
                  labels = c(1,2,3,4,5,6,7,8,9,10))


# Salary

unique(hr$salary)

hr$salary<- factor(hr$salary,
                   levels = c("low","medium","high"),
                   labels = c(0,1,2))

# Structure

str(hr)


# Splitting the data into train and test sets

#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(hr$left, SplitRatio = 0.8)
training_hr = subset(hr, split == TRUE)
test_hr = subset(hr, split == FALSE)


# Exploratory Data Analysis

library(ggplot2)


# Summary of people leaving based on Salary and Satisfaction Level

ggplot(training_hr,aes(x = salary,y = satisfaction_level,fill = factor(left)))+
  geom_boxplot()+
  xlab("Salary Level")+
  ylab("Satisfaction Level")+
  ggtitle("Summary of people leaving based on Salary and Satisfaction Level ")
  
# People with low satisaction level are most likely to leave

#-------------------------------------------------------------------------------

# Summary of people leaving based on Salary and last evaluation

ggplot(training_hr,aes(x = salary,y = last_evaluation, fill = factor(left)))+
  geom_boxplot()+
  xlab("Salary Level")+
  ylab("Last Evaluation")+
  ggtitle("Summary of people leaving based on Salary and Last Evaluation")

# People with low last evaluation are most likely to leave

#-------------------------------------------------------------------------------

# Summary of people leaving based on Salary and number of projects

ggplot(training_hr,aes(x = salary,y = number_project, fill = factor(left)))+
  geom_boxplot()+
  xlab("Salary Level")+
  ylab("Number Project")+
  ggtitle("Summary of people leaving based on Salary and Number of Project")

# We can see that people having worked on less projects at all salary levels except high tend to leave


#-------------------------------------------------------------------------------

# Summary of people leaving based on Salary and average working hours

ggplot(training_hr,aes(x = salary,y = average_montly_hours, fill = factor(left)))+
  geom_boxplot()+
  xlab("Salary Level")+
  ylab("Average Monthly Hours")+
  ggtitle("Summary of people leaving based on Salary and Average Working hours")

# People spending less time in the company tend to leave

#-------------------------------------------------------------------------------

# Summary of people leaving based on time spent in the company


ggplot(training_hr,aes(x = factor(time_spend_company),fill = factor(left)))+
  geom_bar(stat = "count")+
  xlab("Time Spent in company")+
  ylab("Count")+
  ggtitle("Summary of people leaving based on Time Spent in the company")

# People who have spent 2 yrs  or less and more than 6 yrs almost certainly leave

#-------------------------------------------------------------------------------

# Summary of people leaving based on time spent in the company and satisfaction level


ggplot(training_hr,aes(x = factor(time_spend_company),y = satisfaction_level ,fill = factor(left)))+
  geom_boxplot()+
  xlab("Time Spent in company")+
  ylab("Satisfaction Level")+
  ggtitle("Summary of people leaving based on Time Spent in the company and Satisfaction Level")

# In most cases, the satisfaction level does not matter, people with time spent 2 yr or less
# and more than 6 yrs leave

#-------------------------------------------------------------------------------

# Summary of people leaving based on work accidents

ggplot(training_hr,aes(x = factor(Work_accident),fill = factor(left)))+
  geom_bar(stat = "count")+
  xlab("Work Accidents")+
  ylab("Count")+
  ggtitle("Summary of people leaving based on Work Accidents")

# Looks like % of people are little more likely to leave who have accidents 


#-------------------------------------------------------------------------------

# Summary of people leaving based on promotions

ggplot(training_hr,aes(x = factor(promotion_last_5years),fill = factor(left)))+
  geom_bar(stat = "count")+
  xlab("Promotion in last 5 years")+
  ylab("Count")+
  ggtitle("Summary of people leaving based on Promotion")

# Looks like % of people who are leaving does not depend on promotion 

#-------------------------------------------------------------------------------

# Summary of people leaving based on domain they work in

ggplot(training_hr,aes(x = factor(sales),fill = factor(left)))+
  geom_bar(stat = "count")+
  xlab("Domain")+
  ylab("Count")+
  ggtitle("Summary of people leaving based on working Domain")

# People working in the areas of sales, technical and support are more likely to leave

#-------------------------------------------------------------------------------

# Summary of people leaving based on their salary

ggplot(training_hr,aes(x = factor(salary),fill = factor(left)))+
  geom_bar(stat = "count")+
  xlab("Salary")+
  ylab("Count")+
  ggtitle("Summary of people leaving based on Salary")

# % of People who are leaving is independent of salary

#-------------------------------------------------------------------------------





