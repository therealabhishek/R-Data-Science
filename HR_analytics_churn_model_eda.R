
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

#---------------------------------

# Logistic Regression- Model 1

#---------------------------------

# Fitting the logistic regression model to the training set.

classifier1<- glm(formula = left~.,
                  family = binomial,
                  data = training_hr)

summary(classifier1)



# Predicting the results on the test set

prob_pred<- predict(classifier1,type = 'response',newdata = test_hr[-10])
prob_pred


y_pred<- ifelse(prob_pred>0.5,1,0)
y_pred

# Making the confusion matrix

cm<- table(test_hr[,10],y_pred > 0.5)
cm

# Lets check the model accuracy

1-sum(diag(cm))/ sum(cm)

# The current logistic regression gives an accuracy of 78.84% 

# We have calculated the accuracy of the first model without treating multi-collinearity
# Lets calculate the multi-collinearity and eliminate the variables with severe multi-collinearity
# i.e (vif > 10)

library(faraway)
vif(classifier1)

# classifier names(vif < 10)
# satisfaction_level,last_evaluation,average_montly_hours,time_spend_company,sales,salary


#---------------------------------

# Logistic Regression- Model 2

#---------------------------------

# We will build the model using the following variables which have less  multi-collinearity(vif<10).

# satisfaction_level,last_evaluation,average_montly_hours,time_spend_company,sales,salary

# Fitting Logistic Regression Model to the training set

training_hr_n1<- training_hr[,c(-3,-6,-7)]


classifier2<- glm(formula = left~.,
                  family = binomial,
                  data = training_hr_n1)

summary(classifier2)

# Predicting the results on the test set

test_hr_n1<- test_hr[,c(-3,-6,-7)]

prob_pred1<- predict(classifier2,type = 'response',newdata = test_hr_n1[-7])
prob_pred1


y_pred1<- ifelse(prob_pred1>0.5,1,0)
y_pred1

# Making the confusion matrix

cm1<- table(test_hr_n1[,7],y_pred1 > 0.5)
cm1

# Lets check the model accuracy

1-sum(diag(cm1))/ sum(cm1)

# We get the same accuracy(76.46%) after removing the 3 multi-collinear variables
# Perhaps the variables should not be removed



#---------------------------------

# Logistic Regression- Model 3

#---------------------------------

# Removing the variables which are insignificant and building another model

# variables to be considered
# satisfaction_level,average_montly_hours,time_spend_company,sales,salary

training_hr_n2 <- training_hr[,c(-2,-3,-6,-7)]

classifier3<- glm(formula = left~.,
                  family = binomial,
                  data = training_hr_n2)

summary(classifier3)


# Predicting the results on the test set

test_hr_n2<- test_hr[,c(-2,-3,-6,-7)]

prob_pred2<- predict(classifier3,type = 'response',newdata = test_hr_n2[-6])
prob_pred2


y_pred2<- ifelse(prob_pred2>0.5,1,0)
y_pred2

# Making the confusion matrix

cm2<- table(test_hr_n2[,6],y_pred2 > 0.5)
cm2

# Lets check the model accuracy

1-sum(diag(cm2))/ sum(cm2)

# We get reduced accuracy(76.54%) after removing the insignificant vairables



#-------------------------------------------------------------

# Using a different classification algorithm - DECISION TREES

#-------------------------------------------------------------


# Lets try the Decision Tree model to see if we get any change in accuracy

library(rpart)

# Training the model

classifier4<- rpart(formula = left~.,
                    data = training_hr)


# Predicting the Test set results

prob_pred3 = predict(classifier4, newdata = test_hr[-10], type = 'vector')
prob_pred3

y_pred_dt<- ifelse(prob_pred3>0.5,1,0)
y_pred_dt


# Making the confusion matrix

cm3<- table(test_hr[,10],y_pred_dt)
cm3

# Calculating Accuracy

1-sum(diag(cm3))/ sum(cm3)

# Using the decision trees we get a predictive accuracy of 96.97%


#-------------------------------------------------------------

# Using a different classification algorithm - RANDOM FOREST

#-------------------------------------------------------------


library(randomForest)

set.seed(123)

classifier5<- randomForest(x = training_hr[,-10],
                           y = factor(training_hr$left),
                           ntree = 300)



prob_pred4 <-  predict(classifier5, newdata = test_hr[-10])
prob_pred4


cm4<- table(test_hr[,10],prob_pred4)
cm4

# Calculating Accuracy

1-sum(diag(cm4))/ sum(cm4)

# Using the random forest we get a predictive accuracy of 99.10%

#------------------------------------------------------------------------#
