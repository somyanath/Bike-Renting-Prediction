# Clear the environment
rm(list = ls())

#importing the required libraries
library("ggplot2")
library("corrgram")
library("randomForest")
library("C50")

# Load the file
bike_data = read.csv("day.csv")
date_data = bike_data$dteday

# Explore the data
str(bike_data)

###### univariate analysis #####
# Convert the dteday column into date format
bike_data$dteday = as.Date(bike_data$dteday)

# Convert the variables from integer class to factor
# As, it is better to convert the variable's data
# in the form of a factor as it improves the efficiency of the code
bike_data$season = as.factor(bike_data$season)
bike_data$yr = as.factor(bike_data$yr)
bike_data$mnth = as.factor(bike_data$mnth)
bike_data$holiday = as.factor(bike_data$holiday)
bike_data$weekday = as.factor(bike_data$weekday)
bike_data$workingday = as.factor(bike_data$workingday)
bike_data$weathersit = as.factor(bike_data$weathersit)

###### Pre-Proecessing #####

# Check for missing values
sapply(bike_data, function(x) sum(is.na(x)))
# SO there are no missing values in the dataset

# Drop the unimportant variables which does not contribute to the model at all
# As the instant variable is just row index
bike_data$instant = NULL

# Here, we are separating the numerical variables
numeric_index = sapply(bike_data, is.numeric)
#This selects only those columns which are numeric

numeric_data = bike_data[,numeric_index]
#This transfers the data of the columns which are numeric

cnames = colnames(numeric_data)
cnames = cnames[-7]

# We can also, plot the probability density functions of the numerical variables
# to check their distribution and also skewness
old_par = par("mar")
par(mar = c(1,1,1,1))
library(psych)
multi.hist(bike_data[,sapply(bike_data, is.numeric)], main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")
par(mar = old_par)

###### Outlier analysis #####
#Boxplots - Distribution & Outlier Check

# Checking the presence of outliers in the variables w.r.t to a suitable variable
#Box-plot
#Here continuous var is on y-axis.
library("scales")
ggplot(bike_data, aes_string(x = bike_data$season, y = bike_data$temp)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Season") + ylab('Temperature') +ggtitle("Outlier Analysis")

ggplot(bike_data, aes_string(x = bike_data$season, y = bike_data$atemp)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Season") + ylab('Feeling Temperature') +ggtitle("Outlier Analysis")

ggplot(bike_data, aes_string(x = bike_data$weathersit, y = bike_data$hum)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Weather Situation") + ylab('Humidity') +ggtitle("Outlier Analysis")

ggplot(bike_data, aes_string(x = bike_data$weathersit, y = bike_data$windspeed)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Weather Situation") + ylab('Windspeed') + ggtitle("Outlier Analysis")

ggplot(bike_data, aes_string(x = bike_data$mnth, y = bike_data$casual)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Month") + ylab('Count of Casual Riders') + ggtitle("Outlier Analysis")

ggplot(bike_data, aes_string(x = bike_data$mnth, y = bike_data$registered)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Month") + ylab('Count of Registered Riders') + ggtitle("Outlier Analysis")


#Repalce all the outliers with NA and Impute
#Create NA on custAge
for(i in cnames){
  val = numeric_data[,i][numeric_data[,i] %in% boxplot.stats(numeric_data[,i])$out]
  numeric_data[,i][numeric_data[,i] %in% val] = NA
}

library("DMwR")
numeric_data = knnImputation(numeric_data, k = 3)
summary(numeric_data$hum)
summary(numeric_data$casual)
summary(numeric_data$windspeed)

# Now replacing the imputed data back into the data
summary(bike_data$hum)
summary(bike_data$casual)
summary(bike_data$windspeed)
summary(bike_data$cnt)

bike_data$hum = numeric_data$hum
bike_data$windspeed = numeric_data$windspeed
bike_data$casual = numeric_data$casual
bike_data$cnt = bike_data$casual + bike_data$registered

# Now plotting the probabilisty density functions after performing the Outlier Analysis
old_par = par("mar")
par(mar = c(1,1,1,1))
library(psych)
multi.hist(bike_data[,sapply(bike_data, is.numeric)], main = NA, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")
par(mar = old_par)

# Also, plotting the boxplots after performing Outlier Analysis
ggplot(bike_data, aes_string(x = bike_data$weathersit, y = bike_data$hum)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Weather Situation") + ylab('Humidity') +ggtitle("Outlier Analysis")

ggplot(bike_data, aes_string(x = bike_data$weathersit, y = bike_data$windspeed)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Weather Situation") + ylab('Windspeed') + ggtitle("Outlier Analysis")

ggplot(bike_data, aes_string(x = bike_data$mnth, y = bike_data$casual)) +
  geom_boxplot(outlier.color = "red", outlier.size = 3) + theme_bw() +
  xlab("Month") + ylab('Count of Casual Riders') + ggtitle("Outlier Analysis")


##### Feature Selection #####

# Creating the correlation analysis plot to analyse the dependecies of numeric
# variables
library(corrplot)
cor_plot = cor(bike_data[,sapply(bike_data, is.numeric)])
corrplot(cor_plot, method = "color", addgrid.col = "darkgray", addCoef.col = "black",
         outline = TRUE, number.digits = 2)
# The correlation plot tells us that the numerical variables "atemp" - "temp" and "registered"-"cnt"
# are correlated. So, we can drop "atemp" variable. As "cnt" is our target variable
# we want the independant variables to have high correlation with it.

# Now, we'll plot various visualizations to see the variation of a variable w.r.t other variable
# First, plotting the variation of the casual bike count w.r.t yr and season
ggplot(data=bike_data,aes(x= bike_data$yr, y = bike_data$casual, fill = season)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() +
  labs(x="Year", y="Total number of bikes rented by casual users")

# Second, plotting the variation of the casual bike count w.r.t yr and season
ggplot(data=bike_data,aes(x= bike_data$yr, y = bike_data$registered, fill = season)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() + 
  labs(x="Year", y="Total number of bikes rented by registered users")

# Third, plotting the variation of the total bike count w.r.t the yr and season
ggplot(data=bike_data,aes(x= bike_data$yr, y = bike_data$cnt, fill = season)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() + 
  labs(x="Year", y="Total number of bikes rented")
# On the basis of these 3 plots we can say surely say few things:
# 1. There is an increase in bike renting from year 2011 to year 2012
# 2. Bikes are least rented in Spring season and most rented in Fall season
# 3. There is a sudden increase in bike renting during summer


# Plotting to check which day has the highest number of rents
ggplot(data=bike_data,aes(x= bike_data$weekday, y = bike_data$cnt)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() + 
  labs(title="Total Number of bikes rented per day", x="Day", y="Total number of bikes rented")

# Variation of casual renting w.r.t the weekday
ggplot(data=bike_data,aes(x= bike_data$weekday, y = bike_data$casual)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() + 
  labs(title="Casual Number of bikes rented per Day", x="Weekday", y="Casual rented bikes")
# So, there are more casual users on the weekends

# Variation of registered renting w.r.t the weekday
ggplot(data=bike_data,aes(x= bike_data$weekday, y = bike_data$registered)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() + 
  labs(title="Registered Number of bikes rented per day", x="Weekday", y="Registered rented bikes")
# So, there are more registered users on the weekdays
# The distribution of count is similar for every day of the week, so if there is a little
# dependency between the weekday and any other dependent variable then we can drop weekday variable

# Plotting to check which month has the highest number of rents also with the variation of the weather
ggplot(data=bike_data,aes(x= bike_data$mnth, y = bike_data$cnt, fill = weathersit)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme_classic() + 
  labs(title="Number of bikes rented per month", x="Month", y="Total number of bikes rented")
# On the basis of the abaove plot we can say that the bikes are most rented when the weather is 1(Clear, Few Clouds)
# and least bikes are rented when the weather is 3 and no bikes are rented when the weather is 
# 4(Heavy Rain + Ice Pallets + Thunderstorm)

# So lets check the dependency between the month and weather situation
ggplot(data=bike_data,aes(x= bike_data$mnth, y = bike_data$temp)) + 
  geom_point(stat = "identity", aes(color = weathersit)) + 
  labs(title="Variation of weather w.r.t temperature", x="Weather", y="Temperature")
# High temperatures are obtained in the month of 6,7 & 8 which is obvious. Also, there is a high probability
# of 1 weather situation in every month but the probability of 3 weather situation is high
# in the month of 4,5,6,7 & in 10, 11, 12.

# Plotting to check the number of rents w.r.t variation of the temperature
ggplot(data=bike_data,aes(x= bike_data$temp, y = bike_data$cnt)) + 
  geom_point(aes(color = mnth)) + 
  theme_classic() + 
  labs(title="Number of bikes rented w.r.t temperature", x="Temperature", y="Total number of bikes rented")
# On the basis of the above plot we can surely say that 
# more bikes are rented when the temperature is high.

#Chi-Squared test of independence
factor_index = sapply(bike_data, is.factor)
#In the previous we selected only the categorical variables

factor_data = bike_data[,factor_index]

for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$season, factor_data[,i])))
}


for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$yr, factor_data[,i])))
}

for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$mnth, factor_data[,i])))
}


for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$holiday, factor_data[,i])))
}

for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$weekday, factor_data[,i])))
}

for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$workingday, factor_data[,i])))
}

for (i in 1:7){
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$weathersit, factor_data[,i])))
}
# According, to the chi-square test the "mnth" variable has very high dependency with "season" and "weekday" variable
# has very high dependency with "workingday". Thus they can be dropped.

#Dimension reduction
data_del = subset(bike_data, select = -c(dteday, mnth, weekday, atemp))


# Clear the environment except data_del
library(DataCombine)
rmExcept(c("data_del","bike_data", "date_data"))

##### MODEL DEVELOPMENT ####
data_model = data_del
data_model$date = date_data

# Divide the data into train and test using stratified sampling method
# createdatapartition is one more function used to create stratified samples
# Here in createdatapartiotion function 1st arg is of the reference variable on the basis
# of which splitting will take place, 2nd arg is the percentage of observation we need
# in our sample, list = false implies that there will no repetitive observations
library("caret")
set.seed(1234)
train.index = createDataPartition(data_model$cnt, p = 0.75, list = FALSE)
train = data_model[train.index,]
test = data_model[-train.index,]

# Drop the unimportant variables which does not contribute to the model at all
# As the instant variable is just row index
train$date = NULL

##### LINEAR REGRESSION #####
# Linear Regression
# lm is built-in function which helps us to build linear regression on top of the dataset
LR_model = lm(cnt ~ ., data = train)
LR_model_2 = lm(cnt ~ season + yr + holiday + workingday + weathersit + temp + hum + 
                  windspeed + casual, data = train)
LR_model_3 = lm(cnt ~ season + yr + holiday + workingday + weathersit + temp + hum + 
                  windspeed, data = train)
LR_model_4 = lm(cnt ~ season + yr + holiday + workingday + weathersit + temp + hum + 
                  windspeed + registered, data = train)
# We have build 4 models as we are not sure regarding "casual" and "registered" variable
# dependency with each other 

# stepwise model selection using AIC
LR_model_aic = step(LR_model, direction = "both")
LR_model_2_aic = step(LR_model_2, direction = "both")
LR_model_3_aic = step(LR_model_3, direction = "both")
LR_model_4_aic = step(LR_model_4, direction = "both")
# So, according to the AIC our model_4 is the best model. Also, the AIC test has dropped "yr" and "casual"
# variable from the model

# Summary of the model
summary(LR_model_4_aic)

# The logistic regression test's result after performing AIC on it to validate the best method
# suggests us that the most important variables are "temp", "workingday - 1" and "registered" followed
# by "season -2".

#predict using linear regression
LR_predictions = predict(LR_model_4_aic, newdata = test)

# RMSE
library("Metrics")
test_lr_rmse = rmse(test$cnt, LR_predictions)
print(test_lr_rmse)

# Prediction summary
summary(LR_predictions)

#Summary of actual values
summary(test$cnt)
hist(LR_predictions)
hist(test$cnt)
# From the above summary we can see the minimum value of the predicted count is very less as
# compared to that of the actual bike rental count. But apart from that if we check the distribution
# of the predicted count and actual count there is not much deviation. So, we can opt to go with this
# model but we'll try some more complex models before we settle down on one

# Save the results
results <- data.frame(date = test$date, count = LR_predictions)

# Write the results to a csv file
write.csv(results, file = 'LR_predictions.csv', row.names = FALSE, quote=FALSE)


###### Random Forest #####
# In this we'll populate only the important variables as indicated to us by AIC
RF_model = randomForest(cnt ~ season + holiday + workingday + weathersit + 
                          temp + hum + windspeed + registered, train, importance = TRUE, ntree = 500)
print(RF_model)
plot(RF_model)

#predict using random forest
RF_predictions = predict(RF_model, newdata = test)

# RMSE
test_rf_rmse = rmse(test$cnt, RF_predictions)
print(test_rf_rmse)

# Prediction summary
summary(RF_predictions)

#Summary of actual values
summary(test$cnt)
hist(RF_predictions)
hist(test$cnt)
# From the above summary we can see the values of the predicted count are very much deviated from the actual values

# Save the results
results <- data.frame(date = test$date, count = RF_predictions)

# Write the results to a csv file
write.csv(results, file = 'RF_predictions.csv', row.names = FALSE, quote=FALSE)

# So, on the basis of the summary and RMSE we can say that the Linear Rgression model is much more accurate than
# the Random Forest model.