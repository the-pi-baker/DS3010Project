#setwd("C:/Users/ebake/Downloads/")
setwd("~/Downloads/")
data = read.csv('HappinessAlcoholConsumption.csv')

library(MASS)
library(stats)
library(class)
library(leaps)
library(class)
library(rgl)
library(ggplot2)
library(ISLR)
library(tree)

set.seed(1)
numrows=nrow(data)
numrows
indexes = sample(1:numrows, replace = FALSE)
data = data[indexes,]
training = data[1:(numrows*.8),]
testing = data[-c(1:(numrows*.8)),]
dim(training)
dim(testing)

bestSubset = regsubsets(HappinessScore~HDI + GDP_PerCapita + Beer_PerCapita + Spirit_PerCapita + Wine_PerCapita,training)
bestSummary = summary(bestSubset)
bestSummary

# Plots of the various metrics as a function of subset size
# Small is good
plot(bestSummary$rss)
# Large is good
plot(bestSummary$adjr2)
# Small is good
plot(bestSummary$bic)

# The default plot of the regsubsets command
plot(bestSubset,scale='r2')


forwardSubset = regsubsets(HappinessScore~HDI + GDP_PerCapita + Beer_PerCapita + Spirit_PerCapita + Wine_PerCapita,training, method='forward')
forwardSummary = summary(forwardSubset)
forwardSummary

# Plots of the various metrics as a function of subset size
# Small is good
plot(forwardSummary$rss,type='l')
# Large is good
plot(forwardSummary$adjr2,type='l')
# Small is good
plot(forwardSummary$bic,type='l')

# The default plot of the regsubsets command
plot(forwardSubset,scale='r2')

backwardssubset = regsubsets(HappinessScore~HDI + GDP_PerCapita + Beer_PerCapita + Spirit_PerCapita + Wine_PerCapita,training, method='backward')
backwardsummary = summary(backwardssubset)
backwardsummary
plot(backwardssubset,scale='r2')

model1 = lm(training$HappinessScore~training$HDI)
model2 = lm(HappinessScore~HDI+Spirit_PerCapita,data=training)
model3 = lm(training$HappinessScore~training$HDI+training$Spirit_PerCapita+training$Wine_PerCapita)
model4 = lm(training$HappinessScore~training$HDI+training$Spirit_PerCapita+training$Wine_PerCapita+training$Beer_PerCapita)
model5 = lm(HappinessScore~HDI+Spirit_PerCapita+Wine_PerCapita+Beer_PerCapita+GDP_PerCapita, data=training)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)

prediction_1 = predict(model1,training)
tr_accuracy_1 = mean((training$HappinessScore-prediction_2)^2)
tr_accuracy_1

prediction_2 = predict(model2,training)
tr_accuracy_2 = mean((training$HappinessScore-prediction_2)^2)
tr_accuracy_2

prediction_3 = predict(model3,training)
tr_accuracy_3 = mean((training$HappinessScore-prediction_3)^2)
tr_accuracy_3

prediction_4 = predict(model4,training)
tr_accuracy_4 = mean((training$HappinessScore-prediction_4)^2)
tr_accuracy_4

prediction_5 = predict(model5,training)
tr_accuracy_5 = mean((training$HappinessScore-prediction_5)^2)
tr_accuracy_5

summary(lm(training$HappinessScore~training$GDP_PerCapita+training$HDI))

prediction_2 = predict(model2,testing)
tr_accuracy_2 = mean((testing$HappinessScore-prediction_2)^2)
tr_accuracy_2

prediction_5 = predict(model5,testing)
tr_accuracy_5 = mean((testing$HappinessScore-prediction_5)^2)
tr_accuracy_5


