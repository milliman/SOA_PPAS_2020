# VolModel.R
# Prepared for Virtual Practical Predictive Analytics Seminar hosted by
# the Society of Actuaries in 2020. Contains an implementation of a model
# proposed in the North American Actuarial Journal for equity volatility.
#
# Questions or feedback are welcome via email.
# Marshall Lagani (marshall.lagani@gmail.com)

library(dplyr)
library(lubridate)
library(car)
library(ggplot2)

# Set working directory on your local device to correctly execute code.

getwd()
setwd("C:/Users/marsh/Documents/R/PPAS2020") # path for sample data files

### Pull in volatility data and examine.

VolData <- read.csv("dailycalcs.csv",as.is=TRUE)

class(VolData)
names(VolData)
head(VolData)
tail(VolData)
str(VolData)
summary(VolData)
dim(VolData)

cor(VolData$logVIXprev,VolData$log_sig)
cor(VolData$y_d,VolData$y_u)
cor(VolData$logVIXprev,VolData$y_d)
cor(VolData$logVIXprev,VolData$y_u)

cor(select(VolData, logVIXprev, log_sig, y_d, y_u))

### Hmm, I hope no one is too worried about the fact that
### we do not have independence in our explanatory variables.

form.basic <- as.formula(logVIX ~ logVIXprev + log_sig + y_d + y_u)

# Set up prototype for time series forecast.
VolData_histtr <- VolData[1:400,]
VolData_histts <- VolData[401:484,]
VLM_histtr <- lm(form.basic, VolData_histtr)
summary(VLM_histtr)

# Compute r^2 for the in sample data.
# Note that the result should tie back to the summary function.
pred_is <- predict(VLM_histtr, VolData_histtr)
cor(pred_is, VolData_histtr$logVIX)^2

# Compute r^2 for the out of sample data.
# The high decay in out of sample r^2 lets you see that the model
# suffers from some overfitting. This highlights the difference
# between descriptive and predictive analytics.
pred_oos <- predict(VLM_histtr, VolData_histts)
cor(pred_oos, VolData_histts$logVIX)^2

### Another method to put a train/test split on your data.
### Not necessarily appropriate for time series data.

sample_size = floor(0.8 * nrow(VolData))

set.seed(42)
split <- sample(seq_len(nrow(VolData)), size = sample_size)

train <- VolData[split, ]
test <- VolData[-split, ]

head(train)
tail(train)
str(train)
summary(train)
dim(train)

head(test)
tail(test)
str(test)
summary(test)
dim(test)

VLMtr <- lm(form.basic, train)
summary(VLMtr)

p <- predict(VLMtr, test)

summary(p)

VLMts <- lm(form.basic, test)
summary(VLMts)

MSE.VLMtr <- sum((p - test$logVIX)^2)/nrow(test)
print(MSE.VLMtr)
