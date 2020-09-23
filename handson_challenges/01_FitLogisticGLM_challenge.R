# Fit a logistic GLM in R

# Load libraries ####
library(car)

# Import data ####

# from the "car" package datasets
titanic <- TitanicSurvival 

# or from the repository
titanic <- read.csv("handson_challenges/TitanicSurvival.csv",
                    row.names = 1)

# Look at the data
head(titanic)
summary(titanic)

# Challenge ####

# 1) Using the glm() function, build a logistic GLM to predict/estimate passenger survival ("survived")
# using the predictors "sex", "age", and "passengerClass".


# YOUR CODE HERE ####


# 2) Output a summary of the model.


# YOUR CODE HERE ####


# Questions:
# a) Which predictor variables are statistically significant at the 5% level of significance?
# b) How can you interpret the coefficient values? 
# c) Which predictor variables are the most materially significant?



# 3) Extra: Test some reasonable variable interactions in the model.


# YOUR CODE HERE ####
