# Practical concerns: Review and prepare data for model building
# Goal: Identify segments of high and/or upward trending coronary heart disease prevalance

# Load packages ####
library(dplyr)

# Import data ####
# 5 years of seriatim data on heart attack rates by state, year, sex, age, and race
# Data represent a derivative of this dataset: https://www.kaggle.com/mazharkarimi/heart-disease-and-stroke-prevention/metadata 
# Original dataset and its derivatives are protected by a database license and a content license, included in this repository's documents 

heartattack <- readRDS("handson_challenges/heartdiseasedataset_modified.RDS")

# Look at the data ####
head(heartattack)
str(heartattack)

# Challenge ####

## 1) Deal with outlier and/or missing values
# a) Find the number of missing values in each field


# YOUR CODE HERE ####


# b) Are the missing values correlated with any other field? Consider using the table() function for cross-tabulating two categorical variables.


# YOUR CODE HERE ####


# c) Impute the missing values.


# YOUR CODE HERE ####


# 2) Derive a new variable for "geographic region" of USA to reduce dimensionality of that field.


# YOUR CODE HERE ####


# 3) Partition off a 30% holdout subset


# YOUR CODE HERE ####


# 4) Fit logistic regression to estimate heart attack odds given region, year, age, sex, and race


# YOUR CODE HERE ####


# 5) Use vif() function from "car" package to test for multicollinearity in the model below


# YOUR CODE HERE ####

