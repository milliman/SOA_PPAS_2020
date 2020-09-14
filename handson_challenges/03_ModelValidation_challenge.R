# Fit a logistic GLM in R

# Load libraries ####
library(dplyr)
library(ROCR)

# Import data/model (from challenge 2) ####
# Only if needed! Feel free to use your own objects from challenge #2.

heartattack <- readRDS("02_heartdiseasedataset.RDS")
logistic.model <- readRDS("02_SampleLogisticModel.RDS")

# Challenge ####

# 1) Use the predict() function to create a column of probability predictions in the dataset.


# YOUR CODE HERE ####


# 2) Create A/E plots using the holdout subset.

# a) Across a numeric variable like "Year" (i.e. Year on the x-axis)


# YOUR CODE HERE ####


# b) Across a categorical variable like "Race" (i.e. Race on the x-axis)


# YOUR CODE HERE ####


# 3) Calculate the AUC on the holdout data.


# YOUR CODE HERE ####


# 4) Extra: Create a second model and construct a two-way lift chart.


# YOUR CODE HERE ####
