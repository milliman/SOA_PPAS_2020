# BSM_NN.R
# Prepared for Virtual Practical Predictive Analytics Seminar hosted by
# the Society of Actuaries in 2020. Contains an overview of using the neuralnet
# library to fit a neural network model against an example portfolio of options
# priced under Black Scholes.
#
# Questions or feedback are welcome via email.
# Marshall Lagani (marshall.lagani@gmail.com)

library(dplyr)
library(lubridate)
library(car)
library(ggplot2)

# Set working directory on your local device to correctly execute code.

getwd()
setwd("C:/Users/marsh/Documents/R/PPAS2020") #path for sample data files

### Pull in option data and examine.
### The file "BSM.csv" contains option prices computed using the
### Black Scholes model. We will test fitting a neural network model 
### against the Black Scholes prices.

OpData <- read.csv("BSM.csv",as.is=TRUE)

class(OpData)
names(OpData)
head(OpData)
tail(OpData)
str(OpData)
summary(OpData)
dim(OpData)

### Initialize neural network library.

library(neuralnet) #Neural Network Library
?neuralnet
??neuralnet

### Specify formula type and calibrate. You can experiment with different node
### structures by changing the setting input 'hidden' below. Note that the
### call to neuralnet will take a long time to run. Your runtime will vary
### based on your own hardware (primarily CPU and RAM).

c(5,3)

f <- as.formula("Value ~ S + K + r + t + vol")
nn <- neuralnet(f,OpData,hidden=c(5,3),linear.output = TRUE)

### Plot neural network.

plot(nn)

### Looking at the plot, we see an enormous error quote. This turns out to be
### a common issue with fitting neural network models. You must have scaled
### data to get these algorithms to converge to a meaningful result.

### Normalize data set. Allows for convergence in NN fitting.

max = apply(OpData,2,max)
min = apply(OpData,2,min)
scaled = as.data.frame(scale(OpData, center = min, scale = max - min))

# Fit another neural network on the scaled data.

nn2 <- neuralnet(f,scaled,hidden=c(5,3),threshold=0.05)
plot(nn2)

### Compute is the neuralnet version of predict.
### It requires matching a dataset with only input dimensions,
### which we can obtain by trimming column 6 from our dataset.

p1 <- compute(nn2,scaled[,-6])$net.result

cor(p1,scaled$Value)^2

### We waited a long time and while it does well, we might try to compare to
### some simpler approaches. Let's compare to a GLM and a simpler neural
### network model for some examples.

BSM_GLM <- glm(f, family = gaussian, OpData)
summary(BSM_GLM)

p2 <- predict.glm(BSM_GLM, OpData)

cor(p2, OpData$Value)^2

### The more straightforward GLM code does reasonably well, but not as well
### as the more complicated neural network. What about a basic neural network?

nnbasic <- neuralnet(f,scaled,threshold=0.05)
plot(nnbasic)

p <- compute(nnbasic,scaled[,-6])$net.result

cor(p,scaled$Value)^2

### A simpler neural network gets a better answer than the GLM, and
### it seems to be comparable to the more complicated network.
### For the sake of parsimony, let's go with the simpler network structure and
### split the data into train/test sets to measure out of sample performance.

sample_size = floor(0.8 * nrow(OpData))

# Setting a fixed seed will allow us to generate a reproducible result.
set.seed(42)

# Create split parameter and split data set.
split <- sample(seq_len(nrow(scaled)), size = sample_size)
Optrain <- scaled[split, ]
Optest <- scaled[-split, ]

# Train another neural network using simplified structure.
BSM_NN <- neuralnet(f,Optrain,threshold=0.05)
plot(BSM_NN) # Very similar to before.

# Look at correlations in sample and out of sample.
score_is <- compute(BSM_NN,Optrain[,-6])$net.result
cor(score_is,Optrain$Value)^2
score_oos <- compute(BSM_NN,Optest[-6])$net.result
cor(score_oos,Optest$Value)^2

# Look at a couple of error metrics.
# This line computes the absolute mean squared error on the training set.
# You should get 0.0003817584.
sum((score_is - Optrain$Value)^2) / nrow(Optrain)

# This line computes the relative mean squared error on the training set.
# You should get 0.001941489, reflecting a 0.19% MSE. Not bad.
sum((score_is - Optrain$Value)^2) / sum(Optrain$Value)

# The valuable empirical evidence comes from the out of sample testing.
# Executing the lines below should yield an absolute MSE of 0.0003750524
# and 0.001896219 for the MSE, again reflecting a 0.19% error. Nice!
sum((score_oos - Optest$Value)^2) / nrow(Optest)
sum((score_oos - Optest$Value)^2) / sum(Optest$Value)

### We have developed a neural network model and passed a first fitting test.