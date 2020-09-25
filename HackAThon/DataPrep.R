library(tidyverse)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Data prep
training <- read.csv("training.csv", stringsAsFactors = F)

# A few missing values!
sapply(training, function(x) sum(is.na(x)))

# This missing values are isolated, no row has more than 1
apply(training, 1, function(x) sum(is.na(x))) %>% table()

# Impute with mean or mode
for(i in 1:ncol(training)){
  if(class(training[,i]) %in% c("numeric", "integer")){
    training[,i] <- ifelse(is.na(training[,i]), mean(training[,i], na.rm = T), training[,i])
  } else{
    training[,i] <- ifelse(is.na(training[,i]), Mode(training[,i][!is.na(training[,i])]), training[,i])
  }
}

training <- training %>%
  mutate(result = ifelse(result == "pass", 1, 0))

saveRDS(training, 
        "training_prepped.Rds")


# Data prep (testing) ####
testing <- read.csv("testing.csv", stringsAsFactors = F)

# A few missing values!
sapply(testing, function(x) sum(is.na(x)))

# This missing values are isolated, no row has more than 1
apply(testing, 1, function(x) sum(is.na(x))) %>% table()

# Impute with mean or mode
for(i in 1:ncol(testing)){
  if(class(testing[,i]) %in% c("numeric", "integer")){
    testing[,i] <- ifelse(is.na(testing[,i]), mean(testing[,i], na.rm = T), testing[,i])
  } else{
    testing[,i] <- ifelse(is.na(testing[,i]), Mode(testing[,i][!is.na(testing[,i])]), testing[,i])
  }
}

saveRDS(testing, 
        "testing_prepped.Rds")
