# Model tuning
library(tidyverse)
library(xgboost)
library(MLmetrics)
training <- readRDS("training_prepped.Rds")
testing <- readRDS("testing_prepped.Rds")

mod.mat <- model.matrix( ~ .,
                        data = training %>% select(-result))
response <- training$result
d.mat <- xgb.DMatrix(data = mod.mat, label = response)

mod.mat.testing <- model.matrix( ~ .,
                         data = testing)
d.mat.testing <- xgb.DMatrix(data = mod.mat.testing)

tune.grid <- expand.grid(eta = seq(0.005, 0.01, 0.0025),
                         max_depth = c(3:6),
                         subsample = seq(0.7, 1, 0.1),
                         colsample_bytree = seq(0.5, 0.8, 0.1),
                         min_child_weight = 1:3,
                         gamma = 0:1)

# This takes 1-2 hours to run !!!!
results <- data.frame()
timer <- proc.time()
for(i in 1:nrow(tune.grid)){
  params <- list(booster = "gbtree",
                 objective = "binary:logistic",
                 eta = tune.grid$eta[i],
                 max_depth = tune.grid$max_depth[i],
                 min_child_weight = tune.grid$min_child_weight[i],
                 subsample = tune.grid$subsample[i],
                 colsample_bytree = tune.grid$colsample_bytree[i],
                 gamma = tune.grid$gamma[i],
                 nthread = 6) 
  
  temp.xgb <- xgb.cv(params = params,
                     data = d.mat,
                     nrounds = 5000,
                     nfold = 20,
                     metrics = list("logloss", "rmse", "auc", "error"),
                     print_every_n = 100,
                     early_stopping_rounds = 20)
  
  results <- bind_rows(results,
                       data.frame(model_num = i,
                                  iter = temp.xgb$best_iteration,
                                  test_logloss = temp.xgb$evaluation_log$test_logloss_mean[temp.xgb$best_iteration],
                                  test_rmse = temp.xgb$evaluation_log$test_rmse_mean[temp.xgb$best_iteration],
                                  test_auc = temp.xgb$evaluation_log$test_auc_mean[temp.xgb$best_iteration],
                                  test_error = temp.xgb$evaluation_log$test_error_mean[temp.xgb$best_iteration]))
}

proc.time() - timer

top.models <- tune.grid %>% 
  bind_cols(results) %>%
  arrange(test_error) %>%
  head(20)

# Fit top models in list ####
models <- list()
for(i in 1:nrow(top.models)){
  
  params <- list(booster = "gbtree",
                 objective = "binary:logistic",
                 eta = tune.grid$eta[i],
                 max_depth = tune.grid$max_depth[i],
                 min_child_weight = tune.grid$min_child_weight[i],
                 subsample = tune.grid$subsample[i],
                 colsample_bytree = tune.grid$colsample_bytree[i],
                 gamma = tune.grid$gamma[i],
                 nthread = 6) 
  
  models[[i]] <- xgb.train(params = params,
                           data = d.mat,
                           nrounds = results$iter[i],
                           print_every_n = 50)
}

# Form predictions on training and testing as average of top models ####
training[["preds"]] <- sapply(models, function(x){
  predict(x, d.mat, type = "response")
}) %>%
  apply(., 1, mean)

testing[["preds"]] <- sapply(models, function(x){
  predict(x, d.mat.testing, type = "response")
}) %>%
  apply(., 1, mean)

# Test F1 score at various thresholds ####
F1 <- c()
i <- 1
thold <- seq(0.45, 0.55, 0.01) 
for(threshold in thold){
  if(sum(training$preds > threshold) > 0 & sum(training$preds < threshold) > 0){
    F1[i] <- F1_Score(y_true = training$result, 
                      y_pred = ifelse(training$preds > threshold, 1, 0),
                      positive = "1")
  } else{
    F1[i] <- NA
  }
  
  i <- i + 1
}

(F1.scores <- data.frame(thold, F1))

F1.thold <- F1.scores$thold[F1.scores$F1 == max(F1.scores$F1)][1]

# Apply predictions to testing ####
testing_submission <- data.frame(preds = ifelse(testing$preds > F1.thold, 1, 0))
write.csv(testing_submission,
          file = "testing_submission_kullowatz.csv",
          row.names = F)


# Fit next-best models in list ####
top.models2 <- tune.grid %>% 
  bind_cols(results) %>%
  arrange(test_error) %>%
  filter(row_number() %in% c(21:40))

models2 <- list()
for(i in 1:nrow(top.models2)){
  
  params <- list(booster = "gbtree",
                 objective = "binary:logistic",
                 eta = tune.grid$eta[i],
                 max_depth = tune.grid$max_depth[i],
                 min_child_weight = tune.grid$min_child_weight[i],
                 subsample = tune.grid$subsample[i],
                 colsample_bytree = tune.grid$colsample_bytree[i],
                 gamma = tune.grid$gamma[i],
                 nthread = 6) 
  
  models2[[i]] <- xgb.train(params = params,
                           data = d.mat,
                           nrounds = results$iter[i],
                           print_every_n = 50)
}

# Form predictions on training and testing as average of top models ####
training[["preds2"]] <- sapply(models2, function(x){
  predict(x, d.mat, type = "response")
}) %>%
  apply(., 1, mean)

testing[["preds2"]] <- sapply(models2, function(x){
  predict(x, d.mat.testing, type = "response")
}) %>%
  apply(., 1, mean)

# Test F1 score at various thresholds ####
F1.2 <- c()
i <- 1
thold.2 <- seq(0.45, 0.55, 0.01) 
for(threshold in thold.2){
  if(sum(training$preds2 > threshold) > 0 & sum(training$preds2 < threshold) > 0){
    F1.2[i] <- F1_Score(y_true = training$result, 
                      y_pred = ifelse(training$preds2 > threshold, 1, 0),
                      positive = "1")
  } else{
    F1.2[i] <- NA
  }
  
  i <- i + 1
}

(F1.scores.2 <- data.frame(thold.2, F1.2))

F1.thold.2 <- F1.scores.2$thold.2[F1.scores.2$F1.2 == max(F1.scores.2$F1.2)][1]

# Apply predictions to testing ####
testing_submission_2 <- data.frame(preds = ifelse(testing$preds2 > F1.thold.2, 1, 0))
write.csv(testing_submission_2,
          file = "testing_submission_kullowatz_2.csv",
          row.names = F)
