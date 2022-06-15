
# About this script -------------------------------------------------------

# Timeslice dataset are loaded and encoded into factors
# Each has a logistic regression model trained on it with a binary outcome of admitted or discharged

# Variables are scaled first - because, according toHastie,Tibshirani and Friedman points out (page 82 of the pdf or at page 63 of the book)
# The ridge solutions are not equivariant under scaling of the inputs, and so one normally standardizes the inputs before solving.
# https://stats.stackexchange.com/questions/48360/is-standardization-needed-before-fitting-logistic-regression

# For each step the timeslices are handled in loops which iterate through each timeslice in turn

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)

# for mlr3
library(mlr3)

library(mlr3learners)
library(mlr3proba)
library(GGally)
library(precrec)
library(paradox)
library(mlr3tuning)
library(mlr3fselect)
library(mlr3misc)
library(MLmetrics)
library(glmnet)



# Load functions ----------------------------------------------------------


# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")


# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")

#
# add utils file which is shared by real-time app
# using this here will ensure that the real-time app and the ML models use the same locations
source("EDcrowding/real-time/app/utils.R")

# set date of file to include


# Create functions ---------------------------------------------------------



save_results_lr <- function(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, 
                            tsk_ids, tuning_round, scores, model_features, st_err_increment = NA, ts_test, labels) {
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # get predictions
  # according to document in base glmnet here https://cran.r-project.org/web/packages/glmnet/vignettes/glmnet.pdf you can set this:
  # predict(cvfit, newx = x[1:5,], s = "lambda.min")
  # not sure how to do this in mlr3
  
  # save results on validation set
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  
  score = data.table(
    timeslice = name_tsk,
    tsk_ids = "val",
    model_features = model_features,
    tuning_round = tuning_round,
    logloss = pred_val$score(msr("classif.logloss")),
    bbrier = pred_val$score(msr("classif.bbrier")), # binary brier score
    prauc = pred_val$score(msr("classif.prauc")),
    auc = pred_val$score(msr("classif.auc")),
    acc = pred_val$score(msr("classif.acc")),
    tp = pred_val$score(msr("classif.tp")),
    fp = pred_val$score(msr("classif.fp")),
    fn = pred_val$score(msr("classif.fn")),
    tn = pred_val$score(msr("classif.tn")),
    dttm = now()
  ) 
  
  if (is.na(st_err_increment)) {
    score[, st_err_increment := "lambda.min"]
  } else {
    score[, st_err_increment := paste0("lambda.", st_err_increment,"se")]
  }
  
  scores <- bind_rows(scores, score)
  
  # save results on test set
  
  pred_test = as.data.table(predict(learner, ts_test, predict_type = "prob"))
  preds <- pred_test$'1'
  
  score = data.table(
                     timeslice = name_tsk,
                     tsk_ids = "test",
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = LogLoss(preds, labels),
                     prauc = PRAUC(preds, labels),
                     auc = AUC(preds, labels),
                     dttm = now()
  ) 
  
  if (is.na(st_err_increment)) {
    score[, st_err_increment := "lambda.min"]
  } else {
    score[, st_err_increment := paste0("lambda.", st_err_increment,"se")]
  }
  
  scores <- bind_rows(scores, score)
  
  return(scores)
}


get_coefs = function(name_tsk, learner, tsk_ids, tuning_round, param_value, coefs, model_features, dataset, st_err_increment = NA) {
  
  # see this for explanation of lambda.min and lambsa.1se
  # https://institute-and-faculty-of-actuaries.github.io/mlr-blog/post/f-mlr3example/
  

  # this will always return the coefs of the minimum lambda ie lambda.min
  # even if we have truncated the lambda at a multiple of the standard error
  
  coef = data.table()
  for (s_ in c("lambda.min")) {
    c = data.table(summary(coef(learner$model, s = s_)))
    coef  = bind_rows(coef,  data.table(variable = labels(coef(learner$model, s = s_))[[1]][c$i], 
                       coef = c$x, s = s_))
  }
  

  coef[, model_features := model_features]
  coef[, timeslice := name_tsk]
  coef[, tsk_ids := tsk_ids]
  coef[, tuning_round := tuning_round]
  
  # save which version of the lambda was used
  
  if (param_value == "lambda.min") {
    coef[, param_value := "lambda.min"]
  } else {
    coef[, param_value := paste0("lambda.", st_err_increment,"se")]
  }
  
  coef[, dttm := now()]
  
  
  coefs <- bind_rows(coefs, coef)
  return(coefs)
  
}


# Set program parameters --------------------------------------------------


model_period = "Pre"
model_features = "alop"


# Load saved data ----------------------------------------------

# load summ to look up first_ED_admission
# needed for validation set ranges

load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))


scores_file <- paste0("EDcrowding/predict-admission/data-output/lr_",model_features, "_", model_period, "_scores_",today(),".rda")

if (file.exists(scores_file)) {
  load(scores_file)
} else {
  scores <- data.table()
}

# preds_file <- paste0("EDcrowding/predict-admission/data-output/lr_",model_features, "_preds_",today(),".rda")
# 
# if (file.exists(preds_file)) {
#   load(preds_file)
# } else {
#   preds <- data.table()
# }
# 

imps_file <- paste0("EDcrowding/predict-admission/data-output/lr_",model_features, "_", model_period, "_imps_",today(),".rda")

if (file.exists(imps_file)) {
  load(imps_file)
} else {
  imps <- data.table()
}



# Load data and encode factors --------------------------------------------------------------

#timeslices <- c("000")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")
# timeslices <- c("000", "015", "030")
# timeslices <- c("300", "360", "480", "720")




for (ts_ in timeslices) {
  
  # load timeslice 
  inFile = paste0("EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
  load(inFile)
  
  name_ts <- paste0("dm", ts_)
  dt = get(name_ts)
  
  # set training, validation and test set dates
  dt = set_train_val_test_dates(dt, summ, model_period)
  dt = dt[in_set %in% c("Train", "Val")]
  
  dt[, row_id := seq_len(nrow(dt))]
  assign(paste0("task", ts_, "_val_ids"), dt[in_set == "Val", row_id])
  assign(paste0("task", ts_, "_train_ids"), dt[in_set == "Train", row_id])
  
  #  for combined dataset, include a flag for whether post Covid era or not
  if (model_period == "Pre + Post") {
    
    csn_lookup = summ[csn %in% dt[, csn], . (csn, first_ED_admission)]
    csn_lookup[, a_post_Covid := factor(first_ED_admission > covid_start)]
    dt = merge(dt, csn_lookup[, .(csn, a_post_Covid)], by = "csn")
  }
  
  
  # remove train-val-test label and row_id so not included in features
  dt[, row_id := NULL]
  
  
  # remove features not wanted in model
  if (model_features != "alop") {
    if (!grepl("p", model_features)) {
      dt[, colnames(dt)[grep("^p_", colnames(dt))] := NULL]
    }
    if (!grepl("a", model_features)) {
      dt[, colnames(dt)[grep("^a_", colnames(dt))] := NULL]
    }
    if (!grepl("l", model_features)) {
      dt[, colnames(dt)[grep("^l_", colnames(dt))] := NULL]
    }
    if (!grepl("o", model_features)) {
      dt[, colnames(dt)[grep("^o_", colnames(dt))] := NULL]
    }
  }
  
  # encode factors
  ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=TRUE)
  ts[,adm:=as.factor(adm)] 
  
  # remove train-val-test label and row_id so not included in features
  ts[, in_set := NULL]
  
  # save medians for use in test set
  latest_cols = colnames(ts)[grep("latest", colnames(ts))]
  ts_latest = ts[, .SD, .SDcols=latest_cols]
  
  medians_to_save = data.table(variable = latest_cols, median = ts[, sapply(.SD, median, na.rm = TRUE), .SDcols=latest_cols])
  
  # replace columns with missing values with median
  for(j in grep("latest", colnames(ts))){
    set(ts, i = which(is.na(ts[[j]])), j = j, value = median(ts[[j]], na.rm = TRUE))
  }
  
  s = skim(ts)
  p = partition(s)
  
  logicals = data.table(p$logical)
  numerics = data.table(p$numeric)
  
  print(ts_)
  # print(paste("\nDeleting these logical columns which have uniform values:"))
  # print(logicals[mean %in% c(0,1), .(skim_variable, prop_missing = n_missing/nrow(ts))])
  ts[, logicals[mean %in% c(0,1), skim_variable] := NULL]
  
  # print(paste("\nDeleting these logicals which have uniformly missing values:"))
  # print(logicals[n_missing !=0, .(skim_variable, prop_missing = n_missing/nrow(ts))])
  ts[, logicals[n_missing !=0, skim_variable] := NULL]
  
  # print(paste("\nDeleting these numeric columns which have uniform values:"))
  # print(logicals[mean %in% c(0,1), .(skim_variable, prop_missing = n_missing/nrow(ts))])
  ts[, numerics[mean %in% c(0,1) | sd == 0, skim_variable] := NULL]

  # print(paste("\nDeleting these numerics which have uniformly missing values:"))
  # print(numerics[n_missing !=0, .(skim_variable, prop_missing = n_missing/nrow(ts))])
  ts[, numerics[n_missing !=0, skim_variable] := NULL]
  
  # scale columns and save scaled values
  cols_to_scale = colnames(ts)[!colnames(ts) %in% colnames(ts)[grep("csn|^adm",colnames(ts))]]
  ts_to_scale = ts[,.SD, .SDcols = cols_to_scale]
  
  means_to_save = data.table(variable = cols_to_scale, mean_ = ts_to_scale[, sapply(.SD, mean)])
  sds_to_save = data.table(variable = cols_to_scale, sd_ = ts_to_scale[, sapply(.SD, sd)])
  
  ts[, (cols_to_scale) := lapply(.SD, scale), .SDcols=cols_to_scale]
  
  # assign to named data table
  name_tsp <- paste0("dm", ts_, "p")
  assign(name_tsp, ts)
  
  # assign medians to named data table
  name_medians <- paste0("dm", ts_, "p_medians")
  assign(name_medians, medians_to_save)
  
  # assign means and sds to named data table
  name_means <- paste0("dm", ts_, "p_means")
  assign(name_means, means_to_save)
  name_sds <- paste0("dm", ts_, "p_sds")
  assign(name_sds, sds_to_save)
  
  feature_list <- colnames(ts)
  name_feature_list <- paste0("dm", ts_, "p_features")
  assign(name_feature_list, feature_list)
  
  
}





# Set up ML ------------------------------------------------------------


# create task
for (ts_ in timeslices) {
  name_ts <- paste0("dm", ts_, "p")
  ts = get(name_ts)
  
  # create task
  tsk = TaskClassif$new(id = name_ts, backend = ts ,target="adm") 
  tsk$col_roles$name = "csn"
  tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
  tsk$positive = "1" # tell mlr3 which is the positive class
  name_tsk <- paste0("task", ts_)
  assign(name_tsk, tsk)
}

# create learner
# base R package here:
# https://cran.r-project.org/web/packages/glmnet/vignettes/glmnet.pdf

# instance in mlr3 here
# https://mlr3learners.mlr-org.com/reference/mlr_learners_classif.cv_glmnet.html


# see this for explanation of lambda.min and lambsa.1se
# https://institute-and-faculty-of-actuaries.github.io/mlr-blog/post/f-mlr3example/

learner = lrn("classif.cv_glmnet", predict_type = "prob")

learner_params_file <- paste0("EDcrowding/predict-admission/data-output/lr_",model_features,
                              "_", gsub(" +", "", model_period), "_learner_params_",today(),".rda")
learner_params = data.table()


# Sections of the training, validation and test sets follow ---------------


  
for (ts_ in timeslices) {
  

# Load training set -------------------------------------------------------

  
  
  # first load the training data
  # this is already encoded within the MLR3 task created earlier
  name_tsk <- paste0("task", ts_)
  tsk = get(name_tsk)
  tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
  tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
  

# Load test set and configure using saved means, medians and modes -------------------------------------
  
  # usually, we would simply apply the MLR3 test to the test set  
  # and use a pipeline to encode the test set using saved factor encoding and standardisation parameters
  
  # however, I'm using a workaround here as the MLR3 interface to GLMNET does not 
  # allow me to set a specific value for the value of lambda
  # instead, I'm manually re-encoding and standardising the test set using saved parameters
  
  name_ts <- paste0("dm", ts_)
  dt = get(name_ts)
  
  name_medians <- paste0("dm", ts_, "p_medians")
  medians_to_save  = get(name_medians)
  
  name_means <- paste0("dm", ts_, "p_means")
  means_to_save  = get(name_means)
  
  name_sds <- paste0("dm", ts_, "p_sds")
  sds_to_save  = get(name_sds)
  
  name_feature_list <- paste0("dm", ts_, "p_features")
  feature_list = get(name_feature_list)
  
  # keep only test set
  dt = set_train_val_test_dates(dt, summ, model_period)
  dt_test = dt[in_set %in% c("Test")]
  
  # remove features not wanted in model
  if (model_features != "alop") {
    if (!grepl("p", model_features)) {
      dt_test[, colnames(dt_test)[grep("^p_", colnames(dt_test))] := NULL]
    }
    if (!grepl("a", model_features)) {
      dt_test[, colnames(dt_test)[grep("^a_", colnames(dt_test))] := NULL]
    }
    if (!grepl("l", model_features)) {
      dt_test[, colnames(dt_test)[grep("^l_", colnames(dt_test))] := NULL]
    }
    if (!grepl("o", model_features)) {
      dt_test[, colnames(dt_test)[grep("^o_", colnames(dt_test))] := NULL]
    }
  }
  
  # encode factors
  ts_test <- one_hot(cols = "auto", dt = dt_test,  dropUnusedLevels=TRUE)
  ts_test[,adm:=as.factor(adm)] 
  
  # remove train-val-test label and row_id so not included in features
  ts_test[, in_set := NULL]
  
  # replace missing features
  ts_test_cols = colnames(ts_test)
  missing_features = feature_list[!feature_list %in% ts_test_cols] 
  missing_features = missing_features[!grepl("adm", missing_features)]
  
  if (length(missing_features) > 0) {
    
    missing_features_NA = missing_features[grepl("latest", missing_features)]
    missing_features_0 = missing_features[!grepl("latest", missing_features)]
    
    if (length(missing_features_NA[grep("Score", missing_features_NA)])>0) {
      
      if (length(missing_features_0) > 0) {
        
        missing_features_0 = c(missing_features_0, missing_features_NA[grep("Score", missing_features_NA)])
      } else  {
        
        missing_features_0 = missing_features_NA[grep("Score", missing_features_NA)]
        
      }
      
      missing_features_NA = missing_features_NA[!grepl("Score", missing_features_NA)]
    }
    
    # add columns that have zero as default
    if (length(missing_features_0) > 0) {
      missing_cols <- data.table(matrix(0, nrow = nrow(ts_test), ncol = length(missing_features_0)))
      ts_test = bind_cols(ts_test, missing_cols)
      colnames(ts_test) = c(ts_test_cols, missing_features_0)
      ts_test_cols = colnames(ts_test)
    }
    
    # add columns that have NA as default
    if (length(missing_features_NA) > 0)  {
      missing_cols <- data.table(matrix(NA, nrow = nrow(ts_test), ncol = length(missing_features_NA)))
      ts_test = bind_cols(ts_test, missing_cols)
      colnames(ts_test) = c(ts_test_cols, missing_features_NA)
    }
  }
  
  # keep only columns in the original feature list
  ts_test = ts_test[, .SD, .SDcols=feature_list]
  
  # find any missings and replace with medians
  
  # replace columns with missing values with median that was saved earlier
  for(j in grep("latest", colnames(ts_test))){
    set(ts_test, i = which(is.na(ts_test[[j]])), j = j, value = medians_to_save[variable == colnames(ts_test)[j], median])
  }
  
  # scale values 
  for (k in 1:nrow(means_to_save)) {
    ts_test[, (means_to_save$variable[k]) := lapply(.SD, function(x) (x - means_to_save$mean_[k])/sds_to_save$sd_[k]), .SDcols=means_to_save$variable[k]]
    
  }
  
  
# train on full training set and save results on validation set -----------
  # first use the learner which will by default provide lambda.min and lambda.1se, to save importances
  
  
  learner = lrn("classif.cv_glmnet", predict_type = "prob")
  scores <- save_results_lr(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = NA,
                            tuning_round = "tune_lambda", scores, model_features, st_err_increment = NA, 
                            ts_test, labels = ts_test$adm == 1)
  
  imps <- get_coefs(name_tsk, learner, tsk_ids = "all", tuning_round = "lambda",
                    param_value = "lambda.min",
                    imps, model_features, model_period, st_err_increment = NA)

  learner_file  <- paste0("EDcrowding/predict-admission/data-output/lr_",model_features,
                          "_", gsub(" +", "", model_period), "_learner_",name_tsk,"_", "lambda.min", "_", today(),".rda")
  save(learner, file = learner_file)
  
  learner_params = bind_rows(learner_params, data.table(timeslice = ts_, lambda = learner$model$lambda, nzero = learner$model$nzero, cvm = learner$model$cvm, 
                              cvlo = learner$model$cvlo, cvup = learner$model$cvup, cvsd = learner$model$cvsd))
  
  save(learner_params, file = learner_params_file)
  

# now increment the standard error ----------------------------------------
  
  st_err = learner$model$lambda.1se - learner$model$lambda.min
  
  for (st_err_increment in c(1,2,3,4)) {
    
    
    
    # set the lambdas so that the maximum available lambda is 2 standard errors away from the minumum 
    learner$param_set$values = insert_named(
      learner$param_set$values,
      list(
        "lambda" = c(learner$model$lambda[1:sum(learner$model$lambda > learner$model$lambda.min + st_err*st_err_increment)])
        
      )
    )
    
    # save scores; the predictions will use a new lambda.min
    scores <- save_results_lr(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                              tuning_round = "tune_lambda", scores, model_features, st_err_increment,  
                              ts_test, labels = ts_test$adm == 1)
    
    imps <- get_coefs(name_tsk, learner, tsk_ids = "all", tuning_round = "tune_lambda",
                      param_value = "lambda",
                      imps, model_features, model_period, st_err_increment)
    
  }
  
  save(imps, file = imps_file)
  save(scores, file = scores_file)
  
  
  
  
} 


scores[tsk_ids == "test"] %>% ggplot(aes(x = timeslice, y = logloss, colour = st_err_increment, group = st_err_increment)) + geom_line() +
  theme_bw() + labs(title = "Logloss (test set) by timeslice for various increments of the L1 penalty away from the best performing value")

imps[, .N, by = .(timeslice, param_value)]  %>% 
  ggplot(aes(x = timeslice, y = N, colour = param_value, group = param_value)) + geom_line() +
  theme_bw() + labs(title = "Number of variables used in the model for various increments of the L1 penalty away from the best performing value")