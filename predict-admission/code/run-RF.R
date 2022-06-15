
# About this script -------------------------------------------------------

# Timeslice dataset are loaded and encoded into factors
# Each has a ML model trained on it with a binary outcome of admitted or discharged
# The steps are

# - prepare datasets using one-hot encoding, noting which visits should be in train or validation sets
# - train a basic model wihout tuning and score on training and validation sets
# - tune scale pos weight for imbalanced samples (this is included but not used)
# - tune number of boosting rounds  
# - tune tree characteristics: max_depth and min_child_weight, once crudely and then more refined a second time
# - tune gamma 
# - recalibrate number of boosting rounds
# - tune samples and colsamples_bytree, once crudely and then more refined a second time

# For each step the timeslices are handled in loops which iterate through each timeslice in turn

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)

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

library(skimr)



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


# Create function ---------------------------------------------------------

# set up parameters
train_learner_rf <- function(learner, tsk, tsk_train_ids, 
                             # initialise params at default values
                             num.trees = 500,
                             mtry = NA,
                             class.weights = NA,
                             sample.fraction = NA
) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values, 
    list(
      "num.trees" = num.trees
    )
  )
  
  if (!is.na(mtry)) {
    learner$param_set$values = insert_named(
      learner$param_set$values, list("mtry" = mtry))
  }
  
  if (!is.na(class.weights)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("class.weights" = class.weights
      )
    )
  }
  if (!is.na(sample.fraction)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("sample.fraction" = sample.fraction
      )
    )
  }
  
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  return(learner)
}

tune_learner_rf <- function(name_tsk, tsk, learner, tsk_train_ids, tuning_round, scores, model_features,
                            # initialise params at default values
                            num.trees = 500,
                            num.threads = 8,
                            mtry = NA,
                            class.weights = NA, 
                            sample.fraction = NA
) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values, 
    list(
      "num.trees" = num.trees,
      "num.threads" = num.threads
      
    )
  )
  
  if (!is.na(mtry)) {
    learner$param_set$values = insert_named(
      learner$param_set$values, list("mtry" = mtry))
  }
  
  if (!is.na(class.weights)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("class.weights" = class.weights
      )
    )
  }
  if (!is.na(sample.fraction)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("sample.fraction" = sample.fraction
      )
    )
  }
  
  set.seed(17L)
  rr = resample(tsk, learner, rsmp("cv"), store_models = FALSE)
  
  score = data.table(
    timeslice = name_tsk,
    tsk_ids = "train",
    model_features = model_features,
    tuning_round = tuning_round,
    logloss = rr$aggregate(msr("classif.logloss")),
    bbrier = rr$aggregate(msr("classif.bbrier")), # binary brier score
    prauc = rr$aggregate(msr("classif.prauc")),
    auc = rr$aggregate(msr("classif.auc")),
    acc = rr$aggregate(msr("classif.acc")),
    tp = rr$aggregate(msr("classif.tp")),
    fp = rr$aggregate(msr("classif.fp")),
    fn = rr$aggregate(msr("classif.fn")),
    tn = rr$aggregate(msr("classif.tn")),
    num.trees = num.trees,
    mtry = mtry,
    class.weights = class.weights,
    sample.fraction = sample.fraction, 
    dttm = now()
  )
  
  scores <- bind_rows(scores, score)
}


save_results_rf <- function(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids, tuning_round, scores, model_features) {
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # get predictions
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  
  score = data.table(
    timeslice = name_tsk,
    tsk_ids = tsk_ids,
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
    num.trees = learner$param_set$values$num.trees,
    mtry = learner$param_set$values$mtry,
    sample.fraction = learner$param_set$values$sample.fraction,
    dttm = now()
  ) 
  
  scores <- bind_rows(scores, score)
  
  return(scores)
}

update_learner_rf = function(learner, 
                             num.trees = NA,
                             mtry = NA,
                             sample.fraction = NA) {
  
  if (!is.na(num.trees)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("num.trees" = num.trees))
  }
  
  if (!is.na(mtry)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("mtry" = mtry))
  }
  
  if (!is.na(sample.fraction)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("sample.fraction" = sample.fraction))
  }
  
  return(learner)
  
}


save_results_test_set_rf <- function(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, 
                                     tsk_ids, tuning_round, scores, model_features, dataset, labels) {
  
  # get predictions
  
  pred_test = as.data.table(predict(learner, ts, predict_type = "prob"))
  preds <- pred_test$'1'
  
  score = data.table(dataset = dataset,
                     timeslice = name_tsk,
                     tsk_ids = tsk_ids,
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = LogLoss(preds, labels),
                     prauc = PRAUC(preds, labels),
                     auc = AUC(preds, labels),
                     acc = Accuracy(preds, labels),
                     num.trees = learner$param_set$values$num.trees,
                     mtry = learner$param_set$values$mtry,
                     sample.fraction = learner$param_set$values$sample.fraction,
                     dttm = now()
  ) 
  
  scores <- bind_rows(scores, score)
  
  return(scores)
}





# Set program parameters --------------------------------------------------


model_period = "Pre"
model_features = "alop"


# choose features to include - a - admission features; l = location; o = observation; p = pathology

# base_model = TRUE
# tune_num.trees = TRUE
# tune_mtry = FALSE
# tune_sample.fraction = FALSE
# tune_class.weights = FALSE

final_preds = FALSE


# For these last two, the scores_file is updated rather than new scores file created
# Therefore don't update config.R with today's date


final_preds_on_test_set = TRUE #NB currently needs to be run as part of same sesison as medians are not saved!!!!




# Load saved data ----------------------------------------------

# load summ to look up first_ED_admission
# needed for validation set ranges

load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))

# summ[, arrival_hr := as.integer(substr(first_ED_admission, 12,13))]
# 
# 
# summ[, a_arrival_window := factor(case_when(arrival_hr > 21 | arrival_hr < 6 ~ "2200-0600",
#                                             arrival_hr < 12 ~ "0600-1200",
#                                             arrival_hr > 16 ~ "1200-1600",
#                                             arrival_hr < 22 ~ "1600-2200"))]


scores_file <- paste0("EDcrowding/predict-admission/data-output/rf_",model_features, "_", model_period, "_scores_",today(),".rda")

if (file.exists(scores_file)) {
  load(scores_file)
} else {
  scores <- data.table()
}

# preds_file <- paste0("EDcrowding/predict-admission/data-output/rf_",model_features, "_preds_",today(),".rda")
# 
# if (file.exists(preds_file)) {
#   load(preds_file)
# } else {
#   preds <- data.table()
# }
# 

imps_file <- paste0("EDcrowding/predict-admission/data-output/rf_",model_features, "_", model_period, "_imps_",today(),".rda")

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


if (final_preds) {
  
  
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
    print(paste("\nDeleting these logical columns which have uniform values:"))
    print(logicals[mean %in% c(0,1), .(skim_variable, prop_missing = n_missing/nrow(ts))])
    ts[, logicals[mean %in% c(0,1), skim_variable] := NULL]
    
    print(paste("\nDeleting these logicals which have missing values:"))
    print(logicals[n_missing !=0, .(skim_variable, prop_missing = n_missing/nrow(ts))])
    ts[, logicals[n_missing !=0, skim_variable] := NULL]
    
    print(paste("\nDeleting these numerics which have missing values:"))
    print(numerics[n_missing !=0, .(skim_variable, prop_missing = n_missing/nrow(ts))])
    ts[, numerics[n_missing !=0, skim_variable] := NULL]
    
    
    # any columns with NAs will now have NAs throughout so these can be removed completely
    # ts[, colnames(ts)[grep("latest", colnames(ts))] := NULL]
    
    # if (model_period == "Pre") {
    #   ts[, a_covid_surge := NULL]
    # }
    # 
    # 
    
    # assign to named data table
    name_tsp <- paste0("dm", ts_, "p")
    assign(name_tsp, ts)
    
    # assign to named data table
    name_medians <- paste0("dm", ts_, "p_medians")
    assign(name_medians, medians_to_save)
  }
  
}



# Set up ML ------------------------------------------------------------

if (final_preds) {
  
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
  learner = lrn("classif.ranger", predict_type = "prob", importance = "permutation")
}



# 
# # Base model -----------------------------------
# 
# if (base_model) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     print(paste0("Processing with RF default parameters ", name_tsk))
#     scores <- tune_learner_rf(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "base", scores, model_features)
#     scores <- save_results_rf(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", tuning_round = "base", scores, model_features)
#     
#   } 
#   
#   save(scores, file = scores_file)
#   
# }


# 
# 
# # Open previous scores ----------------------------------------------------
# 
# load("EDcrowding/dissertation/data-output/rf_alop_Pre_scores_2021-08-27.rda")
# 
# params_sf = scores[tsk_ids == "val" & model_features == model_features & model_period == model_period &
#                      tuning_round == "sample.fraction",
#                    .SD[which.min(logloss)], by = list(timeslice)]
# 
# scores_rf = data.table(scores[tsk_ids == "val" ]  %>%
#                          pivot_longer(logloss) ) 
# 
# 
# scores_rf[, .SD[which.max(value)], by = list(model_period, model_features, timeslice, tuning_round)] %>%
#   # filter(!tuning_round %in% c("base")) %>%
#   # mutate(model_features = factor(model_features, levels = c("aop", "alop"), labels = c("without location", "with location"))) %>% 
#   ggplot(aes(x = factor(tuning_round, levels = c("base", "num.trees", "mtry", "sample.fraction", "final_preds")), 
#              y = value)) +
#   geom_line() + geom_point() + facet_grid(dataset~ timeslice) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) +
#   labs(title = "Logloss values after each round of tuning: Random Forest models with location features", 
#        x = "Tuning round", 
#        y = "Logloss value", 
#        colour = "Model") +
#   theme(legend.position = "bottom")


if (final_preds) {
  
  learner <- update_learner_rf(learner,
                               num.trees = 500,
                               mtry = 16,
                               sample.fraction = 0.75)

  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    
    set.seed(17L)
    
    # train on full training set and save results on validation set
    scores <- save_results_rf(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                              tuning_round = "fixed_params", scores, model_features)
    
    imps <- get_imps(name_tsk, learner, tsk_ids = "all", tuning_round = "fixed_params",
                     param_value = "fixed_params",
                     imps, model_features, model_period)
    save(imps, file = imps_file)
    
    # save learner data for future prediction
    
    learner_file  <- paste0("EDcrowding/predict-admission/data-output/rf_",model_features,
                            "_", gsub(" +", "", model_period), "_learner_",name_tsk,"_",today(),".rda")
    save(learner, file = learner_file)
    
    #assign to named data table
    name_tsp <- paste0("dm", ts_, "p")
    ts = get(name_tsp)
    
    features_file <- paste0("EDcrowding/predict-admission/data-output/rf_",model_features,
                            "_", gsub(" +", "", model_period), "_features_",name_tsk,"_",today(), ".rda")
    feature_list <- colnames(ts)
    
    save(feature_list, file =features_file)
    
  } 
  
  save(scores, file = scores_file) 
  
}



# Final preds on test set -------------------------------------------------




if (final_preds_on_test_set) {
  
  # model_file_date_name = paste0("model_output_date_", model_period)
  model_file_date = today()
  # 
  # scores_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
  #                       "_", model_period, "_scores_", model_file_date,".rda")
  # load(scores_file)
  

  for (ts_ in timeslices) {
    
    name_tsk = paste0("task", ts_)
    
    # load timeslice 
    inFile = paste0("EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
    name_medians <- paste0("dm", ts_, "p_medians")
    medians_to_save  = get(name_medians)
    
    # # add arrival window as additional factor
    # dt = merge(dt, summ[, .(csn, a_arrival_window)], by = "csn")
    
    # set training, validation and test set dates
    dt = set_train_val_test_dates(dt, summ, model_period)
    dt = dt[in_set %in% c("Test")]
    
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
    ts <- one_hot(cols = "auto", dt = dt,  dropUnusedLevels=TRUE)
    ts[,adm:=as.factor(adm)] 
    
    
    # remove train-val-test label and row_id so not included in features
    ts[, in_set := NULL]
    
    learner_file  <- paste0("EDcrowding/predict-admission/data-output/rf_",model_features,
                            "_", gsub(" +", "", model_period), "_learner_",name_tsk,"_",model_file_date,".rda")
    load(learner_file)
    
    features_file <- paste0("EDcrowding/predict-admission/data-output/rf_",model_features,
                            "_", gsub(" +", "", model_period), "_features_",name_tsk,"_",model_file_date, ".rda")
    load(features_file)
    

    
    ts_cols = colnames(ts)
    missing_features = feature_list[!feature_list %in% ts_cols] 
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
        missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features_0)))
        ts = bind_cols(ts, missing_cols)
        colnames(ts) = c(ts_cols, missing_features_0)
        ts_cols = colnames(ts)
      }
      
      # add columns that have NA as default
      if (length(missing_features_NA) > 0)  {
        missing_cols <- data.table(matrix(NA, nrow = nrow(ts), ncol = length(missing_features_NA)))
        ts = bind_cols(ts, missing_cols)
        colnames(ts) = c(ts_cols, missing_features_NA)
      }
    }
    
    # keep only columns in the original feature list
    ts = ts[, .SD, .SDcols=feature_list]
    
    # find any missings and replace with medians
    
    # replace columns with missing values with median that was saved earlier
    for(j in grep("latest", colnames(ts))){
      set(ts, i = which(is.na(ts[[j]])), j = j, value = medians_to_save[variable == colnames(ts)[j], median])
    }
    
    
    # train on full training set and save results on validation set
    scores <- save_results_test_set(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "test",
                                    tuning_round = "final_preds", scores, model_features, model_period,
                                    labels = ts$adm == 1)
    
    # # get predictions on validation set
    # preds <- get_preds(name_tsk, tsk, learner, train_or_val_ids = dt$row_id, tsk_ids = "all", tuning_round = "final_preds",
    #                    param_value = "final_preds",
    #                    preds, model_features, model_period)
    # 
    # 
    # save(preds, file = preds_file)
    
  } 
  
  save(scores, file = scores_file) 
  
}
