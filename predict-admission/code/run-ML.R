
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





# Set program parameters --------------------------------------------------


model_period = "Pre + Post"
model_features = "alop"


# choose features to include - a - admission features; l = location; o = observation; p = pathology

base_model = FALSE
tune_nr = FALSE
tune_trees = FALSE
# tune_gamma = FALSE # no longer tuning gamma; treat it as zero since in earlier versions tuning showed no variation
recal_nr = FALSE # have now capped nrounds at 30 so will skip this step (24.5.21)
tune_samples = FALSE
# tune_alpha = FALSE # no longer using necessary; we are achieving regularisation in others ways
reduce_lr = FALSE # doesn't make a discernible improvement to metrics, so will skip this step (25.5.21)
final_preds = FALSE # NB - check which prior tuning round final_preds is looking for

# For the last two, the scores_file is updated rather than new scores file created
# Therefore don't update config.R with today's date
final_preds_on_test_set = TRUE
final_preds_pre_on_post_covid_test_set = FALSE #  not yet copied from run-ML-for-dissertation.R




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


scores_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features, "_", model_period, "_scores_",today(),".rda")

if (file.exists(scores_file)) {
  load(scores_file)
} else {
  scores <- data.table()
}

# preds_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features, "_preds_",today(),".rda")
# 
# if (file.exists(preds_file)) {
#   load(preds_file)
# } else {
#   preds <- data.table()
# }
# 

imps_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features, "_", model_period, "_imps_",today(),".rda")

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


if (!final_preds_on_test_set & !final_preds_pre_on_post_covid_test_set) {
  
  for (ts_ in timeslices) {
    
    print(paste("Timeslice", ts_))
    
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
    ts <- one_hot(cols = "auto", dt = dt,  dropUnusedLevels=TRUE)
    ts[,adm:=as.factor(adm)] 
    
    
    # remove train-val-test label and row_id so not included in features
    ts[, in_set := NULL]
    
    # assign to named data table
    name_tsp <- paste0("dm", ts_, "p")
    assign(name_tsp, ts)
    
  }
  
}



# Set up ML ------------------------------------------------------------

if (!final_preds_on_test_set & !final_preds_pre_on_post_covid_test_set) {
  
  # create task
  for (ts_ in timeslices) {
    name_ts <- paste0("dm", ts_, "p")
    ts = get(name_ts)
    
    # create task
    tsk = TaskClassif$new(id = name_ts, backend =  ts ,target="adm") 
    tsk$col_roles$name = "csn"
    tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
    tsk$positive = "1" # tell mlr3 which is the positive class
    name_tsk <- paste0("task", ts_)
    assign(name_tsk, tsk)
  }
  
  # create learner
  learner = lrn("classif.xgboost", predict_type = "prob")
}




# Base model -----------------------------------

if (base_model) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    learner <- train_learner(learner, tsk, tsk_train_ids)
    scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "base", scores, model_features, model_period)
    scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                           tuning_round = "base", scores, model_features, model_period)
    
  } 
  
  save(scores, file = scores_file)
  
}



# Tuning nrounds ----------------------------------------------------------


if (tune_nr) {
  
  for (ts_ in timeslices) {
  # for (ts_ in timeslices[4:12]) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    for(nrounds in c(5, 10, 15, 30)) { # changed on 25.5.21
    # for(nrounds in c(5, 10, 15, 30, 40, 50, 60)) {
        
            # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "nrounds", nrounds = nrounds, 
                             scores, model_features, model_period)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                             tuning_round = "nrounds", 
                             scores, model_features, model_period)
      
      save(scores, file = scores_file) 
    }
  }
}


# Tune tree depth and max child weight ------------------------------------


if (tune_trees) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
                                  tuning_round == "nrounds",
                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
    
    # after evaluating the size of gains in nrounds, I choose to cap nrounds at 30
    # see report-on-tuning.R for more
    if (nrounds > 30) { nrounds = 30}
    
    # tune max_depth
    for(max_depth in c(2, 5, 8)) { # changed on 25.5.21
      # for(max_depth in c(2, 5, 8, 12, 16, 20)) { # first round
        
      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids,
                             tuning_round = "max_depth",
                             nrounds = nrounds,
                             max_depth = max_depth,
                             scores, model_features, model_period)

      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "max_depth", scores, model_features, model_period)

      save(scores, file = scores_file)

    }

    # # tune min_child_weight
    # for(min_child_weight in c(3, 4, 5)) { # first round
    # 
    #   # get scores on training set using cross-validation
    #   scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids,
    #                          nrounds = nrounds,
    #                          tuning_round = "min_child_weight",
    #                          min_child_weight = min_child_weight,
    #                          scores, model_features, model_period)
    # 
    #   scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
    #                          tuning_round = "min_child_weight", scores, model_features, model_period)
    # 
    #   save(scores, file = scores_file)
    # }
    # 
    
    # get best params
    best_param_val_md = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
                                            tuning_round == "max_depth",
                                       .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    best_param_val_mcw = 4 # changed on 21.5.21 
    
    # best_param_val_mcw = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
    #                                          tuning_round == "min_child_weight",
    #                                       .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    # 
    # tune in combination
    for(max_depth in c(best_param_val_md-1, best_param_val_md, best_param_val_md+1)) { # second round 
      for (min_child_weight in c(best_param_val_mcw-1, best_param_val_mcw, best_param_val_mcw+1)) {
        # get scores on training set using cross-validation
        scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                               tuning_round = "tune_trees", 
                               nrounds = nrounds,
                               max_depth = max_depth,
                               min_child_weight = min_child_weight,
                               scores, model_features, model_period)
        
        # train on full training set and save results on validation set
        scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                               tuning_round = "tune_trees", scores, model_features, model_period)
        
        save(scores, file = scores_file) 
        
      }
    }
  } 
  
  # scores[tsk_ids == "val" & tuning_round == "max_depth" & model_features == model_features & dataset == model_period,
  #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, max_depth)]
  #
  # scores[tsk_ids == "val" & tuning_round == "min_child_weight" & model_features == model_features & dataset == model_period,
  #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, min_child_weight)]
  #
  # scores[tsk_ids == "val" & tuning_round == "max_depth" & model_features == model_features & dataset == model_period] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = max_depth, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of tuning max_depth of XGBoost for each timeslice - logloss scores")
  #
  # scores[tsk_ids == "val" & tuning_round == "min_child_weight" & model_features == model_features & dataset == model_period] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = min_child_weight, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of tuning min_child_weight of XGBoost for each timeslice - logloss scores")
  #
  # # trying to plot both params on one chart - not obvious how to
  # scores[tsk_ids == "val" & tuning_round == "tune_trees" & model_features == model_features & dataset == model_period, .(timeslice, logloss, max_depth, min_child_weight)] %>%
  #   pivot_longer(max_depth:min_child_weight)  %>%
  #   ggplot(aes(x = value, y = logloss, col = name)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of tuning min_child_weight of XGBoost for each timeslice - logloss scores")
  #
  # # looking at log loss to date
  #
  # s = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == model_period]  %>%
  #                  pivot_longer(logloss) %>% select(timeslice, name, value, tuning_round, dttm))
  #
  #
  # s[, .SD[which.max(dttm)], by = list(timeslice, tuning_round)] %>%
  #   filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
  #   ggplot(aes(x = factor(tuning_round, levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr")), y = value,
  #              group = "tuning_round")) +
  #   geom_line() + geom_point() + facet_grid(. ~ timeslice) +
  #   theme(axis.text.x=element_text(angle=45,hjust=1)) +
  #   labs(title = "Log loss values after each round of tuning - new approach to train-validation-test split",
  #        x = "Tuning round",
  #        y = "Log loss value")
  #
  # # looking at nrounds to date
  #
  # n = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == model_period & tuning_round == "nrounds"]  %>%
  #                  pivot_longer(nrounds) %>% select(timeslice, name, value, tuning_round, logloss))
  #
  #
  # n[, .SD[which.min(logloss)], by = list(timeslice)] %>%
  #   ggplot(aes(x = timeslice, y = value)) +
  #   geom_line() + geom_point() +
  #   theme(axis.text.x=element_text(angle=45,hjust=1)) +
  #   labs(title = "Best nround values after nround tuning",
  #        x = "Tuning round",
  #        y = "nrounds")

}


# # Tune gamma --------------------------------------------------------------
# 
# if (tune_gamma) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
#                                   tuning_round == "tune_trees",
#                                 .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
#     
#     max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
#                                   nrounds == nrounds & tuning_round == "tune_trees",
#                                 .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
#     
#     
#     min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
#                                            nrounds == nrounds & tuning_round == "tune_trees",
#                                   .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
#     
#     # tune gamma
#     for(gamma in c(0, 0.1, 0.2, 0.3, 0.4)) { 
# 
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "gamma", 
#                              nrounds = nrounds,
#                              max_depth = max_depth, 
#                              min_child_weight = min_child_weight, 
#                              gamma = gamma, 
#                              scores, model_features, model_period)
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "gamma", scores, model_features, model_period)
#     }
#     
#     save(scores, file = scores_file) # aggregate scores of 10 resamplings
# 
#   } 
# 
#   # scores[tsk_ids == "val" & tuning_round == "gamma" & model_features == model_features & dataset == model_period,
#   #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, gamma)]
#   # 
#   # scores[tsk_ids == "train" & tuning_round == "gamma" & model_features == model_features & dataset == model_period] %>%
#   #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
#   #   ggplot(aes(x = gamma, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
#   #   labs(y = "logloss", title = "Results of tuning gamma of XGBoost for each timeslice - logloss scores")
# 
# }

# Recalibrate nrounds -----------------------------------------------------


# if (recal_nr) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
#                                     tuning_round == "tune_trees",
#                                   .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
#     
#     
#     min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
#                                            tuning_round == "tune_trees",
#                                          .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
#     
#     retry_nr = seq(min(scores[tsk_ids == "val"& model_features == model_features  & dataset == model_period &  tuning_round == "tune_trees",
#                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)]), 
#                    max(scores[tsk_ids == "val"& model_features == model_features & dataset == model_period &  tuning_round == "tune_trees",
#                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)]), 
#                    10)
#     
#     # tune nrounds again trying all values of nrounds from the minimum to the maximum best in previous rounds
#     for(nrounds in retry_nr) { 
#       
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "recal_nr", 
#                              nrounds = nrounds,
#                              max_depth = max_depth, 
#                              min_child_weight = min_child_weight, 
#                              # gamma = gamma, 
#                              scores, model_features, model_period)
#       
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "recal_nr", scores, model_features, model_period)
#     }
#     
#     save(scores, file = scores_file) 
#     
#   } 
#   
#   scores[tsk_ids == "val" & tuning_round %in% c( "recal_nr", "nrounds") & model_features == model_features & dataset == model_period,
#          .SD[which.min(logloss)], by = list(timeslice, tuning_round)][,.(timeslice, tuning_round, nrounds, logloss)]
# 
# 
#   scores[tsk_ids == "val" & tuning_round == "recal_nr" & model_features == model_features & dataset == model_period] %>%
#     pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
#     ggplot(aes(x = nrounds, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
#     labs(y = "logloss", title = "Results of recalibrating nrounds of XGBoost for each timeslice - logloss scores")
#   
# }


# Tune subsamples and col samples -----------------------------------------

if (tune_samples) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    
    nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
                                    tuning_round == "tune_trees",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
    
    max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
                                    tuning_round == "tune_trees",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
                                           tuning_round == "tune_trees",
                                         .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    # 
    # gamma = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
    #                             tuning_round == "recal_nr",
    #                           .SD[which.min(logloss)], by = list(timeslice)][,.(gamma)])
    
    # # tune subsample 
    # for(subsample in c(0.6, 0.7, 0.8, 0.9)) { 
    #   
    #   # get scores on training set using cross-validation
    #   scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
    #                          tuning_round = "subsample", 
    #                          nrounds = nrounds,
    #                          max_depth = max_depth, 
    #                          min_child_weight = min_child_weight, 
    #                          # gamma = gamma, 
    #                          subsample = subsample, 
    #                          scores, model_features, model_period)
    # 
    #   # train on full training set and save results on validation set
    #   scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
    #                          tuning_round = "subsample", scores, model_features, model_period)
    # }
    # 
    # save(scores, file = scores_file) 
    
    # tune colsample 
    for(colsample_bytree in c(0.6, 0.7, 0.8, 0.9)) { 

      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "colsample_bytree", 
                             nrounds = nrounds,
                             max_depth = max_depth, 
                             min_child_weight = min_child_weight, 
                             # gamma = gamma, 
                             colsample_bytree = colsample_bytree, 
                             scores, model_features, model_period)
      
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "colsample_bytree", scores, model_features, model_period)
    }
    
    save(scores, file = scores_file) 
    
    # get best param values
    
    best_param_val_sub = 0.8 # changed on 25.5.21
    
    # best_param_val_sub = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
    #                                      tuning_round == "subsample",
    #                                    .SD[which.min(logloss)], by = list(timeslice)][,.(subsample)])
    
    best_param_val_col = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
                                             tuning_round == "colsample_bytree",
                                           .SD[which.min(logloss)], by = list(timeslice)][,.(colsample_bytree)])
    
    
    
    # tune in combination
    
    for(subsample in c(best_param_val_sub - .1, best_param_val_sub, best_param_val_sub + .1)) { 
      for (colsample_bytree in c(best_param_val_col - .1, best_param_val_col, best_param_val_col + .1)) {
        
        # get scores on training set using cross-validation
        scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                               tuning_round = "tune_samples", 
                               nrounds = nrounds,
                               max_depth = max_depth, 
                               min_child_weight = min_child_weight, 
                               # gamma = gamma, 
                               subsample = subsample, 
                               colsample_bytree = colsample_bytree,
                               scores, model_features, model_period)
        
        # train on full training set and save results on validation set
        scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                               tuning_round = "tune_samples", scores, model_features, model_period)
        
      }

    }
    
    save(scores, file = scores_file) 
    
  } 

  # scores[tsk_ids == "val" & tuning_round == "tune_samples" & model_features == model_features & dataset == model_period & subsample != 1,
  #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, subsample, colsample_bytree)]
  # 
  # 
  # scores[tsk_ids == "val" & tuning_round == "subsample" & model_features == model_features & dataset == model_period] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = subsample, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of recalibrating subsample of XGBoost for each timeslice - logloss scores")
  # 
  # scores[tsk_ids == "train" & tuning_round == "colsample_bytree" & model_features == model_features & dataset == model_period] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = colsample_bytree, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of recalibrating colsample_bytree of XGBoost for each timeslice - logloss scores")
  
}


# # Tune alpha --------------------------------------------------------------
# 
# if (tune_alpha) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
#                            tuning_round == "tune_samples",
#                          .SD[which.min(logloss)], by = list(timeslice)]
#     
#     # tune gamma
#     # for(alpha in c(.005, 1, 10, 100)) { 
#     for(alpha in c(0, .01, .1, .5, 2, 5)) { 
#         
#       
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "alpha", 
#                              nrounds = params$nrounds,
#                              max_depth = params$max_depth, 
#                              min_child_weight = params$min_child_weight, 
#                              gamma = params$gamma, 
#                              subsample = params$subsample,
#                              colsample_bytree = params$colsample_bytree,
#                              alpha = alpha,
#                              scores, model_features, model_period)
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "alpha", scores, model_features, model_period)
#     }
#     
#     save(scores, file = scores_file) # aggregate scores of 10 resamplings
#     
#   } 
# 
#   # scores[tsk_ids == "val" & tuning_round == "alpha" & model_features == model_features & dataset == model_period,
#   #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, alpha)]
#   # 
#   # scores[tsk_ids == "val" & tuning_round == "alpha" & model_features == model_features & dataset == model_period & alpha <= 10] %>%
#   #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
#   #   ggplot(aes(x = alpha, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
#   #   labs(y = "logloss", title = "Results of tuning alpha of XGBoost for each timeslice - logloss scores")
# 
# }
# 
# # Looking at improvement on validation set with tuning --------------------
# 
# # s = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == model_period]  %>%
# #   pivot_longer(logloss) %>% select(timeslice, name, value, tuning_round, dttm))
# #   
# # 
# # s[, .SD[which.max(dttm)], by = list(timeslice, tuning_round)] %>% 
# #   ggplot(aes(x = factor(tuning_round, levels = c("base", "nrounds", "tune_trees", "gamma")), y = value)) +
# #   geom_point() + facet_grid(. ~ timeslice) + 
# #   theme(axis.text.x=element_text(angle=45,hjust=1)) 
# #   
# 


# # Reduce learning rate ----------------------------------------------------
# 
# 
# if (reduce_lr) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period & 
#                       tuning_round == "tune_samples",
#                     .SD[which.min(logloss)], by = list(timeslice)]
#     
#     for (eta in c(0.3, 0.2, 0.1, 0.05)) {
#       
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "reduce_lr", 
#                              nrounds = as.integer(params$nrounds*(.3 / eta)),
#                              max_depth = params$max_depth, 
#                              min_child_weight = params$min_child_weight, 
#                              # gamma = params$gamma, 
#                              subsample = params$subsample,
#                              colsample_bytree = params$colsample_bytree,
#                              # alpha = params$alpha,
#                              eta = eta,
#                              scores, model_features, model_period)
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "reduce_lr", scores, model_features, model_period)
#       
#       
#       save(scores, file = scores_file) # aggregate scores of 10 resamplings
#     }
# 
# 
#     
#   } 
#   
#   # scores[tsk_ids == "val" & tuning_round == "reduce_lr" & model_features == model_features & dataset == model_period,
#   #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, alpha)]
# }

# # Looking at improvement on validation set with tuning --------------------
# 
# s = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == model_period])
# 
# s1 = s[, .SD[which.min(logloss)], by = list(timeslice, tuning_round)] %>%
#   filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
#   ggplot(aes(x = factor(tuning_round,
#                         levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr")),
#              y = logloss,
#              group = "tuning_round")) +
#   geom_line() + geom_point() + facet_grid(. ~ timeslice) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) +
#   labs(title = "XGBoost Log loss values after each round of tuning (scores on validation set)",
#        x = "Tuning round",
#        y = "Log loss value") + theme_grey(base_size = 16) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) 
# 
# 
# s2 = s[, .SD[which.max(auc)], by = list(timeslice, tuning_round)] %>%
#   filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
#   ggplot(aes(x = factor(tuning_round,
#                         levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr")),
#              y = auc,
#              group = "tuning_round")) +
#   geom_line() + geom_point() + facet_grid(. ~ timeslice) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) +
#   labs(title = "XGBoost AUC scores after each round of tuning (scores on validation set)",
#        x = "Tuning round",
#        y = "AUC score") + theme_grey(base_size = 16) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) 
# 
# library(gridExtra)
# grid.arrange(s1, s2, nrow = 2)
# Save preds from final model ---------------------------------------------



if (final_preds) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))

    params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == model_period &
                      tuning_round == "tune_samples",
                    .SD[which.min(logloss)], by = list(timeslice)]

    learner <- update_learner(learner,
                              nrounds = params$nrounds,
                              eval_metric = params$eval_metric,
                              max_depth = params$max_depth,
                              min_child_weight = params$min_child_weight,
                              gamma = params$gamma,
                              subsample = params$subsample,
                              colsample_bytree = params$colsample_bytree,
                              eta = params$eta,
                              scale_pos_weight = params$scale_pos_weight,
                              alpha = params$alpha,
                              lambda = params$lambda,
                              early_stopping_rounds = params$early_stopping_rounds)

    set.seed(17L)

    # train on full training set and save results on validation set
    scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                           tuning_round = "final_preds", scores, model_features, model_period)
    
    # # get predictions on validation set
    # preds <- get_preds(name_tsk, tsk, learner, train_or_val_ids = dt$row_id, tsk_ids = "all", tuning_round = "final_preds",
    #                    param_value = "final_preds",
    #                    preds, model_features, model_period)
    # 
    # 
    # save(preds, file = preds_file)

    imps <- get_imps(name_tsk, learner, tsk_ids = "all", tuning_round = "final_preds",
                       param_value = "final_preds",
                       imps, model_features, model_period)
    save(imps, file = imps_file)

    # save learner data for future prediction

    learner_file  <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", model_period), "_learner_",name_tsk,"_",today(),".rda")
    save(learner, file = learner_file)

    #assign to named data table
    name_tsp <- paste0("dm", ts_, "p")
    ts = get(name_tsp)
    
    features_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", model_period), "_features_",name_tsk,"_",today(), ".rda")
    feature_list <- colnames(ts)
    
    save(feature_list, file =features_file)
    
  } 
  
  save(scores, file = scores_file) 
  
}



# Plot importances --------------------------------------------------------

# imps[tsk_ids == "all" & !feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
#                                                     "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6",
#                                         "a_sex_U") &
#        importance > 0.005] %>%
#   ggplot(aes(x = gsub("task","", timeslice), y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title = "Feature importances by timeslice",
#        fill = "Importance",
#        x = "Timeslice",
#        y = "Feature")
# 
# 
# p1 = imps[tsk_ids == "all" & !feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
#                                         "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6",
#                                         "a_sex_U") &
#        timeslice == "task030"  &
#        importance > 0.01] %>%
#   ggplot(aes(x = importance, y = reorder(feature, desc(feature)), fill = importance)) + geom_bar(stat = "identity") +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title = "Feature importances for 30  min timeslice",
#        fill = "Importance",
#        x = "Timeslice",
#        y = "Feature") +
#   theme(legend.position = "bottom")
# 
# 
# p2 = imps[tsk_ids == "all" & !feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
#                                         "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6",
#                                         "a_sex_U") &
#        timeslice == "task120"  &
#        importance > 0.01] %>%
#   ggplot(aes(x = importance, y = reorder(feature, desc(feature)), fill = importance)) + geom_bar(stat = "identity") +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title = "Feature importances for 120  min timeslice",
#        fill = "Importance",
#        x = "Timeslice",
#        y = "Feature") +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(limits = c(0,0.25))
# 
# library(gridExtra)
# grid.arrange(p1, p2,
#              ncol = 2, nrow = 1)



# Final preds on test set -------------------------------------------------



if (final_preds_on_test_set) {
  
  model_file_date_name = paste0("model_output_date_", model_period)
  model_file_date = get(model_file_date_name)
  
  scores_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                        "_", model_period, "_scores_", model_file_date,".rda")
  load(scores_file)
  
  # 
  # if (summ_file_date_for_checking_real_time > summ_file_date & model_period == "Post-SDEC") {
  #   
  #   load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
  #   
  #   summ[, arrival_hr := as.integer(substr(first_ED_admission, 12,13))]
  #   
  #   
  #   summ[, a_arrival_window := factor(case_when(arrival_hr > 21 | arrival_hr < 6 ~ "2200-0600",
  #                                               arrival_hr < 12 ~ "0600-1200",
  #                                               arrival_hr > 16 ~ "1200-1600",
  #                                               arrival_hr < 22 ~ "1600-2200"))]
  #   
  # }
  # 
  for (ts_ in timeslices) {
    
    name_tsk = paste0("task", ts_)
    
    # load timeslice 
    inFile = paste0("EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
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
    
    learner_file  <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", model_period), "_learner_",name_tsk,"_",model_file_date,".rda")
    load(learner_file)
    
    features_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
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

