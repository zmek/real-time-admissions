# About this script -------------------------------------------------------

# Contains the functions used in run-ML.R, with some functions common
# scripts in the predict-admission/code folder


# Identifying training, val and test sets according to model period -------------------------------------

# This function is used by any script requiring a training, validation and test set. 
# It takes the full set of visits (in the dataset called summ) and mark each visit
# according to whether it falls into training, validation and test sets 
# These are chronologically defined, and differ by model period.
# The relevent cutoff dates for each are set in config.R. 

# In some cases this function is used to demarcate a different dataset (ie not the summ dataset) in 
# order to ascertain which dates fall into the relevant periods 

#  set training set 
set_train_val_test_dates <- function(dataset, summ, model_period, return_dates_only = NA) {
  
  # set up pre-Covid model
  
  if (model_period == "Pre") {
    
    start_model_period = start_study
    start_val = start_val_pre
    start_test = start_test_pre
    end_model_period = covid_start
    
  }
  
  # set up post-Covid  model
  
  if (model_period == "Post") {
    
    model_period = "Post"
    start_model_period = covid_start
    start_val = start_val_post
    start_test = start_test_post
    end_model_period = end_study
  }
  
  
  # set up pre and post model - same validation and test set as post Covid
  
  if (  model_period == "Pre + Post") {
    
    start_model_period = start_study
    start_val = start_val_post
    start_test = start_test_post
    end_model_period = end_study
  }

  # set up post SDEC model
  if (model_period == "Post-SDEC") {
    
    start_model_period = SDEC_start
    start_val = start_val_post_SDEC
    start_test = start_test_post_SDEC
    end_model_period = end_SDEC_study
  }
  
  
  if (  model_period == "Pre on Post") {
    
    start_model_period = start_study
    start_val = start_val_pre
    end_val = start_test_pre
    # note - validation set actually ends at end 
    start_test = start_test_post
    end_model_period = end_study
  }
  
  
  if (length(grep("time_pt", colnames(dataset))) > 0) {
    # dataset does not have csns or first_ED_admission
    
    dataset[, in_set := case_when(time_pt < start_model_period ~ "Before",
                                  time_pt < start_val ~ "Train",
                                  time_pt < start_test ~ "Val",
                                  time_pt < end_model_period ~ "Test",
                                  TRUE ~ "After")]
    
    
  } else if (length(grep("first_ED_admission", colnames(dataset))) == 0) {
    # dataset has had had dates removed, but csns should remain
    
    csn_lookup = summ[csn %in% dataset[, csn], . (csn, first_ED_admission)]
    
    csn_lookup[, in_set := case_when(first_ED_admission < start_model_period ~ "Before",
                                     first_ED_admission < start_val ~ "Train",
                                     first_ED_admission < start_test ~ "Val",
                                     first_ED_admission < end_model_period ~ "Test",
                                     TRUE ~ "After")]
    
    dataset = merge(dataset, csn_lookup[, .(csn, in_set)], by = "csn")
    
  } else {
    
    dataset[, in_set := case_when(first_ED_admission < start_model_period ~ "Before",
                                  first_ED_admission < start_val ~ "Train",
                                  first_ED_admission < start_test ~ "Val",
                                  first_ED_admission < end_model_period ~ "Test",
                                  TRUE ~ "After")]
    
    
    
  }
  
  
  
  print(paste("Dataset starts:", start_model_period)) 
  print(paste("Validation starts:", start_val))
  print(paste("Test starts", start_test))
  print(paste("Dataset ends:", end_model_period))  
  
  print(table(dataset$in_set))
  
  if (!is.na(return_dates_only)) {
    
    return(c(start_model_period, start_val, start_test, end_model_period))
    
  } else
  
  return(dataset)
}



# Plot chart to file ------------------------------------------------------

# Function used any time a chart needs to be saved

# plot chart to file
plot_chart = function(chart, file_name, width__ = NA, height__ = NA, layout = NA) {
  
  file_ = paste0("EDCrowding/predict-admission/media/", file_name, ".jpeg")
  
  if (!is.na(width__)) {
    
    width_ = width__
    height_ = height__
    
  } else if  (is.na(layout)) { # assume portrait
    
    width_ = 200
    height_ = 300
    
  } else { # assume it is for presentation
    
    width_ = 400
    height_ = 200
  }
  
  jpeg(file_, res = 300, width = width_, height = height_, units = "mm") 
  print(chart)
  dev.off()
}



# Functions for XGBoost ------------------------------------------



# set up parameters
train_learner <- function(learner, tsk, tsk_train_ids, 
                          # initialise params at default values
                          eval_metric = "logloss",
                          nrounds = 1,
                          max_depth = 6, 
                          min_child_weight = 1, 
                          gamma = 0,
                          subsample = 1,
                          colsample_bytree = 1,
                          eta = 0.3, 
                          scale_pos_weight = 1,
                          alpha = 0,
                          lambda = 1,
                          early_stopping_rounds = 10
) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "eval_metric" = eval_metric,
      "nrounds" = nrounds,
      "max_depth" = max_depth,
      "min_child_weight" = min_child_weight,
      "gamma" = gamma, 
      "subsample" = subsample,
      "colsample_bytree" = colsample_bytree,
      "eta" = eta,
      "scale_pos_weight" = scale_pos_weight,
      "alpha" = alpha,
      "lambda" = lambda
      
    )
  )
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  return(learner)
}

tune_learner <- function(name_tsk, tsk, learner, tsk_train_ids, tuning_round, scores, model_features, dataset,
                         # initialise params at default values
                         eval_metric = "logloss",
                         nrounds = 1,
                         max_depth = 6, 
                         min_child_weight = 1, 
                         gamma = 0,
                         subsample = 1,
                         colsample_bytree = 1,
                         eta = 0.3, 
                         scale_pos_weight = 1,
                         alpha = 0,
                         lambda = 1,
                         early_stopping_rounds = 10) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "eval_metric" = eval_metric,
      "nrounds" = nrounds,
      "max_depth" = max_depth,
      "min_child_weight" = min_child_weight,
      "gamma" = gamma, 
      "subsample" = subsample,
      "colsample_bytree" = colsample_bytree,
      "eta" = eta,
      "scale_pos_weight" = scale_pos_weight,
      "alpha" = alpha,
      "lambda" = lambda
      
    )
  )
  
  set.seed(17L)
  rr = resample(tsk, learner, rsmp("cv"), store_models = FALSE)
  
  score = data.table(dataset = dataset,
                     timeslice = name_tsk,
                     tsk_ids = "train",
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = rr$aggregate(msr("classif.logloss")),
                     bbrier = rr$aggregate(msr("classif.bbrier")), # binary brier score
                     prauc = rr$aggregate(msr("classif.prauc")),
                     auc = rr$aggregate(msr("classif.auc")),
                     acc = rr$aggregate(msr("classif.acc")),
                     bacc =  rr$aggregate(msr("classif.bacc")),
                     tp = rr$aggregate(msr("classif.tp")),
                     fp = rr$aggregate(msr("classif.fp")),
                     fn = rr$aggregate(msr("classif.fn")),
                     tn = rr$aggregate(msr("classif.tn")),
                     eval_metric = eval_metric,
                     nrounds = nrounds,
                     max_depth = max_depth,
                     min_child_weight = min_child_weight,
                     gamma = gamma, 
                     subsample = subsample,
                     colsample_bytree = colsample_bytree,
                     eta = eta,
                     scale_pos_weight = scale_pos_weight,
                     alpha = alpha,
                     lambda = lambda,
                     dttm = now()
  )
  
  scores <- bind_rows(scores, score)
}

update_learner <- function(learner, 
                           eval_metric = NA,
                           nrounds = NA,
                           max_depth = NA, 
                           min_child_weight = NA, 
                           gamma = NA,
                           subsample = NA,
                           colsample_bytree = NA,
                           eta = NA, 
                           scale_pos_weight = NA,
                           alpha = NA,
                           lambda = NA,
                           early_stopping_rounds = NA) {
  
  if (!is.na(eval_metric)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("eval_metric" = eval_metric))
  }
  
  if (!is.na(nrounds)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("nrounds" = nrounds))
  }
  
  if (!is.na(max_depth)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("max_depth" = max_depth))
  }
  
  if (!is.na(min_child_weight)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("min_child_weight" = min_child_weight))
  }
  
  if (!is.na(gamma)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("gamma" = gamma))
  }
  
  if (!is.na(subsample)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("subsample" = subsample))
  }
  
  if (!is.na(colsample_bytree)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("colsample_bytree" = colsample_bytree))
  }
  
  if (!is.na(eta)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("eta" = eta))
  }
  
  if (!is.na(scale_pos_weight)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("scale_pos_weight" = scale_pos_weight))
  }
  
  if (!is.na(alpha)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("alpha" = alpha))
  }
  
  if (!is.na(lambda)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("lambda" = lambda))
  }
  
  return(learner)
}

get_preds <- function(name_tsk, tsk, learner, train_or_val_ids, tsk_ids, tuning_round, param_value, preds, model_features, dataset) {
  pred_values = learner$predict(tsk, row_ids = train_or_val_ids)
  
  pred <- as.data.table(pred_values)
  pred[, dataset := dataset]
  pred[, model_features := model_features]
  pred[, timeslice := name_tsk]
  pred[, tsk_ids := tsk_ids]
  pred[, tuning_round := tuning_round]
  pred[, param_value := param_value]
  pred[, dttm := now()]
  
  preds <- bind_rows(preds, pred)
  return(preds)
  
}

get_imps <- function(name_tsk, learner, tsk_ids, tuning_round, param_value, imps, model_features, dataset) {
  
  imp <- as.data.table(learner$importance())
  setnames(imp, "V1", "importance")
  imp[, dataset := dataset]
  imp[, model_features := model_features]
  imp[, feature := names(learner$importance())]
  imp[, timeslice := name_tsk]
  imp[, tsk_ids := tsk_ids]
  imp[, tuning_round := tuning_round]
  imp[, param_value := param_value]
  imp[, dttm := now()]
  
  imps <- bind_rows(imps, imp)
  return(imps)
  
}


save_results <- function(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids, tuning_round, scores, model_features, dataset) {
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # get predictions
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  
  score = data.table(dataset = dataset,
                     timeslice = name_tsk,
                     tsk_ids = tsk_ids,
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = pred_val$score(msr("classif.logloss")),
                     bbrier = pred_val$score(msr("classif.bbrier")), # binary brier score
                     prauc = pred_val$score(msr("classif.prauc")),
                     auc = pred_val$score(msr("classif.auc")),
                     acc = pred_val$score(msr("classif.acc")),
                     bacc = pred_val$score(msr("classif.bacc")),
                     tp = pred_val$score(msr("classif.tp")),
                     fp = pred_val$score(msr("classif.fp")),
                     fn = pred_val$score(msr("classif.fn")),
                     tn = pred_val$score(msr("classif.tn")),
                     eval_metric = learner$param_set$values$eval_metric,
                     nrounds = learner$param_set$values$nrounds,
                     max_depth = learner$param_set$values$max_depth,
                     min_child_weight = learner$param_set$values$min_child_weight,
                     gamma = learner$param_set$values$gamma, 
                     subsample = learner$param_set$values$subsample,
                     colsample_bytree = learner$param_set$values$colsample_bytree,
                     eta = learner$param_set$values$eta,
                     scale_pos_weight = learner$param_set$values$scale_pos_weight,
                     alpha = learner$param_set$values$alpha,
                     lambda = learner$param_set$values$lambda,
                     dttm = now()
  ) 
  
  scores <- bind_rows(scores, score)
  
  return(scores)
}

save_results_test_set <- function(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, 
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
                     acc = Accuracy(as.numeric(preds>.5), labels),
                     bacc = (Sensitivity(factor(as.numeric(labels)), as.numeric(preds>.5)) + 
                               Specificity(factor(as.numeric(labels)), as.numeric(preds>.5)) )/2,
                     eval_metric = learner$param_set$values$eval_metric,
                     nrounds = learner$param_set$values$nrounds,
                     max_depth = learner$param_set$values$max_depth,
                     min_child_weight = learner$param_set$values$min_child_weight,
                     gamma = learner$param_set$values$gamma, 
                     subsample = learner$param_set$values$subsample,
                     colsample_bytree = learner$param_set$values$colsample_bytree,
                     eta = learner$param_set$values$eta,
                     scale_pos_weight = learner$param_set$values$scale_pos_weight,
                     alpha = learner$param_set$values$alpha,
                     lambda = learner$param_set$values$lambda,
                     dttm = now()
  ) 
  
  scores <- bind_rows(scores, score)
  
  return(scores)
}



# Plot QQ chart  ----------------------------------

# This is called to produce qq plots

plot_qq_chart = function(num_time_pts, distr_coll, num_adm, return_data = FALSE, tsk_ids_,
                         title = "QQ plot comparing observed and predicted distribution", 
                         subtitle = "X axis is cdf aggregated across all predicted distributions; Y axis value is cdf of predicted distribution at observed number of admissions",
                         include_title = TRUE) {
  
  
  # now treat the full set of cdfs (all time points) as a single discrete variable
  distr_coll[, upper_M_discrete_value := cdf]
  distr_coll[, lower_M_discrete_value := lag(cdf), by = time_of_report]
  distr_coll[num_adm_pred == 0, lower_M_discrete_value := 0]
  
  # outFile = paste0("EDcrowding/predict-admission/data-output/predicted_distribution_",today(),".csv")
  # write.csv(distr_coll, file = outFile, row.names = FALSE)
  
  
  # for the array of middle cdf values (now considered a discrete distribution) work out its cdf
  
  lower_M = distr_coll[, .(value = lower_M_discrete_value), probs]
  upper_M = distr_coll[, .(value = upper_M_discrete_value), probs]
  mid_M = distr_coll[, .(value = (upper_M_discrete_value+lower_M_discrete_value)/2, probs)]
  setorder(mid_M, value)
  mid_M[, cum_weight := cumsum(probs)]
  mid_M[, cum_weight_normed := cumsum(probs)/num_time_pts]
  mid_M[, dist := "model mid"]
  
  
  # compare the observed values against their predicted distribution 
  # and find their position on the cdf; combine this into a distribution
  
  adm_coll = merge(num_adm, 
                   distr_coll[, .(time_of_report, num_adm = num_adm_pred, 
                                  lower_E = lower_M_discrete_value, 
                                  upper_E = upper_M_discrete_value,
                                  mid_E = (lower_M_discrete_value + upper_M_discrete_value)/2)], 
                   by = c("time_of_report", "num_adm"))
  
  setorder(adm_coll, mid_E)

    # for the array of middle cdf values (now considered a discrete distribution) work out its cdf
  
  lower_E = adm_coll[, .(value = lower_E)]
  upper_E = adm_coll[, .(value = upper_E)]
  mid_E = adm_coll[, .(value = (upper_E+lower_E)/2)]
  setorder(mid_E, value)
  mid_E_prob = mid_E[, .N, by = value]
  mid_E_prob[, cum_weight := N/num_time_pts]
  mid_E_prob[, cum_weight_normed := cumsum(cum_weight)]
  mid_E_prob[, dist := "actual mid"]
  
  # for each value of the probabilities on mid_E_prob, find the equivalent in mid_M
  
  plot_data_qq = data.table()
  
  for (i in 1:nrow(mid_E_prob)) {
    
    qq_data = data.table(value_on_CORU_chart = mid_E_prob$value[i],
                         observed_cdf = mid_E_prob$cum_weight_normed[i],
                         model_cdf = mid_M[value <= mid_E_prob$value[i], max(cum_weight_normed)],
                         model_value_at_this_point = mid_M[value <= mid_E_prob$value[i], max(value)])
    
    plot_data_qq = bind_rows(plot_data_qq, qq_data)
  }
  
  if (return_data) {
    
     return(plot_data_qq)
    
  } else {
    
    plot_colour = get(paste0("colour_", tsk_ids_))
    p = plot_data_qq %>% ggplot(aes(y = observed_cdf)) + geom_point(aes(x = model_cdf), colour = plot_colour) + geom_line(aes(x = observed_cdf)) +
      labs(title = subtitle,
           x = "Cdf of model distribution",
           y = "Cdf of observed distribution") +
      theme_classic() +
      # scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
      theme(legend.position = "none") 
    
    if (include_title) {
      p = p + labs(title = title, subtitle = subtitle)
    }
    
    return(p)
  }
  
  


  
}



# Predictions for all patients in ED at time point (Step 2 of pipeline)----------------------------

# takes all patients in the ED at a time point, and generates predictions for their
# probability of admission using saved ML models. 

# The T0 model is used if a patient has been in the ED for less than 15 min
# The T15 model is used if a patient has been in for less than 30 min etc

make_predictions = function(time_pts, summ, dm_file_date, model_date,  model_features,  model_period_) {
  
  in_ED_all = data.table()
  
  # get relevant timeslice for all in ED at that time point of interest
  
  for (i in (1:length(time_pts))) {
    
    in_ED = summ[first_ED_admission < time_pts[i] & left_ED > time_pts[i], 
                 .(csn, first_ED_admission, adm, 
                   left_ED,
                   elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
                   remaining = difftime(left_ED, time_pts[i], units = "mins"))]
    
    in_ED[, timeslice := case_when(elapsed < 15 ~ "task000",
                                   elapsed < 30 ~ "task015",
                                   elapsed < 60 ~ "task030",
                                   elapsed < 90 ~ "task060",
                                   elapsed < 120 ~ "task090",
                                   elapsed < 180 ~ "task120",
                                   elapsed < 240 ~ "task180",
                                   elapsed < 300 ~ "task240",
                                   elapsed < 360 ~ "task300",
                                   elapsed < 480 ~ "task360",
                                   elapsed < 720 ~ "task480",
                                   TRUE ~ "task720")]
    
    in_ED[, time_pt := time_pts[i]]
    
    in_ED_all = bind_rows(in_ED_all, in_ED)
    
  }
  # Get data to provide to model 
  
  timeslices <- gsub("task", "", unique(in_ED_all$timeslice))
  
  preds_all_ts <- data.table()
  
  for (ts_ in timeslices) {
    
    print(ts_)
    
    # load timeslice 
    inFile = paste0("EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
    # select csns of interest
    dt = dt[csn %in% in_ED_all[timeslice == paste0("task", ts_), csn]]
    csns <- dt$csn
    dt[, row_id := seq_len(nrow(dt))]
    
    # create vectors identifying test ids
    assign(paste0("task", ts_, "_ids"), dt$row_id)
    
    #  for combined dataset, include a flag for whether post Covid era or not
    if (model_period_ == "Pre + Post") {
      
      csn_lookup = summ[csn %in% dt[, csn], . (csn, first_ED_admission)]
      csn_lookup[, a_post_Covid := factor(first_ED_admission > covid_start, levels = c(TRUE, FALSE))]
      dt = merge(dt, csn_lookup[, .(csn, a_post_Covid)], by = "csn")
    }
    
    # remove row_id so not included in features
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
    
    # encode factors - note, if set dropUnusedLevels to false, no need to bring in additional features
    # this is only possible because validation set has been created at same time as training set 
    # therefore all values present in each factor will be present here
    ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=FALSE)
    ts[,adm:=as.factor(adm)] 
    
    name_tsk <- paste0("task", ts_)
    
    # # add other features used in training the model that might be missing from this one-hot process
    
    features_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", model_period_),
                            "_features_",name_tsk,"_",model_date, ".rda")
    load(features_file)
    
    ts_cols = colnames(ts)
    missing_features = feature_list[!feature_list %in% ts_cols]
    
    missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features)))
    
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
    
    
    
    # load learner
    
    learner_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                           "_", gsub(" +", "", model_period_),
                           "_learner_",name_tsk,"_",model_date, ".rda")
    load(learner_file)
    
    # get predictions - if doing real-time- predictions
    pred_values = as.data.table(predict(learner, ts, predict_type = "prob"))
    setnames(pred_values, c("1", "0"), c("prob.1", "prob.0"))
    pred_values$timeslice = name_tsk
    pred_values$csn = csns
    
    preds_all_ts <- bind_rows(preds_all_ts, pred_values)
    
  }
  
  return(list(in_ED_all, preds_all_ts))
  
}



# Predictions using midpoint of timeslices --------------------------------

# takes all patients in the ED at a time point, and generates predictions for their
# probability of admission using saved ML models. 

# It is as above with slightly different approach to which model is applied to each patient.
# This function was not used in the final paper. 

# Above: 
  # The T0 model is used if a patient has been in the ED for less than 15 min
  # The T15 model is used if a patient has been in for less than 30 min 
  # etc

# Here: 
  # The T0 model is used if a patient has been in the ED for less than 7.5 min 
  # (where 7.5 min is the midpoint between 0 and 15 min, the cutoff for the next timeslice)
  # The T15 model is used if a patient has been in for less than 22.5 min 
  # etc
  


make_predictions_using_midpoint = function(time_pts, summ, dm_file_date, model_date,  model_features,  model_period_) {
  
  # timeslices <- as.integer(c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720"))
  # cutoff = as.integer()
  # for (i in 1:length(timeslices)-1) {
  #   cutoff = c(cutoff, (timeslices[i] + timeslices[i+1])/2)
  #   
  # }
  
  in_ED_all = data.table()
  
  # get relevant timeslice for all in ED at that time point of interest
  
  for (i in (1:length(time_pts))) {
    
    in_ED = summ[first_ED_admission < time_pts[i] & left_ED > time_pts[i], 
                 .(csn, first_ED_admission, adm, 
                   left_ED,
                   elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
                   remaining = difftime(left_ED, time_pts[i], units = "mins"))]
    
    # assign timeslices based on the elapsed duration of the stay, relative to the cutoff  
    in_ED[, timeslice := case_when(elapsed < 7.5 ~ "task000",
                                   elapsed < 22.5 ~ "task015",
                                   elapsed < 45 ~ "task030",
                                   elapsed < 75 ~ "task060",
                                   elapsed < 105 ~ "task090",
                                   elapsed < 150 ~ "task120",
                                   elapsed < 210 ~ "task180",
                                   elapsed < 270 ~ "task240",
                                   elapsed < 330 ~ "task300",
                                   elapsed < 420 ~ "task360",
                                   elapsed < 600 ~ "task480",
                                   TRUE ~ "task720")]
    
    # using this new method, any visit whose entire duration is less than their new timeslice will be missing
    # current fix: let these visits use the earlier timeslice
    
    in_ED[elapsed + remaining <= as.numeric(gsub("task", "", timeslice)),
          timeslice := case_when(elapsed < 15 ~ "task000",
                                 elapsed < 30 ~ "task015",
                                 elapsed < 60 ~ "task030",
                                 elapsed < 90 ~ "task060",
                                 elapsed < 120 ~ "task090",
                                 elapsed < 180 ~ "task120",
                                 elapsed < 240 ~ "task180",
                                 elapsed < 300 ~ "task240",
                                 elapsed < 360 ~ "task300",
                                 elapsed < 480 ~ "task360",
                                 elapsed < 720 ~ "task480",
                                 TRUE ~ "task720")]
    
    in_ED[, time_pt := time_pts[i]]
    
    in_ED_all = bind_rows(in_ED_all, in_ED)
  }
  
  # Get data to provide to model 
  
  timeslices <- gsub("task", "", unique(in_ED_all$timeslice))
  
  preds_all_ts <- data.table()
  
  for (ts_ in timeslices) {
    
    print(ts_)
    
    # load timeslice 
    inFile = paste0("EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
    # select csns of interest
    dt = dt[csn %in% in_ED_all[timeslice == paste0("task", ts_), csn]]
    csns <- dt$csn
    dt[, row_id := seq_len(nrow(dt))]
    
    # create vectors identifying test ids
    assign(paste0("task", ts_, "_ids"), dt$row_id)
    
    #  for combined dataset, include a flag for whether post Covid era or not
    if (model_period_ == "Pre + Post") {
      
      csn_lookup = summ[csn %in% dt[, csn], . (csn, first_ED_admission)]
      csn_lookup[, a_post_Covid := factor(first_ED_admission > covid_start, levels = c(TRUE, FALSE))]
      dt = merge(dt, csn_lookup[, .(csn, a_post_Covid)], by = "csn")
    }
    
    # remove row_id so not included in features
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
    
    
    # # Pre + Post learners needs two flags a_epoch_Pre and a_epoch_Post
    # # therefore set a_epoch to be a factor
    # dt$a_epoch = factor(dt$a_epoch, levels = c("Pre", "Post"))
    # 
    
    # encode factors - note, if set dropUnusedLevels to false, no need to bring in additional features
    # this is only possible because validation set has been created at same time as training set 
    # therefore all values present in eaach factor will be present here
    ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=FALSE)
    ts[,adm:=as.factor(adm)] 
    
    name_tsk <- paste0("task", ts_)
    
    # # add other features used in training the model that might be missing from this one-hot process
    
    features_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", model_period_),
                            "_features_",name_tsk,"_",model_date, ".rda")
    load(features_file)
    
    ts_cols = colnames(ts)
    missing_features = feature_list[!feature_list %in% ts_cols]
    
    missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features)))
    
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
    
    
    
    # load learner
    
    learner_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                           "_", gsub(" +", "", model_period_),
                           "_learner_",name_tsk,"_",model_date, ".rda")
    load(learner_file)
    
    # get predictions - if doing real-time- predictions
    pred_values = as.data.table(predict(learner, ts, predict_type = "prob"))
    setnames(pred_values, c("1", "0"), c("prob.1", "prob.0"))
    pred_values$timeslice = name_tsk
    pred_values$csn = csns
    
    preds_all_ts <- bind_rows(preds_all_ts, pred_values)
    
  }
  
  return(list(in_ED_all, preds_all_ts))
  
}


# Predictions using 3 models only -----------------------------------------

# As above, takes all patients in the ED at a time point, and generates predictions for their
# probability of admission using saved ML models. 

# This time, we only use 3 models: 
# T0 is used for visits up to 90 mins
# T90 is used for visits up to 240 min
# T240 is used for th rest

make_predictions_using_3_models = function(time_pts, summ, dm_file_date, model_date,  model_features,  model_period_) {
  
  # timeslices <- as.integer(c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720"))
  # cutoff = as.integer()
  # for (i in 1:length(timeslices)-1) {
  #   cutoff = c(cutoff, (timeslices[i] + timeslices[i+1])/2)
  #   
  # }
  
  in_ED_all = data.table()
  
  # get relevant timeslice for all in ED at that time point of interest
  
  for (i in (1:length(time_pts))) {
    
    in_ED = summ[first_ED_admission < time_pts[i] & left_ED > time_pts[i], 
                 .(csn, first_ED_admission, adm, 
                   left_ED,
                   elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
                   remaining = difftime(left_ED, time_pts[i], units = "mins"))]
    
    in_ED[, timeslice := case_when(
                                   elapsed < 90 ~ "task000",
                                   elapsed < 240 ~ "task090",
                                   TRUE ~ "task240")]
    
    in_ED[, time_pt := time_pts[i]]
    
    in_ED_all = bind_rows(in_ED_all, in_ED)
    
  }
  # Get data to provide to model 
  
  timeslices <- gsub("task", "", unique(in_ED_all$timeslice))
  
  preds_all_ts <- data.table()
  
  for (ts_ in timeslices) {
    
    print(ts_)
    
    # load timeslice 
    inFile = paste0("EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
    # select csns of interest
    dt = dt[csn %in% in_ED_all[timeslice == paste0("task", ts_), csn]]
    csns <- dt$csn
    dt[, row_id := seq_len(nrow(dt))]
    
    # create vectors identifying test ids
    assign(paste0("task", ts_, "_ids"), dt$row_id)
    
    #  for combined dataset, include a flag for whether post Covid era or not
    if (model_period_ == "Pre + Post") {
      
      csn_lookup = summ[csn %in% dt[, csn], . (csn, first_ED_admission)]
      csn_lookup[, a_post_Covid := factor(first_ED_admission > covid_start, levels = c(TRUE, FALSE))]
      dt = merge(dt, csn_lookup[, .(csn, a_post_Covid)], by = "csn")
    }
    
    # remove row_id so not included in features
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
    
    
    # # Pre + Post learners needs two flags a_epoch_Pre and a_epoch_Post
    # # therefore set a_epoch to be a factor
    # dt$a_epoch = factor(dt$a_epoch, levels = c("Pre", "Post"))
    # 
    
    # encode factors - note, if set dropUnusedLevels to false, no need to bring in additional features
    # this is only possible because validation set has been created at same time as training set 
    # therefore all values present in eaach factor will be present here
    ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=FALSE)
    ts[,adm:=as.factor(adm)] 
    
    name_tsk <- paste0("task", ts_)
    
    # # add other features used in training the model that might be missing from this one-hot process
    
    features_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", model_period_),
                            "_features_",name_tsk,"_",model_date, ".rda")
    load(features_file)
    
    ts_cols = colnames(ts)
    missing_features = feature_list[!feature_list %in% ts_cols]
    
    missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features)))
    
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
    
    
    
    # load learner
    
    learner_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features,
                           "_", gsub(" +", "", model_period_),
                           "_learner_",name_tsk,"_",model_date, ".rda")
    load(learner_file)
    
    # get predictions - if doing real-time- predictions
    pred_values = as.data.table(predict(learner, ts, predict_type = "prob"))
    setnames(pred_values, c("1", "0"), c("prob.1", "prob.0"))
    pred_values$timeslice = name_tsk
    pred_values$csn = csns
    
    preds_all_ts <- bind_rows(preds_all_ts, pred_values)
    
  }
  
  return(list(in_ED_all, preds_all_ts))
  
}
