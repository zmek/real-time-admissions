# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(cowplot)
library(gridExtra)
library(survival)
library(survminer)
library(scales)
library(RColorBrewer)


# Load functions ----------------------------------------------------------


# add config file that will load any parameters needed
source("~/EDcrowding/predict-admission/code/config.R")


# add utils file which will set training, val and test set date ranges
source("~/EDcrowding/predict-admission/code/utils.R")


format_value_ = function(value) {
  if (value > 1) {
    value_ = comma(value)
  } else {
    value_ = as.character(round(value, 2), nsmall = 2)
  }
  return(value_)
}
format_value_ = Vectorize(format_value_)


# Programme parameters ----------------------------------------------------


# for main study
algo = c("xgb")
model_features = c("alop")
model_features_ = c("alop")

tsk_ids = c("val", "test")
model_period = c("Pre", "Post", "Pre + Post", "Post-SDEC")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")

load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))


# # for dissertation
# algo = c("xgb", "rf")
# model_features = c("alop", "aop")
# tsk_ids = c("val", "test")
# model_period = c("Pre")

# Load performance data  ------------------------------


scores_all_xgb = data.table()

### NB may need to rerun post-SDEC XGB as scores file got messed up

# load xgb
for (model_features_ in model_features) {
  for (model_period_ in model_period) {
    
    model_file_date_name = paste0("model_output_date_", model_period_)
    model_file_date = get(model_file_date_name)
    
    scores_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features_, 
                          "_", model_period_, "_scores_", model_file_date,".rda")
    load(scores_file)
    setnames(scores, "dataset", "model_period")
    
    scores_all_xgb = bind_rows(scores_all_xgb, scores)
  }
}

scores_all_xgb[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                             labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]




load("~/EDcrowding/predict-admission/data-output/rf_alop_Pre_scores_2022-04-14.rda")
scores_all_rf =  scores
scores_all_rf$model_period = "Pre"
scores_all_rf[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                            labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]


load("~/EDcrowding/predict-admission/data-output/lr_alop_Pre_scores_2022-05-03.rda")
scores_all_lr =  scores
scores_all_lr$model_period = "Pre"
scores_all_lr[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                            labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]

# Load importances data ---------------------------------------------


imps_all = data.table()


for (model_period_ in model_period) {
  
  model_file_date_name = paste0("model_output_date_", model_period_)
  model_file_date = get(model_file_date_name)
  
  imps_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features_, 
                      "_", model_period_, "_imps_", model_file_date,".rda")
  load(imps_file)
  setnames(imps, "dataset", "model_period")
  
  imps_all = bind_rows(imps_all, imps)
}


imps_all[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                       labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]

load("~/EDcrowding/predict-admission/data-output/rf_alop_Pre_imps_2022-04-14.rda")
imps_rf = imps
imps_rf[, model_period_text := factor(dataset, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                      labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]

load("~/EDcrowding/predict-admission/data-output/lr_alop_Pre_imps_2022-05-03.rda")
imps_lr = imps
imps_lr = imps_lr[dttm >= "2022-05-03 19:44:51"]
imps_lr$model_period = "Pre"
imps_lr[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                      labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]


# Load data on distirbutions (pe_all and prob_dist) ----------------------------------------------------------------


tsk_ids = c("test")

pe_all = data.table()
prob_dist_all = data.table()


for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in model_period) {
      
      name_ = paste0("model_eval_date_", model_period_, "_", tsk_ids_)
      model_eval_file_date = get(name_)
      
      model_eval_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features_, "_", model_period_, "_", 
                               tsk_ids_, "_", model_eval_file_date,".rda")
      load(model_eval_file)
      
      # load probability distribution
      prob_dist_ = prob_dist[[1]]
      
      prob_dist_[, model_period := model_period_]
      prob_dist_[, model_features := model_features_]
      prob_dist_[, tsk_ids := tsk_ids_]
      
      prob_dist_all = bind_rows(prob_dist_all, prob_dist_)
      
      # load results
      
      pe = prob_dist[[2]]
      
      pe[, model_period := model_period_]
      pe[, model_features := model_features_]
      pe[, tsk_ids := tsk_ids_]
      
      pe_all = bind_rows(pe_all, pe)
      # }
      
      
    }
  }
}


pe_all[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                     labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]

# add data adjusting using sliding window curve

for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in c("Post", "Post-SDEC")) {
      
      name_ = paste0("model_eval_date_six_week_rolling_", model_period_, "_", tsk_ids_)
      model_eval_file_date = get(name_)
      
      model_eval_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features_, "_", model_period_, "_", 
                               tsk_ids_, "_six_week_rolling_", model_eval_file_date,".rda")
      load(model_eval_file)
      
      # load probability distribution
      prob_dist_ = prob_dist[[1]]
      
      prob_dist_[, model_period := paste0(model_period_, "_6wk")]
      prob_dist_[, model_features := model_features_]
      prob_dist_[, tsk_ids := tsk_ids_]
      
      prob_dist_all = bind_rows(prob_dist_all, prob_dist_)
      
      # load results
      
      pe = prob_dist[[2]]
      
      pe[, model_period  := paste0(model_period_, "_6wk")]
      pe[, model_features := model_features_]
      pe[, tsk_ids := tsk_ids_]
      
      pe_all = bind_rows(pe_all, pe)
      # }
      
      
    }
  }
}

pe_all[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Post_6wk", "Pre + Post", "Post-SDEC", "Post-SDEC_6wk"),
                                     labels = c("Pre-Covid", "Covid", "Covid with SWSC", "Pre + Post Covid", "Post-SDEC", "Post-SDEC with SWSC"))]


## qq data for 3 models only 


tsk_ids = c("test")

pe_all_3_models = data.table()
prob_dist_all_3_models = data.table()


for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in model_period) {
      
      # name_ = paste0("model_eval_date_", model_period_, "_", tsk_ids_)
      # model_eval_file_date = get(name_)
      model_eval_file_date = "2022-05-05"
      
      model_eval_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_3_models_", model_features_, "_", model_period_, "_", 
                               tsk_ids_, "_", model_eval_file_date,".rda")
      load(model_eval_file)
      
      # load probability distribution
      prob_dist_ = prob_dist[[1]]
      
      prob_dist_[, model_period := model_period_]
      prob_dist_[, model_features := model_features_]
      prob_dist_[, tsk_ids := tsk_ids_]
      
      prob_dist_all_3_models = bind_rows(prob_dist_all_3_models, prob_dist_)
      
      # load results
      
      pe = prob_dist[[2]]
      
      pe[, model_period := model_period_]
      pe[, model_features := model_features_]
      pe[, tsk_ids := tsk_ids_]
      
      pe_all_3_models = bind_rows(pe_all_3_models, pe)
      # }
      
      
    }
  }
}


pe_all_3_models[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                              labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]

# Prepare QQ plot ---------------------------------------------------------



qq_plot_data = data.table()

for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in model_period) {
      
      
      
      # time window, excluding not yet arrived
      
      for (time_window_ in c(4,8)) {
        
        
        # no time window - BUT do it twice anyway for chart formatting
        
        distr_coll = prob_dist_all[model_period == model_period_ & is.na(time_window) & 
                                     tsk_ids == tsk_ids_ & !inc_nya]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all[model_period == model_period_ & tsk_ids == tsk_ids_ & is.na(time_window) &
                           !inc_nya, .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = paste(model_period_, "COVID", tsk_ids_, "set; no time window"), 
                                return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        qq_plot$model_period = model_period_
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$type = "Distribution after Step 3:\nPatients currently in ED,\nadmitted at any time"
        
        qq_plot_data = bind_rows(qq_plot_data, qq_plot)
        
        #  excluding nya
        
        distr_coll = prob_dist_all[model_period == model_period_ & 
                                     tsk_ids == tsk_ids_ & time_window == time_window_ & !inc_nya]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all[model_period == model_period_ & tsk_ids == tsk_ids_ &
                           time_window == time_window_ & !inc_nya, .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = paste(model_period_, "Covid", tsk_ids_, "set - ", time_window_,
                                                 "hours"), return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        qq_plot$model_period = model_period_
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$type = "Distribution after Step 5:\nPatients currently in ED,\nadmitted within prediction window"
        
        qq_plot_data = bind_rows(qq_plot_data, qq_plot)
        
        
        # including not yet arrived
        
        distr_coll = prob_dist_all[model_period == model_period_ & 
                                     tsk_ids == tsk_ids_ & time_window == time_window_ & inc_nya]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all[model_period == model_period_ & tsk_ids == tsk_ids_ &
                           time_window == time_window_ & inc_nya, .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = paste(model_period_, "Covid", tsk_ids_, "set - ", time_window_,
                                                 "hours"), return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        qq_plot$model_period = model_period_
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$type = "Distribution after Step 7:\nPatients currently in ED\nand patients yet to arrive,\nadmitted within prediction window"
        
        qq_plot_data = bind_rows(qq_plot_data, qq_plot)
        
      }
      
    }
  }
}

qq_plot_data$swsc = FALSE




# get qq plot for nya with six week rolling


for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in c("Post", "Post-SDEC")) {
      
      # time window, excluding not yet arrived
      
      for (time_window_ in c(4,8)) {
        
        # time window, including not yet arrived
        
        distr_coll = prob_dist_all[(model_period == paste0(model_period_, "_6wk") & 
                                      tsk_ids == tsk_ids_ & time_window == time_window_ & inc_nya)]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all[(model_period == paste0(model_period_, "_6wk") & tsk_ids == tsk_ids_ &
                            time_window == time_window_ & inc_nya), .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = NA, return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        
        # NB - leave model_period as Post here so that plots work
        qq_plot$model_period = model_period_
        
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$swsc = TRUE
        
        
        qq_plot$type = "Distribution after Step 7:\nPatients currently in ED\nand patients yet to arrive,\nadmitted within prediction window\nusing SWSC at Step 4"
        
        qq_plot_data = bind_rows(qq_plot_data, qq_plot)
        
      }
    }
  }
}


# qq_plot_data[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Post_6wk", "Pre + Post", "Post-SDEC", "Post-SDEC_6wk"),
#                                            labels = c("Pre-Covid", "Covid", "Covid with SWSC", "Pre + Post Covid", "Post-SDEC", "Post-SDEC with SWSC"))]
# 
# qq_plot_data[, type_ := factor(type, levels = c("No time window",
#                                                 "4 hr window" ,
#                                                 "4 hr window including NYA" ,
#                                                 "4 hr window including NYA, with SWSC",
#                                                 "8 hr window" ,
#                                                 "8 hr window including NYA",
#                                                 "8 hr window including NYA, with SWSC"),
#                                labels =   c("At any time",
#                                             "In 4 hr prediction window" ,
#                                             "In 4 hr prediction window\nincluding NYA" ,
#                                             "In 4 hr prediction window\nincluding NYA, using SWSC",
#                                             "In 8 hr prediction window" ,
#                                             "In 8 hr prediction window\nincluding NYA",
#                                             "In 8 hr prediction window\nincluding NYA, with SWSC"))]
# 



# qq plot for 3 models only -----------------------------------------------




qq_plot_data_3_models = data.table()

for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in model_period) {
      
      
      
      # time window, excluding not yet arrived
      
      for (time_window_ in c(4,8)) {
        
        
        # no time window - BUT do it twice anyway for chart formatting
        
        distr_coll = prob_dist_all_3_models[model_period == model_period_ & is.na(time_window) & 
                                              tsk_ids == tsk_ids_ & !inc_nya]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all_3_models[model_period == model_period_ & tsk_ids == tsk_ids_ & is.na(time_window) &
                                    !inc_nya, .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = paste(model_period_, "COVID", tsk_ids_, "set; no time window"), 
                                return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        qq_plot$model_period = model_period_
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$type = "Distribution after Step 3:\nPatients currently in ED,\nadmitted at any time"
        
        qq_plot_data_3_models = bind_rows(qq_plot_data_3_models, qq_plot)
        
        #  excluding nya
        
        distr_coll = prob_dist_all_3_models[model_period == model_period_ & 
                                              tsk_ids == tsk_ids_ & time_window == time_window_ & !inc_nya]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all_3_models[model_period == model_period_ & tsk_ids == tsk_ids_ &
                                    time_window == time_window_ & !inc_nya, .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = paste(model_period_, "Covid", tsk_ids_, "set - ", time_window_,
                                                 "hours"), return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        qq_plot$model_period = model_period_
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$type = "Distribution after Step 5:\nPatients currently in ED,\nadmitted within prediction window"
        
        qq_plot_data_3_models = bind_rows(qq_plot_data_3_models, qq_plot)
        
        
        # including not yet arrived
        
        distr_coll = prob_dist_all_3_models[model_period == model_period_ & 
                                              tsk_ids == tsk_ids_ & time_window == time_window_ & inc_nya]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all_3_models[model_period == model_period_ & tsk_ids == tsk_ids_ &
                                    time_window == time_window_ & inc_nya, .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = paste(model_period_, "Covid", tsk_ids_, "set - ", time_window_,
                                                 "hours"), return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        qq_plot$model_period = model_period_
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$type = "Distribution after Step 7:\nPatients currently in ED\nand patients yet to arrive,\nadmitted within prediction window"
        
        qq_plot_data_3_models = bind_rows(qq_plot_data_3_models, qq_plot)
        
      }
      
    }
  }
}

qq_plot_data_3_models$swsc = FALSE




# get qq plot for nya with six week rolling


for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in c("Post", "Post-SDEC")) {
      
      # time window, excluding not yet arrived
      
      for (time_window_ in c(4,8)) {
        
        # time window, including not yet arrived
        
        distr_coll = prob_dist_all_3_models[(model_period == paste0(model_period_, "_6wk") & 
                                               tsk_ids == tsk_ids_ & time_window == time_window_ & inc_nya)]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all_3_models[(model_period == paste0(model_period_, "_6wk") & tsk_ids == tsk_ids_ &
                                     time_window == time_window_ & inc_nya), .(time_of_report, num_adm = truth)]
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = NA, return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        
        # NB - leave model_period as Post here so that plots work
        qq_plot$model_period = model_period_
        
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$swsc = TRUE
        
        
        qq_plot$type = "Distribution after Step 7:\nPatients currently in ED\nand patients yet to arrive,\nadmitted within prediction window\nusing SWSC at Step 4"
        
        qq_plot_data = bind_rows(qq_plot_data, qq_plot)
        
      }
    }
  }
}


# # Load admissions data for survival curves --------------------------------
# 

admitted = summ[adm %in% c("direct_adm", "indirect_adm")]
admitted[, duration := round(difftime(left_ED, first_ED_admission, units = "hours"), digits = 2)]
admitted[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]
admitted[, quarter := factor(case_when( month(first_ED_admission) <= 3 ~ 1,
                                        month(first_ED_admission) <= 6 ~ 2,
                                        month(first_ED_admission) <= 9 ~ 3,
                                        month(first_ED_admission) <= 12 ~ 4))]
admitted[, weekend := factor(if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]


admitted[, arrival_time := factor(case_when(hour(first_ED_admission) >= 22 | hour(first_ED_admission) < 6 ~ "22:00-06:00",
                                            hour(first_ED_admission) >= 6 & hour(first_ED_admission) < 12 ~ "06:00-12:00",
                                            hour(first_ED_admission) >= 12 & hour(first_ED_admission) < 16 ~ "12:00-16:00",
                                            hour(first_ED_admission) >= 16 & hour(first_ED_admission) < 22 ~ "16:00-22:00"))]

admitted$status = 1


# Update with number of patients in ED

load(paste0("~/EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",num_in_ED_at_arrival_time_file_date,".rda"))

num_in_ED = data.table(num_in_ED)
admitted[, DateTime := as.POSIXct(substr(first_ED_admission, 1, 16))]
admitted = merge(admitted, num_in_ED, by = c("DateTime", "quarter", "weekend", "arrival_time"))

# now set weekend as numeric variable (needed to be factor to merge)
admitted[, weekend := if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]
admitted[, num_MAJORS_RESUS := `num_ED MAJORS` + `num_ED RESUS`]
admitted[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS]

# add num SDEC for post-SDEC model only
admitted[, num_SDEC := num_SDEC + `num_SDEC Waiting`]



# Plot performance  ---------------------------------------------

# Get stats on dataset sizes

s = data.table(scores_all_xgb[tsk_ids == "test" & tuning_round == "final_preds"]  %>%
                 pivot_longer(c(auc,logloss, 
                                # bacc, acc
                 )) %>% select(model_period, model_period_text, timeslice, name, value, tuning_round, dttm))

s[, name_ := case_when(name == "auc" ~ "AUROC",
                       # name == "bacc" ~ "balanced accuracy",
                       # name == "acc" ~ "accuracy",
                       name == "logloss" ~ "log loss")]
s[, type_ := "Performance"]

s_rf = data.table(scores_all_rf[tsk_ids == "test" & tuning_round == "final_preds"]  %>%
                    pivot_longer(c(auc,logloss, 
                                   # bacc, acc
                    )) %>% select(model_period, model_period_text, timeslice, name, value, tuning_round, dttm))

s_rf[, name_ := case_when(name == "auc" ~ "AUROC",
                          # name == "bacc" ~ "balanced accuracy",
                          # name == "acc" ~ "accuracy",
                          name == "logloss" ~ "log loss")]
s_rf[, type_ := "Performance"]

scores_all_lr = scores_all_lr[dttm >= "2022-05-03 19:44:51"]

s_lr = data.table(scores_all_lr[tsk_ids == "test"]  %>%
                    pivot_longer(c(auc,logloss, 
                                   # bacc, acc
                    )) %>% select(model_period, model_period_text, timeslice, name, value, st_err_increment, dttm))

s_lr[, name_ := case_when(name == "auc" ~ "AUROC",
                          # name == "bacc" ~ "balanced accuracy",
                          # name == "acc" ~ "accuracy",
                          name == "logloss" ~ "log loss")]
s_lr[, type_ := "Performance"]

ts_stats = data.table()
for (model_period_ in model_period) {
  for (ts_ in gsub("task", "", unique(s$timeslice))) {
    
    # load timeslice 
    inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
    # set training, validation and test set dates
    dt = set_train_val_test_dates(dt, summ, model_period_)
    
    # get number of features
    model_file_date_name = paste0("model_output_date_", model_period_)
    model_file_date = get(model_file_date_name)
    
    name_tsk = paste0("task", ts_)
    features_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", model_period_), "_features_",name_tsk,"_",model_file_date, ".rda")
    load(features_file)
    feature_list_xgb = feature_list
    
    
    
    
    dt[in_set %in% c("Train", "Val", "Test"), .N, by = .(in_set, adm)] %>% pivot_wider(names_from = adm, values_from = N) %>% 
      mutate(perc_adm = `1`/(`1` + `0`)) %>% select(in_set, perc_adm)
    
    ts_stats_ = data.table(dt[in_set %in% c("Train", "Val", "Test"), .N, by = in_set])
    ts_stats_ = merge(ts_stats_, 
                      dt[in_set %in% c("Train", "Val", "Test"), .N, by = .(in_set, adm)] %>% pivot_wider(names_from = adm, values_from = N) %>% 
                        mutate(perc_adm = `1`/(`1` + `0`)) %>% select(in_set, perc_adm))
    ts_stats_$model_period = model_period_
    ts_stats_$timeslice = paste0("task", ts_)
    ts_stats_$num_vars_xgb = length(feature_list_xgb) - 2
    
    if (model_period_ == "Pre") {
      features_file <- paste0("~/EDcrowding/predict-admission/data-output/rf_",model_features,
                              "_", gsub(" +", "", model_period_), "_features_",name_tsk,"_","2022-04-14", ".rda")
      load(features_file)
      feature_list_rf = feature_list
      
      ts_stats_$num_vars_rf = length(feature_list_rf) - 2
    }
    
    
    ts_stats = bind_rows(ts_stats, ts_stats_)
    
  }
  
}

ts_stats[, name_N := case_when(in_set ==  "Val" ~ "validation set",
                               in_set == "Test"~ "test set",
                               TRUE ~ "training set")]
ts_stats[, name_perc := case_when(in_set ==  "Val" ~ "validation set",
                                  in_set == "Test"~ "test set",
                                  TRUE ~ "training set")]

ts_stats[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                       labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]


# Prepare scores for figures showing results ------------------------------------------------

# xgboost scores

s = bind_rows(s, ts_stats[
  # in_set != "Val"
  , .(model_period, model_period_text, timeslice, 
      name_ = name_N, value = round(N, 0), type_ = "N")])
s = bind_rows(s, ts_stats[
  # in_set != "Val"
  , .(model_period, model_period_text, timeslice, 
      name_ = name_perc, value = perc_adm, type_ = "% admitted")])

s[, type__ := factor(type_, levels = c("N", "% admitted", "Performance"),
                     labels = c("N", "% admitted", "performance"),)]
s[, name__ := factor(name_, levels = c("training set", "validation set", "test set", "AUROC", "log loss"))]
s[, value_ := format_value_(value)]
s[, value_ := if_else(nchar(value_) == 3 & value <=1, paste0(value_,"0"), value_)]
s[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
s$timeslice_ = factor(s$timeslice_, levels = unique(s$timeslice_[order(as.numeric(gsub("task","", s$timeslice)))]))
s
s$st_err_increment = "XGBoost"
s$param_value = "XGBoost"

# RF scores
s_rf[, type__ := factor(type_, levels = c("N", "% admitted", "Performance"),
                        labels = c("N", "% admitted", "performance"),)]
s_rf[, name__ := factor(name_, levels = c("training set", "validation set", "test set", "AUROC", "log loss"))]
s_rf[, value_ := format_value_(value)]
s_rf[, value_ := if_else(nchar(value_) == 3 & value <=1, paste0(value_,"0"), value_)]
s_rf[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
s_rf$timeslice_ = factor(s_rf$timeslice_, levels = unique(s_rf$timeslice_[order(as.numeric(gsub("task","", s_rf$timeslice)))]))

s_rf_no_ts = s_rf
s_rf_no_ts$st_err_increment = "Random Forest"
s_rf_no_ts$param_value = "Random Forest"

s_rf = bind_rows(s_rf, ts_stats[model_period == "Pre"
                                # in_set != "Val"
                                , .(model_period, model_period_text, timeslice,
                                    name_ = name_N, value = round(N, 0), type_ = "N")])
s_rf = bind_rows(s_rf, ts_stats[model_period == "Pre"
                                # in_set != "Val"
                                , .(model_period, model_period_text, timeslice,
                                    name_ = name_perc, value = perc_adm, type_ = "% admitted")])




# Logistic regression scores
s_lr[, type__ := factor(type_, levels = c("N", "% admitted", "Performance"),
                        labels = c("N", "% admitted", "performance"),)]
s_lr[, name__ := factor(name_, levels = c("training set", "validation set", "test set", "AUROC", "log loss"))]
s_lr[, value_ := format_value_(value)]
s_lr[, value_ := if_else(nchar(value_) == 3 & value <=1, paste0(value_,"0"), value_)]
s_lr[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
s_lr$timeslice_ = factor(s_lr$timeslice_, levels = unique(s_lr$timeslice_[order(as.numeric(gsub("task","", s_lr$timeslice)))]))



s_lr_no_ts = s_lr
s_lr = bind_rows(s_lr, ts_stats[model_period == "Pre"
                                # in_set != "Val"
                                , .(model_period, model_period_text, timeslice, 
                                    name_ = name_N, value = round(N, 0), type_ = "N")])
s_lr = bind_rows(s_lr, ts_stats[model_period == "Pre"
                                # in_set != "Val"
                                , .(model_period, model_period_text, timeslice, 
                                    name_ = name_perc, value = perc_adm, type_ = "% admitted")])


# Plot performance charts -------------------------------------------------


# plot performance main plot

plot_performance = s[model_period %in% c("Pre")] %>% 
  ggplot(aes(x = timeslice_,
             y = fct_rev(name__)))  +
  
  # facet_grid(type_~model_period_text, scales = "free", space = "free") +
  facet_grid(type__~., scales = "free", space = "free", switch = "y") +
  
  geom_tile(fill = "white") + 
  geom_text(aes(label = value_), size = 4)  +
  
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  labs(
    # title = paste("Model performance by timeslice: Pre- and Post-Covid"),
    x = "Model used for prediction",       y = "Metric") 

plot_performance
plot_chart(plot_performance, "performance_pre", width__ = 300, height__ = 80, layout = NA) 
plot_chart(plot_performance + 
             theme(strip.text = element_text(size=8)), "performance_pre_wide", width__ = 400, height__ = 80, layout = NA) 


# plot perforance for RF
plot_performance_rf = s_rf[model_period %in% c("Pre")] %>% 
  ggplot(aes(x = timeslice_,
             y = fct_rev(name__)))  +
  
  # facet_grid(type_~model_period_text, scales = "free", space = "free") +
  facet_grid(type__~., scales = "free", space = "free", switch = "y") +
  
  geom_tile(fill = "white") + 
  geom_text(aes(label = value_), size = 4)  +
  
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  labs(
    # title = paste("Model performance by timeslice: Pre- and Post-Covid"),
    x = "Model used for prediction",       y = "Metric") 

plot_performance_rf
plot_chart(plot_performance_rf, "plot_performance_rf_pre", width__ = 300, height__ = 80, layout = NA) 
plot_chart(plot_performance_rf + 
             theme(strip.text = element_text(size=8)), "plot_performance_rf_pre_wide", width__ = 400, height__ = 80, layout = NA) 



# Compare performances across all three  --------------------------------



# numbers of available features
imps_stats_xgb_rf = unique(ts_stats[, .(model_period, timeslice, N = num_vars_xgb, algo = "XGBoost")])
imps_stats_xgb_rf = bind_rows(imps_stats_xgb_rf, unique(ts_stats[!(is.na(num_vars_rf)), .(model_period, timeslice, N = num_vars_rf, algo = "Random Forest")]))
imps_stats_xgb_rf[, num_vars := paste0("(", min(N), "-", max(N), " features)"), by = .(algo, model_period)]


imps_lr$model_period = "Pre"
imps_stats_lr = imps_lr[variable != "(Intercept)", .N, by = .(model_period, st_err_increment = param_value, timeslice)]
imps_stats_lr[, num_vars := paste0("(", min(N), "-", max(N), " variables)"), by = .(model_period, st_err_increment)]
imps_stats_lr[, timeslice := NULL]
imps_stats_lr[, N := NULL]
imps_stats_lr = unique(imps_stats_lr)
vars_stats = imps_stats_lr[, algo := "Logistic Regression"]
vars_stats = bind_rows(vars_stats, data.table(model_period = "Pre",
                                              num_vars = unique(imps_stats_xgb_rf[algo == "XGBoost" & model_period == "Pre", num_vars]),
                                              st_err_increment = "XGBoost",
                                              algo = "XGBoost"))
vars_stats = bind_rows(vars_stats, data.table(model_period = "Pre",
                                              num_vars = unique(imps_stats_xgb_rf[algo == "Random Forest" & model_period == "Pre", num_vars]),
                                              st_err_increment = "Random Forest",
                                              algo = "Random Forest"))
vars_stats[, Algorithm := paste(algo, num_vars)]

vars_stats$order = c(seq(3,7,1), 1,2)
vars_stats[, Algorithm := fct_rev(factor(Algorithm, levels = vars_stats$Algorithm[order(vars_stats$order)]))]

s$algo = "XGBoost"
s_rf_no_ts$algo = "Random Forest"
s_lr_no_ts[, algo := paste("Logistic Regression")]

s_all_algos = bind_rows(s[model_period == "Pre" & type__ == "performance", 
                          .(model_period, model_period_text, timeslice_, name__, value, algo, st_err_increment)], 
                        s_rf_no_ts[model_period == "Pre" & type__ == "performance", 
                                   .(model_period, model_period_text, timeslice_, name__, value, algo, st_err_increment)], 
                        s_lr_no_ts[model_period == "Pre" & type__ == "performance", 
                                   .(model_period, model_period_text, timeslice_, name__, value, algo, st_err_increment)]) 
s_all_algos = merge(s_all_algos, vars_stats, by = c("model_period", "st_err_increment", "algo"))

s_all_algos[, `Lambda value` := factor(st_err_increment, levels = c("lambda.4se", "lambda.3se", "lambda.2se", "lambda.1se", "lambda.min"))]
s_all_algos[, Algorithm := factor(st_err_increment, levels = c("lambda.4se", "lambda.3se", "lambda.2se", "lambda.1se", "lambda.min", "Random Forest", "XGBoost"),
                                  labels = c("GLMNET with lambda.4se",
                                             "GLMNET with lambda.3se", 
                                             "GLMNET with lambda.2se", 
                                             "GLMNET with lambda.1se", 
                                             "GLMNET with lambda.min", "Random Forest", "XGBoost"))]


# s_all_algos[, Algorithm := fct_rev(factor(algo, levels = c("XGBoost", "Random Forest", "GLMNET with lambda.min", 
#                               "GLMNET with lambda.1se",  "GLMNET with lambda.2se",  "GLMNET with lambda.3se",  "GLMNET with lambda.4se")))]
plot_performance_all_algos = s_all_algos[name__ == "log loss" & st_err_increment != "lambda.1se"] %>% 
  # ggplot(aes(x = timeslice_, y = value, colour = Algorithm, group = Algorithm, linetype = Algorithm)) + 
  ggplot(aes(x = timeslice_, y = value, colour = Algorithm, group = Algorithm, linetype = Algorithm)) + 
  
  geom_line(size = .5) + geom_point(size = 3) +
  theme(legend.position = "bottom") + theme_bw() + 
  scale_colour_manual(values = c("#F8766D", "#F8766D","#F8766D","#F8766D", "#00BA38", "#619CFF" )) +
  scale_linetype_manual(values = c(3,4,2,1,1,1)) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  labs(
    # title = paste("Model performance by timeslice: Pre- and Post-Covid"),
    x = "Model used for prediction",       y = "Log loss") 


plot_performance_all_algos

# plot_chart(plot_performance_all_algos, "plot_performance_all_algos_pre", width__ = 300, height__ = 200, layout = NA) 
plot_chart(plot_performance_all_algos, "plot_performance_all_algos_pre_no_annotation", width__ = 300, height__ = 200, layout = NA) 

# add break on y axix


plot_chart(plot_performance_all_algos + scale_y_continuous(breaks = seq(0.3,0.55, 0.05), limits = c(0.275, 0.525)) ,
           "plot_performance_all_algos_pre_no_annotation", width__ = 300, height__ = 200, layout = NA) 



## add num vars

imps_stats = imps_lr[variable != '(Intercept)', .N, by = .(model_period, st_err_increment = param_value, timeslice)]
imps_stats$algo  = "Logistic Regression"
imps_stats_xgb = imps_stats_xgb_rf[model_period == "Pre" &  algo == "XGBoost", 
                                   .(model_period, st_err_increment = "XGBoost", timeslice, N, algo = "XGBoost")]
imps_stats_rf = imps_stats_xgb_rf[model_period == "Pre" &  algo == "Random Forest",
                                  .(model_period, st_err_increment = "Random Forest", timeslice, N, algo = "Random Forest")]
imps_stats = bind_rows(imps_stats, imps_stats_xgb, imps_stats_rf)
imps_stats[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
imps_stats$timeslice_ = factor(imps_stats$timeslice_, levels = unique(imps_stats$timeslice_[order(as.numeric(gsub("task","", imps_stats$timeslice)))]))

plot_data = unique(merge(s_all_algos[name__ == "log loss"], imps_stats, by = c("model_period", "st_err_increment", "timeslice_", "algo"), all.x = TRUE))

plot_performance_all_algos_annotated = plot_performance_all_algos

plot_data = plot_data[st_err_increment != "lambda.1se"]

for (i in 1:nrow(plot_data)) {
  
  if (plot_data$algo[i] == "Random Forest") {
    dot_colour = "#00BA38"
  } else if (plot_data$algo[i] == "XGBoost") {
    dot_colour =  "#619CFF"
  } else {
    dot_colour =  "#F8766D"
    
  }
  
  if (plot_data$timeslice_[i] %in% c("T0", "T90", "T240" ) & 
      plot_data$st_err_increment[i] != "lambda.1se") {
    plot_performance_all_algos_annotated = plot_performance_all_algos_annotated + 
      geom_point(size = 4, colour = "white", x = plot_data$timeslice_[i], y = plot_data$value[i]) +
      annotate("text", x = plot_data$timeslice_[i], y = plot_data$value[i], label = plot_data$N[i], size = 3, colour = dot_colour)
  }
  if (!plot_data$timeslice_[i] %in% c("T0", "T90", "T240") & plot_data$N[i] %in% c(5,9,21,28,48,54,172) &
      plot_data$st_err_increment[i] != "lambda.1se") {
    plot_performance_all_algos_annotated = plot_performance_all_algos_annotated + 
      geom_point(size = 4, colour = "white", x = plot_data$timeslice_[i], y = plot_data$value[i]) +
      annotate("text", x = plot_data$timeslice_[i], y = plot_data$value[i], label = plot_data$N[i], size = 3, colour = dot_colour)
  }
}


plot_performance_all_algos_annotated
plot_chart(plot_performance_all_algos_annotated, "plot_performance_all_algos_some_annotated_pre", width__ = 300, height__ = 200, layout = NA) 


# alternative plotting all annotations

plot_performance_all_algos_white_dots = s_all_algos[name__ == "log loss" & st_err_increment != "lambda.1se"] %>% 
  ggplot(aes(x = timeslice_, y = value, group = Algorithm, linetype = Algorithm)) + 
  geom_line(size = .5, aes(colour = Algorithm)) + geom_point(size = 3, colour = "white") +
  theme(legend.position = "bottom") + theme_bw() + 
  scale_colour_manual(values = c("#F8766D", "#F8766D","#F8766D","#F8766D", "#00BA38", "#619CFF" )) +
  scale_linetype_manual(values = c(3,4,2,1,1,1)) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  labs(
    # title = paste("Model performance by timeslice: Pre- and Post-Covid"),
    x = "Model used for prediction",       y = "Log loss") 


plot_performance_all_algos_white_dots

for (i in 1:nrow(plot_data)) {
  
  if (plot_data$algo[i] == "Random Forest") {
    dot_colour = "#00BA38"
  } else if (plot_data$algo[i] == "XGBoost") {
    dot_colour =  "#619CFF"
  } else {
    dot_colour =  "#F8766D"
    
  }
  plot_performance_all_algos_white_dots = plot_performance_all_algos_white_dots + 
    annotate("text", x = plot_data$timeslice_[i], y = plot_data$value[i], label = plot_data$N[i], size = 3, colour = dot_colour)
}

plot_performance_all_algos_white_dots

plot_chart(plot_performance_all_algos_white_dots, "plot_performance_all_algos_all_annotated_pre", width__ = 300, height__ = 200, layout = NA) 

# Just LR with annotations


plot_performance_lr_annotatation = s_all_algos[algo == "Logistic Regression" & name__ == "log loss" & st_err_increment != "lambda.1se"] %>% 
  # ggplot(aes(x = timeslice_, y = value, group = Algorithm, linetype = Algorithm)) + 
  ggplot(aes(x = timeslice_, y = value, group = `Lambda value`, linetype = `Lambda value`)) + 
  
  # geom_line(size = .5, aes(colour = st_err_increment)) + geom_point(size = 4, colour = "white") +
  geom_line(size = .5, aes(colour = `Lambda value`)) + geom_point(size = 4, colour = "white") +
  
  theme(legend.position = "bottom") + theme_bw() + 
  scale_colour_manual(values = c("#F8766D", "#F8766D","#F8766D","#F8766D", "#00BA38", "#619CFF" )) +
  scale_linetype_manual(values = c(3,4,2,1,1,1)) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  labs(
    # title = paste("Model performance by timeslice: Pre- and Post-Covid"),
    x = "Model used for prediction", 
    
    y = "Log loss") 

plot_performance_lr_annotatation
plot_data_lr = plot_data[algo == "Logistic Regression"]
for (i in 1:nrow(plot_data_lr)) {
  
  
  dot_colour =  "#F8766D"
  
  plot_performance_lr_annotatation = plot_performance_lr_annotatation + 
    annotate("text", x = plot_data_lr$timeslice_[i], y = plot_data_lr$value[i], label = plot_data_lr$N[i], size = 4, colour = dot_colour, fontface =2)
}

plot_performance_lr_annotatation

plot_chart(plot_performance_lr_annotatation + scale_y_continuous(breaks = seq(0.3,0.55, 0.05), limits = c(0.275, 0.525)) ,
           "plot_performance_lr_all_annotated_pre", width__ = 300, height__ = 200, layout = NA) 







# # Plot figure showing performance for supp materials --------------------


for (model_period_ in c("Pre", "Post", "Post-SDEC")) {
  
  plot_ = s[model_period == model_period_]  %>% 
    ggplot(aes(x = timeslice_,
               y = fct_rev(name__)))  +
    facet_grid(type__~model_period_text, scales = "free", space = "free", switch = "y") +
    # facet_wrap(type_~model_period_text, ncol = 1) +
    
    geom_tile(fill = "white") + 
    geom_text(aes(label = value_), size = 4)  +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          # plot.subtitle = element_text(size = 8),
          # plot.title = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size=12))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(
      # title = paste("Model performance by timeslice: Pre- and Post-Covid"),
      x = "Model used for prediction",       y = "Metric") 
  
  
  name_ = paste0("plot_perf_s_",model_period_)
  assign(name_, plot_)
}

plot_performance_supp = plot_grid(plot_perf_s_Pre, plot_perf_s_Post, `plot_perf_s_Post-SDEC`, ncol = 1)
plot_performance_supp

plot_chart(plot_performance_supp, "performance_3_models", width__ = 300, height__ = 400, layout = NA) 
# plot_chart(plot_performance_supp, "performance_3_models_wide", width__ = 400, height__ = 60, layout = NA) 


# Prepare importances data ------------------------------------------------




# prepre Pre Covid and post Post only
# commented out the code needed for Post Covid for now

# imps_prepost = imps_all[model_period %in% c("Pre") & importance > 0.01]
# pre_missing = imps_prepost[, .N, by = .(model_period_text, feature )] %>% 
#   pivot_wider(names_from = model_period_text, values_from = N) %>% 
#   filter(is.na(`Pre-Covid`)) %>% select(feature) %>% mutate(importance = NA, model_period_text = "Pre-Covid")
# 
# pre_ = data.table()
# for (ts_ in timeslices) {
#   
#   pre_ = bind_rows(pre_, data.table(pre_missing, timeslice = paste0( "task", ts_)))
# }
# 
# 
# post_missing = imps_prepost[, .N, by = .(model_period_text, feature )] %>% 
#   pivot_wider(names_from = model_period_text, values_from = N) %>% 
#   filter(is.na(`Post-Covid`)) %>% select(feature) %>% mutate(importance = NA, model_period_text = "Post-Covid")
# 
# post_ = data.table()
# for (ts_ in timeslices) {
#   
#   post_ = bind_rows(post_, data.table(post_missing, timeslice = paste0( "task", ts_)))
# }

# prepare other models for supp materials

imps_all_models = imps_all[
  model_period %in% c("Pre", "Post", "Post-SDEC") &
    importance > 0.01]



pre_s_missing = imps_all_models[, .N, by = .(model_period_text, feature )] %>% 
  pivot_wider(names_from = model_period_text, values_from = N) %>% 
  filter(is.na(`Pre-Covid`)) %>% select(feature) %>% mutate(importance = NA, model_period_text = "Pre-Covid")

pre_s = data.table()
for (ts_ in timeslices) {
  
  pre_s = bind_rows(pre_s, data.table(pre_s_missing, timeslice = paste0( "task", ts_)))
}


post_s_missing = imps_all_models[, .N, by = .(model_period_text, feature )] %>% 
  pivot_wider(names_from = model_period_text, values_from = N) %>% 
  filter(is.na(Covid)) %>% select(feature) %>% mutate(importance = NA, model_period_text = "Covid")

post_s = data.table()
for (ts_ in timeslices) {
  
  post_s = bind_rows(post_s, data.table(post_s_missing, timeslice = paste0( "task", ts_)))
}

# pre_post_s_missing = imps_all_models[, .N, by = .(model_period_text, feature )] %>% 
#   pivot_wider(names_from = model_period_text, values_from = N) %>% 
#   filter(is.na(`Pre + Post Covid`)) %>% select(feature) %>% mutate(importance = NA, model_period_text = "Pre + Post Covid")
# 
# pre_post_s = data.table()
# for (ts_ in timeslices) {
#   
#   pre_post_s = bind_rows(pre_post_s, data.table(pre_post_s_missing, timeslice = paste0( "task", ts_)))
# }


post_SDEC_s_missing = imps_all_models[, .N, by = .(model_period_text, feature )] %>% 
  pivot_wider(names_from = model_period_text, values_from = N) %>% 
  filter(is.na(`Post-SDEC`)) %>% select(feature) %>% mutate(importance = NA, model_period_text = "Post-SDEC")

post_SDEC_s = data.table()
for (ts_ in timeslices) {
  
  post_SDEC_s = bind_rows(post_SDEC_s, data.table(post_SDEC_s_missing, timeslice = paste0( "task", ts_)))
}


# Plot importances main study ---------------------------------------------


# plot_data = bind_rows(bind_rows(imps_prepost, pre_), post_) 
plot_data = imps_all[model_period %in% c("Pre") & importance > 0.01]

plot_data[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
plot_data$timeslice_ = factor(plot_data$timeslice_, levels = unique(plot_data$timeslice_[order(as.numeric(gsub("task","", plot_data$timeslice)))]))

plot_data[, feature := case_when(feature == 'a_num_consults' ~ "o_consults",
                                 TRUE ~ feature)]

plot_data[, imp_type := case_when(substring(feature, 1,1) == "a" ~ "Arrival & prior visits",
                                  substring(feature, 1,1) == "l" ~ "Location",
                                  substring(feature, 1,1) == "o" ~ "Observations & consults",
                                  TRUE ~ "Pathology")]

plot_data[, feature_ := substring(feature, 3, nchar(feature))]


plot_data[, total_imp := sum(importance, na.rm = TRUE), by = .(model_period, timeslice)]
plot_data[, relative_imp := importance/total_imp]

# plot_data[, model_period_text := factor(model_period_text, levels  = c("Pre-Covid", "Post-Covid", "Pre + Post Covid", "Post-SDEC"))]
plot_data[, model_period_text := "Data from 1 May 2019 to 18 March 2020 (Pre-Covid)"]
plot_data$feature_ = gsub("TAF", "MHS", plot_data$feature_)

# Plot figure showing importances
plot_imp = plot_data %>%   
  ggplot(aes(x = timeslice_,
             y = reorder(feature_, desc(feature_)), fill = relative_imp)) + geom_tile() +
  labs(
    # title = paste("Feature importances by timeslice: Pre Covid"),
    #    subtitle = "Including features with importance > 0.01",
    fill = "Relative importance of feature in model",
    x = "Model used for prediction",
    y = "Feature (included if feature importance score > 0.01)") + theme_bw()  +
  
  # facet_grid(imp_type~, scales = "free", space = "free") +
  facet_grid(imp_type~model_period_text, scales = "free", space = "free", switch = "y") +
  
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # scale_fill_gradient(low="white", high="red", na.value = "grey90")
  # # from https://stackoverflow.com/questions/21758175/is-it-possible-to-define-the-mid-range-in-scale-fill-gradient2
  scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd"),
                       values = scales::rescale(c(0, 0.1, 0.25, 0.5)) , na.value = "grey90")


plot_imp 
plot_chart(plot_imp, "feature_importances_main_study", width__ = 250, height__ = 400, layout = NA) 


# combine plots
plot_imp_main = plot_grid(plot_imp, plot_performance +
                            theme(plot.margin=unit(c(t = 0, r = 0.25, b = 0, l = 4.75),"cm"),
                                  strip.text = element_text(size=10)), 
                          ncol = 1, rel_heights  = c(3/4, 1/4),
                          labels = c("a", "b"))

plot_imp_main
# plot_chart(plot_imp_main, "feature_importances_and_perf_main_study", width__ = 300, height__ = 200, layout = NA) 
# plot_chart(plot_imp_main, "feature_importances_and_perf_main_study_wide", width__ = 400, height__ = 200, layout = NA)
plot_chart(plot_imp_main, "feature_importances_and_perf_main_study_tall", width__ = 275, height__ = 400, layout = NA)



# plot importances rf

# plot_data = bind_rows(bind_rows(imps_prepost, pre_), post_) 

imps_rf = imps_rf[importance >0]
imps_rf[, total_imp := sum(importance, na.rm = TRUE), by = .( timeslice)]
imps_rf[, relative_imp := importance/total_imp]

y_cutoff = 0.001
plot_data_rf = imps_rf[importance > y_cutoff]

plot_data_rf[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
plot_data_rf$timeslice_ = factor(plot_data_rf$timeslice_, levels = unique(plot_data_rf$timeslice_[order(as.numeric(gsub("task","", plot_data_rf$timeslice)))]))

plot_data_rf[, feature := case_when(feature == 'a_num_consults' ~ "o_consults",
                                    TRUE ~ feature)]

plot_data_rf[, imp_type := case_when(substring(feature, 1,1) == "a" ~ "Arrival & prior visits",
                                     substring(feature, 1,1) == "l" ~ "Location",
                                     substring(feature, 1,1) == "o" ~ "Observations & consults",
                                     TRUE ~ "Pathology")]

plot_data_rf[, feature_ := substring(feature, 3, nchar(feature))]
plot_data_rf$model_period = "Pre"

# plot_data_rf[, total_imp := sum(importance, na.rm = TRUE), by = .(model_period, timeslice)]
# plot_data_rf[, relative_imp := importance/total_imp]

# plot_data_rf[, model_period_text := factor(model_period_text, levels  = c("Pre-Covid", "Post-Covid", "Pre + Post Covid", "Post-SDEC"))]
plot_data_rf[, model_period_text := "Data from 1 May 2019 to 18 March 2020 (Pre-Covid)"]
plot_data_rf$feature_ = gsub("TAF", "MHS", plot_data_rf$feature_)

# Plot figure showing importances
plot_imp_rf = plot_data_rf[feature !=  "o_num_IdealBodyweightcalculated"] %>%   
  ggplot(aes(x = timeslice_,
             y = reorder(feature_, desc(feature_)), fill = relative_imp)) + geom_tile() +
  labs(
    # title = paste("Feature importances by timeslice: Pre Covid"),
    #    subtitle = "Including features with importance > 0.01",
    fill = "Relative importance of feature in model",
    x = "Model used for prediction",
    y = paste0("Variable (included if feature importance score > ", y_cutoff, ")")) + theme_bw()  +
  
  # facet_grid(imp_type~, scales = "free", space = "free") +
  facet_grid(imp_type~model_period_text, scales = "free", space = "free", switch = "y") +
  
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # scale_fill_gradient(low="white", high="red", na.value = "grey90")
  # # from https://stackoverflow.com/questions/21758175/is-it-possible-to-define-the-mid-range-in-scale-fill-gradient2
  scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd"),
                       values = scales::rescale(c(0, 0.1, 0.25, 0.5)) , na.value = "grey90", labels = label_number(accuracy = 0.1) 
  )


plot_imp_rf 
plot_chart(plot_imp_rf, "feature_importances_rf_main_study", width__ = 250, height__ = 400, layout = NA) 




# combine plots
plot_imp_rf_main = plot_grid(plot_imp_rf, plot_performance_rf +
                               theme(plot.margin=unit(c(t = 0, r = 0.25, b = 0, l = 4.75),"cm"),
                                     strip.text = element_text(size=10)), 
                             ncol = 1, rel_heights  = c(3/4, 1/4),
                             labels = c("a", "b"))

plot_imp_rf_main
# plot_chart(plot_imp_main, "feature_importances_and_perf_main_study", width__ = 300, height__ = 200, layout = NA) 
# plot_chart(plot_imp_main, "feature_importances_and_perf_main_study_wide", width__ = 400, height__ = 200, layout = NA)
plot_chart(plot_imp_rf_main, "feature_importances_and_perf_rf_main_study_tall", width__ = 275, height__ = 400, layout = NA)




# Plot coefficients for LR  -----------------------------

imps_lr[, variable := case_when(variable == 'a_num_consults' ~ "o_consults",
                                TRUE ~ variable)]


imps_lr[, imp_type := case_when(substring(variable, 1,1) == "a" ~ "Arrival & prior visits",
                                substring(variable, 1,1) == "l" ~ "Location",
                                substring(variable, 1,1) == "o" ~ "Observations & consults",
                                TRUE ~ "Pathology")]



imps_lr[, feature_ := substring(variable, 3, nchar(variable))]
imps_lr$feature_ = gsub("TAF", "MHS", imps_lr$feature_)


imps_lr[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
imps_lr$timeslice_ = factor(imps_lr$timeslice_, levels = unique(imps_lr$timeslice_[order(as.numeric(gsub("task","", imps_lr$timeslice)))]))


abs_cutoff = 0.05

plot_imps_lr_min = imps_lr[param_value == 'lambda.min' & variable != "(Intercept)" & abs(coef) > abs_cutoff] %>% 
  ggplot(aes(x = timeslice_, y =  reorder(feature_, desc(feature_)), fill = coef)) + geom_tile() +
  labs(
    
    fill = paste("Value of coefficient (standardised)"),
    x = "Model used for prediction",
    y = paste0("Variable (included if absolute value of coefficient > ", abs_cutoff, ")")) + theme_bw()  +   
  scale_fill_distiller(palette = "RdBu") +
  scale_color_distiller(palette = "RdBu") +
  # scale_fill_gradientn(colours = brewer.pal(9,"RdBu"),
  #                      values = scales::rescale(c(-1, -0.01, 0.01, 1)), na.value = "grey90") +
  facet_grid(imp_type~., scales = "free", space = "free", switch = "y") +
  
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot_imps_lr_min
plot_imps_lr_1se = imps_lr[param_value == 'lambda.1se' & variable != "(Intercept)" & 
                             variable != "o_num_IdealBodyweightcalculated"               ] %>% 
  ggplot(aes(x = timeslice_, y =  reorder(feature_, desc(feature_)), fill = coef)) + geom_tile() +
  labs(
    # title = paste("Feature importances by timeslice: Pre Covid"),
    #    subtitle = "Including features with importance > 0.01",
    fill = paste("Value of coefficient (standardised)"),
    x = "Model used for prediction",
    y = paste("Variable (all)")) + theme_bw()  +  
  scale_fill_distiller(palette = "RdBu") + 
  scale_color_distiller(palette = "RdBu") + 
  
  
  # facet_grid(imp_type~, scales = "free", space = "free") +
  facet_grid(imp_type~., scales = "free", space = "free", switch = "y") +
  
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_imps_lr_2se = imps_lr[param_value == 'lambda.2se' & variable != "(Intercept)" & 
                             variable != "o_num_IdealBodyweightcalculated"               ] %>% 
  ggplot(aes(x = timeslice_, y =  reorder(feature_, desc(feature_)), fill = coef)) + geom_tile() +
  labs(
    # title = paste("Feature importances by timeslice: Pre Covid"),
    #    subtitle = "Including features with importance > 0.01",
    fill = paste("Value of coefficient (standardised)"),
    x = "Model used for prediction",
    y = paste("Variable (all)")) + theme_bw()  +  
  scale_fill_distiller(palette = "RdBu") + 
  scale_color_distiller(palette = "RdBu") + 
  
  
  # facet_grid(imp_type~, scales = "free", space = "free") +
  facet_grid(imp_type~., scales = "free", space = "free", switch = "y") +
  
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_imps_lr_3se = imps_lr[param_value == 'lambda.3se' & variable != "(Intercept)" & 
                             variable != "o_num_IdealBodyweightcalculated"               ] %>% 
  ggplot(aes(x = timeslice_, y =  reorder(feature_, desc(feature_)), fill = coef)) + geom_tile() +
  labs(
    # title = paste("Feature importances by timeslice: Pre Covid"),
    #    subtitle = "Including features with importance > 0.01",
    fill = paste("Value of coefficient (standardised)"),
    x = "Model used for prediction",
    y = paste("Variable (all)")) + theme_bw()  +  
  scale_fill_distiller(palette = "RdBu") + 
  scale_color_distiller(palette = "RdBu") + 
  
  
  # facet_grid(imp_type~, scales = "free", space = "free") +
  facet_grid(imp_type~., scales = "free", space = "free", switch = "y") +
  
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_imps_lr_4se = imps_lr[param_value == 'lambda.4se' & variable != "(Intercept)" & 
                             variable != "o_num_IdealBodyweightcalculated"               ] %>% 
  ggplot(aes(x = timeslice_, y =  reorder(feature_, desc(feature_)), fill = coef)) + geom_tile() +
  labs(
    # title = paste("Feature importances by timeslice: Pre Covid"),
    #    subtitle = "Including features with importance > 0.01",
    fill = paste("Value of coefficient (standardised)"),
    x = "Model used for prediction",
    y = paste("Variable (all)")) + theme_bw()  +  
  scale_fill_distiller(palette = "RdBu", breaks = c(-0.5, 0, 0.5), labels = label_number(accuracy = 0.1)) + 
  scale_color_distiller(palette = "RdBu") + 
  
  
  # facet_grid(imp_type~, scales = "free", space = "free") +
  facet_grid(imp_type~., scales = "free", space = "free", switch = "y") +
  
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_chart(plot_imps_lr_min, "feature_importances_and_perf_lr_min", width__ = 275, height__ = 400, layout = NA)
plot_chart(plot_imps_lr_1se, "feature_importances_and_perf_lr_1se", width__ = 275, height__ = 400, layout = NA)
plot_chart(plot_imps_lr_2se, "feature_importances_and_perf_lr_2se", width__ = 275, height__ = 400, layout = NA)
plot_chart(plot_imps_lr_3se, "feature_importances_and_perf_lr_3se", width__ = 275, height__ = 400, layout = NA)
plot_chart(plot_imps_lr_4se, "feature_importances_and_perf_lr_4se", width__ = 275, height__ = 400, layout = NA)

# plot LR with performance

imps_lr[, algo := paste("GLMNET with", param_value)]
imps_lr[, Algorithm := factor(algo, levels = c("XGBoost", "Random Forest", "GLMNET with lambda.min", 
                                               "GLMNET with lambda.1se",  "GLMNET with lambda.2se",  "GLMNET with lambda.3se",  "GLMNET with lambda.4se"))]

plot_num_variables = imps_lr[, .N, by = .(timeslice_, Algorithm)] %>% 
  ggplot(aes(x = timeslice_, y = N, colour = Algorithm, group = Algorithm, linetype = Algorithm)) + 
  geom_line(size = 1) + #geom_point() +
  theme(legend.position = "bottom") + theme_bw() + 
  scale_colour_manual(values = c("#F8766D", "#F8766D","#F8766D","#F8766D","#F8766D", "#00BA38", "#619CFF" )) +
  scale_linetype_manual(values = c(1,5,2,4,3, 1, 1)) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  labs(
    # title = paste("Model performance by timeslice: Pre- and Post-Covid"),
    x = "Model used for prediction",       y = "Number of variables in model") 


# combine plots
plot_lr_perf = plot_grid(plot_performance_all_algos + geom_point(), plot_num_variables + geom_point()+ 
                           scale_y_continuous(breaks = seq(0,175,25)) +
                           theme(plot.margin=unit(c(t = 0, r = 0.25, b = 0, l = 0.25),"cm"),
                                 strip.text = element_text(size=10)), 
                         ncol = 1, rel_heights  = c(5/8, 3/8),
                         labels = c("a", "b"))

plot_chart(plot_lr_perf, "plot_lr_perf_and_num_vars", width__ = 275, height__ = 400, layout = NA)


# Plot importances supp material ---------------------------------------------


plot_data_s = bind_rows(bind_rows(imps_all_models, pre_s), post_s) 
plot_data_s = bind_rows(
  # bind_rows(
  plot_data_s, #pre_post_s), 
  post_SDEC_s) 

plot_data_s[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
plot_data_s$timeslice_ = factor(plot_data_s$timeslice_, levels = unique(plot_data_s$timeslice_[order(as.numeric(gsub("task","", plot_data_s$timeslice)))]))

plot_data_s[, feature := case_when(feature == 'a_num_consults' ~ "o_consults",
                                   TRUE ~ feature)]

plot_data_s[, imp_type := case_when(substring(feature, 1,1) == "a" ~ "Arrival & prior visits",
                                    substring(feature, 1,1) == "l" ~ "Location",
                                    substring(feature, 1,1) == "o" ~ "Observations & consults",
                                    TRUE ~ "Pathology")]

plot_data_s[, feature_ := substring(feature, 3, nchar(feature))]


plot_data_s[, total_imp := sum(importance, na.rm = TRUE), by = .(model_period, timeslice)]
plot_data_s[, relative_imp := importance/total_imp]

plot_data_s[, model_period_text := factor(model_period_text, levels  = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]

plot_data_s$feature_ = gsub("TAF", "MHS", plot_data_s$feature_)


# Plot figure showing importances
plot_imp_supp = plot_data_s %>% 
  ggplot(aes(x = timeslice_,
             y = reorder(feature_, desc(feature_)), fill = relative_imp)) + geom_tile() +
  labs(
    # title = paste("Feature importances by timeslice: Pre Covid"),
    #    subtitle = "Including features with importance > 0.01",
    fill = "Relative importance of feature in model",
    x = "Model used for prediction",
    y = "Feature (included if feature importance score > 0.01)") + theme_bw()  +
  facet_grid(imp_type~model_period_text, scales = "free", space = "free", switch = "y") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # scale_fill_gradient(low="white", high="red", na.value = "grey90")
  # # from https://stackoverflow.com/questions/21758175/is-it-possible-to-define-the-mid-range-in-scale-fill-gradient2
  scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd"),
                       values = scales::rescale(c(0, 0.1, 0.25, 0.5)), na.value = "grey90")


plot_imp_supp 

# plot_chart(plot_imp_supp +   theme(axis.text.x = element_text(size = 9)), "feature_importances_all_models", width__ = 300, height__ = 200, layout = NA) 
# plot_chart(plot_imp_supp, "feature_importances_all_models_wide", width__ = 400, height__ = 200, layout = NA) 
plot_chart(plot_imp_supp  + theme(axis.text.x = element_text(angle = 45,  hjust=1)), "feature_importances_all_models_tall", width__ = 275, height__ = 400, layout = NA) 


# extract new feature names using order of the original features column
features_for_glossary = data.table(unique(plot_data_s$feature_)[order(unique(plot_data_s$feature))])
write_csv(features_for_glossary, file = paste0("EDCrowding/predict-admission/media/", "feature_list_2", ".csv"))



# Load benchmark data --------------------------------------------------

# by report time
load("~/EDcrowding/predict-admission/model-output/benchmarks_by_report_time_2021-09-16.rda")
adm_report_time[, inc_nya := TRUE]

# for reference
# adm_report_time[, uch_metric := last_ref_day - num_adm_so_far_today]
# adm_report_time[, moving_avg_less_adm_so_far := six_wk_moving - num_adm_so_far_today]

# daily estimates
load("~/EDcrowding/predict-admission/model-output/benchmarks_by_day_2021-09-16.rda")
load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))


pe_rolling6 = data.table()
uch_method = data.table()

adm_day[, first_ED_admission := date] # to make function work

for (model_features_ in model_features)  {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in model_period) {
      
      # if (!(tsk_ids_ == "test" & model_period_ == "Post-SDEC")) {
      
      
      period_dates = set_train_val_test_dates(summ, NA, model_period_, TRUE)
      
      if (tsk_ids_ == "val") {
        
        start_of_set = period_dates[2]
        end_of_set = period_dates[3]
        
      } else if (tsk_ids_ == "test") {
        
        start_of_set = period_dates[3]
        end_of_set = period_dates[4]
      }
      
      pe_rolling6_ = adm_day[date >= start_of_set & date <= end_of_set]
      pe_rolling6_[, delta_exp_value := six_wk_moving - N]
      pe_rolling6_[, model_features := model_features_]
      pe_rolling6_[, tsk_ids := tsk_ids_]
      pe_rolling6_[, model_period := model_period_]
      
      pe_rolling6 = bind_rows(pe_rolling6, pe_rolling6_)
      
      uch_method_ = adm_report_time[date >= start_of_set & date <= end_of_set]
      uch_method_[, model_features := model_features_]
      uch_method_[, tsk_ids := tsk_ids_]
      uch_method_[, model_period := model_period_]
      
      uch_method = bind_rows(uch_method, uch_method_)
      
      
      
      # }
    }
  }
}





# # Plots of survival curve for train, test, val ---------------------------------------------
# 
admitted = admitted[duration < 24]

all_surv_curves = data.table()

for (model_period_ in c("Pre", "Post", "Post-SDEC")) {
  
  dataset_ = set_train_val_test_dates(admitted, NA, model_period_)
  
  for (tsk_ids_ in c("Train", "Val", "Test")) {
    
    # if (!(model_period_ == "Post-SDEC" & tsk_ids_ == "Test")) {
    
    dataset = dataset_[in_set == tsk_ids_]
    
    km_fit <- survfit(Surv(duration, status) ~ 1, data=dataset)
    all_models = data.table(    time = km_fit$time,
                                surv_km = km_fit$surv)
    
    
    empirical = numeric()
    for (i in (1:nrow(all_models))) {
      empirical <- c(empirical, sum(dataset[, duration > all_models$time[i]])/nrow(dataset))
    }
    
    all_models$empirical = empirical
    all_models$model_period = model_period_
    all_models$in_set = tsk_ids_
    
    all_surv_curves = bind_rows(all_surv_curves, all_models)
    # }
    
    
    
  }
  
}

all_surv_curves[, model_period_with_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                                   labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]
all_surv_curves[, in_set := factor(in_set, levels = c("Train", "Val", "Test"),
                                   labels = c("Training", "Validation", "Test"))]

# all survival curves in grid of four
plot_surv = all_surv_curves %>% ggplot(aes(x = time, y = empirical, colour = in_set)) + geom_line(size = .75) +
  facet_wrap(model_period_with_text~.) + theme_bw() +
  theme(legend.position = "bottom",
        aspect.ratio = 1:1) +
  labs(colour = "Set",
       # title = "Survival curves for time spent by admitted patients in ED by model period and dataset",
       x = "Time since arrival in ED (cut off at 24 hours)",
       y = "Survival probability") +
  scale_colour_manual(values = c( "#4891B9", "#F4B183", "#D53E4F", guide = NULL, name = NULL)) +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = .5)) 

plot_surv


plot_chart(plot_surv, "plot_surv", width__ = 300, height__ = 200, layout = NA) 
plot_chart(plot_surv, "plot_surv_wide", width__ = 400, height__ = 200, layout = NA) 
# 
# surv_post_test_set = data.table()
# 
# for (model_period_ in c("Post")) {
#   
#   dataset_ = set_train_val_test_dates(admitted, NA, model_period_)
#   dataset_test = dataset_[in_set == "Test"]
#   date_seq = seq.Date(max(date(dataset_test$DateTime)) - months(3), max(date(dataset_test$DateTime)), by = "1 month")
#   
#   for (j in 1:length(date_seq)) {
#     
#     # if (!(model_period_ == "Post-SDEC" & tsk_ids_ == "Test")) {
#     
#     dataset = dataset_[first_ED_admission >= date_seq[j] - months(3) & 
#                          first_ED_admission < date_seq[j] ]
#     
#     km_fit <- survfit(Surv(duration, status) ~ 1, data=dataset)
#     all_models = data.table(    time = km_fit$time,
#                                 surv_km = km_fit$surv)
#     
#     
#     empirical = numeric()
#     for (i in (1:nrow(all_models))) {
#       empirical <- c(empirical, sum(dataset[, duration > all_models$time[i]])/nrow(dataset))
#     }
#     
#     all_models$empirical = empirical
#     all_models$model_period = paste("3 months to", date_seq[j])
#     all_models$in_set = tsk_ids_
#     
#     surv_post_test_set = bind_rows(surv_post_test_set, all_models)
#     # }
#     
#     
#     
#   }
#   
# }
# 
# # all survival curves in grid of four
# surv_post_test_set %>% ggplot(aes(x = time, y = empirical, colour = model_period)) + geom_line(size = 1.25) +
#   theme_bw() + 
#   theme(legend.position = "bottom") + 
#   labs(colour = "Set",
#        title = "Survival curves for time spent by admitted patients in ED by model period and dataset",
#        x = "Time since arrival in ED (cut off at 24 hours)",
#        y = "Survival probability") +
#   # scale_colour_manual(values = c("#00BA38", "#619CFF","#F8766D", guide = NULL, name = NULL)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# 



# Plot QQ plots -----------------------------------------


# plot for four hours, Pre and Post Covid with six week rolling
qq_plot_data[, time_window_ := factor(case_when(grepl("Step 3", type) ~ "No prediction window applied",
                                                TRUE ~ time_window),
                                      levels = c("No prediction window applied", "4 hour prediction window", "8 hour prediction window"))]


qq_plot_data[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Post_6wk", "Pre + Post", "Post-SDEC", "Post-SDEC_6wk"),
                                           labels = c("Pre-Covid", "Covid", "Covid with SWSC", "Pre + Post Covid", "Post-SDEC", "Post-SDEC with SWSC"))]


qq_pre = qq_plot_data[(!swsc) & model_period == "Pre"] %>% 
  
  ggplot(aes(y = observed_cdf)) + 
  geom_point(aes(x = model_cdf, colour = time_window_), size = 1.5) + 
  geom_line(aes(x = observed_cdf), colour = "grey90") +
  # facet_grid(time_window ~ type, switch = "y") + 
  facet_grid(. ~ type) + 
  
  labs(
    # title = "QQ plots for distributions of number of beds needed",
    #    subtitle = "Abbreviations. NYA: not yet arrived (NYA) patients included. SWSC: Sliding Window Survival Curve",
    x = "Cdf of model distribution",
    y = "Cdf of observed distribution") +
  theme_bw()  +
  # scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
  theme(legend.position = "bottom",
        aspect.ratio = 1:1) +     # theme(axis.text.x = element_text(size = 8),
  #       axis.text.y = element_text(size = 8))  +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size=16))  +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank() )  +
  
  scale_colour_manual(values = c("#7F7F7F" , "#F4B183", "#D53E4F")) +
  theme(legend.title=element_blank())
qq_pre

plot_chart(qq_pre, "qq_pre", width__ = 300, height__ = 200, layout = NA) 
plot_chart(qq_pre, "qq_pre_wide", width__ = 400, height__ = 200, layout = NA) 

# QQ plots for supplementary materials ------------------------------------


# plot for four hours, Pre and Post Covid with six week rolling


qq_all = qq_plot_data[model_period != "Pre + Post"] %>% 
  ggplot(aes(y = observed_cdf)) + 
  geom_point(aes(x = model_cdf, colour = time_window_), size = 1.5) + 
  geom_line(aes(x = observed_cdf), colour = "grey90") +
  facet_grid(model_period_text ~ type, switch = "y") + 
  labs(
    # title = "QQ plots for distributions of number of beds needed",
    #    subtitle = "Abbreviations. NYA: not yet arrived (NYA) patients included. SWSC: Sliding Window Survival Curve",
    x = "Cdf of model distribution",
    y = "Cdf of observed distribution",
    colour = "Prediction window") +
  theme_bw()  +
  # scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
  theme(legend.position = "bottom",
        aspect.ratio = 1:1) +     # theme(axis.text.x = element_text(size = 8),
  #       axis.text.y = element_text(size = 8))  +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank() )  +
  
  scale_colour_manual(values = c("#7F7F7F" , "#F4B183", "#D53E4F")) +
  theme(legend.title=element_blank())


qq_all
plot_chart(qq_all, "qq_all", width__ = 300, height__ = 300, layout = NA) 
# plot_chart(qq_all, "qq_all_wide", width__ = 400, height__ = 200, layout = NA) 

# plot for four hours, Pre and Post Covid with six week rolling
qq_plot_data_3_models[, time_window_ := factor(case_when(grepl("Step 3", type) ~ "No prediction window applied",
                                                         TRUE ~ time_window),
                                               levels = c("No prediction window applied", "4 hour prediction window", "8 hour prediction window"))]


qq_plot_data_3_models[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Post_6wk", "Pre + Post", "Post-SDEC", "Post-SDEC_6wk"),
                                                    labels = c("Pre-Covid", "Covid", "Covid with SWSC", "Pre + Post Covid", "Post-SDEC", "Post-SDEC with SWSC"))]


# plot for 3 models only
qq_all_3_models = qq_plot_data_3_models[model_period != "Pre + Post"] %>% 
  ggplot(aes(y = observed_cdf)) + 
  geom_point(aes(x = model_cdf, colour = time_window_), size = 1.5) + 
  geom_line(aes(x = observed_cdf), colour = "grey90") +
  facet_grid(model_period_text ~ type, switch = "y") + 
  labs(
    # title = "QQ plots for distributions of number of beds needed",
    #    subtitle = "Abbreviations. NYA: not yet arrived (NYA) patients included. SWSC: Sliding Window Survival Curve",
    x = "Cdf of model distribution",
    y = "Cdf of observed distribution",
    colour = "Prediction window") +
  theme_bw()  +
  # scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
  theme(legend.position = "bottom",
        aspect.ratio = 1:1) +     # theme(axis.text.x = element_text(size = 8),
  #       axis.text.y = element_text(size = 8))  +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank() )  +
  
  scale_colour_manual(values = c("#7F7F7F" , "#F4B183", "#D53E4F")) +
  theme(legend.title=element_blank())


qq_all_3_models
plot_chart(qq_all_3_models, "qq_all_3_models", width__ = 300, height__ = 300, layout = NA) 


# comparing 3 models against 12 models
qq_plot_data$num_models = "12 models"
qq_plot_data_3_models$num_models = "3 models"

qq_all_compare_3_12 = bind_rows(qq_plot_data[grep("Step 3", type)],
                                qq_plot_data_3_models[grep("Step 3", type)]) %>% 
  filter(model_period != "Pre + Post") %>% 
  ggplot(aes(y = observed_cdf)) + 
  geom_point(aes(x = model_cdf, colour = num_models), size = 1.5) + 
  geom_line(aes(x = observed_cdf), colour = "grey90") +
  facet_grid(model_period_text ~ type, switch = "y") + 
  labs(
    # title = "QQ plots for distributions of number of beds needed",
    #    subtitle = "Abbreviations. NYA: not yet arrived (NYA) patients included. SWSC: Sliding Window Survival Curve",
    x = "Cdf of model distribution",
    y = "Cdf of observed distribution",
    colour = "Prediction window") +
  theme_bw()  +
  # scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
  theme(legend.position = "bottom",
        # aspect.ratio = 1:1
  ) +     # theme(axis.text.x = element_text(size = 8),
  #       axis.text.y = element_text(size = 8))  +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank() )  +
  
  scale_colour_manual(values = c("#7F7F7F" , "#F4B183", "#D53E4F")) +
  theme(legend.title=element_blank())
qq_all_compare_3_12
plot_chart(qq_all_compare_3_12, "qq_all_compare_3_12", width__ = 100, height__ = 300, layout = NA) 
# Prepare data for the six week rolling against the predictions --------------------------------------------------------



# merge uch metrics into pe_all
# adding unique as a fudge - not sure why I'm getting duplicate rows

## NOTE - now just doing this for 8 hour time window 


# merge the data into pe_all
pe_all[, date := date(time_of_report)]
pe_all[, report_time := substr(time_of_report, 12,16)]
pe_all[, report_time := case_when(report_time %in% c("05:00", "07:00") ~ "06:00",
                                  report_time %in% c("11:00", "13:00") ~ "12:00",
                                  report_time %in% c("15:00", "17:00") ~ "16:00",
                                  report_time %in% c("21:00", "23:00") ~ "22:00",
                                  TRUE ~ report_time)]



pe_all_8hr = unique(merge(pe_all[time_window == 8 & inc_nya & report_time == "16:00"], 
                          pe_rolling6[, .(date, six_wk_moving)], 
                          by = c("date"), all.x = TRUE, allow.cartesian=TRUE))


# get delta from actual
pe_all_8hr[, delta_exp_value := truth - expected_value  ]
pe_all_8hr[, delta_quantile10 := truth - quantile10 ]
pe_all_8hr[, delta_quantile90 := truth - quantile90]
pe_all_8hr = pe_all_8hr[,
                        mean_delta_ := round(mean(delta_exp_value), 2),
                        by = .(model_period, tsk_ids, report_time, time_window, inc_nya)]

pe_all_8hr = pe_all_8hr[,
                        mean_abs_delta := round(mean(abs(delta_exp_value)), 2), by = .(model_period, tsk_ids, report_time, time_window, inc_nya)]





# merge with UCH metric to get num admitted so far today
pe_all_8hr =  merge(pe_all_8hr,
                    unique(uch_method[, .(num_adm_so_far_today,
                                          time_of_report)]) ,
                    by = c("time_of_report"))

# pe_all[, model_period_with_mean_abs := paste(model_period, "Covid | MAE model preds = ", mean_abs_delta, "| MAE UCH metric = ", mean_abs_delta_uch)]
# pe_all[model_period == "Post-SDEC", model_period_with_mean := paste(model_period, ": mean difference = ", mean_delta_)]

pe_all_8hr[, rolling6_metric := six_wk_moving - num_adm_so_far_today]

pe_all_8hr[, delta_exp_value_6wk := truth - rolling6_metric]
pe_all_8hr = pe_all_8hr[,
                        mean_delta_6wk := round(mean(delta_exp_value_6wk), 2), by = .(model_period, tsk_ids, report_time, time_window, inc_nya)]

pe_all_8hr = pe_all_8hr[,
                        mean_abs_delta_6wk := round(mean(abs(delta_exp_value_6wk)), 2), by = .(model_period, tsk_ids, report_time, time_window, inc_nya)]

pe_all_8hr[, model_period_with_mean_abs_6wk := paste0(model_period_text, ": MAE model preds = ", mean_abs_delta, " | MAE 6 week metric = ", mean_abs_delta_6wk)]
# pe_all[model_period == "Post-SDEC", model_period_with_mean := paste(model_period, ": mean difference = ", mean_delta_)]

pe_all_8hr[, model_period_with_mean_abs_6wk_ := factor( model_period_with_mean_abs_6wk,
                                                        levels = unique(pe_all_8hr$model_period_with_mean_abs_6wk)[c(grep("Pre-Covid", unique(pe_all_8hr$model_period_with_mean_abs_6wk)),
                                                                                                                     grep("^Covid:", unique(pe_all_8hr$model_period_with_mean_abs_6wk)),
                                                                                                                     
                                                                                                                     grep("Covid with", unique(pe_all_8hr$model_period_with_mean_abs_6wk)),
                                                                                                                     grep("Pre \\+ Post Covid", unique(pe_all_8hr$model_period_with_mean_abs_6wk)),
                                                                                                                     grep("SDEC", unique(pe_all_8hr$model_period_with_mean_abs_6wk)))])] 

pe_all_8hr[,model_period_with_mean_abs_6wk__ := gsub("\\:", "\\:\n", model_period_with_mean_abs_6wk_)]
pe_all_8hr[,model_period_with_mean_abs_6wk__ := gsub(" \\| ", "\n", model_period_with_mean_abs_6wk__)]
pe_all_8hr[, model_period_with_mean_abs_6wk__ := factor( model_period_with_mean_abs_6wk__,
                                                         levels = unique(pe_all_8hr$model_period_with_mean_abs_6wk__)[c(grep("Pre-Covid", unique(pe_all_8hr$model_period_with_mean_abs_6wk__)),
                                                                                                                        grep("^Covid:", unique(pe_all_8hr$model_period_with_mean_abs_6wk__)),
                                                                                                                        
                                                                                                                        grep("^Covid with", unique(pe_all_8hr$model_period_with_mean_abs_6wk__)),
                                                                                                                        grep("Pre \\+ Post Covid", unique(pe_all_8hr$model_period_with_mean_abs_6wk__)),
                                                                                                                        grep("SDEC", unique(pe_all_8hr$model_period_with_mean_abs_6wk__)))])]

# percentage difference error
pe_all_8hr =pe_all_8hr[, perc_delta_ := delta_exp_value*100/truth]
pe_all_8hr =pe_all_8hr[, perc_delta_6_wk := delta_exp_value_6wk*100/truth]

pe_all_8hr[, mean_perc_error := round(mean(abs(perc_delta_)), 2), by = .(model_period, tsk_ids, report_time, time_window, inc_nya)]
pe_all_8hr[, mean_perc_error_6wk := round(mean(abs(perc_delta_6_wk)), 2), by = .(model_period, tsk_ids, report_time, time_window, inc_nya)]


pe_all_8hr[, model_period_with_mean_perc_error := paste0(model_period_text, ": MPE model = ", round(mean_perc_error,1), " | MPE benchmark = ", round(mean_perc_error_6wk, 1))]

pe_all_8hr[, model_period_with_mean_perc_error_ := factor( model_period_with_mean_perc_error,
                                                           levels = unique(pe_all_8hr$model_period_with_mean_perc_error)[c(grep("Pre-Covid", unique(pe_all_8hr$model_period_with_mean_perc_error)),
                                                                                                                           grep("^Covid:", unique(pe_all_8hr$model_period_with_mean_perc_error)),
                                                                                                                           
                                                                                                                           grep("Covid with", unique(pe_all_8hr$model_period_with_mean_perc_error)),
                                                                                                                           grep("Pre \\+ Post Covid", unique(pe_all_8hr$model_period_with_mean_perc_error)),
                                                                                                                           grep("SDEC", unique(pe_all_8hr$model_period_with_mean_perc_error)))])] 

pe_all_8hr[,model_period_with_mean_perc_error__ := gsub("\\:", "\\:\n", model_period_with_mean_perc_error_)]
pe_all_8hr[,model_period_with_mean_perc_error__ := gsub(" \\| ", "\n", model_period_with_mean_perc_error__)]
pe_all_8hr[, model_period_with_mean_perc_error__ := factor( model_period_with_mean_perc_error__,
                                                            levels = unique(pe_all_8hr$model_period_with_mean_perc_error__)[c(grep("Pre-Covid", unique(pe_all_8hr$model_period_with_mean_perc_error__)),
                                                                                                                              grep("^Covid:", unique(pe_all_8hr$model_period_with_mean_perc_error__)),
                                                                                                                              
                                                                                                                              grep("^Covid with", unique(pe_all_8hr$model_period_with_mean_perc_error__)),
                                                                                                                              grep("Pre \\+ Post Covid", unique(pe_all_8hr$model_period_with_mean_perc_error__)),
                                                                                                                              grep("SDEC", unique(pe_all_8hr$model_period_with_mean_perc_error__)))])]


# Benchmark plot for main paper -------------------------------------------

top_ = pe_all_8hr[
  model_period %in% c("Pre"), 
  .(time_of_report, delta_exp_value, delta_exp_value_6wk, delta_quantile10, delta_quantile90,
    model_period_with_mean_abs_6wk__ ) ] %>% 
  
  ggplot(aes(x = time_of_report)) +
  geom_point(aes(y = delta_exp_value, col = "Model predictions"), size = 2) +
  geom_point(aes(y = delta_exp_value_6wk, col = "Six week rolling average metric"), size = 2) +
  # geom_line(aes(y = delta_exp_value, col = "expected value - truth")) +
  # geom_line(aes(y = expected_value, col = "expected value")) +
  geom_ribbon(aes(ymax = delta_quantile90, ymin = delta_quantile10), fill = 'grey', alpha = 0.5) +
  scale_x_datetime(breaks = "months", date_labels = "%b %Y") +
  geom_hline(yintercept = 0) + theme_bw() + 
  # facet_wrap(time_window~dataset, nrow = 2, scales = "free") +
  # facet_wrap(model_period_with_mean_abs_6wk__~., scales = "free_x", ncol = 3) +
  labs(
    # title = paste0("Comparing with 6 week rolling average for number of admissions 8 hours after 16:00 including not yet arrived"),
    #    subtitle = "Dots show the observed minus expected value; shaded area shows range 10th - 90th around expected value; MAE = mean absolute error", 
    x = "Test set date", y = "Difference from observed value", colour = "(Observed - expected)") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size=15))  +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank() )  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_colour_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 
top_


pe_all_8hr[, delta_exp_value_abs := abs(delta_exp_value)]
pe_all_8hr[, delta_quantile10_abs := abs(delta_exp_value) - (expected_value - quantile10)]
pe_all_8hr[, delta_quantile90_abs := abs(delta_exp_value) - (expected_value - quantile90)]

pe_all_8hr[, .(expected_value, quantile10, quantile90, delta_exp_value_abs, 
               delta_quantile10_abs, delta_quantile90_abs)]


top_alt = pe_all_8hr[
  model_period %in% c("Pre")] %>% 
  
  ggplot(aes(x = time_of_report)) +
  geom_point(aes(y = abs(delta_exp_value), col = "Model predictions"), size = 2) +
  geom_point(aes(y = abs(delta_exp_value_6wk), col = "Six week rolling average metric"), size = 2) +
  # geom_line(aes(y = delta_exp_value, col = "expected value - truth")) +
  # geom_line(aes(y = expected_value, col = "expected value")) +
  geom_ribbon(aes(ymax = delta_quantile90_abs, ymin = delta_quantile10_abs), fill = 'grey', alpha = 0.5) +
  scale_x_datetime(breaks = "months", date_labels = "%b %Y") +
  geom_hline(yintercept = 0) + theme_bw() + 
  # facet_wrap(time_window~dataset, nrow = 2, scales = "free") +
  # facet_wrap(model_period_with_mean_abs_6wk__~., scales = "free_x", ncol = 3) +
  labs(
    # title = paste0("Comparing with 6 week rolling average for number of admissions 8 hours after 16:00 including not yet arrived"),
    #    subtitle = "Dots show the observed minus expected value; shaded area shows range 10th - 90th around expected value; MAE = mean absolute error", 
    x = "Test set date", y = "Absolute difference from observed value", colour = "|(Observed - expected)|") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size=15))  +
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank() )  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_colour_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 


bottom_ = pe_all_8hr[
  model_period %in% c("Pre"), 
  .(time_of_report, model_period_text, `Model predictions`  = delta_exp_value, 
    `Six week moving average metric` = round(delta_exp_value_6wk, 0))] %>% 
  pivot_longer(cols = c(`Model predictions` ,   `Six week moving average metric` )) %>% 
  ggplot(aes(x =value)) + geom_histogram(binwidth = 2.5, alpha = .5, aes(fill = name, col = name), position = "identity") + 
  # facet_wrap(model_period_text~ ., ncol = 3) + geom_vline(xintercept = 0, colour = "red") + 
  theme_bw() +
  labs(
    # subtitle = "Histogram showing distribution of error", 
    y = "Count of predictions",
    x = "(Observed - expected)", fill = "Distribution of error") +
  theme(legend.position = "bottom") +   guides(colour=FALSE) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size=15))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 

plot_benchmark = plot_grid(top_, bottom_, nrow = 2, rel_heights = c(0.7, 0.3), labels = c("a", "b"))
plot_benchmark_alt = plot_grid(top_alt, bottom_, nrow = 2, rel_heights = c(0.7, 0.3), labels = c("a", "b"))


plot_chart(plot_benchmark, "plot_benchmark", width__ = 300, height__ = 250, layout = NA) 
plot_chart(plot_benchmark, "plot_benchmark_wide", width__ = 400, height__ = 200, layout = NA) 

plot_chart(plot_benchmark_alt, "plot_benchmark_alt", width__ = 300, height__ = 250, layout = NA) 
plot_chart(plot_benchmark_alt, "plot_benchmark_alt_wide", width__ = 400, height__ = 200, layout = NA) 


# Benchmark plot for the supp materials -----------------------------------


# plot the six week rolling against predictions
top_all = pe_all_8hr[
  model_period_text %in% c("Pre-Covid", "Covid", "Covid with SWSC", "Post-SDEC", "Post-SDEC with SWSC"), 
  .(time_of_report, delta_exp_value, delta_exp_value_6wk, delta_quantile10, delta_quantile90,
    model_period_with_mean_abs_6wk__ ) ] %>% 
  
  ggplot(aes(x = time_of_report)) +
  geom_point(aes(y = delta_exp_value, col = "Model predictions"), size = 2) +
  geom_point(aes(y = delta_exp_value_6wk, col = "Six week rolling average metric"), size = 2) +
  # geom_line(aes(y = delta_exp_value, col = "expected value - truth")) +
  # geom_line(aes(y = expected_value, col = "expected value")) +
  geom_ribbon(aes(ymax = delta_quantile90, ymin = delta_quantile10), fill = 'grey', alpha = 0.5) +
  scale_x_datetime(breaks = "months", date_labels = "%b %Y") +
  geom_hline(yintercept = 0) + theme_bw() + 
  # facet_wrap(time_window~dataset, nrow = 2, scales = "free") +
  facet_wrap(model_period_with_mean_abs_6wk__~., scales = "free_x", ncol = 5) +
  labs(
    # title = paste0("Comparing with 6 week rolling average for number of admissions 8 hours after 16:00 including not yet arrived"),
    #    subtitle = "Dots show the observed minus expected value; shaded area shows range 10th - 90th around expected value; MAE = mean absolute error", 
    x = "Test set date", y = "Difference from observed value", colour = "(Observed - expected)") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_colour_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 

top_all_alt = pe_all_8hr[
  model_period_text %in% c("Pre-Covid", "Covid", "Covid with SWSC", "Post-SDEC", "Post-SDEC with SWSC")] %>% 
  
  ggplot(aes(x = time_of_report)) +
  geom_point(aes(y = abs(delta_exp_value), col = "Model predictions"), size = 2) +
  geom_point(aes(y = abs(delta_exp_value_6wk), col = "Six week rolling average metric"), size = 2) +
  # geom_line(aes(y = delta_exp_value, col = "expected value - truth")) +
  # geom_line(aes(y = expected_value, col = "expected value")) +
  geom_ribbon(aes(ymax = delta_quantile90_abs, ymin = delta_quantile10_abs), fill = 'grey', alpha = 0.5) +
  scale_x_datetime(breaks = "months", date_labels = "%b %Y") +
  geom_hline(yintercept = 0) + theme_bw() + 
  # facet_wrap(time_window~dataset, nrow = 2, scales = "free") +
  facet_wrap(model_period_with_mean_abs_6wk__~., scales = "free_x", ncol = 5) +
  labs(
    # title = paste0("Comparing with 6 week rolling average for number of admissions 8 hours after 16:00 including not yet arrived"),
    #    subtitle = "Dots show the observed minus expected value; shaded area shows range 10th - 90th around expected value; MAE = mean absolute error", 
    x = "Test set date", y = "Absolute difference from observed value", colour = "|(Observed - expected)|") +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12)) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_colour_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 


bottom_all = pe_all_8hr[
  model_period_text %in% c("Pre-Covid", "Covid", "Covid with SWSC", "Post-SDEC", "Post-SDEC with SWSC"), 
  .(time_of_report, model_period_text, `Model predictions`  = delta_exp_value, 
    `Six week moving average metric` = round(delta_exp_value_6wk, 0))] %>% 
  pivot_longer(cols = c(`Model predictions` ,   `Six week moving average metric` )) %>% 
  ggplot(aes(x =value)) + geom_histogram(binwidth = 2.5, alpha = .5, aes(fill = name, col = name), position = "identity") + 
  facet_wrap(model_period_text~ ., ncol = 5) + geom_vline(xintercept = 0, colour = "red") + theme_bw() +
  labs(
    # subtitle = "Histogram showing distribution of error", 
    y = "Count of predictions",
    x = "(Observed - expected)", fill = "Distribution of error") +
  theme(legend.position = "bottom") +   guides(colour=FALSE) + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 


# title <- ggdraw() + draw_label(paste("Predictions of number of beds needed within"), fontface='bold')

plot_benchmark_all = plot_grid(top_all, bottom_all, nrow = 2, rel_heights = c(0.7, 0.3), labels = c("a", "b"))
plot_benchmark_all_alt = plot_grid(top_all_alt, bottom_all, nrow = 2, rel_heights = c(0.7, 0.3), labels = c("a", "b"))


plot_chart(plot_benchmark_all, "plot_benchmark_all", width__ = 300, height__ = 200, layout = NA) 
plot_chart(plot_benchmark_all, "plot_benchmark_all_wide", width__ = 350, height__ = 200, layout = NA) 

plot_chart(plot_benchmark_all_alt, "plot_benchmark_all_alt", width__ = 300, height__ = 200, layout = NA) 
plot_chart(plot_benchmark_all_alt, "plot_benchmark_all_alt_wide", width__ = 350, height__ = 200, layout = NA) 


# Training, val, test split -----------------------------------------------

summ[, adm := if_else(adm %in% c("direct_dis", "indirect_dis"), "Discharged", "Admitted")]

# Plot admissions and discharges by day -----------------------


# number admitted and discharged
adm_disch = summ[, .N, by = .(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = fct_rev(adm))) + 
  geom_bar(stat = "identity") +
  geom_vline(xintercept = c(date(start_study),
                            # date(start_val_pre), date(start_test_pre),
                            date(covid_start), 
                            # date(start_val_post),
                            # date(start_val_post_SDEC), date(SDEC_start),
                            # date(start_test_post),
                            date(end_SDEC_study))
  ) +
  scale_x_date(breaks = c(date(start_study), 
                          # date(start_val_pre), date(start_test_pre),
                          date(covid_start), 
                          # date(start_val_post),
                          # date(start_val_post_SDEC), date(SDEC_start),
                          # date(start_test_post),
                          date(end_SDEC_study))
  )+ theme_bw() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=14))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(legend.position = "bottom") +
  
  labs(
    # title = "Number of admissions and discharges by day",
    x = "Date", 
    y = "Number of visits",
    fill = "Disposition") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = c( "#4891B9",  "#D53E4F",  guide = NULL, name = NULL)) 



# Plot train val test chronologically -------------------------------------




plot_data2 = data.table()

for (model_period in c("Pre", "Post" ,
                       "Pre + Post",
                       "Post-SDEC"
)) {
  
  dataset = set_train_val_test_dates(summ, NA, model_period)
  period_summ = dataset[, .N, by = .(date(presentation_time), in_set)] 
  period_summ$model_period = model_period
  
  plot_data2 = bind_rows(plot_data2, period_summ)
  
}

plot_data2[, model_period_ := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                     labels = c("Pre-Covid", "Covid", "Pre + Post", "Post-SDEC"))]
plot_data2[, in_set_ := factor(in_set, levels = c("Before", "Train", "Val", "Test", "After"),
                               labels = c("Before", "Training", "Validation", "Test", "After"))]

# for jsut Pre and Post
train_val_test = plot_data2[model_period %in% c("Pre", "Post", "Post-SDEC")]  %>% ggplot(aes(x = date, y = N, fill = in_set_)) +
  
  geom_bar(stat = "identity") +
  labs(
    # title = "Number of visits by day, with colour showing train-val-test split",
    x = "Date", 
    y = "Number of visits",
    fill = "Set") +
  geom_vline(xintercept = c(date(start_val_pre), date(start_test_pre), date(covid_start), date(start_val_post), 
                            date(start_val_post_SDEC), date(SDEC_start),
                            date(start_test_post) 
                            # , date(end_SDEC_study)
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=14))  +
  facet_grid(model_period_ ~ .) + 
  theme(legend.position = "bottom") +
  scale_x_date(breaks = c(date(start_study), date(start_val_pre), date(start_test_pre), 
                          date(start_val_post_SDEC), date(SDEC_start),
                          date(covid_start), date(start_val_post), date(start_test_post), date(end_study)
                          # , date(end_SDEC_study)
  ))+
  scale_fill_manual(values = c("#7F7F7F" ,  "#4891B9", "#F4B183", "#D53E4F", "#7F7F7F", guide = NULL, name = NULL)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot_train_val_test = plot_grid(adm_disch + theme(plot.margin = unit(c(1,1,4,1), "lines")), train_val_test, nrow = 2, rel_heights = c(0.45, 0.55), labels = c("a", "b"))

plot_chart(plot_train_val_test, "plot_train_val_test", width__ = 300, height__ = 300, layout = NA) 

plot_chart(plot_train_val_test, "plot_train_val_test_wide", width__ = 400, height__ = 200, layout = NA)



# Report on class balance -------------------------------------------------

load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_",summ_file_date,".rda"))
load(paste0("~/EDcrowding/predict-admission/data-raw/dm_",dm_file_date,".rda"))
summ[, model_period := if_else(first_ED_admission < covid_start, "Pre-Covid", "Post-Covid")]
summ[, adm := if_else(adm %in% c("direct_dis", "indirect_dis"), "Discharged", "Admitted")]


dm[, adm := NULL]
dm = merge(dm, summ[, .(csn, model_period, adm)], by = "csn")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")

class_balance = data.table()


for (model_period_ in c("Pre", "Post", "Post-SDEC")) {
  
  dataset = set_train_val_test_dates(summ, NA, model_period_)
  dataset = dataset[in_set %in% c("Train", "Val", "Test") ]
  dm_temp = dm[csn %in% dataset$csn]
  
  for (ts_ in timeslices) {
    
    ts_bal = dm_temp[duration > as.numeric(ts_), .N, by = .(adm)]
    ts_bal[, timeslice := ts_]
    ts_bal$model_period = model_period_
    class_balance = bind_rows(class_balance, ts_bal)
    
  }
}


class_balance[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                            labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]


# class_balance[, sum(N), by = .(model_period, timeslice)] 
class_balance[, timeslice_ := paste0("T", as.numeric(gsub("task","", timeslice)))]
class_balance$timeslice_ = factor(class_balance$timeslice_, levels = unique(class_balance$timeslice_[order(as.numeric(gsub("task","", class_balance$timeslice)))]))

class_balance[, N_tot := sum(N), by = .(timeslice, timeslice_, model_period, model_period_text)]
class_balance[, percentage := N/ N_tot]

# 
# odds = class_balance %>% pivot_wider(names_from = adm, values_from = N) %>% mutate(odds = Admitted/Discharged) 
# 
# class_balance = merge(class_balance, odds, by = c("timeslice", "timeslice_", "model_period", "model_period_text"))

# for supplementary matierals

for (model_period_text_ in c("Pre-Covid", "Covid", "Post-SDEC")) {
  
  plot_class_balance = class_balance %>% filter(model_period_text == model_period_text_) %>% 
    ggplot(aes(x = timeslice_, y = N, fill = as.factor(adm))) + geom_bar(stat= "identity", position = "stack") + theme_bw() + 
    theme_bw() +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          # plot.subtitle = element_text(size = 8),
          # plot.title = element_text(size = 10),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          strip.text = element_text(size=14))  +
    labs(
      # title = "Class balance for timeslices",
      # subtitle = "Pre-Covid",
      x = "Model",
      y = "Number of visits",
      fill = "Disposition") +
    theme(legend.position = "bottom") +
    
    scale_y_continuous(breaks = seq(0, 120000, 20000), label = comma) +
    scale_fill_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) +
    facet_grid(.~model_period_text, scales = "free", space = "free", switch = "y") +
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) #+
  
  # geom_text(aes(label = round(percentage, 2)), size = 4, vjust = -1) 
  
  for (ts_ in unique(class_balance$timeslice_)) {
    
    num_neg = class_balance[model_period_text == model_period_text_ & timeslice_ == ts_ & adm == "Discharged", N]
    if (num_neg > 5000) {
      y = num_neg + class_balance[model_period_text == model_period_text_ & timeslice_ == ts_ & adm == "Admitted", N]/2
    } else {
      y = 5000
    }
    plot_class_balance = plot_class_balance + annotate("text", x = ts_, y = y, 
                                                       label = class_balance[model_period_text == model_period_text_ & timeslice_ == ts_ & adm == "Admitted", round(percentage,2)])
  }
  
  name_ = paste0("plot_class_balance", model_period_text_)
  assign(name_, plot_class_balance)
  
}


plot_class_balance_all = plot_grid(`plot_class_balancePre-Covid`, plot_class_balanceCovid, `plot_class_balancePost-SDEC`, ncol = 1)
plot_performance_supp


plot_chart(plot_class_balance_all, "class_balance", width__ = 300, height__ = 300, layout = NA) 

plot_chart(plot_class_balance_all, "class_balance_wide", width__ = 400, height__ = 200, layout = NA)




# Plot distribution of outcome variable -----------------------------------


plot_data3 = data.table()

for (model_period in c("Pre", "Post" 
                       # ,"Pre + Post"
                       , "Post-SDEC"
)) {
  
  dataset = set_train_val_test_dates(summ, NA, model_period, return_dates_only = TRUE)
  adm_report_ = adm_report_time[date >= dataset[1] & date < dataset[4]]
  adm_report_[, in_set := case_when(date < dataset[2] ~ "Train", 
                                    date < dataset[3] ~ "Val",
                                    TRUE ~ "Test")]
  adm_report_$model_period = model_period
  
  plot_data3 = bind_rows(plot_data3, adm_report_)
  
}

plot_data3[, arrival_window := factor(case_when(report_time == "06:00" ~ "2200-0600",
                                                report_time == "12:00"~ "0600-1200",
                                                report_time == "16:00" ~ "1200-1600",
                                                report_time == "22:00" ~ "1600-2200"))]



plot_data3[, model_period_ := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                     labels = c("Pre-Covid", "Covid", "Pre + Post", "Post-SDEC"))]



plot_data3[, x := as.character(time_window)]

plot_hist_adm = plot_data3 %>% 
  ggplot(aes(x =num_adm)) + geom_histogram(binwidth = 2.5, alpha = .5, 
                                           aes(fill = x), position = "identity") + 
  facet_grid(report_time ~ model_period_, switch = "y") + 
  labs(
    # title = "Histogram of admissions from ED", 
    #    subtitle = "Subplots show four report times, and each model period separately",
    x = "Number of admissions within prediction window", fill = "Prediction window") + theme_bw() +
  theme(legend.position = "bottom") +   guides(colour=FALSE) + 
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=14))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 


plot_chart(plot_hist_adm, "adm_histogram_all_models", width__ = 300, height__ = 300, layout = NA) 


# Plot Covid surge --------------------------------------------------------


library(readr)
covid_cases <- data.table(read_csv("~/EDcrowding/real-time/model-input/covid deaths.csv"))
setorder(covid_cases, date)
covid_cases[, rolling7 := frollmean(newDeaths28DaysByDeathDate, 7)]
surge_level = 75
covid_cases[, surge := if_else(rolling7 > surge_level, TRUE, FALSE)]

plot_covid_surge = covid_cases %>% ggplot(aes(x = date, y = rolling7, fill = surge)) + geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y %m") + 
  geom_hline(yintercept = surge_level) +
  labs(
    # title = "7 day rolling average death rates, with suggested cutoff for surge periods",
    #    subtitle = "Source: https://coronavirus.data.gov.uk/details/deaths",
    y = "7 day rolling average",
    x = "Year and month", 
    fill = "Surge period") + theme_bw() +
  theme(legend.position = "bottom") +   guides(colour=FALSE) + 
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=14))  +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c( "#D53E4F", "#4891B9",   guide = NULL, name = NULL)) 

plot_chart(plot_covid_surge, "covid_surge", width__ = 300, height__ = 150, layout = NA) 






# Plot calibration plots --------------------------------------------------


library(caret)
library(formattable)
# Losd data
for (model_period_ in c("Pre"
                        #  , "Post" 
                        # # ,"Pre + Post"
                        # , "Post-SDEC"
)) {
  
  preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features,"_", model_period_,  "_preds_",preds_file_date,".rda")
  
  load(preds_file)
  
  
  for (tsk_ids_ in c("Test")) {
    # for (tsk_ids_ in c("Train", "Val", "Test")) {
    
    plot_data_cal = data.table()
    
    for (ts_ in timeslices) {
      
      name_tsk = paste0("task", ts_)
      x = preds_all_ts[timeslice == name_tsk & in_set == tsk_ids_]
      cal_ = data.frame(obs = factor(x$truth),
                        preds = x$prob.1)
      cal_ts = calibration(obs ~ preds, data = cal_, cuts = 20, class = "1")
      cal_ts_plot = data.table(cal_ts$data)
      cal_ts_plot$timeslice = paste0("T", ts_)
      cal_ts_plot$in_set = tsk_ids_
      cal_ts_plot$timeslice_N = paste0("T", as.numeric(ts_), " (Num visits = ",comma(nrow(x), digits = 0), ")")
      plot_data_cal = bind_rows(plot_data_cal, cal_ts_plot)
      
      
    }
    
    
    
    
    
    p = plot_data_cal  %>% ggplot(aes(x = midpoint/100)) +
      geom_line(aes(y = Percent/100), col = "blue") + geom_point(aes(y = Percent/100), col = "blue") +
      geom_line(aes(y = midpoint/100), colour = "#7F7F7F" , linetype = "dashed") +
      labs(
        # title = paste("Calibration plot by timeslice - ", model_period_, "Covid", tsk_ids_, "set"),
        x = "Midpoint of probability bin (where probability divided into 20 equal bins)",
        y = "Proportion of visits in this bin who were actually admitted") +
      theme_bw() + scale_x_continuous(breaks = seq(0, 1, .2)) +
      scale_y_continuous(breaks = seq(0, 1, .2)) +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            # plot.subtitle = element_text(size = 8),
            # plot.title = element_text(size = 10),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size=14))  +
      facet_wrap(fct_relevel(plot_data_cal$timeslice_N, 
                             levels = levels(factor(plot_data_cal$timeslice_N))[order(as.numeric(gsub("T", "", unlist(strsplit(levels(factor(plot_data_cal$timeslice_N)), " "))[seq(1,60,5)])))])~.) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    
    
    # if (model_period_ == "Pre on Post") {
    #   p = p + labs(subtitle = "Model trained on pre Covid data, applied to post Covid test set")
    # }
    # 
    # if (model_period_ == "Pre on Post 1st 3 months") {
    #   p = p + labs(subtitle = "Model trained on pre Covid data, applied to first three months of Covid")
    # }
    
    name_ = paste0("calib_", model_period_, "_", tsk_ids_)
    assign(name_, p)
    
    
  }
  
}

plot_chart(calib_Pre_Test +  theme(axis.text.x = element_text(size = 10)), "calib_pre", width__ = 325, height__ = 275, layout = NA) 
plot_chart(calib_Post_Test +  theme(axis.text.x = element_text(size = 10)), "calib_post", width__ = 325, height__ = 275, layout = NA) 
plot_chart(`calib_Post-SDEC_Test` +  theme(axis.text.x = element_text(size = 10)), "calib_SDEC", width__ = 325, height__ = 275, layout = NA) 


# Calibration plot 3 models  ----------------------------------------------

plot_data_cal_12 = plot_data_cal

for (model_period_ in c("Pre"
)) {
  
  preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_3_models_",model_features,"_", model_period_,  "_preds_",presds_file_date_3_models,".rda")
  
  load(preds_file)
  for (tsk_ids_ in c("Test")) {
    # for (tsk_ids_ in c("Train", "Val", "Test")) {
    
    plot_data_cal_3 = data.table()
    
    for (ts_ in timeslices) {
      
      name_tsk = paste0("task", ts_)
      x = preds_all_ts[timeslice == name_tsk & in_set == tsk_ids_]
      cal_ = data.frame(obs = factor(x$truth),
                        preds = x$prob.1)
      cal_ts = calibration(obs ~ preds, data = cal_, cuts = 20, class = "1")
      cal_ts_plot = data.table(cal_ts$data)
      cal_ts_plot$timeslice = paste0("T", ts_)
      cal_ts_plot$in_set = tsk_ids_
      cal_ts_plot$timeslice_N = paste0("T", as.numeric(ts_), " (Num visits = ",comma(nrow(x), digits = 0), ")")
      plot_data_cal_3 = bind_rows(plot_data_cal_3, cal_ts_plot)
      
      
    }
  }
  
  preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_12_models_",model_features,"_", model_period_,  "_preds_",presds_file_date_3_models,".rda")
  
  load(preds_file)
  for (tsk_ids_ in c("Test")) {
    # for (tsk_ids_ in c("Train", "Val", "Test")) {
    
    plot_data_cal_12 = data.table()
    
    for (ts_ in timeslices) {
      
      name_tsk = paste0("task", ts_)
      x = preds_all_ts[timeslice == name_tsk & in_set == tsk_ids_]
      cal_ = data.frame(obs = factor(x$truth),
                        preds = x$prob.1)
      cal_ts = calibration(obs ~ preds, data = cal_, cuts = 20, class = "1")
      cal_ts_plot = data.table(cal_ts$data)
      cal_ts_plot$timeslice = paste0("T", ts_)
      cal_ts_plot$in_set = tsk_ids_
      cal_ts_plot$timeslice_N = paste0("T", as.numeric(ts_), " (Num visits = ",comma(nrow(x), digits = 0), ")")
      plot_data_cal_12 = bind_rows(plot_data_cal_12, cal_ts_plot)
      
      
    }
  }
}

scale_colour_manual(values = c("#7F7F7F" , "#F4B183", "#D53E4F")) 

calib_Pre_Test_12 = plot_data_cal_12  %>% ggplot(aes(x = midpoint/100)) +
  geom_line(aes(y = Percent/100), col = "#D53E4F") + geom_point(aes(y = Percent/100), col = "#D53E4F") +
  geom_line(aes(y = midpoint/100), colour = "#7F7F7F" , linetype = "dashed") +
  labs(
    # title = paste("Calibration plot by timeslice - ", model_period_, "Covid", tsk_ids_, "set"),
    x = "Midpoint of probability bin (where probability divided into 20 equal bins)",
    y = "Proportion of visits in this bin who were actually admitted") +
  theme_bw() + scale_x_continuous(breaks = seq(0, 1, .2)) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=14))  +
  facet_wrap(fct_relevel(plot_data_cal$timeslice_N, 
                         levels = levels(factor(plot_data_cal$timeslice_N))[order(as.numeric(gsub("T", "", unlist(strsplit(levels(factor(plot_data_cal$timeslice_N)), " "))[seq(1,60,5)])))])~.) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


calib_3_modelsPre_Test = calib_Pre_Test_12  + geom_line(aes(y = plot_data_cal_3$Percent/100), col = "#F4B183") + geom_point(aes(y = plot_data_cal_3$Percent/100), col = "#F4B183")





plot_chart(calib_3_modelsPre_Test +  theme(axis.text.x = element_text(size = 10)), "calib_3_models_pre_compare_12", width__ = 325, height__ = 275, layout = NA) 
