
# About this script -------------------------------------------------------


# For a given sequence of time points of interest, retrieves all 
# patients in ED at that time point, loads models to predict their
# probability of admission, and then aggregates them into 
# a probability distribution for total number admitted

# Various different distributions are generated, including
# with and without patients who have not yet arrived. 

# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(polynom)
library(readr)

# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)
library(mlr3fselect)
library(mlr3misc)




# Load configuration and other functions  ---------------------------------------------------------------



# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")

# add real-time app utils to get one hot encoding 
source("EDcrowding/real-time/app/utils.R")


# giving the option to use the test set for post-SDEC Cox regression
use_test_set_for_Cox_SDEC = FALSE


# Functions specific to this programme  --------------------------------------------------------



# Get probability distribution from predictions 



get_prob_dist = function(time_pts, in_ED_all, preds_all_ts, base_prob, coefs, poisson_not_yet_arrived, time_window_array_ = c(4, 8)
) {
  
  distr_coll = data.table()
  pt_estimates_coll = data.table()
  
  
  
  for (i in (1:length(time_pts))) {
    
    print(i)
    
    # get patients in ED at this time and set characteristics of time point
    
    in_ED = in_ED_all[time_pt == time_pts[i]]
    num_adm_ = seq(0,nrow(in_ED), 1)
    
    get_report = case_when(as.numeric(substr(time_pts[i], 12,13)) < 7 ~ "6:00",
                           as.numeric(substr(time_pts[i], 12,13)) < 13 ~ "12:00",
                           as.numeric(substr(time_pts[i], 12,13)) < 17 ~ "16:00",
                           TRUE ~ "22:00")
    
    is_weekend = ifelse(weekdays(time_pts[i], abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)
    
    if (nrow(in_ED) == 0) {
      
      # if there is noone in ED
      # only need to do time varying poission for patients not yet arrived
      
      for (time_window_ in time_window_array_) {
        
        # if at end of time points, skip time window calcs
        
        if (!(i == length(time_pts) |
              i == length(time_pts)-1 & time_window_ == 8)) { 
          
          # using empirically derived poisson mean, 
          # generate probs of each number of not-yet-arrived admissions
          # assuming that a maximum of 20 people can arrive after the time point of interest
          # and be admitted before the end of the time window after that
          probs_not_yet_arrived = 
            dpois(seq(0, 20 ,1),
                  lambda = poisson_not_yet_arrived[time_window == time_window_ &
                                                     time_pt == time_pts[i], 
                                                   poisson_mean])
          # save this distribution
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = seq(0, 20 ,1),
                                       probs = probs_not_yet_arrived, cdf = cumsum(probs_not_yet_arrived),
                                       time_window = time_window_,
                                       inc_nya = TRUE,
                                       dist = paste("Empirical poisson")))
          # derive point estimates from this distribution
          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = 0, 
                                 # retrieve true number of admissions within time window of people
                                 # who arrived after the time point of interest
                                 truth = nrow(summ[first_ED_admission > time_pts[i] &
                                                     adm %in% c("direct_adm", "indirect_adm") &
                                                     left_ED <= time_pts[i] + hours(time_window_)]),
                                 # use the distribution to get an expected value and 10th and 90th quantile on cdf
                                 expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile10 = ifelse(nrow(distr[cdf<0.1]) == 0, min(distr$num_adm_pred), distr[cdf<0.1, max(num_adm_pred)]),
                                 quantile90 = ifelse(nrow(distr[cdf<0.9]) == 0, min(distr$num_adm_pred), distr[cdf<0.9, max(num_adm_pred)]),
                                 time_window = time_window_,
                                 inc_nya = TRUE,
                                 dist = paste("Empirical poisson")))
          
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          
        }
        
      }
      
      
      
    } else {
      
      # since there are patients in ED
      # we first derive a probability distribution for each possible number of admission
      
      # get the probability of admission for each individual
      df = merge(in_ED[,.(csn, adm, timeslice, time_so_far, left_ED)], preds_all_ts[,.(csn, prob.1, timeslice)], 
                 by = c("csn", "timeslice"), all.x = TRUE)
      
      # use the generating function to create a distribution from these probs
      probs_in_ED = poly_prod(df) 
      
      # # adding a workaround here because coef is not returning the right number of probabilities in some cases
      # if (length(probs_in_ED) != nrow(df) + 1) {
      #   print(paste0("Fixing length of probs_in_ED for time_pt", i, " with number in ED of ", nrow(df)))
      #   probs_in_ED = c(probs_in_ED, rep(0, nrow(df) + 1 - length(probs_in_ED)))
      # }
      # 
      # save the true number of admissions of these patients
      df[, truth := case_when(adm %in% c("direct_adm", "indirect_adm") ~ 1,
                              TRUE ~ 0)]
      
      # save the distribution
      distr = data.table(bind_cols(time_of_report = time_pts[i],
                                   num_adm_pred = num_adm_,
                                   probs = probs_in_ED, cdf = cumsum(probs_in_ED),
                                   time_window = NA,
                                   inc_nya = FALSE,
                                   dist = paste("Only patients in ED")))
      
      
      # derive point estimates from this distribution
      pt_estimates_coll_ = 
        data.table(bind_cols(time_of_report = time_pts[i],
                             num_in_ED = nrow(df), 
                             truth = sum(df$truth),
                             # use the distribution to get an expected value and 5th and 9th quantile on cdf
                             expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                             quantile10 = ifelse(nrow(distr[cdf<0.1]) == 0, min(distr$num_adm_pred), distr[cdf<0.1, max(num_adm_pred)]),
                             quantile90 = ifelse(nrow(distr[cdf<0.9]) == 0, min(distr$num_adm_pred), distr[cdf<0.9, max(num_adm_pred)]),
                             time_window = NA,
                             inc_nya = FALSE,
                             dist = paste("Only patients in ED")))
      
      distr_coll = bind_rows(distr_coll, distr)
      pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
      
      
      for (time_window_ in time_window_array_) {
        
        # if at end of time points, skip time window calcs
        
        if (!(i == length(time_pts) |
              i == length(time_pts)-1 & time_window_ == 8)) {
          
          tta_prob_ = data.table()
          
          # for each csn, find their probability of still being in ED at this long after arrival
          for (h in 1 : nrow(df)) {
            
            base_prob_ = base_prob[first_ED_admission_date == in_ED_all[csn == df$csn[h]  & time_pt == time_pts[i],
                                                                        first_ED_admission_date] ]
            
            base_prob_$surv_ = base_prob_$surv ^ in_ED_all[csn == df$csn[h] & time_pt == time_pts[i], epow]
            
            col_num_for_still_in_now = sum(base_prob_$time<= df$time_so_far[h])
            
            # it is possible for the time so far to be shorter than any of the survival times save in the cox curve
            if (col_num_for_still_in_now == 0) {
              col_num_for_still_in_now = 1
            }
            
            prob_still_in_now = base_prob_$surv_[col_num_for_still_in_now]
            
            # for each csn, find their probability of still being in ED at this long after arrival plus the time window
            col_num_for_still_in_at_time_window = sum(base_prob_$time<= df$time_so_far[h] + time_window_)
            
            if (col_num_for_still_in_at_time_window == col_num_for_still_in_now) {
              
              prob_still_in_at_time_window_alt = prob_still_in_now
              
            } else {
              
              # prob_still_in_at_time_window = base_prob_$surv_[col_num_for_still_in_at_time_window]
              base_prob_$rebased_prob = base_prob_$surv_ / prob_still_in_now
              prob_still_in_at_time_window_alt = base_prob_$rebased_prob[col_num_for_still_in_at_time_window]
            }
            
            
            
            # save the probability 
            tta_prob_ = bind_rows(tta_prob_, data.table(csn = df$csn[h], 
                                                        # # for debugging:
                                                        # first_ED_admission = base_prob_$surv_$first_ED_admission[j],
                                                        # timeslice = base_prob_$surv_$timeslice[j],
                                                        # prob_still_in_now,
                                                        # prob_still_in_at_time_window,
                                                        # prob_still_in_at_time_window_alt,
                                                        prob_adm_in_time_window = 1 - prob_still_in_at_time_window_alt))
          }
          
          df_ = merge(df, tta_prob_, by = "csn")
          # df_ = merge(df, tta_prob_, by = c("csn", "timeslice", "first_ED_admission"))
          
          # then get distribution using generating function
          # but this time using joint probability of admission and admission within time window
          df_[, prob.1 := prob.1 * prob_adm_in_time_window]
          probs_in_ED_adj_for_time_window = poly_prod(df_) 
          
          # # adding a workaround here because coef is not returning the right number of probabilities in some cases
          # if (length(probs_in_ED) != nrow(df) + 1) {
          #   print(paste0("Fixing length of probs_in_ED for time_pt", i, " with number in ED of ", nrow(df)))
          #   probs_in_ED = c(probs_in_ED, rep(0, nrow(df) + 1 - length(probs_in_ED)))
          # }
          # 
          
          # save the true number of admissions of these patients within the time window
          df_[, truth := case_when(adm %in% c("direct_adm", "indirect_adm") & 
                                     difftime(left_ED, time_pts[i], units = "hours") <= time_window_ ~ 1,
                                   TRUE ~ 0)]
          
          # save the distribution 
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = num_adm_,
                                       probs = probs_in_ED_adj_for_time_window, cdf = cumsum(probs_in_ED_adj_for_time_window),
                                       time_window = time_window_,
                                       inc_nya = FALSE,
                                       dist = paste("Only patients in ED")))
          
          # derive point estimates from this distribution
          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = nrow(df_), 
                                 truth = sum(df_$truth),
                                 expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile10 = ifelse(nrow(distr[cdf<0.1]) == 0, min(distr$num_adm_pred), distr[cdf<0.1, max(num_adm_pred)]),
                                 quantile90 = ifelse(nrow(distr[cdf<0.9]) == 0, min(distr$num_adm_pred), distr[cdf<0.9, max(num_adm_pred)]),
                                 time_window = time_window_,
                                 inc_nya = FALSE,
                                 dist = paste("Only patients in ED")))
          
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          
          # 
          # -----------
          # now create distributions including patients not yet arrived
          
          # generate probs of each number of not-yet-arrived admissions

          probs_not_yet_arrived = dpois(c(num_adm_ , seq(max(num_adm_) + 1, max(num_adm_+20),1)),
                                        lambda = poisson_not_yet_arrived[time_window == time_window_ &
                                                                           time_pt == time_pts[i], 
                                                                         poisson_mean])
          
          # the random variable (number of admissions) is a combination of 
          # admissions from those in ED (indexed by k), and those not yet arrived (indexed by j)
          # assuming that a maximum of 20 people can arrive after the time point of interest
          # and be admitted before the end of the time window after that
          # then for each pair of values of each of these two random variables 
          # calculate the probability of the combination, and sum the random variables value
          # note that the distributions starts at zero so subject 1 from each index
          dist_nya = data.table()
          
          for (k in 1:length(probs_in_ED_adj_for_time_window)) {
            for (j in 1:length(probs_not_yet_arrived)) {
              
              tot_adm_ = k-1 + j-1
              prob_tot_ = probs_in_ED_adj_for_time_window[k] * probs_not_yet_arrived [j]
              row = data.table(num_adm_pred = tot_adm_, prob_tot = prob_tot_)
              
              dist_nya = bind_rows(dist_nya, row)
            }
          }
          
          # then sum the probabilities wherever the sum of the pairs is the same
          dist_nya = dist_nya[, .(probs = sum(prob_tot)), by = num_adm_pred]
          
          # then save the distribution as before
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = seq(0, nrow(dist_nya)-1, 1),
                                       probs = dist_nya$probs, cdf = cumsum(dist_nya$probs),
                                       time_window = time_window_,
                                       inc_nya = TRUE,
                                       dist = paste("Empirical poisson")))
          
          # and the point estimates
          pt_estimates_coll_ =
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = nrow(df_), 
                                 # truth is sum of those in ED and those not yet arrived 
                                 # who were admitted within time window
                                 truth = sum(df_$truth) + 
                                   nrow(summ[first_ED_admission > time_pts[i] &
                                               adm %in% c("direct_adm", "indirect_adm") &
                                               left_ED <= time_pts[i] + hours(time_window_)]),
                                 expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile10 = ifelse(nrow(distr[cdf<0.1]) == 0, min(distr$num_adm_pred), distr[cdf<0.1, max(num_adm_pred)]),
                                 quantile90 = ifelse(nrow(distr[cdf<0.9]) == 0, min(distr$num_adm_pred), distr[cdf<0.9, max(num_adm_pred)]),
                                 time_window = time_window_,
                                 inc_nya = TRUE,
                                 dist = paste("Empirical poisson")))
          
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          
          
          
          
          
        }
      }
    }
    
    
  }
  
  
  return(list(distr_coll, pt_estimates_coll))
  
}





# Load data ---------------------------------------------------------------



# running this in November 2021 I found that summ from 2021-07-20 has a patient with csn  1027262100 )
# who is not in summ from 2021-09-09
# because the design matrices were created in September, they may not be the same as those used in the paper
# therefore overwrite some parameters based on this github commit 
# https://github.com/zmek/EDcrowding/commit/5536dd2755232ea2940ec9c3b9096085ff774006


num_in_ED_at_arrival_time_file_date = "2021-07-23"

# obs_file_date =  "2021-07-20"
# labs_file_date =  "2021-07-20"
# 
# dm_file_date =  "2021-07-21"


# but this will need to be changed for later models
load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))

# load file with each location
load(paste0("EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",num_in_ED_at_arrival_time_file_date,".rda"))

# load rolling average survival curves
load(paste0("EDcrowding/real-time/model-input/cox_results_six_week_rolling_",cox_surv_date_six_week_rolling,".rda"))
load(paste0("EDcrowding/real-time/model-input/scale_params_six_week_rolling_",cox_surv_date_six_week_rolling,".rda"))
load(paste0("EDcrowding/real-time/model-input/cox_surv_six_week_rolling_",cox_surv_date_six_week_rolling,".rda"))




# load poisson means for not-yet-arrived to get max nya by time of day and time window
load("EDcrowding/real-time/model-input/poisson_not_yet_arrived.rda")

# load results of poisson equations
poisson_file = paste0("EDcrowding/real-time/model-input/poisson_results_", poisson_results_date, ".rda")
load(poisson_file)

# Create set of time points to evaluate over ------------------------------
model_features = "alop"
tsk_ids = "test"


if (tsk_ids %in% c("val", "test")) {
  
  # for (model_period_ in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
    # for (model_period_ in c("Post", "Pre + Post", "Post-SDEC")) {
    for (model_period_ in c("Post-SDEC")) {
    
    print(paste("Processing", model_period_))
    
    # dataset = set_train_val_test_dates(summ, NA, model_period_)
    # dataset = dataset[in_set == "Val"] 
    # 
    
    period_dates = set_train_val_test_dates(summ, NA, model_period_, TRUE)
    
    # Create set of time points to evaluate over  - using validation set at the moment
    
    if (tsk_ids == "val") {
      
      start_of_set = period_dates[2]
      end_of_set = period_dates[3]
      
    } else if (tsk_ids == "test") {
      
      start_of_set = period_dates[3]
      end_of_set = period_dates[4]
    }
    
    
    next_dt = start_of_set
    
    time_pts = POSIXct()
    while (next_dt < end_of_set) {
      next_pt <- next_dt + c(hours(6), hours(12), hours(16), hours(22))
      time_pts <- c(time_pts, next_pt)
      next_dt = next_dt + days(1)
    }
    
    # get poisson results (trained on training data) for not-yet-arrived
    poisson_results_ = poisson_results[model_period == model_period_]
    poisson_nya = poisson_not_yet_arrived_all[model_period == model_period_]
    
    poisson_means = get_poisson_means(time_pts, poisson_results_, poisson_nya, model_period_)
    
    # # temp code - merging with old file to find out why predictions are higher
    # poisson_nya$weekend = as.double(poisson_nya$weekend)
    # poisson_means = merge(poisson_means, poisson_nya, by = c("time_window", "time_of_report", "weekend"))
    # poisson_means[, poisson_mean :=  poisson_mean.y]
    # poisson_means[, max_nya :=  max_nya.y]

    
    # get survival curves trained on training data
    base_prob = cox_surv[model_period == model_period_]
    coefs = cox_results[model_period == model_period_]
    scale_params_ = scale_params[model_period == model_period_]
    setnames(coefs, "coefficents", "coef")
    

    
    # get model output file date - this is the date the ML models were traindd
    name_ = paste0("model_output_date_", model_period_)
    model_output_file_date = get(name_)
    
    # if (model_period_ == "Post-SDEC" & tsk_ids == "test" & summ_file_date_for_checking_real_time > summ_file_date) {
    #   
    #   # load a later summ in order to get people in ED during the post-SDEC test period - see config.R for more details
    #   load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
    #   adm_preds = make_predictions(time_pts, summ, dm_file_date, model_output_file_date,  model_features,  model_period_)
    #   
    #   # reload original summ file after
    #   load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
    #   
    # } else {
      
      adm_preds = make_predictions_using_3_models(time_pts, summ, dm_file_date, model_output_file_date,  model_features,  model_period_)
      
    # }
    
    # individual predictions for all individuals in ED at each time point
    
    
    # calculate number in ED for each patient's arrival time
    
    in_ED_all = get_cox_coef_by_csn(in_ED_all = adm_preds[[1]], coefs, real_time = FALSE, num_in_ED, model_period_, scale_params_, use_test_set_for_Cox_SDEC,
                                    use_six_week_rolling = TRUE)
    
    # get time in ED so far
    in_ED_all[, time_so_far := as.numeric(difftime(time_pt, first_ED_admission, units = "hours"))]
    
    prob_dist = get_prob_dist(time_pts, in_ED_all, preds_all_ts = adm_preds[[2]], 
                              base_prob, coefs, 
                              poisson_not_yet_arrived = poisson_means
    )
    

      
    prob_dist_file = paste0("EDcrowding/predict-admission/model-output/model_eval_xgb_3_models_", model_features, "_", model_period_, "_",
                              tsk_ids, "_six_week_rolling_", Sys.Date(),".rda")

    
    save(prob_dist, file = prob_dist_file)
    
  }
}



# Get aggregate predictions ---------------------------------------------------------




# get tta predictions



# # for debugging
# preds_all_ts = adm_preds[[2]]
# poisson_not_yet_arrived = poisson_not_yet_arrived[epoch == model_period_ & in_set == "Train"]

# aggregate predictions







