
# Load libraries ----------------------------------------------------------


library(dplyr)
library(ggfortify)
library(data.table)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
# library(polynom)


# Function to add exponential tail to survival curve -----------------------


add_exponential_tail <- function(cox_surv_curv) {
  
  # # add an exponential tail
  # if (model_period %in% c("Pre", "Pre + Post")) {
  #   start_tail = 150
  # } else if (model_period == "Post") {
  #   start_tail = 48
  # } else { # post-SDEC
  #   start_tail = 30
  # }
  
  # start the tail from the last 6 observations
  start_tail = as.numeric(cox_curve$time)[length(as.numeric(cox_curve$time)) -5]
  
  cox_tail = as.numeric(cox_curve$time)[cox_curve$time > start_tail]
  # set as the end time to predict as something infeasbily long
  # divide into 48 hour sections
  # this will give a tail to the disribution for any feasible LOS in ED
  times_to_pred = seq(start_tail + 1, max(cox_tail)*10, ((max(cox_tail)*10 - (start_tail + 1)) / length(cox_tail)))
  
  s =  cox_surv_curv[time > start_tail]
  exp_model = lm(log(s$surv)~ s$time)
  pred_surv <- exp(predict(exp_model,list(Time=times_to_pred)))
  
  # here, predict() returns the same number of probabilities as the original number of time points it was fitted on
  # hence this is a horrible workaround to get the same number of times to predict as the model wants to create
  extra_bit = data.table(time = times_to_pred[1:length(times_to_pred)-1], 
                         surv = pred_surv)
  
  # only include the extra bit that is after the maximum as.numeric(cox_curve$time)
  
  # cox_surv_curv = bind_rows(cox_surv_curv[time <= 24], extra_bit) 
  cox_surv_curv = bind_rows(cox_surv_curv, extra_bit[time > max(as.numeric(cox_curve$time))]) 
  
  
  return(cox_surv_curv)
  
}





# Load data ---------------------------------------------------------------


# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")

# add real-time app utils to get one hot encoding 
source("EDcrowding/real-time/app/utils.R")



# if (summ_file_date_for_checking_real_time > summ_file_date) {
#   
#   load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
#   
# } else {
  
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
# }




# Get admitted patients only ----------------------------------------------


admitted = summ[adm %in% c("direct_adm", "indirect_adm")]
# admitted[, epoch := factor(epoch, levels = c("Pre", "Post", "After"))]
# admitted[, tta := as.integer(difftime(first_outside_proper_admission, first_ED_admission, units = "hours"))]

# round down duration to 2 decimal places to reduce the number of times to compute survival analysis
admitted[, duration := round(difftime(left_ED, first_ED_admission, units = "hours"), digits = 2)]
admitted[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]
# admitted = admitted[epoch != "After",.(csn, duration, epoch, in_set, set = paste(epoch, in_set), first_ED_admission)]
# admitted[, tod := factor((hour(first_ED_admission) %/% 4)+1)]
# admitted[, tod_6hr := factor((hour(first_ED_admission) %/% 6)+1)]
admitted[, quarter := factor(case_when( month(first_ED_admission) <= 3 ~ 1,
                                 month(first_ED_admission) <= 6 ~ 2, 
                                 month(first_ED_admission) <= 9 ~ 3, 
                                 month(first_ED_admission) <= 12 ~ 4))]
# admitted[, year := factor(year(first_ED_admission))]
admitted[, weekend := factor(if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]
# the lab closes at 10 pm 
# admitted[, night := factor(ifelse(hour(first_ED_admission) < 22 & hour(first_ED_admission) > 7, 0, 1))]

admitted[, arrival_time := factor(case_when(hour(first_ED_admission) >= 22 | hour(first_ED_admission) < 6 ~ "22:00-06:00",
                                           hour(first_ED_admission) >= 6 & hour(first_ED_admission) < 12 ~ "06:00-12:00",
                                           hour(first_ED_admission) >= 12 & hour(first_ED_admission) < 16 ~ "12:00-16:00",
                                           hour(first_ED_admission) >= 16 & hour(first_ED_admission) < 22 ~ "16:00-22:00"))]

admitted$status = 1


# Update with number of patients in ED ------------------------------------

# load file with each location
load(paste0("EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",num_in_ED_at_arrival_time_file_date,".rda"))

num_in_ED = data.table(num_in_ED)
admitted[, DateTime := as.POSIXct(substr(first_ED_admission, 1, 16))]
admitted = merge(admitted, num_in_ED, by = c("DateTime", "quarter", "weekend", "arrival_time"))

# now set weekend as numeric variable (needed to be factor to merge)
admitted[, weekend := if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]


admitted[, num_MAJORS_RESUS := `num_ED MAJORS` + `num_ED RESUS`]
admitted[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS]

# add num SDEC for post-SDEC model only 
admitted[, num_SDEC := num_SDEC + `num_SDEC Waiting`]





# # apply one hot encoding of factors
# admitted = one_hot(admitted)


# Empirical plots of survival curve for train, test, val ---------------------------------------------

admitted = admitted[duration < 24]

all_surv_curves = data.table()

for (model_period_ in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
  
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
                                          labels = c("Pre Covid", "Post Covid", "Pre + Post Covid", "Post-SDEC"))]
all_surv_curves[, in_set := factor(in_set, levels = c("Train", "Val", "Test"))]

# all survival curves in grid of four
all_surv_curves %>% ggplot(aes(x = time, y = empirical, colour = in_set)) + geom_line(size = 1.25) +
  facet_wrap(model_period_with_text~.) + theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(colour = "Set",
       title = "Survival curves for time spent by admitted patients in ED by model period and dataset",
       x = "Time since arrival in ED (cut off at 24 hours)",
       y = "Survival probability") +
  scale_colour_manual(values = c("#00BA38", "#619CFF","#F8766D", guide = NULL, name = NULL))

# # all survival curves on one chart
# all_surv_curves[model_period != "Pre + Post"] %>% ggplot(aes(x = time, y = empirical, colour = in_set,  linetype = model_period_with_text)) + geom_line(size = 1.25) +
#   # facet_wrap(model_period_with_text~.) + 
#   theme_bw() + 
#   theme(legend.position = "right") + 
#   labs(colour = "Set",
#        linetype = "Model period", 
#        title = "Survival curves for time spent by admitted patients in ED by model period and dataset",
#        x = "Time since arrival in ED (cut off at 24 hours)",
#        y = "Survival probability") +
#   scale_linetype_manual(values = c("dotted", "dotdash", "solid")) + #"longdash", 
#   scale_colour_manual(values = c("#00BA38","#619CFF", "#F8766D", guide = NULL, name = NULL))
# 
# 
# 
# # add pre Covid training set to post - doesn't show much
# all_surv_curves[model_period == "Post" | (model_period == "Pre" & in_set == "Train")] %>% 
#   ggplot(aes(x = time)) + geom_line(aes( y = empirical, colour = in_set, linetype = model_period ), size = 1.25) +
#   # facet_grid(model_period_with_text~.) + 
#   theme_bw() + 
#   theme(legend.position = "bottom") + 
#   labs(colour = "Set",
#        title = "Survival curves for time spent by admitted patients in ED by model period and dataset",
#        x = "Time since arrival in ED (cut off at 24 hours)",
#        y = "Survival probability") +
#   scale_colour_manual(values = c("#00BA38", "#619CFF","#F8766D", guide = NULL, name = NULL))
# 
# # add post-SDEC to post - shows how much SDEC has extended the time spent
# all_surv_curves[model_period == "Post" | (model_period == "Post-SDEC")] %>% 
#   ggplot(aes(x = time)) + geom_line(aes( y = empirical, colour = in_set, linetype = model_period ), size = 1.25) +
#   # facet_grid(model_period_with_text~.) + 
#   theme_bw() + 
#   theme(legend.position = "bottom") + 
#   labs(colour = "Set",
#        title = "Comparing Post Covid sets with Post-SDEC training training and validation set",
#        x = "Time since arrival in ED (cut off at 24 hours)",
#        y = "Survival probability") +
#   scale_colour_manual(values = c("#00BA38", "#619CFF","#F8766D", guide = NULL, name = NULL))


# Derive cox models using six week rolling average --------------------------------------------------------


cox_results = data.table()
cox_surv = data.table()
scale_params = data.table()

admitted[, first_ED_admission_date := date(first_ED_admission)]
admitted = admitted[!is.na(num_elsewhere)]

admitted[, weekend_1 := as.numeric(weekend ==1)]

admitted[, arrival_time_06_12 := as.numeric(arrival_time == "06:00-12:00")]
admitted[, arrival_time_12_16 := as.numeric(arrival_time == "12:00-16:00")]
admitted[, arrival_time_16_22 := as.numeric(arrival_time == "16:00-22:00")]
admitted[, arrival_time_22_06 := as.numeric(arrival_time == "22:00-06:00")]

for (model_period in c("Post", "Post-SDEC")) {
# for (model_period in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
    
  
  dataset = set_train_val_test_dates(admitted, NA, model_period)
  # dataset = dataset[in_set == "Train"]
  
  start_date = dataset[in_set %in% c("Train", "Val", "Test"), min(first_ED_admission_date)] 
  end_date = dataset[in_set %in% c("Train", "Val", "Test"), max(first_ED_admission_date)]
  dates_ = seq.Date(start_date, end_date, by = "1 day")
  
  for (i in 2:length(dates_)) {
    
    print(i)
    
    df_ = dataset[first_ED_admission_date >= dates_[i] - weeks(6) & first_ED_admission_date < dates_[i]]
    
    if (model_period == "Post-SDEC") {
      
      df_[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS - num_SDEC - `num_SDEC Waiting`]
      
      cols_to_scale = c("num_MAJORS_RESUS", "num_elsewhere", "num_SDEC",
                        # "quarter_1", "quarter_2", "quarter_3", "quarter_4",
                        "weekend_1",
                        "arrival_time_06_12", "arrival_time_12_16", "arrival_time_16_22", "arrival_time_22_06")
      
      scale_params_ = data.table(df_ %>% select(all_of(cols_to_scale))  %>%  pivot_longer(cols_to_scale))
      scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
      scale_params_$first_ED_admission_date = dates_[i]
      scale_params_$model_period = model_period
      scale_params = bind_rows(scale_params, scale_params_)
      
      df_[, num_MAJORS_RESUS_scaled := scale(num_MAJORS_RESUS)]
      df_[, num_SDEC_scaled := scale(num_SDEC)]  
      df_[, num_elsewhere_scaled := scale(num_elsewhere)]
      
    } else {
      
      cols_to_scale = c("num_MAJORS_RESUS", "num_elsewhere",
                        # "quarter_1", "quarter_2", "quarter_3", "quarter_4",
                        "weekend_1",
                        "arrival_time_06_12", "arrival_time_12_16", "arrival_time_16_22", "arrival_time_22_06")
      
      scale_params_ = data.table(df_ %>% select(all_of(cols_to_scale)) %>%  pivot_longer(cols_to_scale))
      scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
      scale_params_$first_ED_admission_date = dates_[i]
      scale_params_$model_period = model_period
      scale_params = bind_rows(scale_params, scale_params_)
      
      df_[, num_MAJORS_RESUS_scaled := scale(num_MAJORS_RESUS)]
      df_[, num_elsewhere_scaled := scale(num_elsewhere)] 
      
    }
    
    # df_[, quarter_2 := scale(quarter_2)]
    # df_[, quarter_3 := scale(quarter_3)]
    # df_[, quarter_4 := scale(quarter_4)]
    
    df_[, weekend_1 := scale(weekend)]
    
    df_[, arrival_time_06_12 := scale(arrival_time_06_12)]
    df_[, arrival_time_12_16 := scale(arrival_time_12_16)]
    df_[, arrival_time_16_22 := scale(arrival_time_16_22)]
    df_[, arrival_time_22_06 := scale(arrival_time_22_06)]
    
    
    if (model_period == "Pre + Post") {
      
      # extra addition to scale_params for additional factor
      df_[, post_Covid_1 := as.numeric(first_ED_admission > covid_start)]
      
      cols_to_scale = c("post_Covid_1")
      
      scale_params_ = data.table(df_ %>% select(all_of(cols_to_scale)) %>%  pivot_longer(cols_to_scale))
      scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
      scale_params_$first_ED_admission_date = dates_[i]
      scale_params_$model_period = model_period
      scale_params = bind_rows(scale_params, scale_params_)
      
      df_[, post_Covid_1 := scale(post_Covid_1)]
      
      cox <- coxph(Surv(duration, status) ~ 
                     # quarter_2 + quarter_3 + quarter_4 + 
                     weekend_1 + 
                     arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                     num_MAJORS_RESUS_scaled + 
                     num_elsewhere_scaled + post_Covid_1, data = df_, model = TRUE)
      
      
    } else if (model_period == "Pre") {
      # no quarter 2 for Pre Covid, as quarter 1 is not including in the training set
      # therefore quarter 2 is needed as reference category
      
      cox <- coxph(Surv(duration, status) ~ 
                     # quarter_3 + quarter_4 + 
                     weekend_1 + 
                     arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                     num_MAJORS_RESUS_scaled + 
                     num_elsewhere_scaled , data = df_, model = TRUE)
      
    } else if (model_period == "Post"){
      
      cox <- coxph(Surv(duration, status) ~ 
                     # quarter_2 + quarter_3 + quarter_4 + 
                     weekend_1 + 
                     arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                     num_MAJORS_RESUS_scaled + 
                     num_elsewhere_scaled , data = df_, model = TRUE)
      
      
    } else { # model period = Post SDEC
      
      
      
      # no quarter 3 for post-SDEC
      
      cox <- coxph(Surv(duration, status) ~ 
                     # quarter_2 + quarter_4 + 
                     weekend_1 + 
                     arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                     num_MAJORS_RESUS_scaled + 
                     num_SDEC_scaled +
                     num_elsewhere_scaled , data = df_, model = TRUE)
      
    }
    
    cox_curve = summary(survfit(cox))
    cox_surv_curv = data.table(    time = cox_curve$time, 
                                   surv = cox_curve$surv)
    cox_surv_curv = add_exponential_tail(cox_surv_curv)
     
    cox_surv_curv$first_ED_admission_date = dates_[i]
    cox_surv_curv$model_period = model_period
    
    
    
    results <- data.table(first_ED_admission_date = dates_[i],
                          variables = names(cox$coefficients),
                          coefficents = cox$coef,
                          st_error = summary(cox)$coefficients[, 3],
                          CI_lower = exp(confint(cox))[,1], 
                          CI_upper = exp(confint(cox))[,2],
                          N = nrow(df_))
    results$model_period = model_period
    
    
    cox_results = bind_rows(cox_results, results)
    cox_surv = bind_rows(cox_surv, cox_surv_curv)
  }

  # scale the variables for Cox regresion

  
  

}


# save for use in real-time predictions
outFile = (paste0("EDcrowding/real-time/model-input/cox_results_six_week_rolling_",today(),".rda"))
save(cox_results, file = outFile)

outFile = (paste0("EDcrowding/real-time/model-input/scale_params_six_week_rolling_",today(),".rda"))
save(scale_params, file = outFile)

# cox_results = cox_results[!is.na(coefficents)]
# cox_results[, coefficents_formatted := paste(as.character(round(coefficents, 2)), "[", 
#                                               as.character(round(CI_lower, 2)), ", ", 
#                                               as.character(round(CI_upper, 2)), "]")]
# 
# # also save to csv file for reporting pruposes
# outFile = (paste0("EDcrowding/predict-admission/model-output/cox_results_",today(),".csv"))
# write_csv(cox_results, file = outFile)

outFile = (paste0("EDcrowding/real-time/model-input/cox_surv_six_week_rolling_",today(),".rda"))
save(cox_surv, file = outFile)



# Check  distributions  ----------------------------------------------------

# truncating admitted at 48 hours for easier plotting

model_period_ = "Post"

dates_ = seq.Date(date(start_test_post), date(end_study), by = "1 week")

plot_data = data.table()


for (i in 1:length(dates_)) {
  
  first_ED_admission_date_ = dates_[i]
  
  
  dataset = admitted[first_ED_admission_date == first_ED_admission_date_]
  
  scale_params_ = scale_params[first_ED_admission_date == first_ED_admission_date_]
  
  
  if (model_period_ == "Pre + Post") {
    
    dataset[, post_Covid_1 := as.numeric(first_ED_admission > covid_start)]
    
    dataset[, post_Covid_1 := (post_Covid_1 - scale_params_[name == "post_Covid_1", mean])/
              scale_params_[name == "post_Covid_1", sd]]
    
  } else if (model_period_ == "Post-SDEC") {
    
    dataset[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS - num_SDEC - `num_SDEC Waiting`]
    
    # I have moved this from below but haven't checked it
    dataset[, num_SDEC_scaled := (num_SDEC - scale_params_[name == "num_SDEC", mean])/
              scale_params_[name == "num_SDEC", sd]]

  }
  
  
  # scale the variables for Cox regresion
  # note that it should not matter if all of these are not used in the final cox predictions
  # because scale params were saved for them anyway
  
  dataset[, num_MAJORS_RESUS_scaled := (num_MAJORS_RESUS - scale_params_[name == "num_MAJORS_RESUS", mean])/
            scale_params_[name == "num_MAJORS_RESUS", sd]]
  dataset[, num_elsewhere_scaled := (num_elsewhere - scale_params_[name == "num_elsewhere", mean])/
            scale_params_[name == "num_elsewhere", sd]]
  # dataset[, num_SDEC_scaled := (num_SDEC - scale_params_[name == "num_SDEC", mean])/
  #           scale_params_[name == "num_SDEC", sd]]
  
  # dataset[, quarter_2 := (quarter_2 - scale_params_[name == "quarter_2", mean])/
  #           scale_params_[name == "quarter_2", sd]]
  # dataset[, quarter_3 := (quarter_3 - scale_params_[name == "quarter_3", mean])/
  #           scale_params_[name == "quarter_3", sd]]
  # dataset[, quarter_4 := (quarter_4 - scale_params_[name == "quarter_4", mean])/
  #           scale_params_[name == "quarter_4", sd]]
  
  dataset[, weekend_1 := (weekend_1 - scale_params_[name == "weekend_1", mean])/
            scale_params_[name == "weekend_1", sd]]
  
  dataset[, arrival_time_06_12 := (arrival_time_06_12 - scale_params_[name == "arrival_time_06_12", mean])/
            scale_params_[name == "arrival_time_06_12", sd]]
  dataset[, arrival_time_12_16 := (arrival_time_12_16 - scale_params_[name == "arrival_time_12_16", mean])/
            scale_params_[name == "arrival_time_12_16", sd]]
  dataset[, arrival_time_16_22 := (arrival_time_16_22 - scale_params_[name == "arrival_time_16_22", mean])/
            scale_params_[name == "arrival_time_16_22", sd]]

  
  
  # get the modelled distribution
  model_prob = cox_surv[time < 48 & first_ED_admission_date == first_ED_admission_date_, .(time, surv)]
  model_prob[, model := "cox model"]
  
  # get the empirical distribution
  empirical = numeric()
  for (i in (1:nrow(model_prob))) {
    empirical <- c(empirical, sum(dataset[, duration > model_prob$time[i]])/nrow(dataset))
  }
  
  empirical_curv = data.table(    time = model_prob$time, 
                                  surv = empirical)
  empirical_curv[, model := "empirical data"]
  
  # get 10 random patients
  
  coefs = cox_results[first_ED_admission_date == first_ED_admission_date_]
  setnames(coefs, "coefficents", "coef")
  
  if (model_period_ == c("Post")) {
    
    dataset[, pow := 
              # (coefs[variables == "quarter_2", coef]*quarter_2) + 
              # (coefs[variables == "quarter_3", coef]*quarter_3) + 
              # (coefs[variables == "quarter_4", coef]*quarter_4) + 
              (coefs[variables == "weekend_1", coef]*weekend_1) + 
              (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
              (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
              (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
              (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
              ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled)]

    
  } else if (model_period_ == c("Pre + Post")) {
    # 
    # dataset[, post_Covid := as.numeric(first_ED_admission > covid_start)]
    # dataset[, post_Covid_1 := scale(post_Covid)]
    # 
    dataset[, pow := 
              # (coefs[variables == "quarter_2", coef]*quarter_2) + 
              # (coefs[variables == "quarter_3", coef]*quarter_3) + 
              # (coefs[variables == "quarter_4", coef]*quarter_4) + 
              (coefs[variables == "weekend_1", coef]*weekend_1) + 
              (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
              (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
              (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
              (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
              ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled) +
              ( coefs[variables == "post_Covid_1", coef] * post_Covid_1)]
    
  } else if (model_period_ == "Pre") {
    
    dataset[, pow := 
              # (coefs[variables == "quarter_2", coef]*quarter_2) + 
              # (coefs[variables == "quarter_3", coef]*quarter_3) + 
              # (coefs[variables == "quarter_4", coef]*quarter_4) +
              (coefs[variables == "weekend_1", coef]*weekend_1) + 
              (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
              (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
              (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
              (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
              ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled)]
    
  } else if (model_period_ == "Post-SDEC") {

    
    dataset[, pow := 
              # (coefs[variables == "quarter_2", coef]*quarter_2) + 
              # # (coefs[variables == "quarter_3", coef]*quarter_3) + 
              # (coefs[variables == "quarter_4", coef]*quarter_4) +
              (coefs[variables == "weekend_1", coef]*weekend_1) + 
              (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
              (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
              (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
              (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
              ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled) +
              ( coefs[variables == "num_SDEC_scaled", coef] * num_SDEC_scaled) ]
    
  }
  
  
  dataset[, epow := exp(pow)]
  
  
  
  random_data = data.table()
  for (i in floor(runif(10, min=1, max=nrow(dataset)))) {

    random_data = bind_rows(random_data, data.table(time = model_prob$time,
                                                    surv = model_prob$surv ^ dataset$epow[i],
                                                    model = as.character(i)))
  }
  
  average_data = random_data[, mean(surv), by = "time"]
  setnames(average_data, "V1", "surv")
  average_data$model = paste("10 random admissions")
  
  plot_data_ = bind_rows(empirical_curv, model_prob, average_data)
  plot_data_$first_ED_admission_date = first_ED_admission_date_
  
  # add original survival curve from test set
  plot_data = bind_rows(plot_data,plot_data_, all_surv_curves[model_period == "Post" & in_set == "Train", 
                                                   .(time, surv = empirical, model = "training set", 
                                                     first_ED_admission_date = first_ED_admission_date_)])
  


}

library(gridExtra)

plot_data %>% ggplot(aes (x =time, y = surv, col = model)) + geom_line() + theme(legend.position = "bottom") +
  labs(title = "Probability of still being in ED for patients admitted on a single day using six week rolling average",
       x = "Time since admission (hours)") +
  # labs(title = paste("Admissions on", first_ED_admission_date)) +
  facet_wrap(.~first_ED_admission_date)


name_ = paste0("plot", gsub(" ", "", first_ED_admission_date_))
assign(name_, plot_)

# plot over all Post-Covid
grid.arrange(`plot2020-05-01`, `plot2020-08-01`, `plot2020-11-01`, `plot2021-02-01`, `plot2021-05-01`, ncol = 1)

# plot for test set
grid.arrange(`plot2021-05-20`, `plot2021-06-20`, `plot2021-07-19`, ncol = 2)



# Plot with original curve ------------------------------------------------





# Checking using the function from utils.R --------------------------------


admitted = summ[adm %in% c("direct_adm", "indirect_adm")]
# admitted[, epoch := factor(epoch, levels = c("Pre", "Post", "After"))]
# admitted[, tta := as.integer(difftime(first_outside_proper_admission, first_ED_admission, units = "hours"))]

# round down duration to 2 decimal places to reduce the number of times to compute survival analysis
admitted[, duration := round(difftime(left_ED, first_ED_admission, units = "hours"), digits = 2)]
admitted[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]

for (model_period_ in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
  
  dataset = set_train_val_test_dates(admitted, NA, model_period_)
  dataset = dataset[in_set == "Train" & duration < 48]
  
  base_prob = cox_surv[model_period == model_period_]
  coefs = cox_results[model_period == model_period_]
  scale_params_ = scale_params[model_period == model_period_]
  setnames(coefs, "coefficents", "coef")
  
  dataset = get_cox_coef_by_csn(dataset, coefs, FALSE, num_in_ED, model_period_, scale_params_)
  
  
  # get the modelled distribution
  model_prob = base_prob[time < 48 & model_period == model_period_, .(time, surv)]
  model_prob[, model := "cox model"]
  
  # get the empirical distribution
  empirical = numeric()
  for (i in (1:nrow(model_prob))) {
    empirical <- c(empirical, sum(dataset[, duration > model_prob$time[i]])/nrow(dataset))
  }
  
  empirical_curv = data.table(    time = model_prob$time, 
                                  surv = empirical)
  empirical_curv[, model := "empirical data"]
  
  
  
  random_data = data.table()
  for (i in floor(runif(100, min=1, max=nrow(dataset)))) {
    
    random_data = bind_rows(random_data, data.table(time = base_prob$time,
                                                    surv = base_prob$surv ^ dataset$epow[i],
                                                    model = as.character(i)))
  }
  
  average_data = random_data[, mean(surv), by = "time"]
  setnames(average_data, "V1", "surv")
  average_data$model = "100 random patients"
  
  plot_data = bind_rows(empirical_curv, model_prob, average_data)
  plot_ = plot_data[time < 48] %>% ggplot(aes (x =time, y = surv, col = model)) + geom_line() + theme(legend.position = "bottom") +
    labs(title = model_period_)
  
  
  name_ = paste0(gsub(" ", "", model_period_), "plot")
  assign(name_, plot_)
}



library(gridExtra)
grid.arrange(Preplot, `Pre+Postplot`, Postplot, `Post-SDECplot`)





# Test set predictions ----------------------------------------------------

model_features = "alop"
tsk_ids = "test"

load(paste0("EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",num_in_ED_at_arrival_time_file_date,".rda"))
num_in_ED = data.table(num_in_ED)

# load Cox regression results (survival probs and regression coefs)
# load rolling average survival curves
load(paste0("EDcrowding/real-time/model-input/cox_results_six_week_rolling_",cox_surv_date_six_week_rolling,".rda"))
load(paste0("EDcrowding/real-time/model-input/scale_params_six_week_rolling_",cox_surv_date_six_week_rolling,".rda"))
load(paste0("EDcrowding/real-time/model-input/cox_surv_six_week_rolling_",cox_surv_date_six_week_rolling,".rda"))




distr_coll = data.table()
adm_coll = data.table()



for (model_period_ in c(
  # "Pre", 
                        "Post", 
                        # "Pre + Post", 
                        "Post-SDEC")) {
  
  # load time points of test set
  
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
  
  
  # get survival curves trained on training data
  base_prob = cox_surv[model_period == model_period_]
  coefs = cox_results[model_period == model_period_]
  scale_params_ = scale_params[model_period == model_period_]
  setnames(coefs, "coefficents", "coef")
  
  # get model output file date - this is the date the ML models were traindd
  name_ = paste0("model_output_date_", model_period_)
  model_output_file_date = get(name_)
  
  # use this function (not because we need admission probability, but because it will return who is in the ED at the time point)
  adm_preds = make_predictions(time_pts, summ, dm_file_date, model_output_file_date,  model_features,  model_period_)
  
  
  
  in_ED_all = get_cox_coef_by_csn(in_ED_all = adm_preds[[1]], coefs, real_time = FALSE, num_in_ED, model_period_, scale_params_, use_test_set_for_Cox_SDEC,
                                  use_six_week_rolling = TRUE)
  in_ED_all[, tta := as.numeric(difftime(left_ED, first_ED_admission, units = "hours"))]
  
  # get predictions for patients yet to arrive for each time point
  for (i in (1:length(time_pts))) {
    
    print(i)
        
    
    # looking only at patients who were later admitted
    in_ED = in_ED_all[time_pt == time_pts[i] & adm %in% c("direct_adm", "indirect_adm")]
    
    if (nrow(in_ED) != 0) {
      
      # get the probability of admission for each individual
      # df = merge(in_ED[,.(csn, adm, timeslice, time_so_far, left_ED)], preds_all_ts[,.(csn, prob.1, timeslice)], 
      #            by = c("csn", "timeslice"), all.x = TRUE)
      df = in_ED
      distr = data.table()
      adm_ = data.table()
      
      
      # for each csn, find their probability of still being in ED at this long after arrival
      for (h in 1 : nrow(df)) {
        
        base_prob_ = base_prob[first_ED_admission_date == in_ED_all[csn == df$csn[h]  & time_pt == time_pts[i],
                                                                    first_ED_admission_date] ]
        
        # get patients cumulative probability of even
        base_prob_$cdf = 1 - base_prob_$surv
        
        
        
        # save the probability 
        distr = bind_rows(distr, data.table(csn = df$csn[h], 
                                            # sampling uniformally from the cdf to get fewer rows
                                            time = base_prob_[seq(0, floor(nrow(base_prob_)/100)*100, 10), time],
                                            cdf = base_prob_[seq(0, floor(nrow(base_prob_)/100)*100, 10), cdf]))
        
        adm_ =  bind_rows(adm_, data.table(csn = df$csn[h], 
                                           tta = df$tta[h],
                                           tta_cdf = base_prob_[time > df$tta[h], min(cdf)]))
        
      }
      
      distr$time_of_report = time_pts[i]
      distr$model_period = model_period_
      
      adm_$time_of_report = time_pts[i]
      adm_$model_period = model_period_  
      
      distr[, prob := 1/length(unique(distr[, cdf]))]
      # add cdfs of the probability of each cdf value being observed
      # all are equally likely because they were all calculated from survival curves with the same number of survival times
      distr[, cdf_of_cdfs := cumsum(prob), by = csn]
      
      distr_coll = bind_rows(distr_coll, distr)
      adm_coll = bind_rows(adm_coll, adm_)
      
      
    }
    
    
    
  }
}
qq_plot_data_6week = data.table()


for (model_period_ in c("Pre", "Post", 
                        # "Pre + Post", 
                        "Post-SDEC")) { 
  
  qq_data = data.table()
  num_csns = length(unique(distr_coll[model_period == model_period_, csn]))
  cdf_increments = unique(distr_coll[model_period == model_period_, cdf_of_cdfs])
  
  
  for (c in cdf_increments) {
    
    
    
    qq_data  = bind_rows(qq_data , data.table(model_cdf = c,
                                              observed_cdf = nrow(adm_coll[tta_cdf <= c & model_period == model_period_])/nrow(adm_coll[model_period == model_period_]),
                                              model_period = model_period_))
    
    qq_plot_data_6week = bind_rows(qq_plot_data_6week, qq_data )
  }
}



# plot chart

qq_plot_data_6week[, model_period_ := case_when(model_period == "Post" ~  "Post_6wk",
                                                model_period == "Post-SDEC" ~  "Post-SDEC_6wk")]
qq_plot_data_6week[, model_period_text := factor(model_period_, levels = c("Pre", "Post", "Post_6wk", "Pre + Post", "Post-SDEC", "Post-SDEC_6wk"),
                                           labels = c("Pre-Covid", "Covid", "Covid with SWSC", "Pre + Post Covid", "Post-SDEC", "Post-SDEC with SWSC"))]


qq_cox_6week_rolling = qq_plot_data_6week %>% 
  
  ggplot(aes(y = observed_cdf)) + 
  geom_point(aes(x = model_cdf), size = 1.5) + 
  geom_line(aes(x = observed_cdf), colour = "grey90") +
  facet_grid(~ ~ model_period_text, switch = "y") +
  
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
  
  scale_colour_manual(values = c("#F4B183", "#D53E4F")) +
  theme(legend.title=element_blank())

plot_chart(qq_cox_6week_rolling, "qq_cox_6week_rolling", width__ = 300, height__ = 200, layout = NA) 
plot_chart(qq_cox_6week_rolling, "qq_cox_6week_rolling_wide", width__ = 400, height__ = 200, layout = NA) 
