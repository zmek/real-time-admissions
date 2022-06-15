
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


add_exponential_tail <- function(cox_surv_curv, model_period) {
  
  # add an exponential tail
  if (model_period %in% c("Pre", "Pre + Post")) {
    start_tail = 150
  } else if (model_period == "Post") {
    start_tail = 48
  } else { # post-SDEC
    start_tail = 30
  }
  
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
  extra_bit = data.table(time = times_to_pred[1:length(times_to_pred)-1], surv = pred_surv, model_period = model_period)
  
  cox_surv_curv = bind_rows(cox_surv_curv[time <= 24], extra_bit) 
  
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
  
  # load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
  
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

# all survival curves on one chart
all_surv_curves[model_period != "Pre + Post"] %>% ggplot(aes(x = time, y = empirical, colour = in_set,  linetype = model_period_with_text)) + geom_line(size = 1.25) +
  # facet_wrap(model_period_with_text~.) + 
  theme_bw() + 
  theme(legend.position = "right") + 
  labs(colour = "Set",
       linetype = "Model period", 
       title = "Survival curves for time spent by admitted patients in ED by model period and dataset",
       x = "Time since arrival in ED (cut off at 24 hours)",
       y = "Survival probability") +
  scale_linetype_manual(values = c("dotted", "dotdash", "solid")) + #"longdash", 
  scale_colour_manual(values = c("#00BA38","#619CFF", "#F8766D", guide = NULL, name = NULL))



# add pre Covid training set to post - doesn't show much
all_surv_curves[model_period == "Post" | (model_period == "Pre" & in_set == "Train")] %>% 
  ggplot(aes(x = time)) + geom_line(aes( y = empirical, colour = in_set, linetype = model_period ), size = 1.25) +
  # facet_grid(model_period_with_text~.) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(colour = "Set",
       title = "Survival curves for time spent by admitted patients in ED by model period and dataset",
       x = "Time since arrival in ED (cut off at 24 hours)",
       y = "Survival probability") +
  scale_colour_manual(values = c("#00BA38", "#619CFF","#F8766D", guide = NULL, name = NULL))

# add post-SDEC to post - shows how much SDEC has extended the time spent
all_surv_curves[model_period == "Post" | (model_period == "Post-SDEC")] %>% 
  ggplot(aes(x = time)) + geom_line(aes( y = empirical, colour = in_set, linetype = model_period ), size = 1.25) +
  # facet_grid(model_period_with_text~.) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(colour = "Set",
       title = "Comparing Post Covid sets with Post-SDEC training training and validation set",
       x = "Time since arrival in ED (cut off at 24 hours)",
       y = "Survival probability") +
  scale_colour_manual(values = c("#00BA38", "#619CFF","#F8766D", guide = NULL, name = NULL))


# Derive cox models --------------------------------------------------------


cox_results = data.table()
cox_surv = data.table()
scale_params = data.table()

for (model_period in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
  
  dataset = set_train_val_test_dates(admitted, NA, model_period)
  dataset = dataset[in_set == "Train"]
  
  # scale the variables for Cox regresion
  dataset = dataset[!is.na(num_elsewhere)]
  
  dataset[, quarter_1 := as.numeric(quarter ==1)]
  dataset[, quarter_2 := as.numeric(quarter ==2)]
  dataset[, quarter_3 := as.numeric(quarter ==3)]
  dataset[, quarter_4 := as.numeric(quarter ==4)]
  
  dataset[, weekend_1 := as.numeric(weekend ==1)]
  
  dataset[, arrival_time_06_12 := as.numeric(arrival_time == "06:00-12:00")]
  dataset[, arrival_time_12_16 := as.numeric(arrival_time == "12:00-16:00")]
  dataset[, arrival_time_16_22 := as.numeric(arrival_time == "16:00-22:00")]
  dataset[, arrival_time_22_06 := as.numeric(arrival_time == "22:00-06:00")]
  
  if (model_period == "Post-SDEC") {
    
    dataset[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS - num_SDEC - `num_SDEC Waiting`]
    
    cols_to_scale = c("num_MAJORS_RESUS", "num_elsewhere", "num_SDEC",
                      "quarter_1", "quarter_2", "quarter_3", "quarter_4",
                      "weekend_1",
                      "arrival_time_06_12", "arrival_time_12_16", "arrival_time_16_22", "arrival_time_22_06")
    
    scale_params_ = data.table(dataset %>% select(all_of(cols_to_scale))  %>%  pivot_longer(cols_to_scale))
    scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
    scale_params_$model_period = model_period
    scale_params = bind_rows(scale_params, scale_params_)
    
    dataset[, num_MAJORS_RESUS_scaled := scale(num_MAJORS_RESUS)]
    dataset[, num_SDEC_scaled := scale(num_SDEC)]  
    dataset[, num_elsewhere_scaled := scale(num_elsewhere)]
    
  } else {
    
    cols_to_scale = c("num_MAJORS_RESUS", "num_elsewhere",
                      "quarter_1", "quarter_2", "quarter_3", "quarter_4",
                      "weekend_1",
                      "arrival_time_06_12", "arrival_time_12_16", "arrival_time_16_22", "arrival_time_22_06")
    
    scale_params_ = data.table(dataset %>% select(all_of(cols_to_scale)) %>%  pivot_longer(cols_to_scale))
    scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
    scale_params_$model_period = model_period
    scale_params = bind_rows(scale_params, scale_params_)
    
    dataset[, num_MAJORS_RESUS_scaled := scale(num_MAJORS_RESUS)]
    dataset[, num_elsewhere_scaled := scale(num_elsewhere)] 
    
  }
  
  dataset[, quarter_2 := scale(quarter_2)]
  dataset[, quarter_3 := scale(quarter_3)]
  dataset[, quarter_4 := scale(quarter_4)]
  
  dataset[, weekend_1 := scale(weekend)]
  
  dataset[, arrival_time_06_12 := scale(arrival_time_06_12)]
  dataset[, arrival_time_12_16 := scale(arrival_time_12_16)]
  dataset[, arrival_time_16_22 := scale(arrival_time_16_22)]
  dataset[, arrival_time_22_06 := scale(arrival_time_22_06)]

  
  if (model_period == "Pre + Post") {
    
    # extra addition to scale_params for additional factor
    dataset[, post_Covid_1 := as.numeric(first_ED_admission > covid_start)]
    
    cols_to_scale = c("post_Covid_1")
    
    scale_params_ = data.table(dataset %>% select(all_of(cols_to_scale)) %>%  pivot_longer(cols_to_scale))
    scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
    scale_params_$model_period = model_period
    scale_params = bind_rows(scale_params, scale_params_)
    
    dataset[, post_Covid_1 := scale(post_Covid_1)]

    cox <- coxph(Surv(duration, status) ~ 
                   quarter_2 + quarter_3 + quarter_4 + weekend_1 + 
                   arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                   num_MAJORS_RESUS_scaled + 
                   num_elsewhere_scaled + post_Covid_1, data = dataset, model = TRUE)
    
    
  } else if (model_period == "Pre") {
    # no quarter 2 for Pre Covid, as quarter 1 is not including in the training set
    # therefore quarter 2 is needed as reference category
    
    cox <- coxph(Surv(duration, status) ~ 
                   quarter_3 + quarter_4 + weekend_1 + 
                   arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                   num_MAJORS_RESUS_scaled + 
                   num_elsewhere_scaled , data = dataset, model = TRUE)
    
  } else if (model_period == "Post"){
    
    cox <- coxph(Surv(duration, status) ~ 
                   quarter_2 + quarter_3 + quarter_4 + weekend_1 + 
                   arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                   num_MAJORS_RESUS_scaled + 
                   num_elsewhere_scaled , data = dataset, model = TRUE)
    
    
  } else { # model period = Post SDEC
    

    
    # no quarter 3 for post-SDEC
    
    cox <- coxph(Surv(duration, status) ~ 
                   quarter_2 + quarter_4 + weekend_1 + 
                   arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                   num_MAJORS_RESUS_scaled + 
                   num_SDEC_scaled +
                   num_elsewhere_scaled , data = dataset, model = TRUE)
    
  }
  
  cox_curve = summary(survfit(cox))
  cox_surv_curv = data.table(    time = cox_curve$time, 
                                 surv = cox_curve$surv, 
                                 model_period = model_period)
  cox_surv_curv = add_exponential_tail(cox_surv_curv, model_period)
    
  
  results <- data.table(model_period = model_period, 
                        variables = names(cox$coefficients),
                        coefficents = cox$coef,
                        CI_lower = confint(cox)[,1], 
                        CI_upper = confint(cox)[,2],
                        st_error = summary(cox)$coefficients[, 3],
                        exp_coef = exp(cox$coef),
                        exp_CI_lower = exp(confint(cox))[,1], 
                        exp_CI_upper = exp(confint(cox))[,2])
  
  cox_results = bind_rows(cox_results, results)
  cox_surv = bind_rows(cox_surv, cox_surv_curv)

}


# save for use in real-time predictions
outFile = (paste0("EDcrowding/real-time/model-input/cox_results_",today(),".rda"))
save(cox_results, file = outFile)

outFile = (paste0("EDcrowding/real-time/model-input/scale_params_",today(),".rda"))
save(scale_params, file = outFile)

cox_results = cox_results[!is.na(coefficents)]

cox_results[, coefficents_formatted := paste0(format(round(coefficents, 2), nsmall = 2), " [", 
                                              format(round(CI_lower, 2), nsmall = 2), ", ", 
                                              format(round(CI_upper, 2), nsmall = 2), "]")]

cox_results[, coefficents_exp_formatted := paste0(format(round(exp_coef, 2), nsmall = 2), " [", 
                                              format(round(exp_CI_lower, 2), nsmall = 2), ", ", 
                                              format(round(exp_CI_upper, 2), nsmall = 2), "]")]



# also save to csv file for reporting pruposes
outFile = (paste0("EDcrowding/predict-admission/model-output/cox_results_",today(),".csv"))
write_csv(cox_results, file = outFile)

outFile = (paste0("EDcrowding/predict-admission/model-output/scale_params_",today(),".csv"))
write_csv(scale_params, file = outFile)

outFile = (paste0("EDcrowding/real-time/model-input/cox_surv_",today(),".rda"))
save(cox_surv, file = outFile)


# Adding an extra one for post-SDEC test set ------------------------------




cox_results = data.table()
cox_surv = data.table()
scale_params = data.table()

for (model_period in c("Post-SDEC")) {
  
  dataset = set_train_val_test_dates(admitted, NA, model_period)
  dataset = dataset[in_set == "Test"]
  
  # scale the variables for Cox regresion
  dataset = dataset[!is.na(num_elsewhere)]
  
  dataset[, quarter_1 := as.numeric(quarter ==1)]
  dataset[, quarter_2 := as.numeric(quarter ==2)]
  dataset[, quarter_3 := as.numeric(quarter ==3)]
  dataset[, quarter_4 := as.numeric(quarter ==4)]
  
  dataset[, weekend_1 := as.numeric(weekend ==1)]
  
  dataset[, arrival_time_06_12 := as.numeric(arrival_time == "06:00-12:00")]
  dataset[, arrival_time_12_16 := as.numeric(arrival_time == "12:00-16:00")]
  dataset[, arrival_time_16_22 := as.numeric(arrival_time == "16:00-22:00")]
  dataset[, arrival_time_22_06 := as.numeric(arrival_time == "22:00-06:00")]
  
  # if (model_period == "Post-SDEC") {
    
    dataset[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS - num_SDEC - `num_SDEC Waiting`]
    
    # all of post-SDEC test set is currently in quarter_3
    cols_to_scale = c("num_MAJORS_RESUS", "num_elsewhere", "num_SDEC",
                      "quarter_1", "quarter_2", "quarter_3", "quarter_4",
                      "weekend_1",
                      "arrival_time_06_12", "arrival_time_12_16", "arrival_time_16_22", "arrival_time_22_06")
    
    scale_params_ = data.table(dataset %>% select(all_of(cols_to_scale))  %>%  pivot_longer(cols_to_scale))
    scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
    scale_params_$model_period = model_period
    scale_params = bind_rows(scale_params, scale_params_)
    
    dataset[, num_MAJORS_RESUS_scaled := scale(num_MAJORS_RESUS)]
    dataset[, num_SDEC_scaled := scale(num_SDEC)]  
    dataset[, num_elsewhere_scaled := scale(num_elsewhere)]
    
  # } 
  
  dataset[, quarter_2 := scale(quarter_2)]
  dataset[, quarter_3 := scale(quarter_3)]
  dataset[, quarter_4 := scale(quarter_4)]
  
  dataset[, weekend_1 := scale(weekend)]
  
  dataset[, arrival_time_06_12 := scale(arrival_time_06_12)]
  dataset[, arrival_time_12_16 := scale(arrival_time_12_16)]
  dataset[, arrival_time_16_22 := scale(arrival_time_16_22)]
  dataset[, arrival_time_22_06 := scale(arrival_time_22_06)]
  
  
  # if (model_period == "Pre + Post") {
  #   
  #   # extra addition to scale_params for additional factor
  #   dataset[, post_Covid_1 := as.numeric(first_ED_admission > covid_start)]
  #   
  #   cols_to_scale = c("post_Covid_1")
  #   
  #   scale_params_ = data.table(dataset %>% select(all_of(cols_to_scale)) %>%  pivot_longer(cols_to_scale))
  #   scale_params_ = scale_params_[, .(mean = mean(value), sd = sd(value)), by = name]
  #   scale_params_$model_period = model_period
  #   scale_params = bind_rows(scale_params, scale_params_)
  #   
  #   dataset[, post_Covid_1 := scale(post_Covid_1)]
  #   
  #   cox <- coxph(Surv(duration, status) ~ 
  #                  quarter_2 + quarter_3 + quarter_4 + weekend_1 + 
  #                  arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
  #                  num_MAJORS_RESUS_scaled + 
  #                  num_elsewhere_scaled + post_Covid_1, data = dataset, model = TRUE)
  #   
  #   
  # } else if (model_period == "Pre") {
  #   # no quarter 2 for Pre Covid, as quarter 1 is not including in the training set
  #   # therefore quarter 2 is needed as reference category
  #   
  #   cox <- coxph(Surv(duration, status) ~ 
  #                  quarter_3 + quarter_4 + weekend_1 + 
  #                  arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
  #                  num_MAJORS_RESUS_scaled + 
  #                  num_elsewhere_scaled , data = dataset, model = TRUE)
  #   
  # } else if (model_period == "Post"){
  #   
  #   cox <- coxph(Surv(duration, status) ~ 
  #                  quarter_2 + quarter_3 + quarter_4 + weekend_1 + 
  #                  arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
  #                  num_MAJORS_RESUS_scaled + 
  #                  num_elsewhere_scaled , data = dataset, model = TRUE)
  #   
  #   
  # } else { # model period = Post SDEC
    
    
    
    # no quarter 3 for post-SDEC
    
    cox <- coxph(Surv(duration, status) ~ 
                   # quarter_2 + quarter_4 + 
                   weekend_1 + 
                   arrival_time_06_12 + arrival_time_12_16 + arrival_time_16_22 +
                   num_MAJORS_RESUS_scaled + 
                   num_SDEC_scaled +
                   num_elsewhere_scaled , data = dataset, model = TRUE)
    
  # }
  
  cox_curve = summary(survfit(cox))
  cox_surv_curv = data.table(    time = cox_curve$time, 
                                 surv = cox_curve$surv, 
                                 model_period = model_period)
  cox_surv_curv = add_exponential_tail(cox_surv_curv, model_period)
  
  
  results <- data.table(model_period = model_period, 
                        variables = names(cox$coefficients),
                        coefficents = cox$coef,
                        st_error = summary(cox)$coefficients[, 3],
                        CI_lower = exp(confint(cox))[,1], 
                        CI_upper = exp(confint(cox))[,2])
  
  cox_results = bind_rows(cox_results, results)
  cox_surv = bind_rows(cox_surv, cox_surv_curv)
  
}


# save for use in real-time predictions
outFile = (paste0("EDcrowding/real-time/model-input/cox_results_SDEC_test_set_",today(),".rda"))
save(cox_results, file = outFile)

outFile = (paste0("EDcrowding/real-time/model-input/scale_params_SDEC_test_set_",today(),".rda"))
save(scale_params, file = outFile)

cox_results = cox_results[!is.na(coefficents)]
cox_results[, coefficents_formatted := paste(as.character(round(coefficents, 2)), "[", 
                                             as.character(round(CI_lower, 2)), ", ", 
                                             as.character(round(CI_upper, 2)), "]")]

# also save to csv file for reporting pruposes
outFile = (paste0("EDcrowding/predict-admission/model-output/cox_results_SDEC_test_set_",today(),".csv"))
write_csv(cox_results, file = outFile)

outFile = (paste0("EDcrowding/real-time/model-input/cox_surv_SDEC_test_set_",today(),".rda"))
save(cox_surv, file = outFile)


# Check  distributions  ----------------------------------------------------

# truncating admitted at 48 hours for easier plotting


for (model_period_ in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
  
  dataset = set_train_val_test_dates(admitted, NA, model_period_)
  dataset = dataset[in_set == "Train" & duration < 48]
  
  scale_params_ = scale_params[model_period == model_period_]
  
  # scale the variables for Cox regresion
  dataset = dataset[!is.na(num_elsewhere)]
  
  dataset[, quarter_1 := as.numeric(quarter ==1)]
  dataset[, quarter_2 := as.numeric(quarter ==2)]
  dataset[, quarter_3 := as.numeric(quarter ==3)]
  dataset[, quarter_4 := as.numeric(quarter ==4)]
  
  dataset[, weekend_1 := as.numeric(weekend ==1)]
  
  dataset[, arrival_time_06_12 := as.numeric(arrival_time == "06:00-12:00")]
  dataset[, arrival_time_12_16 := as.numeric(arrival_time == "12:00-16:00")]
  dataset[, arrival_time_16_22 := as.numeric(arrival_time == "16:00-22:00")]
  dataset[, arrival_time_22_06 := as.numeric(arrival_time == "22:00-06:00")]
  
  if (model_period_ == "Pre + Post") {
    
    dataset[, post_Covid_1 := as.numeric(first_ED_admission > covid_start)]
    
    dataset[, post_Covid_1 := (post_Covid_1 - scale_params_[name == "post_Covid_1", mean])/
              scale_params_[name == "post_Covid_1", sd]]
    
  } else if (model_period_ == "Post-SDEC") {
    
    dataset[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS - num_SDEC - `num_SDEC Waiting`]

  }
  
  
  # scale the variables for Cox regresion
  # note that it should not matter if all of these are not used in the final cox predictions
  # because scale params were saved for them anyway
  
  dataset[, num_MAJORS_RESUS_scaled := (num_MAJORS_RESUS - scale_params_[name == "num_MAJORS_RESUS", mean])/
            scale_params_[name == "num_MAJORS_RESUS", sd]]
  dataset[, num_elsewhere_scaled := (num_elsewhere - scale_params_[name == "num_elsewhere", mean])/
            scale_params_[name == "num_elsewhere", sd]]
  dataset[, num_SDEC_scaled := (num_SDEC - scale_params_[name == "num_SDEC", mean])/
            scale_params_[name == "num_SDEC", sd]]
  
  dataset[, quarter_2 := (quarter_2 - scale_params_[name == "quarter_2", mean])/
            scale_params_[name == "quarter_2", sd]]
  dataset[, quarter_3 := (quarter_3 - scale_params_[name == "quarter_3", mean])/
            scale_params_[name == "quarter_3", sd]]
  dataset[, quarter_4 := (quarter_4 - scale_params_[name == "quarter_4", mean])/
            scale_params_[name == "quarter_4", sd]]
  
  dataset[, weekend_1 := (weekend_1 - scale_params_[name == "weekend_1", mean])/
            scale_params_[name == "weekend_1", sd]]
  
  dataset[, arrival_time_06_12 := (arrival_time_06_12 - scale_params_[name == "arrival_time_06_12", mean])/
            scale_params_[name == "arrival_time_06_12", sd]]
  dataset[, arrival_time_12_16 := (arrival_time_12_16 - scale_params_[name == "arrival_time_12_16", mean])/
            scale_params_[name == "arrival_time_12_16", sd]]
  dataset[, arrival_time_16_22 := (arrival_time_16_22 - scale_params_[name == "arrival_time_16_22", mean])/
            scale_params_[name == "arrival_time_16_22", sd]]

  
  
  # get the modelled distribution
  model_prob = cox_surv[time < 48 & model_period == model_period_, .(time, surv)]
  model_prob[, model := "cox model"]
  
  # get the empirical distribution
  empirical = numeric()
  for (i in (1:nrow(model_prob))) {
    empirical <- c(empirical, sum(dataset[, duration > model_prob$time[i]])/nrow(dataset))
  }
  
  empirical_curv = data.table(    time = model_prob$time, 
                                  surv = empirical)
  empirical_curv[, model := "empirical data"]
  
  # get 100 random patients
  
  coefs = cox_results[model_period == model_period_]
  setnames(coefs, "coefficents", "coef")
  
  if (model_period_ == c("Post")) {
    
    dataset[, pow := (coefs[variables == "quarter_2", coef]*quarter_2) + 
              (coefs[variables == "quarter_3", coef]*quarter_3) + 
              (coefs[variables == "quarter_4", coef]*quarter_4) + 
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
    dataset[, pow := (coefs[variables == "quarter_2", coef]*quarter_2) + 
              (coefs[variables == "quarter_3", coef]*quarter_3) + 
              (coefs[variables == "quarter_4", coef]*quarter_4) + 
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
              (coefs[variables == "quarter_3", coef]*quarter_3) + 
              (coefs[variables == "quarter_4", coef]*quarter_4) +
              (coefs[variables == "weekend_1", coef]*weekend_1) + 
              (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
              (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
              (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
              (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
              ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled)]
    
  } else if (model_period_ == "Post-SDEC") {

    
    dataset[, pow := (coefs[variables == "quarter_2", coef]*quarter_2) + 
              # (coefs[variables == "quarter_3", coef]*quarter_3) + 
              (coefs[variables == "quarter_4", coef]*quarter_4) +
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
  for (i in floor(runif(100, min=1, max=nrow(dataset)))) {

    random_data = bind_rows(random_data, data.table(time = model_prob$time,
                                                    surv = model_prob$surv ^ dataset$epow[i],
                                                    model = as.character(i)))
  }
  
  average_data = random_data[, mean(surv), by = "time"]
  setnames(average_data, "V1", "surv")
  average_data$model = "100 random patients"
  
  plot_data = bind_rows(empirical_curv, model_prob, average_data)
  plot_ = plot_data %>% ggplot(aes (x =time, y = surv, col = model)) + geom_line() + theme(legend.position = "bottom") +
    labs(title = model_period_)
  
  
  name_ = paste0(gsub(" ", "", model_period_), "plot")
  assign(name_, plot_)

}

library(gridExtra)
grid.arrange(Preplot, `Pre+Postplot`, Postplot, `Post-SDECplot`)



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

# load file with number in ED
load(paste0("EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",num_in_ED_at_arrival_time_file_date,".rda"))
num_in_ED = data.table(num_in_ED)

# load Cox regression results (survival probs and regression coefs)
load(paste0("EDcrowding/real-time/model-input/cox_surv_",cox_surv_date,".rda"))
load(paste0("EDcrowding/real-time/model-input/cox_results_",cox_surv_date, ".rda"))
load(paste0("EDcrowding/real-time/model-input/scale_params_",cox_surv_date, ".rda"))

load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))


distr_coll = data.table()
adm_coll = data.table()


for (model_period_ in c("Pre", "Post", 
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
  
  
  
  in_ED_all = get_cox_coef_by_csn(in_ED_all = adm_preds[[1]], coefs, FALSE, num_in_ED, model_period_, scale_params_, FALSE)
  in_ED_all[, tta := as.numeric(difftime(left_ED, first_ED_admission, units = "hours"))]
  
  in_ED_all[, time_so_far := as.numeric(difftime(time_pt, first_ED_admission, units = "hours"))]

  # get predicted survival curve for each patient
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
        
        base_prob_csn = base_prob
        base_prob_csn$surv_ = base_prob_csn$surv ^ in_ED_all[csn == df$csn[h] & time_pt == time_pts[i], epow]
        
        col_num_for_still_in_now = sum(base_prob_csn$time<= df$time_so_far[h])
        
        # it is possible for the time so far to be shorter than any of the survival times save in the cox curve
        if (col_num_for_still_in_now == 0) {
          col_num_for_still_in_now = 1
        }
        
        prob_still_in_now = base_prob_csn$surv_[col_num_for_still_in_now]
        base_prob_csn$surv_rebased = base_prob_csn$surv_/prob_still_in_now
        # base_prob_csn$time = base_prob_csn$time - base_prob$time[col_num_for_still_in_now]
        # 
        # base_prob_csn = base_prob_csn[time >= 0]
        
        # get patients cumulative probability of even
        base_prob_csn$cdf = 1 - base_prob_csn$surv_rebased
        base_prob_csn$time_ = base_prob_csn$time - df$time_so_far[h]
        base_prob_csn = base_prob_csn[time_ >=0]
        
        distr = bind_rows(distr, data.table(csn = df$csn[h], 
                                            time = base_prob_csn[surv_rebased <= 1, time_],
                                            cdf = base_prob_csn[surv_rebased <= 1, cdf]))

        
        adm_ =  bind_rows(adm_, data.table(csn = df$csn[h], 
                                           tta_remaining = df$tta[h] - df$time_so_far[h],
                                           tta_cdf = base_prob_csn[time_ > df$tta[h] -  df$time_so_far[h], min(cdf)]))

      }
      
      
      
      distr$time_of_report = time_pts[i]
      distr$model_period = model_period_
      
      adm_$time_of_report = time_pts[i]
      adm_$model_period = model_period_  

      distr_coll = bind_rows(distr_coll, distr)
      adm_coll = bind_rows(adm_coll, adm_)
      
      
      
    }
  }
}

qq_plot_data = data.table()


for (model_period_ in c("Pre", "Post", 
                        # "Pre + Post", 
                        "Post-SDEC")) { 
  
# # this was not necessary - trying to replicate the discrete distribution plots but 
# # not needed because the survival curve is continuous 
#   distr = distr_coll[model_period == model_period_]
# 
#   mid_E_prob = distr[, .N, by = cdf] # find out how many times each cdf value occured
#   mid_E_prob[, value := cdf]
#   setorderv(mid_E_prob, "value")
#   mid_E_prob[, cum_weight  := N/nrow(distr)] # use distr to get the number of cdf values recorded
#   
# 
#   mid_E_prob[, cum_weight_normed  := cumsum(cum_weight)]
# 

  qq_data = data.table()
  # num_csns = length(unique(distr_coll[model_period == model_period_, csn]))
  cdf_increments = seq(0,1, .01)
  
  for (c in cdf_increments) {
    
    
    
    qq_data  = bind_rows(qq_data , data.table(model_cdf = c,
                                              observed_cdf = nrow(adm_coll[tta_cdf <= c & model_period == model_period_])/nrow(adm_coll[model_period == model_period_]),
                                              model_period = model_period_))
    
    qq_plot_data = bind_rows(qq_plot_data, qq_data )
  }
  
  # this was not necessary - trying to replicate the discrete distribution plots but 
  # not needed because the survival curve is continuous 
  # for (c in cdf_increments) {
  #   
  #   
  #   
  #   qq_data  = bind_rows(qq_data , data.table(
  #                       model_cdf = mid_E_prob[value <= c, max(cum_weight_normed)],
  #                        observed_cdf = nrow(adm_coll[tta_cdf <= c & model_period == model_period_])/
  #                          nrow(adm_coll[model_period == model_period_]),
  #                        model_period = model_period_))
  # 
  #   qq_plot_data = bind_rows(qq_plot_data, qq_data )
  # }
}


qq_plot_data[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                         labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]



# plot chart


qq_plot_data[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Post_6wk", "Pre + Post", "Post-SDEC", "Post-SDEC_6wk"),
                                           labels = c("Pre-Covid", "Covid", "Covid with SWSC", "Pre + Post Covid", "Post-SDEC", "Post-SDEC with SWSC"))]


qq_cox = qq_plot_data %>% 
  
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

qq_cox
plot_chart(qq_cox, "qq_cox", width__ = 300, height__ = 200, layout = NA) 
plot_chart(qq_cox, "qq_cox_wide", width__ = 400, height__ = 200, layout = NA) 
