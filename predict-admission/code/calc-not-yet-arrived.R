
# About this script ------------------------------------------------------

# This using historical data to generate means for a poisson distribution of 
# patients who have not yet arrived

# Three 

# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)



# Load data ---------------------------------------------------------------


# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")

# add real-time app utils to get one hot encoding 
source("EDcrowding/real-time/app/utils.R")



# Load data ---------------------------------------------------------------

load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))

# load earlier versions of filef
not_in_ED_yet_file = "EDcrowding/flow-mapping/data-raw/not_yet_in_ED_yet.rda"

load(not_in_ED_yet_file)


# save copy of last version of file
poisson_file = "EDcrowding/real-time/model-input/poisson_not_yet_arrived.rda"

if (file.exists(poisson_file)) {
  load(poisson_file)
  save(poisson_not_yet_arrived, file = paste0("EDcrowding/real-time/model-input/poisson_means_archived_on_",Sys.Date(),".rda"))
}



# Generate time points  ------------------------------
# time points are geared to the reporting times used by the hospital bed planners
# get each time point throughout the study period

start_of_set = start_study
end_of_set = end_study
next_dt = start_of_set

time_pts = POSIXct()
# each sample to be more than 12 hours and less than 24 hours after the previous one
while (next_dt < end_of_set) {
  next_pt <- next_dt + c(hours(6), hours(12), hours(16), hours(22))
  time_pts <- c(time_pts, next_pt)
  next_dt = next_dt + days(1)
}


# # Summarise number not in who get admitted later --------------------------
# # This is done for each time point, for any time window after the report time 
# # Time window could be anything from 1 to 24 hours after the reporting window
# 
# not_in_ED_yet_all = data.table()
# 
# for (i in 1:length(time_pts)) {
#   
#   if (i %% 100 == 0) {
#     print(paste("Processed ", i, " dates"))
#   }
#   
#   for (time_window in seq(1, 24, 1)) {
#     # for (time_window in c(2,3,4,6,8,12)) {
#       
#     not_in_ED_yet = data.table(time_window = time_window, 
#                                N = summ[time_pt >time_pts[i] & time_pt < time_pts[i] + hours(time_window)
#                          & first_outside_proper_admission < time_pts[i] + hours(time_window), .N])
#     
#     not_in_ED_yet$time_pt <- time_pts[i]
#     
#     not_in_ED_yet_all = bind_rows(not_in_ED_yet_all, not_in_ED_yet)
#     
#   }
# }
# 
# 
# not_in_ED_yet_all[, weekend:= if_else(weekdays(time_pt, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]
# not_in_ED_yet_all[, time_of_report:= paste0(hour(time_pt), ":", substr(time_pt, 15, 16))]
# 
# not_in_ED_yet_all = bind_rows(not_in_ED_yet_old, not_in_ED_yet_all)
# 
# # save file for later reference
# save(not_in_ED_yet_all, file = not_in_ED_yet_file)


# Make calcs for poisson means --------------------------------------------


poisson_not_yet_arrived_all = data.table()

for (model_period in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
  
  dataset = set_train_val_test_dates(not_in_ED_yet_all, NA, model_period)
  dataset = dataset[in_set == "Train"]
  
  # generate poissson means
  poisson_not_yet_arrived = dataset[, .(poisson_mean = mean(N), num_obs = .N), 
                                              by = .(weekend, time_of_report, time_window)] 

  # save the maximum number of not yet arrived
  max_nya = not_in_ED_yet_all[, max(N), by = .(weekend, time_of_report, time_window)]
  setnames(max_nya, "V1", "max_nya")
  
  poisson_not_yet_arrived = merge(poisson_not_yet_arrived, max_nya, 
        by = c( "weekend",  "time_of_report", "time_window"))
  
  poisson_not_yet_arrived[, model_period := model_period]
  
  poisson_not_yet_arrived_all = bind_rows(poisson_not_yet_arrived_all, poisson_not_yet_arrived)

}


# Save to file ------------------------------------------------------------




save(poisson_not_yet_arrived_all, file = "EDcrowding/real-time/model-input/poisson_not_yet_arrived_using_empirical_data.rda")


# Using Poisson equation --------------------------------------------------

not_in_ED_yet_all[, quarter := factor(case_when( month(time_pt) <= 3 ~ 1,
                                        month(time_pt) <= 6 ~ 2, 
                                        month(time_pt) <= 9 ~ 3, 
                                        month(time_pt) <= 12 ~ 4))]

not_in_ED_yet_all[, time_of_report := factor(case_when(hour(time_pt) > 22 | hour(time_pt) <= 6 ~ "6:00",
                                               hour(time_pt) > 6 & hour(time_pt) <= 12 ~ "12:00",
                                               hour(time_pt) > 12 & hour(time_pt) <= 16 ~ "16:00",
                                               hour(time_pt) > 16 & hour(time_pt) <= 22 ~ "22:00"))]

not_in_ED_yet_all[, quarter_1 := as.numeric(quarter ==1)]
not_in_ED_yet_all[, quarter_2 := as.numeric(quarter ==2)]
not_in_ED_yet_all[, quarter_3 := as.numeric(quarter ==3)]
not_in_ED_yet_all[, quarter_4 := as.numeric(quarter ==4)]

not_in_ED_yet_all[, weekend_1 := as.numeric(weekend ==1)]

not_in_ED_yet_all[, time_of_report_6 := as.numeric(time_of_report == "6:00")]
not_in_ED_yet_all[, time_of_report_12 := as.numeric(time_of_report == "12:00")]
not_in_ED_yet_all[, time_of_report_16 := as.numeric(time_of_report == "16:00")]
not_in_ED_yet_all[, time_of_report_22 := as.numeric(time_of_report == "22:00")]

# not_in_ED_yet_all[, mon := as.numeric(day_of_week =="Mon")]
# not_in_ED_yet_all[, tue := as.numeric(day_of_week =="Tue")]
# not_in_ED_yet_all[, wed := as.numeric(day_of_week =="Wed")]
# not_in_ED_yet_all[, thu := as.numeric(day_of_week =="Thu")]
# not_in_ED_yet_all[, fri := as.numeric(day_of_week =="Fri")]
# not_in_ED_yet_all[, sat := as.numeric(day_of_week =="Sat")]


poisson_results = data.table()
poisson_scores = data.table()
for (model_period_ in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
  
  dataset = set_train_val_test_dates(not_in_ED_yet_all, NA, model_period_)
  dataset = dataset[in_set == "Train"]
  
  for (time_window_ in c(4,8)) {
  # for (time_window_ in seq(1, 24, 1)) {
    
    if (model_period_ == "Pre") {
      poisson_q_w <- glm(N ~ #quarter_1 + 
                           quarter_2 + quarter_3 + #quarter_4 +  
                           # mon + tue + wed + thu + fri + sat +
                           weekend_1 + 
                           time_of_report_12 + time_of_report_16 + time_of_report_22 , family="poisson", data=dataset[in_set == "Train" & time_window == time_window_])
    
    
    } else if (model_period_ == "Post") {
      
      poisson_q_w <- glm(N ~ quarter_1 + quarter_2 + quarter_3 +# quarter_4 +  
                           # mon + tue + wed + thu + fri + sat +
                           weekend_1 + 
                           time_of_report_12 + time_of_report_16 + time_of_report_22 , family="poisson", data=dataset[in_set == "Train" & time_window == time_window_])
      
    } else if (model_period_ == "Pre + Post") {
      
      poisson_q_w <- glm(N ~ quarter_1 + quarter_2 + quarter_3 + #quarter_4 +  
                           # mon + tue + wed + thu + fri + sat +
                           weekend_1 + 
                           time_of_report_12 + time_of_report_16 + time_of_report_22 , family="poisson", data=dataset[in_set == "Train" & time_window == time_window_])
      
    } else if (model_period_ == "Post-SDEC") {
      
      poisson_q_w <- glm(N ~ quarter_1 + quarter_2 + #quarter_3 + quarter_4 +  
                           # mon + tue + wed + thu + fri + sat +
                           weekend_1 + 
                           time_of_report_12 + time_of_report_16 + time_of_report_22 , family="poisson", data=dataset[in_set == "Train" & time_window == time_window_])
    }
    
    
      
  
    
    results <- data.table(model_period = model_period_, 
                          time_window = time_window_,
                          variables = names(poisson_q_w$coefficients),
                          coefficents = poisson_q_w$coefficients,
                          st_error = summary(poisson_q_w)$coefficients[,2],
                          CI_lower = exp(confint(poisson_q_w))[,1], 
                          CI_upper = exp(confint(poisson_q_w))[,2],
                          N = nrow(dataset[in_set == "Train" & time_window == time_window_]))

    
    results$model_period = model_period_
    
    poisson_results = bind_rows(poisson_results, results)
    
    scores <- data.table(model_period = model_period_, 
                         time_window = time_window_,
                         null_df = poisson_q_w$df.null,
                         residual_df = poisson_q_w$df.deviance,
                         null_deviance = poisson_q_w$null.deviance,
                         residual_deviance = poisson_q_w$deviance, 
                         AIC = poisson_q_w$aic)
    poisson_scores = bind_rows(poisson_scores, scores)
  }
  
}

poisson_results[, exp_coef := exp(coefficents)]
poisson_results[, coefficents_formatted := paste0(format(round(exp_coef, 2), nsmall = 2), " [", 
                                                  format(round(CI_lower, 2), nsmall = 2), ", ", 
                                                  format(round(CI_upper, 2), nsmall = 2), "]")]

save(poisson_results, file = paste0("EDcrowding/real-time/model-input/poisson_results_", today(), ".rda"))

save(poisson_not_yet_arrived_all, file = "EDcrowding/real-time/model-input/poisson_not_yet_arrived_using_empirical_data.rda")


# also save to csv file for reporting pruposes
outFile = (paste0("EDcrowding/predict-admission/model-output/poisson_results_for_paper_",today(),".csv"))
write_csv(poisson_results[time_window %in% c(4,8)] 
          # %>% mutate(text_ = paste(time_window, "hours")) %>% select(-time_window) %>% 
          #   pivot_wider(names_from = text_, values_from = coefficents)
          , file = outFile)

# also save to csv file for reporting pruposes
outFile = (paste0("EDcrowding/predict-admission/model-output/poisson_scores_for_paper_",today(),".csv"))
write_csv(poisson_scores[time_window %in% c(4,8)] 
          # %>% mutate(text_ = paste(time_window, "hours")) %>% select(-time_window) # %>% 
          # pivot_wider(names_from = text_, values_from = null_df:AIC)
          , file = outFile)



# Test predictions --------------------------------------------------------


model_features = "alop"
model_features_ = "alop"
tsk_ids = "test"

# load file with data to retrieve true numbers of not yet arrived in test set
not_in_ED_yet_file = "EDcrowding/flow-mapping/data-raw/not_yet_in_ED_yet.rda"
load(not_in_ED_yet_file)


# load poisson means for not-yet-arrived to get max nya by time of day and time window
load("EDcrowding/real-time/model-input/poisson_not_yet_arrived.rda")

# load results of poisson equations
poisson_file = paste0("EDcrowding/real-time/model-input/poisson_results_", poisson_results_date, ".rda")
load(poisson_file)


distr_coll = data.table()
pt_estimates_coll = data.table()


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
  
  
  # get poisson results (trained on training data) for not-yet-arrived
  poisson_results_ = poisson_results[model_period == model_period_]
  poisson_nya = poisson_not_yet_arrived_all[model_period == model_period_]
  
  poisson_means = get_poisson_means(time_pts, poisson_results_, poisson_nya, model_period_)
  

  # get predictions for patients yet to arrive for each time point
  for (i in (1:length(time_pts))) {
    
    print(i)
    
    for (time_window_ in c(4,8)) {
      
      if (!(i == length(time_pts) |
            i == length(time_pts)-1 & time_window_ == 8)) { 
        
        # using empirically derived poisson mean, 
        # generate probs of each number of not-yet-arrived admissions
        # assuming that a maximum of 20 people can arrive after the time point of interest
        # and be admitted before the end of the time window after that
        probs_not_yet_arrived = 
          dpois(seq(0, 20 ,1),
                lambda = poisson_means[time_window == time_window_ &
                                                   time_pt == time_pts[i], 
                                                 poisson_mean])
        # save this distribution
        distr = data.table(bind_cols(time_of_report = time_pts[i],
                                     num_adm_pred = seq(0, 20 ,1),
                                     probs = probs_not_yet_arrived, cdf = cumsum(probs_not_yet_arrived),
                                     time_window = time_window_,
                                     model_period = model_period_))
        
        # ugly fix to align BST time_pts
        if (hour(time_pts[i]) %in% c(5, 11, 15, 21)) {
          truth_ = not_in_ED_yet_all[time_pt == time_pts[i] + hours(1) & time_window == time_window_, N]
          
        } else {
          truth_ = not_in_ED_yet_all[time_pt == time_pts[i]  & time_window == time_window_, N]
          
        }
        # derive point estimates from this distribution
        pt_estimates_coll_ = 
          data.table(bind_cols(time_of_report = time_pts[i],
                               # retrieve true number of admissions within time window of people
                               # who arrived after the time point of interest
                               truth = truth_,
                               # use the distribution to get an expected value and 10th and 90th quantile on cdf
                               expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                               time_window = time_window_,
                               model_period = model_period_))
        
        distr_coll = bind_rows(distr_coll, distr)
        pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
        
      }
    }
  }
}

pe_all = pt_estimates_coll
prob_dist_all = distr_coll
pe_all[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                     labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]
distr_coll[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                                     labels = c("Pre-Covid", "Covid", "Pre + Post Covid", "Post-SDEC"))]



# prepare qq plot
qq_plot_data = data.table()

for (model_features_ in model_features) {
  for (tsk_ids_ in "test") {
    
    for (model_period_ in c("Pre", "Post", "Post-SDEC")) {
      
      
      
      # time window, excluding not yet arrived
      
      for (time_window_ in c(4,8)) {
        
        
        # no time window - BUT do it twice anyway for chart formatting
        
        distr_coll = prob_dist_all[model_period == model_period_ & time_window == time_window_]
        num_time_pts = length(unique(distr_coll$time_of_report))
        num_adm = pe_all[model_period == model_period_ & time_window == time_window_,
                         .(time_of_report, num_adm = truth)]If
        
        qq_plot = plot_qq_chart(num_time_pts, distr_coll, num_adm, tsk_ids_,
                                subtitle = paste(model_period_, "COVID", tsk_ids_, "set; no time window"), 
                                return_data = TRUE, include_title = FALSE)
        
        qq_plot$tsk_ids = tsk_ids_
        qq_plot$model_period = model_period_
        qq_plot$model_features = model_features_
        qq_plot$time_window = paste(time_window_, "hour prediction window")
        qq_plot$type = "Distribution after Step 6:\nPatients yet to arrive"
        
        qq_plot_data = bind_rows(qq_plot_data, qq_plot)
        
      }
      
    }
  }
}


# plot chart

qq_plot_data[, time_window_ := factor(case_when(grepl("Step 3", type) ~ "No prediction window applied",
                                                TRUE ~ time_window),
                                      levels = c("No prediction window applied", "4 hour prediction window", "8 hour prediction window"))]


qq_plot_data[, model_period_text := factor(model_period, levels = c("Pre", "Post", "Post_6wk", "Pre + Post", "Post-SDEC", "Post-SDEC_6wk"),
                                           labels = c("Pre-Covid", "Covid", "Covid with SWSC", "Pre + Post Covid", "Post-SDEC", "Post-SDEC with SWSC"))]


qq_poisson = qq_plot_data %>% 
  
  ggplot(aes(y = observed_cdf)) + 
  geom_point(aes(x = model_cdf, colour = time_window_), size = 1.5) + 
  geom_line(aes(x = observed_cdf), colour = "grey90") +
  facet_grid(time_window ~ model_period_text, switch = "y") +

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

plot_chart(qq_poisson, "qq_poisson", width__ = 300, height__ = 200, layout = NA) 
plot_chart(qq_poisson, "qq_poisson_wide", width__ = 400, height__ = 200, layout = NA) 
