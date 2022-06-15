# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)


# library(Metrics)
library(cowplot)
library(gridExtra)

model_features = "alop"
model_period = c("Pre", "Post", "Pre + Post", "Post-SDEC")
tsk_ids = "test"


# Create functions --------------------------------------------------------



# Load functions ----------------------------------------------------------


# add config file that will load any parameters needed
source("~/EDcrowding/predict-admission/code/config.R")


# add utils file which will set training, val and test set date ranges
source("~/EDcrowding/predict-admission/code/utils.R")

#
# add utils file which is shared by real-time app
# using this here will ensure that the real-time app and the ML models use the same locations
source("~/EDcrowding/real-time/app/utils.R")

# set date of file to include

# load summ to look up first_ED_admission
# needed for validation set ranges

# if (summ_file_date_for_checking_real_time > summ_file_date) {
#   
#   load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
#   
# } else {

load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))

# }

# load moves to get location
load(paste0("~/EDcrowding/flow-mapping/data-raw/moves_", summ_file_date,".rda"))

# load file with num pats in each location
load(paste0("~/EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",num_in_ED_at_arrival_time_file_date,".rda"))


# Load Poisson and Cox data -----------------------------------------------

setwd("//uclcmddprafss21/Home/zelking1/Documents/EDcrowding/real-time/code")


# load file with max number of not-yet-arrived for current report time
poisson_file = "../model-input/poisson_not_yet_arrived.rda"
load(poisson_file)

# load results of poisson equations
poisson_results_file = paste0("../model-input/poisson_results_", poisson_results_date, ".rda")
load(poisson_results_file)




# load cox survival curve files
cox_surv_file =  paste0("../model-input/cox_surv_",cox_surv_date,".rda")
load(cox_surv_file)
cox_results_file =  paste0("../model-input/cox_results_",cox_surv_date,".rda")
load(cox_results_file)
cox_params_file = paste0("../model-input/scale_params_",cox_surv_date, ".rda")
load(cox_params_file)





pe_all = data.table()


for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in model_period) {
      
      # currently no test set for post SDEC
      # if (!(tsk_ids_ == "test" & model_period_ == "Post-SDEC")) {
      
      name_ = paste0("model_eval_date_", model_period_, "_", tsk_ids_)
      model_eval_file_date = get(name_)
      
      
      
      model_eval_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features_, "_", model_period_, "_", 
                               tsk_ids_, "_", model_eval_file_date,".rda")
      load(model_eval_file)
      
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
                                     labels = c("Pre-Covid", "Post-Covid", "Pre + Post Covid", "Post-SDEC"))]


prob_dist_all = data.table()

for (model_features_ in model_features) {
  for (tsk_ids_ in tsk_ids) {
    
    for (model_period_ in model_period) {
      
      # currently there is a test set for post SDEC but only has certain distributions 
      # if (!(tsk_ids_ == "test" & model_period_ == "Post-SDEC")) {
      
      name_ = paste0("model_eval_date_", model_period_, "_", tsk_ids_)
      model_eval_file_date = get(name_)
      
      model_eval_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features_, "_", model_period_, "_", 
                               tsk_ids_, "_", model_eval_file_date,".rda")
      load(model_eval_file)
      
      prob_dist_ = prob_dist[[1]]
      
      prob_dist_[, model_period := model_period_]
      prob_dist_[, model_features := model_features_]
      prob_dist_[, tsk_ids := tsk_ids_]
      
      prob_dist_all = bind_rows(prob_dist_all, prob_dist_)
      # }
      
      
    }
  }
  
}





# Choose time point of interest --------------------------------------------

tpoi = as.POSIXct("2021-05-11 16:00:00 UTC")
tpoi_model_period = "Post"



timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")





# plot patients in ED (steps 1 and 2) ----------------------------------------

summ_ = summ[first_ED_admission < tpoi & left_ED >= tpoi & age >= 18]
summ_[, elapsed := as.numeric(difftime(tpoi, first_ED_admission, units = "mins"))]



# get predictions 
# NOTE - this 
preds_file = paste0("~/EDcrowding/predict-admission/data-output/xgb_",
                    model_features, "_",tpoi_model_period,"_preds_", preds_file_date, ".rda")

load(preds_file)
# get model output file date - this is the date the ML models were traindd
name_ = paste0("model_output_date_", tpoi_model_period)
model_output_file_date = get(name_)

adm_preds = make_predictions(tpoi, summ, dm_file_date, model_output_file_date,  model_features,  tpoi_model_period)

preds_all_ts = adm_preds[[2]]
preds_all_ts[, timeslice := as.numeric(gsub("task", "", timeslice))]

preds_ = data.table()
for (i in 1:nrow(summ_)) {
  preds__ = preds_all_ts[csn == summ_$csn[i] & timeslice <= summ_$elapsed[i], .SD[which.max(timeslice)]]
  preds_ = bind_rows(preds_, preds__[, .(csn, prob.1, timeslice)])
}

preds_$ow = paste0("0-",preds_$timeslice)
preds_$ow = factor(preds_$ow, levels = unique(preds_$ow[order(preds_$timeslice)]))
# using time since admission



# Chart settings ----------------------------------------------------------
# 
# theme_get()
# 
# bw <- theme_set(theme_bw())
# theme_set(bw)
# 
# classic <- theme_set(theme_classic())
# theme_set(classic)
# 
# theme_update(axis.text.x = element_text(size = 12),
#              axis.text.y = element_text(size = 12),
#              axis.title.x = element_text(size = 12),
#              axis.title.y = element_text(size = 12), 
#              panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Step 1 ------------------------------------------------------------------


cut_hr = 8
pats_in_ED =
  merge(summ_, preds_)[elapsed < cut_hr*60]%>% ggplot(aes(x = elapsed, y = fct_rev(ow))) +
  # , col = prob.1) +
  geom_point(shape = 21, colour = "black", size = 5, stroke = 1.5) +
  geom_vline(xintercept = as.numeric(timeslices[timeslices < cut_hr*60]), linetype = "dotted", size = 1.2) +
  theme_bw() +
  scale_x_continuous(breaks = as.numeric(timeslices[timeslices < cut_hr*60])) +
  labs(
    # title = paste("Step 1: Get patients in ED at time point"),
    # subtitle = paste("Total number of patients = ", nrow(summ_)),
    x = "Minutes since arrival",
    y = "Observation window")  +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        # axis.title.x = element_text(size = 12),
        # axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        # legend.title = element_text(size = 8), 
        # legend.text = element_text(size = 8),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank())


pats_in_ED
pats_in_ED_with_preds = 
  merge(summ_, preds_)[elapsed < cut_hr*60]%>% ggplot(aes(x = elapsed, y = fct_rev(ow),  fill = prob.1, color = prob.1)) +
  geom_point(shape = 21, size = 5, stroke = 1.5) +
  geom_vline(xintercept = as.numeric(timeslices[timeslices < cut_hr*60]), linetype = "dotted", size = 1.2) + 
  scale_fill_distiller(palette = "Spectral") + 
  scale_color_distiller(palette = "Spectral") + 
  scale_x_continuous(breaks = as.numeric(timeslices[timeslices < cut_hr*60])) +
  theme_bw() +
  labs(
    # title = paste("Step 2: Get each patient's probability of admission"),
    # subtitle = paste("Total number of patients = ", nrow(summ_)),
    x = "Minutes since arrival",
    y = "Observation window",
    fill = NULL) +
  theme(legend.position = "bottom")  +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        # axis.title.x = element_text(size = 12),
        # axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 13),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +guides(colour = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank())

pats_in_ED_with_preds
# deep red #D53E4F
fill_colours = data.table(ggplot_build(pats_in_ED_with_preds)$data[[1]])
min_col =fill_colours[x == min(fill_colours$x), fill] # "#4891B9"


ggplot_build(pats_in_ED_with_preds)$data

# +
#   scale_x_reverse()




# Load prob distribution data ---------------------------------------------------------------


prob_dist_ = prob_dist_all[time_of_report == tpoi]
plot_dist1 = prob_dist_[is.na(time_window) & num_adm_pred <= nrow(summ_)-30]  %>% ggplot(aes(x = num_adm_pred, y = probs)) + 
  geom_bar(stat = "identity", fill = "#4891B9") +
  # geom_vline(xintercept = pe_all[time_of_report == tpoi & is.na(time_window), truth],
  #            colour = "red", linetype = "dashed", size = 2) + theme_bw() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(
    # title = paste0("Step 3: Generate a probability distribution for total number of beds\n needed at some point by these patients"),
    # subtitle = paste0("Red line shows actual number admitted (", pe_all[time_of_report == tpoi & is.na(time_window), truth],")"),
    x = "Number of admissions at any time\nof patients currently in ED",
    y = "Probability")  +
  theme(legend.position = "bottom")  +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        # axis.title.x = element_text(size = 12),
        # axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank())
plot_dist1 

for (time_window_ in c( 4, 8)) {
  
  plot = prob_dist_[time_window == time_window_ & !inc_nya & num_adm_pred <= nrow(summ_)-40]  %>%
    ggplot(aes(x = num_adm_pred, y = probs)) + geom_bar(stat = "identity", fill = "#F4B183") +
    # geom_vline(xintercept = pe_all[time_of_report == tpoi & time_window == time_window_ & !inc_nya, truth],
    #            colour = "red", linetype = "dashed", size = 2) + theme_bw() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(
      # title = paste0("Step 5: Generate a probability distribution for total number of beds\n needed by these patients within ", time_window_," hours"),
      # subtitle = paste0("Red line shows actual number admitted (", pe_all[time_of_report == tpoi & time_window == time_window_ & !inc_nya, truth],") in ",time_window_," hours"),
      x = "Number of admissions in prediction window\nof patients currently in ED",
      y = "Probability") +
    theme(plot.title = element_text(size = 10, hjust = 0.5)) +
    theme(legend.position = "bottom")  +
    theme(axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          # axis.title.x = element_text(size = 12),
          # axis.title.y = element_text(size = 12),
          # plot.subtitle = element_text(size = 8),
          # plot.title = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size=12))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank()) 
  
  name_ = paste0("plot_dist2_tw_",time_window_)
  assign(name_, plot)
  
  
  plot =  prob_dist_[time_window == time_window_ & inc_nya & num_adm_pred <= nrow(summ_) & num_adm_pred <= nrow(summ_)-40]  %>% 
    ggplot(aes(x = num_adm_pred, y = probs)) + geom_bar(stat = "identity", fill = "#D53E4F") +
    # geom_vline(xintercept = pe_all[time_of_report == tpoi & time_window == time_window_ & inc_nya, truth],
    #            colour = "red", linetype = "dashed", size = 2) + theme_bw() +
    theme_bw() +
    theme(axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          # axis.title.x = element_text(size = 12),
          # axis.title.y = element_text(size = 12),
          # plot.subtitle = element_text(size = 8),
          # plot.title = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size=12))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank()) 
  
  name_ = paste0("plot_dist3_tw_",time_window_)
  assign(name_, plot)
  
}

# plot_dist2_tw_4
# plot_dist3_tw_4




# Plot survival curve ----------------------------------------------------------


base_prob = cox_surv[model_period == tpoi_model_period]
coefs = cox_results[model_period == tpoi_model_period]
scale_params_ = scale_params[model_period == tpoi_model_period]

setnames(coefs, "coefficents", "coef")



in_ED_data = get_cox_coef_by_csn(in_ED_all = summ_, 
                                 coefs = coefs, 
                                 real_time = FALSE, 
                                 num_in_ED , 
                                 tpoi_model_period, scale_params_)

# patient with max and min epow
min_pat = in_ED_data[epow == min(epow), csn]
max_pat = in_ED_data[epow == max(epow), csn]

base_prob$min_pat = base_prob$surv ^ in_ED_data[csn == min_pat, epow]
base_prob$max_pat = base_prob$surv ^ in_ED_data[csn == max_pat, epow]

base_prob[time == 4, max_pat]


since_arrival = 4
time_window_ = 4
col_num_for_still_in_now = sum(base_prob$time<= since_arrival)
prob_still_in_now = base_prob$surv[col_num_for_still_in_now]
base_prob$rebased_prob = base_prob$surv / prob_still_in_now

plot_surv_4 = base_prob[time < 25] %>% ggplot(aes(x = time)) + 
  geom_rect(xmin = since_arrival, ymin = 0, xmax = since_arrival + time_window_, ymax = 1, fill = "#F4B183", alpha = 0.7) +
  # geom_line(aes( y = 1 - rebased_prob), size = 1, colour = "#7F7F7F" , linetype = "dashed") +
  geom_line(aes( y = 1 - surv), size = 1, colour = "#7F7F7F" ) +
  # geom_line(aes( y = 1 - min_pat), size = 1.5, colour = "#D53E4F") +
  geom_hline(yintercept = base_prob[time == since_arrival, 1-surv], size = 1, colour = "#7F7F7F" ) +
  geom_hline(yintercept = base_prob[time == since_arrival + time_window_, 1-surv],  size = 1,  colour = "#7F7F7F") +
  # geom_hline(yintercept = base_prob[time == since_arrival + time_window_, 1-surv/prob_still_in_now], size = 1, colour = "#7F7F7F", linetype = "dashed" ) +
  
  # geom_hline(yintercept = base_prob[time == since_arrival + time_window_, 1-min_pat], linetype = "dotted", size = 1, colour = "#D53E4F") +
  scale_x_continuous(breaks = seq(0,24,2)) +
  scale_y_continuous(breaks = c(0, round(base_prob[time == since_arrival, 1-surv], 2), 
                                # round( base_prob[time == since_arrival + time_window_, 1-surv/prob_still_in_now], 2),
                                round(base_prob[time == since_arrival+ time_window_, 1-surv], 2), 
                                1), 
                     limits = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", 
        legend.spacing = unit(0.04, "cm")) +
  labs(
    # title = paste0("Step 4: Using a survival curve which takes account of the busyness of the ED \n at the time each patient arrived, get their probability of being admitted within ",time_window_," hours"),
    # subtitle = "Calculated only for admitted patients",
    y = "Probability",
    x = "Time since arrival at ED (hours)",
    colour = NULL) +
  # scale_colour_manual(values = c("#7F7F7F" )) + #, "#F4B183", "#D53E4F")) +
  theme(legend.position = "bottom")  +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        # axis.title.x = element_text(size = 12),
        # axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        axis.title.y=element_blank()
        # ,
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank()
  ) +
  guides(colour = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank())

plot_surv_4

# plotting the other way up
since_arrival = 3
col_num_for_still_in_now = sum(base_prob$time<= since_arrival)
prob_still_in_now = base_prob$surv[col_num_for_still_in_now]
base_prob$rebased_prob = base_prob$surv / prob_still_in_now

plot_surv_4_alt = base_prob[time < 25] %>% ggplot(aes(x = time)) + 
  geom_rect(xmin = since_arrival, ymin = 0, xmax = since_arrival + time_window_, ymax = 1, fill = "#F4B183", alpha = 0.7) +
  geom_line(aes( y = surv), size = 1, colour = "#7F7F7F" ) +
  geom_line(aes( y = rebased_prob), size = 1, colour = "red", linetype = "dashed") +
  
  # geom_line(aes( y = 1 - min_pat), size = 1.5, colour = "#D53E4F") +
  geom_hline(yintercept = base_prob[time == since_arrival, surv], linetype = "dotted", size = 1, "#7F7F7F" ) +
  geom_hline(yintercept = base_prob[time == since_arrival + time_window_, rebased_prob], linetype = "dashed", size = 1, "#7F7F7F" , colour = "red" ) +
  
  geom_hline(yintercept = base_prob[time == since_arrival + time_window_, surv], linetype = "dotted", size = 1, "#7F7F7F" ) +
  # geom_hline(yintercept = base_prob[time == since_arrival + time_window_, 1-min_pat], linetype = "dotted", size = 1, colour = "#D53E4F") +
  scale_x_continuous(breaks = seq(0,24,2)) +
  scale_y_continuous(breaks = c(0, round(base_prob[time == since_arrival, surv], 2), 
                                # round( base_prob[time == since_arrival + time_window_, 1-min_pat], 2),
                                round(base_prob[time == since_arrival+ time_window_, rebased_prob], 2), 
                                round(base_prob[time == since_arrival+ time_window_, surv], 2), 
                                1), 
                     limits = c(0,1)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right", 
        legend.spacing = unit(0.04, "cm")) +
  labs(
    # title = paste0("Step 4: Using a survival curve which takes account of the busyness of the ED \n at the time each patient arrived, get their probability of being admitted within ",time_window_," hours"),
    # subtitle = "Calculated only for admitted patients",
    y = "Probability",
    x = "Time since arrival at ED (hours)",
    colour = NULL) +
  # scale_colour_manual(values = c("#7F7F7F" )) + #, "#F4B183", "#D53E4F")) +
  theme(legend.position = "bottom")  +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        # axis.title.x = element_text(size = 12),
        # axis.title.y = element_text(size = 12),
        # plot.subtitle = element_text(size = 8),
        # plot.title = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        strip.text = element_text(size=12))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        axis.title.y=element_blank()
        # ,
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank()
  ) +
  guides(colour = "none") +
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_blank())

plot_surv_4_alt
# Plot Poisson -----------------------------------------------------------------


poisson_results_ = poisson_results[model_period == tpoi_model_period]
poisson_nya = poisson_not_yet_arrived_all[model_period == tpoi_model_period]

time_window_array_ = c(4, 8)
poisson_means = unique(get_poisson_means(tpoi, poisson_results_, poisson_nya, tpoi_model_period, 
                                         time_window_array_ ))


for (time_window_ in c( 4, 8)) {
  
  num_adm_ = seq(0, poisson_means[time_window == time_window_, max_nya + 5], 1)
  
  probs_not_yet_arrived = dpois(num_adm_,
                                lambda = poisson_means[time_window == time_window_, poisson_mean])
  
  spline_int <- as.data.table(spline(num_adm_, probs_not_yet_arrived))
  
  
  plot = data.table( num_adm_ = num_adm_ ,  probs_not_yet_arrived = probs_not_yet_arrived) %>% 
    ggplot(aes(x = num_adm_, y = probs_not_yet_arrived)) + 
    geom_bar(stat = "identity", fill = "#7F7F7F") +
    geom_line(data = spline_int, aes(x = x, y = y), size = 1.5) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    # geom_vline(xintercept = pe_all[time_of_report == tpoi & time_window == time_window_ & !inc_nya, truth],
    #            colour = "red", linetype = "dashed", size = 2) + theme_bw() +
    labs(
      # title = paste0(
      # "Patients in ED at ",
      #                   "time point", # substring(tpoi, 1, 16),
      #                   "Step 6: Generate a Poisson distribution for the number of patients\n", 
      #                   "who have not yet arrived and will be admitted within ", time_window_, " hours"),
      # subtitle = paste0("Red line shows actual number admitted (", pe_all[time_of_report == tpoi & time_window == time_window_ & !inc_nya, truth],") in ",time_window_," hours"),
      x = "Number of admissions of patients\nwho have not yet arrived",
      y = "Probability") +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom")  +
    theme(axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          # axis.title.x = element_text(size = 12),
          # axis.title.y = element_text(size = 12),
          # plot.subtitle = element_text(size = 8),
          # plot.title = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          strip.text = element_text(size=12))  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.title.x=element_blank(),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          axis.title.y=element_blank()
          # ,
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank()
    ) +
    guides(colour = "none") +
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank())
  
  name_ = paste0("plot_poisson_nya_tw_",time_window_)
  assign(name_, plot)
  
}
plot_poisson_nya_tw_4

# Combine into single plot ------------------------------------------------

#  
# 
# top_ = plot_grid(pats_in_ED + theme(plot.margin = unit(c(1,1,4,1), "lines")) , 
#                  pats_in_ED_with_preds + theme(plot.margin = unit(c(1,1,0,1), "lines")), 
#                  nrow = 1,
#                  labels = c("a", "b")
#                  # labels = c("a Step 1: Get patients", "b", "c")
#                  )
# middle_ = plot_grid(
#   plot_dist1 + theme(plot.margin = unit(c(1,1,1,1), "lines")) , 
#   
#   plot_surv_4 + theme(plot.margin = unit(c(1,4,0,1), "lines")) , 
#                     labels = c("c", "d"), 
#   rel_widths = c(1/3, 2/3)
#                     )
# bottom_ = plot_grid(plot_dist2_tw_4+ theme(plot.margin = unit(c(1,1,1,1), "lines")), 
#                     plot_poisson_nya_tw_4+ theme(plot.margin = unit(c(1,1,1,1), "lines")), 
#                     plot_dist3_tw_4+ theme(plot.margin = unit(c(1,1,.5,1), "lines")), 
#                     labels = c("e", "f", "g"),
#                     
#                     nrow = 1)
# 
# 
# 
# # without title
# plot_one_time_point = plot_grid(top_, middle_, bottom_, nrow = 3 #,rel_heights = c(0.3, 0.4, 0.3)
#           )
# 
# 
# plot_chart(plot_one_time_point, "one_time_point", width__ = 300, height__ = 300, layout = NA) 
# plot_chart(plot_one_time_point, "one_time_point_wide", width__ = 400, height__ = 200, layout = NA) 
# 
# # with title
# # plot_grid(title, top_, middle_, bottom_, nrow = 4, rel_heights = c(0.1, 0.3, 0.3, 0.3))


# Save individual plots ---------------------------------------------------

setwd("//uclcmddprafss21/Home/zelking1/Documents")
plot_chart(pats_in_ED, paste("Figure 1a", nrow(summ_), "pats"), width__ = 150, height__ = 100, layout = NA) 
plot_chart(pats_in_ED_with_preds, "Figure 1b", width__ = 150, height__ = 120, layout = NA) 
plot_chart(plot_dist1, "Figure 1c", width__ = 150, height__ = 100, layout = NA) 
plot_chart(plot_surv_4, "Figure 1d", width__ = 150, height__ = 100, layout = NA) 

plot_chart(plot_dist2_tw_4, "Figure 1e", width__ = 150, height__ = 100, layout = NA) 
plot_chart(plot_poisson_nya_tw_4, "Figure 1f", width__ = 150, height__ = 100, layout = NA) 
plot_chart(plot_dist3_tw_4, "Figure 1g", width__ = 150, height__ = 100, layout = NA) 