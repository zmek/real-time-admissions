
# About this script -------------------------------------------------------

# Use this for charts comparing the allocation of visits between training set and test set. 
# Shows that proportions of train, val and test don't change between timeslices
# in a material way



# Functions ---------------------------------------------------------------


rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)



# Load functions and set configuration ----------------------------------------------------------------



# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")



# Load data ---------------------------------------------------------------

if (summ_file_date_for_checking_real_time > summ_file_date) {
  
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
} else {
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
}



summ[, adm := if_else(adm %in% c("direct_dis", "indirect_dis"), "Discharged", "Admitted")]
     
# Get training, test and validation for each period -----------------------


# number admitted and discharged
summ[, .N, by = .(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = fct_rev(adm))) + 
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
               )+
  theme(legend.position = "bottom") +
  
  labs(title = "Number of admissions and discharges by day",
       x = "Date", 
       y = "Number of visits",
       fill = "Disposition") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


plot_data = data.table()

for (model_period in c("Pre", "Post", "Pre + Post", "Post-SDEC")) {
  
  dataset = set_train_val_test_dates(summ, NA, model_period)
  period_summ = dataset[, .N, by = .(date(presentation_time), in_set)] 
  period_summ$model_period = model_period
  
  plot_data = bind_rows(plot_data, period_summ)

}

plot_data[, model_period_ := factor(model_period, levels = c("Pre", "Post", "Pre + Post", "Post-SDEC"))]
plot_data[, in_set_ := factor(in_set, levels = c("Before", "Train", "Val", "Test", "After"))]

# for main study
plot_data  %>% ggplot(aes(x = date, y = N, fill = in_set_)) +

  geom_bar(stat = "identity") +
  labs(title = "Number of visits by day, with colour showing train-val-test split",
       x = "Date", 
       y = "Number of visits",
       fill = "Set") +
  geom_vline(xintercept = c(date(start_val_pre), date(start_test_pre), date(covid_start), date(start_val_post), 
                            date(start_val_post_SDEC), date(SDEC_start),
                            date(start_test_post), date(end_SDEC_study))) +
  facet_grid(model_period_ ~ .) + 
  theme(legend.position = "bottom") +
  scale_x_date(breaks = c(date(start_study), date(start_val_pre), date(start_test_pre), date(start_val_post_SDEC), date(SDEC_start),
                          date(covid_start), date(start_val_post), date(start_test_post), date(end_study), date(end_SDEC_study)))+
  scale_fill_manual(values = c("#717171" ,  "#00BA38", "#619CFF","#F8766D", "#919191", guide = NULL, name = NULL)) +
  theme(axis.text.x=element_text(angle=45, hjust=1))



# for dissertation
plot_data[model_period_ %in% c("Pre", "Post") & !(model_period == "Post" & date == "2020-03-18") &
            !(model_period == "Pre" & date == "2020-03-19")] %>% ggplot(aes(x = date, y = N, fill = in_set_)) +
  
  geom_bar(stat = "identity") +
  labs(title = "Number of visits by day, with colour showing train-val-test split",
       x = "Date", 
       y = "Number of visits",
       fill = "Set") +
  geom_vline(xintercept = c(date(start_val_pre), date(start_test_pre), date(covid_start), date(start_val_post), 
                            date(start_test_post))) +
  facet_grid(model_period_ ~ .) + theme(legend.position = "bottom") +
  scale_x_date(breaks = c(date(start_study), date(start_val_pre), date(start_test_pre), 
                               date(covid_start), date(start_val_post), date(start_test_post), date(end_study)))+
  scale_fill_manual(values = c("#717171" ,  "#00BA38", "#619CFF","#F8766D", "#919191", guide = NULL, name = NULL)) +
 theme(axis.text.x=element_text(angle=45, hjust=1))
  

# # number of arrivals
# p1 = summ[, .N, by = .(date(presentation_time), in_set)] %>% ggplot(aes(x = date, y = N, fill = in_set)) + 
#   geom_bar(stat = "identity") +
#   labs(title = "Number of visits by day, with colour showing Pre and Post train-val-test split",
#        x = "Date", 
#        y = "Number of visits",
#        fill = "Set") +
#   geom_vline(xintercept = c(date("2019-11-19"), date("2019-12-13"), date("2020-03-19"), date("2020-12-01"), date("2020-12-29"))) 
# 
# # number of admissions without lines
# p2_ = summ[, .N, by = .(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = fct_rev(adm))) + 
#   geom_bar(stat = "identity") +
#   # geom_vline(xintercept = c(date("2019-11-19"), date("2019-12-13"), date("2020-03-19"), date("2020-12-01"), date("2020-12-29"))) +
#   labs(title = "Number of admissions and discharges by day",
#        x = "Date", 
#        y = "Number of visits",
#        fill = "Disposition") 
# 
# 
# # number of admissions with lines
# p2 = summ[, .N, by = .(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = fct_rev(adm))) + 
#   geom_bar(stat = "identity") +
#   geom_vline(xintercept = c(date("2019-11-19"), date("2019-12-13"), date("2020-03-19"), date("2020-12-01"), date("2020-12-29"))) +
#   labs(title = "Number of admissions and discharges by day",
#        x = "Date", 
#        y = "Number of visits",
#        fill = "Disposition") 
# 
# 
# p3 = pre_post[, .N, by = .(date(presentation_time), in_set)] %>% ggplot(aes(x = date, y = N, fill = in_set)) + 
#   geom_bar(stat = "identity") +
#   labs(title = "Number of visits by day, with colour showing combined train-val-test split",
#        x = "Date", 
#        y = "Number of visits",
#        fill = "Set") +
#   geom_vline(xintercept = c(date("2020-12-01"), date("2020-12-29"))) 
# 
# library(gridExtra)
# grid.arrange(p1, p3)
# 
# # Raw nuumbers 
# summ[, .N, by = .(epoch, in_set)] %>% ggplot(aes(x = fct_rev(epoch), y = N, fill = in_set)) + geom_bar(stat = "identity") +
#   labs(title = "Numbers in each set during each period",
#        x = "epoch period", 
#        y = "Number of visits",
#        fill = "Set") 
# 
# # Raw nuumbers to get counts on the plot
# summ %>%  ggplot(aes(x = fct_rev(epoch), fill = in_set)) + geom_bar(stat = "count") +
#   labs(title = "Number of visits in each set during each period",
#        x = "epoch period", 
#        y = "Number of visits",
#        fill = "Set") + 
#   stat_count(geom = "text", colour = "black", size = 3.5,
#              aes(label = ..count..),position=position_stack(vjust=0.5))
#   
# 
# 
# 
# 
# 
# # Proportions 
# summ[, .N, by = .(epoch, in_set)] %>% ggplot(aes(x = fct_rev(epoch), y = N, fill = in_set)) + geom_bar(stat = "identity", position = "fill") +
#   labs(title = "Proportions in each set during each period",
#        x = "epoch period", 
#        y = "Number of visits",
#        fill = "Set") 
# 
# 
# 
# 
# # proportion of admissions with lines
# summ[, .N, by = .(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = fct_rev(adm))) + 
#   geom_bar(stat = "identity", position = "fill") +
#   geom_vline(xintercept = c(date("2019-11-19"), date("2019-12-13"), date("2020-03-19"), date("2020-12-01"), date("2020-12-29"))) +
#   labs(title = "Proportion of admissions and discharges by day",
#        x = "Date", 
#        y = "Number of visits",
#        fill = "Disposition") 
# 
# 
# 
# # proportion of admissions in total
# summ[, .N, by = .(epoch, in_set, adm)] %>% ggplot(aes(x = in_set, y = N, fill = fct_rev(adm))) + 
#   geom_bar(stat = "identity", position = "fill") +
#   facet_grid(. ~ epoch) +
#   labs(title = "Proportion of admissions and discharges across training and test sets",
#        x = "Set", 
#        y = "Proportion of visits",
#        fill = "Disposition") 
# 
# 
# # Number admitted by month
# summ[, ym := substr(presentation_time, 1, 7)]
# setorder(summ, ym)
# 
# 
# summ[!ym %in% c("2019-04", "2021-04"), .(num = sum(adm == "Admitted")), by = .(epoch, ym)] %>% ggplot(aes(x = ym, y = num, fill = epoch)) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(. ~ epoch, scales = "free_x") +
#   
#   theme(axis.text.x=element_text(angle=90,hjust=1)) +
#   labs(title = "Number of admissions by month, pre and post Covid",
#        x = "Month", 
#        y = "Number of admissions",
#        fill = "Set") +
#   theme(legend.position = "none")
# 
# # Number admitted by month, assigned to each set
# 
# summ[!ym %in% c("2019-04", "2021-04"), .(num = sum(adm == "Admitted")), by = .(epoch, in_set, ym)] %>% ggplot(aes(x = ym, y = num, fill = in_set)) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(. ~ epoch, scales = "free_x") +
#   
#   theme(axis.text.x=element_text(angle=90,hjust=1)) +
#   labs(title = "Number of admissions by month, pre and post Covid",
#        x = "Month", 
#        y = "Number of admissions",
#        fill = "Set")
# 
# # proportion of admissions by month
# 
# summ[!ym %in% c("2019-04", "2021-04"), .(prop = sum(adm == "Admitted")/.N), by = .(epoch, in_set, ym)] %>% ggplot(aes(x = ym, y = prop, fill = in_set)) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(. ~ epoch, scales = "free_x") +
#   
#   theme(axis.text.x=element_text(angle=90,hjust=1)) +
#   labs(title = "Proportion of admissions by month",
#        x = "Month", 
#        y = "Proportion of admissions",
#        fill = "Set") 
# 
# 
# 
# 
# 
# # Demographics ------------------------------------------------------------
# 
# 
# summ %>% ggplot(aes(x = adm, y = age)) + geom_boxplot() + facet_grid(epoch ~ in_set)
# 
# 
# summ[, arrival := gsub(" |-", "", arrival_method)]
# summ[, arrival := if_else(!arrival %in% c("Ambulance",
#                                                "Walkin",
#                                                "PublicTrans",
#                                                "Ambnomedic"), "Other", arrival) ]  %>%
#   ggplot(aes(x = arrival, y = age)) + geom_violin() + facet_grid(epoch ~ in_set)
# 
# 
# # prior visits
# 
# summ <- merge(summ, visits %>% select(csn, 
#                                       # days_since_last_visit, 
#                                       num_prior_adm_after_ED, num_prior_ED_visits, 
#                                       # prop_adm_from_ED
# ), all.x = TRUE)
# 
# 
# summ %>% ggplot(aes(x = adm, y = num_prior_ED_visits)) + geom_boxplot() + facet_grid(epoch ~ in_set) +
#   labs(y = "Number of prior ED visit")
# 
# summ %>% ggplot(aes(x = adm, y = num_prior_adm_after_ED/num_prior_ED_visits)) + geom_violin() + facet_grid(epoch ~ in_set) +
#   labs(y = "Proportion of admissions from prior ED visits")
# 
# # Old code ----------------------------------------------------------------
# 
# 
# 
# # I first did this - but note this does not stratify using the dependent variable
# # use the following code to divide into relevant group - train, val or teset
# 
# test = data.table(row_id = sample(nrow(dm), 0.2 * nrow(dm)), in_set = "test")
# rest = base::setdiff(seq_len(nrow(dm)), test$row_id)
# 
# val = data.table(row_id = sample(rest, 1/8 * length(rest)), in_set = "val")
# train = data.table(row_id = setdiff(seq_len(nrow(dm)), c(val$row_id, test$row_id)), in_set = "train")
# 
# row_ids = bind_rows(bind_rows(test, val), train)
# 
# dm[, row_id := seq_len(nrow(dm))]
# dm = merge(dm, row_ids, by = "row_id")
# 
# # So instead I used a tidy models split
# 
# 
# # Generate timeslices -----------------------------------------------------
# 
# # generate timeslices using generate-timeslices.R
# 
# # process slices using beginning of run-ML.R
# 
# 
# 
# timeslices <- c("000", "015", "030", "060", "090", "120", "180", "240", "300", "360", "480")
# 
# adm_summ <- data.table()
# in_setsumm <- data.table()
# adm_in_setsumm <- data.table()
# 
# for (ts_ in timeslices) {
#   name_ <- paste0("dm", ts_, "p")
#   ts = get(name_)
#   num_adm <- ts[, .N, by = .(adm)]
#   num_adm[, model := ts_]
#   
#   num_adm_set <- ts[, .N, by = .(adm, in_set)]
#   num_adm_set[, model := ts_]
#   
#   num_set <- ts[, .N, by = .(in_set)]
#   num_set[, model := ts_]
#   
#   adm_summ <- bind_rows(adm_summ, num_adm)
#   in_setsumm <- bind_rows(in_setsumm, num_set)
#   adm_in_setsumm <- bind_rows(adm_in_setsumm, num_adm_set)
#   
# }
# 
# in_setsumm[, in_set := factor(in_set, levels = c("train", "val", "test"))]
# adm_in_setsumm[, in_set := factor(in_set, levels = c("train", "val", "test"))]
# 
# # look at class balance as timeslices progress
# adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
#   labs(title = "Numbers admitted / not admitted in each timeslice", 
#        fill = "Admitted (1 = TRUE)",
#        x = "Timeslice") +
#   theme(legend.position = "bottom") 
# 
# # same chart with proportions
# 
# adm_summ[, perc := N/sum(N), by = .(model)]
# adm_summ[, label := paste(round(perc*100, 1), "%")]
# 
# adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
#   geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
#   labs(title = "Proportions admitted / not admitted in each timeslice (using prior split into train, val and test)", 
#        fill = "Set",
#        x = "Timeslice") +
#   theme(legend.position = "bottom")
# 
# 
# # look at class balance as timeslices progress with train-test-val split
# adm_in_setsumm %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity", position = "fill") + 
#   labs(title = "Proportion admitted / not admitted in each timeslice", 
#        fill = "Admitted (1 = TRUE)",
#        x = "Timeslice") +
#   theme(legend.position = "bottom")  +
#   facet_wrap(. ~ in_set)
# 
# 
# 
# # look at train, val, test props within timeslice
# 
# in_setsumm[, perc := N/sum(N), by = .(model)]
# in_setsumm[, label := paste(round(perc*100, 1), "%")]
# 
# in_setsumm %>% ggplot(aes(x = model, y = N, fill = in_set)) + geom_bar(stat = "identity") + 
#   geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
#   labs(title = "Proportions in train, validation and test sets in each timeslice (using prior split into train, val and test)", 
#        fill = "Set",
#        x = "Timeslice") +
#   theme(legend.position = "bottom") 
#   
# 
