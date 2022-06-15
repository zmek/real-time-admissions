
# About this script -------------------------------------------------------

# Sets configuration for running predictions

summ_file_date = "2021-07-20"


# for evaluation of real-time predictions, all the predictions are saved
# therefore, the only additional data needed to evaluate them is the truth 
# to get this: 
# - run get-ED-data-from-Star.R
# - set this parameter to the date that get-ED-data-from-Star.R was run
# - edit the file create-data-tables.R and change summ_file_date to summ_file_date_for_checking_real_time
# - then run create-data-tables.R
# - this should result in a summ data table that can be used to get the truth for real-time predicitons

summ_file_date_for_checking_real_time = "2021-09-09"

# NOTE - the files below are all LATER than summ_file_date
# this has been done to keep the original study dates
# but allow for a later post-SDEC test set running up to end_SDEC_study; 
# unlike the evaluation of real-time predictions (noted above), 
# this later test set needs later design matrix data to make predictions
num_in_ED_at_arrival_time_file_date = "2021-09-16"

# obs_file_date =  "2021-12-02"
# labs_file_date =  "2021-09-16"

obs_file_date =  "2021-12-05" # note that obs_raw was retrieved on 2021-12-02
labs_file_date =  "2021-12-04" # note that obs_raw was retrieved on 2021-09-16
consults_file_date = "2021-12-09"


dm_file_date =  "2021-12-13"
dm_single_file_date = "2021-10-15"

cox_surv_date = "2021-09-23"
cox_surv_date_SDEC_test = "2021-09-16"
cox_surv_date_six_week_rolling = "2022-01-21"





poisson_results_date = "2021-09-23"




# Parameters for starting validation and test sets overall ----------------

# train, validation and test set dates for original study

covid_start <- as.POSIXct('2020-03-19 00:00:00')

start_val_pre <- as.POSIXct('2019-11-19 00:00:00')
start_test_pre <- as.POSIXct('2019-12-13 00:00:00')

start_val_post <- as.POSIXct('2021-03-19 00:00:00')
start_test_post <- as.POSIXct('2021-05-01 00:00:00')

start_study = as.POSIXct('2019-05-01 00:00:00')
end_study = as.POSIXct('2021-07-20 00:00:00')


# parameters for SDEC only study

SDEC_start <- as.POSIXct('2020-11-17 00:00:00')

start_val_post_SDEC = as.POSIXct('2021-05-17 00:00:00')
start_test_post_SDEC = as.POSIXct('2021-06-17 00:00:00')

end_SDEC_study = as.POSIXct('2021-07-20 00:00:00')



# Model output files ------------------------------------------------------

model_output_date_Post = "2021-12-14"
`model_output_date_Post-SDEC` = "2021-12-15"
model_output_date_Pre = "2021-12-14"
`model_output_date_Pre + Post` = "2021-12-15"

preds_file_date = "2022-01-11"
presds_file_date_3_models = "2022-05-11"

diss_model_output_date_Pre_alop = "2021-08-11"
diss_model_output_date_Post_alop = "2021-08-16"

diss_model_output_date_Pre_aop = "2021-08-16"
diss_model_output_date_Post_aop = "2021-08-17"

diss_model_output_date_Pre_alop_rf = "2021-08-27"
diss_model_output_date_Post_alop_rf = "2021-08-29"

diss_model_output_date_Pre_on_Post = "2021-08-27"

model_output_date_Pre_single = "2021-10-15"
model_output_date_Post_single = "2021-10-15"
`model_output_date_Post-SDEC_single` = "2021-10-15"
`model_output_date_Pre + Post_single` = "2021-10-15"


model_output_date_Pre_alop_lr = "2022-05-03"

# Adjustment for Cox regression coeffient ---------------------------------


# pow_adjustment = data.table(model_period = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
#                             adjustment = c(0.3, 0, 0.3, 0.2))


# removed the King Utley correction on "2021-08-24"

pow_adjustment = data.table(model_period = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                            adjustment = c(0, 0, 0, 0))


# Probability distribution files ------------------------------------------------------



model_eval_date_Post_train = "2021-09-08"
`model_eval_date_Post-SDEC_train` = "2021-09-08"
model_eval_date_Pre_train = "2021-09-08"
`model_eval_date_Pre + Post_train` = "2021-09-08"

model_eval_date_Post_val = "2021-09-07"
`model_eval_date_Post-SDEC_val` = "2021-09-07"
model_eval_date_Pre_val = "2021-09-08"
`model_eval_date_Pre + Post_val` = "2021-09-07"

# # versions using the mid point between timeslices for prediction
# 
# model_eval_date_Post_test = "2021-12-16"
# model_eval_date_Pre_test =  "2021-12-16"
# `model_eval_date_Pre + Post_test` =  "2021-12-16"
# `model_eval_date_Post-SDEC_test` =  "2021-12-16"
# model_eval_date_six_week_rolling_Post_test = "2021-12-17"
# 
# earlier version not using mid point

model_eval_date_Post_test = "2021-12-14"
model_eval_date_Pre_test =  "2021-12-14"
`model_eval_date_Pre + Post_test` =  "2021-12-15"
`model_eval_date_Post-SDEC_test` =  "2021-12-15"
model_eval_date_six_week_rolling_Post_test = "2021-12-15"
`model_eval_date_six_week_rolling_Post-SDEC_test` =  "2022-01-21"


# For charts --------------------------------------------------------------


colour_train = "#00BA38"
colour_val = "#619CFF"
colour_test = "#F8766D"
