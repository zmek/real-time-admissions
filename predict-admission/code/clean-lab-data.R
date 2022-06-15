
# About this file ---------------------------------------------------------

# Takes flowsheet data and cleans it ready for creating design matrix
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(readr)



# Load function -----------------------------------------------------------


# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")


# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

#
# add utils file which is shared by real-time app
# using this here will ensure that the real-time app and the ML models use the same locations
source("EDcrowding/real-time/app/utils.R")



# Load data ---------------------------------------------------------------

load(paste0("EDcrowding/predict-admission/data-raw/lab_orders_raw_",labs_file_date,".rda"))
load(paste0("EDcrowding/predict-admission/data-raw/lab_results_raw_",labs_file_date,".rda"))
# 
# if (summ_file_date_for_checking_real_time > summ_file_date) {
#   
#   load(paste0("EDcrowding/flow-mapping/data-raw/moves_",summ_file_date_for_checking_real_time,".rda"))
#   load(paste0("EDcrowding/flow-mapping/data-raw/summ_",summ_file_date_for_checking_real_time,".rda"))
#   
# } else {
  
  load(paste0("EDcrowding/flow-mapping/data-raw/moves_",summ_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_",summ_file_date,".rda"))
  
# }






# Process data ------------------------------------------------------------



# remove csns that are not included 
lab_orders <- data.table(lab_orders_raw)
lab_orders <- lab_orders[csn %in% summ$csn]
setkey(lab_orders, csn)

lab_results <- data.table(lab_results_raw)
lab_results <- lab_results[csn %in% summ$csn]
setkey(lab_results, csn)

# remove labs that are returned after ED

lab_orders <- merge(lab_orders, summ[,.(csn, first_ED_admission, left_ED)]) 
lab_orders <- lab_orders[request_datetime <= left_ED]

lab_results <- merge(lab_results, summ[,.(csn, first_ED_admission, left_ED)]) 
lab_results <- lab_results[result_last_modified_time <= left_ED]


clean_labs = clean_lab_data(lab_orders, lab_results, NA, summ, real_time = FALSE)

lab_orders_real = clean_labs[[1]]
lab_results_real = clean_labs[[2]]



# keep only lab orders that are used more than 50 times
battery_count = lab_orders_real[, .N, by = battery_code]
lab_orders_real = lab_orders_real[battery_code %in% battery_count[N>50, battery_code]]

# Save data ---------------------------------------------------------------


# create final dataset of orders
lab_orders_real <- lab_orders_real[, .(csn, request_datetime, battery_code, elapsed_mins)]

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_orders_real_",today(),".rda")
save(lab_orders_real, file = outFile)

# create final dataset of results (real values)
lab_results_real <- lab_results_real[, .(csn, result_last_modified_time, abnormal_flag, value_as_real, test_lab_code, elapsed_mins, oor_low, oor_high, abnormal)]

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_results_real_",today(),".rda")
save(lab_results_real, file = outFile)

# save record of which batteries were included
lab_orders_to_include = unique(lab_orders_real$battery_code)
outFile = paste0("EDcrowding/real-time/model-input/lab_orders_to_include.rda")
save(lab_orders_to_include, file = outFile)

