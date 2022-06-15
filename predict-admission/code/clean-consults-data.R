
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

load(paste0("EDcrowding/predict-admission/data-raw/consults_raw_",consults_file_date,".rda"))
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
consults <- data.table(consults_raw)
consults <- consults[csn %in% summ$csn]
setkey(consults, csn)

# remove labs that are returned after ED

consults <- merge(consults, summ[,.(csn, first_ED_admission, left_ED)]) 
consults <- consults[scheduled_datetime <= left_ED]

# adding this correction for now as it appears that suspiciously many visits have consults in the hour before ED admission
# but only in British summer time
# to see this, run clean_consults_data WITHOUT the correction below
# then 
# c = consults_real[elapsed_mins > -60 & elapsed_mins < 0, month(scheduled_datetime)]
# table(c)
# consults[dst(scheduled_datetime), scheduled_datetime := scheduled_datetime + hours(1)]

consults_real = clean_consults_data(consults, summ, real_time = FALSE)
consults_real <- consults_real[, .(csn, scheduled_datetime, code, elapsed_mins)]




# Save data ---------------------------------------------------------------


# create final dataset of orders

outFile = paste0("EDcrowding/predict-admission/data-raw/consults_real_",today(),".rda")
save(consults_real, file = outFile)


