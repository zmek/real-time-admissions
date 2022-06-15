
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

# if (summ_file_date_for_checking_real_time > summ_file_date) {
#   
#   load(paste0("EDcrowding/predict-admission/data-raw/obs_raw_",obs_file_date,".rda"))
#   load(paste0("EDcrowding/flow-mapping/data-raw/moves_",summ_file_date_for_checking_real_time,".rda"))
#   load(paste0("EDcrowding/flow-mapping/data-raw/summ_",summ_file_date_for_checking_real_time,".rda"))
  
# } else {
  
  load(paste0("EDcrowding/predict-admission/data-raw/obs_raw_",obs_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/moves_",summ_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_",summ_file_date,".rda"))
# }







# mapping of obs visit id not yet availablein Star
# look up mapping here:  https://docs.google.com/spreadsheets/d/1k5DqkOfUkPZnYaNRgM-GrM7OC2S4S2alIiyTC8-OqCw/edit#gid=1661666003

# vo_mapping now saved in a place where the real-time app can access it
vo_mapping <- read_csv("EDcrowding/real-time/model-input/Emap Mapping Spreadsheet.csv",
                       col_types = cols(
                         `Friendly name` = col_character(),
                         `epic id` = col_double()
                       )) %>% data.table()

# # vo_mapping = vo_mapping[,.(`Friendly name`, `epic id`)]
# setnames(vo_mapping, "Friendly name", "obs_name")
# setnames(vo_mapping, "epic id", "id_in_application")
# # # some observations have multiple mappings
# # vo_mapping[,.N, by = id_in_application][N>1]
# vo_mapping = unique(vo_mapping[, obs_name := max(obs_name), by = id_in_application])
# vo_mapping[, obs_name := gsub(" ", "", obs_name)]

# Process data ------------------------------------------------------------

# remove  csns that are not included in summary of ED visits
obs <- data.table(obs_raw)
setkey(obs, csn)
obs <- obs[csn %in% summ$csn]

# remove obs that take place after ED
obs <- merge(obs, summ[,.(csn, first_ED_admission, left_ED)]) 
obs <- obs[observation_datetime <= left_ED]

obs_real = clean_obs_data(obs, summ, vo_mapping, real_time = FALSE)

obs_real = obs_real[obs_name != "Tempsrc"]
outFile = paste0("EDcrowding/predict-admission/data-raw/obs_real_",today(),".rda")
save(obs_real, file = outFile)

# # save record of which obs were included for real-time prediction
# obs_to_include = vo_mapping[obs_name %in% unique(obs_real$obs_name), .(obs_name, id_in_application)]
# outFile = paste0("EDcrowding/real-time/model-input/obs_to_include.rda")
# save(obs_to_include, file = outFile)

# # save for Nel
# outFile = paste0("EDcrowding/real-time/data-raw/obs_to_include.csv")
# write_csv(obs_to_include, file = outFile)
