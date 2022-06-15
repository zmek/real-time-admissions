# About this file
# ==============

# Generates timeslices for ML. A subset of cases are held out for final testing



# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Create functions --------------------------------------------------------


# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

#
# add utils file which is shared by real-time app
# using this here will ensure that the real-time app and the ML models use the same locations
source("EDcrowding/real-time/app/utils.R")



# Load data ---------------------------------------------------------------

if (summ_file_date_for_checking_real_time > summ_file_date) {
  load(paste0("EDcrowding/flow-mapping/data-raw/moves_", summ_file_date_for_checking_real_time,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
  
} else {
  
  load(paste0("EDcrowding/flow-mapping/data-raw/moves_", summ_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
}

# load beginning of data matrix

load(paste0("EDcrowding/predict-admission/data-raw/dm_", dm_file_date,".rda"))


# observation data

load(paste0("EDcrowding/predict-admission/data-raw/obs_real_", obs_file_date,".rda"))


# lab data

load(paste0("EDcrowding/predict-admission/data-raw/lab_orders_real_", labs_file_date,".rda"))
load(paste0("EDcrowding/predict-admission/data-raw/lab_results_real_", labs_file_date,".rda"))



# Merge dm with summ to get ED admission and discharge times --------------

dm = merge(dm, summ[, .(csn, first_ED_admission, left_ED)], by = "csn")

dm[, arrival_hr := as.integer(substr(first_ED_admission, 12,13))]

dm[, a_arrival_window := case_when(arrival_hr > 21 | arrival_hr < 6 ~ "2200-0600",
                                 arrival_hr < 12 ~ "0600-1200",
                                 arrival_hr > 16 ~ "1200-1600",
                                 arrival_hr < 22 ~ "1600-2200")]

dm[, c("first_ED_admission", "left_ED", "arrival_hr") := NULL]


# # Set up a series of time points to sample --------------------------------

load("EDcrowding/flow-mapping/data-raw/in_ED_at_time_pt_2021-10-07.rda")
# in_ED_all = in_ED_all_old
# 
# start_of_set = start_study
# 
# if (summ_file_date_for_checking_real_time > summ_file_date) {
#   end_of_set = summ_file_date_for_checking_real_time
# } else {
#   end_of_set = end_study
# }
# 
# 
# next_dt = start_of_set
# 
# time_pts = POSIXct()
# while (next_dt < end_of_set) {
#   next_pt <- next_dt + c(hours(6), hours(12), hours(16), hours(22))
# 
#   # if it's British summer time, move the hour back one because the report is generated at the same hours
#   if (sum(dst(next_pt) > 0)) {
#     next_pt = force_tz(next_pt - hours(1), tz = "GMT")
#   }
# 
#   time_pts <- c(time_pts, next_pt)
#   next_dt = next_dt + days(1)
# }
# 
# 
# time_pts = time_pts[time_pts>max(in_ED_all_old$time_pt)]
# 
# # # get patients in ED at those times - commented out as this has now been saved
# in_ED_all = data.table()
# 
# for (i in 1: length(time_pts)) {
# 
#   if (i %% 100 == 0) {print(paste("processed", i, "time pointts"))}
# 
#   in_ED = summ[first_ED_admission < time_pts[i] & left_ED >= time_pts[i],
#                .(csn,
#                  elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
#                  remaining = difftime(left_ED, time_pts[i], units = "mins"))]
#   in_ED$time_pt = time_pts[i]
# 
#   in_ED_all = bind_rows(in_ED_all, in_ED)
# 
#   # if (i %% 1000 == 0) {
#   #   save(in_ED_all, file = paste0("EDcrowding/flow-mapping/data-raw/in_ED_at_time_pt_",today(),".rda"))
#   # }
#   # 
#   # save(in_ED_all, file = paste0("EDcrowding/flow-mapping/data-raw/in_ED_at_time_pt_",today(),".rda"))
# }
# 
# in_ED_all = bind_rows(in_ED_all_old, in_ED_all)
# save(in_ED_all, file = paste0("EDcrowding/flow-mapping/data-raw/in_ED_at_time_pt_",today(),".rda"))

# merge with design matrix keeping only rows that appear in in_ED_all
dm = merge(in_ED_all, dm, by = "csn")

dm[, elapsed := as.integer(round(elapsed))]


# Prepare location data --------------------------------------------------

loc <- prep_moves_for_ML(moves, dm, real_time = FALSE)
loc[, duration := NULL]

loc = unique(loc)

# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)


dm[, remaining := NULL]
# dm[, duration := NULL]


options(dplyr.summarise.inform = FALSE)

dm_single = data.table()
# load("EDcrowding/predict-admission/data-raw/dm_single_2021-10-07.rda")
# dm_single = unique(dm_single)
dm_single_file = paste0("EDcrowding/predict-admission/data-raw/dm_single_",today(),".rda")
# save(dm_single, file = dm_single_file)

 

for (i in 117001:nrow(dm)) {
  
  if (i %% 100 == 0) {print(paste("processed", i, "rows"))}
  
  ts <- create_timeslice(loc[csn == dm$csn[i]], 
                         dm[i], 
                         obs_real[csn == dm$csn[i]], 
                         lab_orders_real[csn == dm$csn[i]], 
                         lab_results_real[csn == dm$csn[i]], 0, dm$elapsed[i], real_time = FALSE,
                         single_timeslice = TRUE)
  
  dm_single = bind_rows(dm_single, ts)
  
  if (i %% 1000 == 0| i == nrow(dm)) {
    
    dm_single_temp = dm_single
    load(dm_single_file)
    dm_single = bind_rows(dm_single, dm_single_temp)
    save(dm_single, file = dm_single_file)
    dm_single = data.table()
    
  }
  
}


# Clean data --------------------------------------------------------------

load("EDcrowding/predict-admission/data-raw/dm_single_2021-10-07.rda")




inFile = paste0("EDcrowding/predict-admission/data-raw/dm_single_", dm_single_file_date, ".rda")
load(inFile)

dm_single[, a_arrival_window := factor(a_arrival_window)]
dm_single[, l_current := factor(l_current)]
dm_single[, elapsed := as.numeric(elapsed)]
dm_single[, time_pt := NULL]
dm_single[, duration := NULL]

# set all logical cols to false
dm_logical = dm_single[ , .SD, .SDcols = is.logical]

for(j in seq_along(dm_logical)){
  set(dm_logical, i = which(is.na(dm_logical[[j]])), j = j, value = FALSE)
}

cols_logical = colnames(dm_logical)
dm_single = bind_cols(dm_single[,.SD, .SDcols = !cols_logical], dm_logical)


# set all count cols to zero
cols_numeric = colnames(dm_single)[grep("_num|_has", colnames(dm_single))]
dm_numeric = dm_single[,.SD, .SDcols = cols_numeric]

for(j in seq_along(dm_numeric)){
  set(dm_numeric, i = which(is.na(dm_numeric[[j]])), j = j, value = 0)
}

dm_single = bind_cols(dm_single[,.SD, .SDcols = !cols_numeric], dm_numeric)


dm_single[, row_id := seq_len(nrow(dm_single))]
save(dm_single, file = paste0("EDcrowding/predict-admission/data-raw/dm_single_",today(),".rda"))






