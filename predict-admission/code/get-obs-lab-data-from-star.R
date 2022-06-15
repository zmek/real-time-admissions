# About this file
# ===============

# This file collects data from Star, which requires extracting data from non-materialised tables.



# Load libraries ----------------------------------------------------------


library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Load data ---------------------------------------------------------------




# Set up connection
ctn <- DBI::dbConnect() # Details withheld for information security reasons





# Get and process obs data ------------------------------------------------------

# added a time interval to reduce the number of rows returned

sqlQuery <- "
    SQL query withheld for information security reasons
"

sqlQuery <- gsub('\n','',sqlQuery)
obs_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))


save(obs_raw, file = paste0('EDcrowding/predict-admission/data-raw/obs_raw_',today(),'.rda'))



# # Get and process lab orders ------------------------------------------------

sqlQuery <- "
    SQL query withheld for information security reasons
"

sqlQuery <- gsub('\n','',sqlQuery)
lab_orders_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))

lab_orders_raw = data.table(lab_orders_raw)

save(lab_orders_raw, file = paste0('EDcrowding/predict-admission/data-raw/lab_orders_raw_',today(),'.rda'))


# Get and process lab results -------------------------------------------------------------



sqlQuery <- "
    SQL query withheld for information security reasons
"

sqlQuery <- gsub('\n','',sqlQuery)
lab_results_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))

# add battery code for lab results

lab_results_raw = data.table(lab_results_raw)
lab_results_raw = merge(lab_results_raw, lab_orders_raw[, .(csn, lab_order_id, battery_code)], by = c("csn", "lab_order_id"), all.x = TRUE)


save(lab_results_raw, file = paste0('EDcrowding/predict-admission/data-raw/lab_results_raw_',today(),'.rda'))




# Get and process consults -------------------------------------------------------------



sqlQuery <- "
    SQL query withheld for information security reasons
    "

sqlQuery <- gsub('\n','',sqlQuery)
consults_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))

# add battery code for lab results


save(consults_raw, file = paste0('EDcrowding/predict-admission/data-raw/consults_raw_',today(),'.rda'))






