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


# if (summ_file_date_for_checking_real_time > summ_file_date) {
#   
#   load(paste0("EDcrowding/flow-mapping/data-raw/moves_", summ_file_date_for_checking_real_time,".rda"))
#   load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
#   load(paste0("EDcrowding/flow-mapping/data-raw/visits_all_", summ_file_date_for_checking_real_time,".rda"))
#   
# } else {
  
  load(paste0("EDcrowding/flow-mapping/data-raw/moves_", summ_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/visits_all_", summ_file_date,".rda"))
# }




# observation data

load(paste0("EDcrowding/predict-admission/data-raw/obs_real_", obs_file_date,".rda"))


# lab data

load(paste0("EDcrowding/predict-admission/data-raw/lab_orders_real_", labs_file_date,".rda"))
load(paste0("EDcrowding/predict-admission/data-raw/lab_results_real_", labs_file_date,".rda"))

# consults data
load(paste0("EDcrowding/predict-admission/data-raw/consults_real_", consults_file_date,".rda"))


# Covid surge data --------------------------------------------------------


# covid surge data
# Source: https://coronavirus.data.gov.uk/details/deaths
# then click downloads
# select newDeaths28DaysByDeathDate
# download and copy to EDcrowding/real-time/model-input/
# archive old version of covid deaths.csv and rename this as new version
# alternative is to use dates of lockdowns https://commonslibrary.parliament.uk/research-briefings/cbp-9068/

# for automatically retrieving via api
# url = "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newDeaths28DaysByDeathDate&format=csv"
library(readr)
covid_cases <- data.table(read_csv("EDcrowding/real-time/model-input/covid deaths.csv"))
setorder(covid_cases, date)
covid_cases[, rolling7 := frollmean(newDeaths28DaysByDeathDate, 7)]
surge_level = 75
covid_cases[, surge := if_else(rolling7 > surge_level, TRUE, FALSE)]
# covid_cases %>% ggplot(aes(x = date, y = rolling7, fill = surge)) + geom_bar(stat = "identity") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y %m") +
#   geom_hline(yintercept = surge_level) + 
#   labs(title = "7 day rolling average death rates, with suggested cutoff for surge periods", 
#        subtitle = "Source: https://coronavirus.data.gov.uk/details/deaths",
#        y = "7 day rolling average")

# Create admission details--------------------------------------------------

summ <- merge(summ, visits %>% select(csn, 
                                  num_adm_after_ED, num_ED_visits), all.x = TRUE)

dm <- prep_summ_for_ML(summ, NA, real_time = FALSE)


# prepare outcome variable
dm <- merge(dm, summ[, .(csn, adm)], by = "csn")
dm[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]

setkey(dm, csn)

# set COVID surge info

dm[, date := date(first_ED_admission)]
dm = merge(dm, covid_cases[,.(date, surge)], by = "date", all.x = TRUE)
dm[, covid_surge := case_when(first_ED_admission < covid_start  ~ NA,
                        TRUE ~ surge)]
dm[, surge := NULL]
dm[, date := NULL]

# Prepare location data --------------------------------------------------

loc <- prep_moves_for_ML(moves, dm, real_time = FALSE)

# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)


dm[, c("presentation_time", "first_ED_admission", "left_ED", "first_outside_proper_admission") := NULL]

cols = colnames(copy(dm)[, c("csn", "adm", "duration") := NULL])
cols_ = paste("a", cols, sep="_")
setnames(dm, cols, cols_)



# Create timeslices -------------------------------------------------------


timeslices <- c(0, 15, 30, 60, 90, 120, 180, 240, 300, 360, 480, 720, 24*60, 48*60)

for (i in seq(1, length(timeslices) -1, 1)) {
  print(paste0("Processing timeslice ", timeslices[i]))
  filenum <- case_when(nchar(as.character(timeslices[i])) == 1 ~ paste0("00", timeslices[i]),
                       nchar(as.character(timeslices[i])) == 2 ~ paste0("0", timeslices[i]),
                      TRUE ~ as.character(timeslices[i]))
  name_ <- paste0("dm", filenum)
  
  # commenting out this version, which allows timeslices to learn from data after their cutoff 
  # ts <- create_timeslice(loc, dm, obs_real, lab_orders_real, lab_results_real, timeslices[i], timeslices[i+1], real_time = FALSE)
  ts <- create_timeslice(loc, dm, obs_real, lab_orders_real, lab_results_real, consults_real, timeslices[i], timeslices[i], real_time = FALSE)
  
  
  assign(name_, ts)
}



save(dm000, file = paste0("EDcrowding/predict-admission/data-raw/dm000_",today(),".rda"))
save(dm015, file = paste0("EDcrowding/predict-admission/data-raw/dm015_",today(),".rda"))
save(dm030, file = paste0("EDcrowding/predict-admission/data-raw/dm030_",today(),".rda"))
save(dm060, file = paste0("EDcrowding/predict-admission/data-raw/dm060_",today(),".rda"))
save(dm090, file = paste0("EDcrowding/predict-admission/data-raw/dm090_",today(),".rda"))
save(dm120, file = paste0("EDcrowding/predict-admission/data-raw/dm120_",today(),".rda"))
save(dm180, file = paste0("EDcrowding/predict-admission/data-raw/dm180_",today(),".rda"))
save(dm240, file = paste0("EDcrowding/predict-admission/data-raw/dm240_",today(),".rda"))
save(dm300, file = paste0("EDcrowding/predict-admission/data-raw/dm300_",today(),".rda"))
save(dm360, file = paste0("EDcrowding/predict-admission/data-raw/dm360_",today(),".rda"))
save(dm480, file = paste0("EDcrowding/predict-admission/data-raw/dm480_",today(),".rda"))
save(dm720, file = paste0("EDcrowding/predict-admission/data-raw/dm720_",today(),".rda"))
# save(dm1440, file = paste0("EDcrowding/predict-admission/data-raw/dm1440_",today(),".rda"))

dm[, row_id := seq_len(nrow(dm))]
save(dm, file = paste0("EDcrowding/predict-admission/data-raw/dm_",today(),".rda"))






