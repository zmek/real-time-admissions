# ===============
# About this file
# ===============

# - Loads data on ED patients locations
# - Calculates numbers in ED at time of each patient's arrival; this output is used for
# as an input to the Cox regression calculations

# Also:
# - Formats a matrix for input into a ML model to predict location (this model formed
# part of the intitial exploration but was not used in the final analysis); matrix
# holds number of patients in any location in each hour, and a second matrix returns
# the number of patients who moved between locations in any hour



# Programme instructions --------------------------------------------------

data_for_agg_ML = FALSE # set this to TRUE to create transition matrixs between locations
data_for_indiv_ML = TRUE # set this to TRUE to generate numbers in ED at each arrival time


# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


get_nums_by_dttm <- function(date_range, moves, edgdf, data_for_agg_ML) {
  
  num_in_location <- data.table()
  moved_from_location <- data.table()
  
  
  
  for (i in 2:length(date_range)) {
    
    if (i %% 100 == 0) {
      print(paste("Processed",i,"datetimes"))
    }
    
    # number in ED location at the timepoint
    num = moves[admission <= date_range[i] & discharge >= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]
    # if noone is in the location, we still need a row to represent the timeslot
    if (nrow(num) == 0) {
      num = data.table(DateTime = date_range[i], location = "MAJORS", N = 0)
    } else {
      num$DateTime = date_range[i] 
      num$tot_in_ED = sum(num$N)
    }
    num_in_location <- bind_rows(num_in_location, num)
    
    if (data_for_agg_ML) {
      
      # numbers leaving ED location within the hour up to the timepoint
      moved = moves[discharge > date_range[i-1] & discharge <= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]
      
      # if noone moved, we still need a row to represent the timeslot
      if (nrow(moved) == 0) {
        moved = data.table(DateTime = date_range[i])
      } else {
        moved$DateTime = date_range[i] 
      }
      moved_from_location <- bind_rows(moved_from_location, moved)
    }
  }
  
  if (data_for_agg_ML)  {
    
    adm_during_hour = data.table()
    for (i in 2:length(date_range)) {
      
      # for debugging
      summ[first_outside_proper_admission > as.POSIXct('2019-11-26 11:00:00') & first_outside_proper_admission <= as.POSIXct('2019-11-26 16:00:00')]
      
      # get hours between presentation and admission for each person admitted in hour
      wait = data.table(summ[first_outside_proper_admission > date_range[i-1] & first_outside_proper_admission <= date_range[i], 
                             # .(csn, wait = as.integer(floor(difftime(first_outside_proper_admission, first_ED_admission, units = "hours"))))] %>%
                             .(csn, wait = as.integer(floor_date(first_outside_proper_admission, "hour") -
                                                        floor_date(first_ED_admission, "hour")))] %>% 
                          
                          group_by(wait) %>% summarise(N = n() , .groups = 'drop'))
      
      if (nrow(wait) == 0) {
        
        wait = data.table(adm = 0)
        wait[, DateTime := date_range[i]]
        
      } else {
        wait[, adm := sum(N)]
        wait[, DateTime := date_range[i]]
        
        # the row below will convert any who waited more than 12 hours to 12
        wait[wait > 12, wait := 12]
        # this results in multiple rows per date time if more than one person had a (different length) wait of more than 12 hours
        # so summarise by datetime
        wait = unique(wait[, N := sum(N), by = .(wait, DateTime)])
        
      }
      
      
      adm_during_hour = bind_rows(adm_during_hour, wait)    
      
    }
    
    # in case there are empty cells in the array of adm_wait (noone was admitted after that number of hours)
    nums_in_array = unique(adm_during_hour[!is.na(wait), wait])
    nums_in_array = nums_in_array[order(nums_in_array)]
    
    # select cols in the correct order
    cols = c("adm", "DateTime", paste0("adm_wait", nums_in_array))
    adm_during_hour = adm_during_hour %>% pivot_wider(names_from = wait, names_prefix = "adm_wait", values_from = N, values_fill = 0) %>% 
      select(all_of(cols))
    
    # pivot num_in_location to wide array
    num_in_location[, time_of_day := hour(DateTime)]
    num_in_location[, month := month(DateTime)]
    num_in_location[, day_of_week := wday(DateTime)]
    
    moved_from_location[, time_of_day := hour(DateTime)]
    moved_from_location[, month := month(DateTime)]
    moved_from_location[, day_of_week := wday(DateTime)]
    
    # add admission array
    moved_w <- moved_from_location %>% pivot_wider(names_from = location, values_from = N, values_fill = 0) 
    moved_w <-moved_w %>% 
      # reorder colnames
      select(DateTime:day_of_week, colnames(moved_w)[5:(ncol(moved_w))][order(colnames(moved_w)[5:(ncol(moved_w))])]) %>% 
      left_join(adm_during_hour) 
    # NA rows are created if noone moved during a time slot; these become NA column after pivot
    if (sum(grepl("NA", colnames(moved_w)))>0) {
      moved_w <- moved_w %>% select(-`NA`)
    }
    
    
    # add admission array
    num_w <- num_in_location  %>% pivot_wider(names_from = location, values_from = N, values_fill = 0) 
    num_w <-num_w %>% 
      # reorder colnames
      select(DateTime:day_of_week, colnames(num_w)[5:(ncol(num_w))][order(colnames(num_w)[5:(ncol(num_w))])]) %>% 
      left_join(adm_during_hour) 
    # NA rows are created if noone moved during a time slot; these become NA column after pivot
    if (sum(grepl("NA", colnames(num_w)))>0) {
      num_w <- num_w %>% select(-`NA`)
    }
    
    output <- list(moved_w, num_w)
    
  } else {
    
    output <- num_in_location  %>% pivot_wider(names_from = location, names_prefix = "num_", values_from = N, values_fill = 0) 

  }
  
  
  return(output)
  
}

# Create functions --------------------------------------------------------


# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

#
# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")



# Load data ---------------------------------------------------------------


if (summ_file_date_for_checking_real_time > summ_file_date) {
  
  inFile = paste0("EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",num_in_ED_at_arrival_time_file_date,".rda")
  load(inFile)
  
  num_in_ED_old = num_in_ED
  
  load(paste0("EDcrowding/flow-mapping/data-raw/moves_", summ_file_date_for_checking_real_time,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/edgedf_", summ_file_date_for_checking_real_time,".rda"))
  
} else {
  
  # restart from scratch
  load(paste0("EDcrowding/flow-mapping/data-raw/moves_", summ_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
  load(paste0("EDcrowding/flow-mapping/data-raw/edgedf_", summ_file_date,".rda"))
  
}



# Pre-process data ----------------------------------------------------

if (summ_file_date_for_checking_real_time > summ_file_date) {
  
  # just add the additional data needed after the original summ_file_date
  summ = summ[first_ED_admission >= max(num_in_ED$DateTime)]
  summ = summ[first_ED_admission < end_SDEC_study]
  
} else {
  
  # start from scratch
  summ = summ[first_ED_admission >= start_study]
  summ = summ[first_ED_admission < end_study]
  
}

# added this on 22.5.21
moves = moves[csn %in% summ$csn]
edgedf = edgedf[csn %in% summ$csn]


# Update edge list -----------------------------


# identify moves where patient is admitted from ED
edgedf[from_dept %in% c("ED", "UCHT00CDU") & !(to_dept %in% c("ED", "UCHT00CDU") | is.na(to_dept)), adm := "Admitted"]
# checking - missing 29 csns all look like prior inpatient then discharged
summ[adm ==1 & !(csn %in% edgedf[adm == "Admitted",csn])]

# identify moves where patient is discharged from ED
edgedf[from_dept %in% c("ED", "UCHT00CDU") & is.na(to_dept), adm := "Discharged"]
# checking
summ[adm == 0 & !(csn %in% edgedf[adm == "Discharged",csn])]

# update "to" column with admitted or discharged
edgedf[from_dept %in% c("ED", "UCHT00CDU"), to_new := case_when(!is.na(adm) ~ adm,
                                                                TRUE ~  to)]
# check number of admission and discharges matches summ
# note that this won't exactly match summ$adm because some visits involve > 1 admission
rpt(edgedf[from_dept %in% c("ED", "UCHT00CDU") & to_new == "Discharged"])
rpt(edgedf[from_dept %in% c("ED", "UCHT00CDU") & to_new == "Admitted"])


# Save num in ED at arrival time ------------------------------------------

if (data_for_indiv_ML) {
  
  # if only admitted patients 
  # date_range = unique(summ[adm %in% c("direct_adm", "indirect_adm"), first_ED_admission])

  # OR useful way of reducing number to process if using all patients not just those admitted
  date_range <- as.POSIXct(unique(substr(summ$first_ED_admission, 1,16)))
  num_in_ED <- get_nums_by_dttm(date_range, moves, edgedf, data_for_agg_ML)
  num_in_ED = data.table(num_in_ED)
  
  # create factors that will be needed for Cox regression later
  num_in_ED[, quarter := factor(case_when( month(DateTime) <= 3 ~ 1,
                                           month(DateTime) <= 6 ~ 2, 
                                           month(DateTime) <= 9 ~ 3, 
                                           month(DateTime) <= 12 ~ 4))]
  num_in_ED[, weekend := factor(if_else(weekdays(DateTime, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]
  num_in_ED[, arrival_time := factor(case_when(hour(DateTime) >= 22 | hour(DateTime) < 6 ~ "22:00-06:00",
                                               hour(DateTime) >= 6 & hour(DateTime) < 12 ~ "06:00-12:00",
                                               hour(DateTime) >= 12 & hour(DateTime) < 16 ~ "12:00-16:00",
                                               hour(DateTime) >= 16 & hour(DateTime) < 22 ~ "16:00-22:00"))]
  
  if (summ_file_date_for_checking_real_time > summ_file_date) {
    
    num_in_ED = bind_rows(num_in_ED_old, num_in_ED)
  }
  
  
  outFile = paste0("EDcrowding/flow-mapping/data-raw/num-in-ED-at-arrival-time_",today(),".rda")
  save(num_in_ED, file = outFile) 
  
    
}




# Create adjacency matrix for the two epochs ------------------------------


if (data_for_agg_ML) {
  
  # split into before and after covid
  
  edgedf_before_covid <- edgedf[csn %in% summ[first_ED_admission < covid_start, csn]] # changed this to first_ED_admission on 22.5.21
  rpt(edgedf_before_covid)
  
  edgedf_after_covid <- edgedf[csn %in% summ[first_ED_admission >= covid_start, csn]]
  rpt(edgedf_after_covid)
  
  #  after covid, Diagnostics becomes a very infrequently used location
  edgedf_after_covid[, from := case_when(from == "DIAGNOSTICS" ~ "Other",
                                         TRUE ~ from)]
  edgedf_after_covid[, to_new := case_when(to_new == "DIAGNOSTICS" ~ "Other",
                                           TRUE ~ to_new)]
  
  # create adjacency matrices
  
  before_covid_adj_matrix <- edgedf_before_covid[from_dept %in% c("ED", "UCHT00CDU")] %>%
    group_by(from, to_new) %>%
    summarise(tot = n()) %>%
    pivot_wider(names_from = to_new, values_from = tot) %>%
    column_to_rownames("from") %>%
    replace(is.na(.), 0) 
  
  before_covid_adj_matrix <- before_covid_adj_matrix %>% 
    # reorder colnames
    select(colnames(before_covid_adj_matrix %>% select(-Admitted, -Discharged))[order(colnames(before_covid_adj_matrix %>% select(-Admitted, -Discharged)))], 
           Admitted, Discharged)
  
  
  after_covid_adj_matrix <- edgedf_after_covid[from_dept %in% c("ED", "UCHT00CDU")] %>%
    group_by(from, to_new) %>%
    summarise(tot = n()) %>%
    pivot_wider(names_from = to_new, values_from = tot) %>%
    column_to_rownames("from") %>%
    replace(is.na(.), 0) 
  
  after_covid_adj_matrix <- after_covid_adj_matrix %>% 
    # reorder colnames
    select(colnames(after_covid_adj_matrix %>% select(-Admitted, -Discharged))[order(colnames(after_covid_adj_matrix %>% select(-Admitted, -Discharged)))], 
           Admitted, Discharged)
  
  outFile = paste0("EDcrowding/data-prep-for-ML/data-output/before_covid_adj_matrix_",today(),".csv")
  write.csv(before_covid_adj_matrix, file = outFile, row.names = TRUE)
  
  outFile = paste0("EDcrowding/data-prep-for-ML/data-output/after_covid_adj_matrix_",today(),".csv")
  write.csv(after_covid_adj_matrix, file = outFile, row.names = TRUE)
  
}


# Process data by time periods ------------------------------------------------------------

if (data_for_agg_ML) {
  
  # create a series with all the required time periods by hour

  
  # before Covid started
  date_range <- seq(matrix_start_date - hours(1), covid_start, by = "hours")
  before_covid <- get_nums_by_dttm(date_range, moves, edgedf_before_covid, data_for_agg_ML) # couldn't get the function to work
  
  outFile = paste0("EDcrowding/data-prep-for-ML/data-output/before_covid_moved_from_location_",today(),".csv")
  write.csv(before_covid[[1]], file = outFile, row.names = FALSE)
  
  outFile = paste0("EDcrowding/data-prep-for-ML/data-output/before_covid_num_in_location_",today(),".csv")
  write.csv(before_covid[[2]], file = outFile, row.names = FALSE)
  
  # after Covid started
  date_range <- seq(covid_start - hours(1), matrix_end_date, by = "hours")
  after_covid <- get_nums_by_dttm(date_range, moves, edgedf_after_covid, data_for_agg_ML)
  
  
  outFile = paste0("EDcrowding/data-prep-for-ML/data-output/after_covid_moved_from_location_",today(),".csv")
  write.csv(after_covid[[1]], file = outFile, row.names = FALSE)
  
  outFile = paste0("EDcrowding/data-prep-for-ML/data-output/after_covid_num_in_location_",today(),".csv")
  write.csv(after_covid[[2]], file = outFile, row.names = FALSE)
  
  
  
}


