# ===============
# About this file
# ===============

# This is a shared file between the ML model preparation and the real-time app.
# It includes all the functions called by both elements of the project



# Simple report on number of csns -----------------------------------------

# Used to report on the number of unique visits in any dataset
# 
rpt <- function(dataframe) {
  return(dataframe %>% select(csn) %>% n_distinct())
}



# Generate probability distribution over number of beds -------------------

# A probability generating function, taking as input an array of individual probabilities
# and returning the coefficients of the generating function.
# This function is the most important element of Steps 3, 5, and 7 in the pipeline.
# By passing it a set of individual probabilities of admission, it returns the probabilities
# over a discrete distribution of number of beds needed by those patients

poly_prod = function(df){
  
  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-df$prob.1[n],df$prob.1[n]))
  }
  return(coef(y))
}



# Clean location data - subsidiary functions ---------------------------------



 # Function to split the location string into elements
# split HL7 string into locations
split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# clean room data
 # function removes bay and chair numbers

clean_room_names <- function(department, room) {
  if (department == "ED" && !is.na(room)) {
    room = gsub("UCHED ", "", room)
    room = gsub("UCH ED ", "", room)
    room = gsub("UCH ", "", room)
    room = gsub("^ED ","",room)  
    room = gsub("CHAIR [0-9]{2}", "", room)
    room = gsub("[0-9]{3}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("MAJ-CHAIR","MAJORS CH",room)
    room = gsub("MAJCH","MAJORS CH",room)
    room = gsub("SPECIALTY ASSESSMENT AREA","SAA",room)
    room = gsub("RAT-CHAIR","RAT",room)
    room = gsub("RATBED","RAT",room)
    room = gsub("SDEC POST-TRIAGE WAITING AREA","SDEC Waiting/TRIAGE",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
    room = gsub("^ ","",room)  
    room = gsub("OTF POOL","OTF",room)  
    room = gsub("CONS", "", room)
    # room = gsub(" ","_",room)  
  }
  else if (grepl("UCHT00CDU",department)) {
    room = "CDU"
  }
  else if (department %in% c( "1020100166", "1020100170")) {
    if (grepl("TRIAGE", room) | grepl("null", room)) {
    room = "SDEC Waiting/TRIAGE"
    } else {
    room = "SDEC" }
  }
  return(room)
}
clean_room_names <- Vectorize(clean_room_names)

# function to group room names
# NB RAT COVID MAJORS could be both - here it is treated as MAJORS
group_room_names <- function(room) {
  if (room == "SDEC Waiting/TRIAGE") {
    room_ <- "SDEC Waiting"
  } else {
    room_ <- case_when(
      length(grep("OTF", room)) >0 ~ "OTF",
      length(grep("UTC", room)) >0 ~ "ED UTC",
      length(grep("MAJ", room)) >0 ~ "ED MAJORS",
      length(grep("RAT", room)) >0 ~ "ED RAT",
      length(grep("TRIAGE", room)) >0 ~ "ED TRIAGE",
      length(grep("SPECIALTY ASSESSMENT AREA", room)) >0 ~ "ED SAA",
      room == "SDEC Waiting/TRIAGE" ~ room,
      length(grep("SDEC", room)) >0 ~ "SDEC",
      room %in% c( "null", "WR POOL" , "WR_POOL") ~ "ED Waiting",
      TRUE ~ paste("ED", room))
  }

  return(room_)
}
group_room_names <- Vectorize(group_room_names)


# Clean location data - main function -------------------------------------


# This function cleans location data using above functions. when handling real-time
# it has to do some of the processing done by the scripts in the flow-mapping folder, 
# that cleaned the bed move data and handled anomalous cases location moves. 

# Some careful handling of a number of problem cases is needed
# - OTF (Off the Floor) - patients are marked as OTF when they are in transit out of
# ED to another location. However, until their arrival at the onward ward, they are still
# under the ED jurisdiction. Patients marked as OTF are treated as still being in their
# previous location
# - repeated rows in the same location. When a patient moves from one bay or bed in the same location
# to another, they get an additional location move record. We chose to treat these as the
# same location, so handle that here

clean_location_data <- function(bed_moves, real_time = TRUE) {
  
  setkey(bed_moves, csn)
  
  if(real_time) {
    # add processing that is needed for the real-time app only
    # for non real-time processing, this has already been done
    bed_moves <- bed_moves %>% mutate(department = split_location(location_string, 1))
    bed_moves <- bed_moves %>% mutate(room = split_location(location_string, 2))
    bed_moves <- bed_moves %>% 
      mutate(ED = case_when(department == "ED" | 
                              department == "UCHT00CDU" | 
                              department == "1020100166" |
                              department == "1020100170" ~ 1,
                            TRUE ~ 0))
    bed_moves <- bed_moves %>% mutate(room3 = clean_room_names(department, room))
    bed_moves <- bed_moves %>% mutate(room4 = group_room_names(room3))
  }
  

  bed_moves <- bed_moves %>% mutate(duration_row = difftime(discharge, admission, units = "hours"))
  
  # indicate whether row is OTF location
  bed_moves <- bed_moves %>% 
    mutate(OTF_row = case_when(room == "UCHED OTF POOL" | room == "SDEC OTF" ~ 1, TRUE ~ 0))
  
  # mark SDEC as a ED location
  bed_moves[department %in% c("1020100166", "1020100170") , department := "ED"]
  
  # identify ED rows
  bed_moves[, "ED" := if_else(department == "ED", 1, 0)]
  
  # update location for ED rows only 
  bed_moves <- bed_moves %>% 
    mutate(location = case_when(department == "ED" & room4 == "ED TRIAGE" ~ "ED Waiting",
                              TRUE ~ room4)) 

  # Delete moves rows where admission == discharge
  bed_moves <- bed_moves[admission != discharge | is.na(discharge)]
  
  # Deal with rogue rows with null discharge
  cols = c("csn","admission","discharge", "department", "location")
  leadcols = paste("lead", cols, sep="_")
  lagcols = paste("lag", cols, sep="_")
  bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
  bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
  
  # check - these rows now have matching lead admission and lag discharge
  bed_moves[is.na(discharge) & lead_csn == csn & lead_admission == lag_discharge]
  
  # for these we can simply delete the row - so long as either lead or lag location are same as rogue row
  
  bed_moves[is.na(discharge) & lead_csn == csn & lead_admission == lag_discharge & 
              (lead_location == location | lag_location == location), delete_row := TRUE]
  bed_moves = bed_moves[is.na(delete_row)]
  
  # for the rest, drop the row and assume the person stayed in the previous location
  bed_moves[is.na(discharge) & lead_csn == csn, delete_row := TRUE ]
  bed_moves = bed_moves[is.na(delete_row)]
  bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
  bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
  bed_moves[csn == lag_csn & admission != lag_discharge, admission := lag_discharge]
  
  # Deal with OTF rows for non-real-time data
  
  if (!real_time) {
    
    # remove rows where OTF and set location to be same as previous
    # this is because we don't make real-time predictions for patients in real-time locations
    # but in historic data patients can move in and out of OTF locations

    bed_moves[, "otf" := case_when(location == "OTF" ~ 1, 
                               TRUE ~ 0)]
    bed_moves[, num_OTF := sum(otf), by = csn]
    
    # reset lead and lag values
    bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
    bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
    
    # find rows where OTF is last row
    bed_moves[csn == lag_csn & location == "OTF" & csn != lead_csn, otf_row_to_drop_last_row := TRUE]
    # drop these last rows
    bed_moves <- bed_moves[is.na(otf_row_to_drop_last_row)]
    
    # reset lead and lag values
    bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
    bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]

    # find rows where OTF is first row - check bed_moves[csn == "1022475707"]
    bed_moves[csn != lag_csn & location == "OTF" & csn == lead_csn, otf_row_to_drop_first_row := TRUE]
    # drop these first rows
    bed_moves <- bed_moves[is.na(otf_row_to_drop_first_row)]
    
    # reset lead and lag values
    bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
    bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
    
    # find other rows with OTF 
    bed_moves[csn == lag_csn & location == "OTF", otf_row_to_drop := TRUE]

    # remove OTF rows
    bed_moves <- bed_moves[is.na(otf_row_to_drop)]
    
    # reset lead and lag values
    bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
    bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
    
    
  }

  # Deal with repeated locations 
  # update with next row's discharge time when locations are repeated

  
  # first deal with cases where the first row is to be dropped
  # the lag method won't work for these to amend the admission date of the remaining row
  
  bed_moves[, min_admission := min(admission), by = csn]
  bed_moves[csn == lead_csn & location == lead_location & 
              admission == min_admission, drop_row := TRUE]
  
  # need to do this recursively
  while(nrow(bed_moves[(drop_row)]) > 0) {
    
    bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
    bed_moves[lag(drop_row) & csn == lag_csn, admission := lag_admission]
    bed_moves <- bed_moves[is.na(drop_row)]
    
    # reset lead and lag cols
    bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
    bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
    
    bed_moves[csn == lead_csn & location == lead_location & 
                admission == min_admission, drop_row := TRUE]
  }

  bed_moves[, min_admission := NULL]
  
  
  # then deal with other cases to remove rows where location is repeated
  bed_moves[csn == lead_csn & location == lead_location, drop_row := TRUE]
  bed_moves <- bed_moves[is.na(drop_row)]
  
  # update admission date of the remaining row - this time the lag method will work
  bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
  bed_moves[csn == lag_csn & admission != lag_discharge, amend_row := TRUE]
  
  # This statement below will reset the admission date to last discharge
  # thereby catering for repeated drop_rows
  bed_moves[csn == lag_csn & admission != lag_discharge, admission := lag_discharge]
  
  # remove any lag and lead rows for avoidance of error
  set(bed_moves, NULL , c("drop_row", "amend_row", "delete_row",
                      # "lag_csn", # removing for now to check logic in get_prior_adm
                      "lag_location", "lag_department", "lag_admission", "lag_discharge",  
                      "lead_csn", "lead_location", "lead_department", "lead_admission", "lead_discharge"), NULL)
  
  if (!real_time) {
    
    # reset lead and lag values
    bed_moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
    bed_moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
    
    # redo first and last row checks in case there were repeated OTF rows at beginning or end
    # find rows where OTF is last row
    bed_moves[csn == lag_csn & location == "OTF" & csn != lead_csn, otf_row_to_drop_last_row := TRUE]
    # rpt(bed_moves[(otf_row_to_drop_last_row)]) # this is number of csns where a row will be dropped
    bed_moves <- bed_moves[is.na(otf_row_to_drop_last_row)]
    
    # find rows where OTF is first row - check bed_moves[csn == "1022475707"]
    bed_moves[csn != lag_csn & location == "OTF" & csn == lead_csn, otf_row_to_drop_first_row := TRUE]
    # rpt(bed_moves[(otf_row_to_drop_first_row)]) # this is number of csns where a row will be dropped
    bed_moves <- bed_moves[is.na(otf_row_to_drop_first_row)]
    # rpt(bed_moves) 
    
  }
  
  
  # get first ED rows
  bed_moves[ED == 1, first_ED := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
  first_ED_ = unique(bed_moves[(first_ED), list(csn, admission)])
  setnames(first_ED_, "admission", "first_ED_admission")
  bed_moves = merge(bed_moves, first_ED_, all.x = TRUE)
  rm(first_ED_)
  
  bed_moves[, "outside" := department != "ED"]
  
  return(bed_moves)
  
}



# Clean obs data -----------------------------------------------------------

# This function cleans the observations data.
clean_obs_data = function(obs, summ, vo_mapping, real_time = TRUE) {
  
  if (real_time) {

    # merge the real-time data with summ dataset to calculate time the observation
    # was taken since arrival
    obs_real <- data.table(obs)
    setkey(obs_real, csn)
    obs_real <- merge(obs_real, summ[,.(csn, first_ED_admission)]) 
    
  } else {

    # historical data has already been merged with summ
    # this was done in order to filter out obs taking place after ED

    # Additional observations not currently used by the real-time application
    # but which are used by the retrospective models reported in the paper
    vo_mapping = bind_rows(vo_mapping,
                           data.table(`Friendly name` = c("Manchester Triage Score", "Subjective Pain Score", "Objective Pain Score"),
                                      `epic id` = c(1600100999, 34562, 34563)))
    

    obs_real = obs
  }

  
  # add elapsed time since arrival at ED
  obs_real[, elapsed_mins := as.numeric(difftime(observation_datetime, first_ED_admission, units = "mins"))]
  
  # remove obs from prior to ED by more than 2 hours
  obs_real <- obs_real[elapsed_mins >= -120]
  
  # vo_mapping is a static file that relates the data retrieved from EMAP to user friendly names
  vo_mapping = vo_mapping[,.(`Friendly name`, `epic id`)]
  setnames(vo_mapping, "Friendly name", "obs_name")
  setnames(vo_mapping, "epic id", "id_in_application")
  # # some observations have multiple mappings, so handle this
  # vo_mapping[,.N, by = id_in_application][N>1]
  vo_mapping = unique(vo_mapping[, obs_name := max(obs_name), by = id_in_application])
  vo_mapping[, obs_name := gsub(" ", "", obs_name)]
  
  # add mapped names to obs data
  obs_real[, id_in_application := as.numeric(id_in_application)]
  obs_real = merge(obs_real, vo_mapping, by = "id_in_application", allow.cartesian=TRUE, all.x = TRUE)
  
  # remove rows where there is no obs_name - ie not in mapping
  obs_real = obs_real[(!is.na(obs_name))]
  
  # remove reported symptoms as this is free text and not widely used (only 1825 csns from Jan 2020 to end Feb 2021)
  obs_real = obs_real[obs_name != "Reportedsymptomsonadmission"]
  
  if (!real_time) {
    
    # remove observations that are used less than 750 times in ED through the whole study period
    # this is to make a more manageable number of obs variables
    obs_real[, times_used := .N, by = obs_name]
    obs_real = obs_real[times_used > 750]
  }

  
  # Temperature measured in both celcius and farenheit
  # Note that making a manual conversion will generate a different values to the original temp value
  obs_real[obs_name == "Temperature", value_as_real := (value_as_real - 32) * 5/9]
  obs_real[obs_name %in% c("Temperature", "Temp(inCelsius)"), num_temp_in_dttm := .N, by = .(csn, observation_datetime)]
  obs_real[num_temp_in_dttm == 2 & obs_name == "Temperature", delete_row := TRUE]
  obs_real = obs_real[is.na(delete_row)]
  obs_real[obs_name == "Temperature", obs_name := "Temp(inCelsius)"]
  
  # remove duplicate measurements (there are still a bunch with multiple temp measurements in one obs event)
  obs_real[obs_name %in% c("Temperature", "Temp(inCelsius)"), num_temp_in_dttm := .N, by = .(csn, observation_datetime)]
  obs_real[num_temp_in_dttm > 1]
  
  # first delete cols id_in_application and visit_observation_type_id as these have diff values for celcius and f meas
  obs_real[, id_in_application := NULL]
  obs_real[, visit_observation_type_id := NULL]
  obs_real = unique(obs_real)
  

  # convert ACVPU to numeric
  obs_real[, value_as_real := case_when(obs_name == "ACVPU" & value_as_text == "A" ~ 1,
                                          obs_name == "ACVPU" & value_as_text == "C" ~ 2,
                                          obs_name == "ACVPU" & value_as_text == "V" ~ 3,
                                          obs_name == "ACVPU" & value_as_text == "P" ~ 4,
                                          obs_name == "ACVPU" & value_as_text == "U" ~ 5,
                                          TRUE ~ value_as_real
    )]

  
  # convert Bloodpressure to numeric
  
  Bloodpressure <- as_tibble(obs_real[obs_name == "Bloodpressure"]) %>% select(-value_as_real, -obs_name) %>% 
    separate(value_as_text, into = c("Bloodpressure_sys","Bloodpressure_dia"), sep = "/") %>% 
    mutate(Bloodpressure_sys = as.numeric(Bloodpressure_sys),
           Bloodpressure_dia = as.numeric(Bloodpressure_dia)) %>% 
    pivot_longer(Bloodpressure_sys:Bloodpressure_dia, names_to = "obs_name", values_to = "value_as_real"
    )
  
  Bloodpressure <- Bloodpressure %>% mutate(value_as_text = NA)
  
  obs_real <- bind_rows(obs_real[obs_name != "Bloodpressure"], Bloodpressure)
  
  # convert resp assist type
  
  obs_real[, value_as_real := case_when(obs_name == "Roomairoroxygen" & value_as_text == "Supplemental Oxygen" ~ 1,
                                        obs_name == "Roomairoroxygen" & value_as_text == "Room air" ~ 0,
                                        TRUE ~ value_as_real)]
  
  
  if (!real_time) {
    
    # convert pain score to factors
    obs_real[value_as_text == "Severe\\Very Severe", value_as_text := "Severe_VerySevere"]
    
    obs_real[, value_as_real := case_when(obs_name %in% c( "SubjectivePainScore", "ObjectivePainScore" ) & value_as_text == "Nil" ~ 1,
                                          obs_name %in% c( "SubjectivePainScore", "ObjectivePainScore" ) & value_as_text == "Mild" ~ 2,
                                          obs_name %in% c( "SubjectivePainScore", "ObjectivePainScore" ) & value_as_text == "Moderate" ~ 3,
                                          obs_name %in% c( "SubjectivePainScore", "ObjectivePainScore" ) & value_as_text == "Severe_VerySevere" ~ 4,
                                          TRUE ~ value_as_real
    )]
    
    obs_real[obs_name == "ManchesterTriageScore", value_as_real := as.numeric(value_as_text)]
  
  }
  

  
  # convert text to numeric where straightfoward
  obs_real[obs_name == "NEWSscore", value_as_real := as.numeric(value_as_text)]
  obs_real[obs_name == "NEWS2score", value_as_real := as.numeric(value_as_text)]
  obs_real[obs_name == "RASS", value_as_real := as.numeric(value_as_text)]
  obs_real[obs_name == "Painscore-verbalatrest", value_as_real := as.numeric(value_as_text)]
  obs_real[obs_name == "Painscore-verbalonmovement", value_as_real := as.numeric(value_as_text)]
  
  # remove outliers
  obs_real <- obs_real %>%
    mutate(value_as_real = case_when(value_as_real >46 & obs_name == "Temp(inCelsius)" ~ NA_real_,
                                     TRUE ~ value_as_real))
  
  # create final dataset of results (real values)
  obs_real <- obs_real[, .(csn, observation_datetime, value_as_real, obs_name, elapsed_mins)]
  
  # remove any punctuation that will make column names problematic
  obs_real[, obs_name := gsub("\\(|\\)|\\-|\\>|\\?|\\/","", obs_name)]
  obs_real[, obs_name := gsub("Currenttemperature>37.5orhistoryoffeverinthelast24hours","Fever", obs_name)]
  
}


# Clean lab data ----------------------------------------------------------
# This function cleans the pathology data.

clean_lab_data = function(lab_orders, lab_results, lab_orders_to_include, summ, real_time = TRUE) {
  
  if (real_time) {

#   merge with summ dataset to get time since arrival
    setkey(lab_orders, csn)
    lab_orders_real <- merge(lab_orders, summ[,.(csn, first_ED_admission)], by = "csn") 
    
    setkey(lab_results, csn)
    lab_results_real <- merge(lab_results, summ[,.(csn, first_ED_admission)]) 
    
  } else {

  # this step has arleady been done for the historical data
    
    lab_orders_real = lab_orders
    lab_results_real = lab_results
  }
  

  # add elapsed time
  lab_orders_real[, elapsed_mins := as.numeric(difftime(request_datetime, first_ED_admission, units = "mins"))]
  lab_results_real[, elapsed_mins := as.numeric(difftime(result_last_modified_time, first_ED_admission, units = "mins"))] 
  
  # remove obs from prior to ED by more than 2 hours
  lab_orders_real <- lab_orders_real[elapsed_mins >= -120]
  lab_results_real <- lab_results_real[elapsed_mins >= -120]
  
  # create out of range values
  lab_results_real <- lab_results_real %>% 
    mutate(oor_low = value_as_real < range_low,
           oor_high = value_as_real > range_high,
           abnormal = abnormal_flag == "A")
  
  
  # remove lab battery orders not included in ML model
  
  if (real_time) {
    
    lab_orders_real = lab_orders_real[battery_code %in% lab_orders_to_include]
  }


  
  return(list(lab_orders_real, lab_results_real))
  
}


# Clean consults data ----------------------------------------------------------
 # This function cleans the consults data (currenly only used in the historical data, not in real-time)
clean_consults_data = function(consults, summ, real_time = TRUE) {
  
  # add elapsed time
  consults[, elapsed_mins := as.numeric(difftime(scheduled_datetime, first_ED_admission, units = "mins"))]

  # remove obs from prior to ED by more than 2 hours
  consults <- consults[elapsed_mins >= -120]
  
  # remove lab battery orders not included in ML model
  
  return(consults)
  
}



# Create design matrix for ML ---------------------------------------------

# This function summarises visit information for input into ML models

prep_summ_for_ML <- function(summ, time_of_extract, real_time = TRUE) {
  
  if (real_time) {
    
    dm <- summ[,.(csn, age, sex, presentation_time, arrival_method, first_ED_admission, patient_class,
                  num_prior_adm_after_ED = num_adm_after_ED, num_prior_ED_visits = num_ED_visits)]

#     temporal information
    dm[, tod := factor((hour(presentation_time) %/% 4)+1)]
    dm[, night := factor(ifelse(hour(presentation_time) < 22 & hour(presentation_time) > 7, 0, 1))]
    dm[, weekend := factor(if_else(weekdays(presentation_time, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]
    dm[, quarter := factor(case_when( month(presentation_time) <= 3 ~ 1,
                                      month(presentation_time) <= 6 ~ 2, 
                                      month(presentation_time) <= 9 ~ 3, 
                                      month(presentation_time) <= 12 ~ 4))]
#     age information
    dm[, gt70 := factor(age >= 70)]
    dm[, sex := factor(sex)]
    
    
  } else {

    # if not real-time we need to know when the visit ended so that we only include the visit
    # in relevant timeslices
    dm <- summ[,.(csn, age, sex, presentation_time, arrival_method, first_ED_admission, patient_class,
                  num_prior_adm_after_ED = num_adm_after_ED, num_prior_ED_visits = num_ED_visits,
                  left_ED, first_outside_proper_admission, min_I)]
    
    dm[, age_gt70 := age >= 70]
    dm[, female := sex == "F"]
    dm[, other_gender := !sex %in% c( "F", "M")]
    dm[, sex := NULL]
    
    dm[, weekend := weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat")]
    dm[, quarter_of_year := factor(case_when( month(first_ED_admission) <= 3 ~ 1,
                                      month(first_ED_admission) <= 6 ~ 2, 
                                      month(first_ED_admission) <= 9 ~ 3, 
                                      month(first_ED_admission) <= 12 ~ 4))]

    # add arrival window (which is used in the retrospective models only )
    dm[, arrival_hr := as.integer(substr(first_ED_admission, 12,13))]
    
    dm[, arrival_window := factor(case_when(arrival_hr > 21 | arrival_hr < 6 ~ "2200-0600",
                                              arrival_hr < 12 ~ "0600-1200",
                                              arrival_hr > 16 ~ "1200-1600",
                                              arrival_hr < 22 ~ "1600-2200"))]
    
    dm[, c("arrival_hr") := NULL]
    
    
    
  }
  


  
  if (real_time) {
    
    dm[, inpatient := if_else(patient_class == "INPATIENT", 1, 0)]
    dm[, inpatient := factor(inpatient)]
  

  } else {
    
    # was an inpatient, as part of this visit, before ED
    dm[, inpatient_prior_to_ED := min_I < first_ED_admission]
    dm[, inpatient_prior_to_ED := if_else(is.na(inpatient_prior_to_ED), FALSE, inpatient_prior_to_ED)]
    # dm[, inpatient := factor(inpatient)]
    dm[, min_I := NULL]
  }
  
  dm[, patient_class := NULL]
  
  # include a feature to capture time delay between presentation and arrival at ED;
  dm[, time_from_pres_to_ED_arrival := as.numeric(difftime(first_ED_admission, presentation_time, units = "mins"))]
  # small number have negative time for before ED
  dm[, time_from_pres_to_ED_arrival := if_else(time_from_pres_to_ED_arrival <0, 0, time_from_pres_to_ED_arrival) ]


  
  if (real_time) {
    
    dm[, duration := as.numeric(difftime(time_of_extract, first_ED_admission, units = "mins"))]
    
    
    # simplify arrival method
    dm[, arrival := gsub(" |-", "", arrival_method)]
    dm[, arrival := factor(if_else(!arrival %in% c("Ambulance",
                                                   "Walkin",
                                                   "PublicTrans",
                                                   "Ambnomedic"), "Other", arrival)) ]
    dm[, arrival_method := NULL]

  } else {
    
    dm[, duration := as.numeric(difftime(left_ED, first_ED_admission, units = "mins"))]  
    
    
    # simplify arrival method
    dm[, arrival := gsub(" |-", "", arrival_method)]
    dm[, arrival := factor(if_else(!arrival %in% c("Ambulance",
                                                   "Walkin",
                                                   "PublicTrans",
                                                   "Ambnomedic"), "Other", arrival)) ]
    dm[, arrival_method := NULL]
    setnames(dm, "arrival", "arrival_method")
  }
  

  

  setkey(dm, csn)
  
  return(dm)
  
}


# Summarise location information  -----------------------------------------

# summarise visit information at location level for input into ML models

prep_moves_for_ML <- function(moves, dm, real_time = TRUE) {
  # note this seems to return both dm and loc
  
  if (real_time) {
    
    loc <- moves[csn %in% dm$csn, .(csn, admission, discharge, location, outside)]
    loc <- merge(loc, dm[,.(csn, first_ED_admission, duration)])
    
  } else {

    # if not real-time data, we need to know when each visit ended (ie the patient left ED)
    loc <- moves[csn %in% dm$csn, .(csn, admission, discharge, location, outside, first_ED_admission, first_outside_proper_admission)]
    loc <- merge(loc, dm[,.(csn, duration)])
    
  }
  
  
  # remove rows where admission occurs to locations before arrival at ED
  loc <- loc[admission >= first_ED_admission]
  
  if (!real_time) {
    
    # remove rows after ED
    loc <- loc[discharge <= first_outside_proper_admission | is.na(first_outside_proper_admission)]
  }
  
  
  loc[, admission_e := as.numeric(difftime(admission, first_ED_admission, units = "mins"))]
  loc[, discharge_e := as.numeric(difftime(discharge, first_ED_admission, units = "mins"))]
  
  
  if (real_time) {
    
    loc[, c("admission", "discharge", "first_ED_admission") := NULL]
    
    dm[, c("presentation_time", "first_ED_admission") := NULL]
    
    cols = colnames(copy(dm)[, c("csn","duration") := NULL])
    cols_ = paste("a", cols, sep="_")
    setnames(dm, cols, cols_)
    
    setkey(loc, csn)
  } else {
    
    loc[, c("admission", "discharge", "first_ED_admission", "first_outside_proper_admission") := NULL]
    
    setkey(loc, csn)
    
    return(loc)
  }
  
  

}


# Create timeslices -------------------------------------------------------


# # function to create timeslices from the main design matrix
# This takes as inputs all of the cleaned datasets mentioned above which only have data from during the ED visit,
# loc - cleaned location data, one row per location (note that locations have been grouped as decribed above)
# dm - summary at visit level, including age, arrival method, prior visit history
# obs_real - summary of obs, with elapsed time since arrival of each obs
# lab_orders_real - summary of lab orders requested with elapsed time since arrival
# lab_results_real - summary of lab results, with elapsed time since arrival
# consults_real - summary of consults data
# cutoff - the cutoff time for start of the timeslice (eg 60 min, 90 min after ED arrival) - only
# patients who have been in longer than this will be included
# nextcutoff - the cutoff time for the end of the timeslice
# single_timeslice - used at one point to compare the timeslices approach with using only a single timeslice;
# not used in the final analysis

create_timeslice <- function(loc, dm, obs_real, lab_orders_real, lab_results_real, consults_real = NA, cutoff, nextcutoff, real_time = TRUE,
                              single_timeslice = FALSE) {
  
  if (real_time) {
    
    loc_cutoff <- loc[duration > cutoff & duration <= nextcutoff]
    
  } else if (single_timeslice) {
    
    loc_cutoff <- loc[admission_e <= nextcutoff]
    
  }
    
  else {
    
    # select only patients with duration longer than cutoff 
    loc_cutoff <- loc[duration > cutoff]
    
    # select locations that began before the cutoff (up to midpoint before next cutoff)
    loc_cutoff <- loc_cutoff[admission_e <= cutoff + (nextcutoff - cutoff)/2]
  }
  
  # count number of location rows up to ED - note will include any pre- ED locations
  loc_count <- loc_cutoff[, .N, by = csn]
  setnames(loc_count, "N", "l_num_locations")
  
  # get current location

  if (real_time) {
    
    loc_count <- merge(loc_count, loc_cutoff[is.na(discharge_e), location, by = csn],
                       all.x = TRUE)
  } else if (single_timeslice) {
    
    loc_count <- merge(loc_count, loc_cutoff[(admission_e <= nextcutoff & discharge_e > nextcutoff), location, by = csn],
                       all.x = TRUE)
    
  } else {
    
    loc_count <- merge(loc_count, loc_cutoff[(admission_e <= cutoff & discharge_e > cutoff), location, by = csn],
                       all.x = TRUE)
  }
  
  setnames(loc_count, "location", "l_current")
  
  if (!single_timeslice) {
    loc_count[, l_current := factor(l_current)]
  }

  # loc_cutoff_csn summarises the location data at the visit level using a pivot_wider function
  if (nrow(loc_cutoff) > 0) {
    loc_cutoff_csn <- merge(loc_count, data.table(
      loc_cutoff[(!outside), .N > 0, by = .(csn, location)]  %>% 
        pivot_wider(names_from = location, names_prefix = "l_visited_", values_from = V1, values_fill = 0)
    ), all.x = TRUE)
  } else {
    loc_cutoff_csn = loc_cutoff
    setnames(loc_cutoff_csn, "location", "l_num_locations")
  }

  # rename CDU
  
  if (sum(grepl("UCHT00CDU", colnames(loc))) > 0) {
    setnames(loc_cutoff_csn, "num_UCHT00CDU", "num_CDU")
  }
  
  # observation data - this will create counts of all observation data prior to cutoff + margin
  # select for cutoff
  
  if (real_time) {
    
    obs_cutoff <- obs_real[duration > cutoff & duration <= nextcutoff]
    
  } else if (single_timeslice) {
    
    obs_cutoff <- obs_real[elapsed_mins <= nextcutoff]
    
  } else {
    
    # select for cutoff
    obs_cutoff <- obs_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  }
  
  # obs_cutoff_csn summarises the obs data at the visit level using a summarising function
  # within the data.table library

  if (nrow(obs_cutoff) > 0) {
    
    if (real_time) {
      
      # add number of observation measurements up to cutoff
      obs_cutoff[, o_num_meas := .N, by = csn]
      
      # add number of types of results by csn
      obs_cutoff_csn <- merge(unique(obs_cutoff[, .(csn, o_num_meas)]), 
                              obs_cutoff[, .(o_num_types = uniqueN(obs_name)), by = csn], by = "csn", all.x = TRUE)
      
      # add number of observation events per csn
      obs_cutoff_csn <- merge(obs_cutoff_csn, obs_cutoff[, .(o_num_events = uniqueN(elapsed_mins)), by = csn], all.x = TRUE)
      
      # blood pressure has two measurements per event, so delete one type
      obs_cutoff_csn[, o_num_types := o_num_types -1]
      # obs_cutoff_csn[, o_num := o_num - o_num_Bloodpressure_sys]
      obs_cutoff_csn[, o_has := 1] # this will be 1 for all csns currently; zeros added later
      
    } else {
      
      # add number of observation measurements up to cutoff
      obs_cutoff[, o_num_obs := .N, by = csn]
      
      # add number of types of results by csn
      obs_cutoff_csn <- merge(unique(obs_cutoff[, .(csn, o_num_obs)]), 
                              obs_cutoff[, .(o_num_obs_type = uniqueN(obs_name)), by = csn], by = "csn", all.x = TRUE)
      
      # add number of observation events per csn
      obs_cutoff_csn <- merge(obs_cutoff_csn, obs_cutoff[, .(o_num_obs_events = uniqueN(elapsed_mins)), by = csn], all.x = TRUE)
      
      # blood pressure has two measurements per event, so delete one type
      obs_cutoff_csn[, o_num_obs_type := o_num_obs_type -1]
      # obs_cutoff_csn[, o_num := o_num - o_num_Bloodpressure_sys]
      obs_cutoff_csn[, o_has_obs := 1] # this will be 1 for all csns currently; zeros added later
    }

    
    
    # add count of times when O2 sat dropped below 90 or 95
    
    sat_lt90 <- obs_cutoff[obs_name == "Oxygensaturation" & value_as_real < 90, .N, by = .(csn)]
    setnames(sat_lt90, "N", "o_num_o2sat_lt90")
    sat_lt95 <- obs_cutoff[obs_name == "Oxygensaturation" & value_as_real < 95, .N, by = .(csn)]
    setnames(sat_lt95, "N", "o_num_o2sat_lt95")
    
    obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt90, all.x = TRUE, by = "csn")
    obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt95, all.x = TRUE, by = "csn")
    
    # add count of times when news score was medium or high
    
    news_med <- obs_cutoff[obs_name == "NEWSscore" & value_as_real < 7 & value_as_real > 4, .N, by = .(csn)] 
    setnames(news_med, "N", "o_num_news_med")
    news_high <- obs_cutoff[obs_name == "NEWSscore" & value_as_real >= 7, .N, by = .(csn)] 
    setnames(news_high, "N", "o_num_news_high")
    
    obs_cutoff_csn <- merge(obs_cutoff_csn, news_med, all.x = TRUE, by = "csn")
    obs_cutoff_csn <- merge(obs_cutoff_csn, news_high, all.x = TRUE, by = "csn")
    
    # add count of times GCS <= 8
    GCS_lt9 <- obs_cutoff[obs_name == "GCStotal" & value_as_real < 9, .N, by = .(csn)] 
    setnames(GCS_lt9, "N", "o_num_GCS_lt9")
    
    obs_cutoff_csn <- merge(obs_cutoff_csn, GCS_lt9, all.x = TRUE, by = "csn")
    
    # generate counts of each observation by csn
    if (real_time) {
      obs_cutoff_csn_w <- obs_cutoff[, .N, by = .(csn, obs_name)] %>%
        pivot_wider(names_from = obs_name, names_prefix = "o_num_", values_from = N, values_fill = 0)
      
    } else {
      obs_cutoff_csn_w <- obs_cutoff[, .N, by = .(csn, obs_name)] %>%
        pivot_wider(names_from = obs_name, names_prefix = "o_num_", values_from = N, values_fill = 0)
    }

    
    if (sum(grepl("o_num_Bloodpressure_dia", colnames(obs_cutoff_csn_w))) > 0) {
      obs_cutoff_csn_w <- obs_cutoff_csn_w %>% select(-o_num_Bloodpressure_dia)
      obs_cutoff_csn_w <- obs_cutoff_csn_w %>% rename(o_num_Bloodpressure = o_num_Bloodpressure_sys)
    }
    
    obs_cutoff_csn <- data.table(merge(obs_cutoff_csn, obs_cutoff_csn_w, all.x = TRUE))
    
    # so far we have counted the occurence of certain observations; now add valued obs data
    
    if (real_time) {
      
      if  (nrow(     obs_cutoff %>% 
                     # corrected error in nrow evaluation
                     filter(obs_name %in% c("TempinCelsius", "Heartrate","MAPnoninvasive", "Respiratoryrate", "FiO2" )) ) > 0) {
        
        obs_cutoff_csn_val <- data.table(
          obs_cutoff %>% 
            filter(obs_name %in% c("TempinCelsius", "Heartrate","MAPnoninvasive", "Respiratoryrate", "FiO2" )) %>% 
            group_by(csn, obs_name) %>%
            # using max allows for possibility of two measurements in same minute
            summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
            pivot_wider(names_from = obs_name, names_prefix = "o_latest_", values_from = latest_value)
        )
      }
      
      
    } else if (single_timeslice) {
      
      if  (nrow(     obs_cutoff %>% 
                     # corrected error in nrow evaluation
                     filter(obs_name %in% c("TempinCelsius", "Heartrate","MAPnoninvasive", "Respiratoryrate", "FiO2" )) ) > 0) {
        
        obs_cutoff_csn_val <- data.table(
          obs_cutoff %>% 
            filter(obs_name %in% c("TempinCelsius", "Heartrate","MAPnoninvasive", "Respiratoryrate", "FiO2" )) %>% 
            group_by(csn, obs_name) %>%
            filter(elapsed_mins == max(elapsed_mins), !is.na(value_as_real)) %>%
            # using max allows for possibility of two measurements in same minute
            summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
            pivot_wider(names_from = obs_name, names_prefix = "o_latest_", values_from = latest_value)
        )
      }


    } else {

   # obs_cutoff_csn_val summarises the valued obs data at the visit level
      
      obs_cutoff_csn_val <- data.table(
        obs_cutoff %>% 
          filter(obs_name %in% c("TempinCelsius", "Heartrate","MAPnoninvasive", "Respiratoryrate", "FiO2" ,
                                 "ManchesterTriageScore", "SubjectivePainScore", "ObjectivePainScore", "NEWSscore")) %>% 
          group_by(csn, obs_name) %>%
          filter(elapsed_mins == max(elapsed_mins), !is.na(value_as_real)) %>%
          # using max allows for possibility of two measurements in same minute
          summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
          pivot_wider(names_from = obs_name, names_prefix = "o_latest_", values_from = latest_value)
      )

    }

  }
  
  
  
  # 
  # add lab data
  
  if (real_time) {
    
    # select for cutoff
    lab_orders_cutoff <- lab_orders_real[duration > cutoff & duration <= nextcutoff]
    lab_results_cutoff <- lab_results_real[duration > cutoff & duration <= nextcutoff]
    
  } else if (single_timeslice) {
    
    lab_orders_cutoff <- lab_orders_real[elapsed_mins <= nextcutoff]
    lab_results_cutoff <- lab_results_real[elapsed_mins <= nextcutoff]
    
  } else {
    
    # select for cutoff
    lab_orders_cutoff <- lab_orders_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
    lab_results_cutoff <- lab_results_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  }
  
   # lab_cutoff_csn summarises the lab orders data at the visit level
   # lab_cutoff_csn_val summarises the lab results data at the visit level

  
  # add number of types of orders by csn
  
  if (nrow(lab_orders_cutoff) > 0) {
    lab_cutoff_csn <- lab_orders_cutoff[, .(p_num_battery = uniqueN(battery_code)), by = csn]
    
    # add whether each cluster was requested
    
    lab_cutoff_csn_battery = lab_orders_cutoff[, (N =.N > 0), by = .(csn, battery_code)] %>% 
      pivot_wider(names_from = battery_code, names_prefix = "p_req_battery_", values_from = V1, values_fill = 0)
    
    if (nrow(lab_results_cutoff) > 0) {
      # add number of lab results that are out of range high and low
      if (nrow(lab_results_cutoff[(oor_high), .(p_num_oor_high =.N), by = csn]) > 0) {
        lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(oor_high), .(p_num_oor_high =.N), by = csn], all.x = TRUE)
        
      }
      
      if (nrow(lab_results_cutoff[(oor_low), .(p_num_oor_low =.N), by = csn]) > 0) {
        lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(oor_low), .(p_num_oor_low =.N), by = csn], all.x = TRUE)
        
      }
      
      if (nrow(lab_results_cutoff[(abnormal), .(p_num_abnormal =.N), by = csn]) > 0) {
        lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(abnormal), .(p_num_abnormal =.N), by = csn], all.x = TRUE)
        
      }
      

      # if there are valued results from the following test lab codes, then summarise them
      if (nrow(lab_results_cutoff %>%
               filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC",
                                           "HTRT", "Lac", "pCO2", "HCO3", "pH", "ALB")))> 0) {
        # add number out of range results for each lab test in APACHE and those identified by ED clinicians
        lab_cutoff_csn_oor = data.table(lab_results_cutoff %>% 
                                          filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC",
                                                                      "HTRT", "Lac", "pCO2", "HCO3", "pH", "ALB")) %>% 
                                          group_by(csn, test_lab_code) %>%
                                          summarise(low = sum(oor_low), 
                                                    high = sum(oor_high)) %>%
                                          pivot_longer(low:high) %>% 
                                          mutate(test_lab_code = paste0(name, "_", test_lab_code)) %>% select(-name) %>% 
                                          pivot_wider(names_from = test_lab_code, names_prefix = "p_num_oor_", values_from = value)
        )
        
        if (nrow(lab_cutoff_csn_oor) > 0) {
          lab_cutoff_csn <- merge(lab_cutoff_csn, lab_cutoff_csn_oor, all.x = TRUE)
          
        }
        
        # add score for each lab test in APACHE (add other values from ED clinicians)
        
        if (real_time | single_timeslice) {

          # first check whether any one has two measurements in same minute
          lab_cutoff_csn_val_temp <- lab_results_cutoff %>%
            filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC",
                                        "HTRT", "Lac", "pCO2", "HCO3", "pH", "ALB")) %>% 
            group_by(csn, test_lab_code) 
          
          if (nrow(lab_cutoff_csn_val_temp %>% summarise(N = n()) %>% filter(N > 1)) > 0) {
            lab_cutoff_csn_val <- data.table(
              lab_cutoff_csn_val_temp %>% 
                filter(!is.na(value_as_real)) %>% 
                # using max allows for possibility of two measurements in same minute
                summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
                pivot_wider(names_from = test_lab_code, names_prefix = "p_latest_", values_from = latest_value))
          } else {
            lab_cutoff_csn_val <- data.table(
              lab_cutoff_csn_val_temp %>% 
                filter(!is.na(value_as_real))  %>%
                # using max will cause warnings if there are no values to maximise
                summarise(latest_value = value_as_real)  %>%
                pivot_wider(names_from = test_lab_code, names_prefix = "p_latest_", values_from = latest_value))
          }
          
        } else {
          
          lab_cutoff_csn_val <- data.table(
            lab_results_cutoff %>%
              filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC",
                                          "HTRT", "Lac", "pCO2", "HCO3", "pH", "ALB")) %>% 
              group_by(csn, test_lab_code) %>%
              filter(elapsed_mins == max(elapsed_mins), !is.na(value_as_real)) %>%
              # using max allows for possibility of two measurements in same minute
              summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
              pivot_wider(names_from = test_lab_code, names_prefix = "p_latest_", values_from = latest_value)
          )
        }
        

      }
      
    }
    lab_cutoff_csn <- data.table(merge(lab_cutoff_csn, lab_cutoff_csn_battery, all.x = TRUE))
  }
  
  # add consults 
  
  if (real_time) { # consults not currently used in real-time
    
    # consults_cutoff <- consults_real[duration > cutoff & duration <= nextcutoff]
    
  } else if (single_timeslice) {
    
    consults_cutoff <- consults_real[elapsed_mins <= nextcutoff]

  } else {
    
    # select for cutoff
    consults_cutoff <- consults_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  }
  
  
  if (!real_time) {
    
    if (nrow(consults_cutoff) > 0) {
      consults_cutoff_csn <- consults_cutoff[, .(a_num_consults = uniqueN(code)), by = csn]
      
      # # add whether each consult was requested
      # 
      # consults_cutoff_csn_code = consults_cutoff[, (N =.N > 0), by = .(csn, code)] %>% 
      #   pivot_wider(names_from = code, names_prefix = "c_req_cons_", values_from = V1, values_fill = 0)
      # 
    }
  }
  
  ## combine everything
  # just use csn from summ to start with - add the other summ fields (which may have genuine NAs) later
  
  if (real_time) {
    
    # each csn only appears in one timeslice
    matrix_cutoff <- merge(data.table(csn = dm[duration > cutoff & duration <= nextcutoff, csn]), loc_cutoff_csn, all.x = TRUE, by = "csn")
    
  } else if (single_timeslice) {
    
    matrix_cutoff <- merge(data.table(csn = dm[, csn]), loc_cutoff_csn, all.x = TRUE, by = "csn")
    
  } else {
    
    # each csn appears in as many timeslices as relevant
    matrix_cutoff <- merge(data.table(csn = dm[duration > cutoff, csn]), loc_cutoff_csn, all.x = TRUE, by = "csn")
  }
  
  if (exists("obs_cutoff_csn") > 0) {
    matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn, by = "csn", all.x = TRUE)
    
  }
  
  if (exists("lab_cutoff_csn") > 0) {
    matrix_cutoff <- merge(matrix_cutoff, lab_cutoff_csn, by = "csn", all.x = TRUE)
    
  }
  
  if (exists("consults_cutoff_csn") > 0) {
    matrix_cutoff <- merge(matrix_cutoff, consults_cutoff_csn, by = "csn", all.x = TRUE)
    
  }
  
  # replace all counts with NA as 0
  # from https://stackoverflow.com/questions/37391573/replace-na-with-0-only-in-numeric-columns-in-data-table
  
  
    for(j in seq_along(matrix_cutoff)){
      set(matrix_cutoff, i = which(is.na(matrix_cutoff[[j]])), j = j, value = 0)
  }
  

  
  
  if (real_time) {
    
    # add whether in covid surge (currently hard coded)
    matrix_cutoff[, covid_surge := FALSE]
  } 
  
  # add other info where there may be genuine NAs
  
  if (single_timeslice) {
    
    matrix_cutoff <- merge(matrix_cutoff, dm, by = c("csn"))
    
    
  } else {
    
    matrix_cutoff <- merge(matrix_cutoff, dm[duration > cutoff], by = c("csn"))
    
  }

  if (exists("obs_cutoff_csn_val") > 0) {
    matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn_val, by = "csn", all.x = TRUE) 
  }
  
  if (exists("lab_cutoff_csn_val")) {

      matrix_cutoff <- merge(matrix_cutoff, lab_cutoff_csn_val, by = "csn", all.x = TRUE)

  }
  
  if (!real_time & !single_timeslice) {
    
    matrix_cutoff[, duration := NULL]
    
    
    matrix_cutoff[is.na(o_latest_ManchesterTriageScore), o_latest_ManchesterTriageScore := 999]
    matrix_cutoff[is.na(o_latest_ObjectivePainScore), o_latest_ObjectivePainScore := 999]
    matrix_cutoff[is.na(o_latest_SubjectivePainScore), o_latest_SubjectivePainScore := 999]
    
    
    matrix_cutoff[, o_latest_ManchesterTriageScore := factor(o_latest_ManchesterTriageScore, 
                                                                  levels = c(1,2,3,4,5, 999),
                                                                  labels = c("Red", "Orange", "Yellow", "Green", "Blue", "Not recorded"))]
    matrix_cutoff[, o_latest_ObjectivePainScore := factor(o_latest_ObjectivePainScore, 
                                                               levels = c(1,2,3,4, 999),
                                                               labels= c("Nil", "Mild", "Moderate", "Severe", "Not recorded"))]
    matrix_cutoff[, o_latest_SubjectivePainScore := factor(o_latest_SubjectivePainScore, 
                                                                levels = c(1,2,3,4, 999),
                                                                labels= c("Nil", "Mild", "Moderate", "Severe", "Not recorded"))]
  }
  
  return(matrix_cutoff)
  
  
}


# One-hot encoding --------------------------------------------------------
# This function does the one hot encoding for ML


one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=FALSE){
  
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
  if(length(cols) == 0) {
    return(dt) }
  else {
    
    # Build tempDT containing and ID column and 'cols' columns
    tempDT <- dt[, cols, with=FALSE]
    tempDT[, ID := .I]
    setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
    for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
    
    # One-hot-encode
    if(dropUnusedLevels == TRUE){
      newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = T, fun = length)
    } else{
      newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = F, fun = length)
    }
    
    # Combine binarized columns with the original dataset
    result <- cbind(dt, newCols[, !"ID"])
    
    # If dropCols = TRUE, remove the original factor columns
    if(dropCols == TRUE){
      result <- result[, !cols, with=FALSE]
    }
    
    # in case col names have characters that MLR3 doesn't like
    colnames(result) <- make.names(colnames(result), unique = TRUE) 
      
    return(result)
  }
}

# # Calculate coefficient to stretch or squeeze base survival curve -----------------------------
#  This function used in the Cox regression stage. It takes as input
# - in_ED_all - the visits upon which to do the calculation
# - num_in_ED - the dataset of how many people were in ED when each patient arrived
# - the Cox coefficients that were calculated during model training
# - scale_params_ - the scaling parameters used in the Cox
# - use_six_week_rolling - whether to use only the last 6 weeks data in the Cox regression
# 
# and returns as an output the in_ED_all dataset, with the Cox coefficient saved
# 


get_cox_coef_by_csn = function(in_ED_all, coefs, real_time = TRUE, num_in_ED = NA, model_period_ , scale_params_, 
                               use_test_set_for_Cox_SDEC = FALSE, use_six_week_rolling = FALSE) {
  
  if (!use_six_week_rolling) {
    
    # get quarter in which patient arrived
    in_ED_all[, quarter := factor(case_when( month(first_ED_admission) <= 3 ~ 1,
                                             month(first_ED_admission) <= 6 ~ 2, 
                                             month(first_ED_admission) <= 9 ~ 3, 
                                             month(first_ED_admission) <= 12 ~ 4), levels = c(1, 2, 3, 4))]
  }
  

  
  # get whether patient arrived on a weekend day
  in_ED_all[, weekend := factor(if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0), levels = c(0, 1))]
  
  # get patient's arrival window as a factor
  in_ED_all[, arrival_time := factor(case_when(hour(first_ED_admission) >= 22 | hour(first_ED_admission) < 6 ~ "22:00-06:00",
                                               hour(first_ED_admission) >= 6 & hour(first_ED_admission) < 12 ~ "06:00-12:00",
                                               hour(first_ED_admission) >= 12 & hour(first_ED_admission) < 16 ~ "12:00-16:00",
                                               hour(first_ED_admission) >= 16 & hour(first_ED_admission) < 22 ~ "16:00-22:00"),
                                     levels = c("06:00-12:00", "12:00-16:00", "16:00-22:00", "22:00-06:00"))]
  
  if (!(real_time)) {
    
    # merge to get numbers in ED at that time
    in_ED_all[, DateTime := as.POSIXct(substr(first_ED_admission, 1, 16))]
    
    if (!use_six_week_rolling) {
      
      in_ED_all = merge(in_ED_all, num_in_ED, by = c("DateTime", "quarter", "weekend", "arrival_time"))
      
    } else {
      
      in_ED_all = merge(in_ED_all, num_in_ED, by = c("DateTime", "weekend", "arrival_time"))
      
    }
    
    # now set weekend as numeric variable (needed to be factor to merge)
    in_ED_all[, weekend := if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]
    
    
    in_ED_all[, num_MAJORS_RESUS := `num_ED MAJORS` + `num_ED RESUS`]
    in_ED_all[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS]
    
    # add num SDEC for post-SDEC model only 
    in_ED_all[, num_SDEC := num_SDEC + `num_SDEC Waiting`]
  }
  
  
  # scale the variables for Cox regresion
  in_ED_all = in_ED_all[!is.na(num_elsewhere)]
  
  if (!use_six_week_rolling) {
    
    in_ED_all[, quarter_1 := as.numeric(quarter ==1)]
    in_ED_all[, quarter_2 := as.numeric(quarter ==2)]
    in_ED_all[, quarter_3 := as.numeric(quarter ==3)]
    in_ED_all[, quarter_4 := as.numeric(quarter ==4)]
    
  }
  

  in_ED_all[, weekend_1 := as.numeric(weekend ==1)]
  
  in_ED_all[, arrival_time_06_12 := as.numeric(arrival_time == "06:00-12:00")]
  in_ED_all[, arrival_time_12_16 := as.numeric(arrival_time == "12:00-16:00")]
  in_ED_all[, arrival_time_16_22 := as.numeric(arrival_time == "16:00-22:00")]
  in_ED_all[, arrival_time_22_06 := as.numeric(arrival_time == "22:00-06:00")]
  
  if (model_period_ == "Pre + Post") {
    
    in_ED_all[, post_Covid_1 := as.numeric(first_ED_admission > covid_start)]
    
    in_ED_all[, post_Covid_1 := (post_Covid_1 - scale_params_[name == "post_Covid_1", mean])/
              scale_params_[name == "post_Covid_1", sd]]
    
  } else if (model_period_ == "Post-SDEC" &
             # note that the real-time app assumes we are using the post-SDEC model
             # so the adjustment below has already been made
             ! real_time) { 
    
    in_ED_all[, num_elsewhere := tot_in_ED - num_MAJORS_RESUS - num_SDEC - `num_SDEC Waiting`]
    
  }
  
  if (!use_six_week_rolling) {
    
    # scale the variables for Cox regression
    # note that it should not matter if all of these are not used in the final cox predictions
    # because scale params were saved for them anyway
    
    in_ED_all[, num_MAJORS_RESUS_scaled := (num_MAJORS_RESUS - scale_params_[name == "num_MAJORS_RESUS", mean])/
                scale_params_[name == "num_MAJORS_RESUS", sd]]
    in_ED_all[, num_elsewhere_scaled := (num_elsewhere - scale_params_[name == "num_elsewhere", mean])/
                scale_params_[name == "num_elsewhere", sd]]
    
    if (model_period_ == "Post-SDEC") {
      in_ED_all[, num_SDEC_scaled := (num_SDEC - scale_params_[name == "num_SDEC", mean])/
                  scale_params_[name == "num_SDEC", sd]]
    }
    
    # since some training sets may not have all quarters, therefore mean and SD = 0
    
    if (scale_params_[name == "quarter_2", sd] != 0) {
      in_ED_all[, quarter_2 := (quarter_2 - scale_params_[name == "quarter_2", mean])/
                  scale_params_[name == "quarter_2", sd]]
    }
    
    if (scale_params_[name == "quarter_3", sd] != 0) {
      in_ED_all[, quarter_3 := (quarter_3 - scale_params_[name == "quarter_3", mean])/
                  scale_params_[name == "quarter_3", sd]]
    }
    if (scale_params_[name == "quarter_4", sd] != 0) {
      in_ED_all[, quarter_4 := (quarter_4 - scale_params_[name == "quarter_4", mean])/
                  scale_params_[name == "quarter_4", sd]]
    }    
    
    in_ED_all[, weekend_1 := (weekend_1 - scale_params_[name == "weekend_1", mean])/
                scale_params_[name == "weekend_1", sd]]
    
    in_ED_all[, arrival_time_06_12 := (arrival_time_06_12 - scale_params_[name == "arrival_time_06_12", mean])/
                scale_params_[name == "arrival_time_06_12", sd]]
    in_ED_all[, arrival_time_12_16 := (arrival_time_12_16 - scale_params_[name == "arrival_time_12_16", mean])/
                scale_params_[name == "arrival_time_12_16", sd]]
    in_ED_all[, arrival_time_16_22 := (arrival_time_16_22 - scale_params_[name == "arrival_time_16_22", mean])/
                scale_params_[name == "arrival_time_16_22", sd]]
    
    
  } else { # where using six week approach, each arrival date has its own scale_params_ saved earlier
    
    # need to make scale_params_ according to admission date
    
    in_ED_all[, first_ED_admission_date := date(first_ED_admission)]
    in_ED_all = merge(in_ED_all, scale_params_ %>% pivot_wider(names_from = name, values_from = c(mean, sd)), all.x = TRUE,
          by = "first_ED_admission_date")
    
    in_ED_all[, num_MAJORS_RESUS_scaled := (num_MAJORS_RESUS - mean_num_MAJORS_RESUS)/sd_num_MAJORS_RESUS]
    in_ED_all[, num_elsewhere_scaled := (num_elsewhere - mean_num_elsewhere)/sd_num_elsewhere]
    
    if (model_period_ == "Post-SDEC") {
      in_ED_all[, num_SDEC_scaled := (num_SDEC - mean_num_SDEC)/sd_num_SDEC]
    }
    
    
    
    in_ED_all[, weekend_1 := (weekend_1 - mean_weekend_1)/sd_weekend_1]
    in_ED_all[, arrival_time_06_12 := (arrival_time_06_12 - mean_arrival_time_06_12)/sd_arrival_time_06_12]
    in_ED_all[, arrival_time_12_16 := (arrival_time_12_16 - mean_arrival_time_12_16)/sd_arrival_time_12_16]
    in_ED_all[, arrival_time_16_22 := (arrival_time_16_22 - mean_arrival_time_16_22)/sd_arrival_time_16_22]
    in_ED_all[, arrival_time_22_06 := (arrival_time_22_06 - mean_arrival_time_22_06)/sd_arrival_time_22_06]
    
  }
  
  
  if (!use_six_week_rolling) { 
    
    if (model_period_ == c("Post")) {
      
      in_ED_all[, pow := (coefs[variables == "quarter_2", coef]*quarter_2) + 
                  (coefs[variables == "quarter_3", coef]*quarter_3) + 
                  (coefs[variables == "quarter_4", coef]*quarter_4) + 
                  (coefs[variables == "weekend_1", coef]*weekend_1) + 
                  (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
                  (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
                  (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
                  (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
                  ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled)]
      
      
    } else if (model_period_ == c("Pre + Post")) {
      
      in_ED_all[, pow := (coefs[variables == "quarter_2", coef]*quarter_2) + 
                  (coefs[variables == "quarter_3", coef]*quarter_3) + 
                  (coefs[variables == "quarter_4", coef]*quarter_4) + 
                  (coefs[variables == "weekend_1", coef]*weekend_1) + 
                  (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
                  (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
                  (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
                  (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
                  ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled) +
                  ( coefs[variables == "post_Covid_1", coef] * post_Covid_1)]
      
    } else if (model_period_ == "Pre") {
      
      in_ED_all[, pow := 
                  # (coefs[variables == "quarter_2", coef]*quarter_2) + 
                  (coefs[variables == "quarter_3", coef]*quarter_3) + 
                  (coefs[variables == "quarter_4", coef]*quarter_4) +
                  (coefs[variables == "weekend_1", coef]*weekend_1) + 
                  (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
                  (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
                  (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
                  (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
                  ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled)]
      
    } else if (model_period_ == "Post-SDEC" & !use_test_set_for_Cox_SDEC) {
      
      in_ED_all[, pow := (coefs[variables == "quarter_2", coef]*quarter_2) + 
                  # (coefs[variables == "quarter_3", coef]*quarter_3) + 
                  (coefs[variables == "quarter_4", coef]*quarter_4) +
                  (coefs[variables == "weekend_1", coef]*weekend_1) + 
                  (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
                  (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
                  (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
                  (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
                  ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled) +
                  ( coefs[variables == "num_SDEC_scaled", coef] * num_SDEC_scaled) ]
      
    }  else if (model_period_ == "Post-SDEC" & use_test_set_for_Cox_SDEC) {
      
      in_ED_all[, pow := #(coefs[variables == "quarter_2", coef]*quarter_2) + 
                  # (coefs[variables == "quarter_3", coef]*quarter_3) + 
                  #(coefs[variables == "quarter_4", coef]*quarter_4) +
                  (coefs[variables == "weekend_1", coef]*weekend_1) + 
                  (coefs[variables == "arrival_time_06_12", coef]*arrival_time_06_12) +
                  (coefs[variables == "arrival_time_12_16", coef]*arrival_time_12_16) +
                  (coefs[variables == "arrival_time_16_22", coef]*arrival_time_16_22 )+
                  (coefs[variables == "num_MAJORS_RESUS_scaled", coef] * num_MAJORS_RESUS_scaled)  +
                  ( coefs[variables == "num_elsewhere_scaled", coef] * num_elsewhere_scaled) +
                  ( coefs[variables == "num_SDEC_scaled", coef] * num_SDEC_scaled) ]
      
    } 
    
    in_ED_all[, epow := exp(pow + pow_adjustment[model_period == model_period_, adjustment])]
    
    
  } else { # use six week rolling average instead
    
    in_ED_all = merge(in_ED_all, coefs[, .(first_ED_admission_date, variables, coef)] %>%
                        pivot_wider(names_from = variables, values_from = c(coef), names_prefix = "coef_"), all.x = TRUE,
                      by = "first_ED_admission_date")
    
    if (model_period_ == c("Post")) {
      
      in_ED_all[, pow := 
                (coef_weekend_1*weekend_1) + 
                (coef_arrival_time_06_12*arrival_time_06_12) +
                (coef_arrival_time_12_16*arrival_time_12_16) +
                (coef_arrival_time_16_22*arrival_time_16_22 )+
                (coef_num_MAJORS_RESUS_scaled* num_MAJORS_RESUS_scaled)  +
                (coef_num_elsewhere_scaled * num_elsewhere_scaled)]
      
      
    } else if (model_period_ == c("Post-SDEC")) {
      
      in_ED_all[, pow := 
                  (coef_weekend_1*weekend_1) + 
                  (coef_arrival_time_06_12*arrival_time_06_12) +
                  (coef_arrival_time_12_16*arrival_time_12_16) +
                  (coef_arrival_time_16_22*arrival_time_16_22 )+
                  (coef_num_MAJORS_RESUS_scaled* num_MAJORS_RESUS_scaled)  +
                  (coef_num_elsewhere_scaled * num_elsewhere_scaled) +
                  (coef_num_SDEC_scaled * num_SDEC_scaled)] 
    
      
      
    } 
    
    # need to add other model periods if necessary
    
    in_ED_all[, epow := exp(pow)]
    
  }

  
  
  return(in_ED_all)
}


# Get poisson mean for time points ----------------------------------------
#  This function returns the poisson means for a series of time points; used in Step 6 of the pipeline
# This is done from a saved dataset as appropriate to the model period (Pre, Post Covid etc),


get_poisson_means = function(time_pts, poisson_results, poisson_nya, model_period_, time_window_array_ = c(4, 8)) {
  
  poisson_means = data.table()
  time_pts_ = data.table(time_pt = time_pts)
  
  time_pts_[, quarter := factor(case_when( month(time_pt) <= 3 ~ 1,
                                                   month(time_pt) <= 6 ~ 2, 
                                                   month(time_pt) <= 9 ~ 3, 
                                                   month(time_pt) <= 12 ~ 4))]
  
  time_pts_[, time_of_report := factor(case_when(hour(time_pt) > 22 | hour(time_pt) <= 6 ~ "6:00",
                                                       hour(time_pt) > 6 & hour(time_pt) <= 12 ~ "12:00",
                                                       hour(time_pt) > 12 & hour(time_pt) <= 16 ~ "16:00",
                                                       hour(time_pt) > 16 & hour(time_pt) <= 22 ~ "22:00"))]
  
  time_pts_[, quarter_1 := as.numeric(quarter ==1)]
  time_pts_[, quarter_2 := as.numeric(quarter ==2)]
  time_pts_[, quarter_3 := as.numeric(quarter ==3)]
  time_pts_[, quarter_4 := as.numeric(quarter ==4)]
  
  time_pts_[, weekend:= if_else(weekdays(time_pt, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]
  time_pts_[, weekend_1 := as.numeric(weekend ==1)]
  
  time_pts_[, time_of_report_6 := as.numeric(time_of_report == "6:00")]
  time_pts_[, time_of_report_12 := as.numeric(time_of_report == "12:00")]
  time_pts_[, time_of_report_16 := as.numeric(time_of_report == "16:00")]
  time_pts_[, time_of_report_22 := as.numeric(time_of_report == "22:00")]
  
  for (time_window_ in time_window_array_) {
    
    time_pts_$time_window = time_window_
    
    coefs = poisson_results[time_window == time_window_]
    setnames(coefs, "coefficents", "coef")

    if (model_period_ == c("Pre")) {
    
      time_pts_[, log_mu :=  (coefs[variables == "(Intercept)", coef]) +
                # (coefs[variables == "quarter_1", coef]*quarter_1)  +
                (coefs[variables == "quarter_2", coef]*quarter_2) +
                (coefs[variables == "quarter_3", coef]*quarter_3)  +
                # (coefs[variables == "quarter_4", coef]*quarter_4) +
                (coefs[variables == "weekend_1", coef]*weekend_1) +
                (coefs[variables == "time_of_report_12", coef]*time_of_report_12) +
                (coefs[variables == "time_of_report_16", coef]*time_of_report_16) +
                (coefs[variables == "time_of_report_22", coef]*time_of_report_22 )
              ]
      time_pts_[, mu := exp(log_mu)]
    
    } else if (model_period_ == c("Post")) {
      
      time_pts_[, log_mu :=  (coefs[variables == "(Intercept)", coef]) +
                  (coefs[variables == "quarter_1", coef]*quarter_1)  +
                  (coefs[variables == "quarter_2", coef]*quarter_2) +
                  (coefs[variables == "quarter_3", coef]*quarter_3)  +
                  # (coefs[variables == "quarter_4", coef]*quarter_4) +
                  (coefs[variables == "weekend_1", coef]*weekend_1) +
                  (coefs[variables == "time_of_report_12", coef]*time_of_report_12) +
                  (coefs[variables == "time_of_report_16", coef]*time_of_report_16) +
                  (coefs[variables == "time_of_report_22", coef]*time_of_report_22 )
                ]
      time_pts_[, mu := exp(log_mu)]
      
    } else if (model_period_ == c("Pre + Post")) {
      
      time_pts_[, log_mu :=  (coefs[variables == "(Intercept)", coef]) +
                  (coefs[variables == "quarter_1", coef]*quarter_1)  +
                  (coefs[variables == "quarter_2", coef]*quarter_2) +
                  (coefs[variables == "quarter_3", coef]*quarter_3)  +
                  # (coefs[variables == "quarter_4", coef]*quarter_4) +
                  (coefs[variables == "weekend_1", coef]*weekend_1) +
                  (coefs[variables == "time_of_report_12", coef]*time_of_report_12) +
                  (coefs[variables == "time_of_report_16", coef]*time_of_report_16) +
                  (coefs[variables == "time_of_report_22", coef]*time_of_report_22 )
                ]
      time_pts_[, mu := exp(log_mu)]
      
    } else if (model_period_ == c("Post-SDEC")) {
      
      time_pts_[, log_mu :=  (coefs[variables == "(Intercept)", coef]) +
                  (coefs[variables == "quarter_1", coef]*quarter_1)  +
                  (coefs[variables == "quarter_2", coef]*quarter_2) +
                  # (coefs[variables == "quarter_3", coef]*quarter_3)  +
                  # (coefs[variables == "quarter_4", coef]*quarter_4) +
                  (coefs[variables == "weekend_1", coef]*weekend_1) +
                  (coefs[variables == "time_of_report_12", coef]*time_of_report_12) +
                  (coefs[variables == "time_of_report_16", coef]*time_of_report_16) +
                  (coefs[variables == "time_of_report_22", coef]*time_of_report_22 )
                ]
      
      time_pts_[, mu := exp(log_mu)]
    }
    
    poisson_means = bind_rows(poisson_means, time_pts_[, .(time_pt, time_of_report, time_window, weekend, poisson_mean = mu)])

  }
  
  poisson_nya$weekend = as.double(poisson_nya$weekend)
  poisson_means = merge(poisson_means, poisson_nya[, .(weekend, time_of_report, time_window, max_nya)], by = c("time_window", "time_of_report", "weekend"))
  
  return(poisson_means)
}


# Generate probability distribution ---------------------------------------

# For a time window, generate a probability distribution
# This takes in all patients in ED at the time point, and returns a probability distribution over the number of beds
# needed until different conditions:
# - If a time_window array is not passed, returns overall distribution for pats currently in ED - NO time window projections
# - inc_nya returns overall distribution including all pats in ED at the time point, and those who have not yet arrived but
# might be expected to do so and be admitted within time window
# - only_nya returns distribution excluding all pats in ED; includes only those who have not yet arrived but
# might be expected to do so and be admitted within time window
# 
# Inputs:
# - preds_all_ts - a dataset of patients in ED, by time point, coupled with their probability of admission (from Step 2)
# - poisson_not_yet_arrived - the poisson means for each time point; these will be used to generate a probability
# distribution over the patients who have not yet arrived
# - cox_results -
# - base_prob - the base survival curve - this will be stretched or squeezed using a coefficient that is specific to
# each visit and the number of patients in ED at their arrival time
# - inc_nya - whether to include the not yet arrived
# - only_nya - whether to include only those not yet arrived
# - time_of_extract - the time point at which to create the predictions
# - time_window_array_ - an array of prediction windows (typically 4 and 8 hours) over which to calculate
# 
# Outputs
# - distr_all - by time point, the distribution of probabilities over number of beds, includes
# metadata on which time window was used, whether inc_nya or only_nya


get_prob_dist = function(preds_all_ts,
                         poisson_not_yet_arrived, cox_results, base_prob, 
                         inc_nya, only_nya,
                         time_of_extract, time_window_array_ ) {
  
  
  distr_all = data.table()
  preds_all_ts[, time_so_far := as.numeric(difftime(time_of_extract, first_ED_admission, units = "hours"))]
  
  if (is.na(time_window_array_)[1]) {
    
    # total possible number of admissions is the number of patients in ED now
    num_adm_ = seq(0,nrow(preds_all_ts), 1)
    
    # aggregated 
    df = preds_all_ts
    probs_in_ED = poly_prod(df) 
    
    dist_ = bind_cols(num_adm_pred = num_adm_,
                      probs = probs_in_ED,
                      cdf = cumsum(probs_in_ED),
                      prob_this_num = 1 - cumsum(probs_in_ED),
                      time_window = NA,
                      inc_nya = FALSE,
                      dist = "Only patients in ED")
    distr_all = bind_rows(distr_all, dist_) 
    
    return(distr_all)
    
  } else if (only_nya) { 
    
    # return only the not yet arrived
    for (time_window_ in time_window_array_) {
      
      # get the probable number of not yet arrived, using a mean and a max value for the distribution
      # derived from empirical data
      num_adm_ = seq(0, poisson_not_yet_arrived[time_window == time_window_, max_nya],1)
      
      probs_not_yet_arrived = dpois(num_adm_,
                                    lambda = poisson_not_yet_arrived[time_window == time_window_, poisson_mean])
      
      # for error debugging
      if (length(probs_not_yet_arrived) != length(num_adm_)) {
        
        print("Array length mismatch error - only_nya")
        print(paste("Time window =", time_window_))
        print(paste("max_nya =", poisson_not_yet_arrived[time_window == time_window_, max_nya]))
        print(paste("length(probs_not_yet_arrived) =", length(probs_not_yet_arrived)))
        print(paste("length(num_adm_) = ", length(num_adm_)))
        
      }
      
      dist_ = bind_cols(num_adm_pred = num_adm_,
                            probs = probs_not_yet_arrived,
                            cdf = cumsum(probs_not_yet_arrived),
                            prob_this_num = 1 - cumsum(probs_not_yet_arrived),
                            time_window = time_window_,
                            inc_nya = TRUE,
                            dist = "Only not yet arrived")
      distr_all = bind_rows(distr_all, dist_) 
      
    }
    
    return(distr_all)
    
 
  } else if (inc_nya) {
    
    # return patients in ED and not yet arrived
    num_adm_ = seq(0,nrow(preds_all_ts), 1)
    for (time_window_ in time_window_array_) {
      
      tta_prob_ = data.table()
      df = preds_all_ts

      # for each csn in turn, get their probability of being in the ED for the elapsed time so far,
      # and their probability of still being there after the time window ends
      for (h in 1 : nrow(df)) {

        # stretch or squeeze the base survival curve based on the coefficient calculated for this particular visit;
        # see get_cox_coef_by_csn() function which returns epow
        base_prob$surv_ = base_prob$surv ^ df$epow[h]

        # identify which point of the survival curve relates to the time the patient has been in so far
        col_num_for_still_in_now = sum(base_prob$time<= df$time_so_far[h])
        
        # it is possible for the time so far to be shorter than any of the survival times saved in the cox curve
        if (col_num_for_still_in_now == 0) {
          col_num_for_still_in_now = 1
        }

        # get the probability of the patient being in the ED at this point (ie give the time so far)
        prob_still_in_now = base_prob$surv_[col_num_for_still_in_now]
        
        # now find their probability of still being in ED at this long after arrival plus the time window
        col_num_for_still_in_at_time_window = sum(base_prob$time<= df$time_so_far[h] + time_window_)
        prob_still_in_at_time_window = base_prob$surv_[col_num_for_still_in_at_time_window]

        # rebase the survival curve using their probability of still being in now
        base_prob$rebased_prob = base_prob$surv_ / prob_still_in_now

        # from the rebased curve, read their probability of still being in at the time window
        prob_still_in_at_time_window_alt = base_prob$rebased_prob[col_num_for_still_in_at_time_window]

        # save the probability 
        tta_prob_ = bind_rows(tta_prob_, data.table(csn = df$csn[h], 
                                                    prob_adm_in_time_window = 1 - prob_still_in_at_time_window_alt))
      }

      # merge the time window probabilities with the df dataset which contains all patients in ED at the time point
      df = merge(df, tta_prob_, by = "csn")

      # multiply their original probability of admission with their probability of still being in within the time window
      df[, prob.1 := prob.1 * prob_adm_in_time_window]
      
      # get probabilities for patients currnently in ED using the function poly_prod() defined above
      # this will return an array of probabilities across the total possible number of beds for the
      # probability of that number of beds being needed
      probs_in_ED = poly_prod(df) 
      
      # get the probable number of not yet arrived, using a poisson mean, over a number of beds
      # up to a maximum possible number of beds (max_nya) for the time point, derived from empirical data
      probs_not_yet_arrived = dpois(seq(0, poisson_not_yet_arrived[time_window == time_window_, max_nya] ,1),
                                    lambda = poisson_not_yet_arrived[time_window == time_window_, poisson_mean])
      
      # each of prob_in_ED and probs_not_yet_arrived is a prob distribution for a 
      # random variable N where N is the number of patients to be admitted
      # get joint probability for each combination of the two random variables
      dist_ = data.table()
      
      for (i in 1:length(probs_in_ED)) {
        for (j in 1:length(probs_not_yet_arrived)) {
          
          tot_adm_ = i-1 + j-1
          prob_tot_ = probs_in_ED[i] * probs_not_yet_arrived [j]
          row = data.table(num_adm_pred = tot_adm_, prob_tot = prob_tot_)
          
          dist_ = bind_rows(dist_, row)
        }
      }
      
      # get the probilitity for each unique number of admissions (summing across the two distributions)
      dist_ = dist_[, .(probs = sum(prob_tot)), by = num_adm_pred]
      dist_[, cdf := cumsum(probs)]
      dist_[, prob_this_num := 1 - cdf]
      dist_$time_window = time_window_
      
      dist_$inc_nya = TRUE
      dist_$dist = "Empirical poisson"
      distr_all = bind_rows(distr_all, dist_)       
      
      
    }
    
    return(distr_all)
    
  } else {
    
    # return only the patients in ED
    # this uses similar logic as above, but simpler because we do not need to include the not-yet-arrive patients
    
    # the probabilities of each of these numbers being admitted
    num_adm_ = seq(0,nrow(preds_all_ts), 1)
    
    # get probabilty without time window

    for (time_window_ in time_window_array_) {
      
      tta_prob_ = data.table()
      df = preds_all_ts
      
      # for each csn, find their probability of still being in ED at this long after arrival
      for (h in 1 : nrow(df)) {
        
        base_prob$surv_ = base_prob$surv ^ df$epow[h]
        
        col_num_for_still_in_now = sum(base_prob$time<= df$time_so_far[h])
        
        # it is possible for the time so far to be shorter than any of the survival times save in the cox curve
        if (col_num_for_still_in_now == 0) {
          col_num_for_still_in_now = 1
        }
        
        prob_still_in_now = base_prob$surv_[col_num_for_still_in_now]
        
        # for each csn, find their probability of still being in ED at this long after arrival plus the time window
        col_num_for_still_in_at_time_window = sum(base_prob$time<= df$time_so_far[h] + time_window_)
        
        # Added this fix for patients who have been in for a long time such that
        # their probability of still being in at time window is the same as their probability of being in now
        if (col_num_for_still_in_at_time_window == col_num_for_still_in_now ) {
          col_num_for_still_in_at_time_window = col_num_for_still_in_at_time_window + 1
        }
        
        prob_still_in_at_time_window = base_prob$surv_[col_num_for_still_in_at_time_window]
        
        base_prob$rebased_prob = base_prob$surv_ / prob_still_in_now
        prob_still_in_at_time_window_alt = base_prob$rebased_prob[col_num_for_still_in_at_time_window]
        # save the probability 
        tta_prob_ = bind_rows(tta_prob_, data.table(csn = df$csn[h], 
                                                    prob_adm_in_time_window = 1 - prob_still_in_at_time_window_alt))
      }
      
      df = merge(df, tta_prob_, by = "csn")
      df[, prob.1 := prob.1 * prob_adm_in_time_window]
      
      # get probabilities for patients in ED
      probs_in_ED = poly_prod(df) 
      
      # for error debugging
      if (length(probs_in_ED) != length(num_adm_)) {
      
        
        print("Array length mismatch error - only patients in ED")
        print(paste("Time window =", time_window_))
        print(paste("length(probs_in_ED) =", length(probs_in_ED)))
        print(paste("length(num_adm_) = ", length(num_adm_)))
        print("Dataframe of patients where prob.1 is NA")
        print(df[is.na(prob.1)])
        
      }
      
      

      dist_ = data.table(bind_cols(num_adm_pred = num_adm_,
                          probs = probs_in_ED, 
                          cdf = cumsum(probs_in_ED),
                          prob_this_num = 1 - cumsum(probs_in_ED),
                          time_window = time_window_,
                          inc_nya = FALSE,
                          dist = "Only patients in ED"))
      distr_all = bind_rows(distr_all, dist_) 
    }
    
    return(distr_all)
  }
  
}



# Get data on prior admissions --------------------------------------------
#  This function is used in real-time to retrieve data on the number of admissions that
# have occurred within a range of dates and times. It is used only to format information for the emails
# that are sent out four times a day by the real-time app

# get prior admission data
get_prior_adm <- function(search_start, search_string, ctn) {
  
  sqlQuery <- paste("
   SQL query withheld for information security reasons
")
  
sqlQuery <- gsub('\n','',sqlQuery)
  prev_moves <- data.table(dbGetQuery(ctn, sqlQuery))
  
  prev_moves <- setorderv(prev_moves, c("csn", "admission"))
  
  # clean location data
  prev_moves <- clean_location_data(prev_moves)
  
  # find the latest move out of an ED location
  prev_moves[department == "ED" & !(is.na(discharge) & first_ED), last_ED_discharge := max(discharge, na.rm = TRUE), by = csn ]
  
  # update locations to match report
  prev_moves[, department := case_when(department == "T01" ~ "AMU",
                                                    department == "T01ECU" ~ "ECU",
                                                    department %in% c("T06H", "T06C") ~ "T06H & T06C",
                                                    TRUE ~ department)]
  
  # filter only such moves that happened on the reference day of interest
  prev_moves_discharge_to = prev_moves[admission == lag(last_ED_discharge) & 
                                         date(admission) == date(search_string) &
                                         csn == lag_csn , .N, by = department]
  
  # the above gets people who are still in ED, because they have no inferred discharge from ED
  # remove the people who are still in ED at the current time
  prev_moves_discharge_to = prev_moves_discharge_to[department != "ED"]
  
  prev_moves_discharge_to[, not_tower := !grepl("^T[0-9]|^AMU|^ECU", department)]
  setorderv(prev_moves_discharge_to, c("not_tower", "department"))
  
  return(prev_moves_discharge_to)
  
}


# Prepare plots for real-time emails  ----------------------------------------
#  The following functions format the charts that are prepared by the real-time app
# to be sent via email


format_dist_for_charts = function(distr_all, chart_colours, tw_end_of_day) {
  
  # if time window is 4 or 8 hours, two identical distributions will have been created
  distr_all = unique(distr_all)
  
  distr_all[, prob_discrete := case_when(prob_this_num > 0.90 ~ "> 90%",
                                         prob_this_num > 0.70 ~ "70-90%",
                                         prob_this_num > 0.4 ~ "40-70%",
                                         prob_this_num > 0.1 ~ "10-40%",
                                         TRUE ~ "< 10%")]
  distr_all[, prob_discrete := factor(prob_discrete, levels = c("> 90%", "70-90%", "40-70%", "10-40%", "< 10%"))]
  
  distr_all[, chart_colour := case_when(prob_this_num > 0.90 ~ chart_colours[1],
                                         prob_this_num > 0.70 ~ chart_colours[2],
                                         prob_this_num > 0.4 ~ chart_colours[3],
                                         prob_this_num > 0.1 ~ chart_colours[4],
                                         TRUE ~ chart_colours[5])]
  
  if (!is.na(tw_end_of_day)) {
    
    distr_all[, time_window_text := case_when(time_window %in% c(4, 8) ~ paste("In",time_window, "hours"),
                                              TRUE ~ "By midnight")]
    
    if (tw_end_of_day <= 4) {
      distr_all[, time_window_text_ := factor(time_window_text, levels = c("By midnight", "In 4 hours", "In 8 hours"))]
    } else if (tw_end_of_day <= 8) {
      distr_all[, time_window_text_ := factor(time_window_text, levels = c("In 4 hours",  "By midnight", "In 8 hours"))]
    } else {
      distr_all[, time_window_text_ := factor(time_window_text, levels = c("In 4 hours",  "In 8 hours", "By midnight"))]
    }
    
  }
  
  return(distr_all)
  
}

format_gender_dist_for_charts = function(distr_men, distr_women, distr_all, chart_colours, tw_end_of_day) {
  
  # if time window is 4 or 8 hours, two identical distributions will have been created
  distr_men = unique(distr_men)
  distr_women = unique(distr_women)
  
  distr_men[, prob_discrete := case_when(prob_this_num > 0.90 ~ "> 90%",
                                         prob_this_num > 0.70 ~ "70-90%",
                                         prob_this_num > 0.4 ~ "40-70%",
                                         prob_this_num > 0.1 ~ "10-40%",
                                         TRUE ~ "< 10%")]
  distr_men$sex <- "Male"

  distr_women[, prob_discrete := case_when(prob_this_num > 0.90 ~ "> 90%",
                                         prob_this_num > 0.70 ~ "70-90%",
                                         prob_this_num > 0.4 ~ "40-70%",
                                         prob_this_num > 0.1 ~ "10-40%",
                                         TRUE ~ "< 10%")]
  
  distr_women$sex <- "Female"
  
  distr_both = bind_rows(distr_men, distr_women)
  
  distr_both[, prob_discrete := factor(prob_discrete, levels = unique(distr_all$prob_discrete))]
  
  distr_both[, chart_colour := case_when(prob_this_num > 0.90 ~ chart_colours[1],
                                        prob_this_num > 0.70 ~ chart_colours[2],
                                        prob_this_num > 0.4 ~ chart_colours[3],
                                        prob_this_num > 0.1 ~ chart_colours[4],
                                        TRUE ~ chart_colours[5])]
  if (!is.na(tw_end_of_day)) {
    
    
    distr_both[, time_window_text := case_when(time_window %in% c(4, 8) ~ paste("In",time_window, "hours"),
                                               TRUE ~ "By midnight")]
    
    if (tw_end_of_day <= 4) {
      distr_both[, time_window_text_ := factor(time_window_text, levels = c("By midnight", "In 4 hours", "In 8 hours"))]
    } else if (tw_end_of_day <= 8) {
      distr_both[, time_window_text_ := factor(time_window_text, levels = c("In 4 hours",  "By midnight", "In 8 hours"))]
    } else {
      distr_both[, time_window_text_ := factor(time_window_text, levels = c("In 4 hours",  "In 8 hours", "By midnight"))]
    }
    
    return(distr_both)
  }

  
}


plot_sub_chart = function(dataset, title_) {
  
  dataset[, prob_discrete := factor(prob_discrete, levels = unique(dataset$prob_discrete))]
  
  dataset %>% ggplot(aes(x = num_adm_pred, y = 1, fill = prob_discrete)) + geom_bar(stat = "identity") +
    # scale_fill_gradient2(low = "yellow", high = "red", limits = c(0,1), breaks = seq(0,1, .25)) +
    coord_cartesian(xlim = c(0,30), default = TRUE)  +
    labs(title = title_) +
    theme_classic() + theme(text = element_text(size = 10)) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(legend.position = "none")  +
    scale_fill_manual(values = unique(dataset$chart_colour))
  
  
}


get_main_plot_for_time_window = function(time_window_, distr_all, chart_colours) {
  
  plot_all = distr_all[num_adm_pred <= 40 & time_window == time_window_] %>% ggplot(aes(x = num_adm_pred, y = 1, fill = prob_discrete)) + geom_col() +
    # scale_fill_gradient2(low = "yellow", high = "red", limits = c(0,1), breaks = seq(0,1, .25)) + 
    # facet_rep_wrap(time_window_text_~., strip.position = "top", ncol = 3, repeat.tick.labels = TRUE) + 
    # this gets zero bar but then can't set scale - get around this by capping scale at 60
    # but also can't set access breaks
    coord_cartesian(xlim = c(0,40), default = TRUE)  + 
    # scale_x_continuous(limits = c(0,60), breaks = seq(0,60,5)) +
    
    coord_flip() +
    labs(x = "Number of beds", 
         fill = "Probability") +
    theme_classic() +
    # theme(text = element_text(size = 14)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "right")  +
    scale_fill_manual(values = chart_colours) + 
    guides(fill = guide_legend(reverse=T))
  
  if (time_window_ %in% c(4, 8)) {
    plot_all = plot_all + labs(title = paste("In",time_window_, "hrs"))
  } else {
    plot_all = plot_all + labs(title = paste("By midnight") )
  }
}


get_subplots_for_time_window = function(time_window_, distr_SDEC, distr_not_SDEC, distr_only_nya) {
  
  plot_SDEC = plot_sub_chart(distr_SDEC[time_window == time_window_ & num_adm_pred <= 30], "Admissions from SDEC")
  plot_not_SDEC  = plot_sub_chart(distr_not_SDEC[time_window == time_window_ & num_adm_pred <= 30], "Admissions from elsewhere in ED")
  plot_nya = plot_sub_chart(distr_only_nya[time_window == time_window_ & num_adm_pred <= 30], "Admissions of patients not yet arrived")
  
  subplots = grid.arrange(plot_SDEC, plot_not_SDEC, plot_nya, nrow = 3)
}


get_main_plot_for_all_pats_in_ED = function(distr_all, chart_colours) {
  
  plot_all = distr_SDEC_no_tw  %>% ggplot(aes(x = num_adm_pred, y = 1, fill = prob_discrete)) + geom_col() +
    # scale_fill_gradient2(low = "yellow", high = "red", limits = c(0,1), breaks = seq(0,1, .25)) + 
    # facet_rep_wrap(time_window_text_~., strip.position = "top", ncol = 3, repeat.tick.labels = TRUE) + 
    # this gets zero bar but then can't set scale - get around this by capping scale at 60
    # but also can't set access breaks
    coord_cartesian(xlim = c(0,40), default = TRUE)  + 
    # scale_x_continuous(limits = c(0,60), breaks = seq(0,60,5)) +
    
    coord_flip() +
    labs(x = "Number of beds", 
         fill = "Probability") +
    theme_classic() +
    # theme(text = element_text(size = 14)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "right")  +
    scale_fill_manual(values = chart_colours) + 
    guides(fill = guide_legend(reverse=T))
  
}

