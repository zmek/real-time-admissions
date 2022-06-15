
# About this file ---------------------------------------------------------

# - Identifies final classification of each visit into admission or discharge
# - Converts raw bed moves data into a data table (faster processing in R)
# - Creates an edge list for locations moves for all patients (used for exploratory purposes)

# Note that, in the course of deciding how to define the final classification
# (ie the outcome variable), various locations which were not coded as the ED department
# on Epic, could nonetheless be deemed as part of that department.

# Hence, in this script, visits are coded as to whether they
# included the CDU (an observation unit), the EAU (acute medicine unit) and SDEC
# (same-day emergency care). Ultimately, CDU and SDEC were deemed to be part of the ED
# but the EAU is not

# Also note that we were interested at one point during that exploratory phase in
# distinguishing between direct and indirect admissions and dicharges. Hence, in this
# script, the outcome variable has four values
# - direct_adm - went to a ward after ED, without going via CDU or SDEC
# - indirect_adm - went to a ward after ED, first going via CDU or SDEC
# - direct_dis - discharged after ED, without going via CDU or SDEC
# - indirect_dis - discharged after ED, first going via CDU or SDEC

# In later scripts, these four categories are compressed into a binary outcome of
# admission or discharge

# The script also had to deal with multiple ED exits, where a visit moved to CDU or SDEC
# and then back to ED, or in some cases to a ward (very briefly)and back.

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)

#######

# Create functions --------------------------------------------------------


# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")


# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

#
# add utils file which is shared by real-time app
# using this here will ensure that the real-time app and the ML models use the same locations
source("EDcrowding/real-time/app/utils.R")

# temporarily override summ_file_date to get most recent patient data extract
summ_file_date = "2022-03-21"

# Load data ---------------------------------------------------------------
load(paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_",summ_file_date,".rda"))
load(paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_",summ_file_date,".rda"))

# clean location data ------------------------------------------------------

moves = clean_location_data(data.table(ED_bed_moves_raw), real_time = FALSE)

# applying same location naming as being done for Ken's work in prepare-ED-data-for-ML-new.R
moves[, location := case_when(
                              location %in% c("ED PAEDS", "ED SAA") ~ "Other",
                              location == "ED DIAGNOSTICS" & admission > date(covid_start) ~ "Other",
                              
                              TRUE ~ location)]


# # Set up column names for lead and lag functionality
cols = c("csn","admission","discharge", "department", "location")
leadcols = paste("lead", cols, sep="_")
lagcols = paste("lag", cols, sep="_")


# Other -------------------------------------------------------------------

# reset lead columns
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]

# update lead location where csns change
moves[, lead_department := if_else(csn != lead_csn, NA_character_, lead_department)]
moves[, lead_location := if_else(csn != lead_csn, NA_character_, lead_location)]
moves[, lead_csn := if_else(csn != lead_csn, NA_character_, lead_csn)]


# calc row duration
moves[, "row_duration" := difftime(discharge, admission, units = "mins")]

# identify ED exits
moves[, "ED_exit" := if_else(lead_department !="ED" & department == "ED", TRUE, FALSE)]
moves[, "num_ED_exit" := sum(ED_exit, na.rm = TRUE), by = csn]

# identify if ED exit is less than 15 min 
moves[, ED_exit_short := if_else(ED_exit & csn == lead_csn & 
                                    shift(row_duration, 1, NA, "lead") < 15 &
                                    lead_department == "ED", TRUE, FALSE)]

# identify exits to locations that may be considered to be 'internal' to the ED
moves[, "exit_to_EAU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "EAU", 1, 0)]
moves[, "exit_to_CDU" := if_else(ED_exit & !ED_exit_short & department =="ED" & lead_department == "UCHT00CDU", 1, 0)]

moves[, "visited_EAU" := sum(department == "EAU", na.rm = TRUE) > 0, by = csn]
moves[, "visited_CDU" := sum(department == "UCHT00CDU", na.rm = TRUE) > 0, by = csn]

# group exits via relevant locations
moves[, "obs" := case_when(department == "UCHT00CDU" ~ 1, 
                                   TRUE ~ 0)]
moves[, "same_day" := case_when(location == "SDEC" ~ 1, 
                           TRUE ~ 0)]
# moves[, "acute" := case_when(department == "EAU" ~ 1, 
#                              department == "T01" ~ 1, 
#                              TRUE ~ 0)]

moves[, "visited_obs" := sum(obs, na.rm = TRUE) > 0, by = csn]
moves[, "visited_same_day" := sum(same_day, na.rm = TRUE) > 0, by = csn]

# identify whether visit limited to observation or same day locations (referred to as inside)
moves[, "ED" := if_else(department == "ED", 1, 0)]
moves[, "ED_obs_same_day" := ED + obs + same_day]
moves[, "outside" := ED_obs_same_day == 0]
moves[, "visited_outside":= sum(ED_obs_same_day == 0, na.rm = TRUE) > 0, by = csn]
moves[, "inside_exit" := if_else(!outside & !is.na(lead_csn) & shift(outside, 1, NA, "lead"), TRUE, FALSE)]


# get last inside rows
moves[(!outside), last_inside := if_else(discharge == max(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]
moves[ED == 1, last_ED := if_else(discharge == max(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]

last_inside_ = unique(moves[(last_inside), list(csn, discharge)])
setnames(last_inside_, "discharge", "last_inside_discharge")

last_ED_ = unique(moves[(last_ED), list(csn, discharge)])
setnames(last_ED_, "discharge", "last_ED_discharge")

moves = merge(moves, last_inside_, all.x = TRUE)
moves = merge(moves, last_ED_, all.x = TRUE)
rm(last_ED_, last_inside_)

# get first outside rows (note these may not be the same as last inside rows in the case of multiple ED exits)
moves[ED_exit == 1, first_ED_exit := if_else(discharge == min(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]
moves[inside_exit == 1 & shift(outside, 1, NA, "lead"), first_inside_exit := if_else(discharge == min(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]

# add first  non ED location 
first_outside_ED = unique(moves[(first_ED_exit), list(csn, lead_admission)])
setnames(first_outside_ED, "lead_admission", "first_outside_ED_admission")
moves = merge(moves, first_outside_ED, all.x = TRUE)

# add first 'proper' location (ie not obs or same day)
first_outside_proper_ = unique(moves[(first_inside_exit), list(csn, lead_admission, lead_location)])
setnames(first_outside_proper_, "lead_admission", "first_outside_proper_admission")
setnames(first_outside_proper_, "lead_location", "first_outside_proper")
moves = merge(moves, first_outside_proper_, all.x = TRUE)
rm(first_outside_proper_)

# identify first location
moves[, "first_admission" := min(admission), by = csn]
moves[, "first_location" := location[which(admission == first_admission)], by = csn]
moves[, "first_dept" := department[which(admission == first_admission)], by = csn]

# # identify final location
moves[, "final_admission" := max(admission), by = csn]
moves[, "final_location" := location[which(admission == final_admission)], by = csn]
moves[, "final_dept" := department[which(admission == final_admission)], by = csn]


# Assign final classification ---------------------------------------------

direct_adm <- moves[(!visited_same_day) & !visited_obs & visited_outside, unique(csn)]
indirect_adm <- moves[(visited_same_day & !visited_obs & visited_outside) |
                        ((!visited_same_day) & visited_obs & visited_outside) |
                        (visited_same_day & visited_obs & visited_outside), unique(csn)]
indirect_dis <- moves[((!visited_same_day) & visited_obs & !visited_outside) |
                        (visited_same_day & !visited_obs & !visited_outside) |
                        (visited_same_day & visited_obs & !visited_outside), unique(csn)]
direct_dis <- moves[(!visited_same_day) & !visited_obs & !visited_outside, unique(csn)]

summ <- data.table(ED_csn_summ_raw %>% filter(csn %in% moves$csn) %>% 
                     mutate(adm = case_when(csn %in% direct_adm ~ "direct_adm",
                                            csn %in% indirect_adm ~ "indirect_adm",
                                            csn %in% indirect_dis ~ "indirect_dis",
                                            csn %in% direct_dis ~ "direct_dis")))


setkey(summ, csn)
rpt(summ)

# Add relevant transition times to summary table -------------------------------------------

summ <- merge(summ, (unique(moves[,.(csn, first_ED_admission, first_outside_ED_admission, 
                                     first_outside_proper_admission, last_ED_discharge, last_inside_discharge, 
                                     first_outside_proper)])))

summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

# correct the visits where the patient was previously an inpatient
# but was discharged after ED
summ[is.na(first_outside_proper_admission) & adm %in% c("direct_adm", "indirect_adm"), 
     adm := "direct_dis"]

# Save summ and moves  ---------------------------------------------------------------

outFile = paste0("EDcrowding/flow-mapping/data-raw/moves_",today(),".rda")
save(moves, file = outFile)
rm(outFile)


outFile = paste0("EDcrowding/flow-mapping/data-raw/summ_",today(),".rda")
save(summ, file = outFile)
rm(outFile)



# Prep for network analysis -----------------------------------------------


# update adm to binary
summ[, adm_binary := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]

# remove nas where not ED
moves[is.na(location), location := case_when(department == "UCLHHOME" ~ "UCLHHOME",
                                             TRUE ~ "Inpatient ward")]


# add columns to moves identifying departments of interest
moves[, MAJORS := location == "ED MAJORS"]
moves[, visited_MAJORS:= sum(MAJORS) > 0, by = csn]

moves[, RESUS := location == "ED RESUS"]
moves[, visited_RESUS:= sum(RESUS) > 0, by = csn]

moves[, TAF := location == "ED TAF"]
moves[, visited_TAF:= sum(TAF) > 0, by = csn]

moves[, CDU := location == "ED CDU"]
moves[, visited_CDU:= sum(CDU) > 0, by = csn]

moves[, UTC := location == "ED UTC"]
moves[, visited_UTC:= sum(UTC) > 0, by = csn]

moves[, SDEC := location %in% c("SDEC Waiting", "SDEC")]
moves[, visited_SDEC:= sum(SDEC) > 0, by = csn]

moves[, RAT := location == "ED RAT"]
moves[, visited_RAT:= sum(RAT) > 0, by = csn]



# reset lead columns
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]

# update lead location where csns change
moves[, lead_department := if_else(csn != lead_csn, NA_character_, lead_department)]
moves[, lead_location := if_else(csn != lead_csn, NA_character_, lead_location)]
moves[, lead_csn := if_else(csn != lead_csn, NA_character_, lead_csn)]


# create final edge list
edgedf <- moves[,.(csn, from = location, to = lead_location, 
                     from_dept = department, to_dept = lead_department, 
                   visited_MAJORS, visited_RESUS, visited_TAF, visited_CDU, visited_UTC, visited_SDEC, visited_RAT, 
                   dttm = discharge)]

# identify moves where patient is admitted from ED
edgedf[from_dept %in% c("ED", "UCHT00CDU") & !(to_dept %in% c("ED", "UCHT00CDU") | is.na(to_dept)), adm := "Admitted"]
# checking - missing 29 csns all look like prior inpatient then discharged
summ[adm_binary ==1 & !(csn %in% edgedf[adm == "Admitted",csn])]

# identify moves where patient is discharged from ED
edgedf[from_dept %in% c("ED", "UCHT00CDU") & is.na(to_dept), adm := "Discharged"]
# checking
summ[adm_binary == 0 & !(csn %in% edgedf[adm == "Discharged",csn])]

# update "to" column with admitted or discharged
edgedf[, to_new := case_when(!is.na(adm) ~ adm, TRUE ~  to)]

# remove rows with to_new = NA as these are inpatients
edgedf = edgedf[!is.na(to_new)]

# 
# check number of admission and discharges matches summ
# note that this won't exactly match summ$adm because some visits involve > 1 admission
rpt(edgedf[from_dept %in% c("ED", "UCHT00CDU") & to_new == "Discharged"])
rpt(edgedf[from_dept %in% c("ED", "UCHT00CDU") & to_new == "Admitted"])

# remove edges after ED
edgedf = edgedf[(from_dept %in% c("ED", "UCHT00CDU"))]

# # create hashed version for export
# edgedf_hashed = edgedf
# 
# edgedf_hashed$hash <-lapply(edgedf_hashed$csn, function(x) {digest(x, algo="md5", serialize = F)})
# edgedf_hashed[, csn := NULL]
# edgedf_hashed$hash <- as.character(edgedf_hashed$hash)
# 

edgedf[, c("to", "adm") := NULL]
setnames(edgedf, "to_new", "to")

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_",today(),".rda")
save(edgedf, file = outFile)
rm(outFile)

write.csv(edgedf, file = paste0("EDcrowding/flow-mapping/data-raw/edgedf_",today(),".csv"),
          row.names = FALSE)

