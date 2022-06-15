# About this folder

This is a series of files to extract data from Star and analyse patient moves between locations.  The focus is on moves within ED. 

## Order to run files in

### 1. Retrieve data

get-ED-data-from-Star.R

Retrieve and process data from EMAP  

output (all as tibbles)
- ED_bed_moves_raw
- ED_csn_summ_raw
- visits

plus other files saved from the SQL extracts

- csn_summ - all csns over time
- bed_moves - all bed moves over time
- patient_class - latest patient class
- all_patient_class - patient class audit table

Note that department names in EMAP Star relating to ED are the following
- ED
- "1020100166" and "1020100170" - these are both SDEC
- "UCHT00CDU" - this is CDU
and that some locations have null as department

For reference, there are other departments with numbers rather than names on EMAP. These are:
- 1020100171 which is T06PACU
- 1020100172 which is T07SWH
- 1021500003 which seems to refer to some kind of virtual clinic
- 1021500045 which seems to refer to some kind of virtual clinic
- 1021800001 - some kind of theatre pool
- 1021800003 - GWB - new building
- 1021800004 - GWB - new building
- 2019000004 - discharge planning

### 2. Create data tables

create-data.tables.R

Does further processing on the datasets created by get-ED-data-from-Star.R to (a) identify exits from ED to various locations of interest and (b) calculate admission class which will be the final label for ML and (c) create a simplified edge list (ie rows with from and to nodes) that will be used for network maps

input
- ED_csn_summ_raw - a summary of all hospital visits (one row per visit)
- ED_bed_moves_raw - a summary of all location moves (one row per move, so multiple rows per visit)

output (all as data tables)
- moves - cleaned location data with one row per move
- edgedf
- summ - updated version of ED_csn_summ_raw with admission class

Once a new set of output is ready, predict-admissions/code/config.R needs to be updated with the new summ_file_date so that scripts pick up the correct output

### 3. prepare-ED-data-for-ML.R

Does further processing useful to ML models used later, including creating datasets of all patients in ED at certain timepoints of interest. This file is used as an input to the Cox regression part of the pipeline

Outputs include

- num-in-ED-at-arrival-time - records how many other people were in ED at the arrival times of each patient
- before_covid_adj_matrix.csv and the same for after Covid - adjacency matrices for flow between locations (prepared for Ken Li and Joe Farrington's work, no longer relevant to this project)
- num_in_location and moved_from_location - for both before and after Covid, these records the numbers in location at each hour, and moves between locations at each hour (prepared for Ken Li and Joe Farrington's work)

Note that utils.R and config.R are loaded from the predict-admissions folder. 

- utils.R creates functions that are shared between scripts. 
- config.R sets parameters like dates for training, validation and test sets, and dates for which files to use. 

