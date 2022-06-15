# About the script  -------------------------------------------


# Gets all patients currently in ED from Star
# Retrieves location, visit, obs and labs data for each patient
# Retrieves pre-trained ML models and makes predictions for each individual
# Creates probability distribution
# Creates charts with predictions
# Formats and sends emails

# Load libraries and utils ------------------------------------------------
print("Loading libraries")


source('../app/libraries.R')
source('../app/config.R')
source('../app/utils.R')

options(dplyr.summarise.inform = FALSE)


# Set up connection -------------------------------------------------------

# Set up connection
ctn <- DBI::dbConnect() # Details withheld for information security reasons



# Get  data ---------------------------------------------------------

print("Retrieving data from Star")

time_of_extract = Sys.time()


# get all patients in ED now; one row per encounter

query <- readr::read_file('../app/sql/summ_patients_in_ED_now.sql')
summ_now <- DBI::dbGetQuery(ctn, query)
setDT(summ_now)
summ_now <- summ_now %>% mutate(presentation_time = coalesce(presentation_time, admission_time))
summ_now <- summ_now %>% mutate(time_since_arrival = difftime(time_of_extract, presentation_time, units = "mins"))
summ_now_orig <- summ_now

# get demographic data for patients in ED now; one row per patient mrn

query <- readr::read_file('../app/sql/demog_patients_in_ED_now.sql')
demog_raw <- DBI::dbGetQuery(ctn, query)
setDT(demog_raw)

# get location data for patients in ED now; one row per location

query <- readr::read_file('../app/sql/moves_patients_in_ED_now.sql')
moves_now <- DBI::dbGetQuery(ctn, query)
setDT(moves_now)
moves_now_orig <- moves_now

# visit history; one row for each prior encounter 

query <- readr::read_file('../app/sql/summ_hist_patients_in_ED_now.sql')
summ_hist <- DBI::dbGetQuery(ctn, query)
setDT(summ_hist)

# patient class change history

query <- readr::read_file('../app/sql/class_hist_patients_in_ED_now.sql')
patient_class_hist <- DBI::dbGetQuery(ctn, query)
setDT(patient_class_hist)

# observation data

query <- readr::read_file('../app/sql/obs_patients_in_ED_now.sql')
obs <- DBI::dbGetQuery(ctn, query)
setDT(obs)

# lab orders

query <- readr::read_file('../app/sql/lab_orders_patients_in_ED_now.sql')
lab_orders <- DBI::dbGetQuery(ctn, query)
setDT(lab_orders)


# lab results

query <- readr::read_file('../app/sql/lab_results_patients_in_ED_now.sql')
lab_results <- DBI::dbGetQuery(ctn, query)
setDT(lab_results)

# all patients in ED in the last three days - to calculate number in at any patient's arrival time
query <- readr::read_file('../app/sql/moves_patients_in_ED_at_arrival_time.sql')
all_pats_in_ED <- DBI::dbGetQuery(ctn, query)
setDT(all_pats_in_ED)


# Clean summ data --------------------------------------------------------------

print("Cleaning summary data")
# 
# # these patients have more than one row with null discharge - needs to be handled later
# moves_now[is.na(discharge), .N, by = csn][N>1]
# 
# # sometimes these patients have duplicate rows on summ table due to this problem
# summ_now[, .N, by = csn][N>1]
# 

# process summ data
summ_now <- unique(summ_now)

# filter out under 18s
summ_now = summ_now %>% left_join(demog_raw, by = "mrn") %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(date_of_birth <= "1915-01-01" ~ NA_integer_,
                         TRUE ~ as.integer(as.period(interval(start = date_of_birth, end = admission_time))$year)))

# # check for NA age eg patient had no record on demographics table
# summ_now %>% filter(is.na(age))

# flag under 18s but don't delete them
summ_now = summ_now %>% mutate(under_18 = case_when(age >= 18 ~ FALSE,
                                                    is.na(age) ~ FALSE, # retain patients with NA age for now
                                                    age <18 ~ TRUE))

# add visit history

summ_hist = summ_hist %>% left_join(patient_class_hist, by = "csn") 

prior_visits <- summ_hist %>% filter(!is.na(discharge_time)) %>% 
  select(mrn, csn, patient_class, admission_time, discharge_time) %>%  
  filter(patient_class %in% c("EMERGENCY", "INPATIENT"))  %>% 
  left_join(patient_class_hist, by = "csn") %>% 
  mutate(type = case_when(is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "planned_inpatient",
                          !is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "emergency_inpatient",
                          patient_class == "EMERGENCY" ~ "emergency_discharge"))

summ_now = summ_now %>% left_join(
  prior_visits %>% 
    group_by(mrn) %>% 
    summarise(num_adm_after_ED = sum(type == "emergency_inpatient"),
              num_ED_visits = sum(type %in% c("emergency_inpatient", "emergency_discharge"))), 
  by = "mrn"
)

summ_now <- summ_now %>% mutate_at(vars(num_adm_after_ED, num_ED_visits), replace_na, 0)

# Clean location data ------------------------------------------------------------

print("Cleaning location data")

moves_now <- clean_location_data(moves_now)

# # check
# 
# moves_now %>% filter(is.na(admission))
# moves_now %>% filter(admission == discharge)
# moves_now %>% filter(admission > discharge)

# this may return people who had a NA discharge row in addition to their latest row
# they may now show as not being in ED because their rogue ED row with null discharge has been deleted
# so delete these from both summary and moves details
summ_now = summ_now[!(csn %in% moves_now[is.na(discharge) & department != "ED", csn])]
moves_now = moves_now[!(csn %in% moves_now[is.na(discharge) & department != "ED", csn])]

# update summ_now to mark any patients who are OTF now
# this will also delete the patients who are no longer in moves_now
summ_now = left_join(summ_now, 
                     moves_now %>% filter(OTF_row == 1, 
                                          is.na(discharge)) %>% select(csn) %>% mutate(OTF_now = TRUE),
                     by = "csn"
)

# add current location to summ_now
summ_now = merge(summ_now, moves_now[is.na(discharge), .(csn, location)], by = "csn", all.x = TRUE)

print(paste("Number currently in ED:", rpt(summ_now)))
print(paste("of which number in SDEC: ", rpt(summ_now[location %in% c( "SDEC", "SDEC Waiting")])))
print(paste("and number elsewhere: ", rpt(summ_now[!(location %in% c( "SDEC", "SDEC Waiting"))])))
print(paste("Number currently in ED less than 18:", rpt(summ_now[(under_18)])))
print(paste("Number currently in ED with OTF status:", rpt(summ_now[(OTF_now)])))


summ_now = summ_now %>% mutate(exclude = case_when(OTF_now ~ TRUE, 
                                                   under_18 ~ TRUE,
                                                   TRUE ~ FALSE))

# create a moves table with only patients who are not under 18 or OTF
moves <- moves_now %>% anti_join(summ_now %>% filter(OTF_now | under_18) %>% select(csn), by = "csn") %>% 
  select(csn, admission, discharge, department, location, first_ED_admission, outside) %>% 
  arrange(csn, admission)
setkey(moves, csn)

# added unique summ here as there are duplicate rows on summ_now
# these are caused by incomplete rows on the star.location_visit table
summ <- merge(data.table(unique(summ_now) %>% 
                           select(-OTF_now, -exclude)), (unique(moves[,.(csn, first_ED_admission)])), 
              by = "csn")

# Process lab data --------------------------------------------------------

print("Cleaning lab data")

# remove lab battery orders not included in ML model
load("../model-input/lab_orders_to_include.rda")
clean_labs = clean_lab_data(lab_orders, lab_results, lab_orders_to_include, summ)

lab_orders_real = clean_labs[[1]]
lab_results_real = clean_labs[[2]]

# Process obs data --------------------------------------------------------

print("Cleaning obs data")

# read mapped names for obs codes
vo_mapping <- read_csv("../model-input/Emap Mapping Spreadsheet.csv", 
                       col_types = cols(
                         `Friendly name` = col_character(),
                         `epic id` = col_double()
                       )) %>% data.table()
obs_real = clean_obs_data(obs, summ, vo_mapping)


# Calculate number in ED at each patient's arrival time -------------------


all_pats_in_ED = clean_location_data(all_pats_in_ED)

num_MAJORS_RESUS = as.integer()
num_elsewhere = as.integer()
num_SDEC = as.integer()


for (i in 1:nrow(summ)) {
  num = all_pats_in_ED[admission < summ$admission_time[i] & 
                         (discharge > summ$admission_time[i] | is.na(discharge)) & 
                         (discharge_time > summ$admission_time[i] | is.na(discharge_time)) &
                         department %in% c("ED", "1020100166", "1020100170") & 
                         location != "OTF", .N, by = location]
  
  num[, MAJORS_RESUS := location %in% c('ED MAJORS', 'ED RESUS')]
  
  num[, SDEC := location %in% c('SDEC', 'SDEC Waiting')]
  
  num_MAJORS_RESUS = c(num_MAJORS_RESUS, sum(num[(MAJORS_RESUS), N], na.rm = TRUE))
  num_SDEC = c(num_SDEC, sum(num[(SDEC), N], na.rm = TRUE))
  num_elsewhere = c(num_elsewhere, sum(num[!(MAJORS_RESUS | SDEC), N], na.rm = TRUE))
}

in_ED_data = data.table(csn = summ$csn, first_ED_admission = summ$first_ED_admission, num_MAJORS_RESUS, num_SDEC, num_elsewhere)

# Generate timeslices -----------------------------------------------------

print("Generating timeslices")


dm <- prep_summ_for_ML(summ, time_of_extract)
loc <- prep_moves_for_ML(moves, dm)

# Add duration to obs and lab data
obs_real <- merge(obs_real, dm[,.(csn, duration)], by = "csn")
lab_orders_real <- merge(lab_orders_real, dm[,.(csn, duration)], by = "csn")
lab_results_real <- merge(lab_results_real, dm[,.(csn, duration)], by = "csn")

last_timeslice = if_else(max(summ$time_since_arrival) > 24*60, as.numeric(max(summ$time_since_arrival)), 24*60)
timeslices <- c(0, 15, 30, 60, 90, 120, 180, 240, 300, 360, 480, 720, last_timeslice)

timeslices_to_predict <- as.character()

for (i in seq(1, length(timeslices) -1, 1)) {
  print(paste0("Processing timeslice ", timeslices[i]))
  
  if (nrow(dm[duration >timeslices[i] & duration < timeslices[i+1]]) > 0) {
    ts_ <- case_when(nchar(as.character(timeslices[i])) == 1 ~ paste0("00", timeslices[i]),
                     nchar(as.character(timeslices[i])) == 2 ~ paste0("0", timeslices[i]),
                     TRUE ~ as.character(timeslices[i]))
    name_ <- paste0("dm", ts_)
    ts <- create_timeslice(loc, dm, obs_real, lab_orders_real, consults_real = NA, lab_results_real, timeslices[i], timeslices[i+1])
    
    assign(name_, ts)
    timeslices_to_predict = c(timeslices_to_predict, name_)
  }
}



# Make predictions --------------------------------------------------------

print("Making predictions")

# For shadow testing we will save results from various different models

preds_all_models <- data.table()

for (ts_ in timeslices_to_predict) {
  
  # once 720 timeslice model is available remove this length - 1
  # for (ts_ in timeslices_to_predict[1:length(timeslices_to_predict) - 1]) {
  
  print(paste0("Processing timeslice ", ts_))
  
  name_ts <- ts_
  dt = get(name_ts)
    
  for (i in 1:nrow(model_details)) {
    
    use_dataset = model_details$epoch[i]
    model_date = model_details$model_date[i]
    algo =  model_details$algo[i]
    
    # encode factors
    ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=TRUE)
    name_tsk <- gsub("dm", "task", ts_)
    
    # add other features that might be missing
    features_file  <- paste0("../model-input/ml-models/", algo, "_",model_features,
                             "_", gsub(" +", "", use_dataset), "_features_",name_tsk,"_",model_date,".rda")
    load(features_file)
    
    ts_cols = colnames(ts)
    missing_features = feature_list[!feature_list %in% ts_cols] 
    missing_features = missing_features[!grepl("adm", missing_features)]
    
    if (length(missing_features) > 0) {
      
      missing_features_NA = missing_features[grepl("latest", missing_features)]
      missing_features_0 = missing_features[!grepl("latest", missing_features)]
      
      # add columns that have zero as default
      if (length(missing_features_0) > 0) {
        missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features_0)))
        ts = bind_cols(ts, missing_cols)
        colnames(ts) = c(ts_cols, missing_features_0)
        ts_cols = colnames(ts)
      }
      
      # add columns that have NA as default
      if (length(missing_features_NA) > 0)  {
        missing_cols <- data.table(matrix(NA, nrow = nrow(ts), ncol = length(missing_features_NA)))
        ts = bind_cols(ts, missing_cols)
        colnames(ts) = c(ts_cols, missing_features_NA)
      }
    }

    
    # set epoch to post covid since this is what we are predicting
    ts$a_epoch_Post = 1
    
    # load learner
    learner_file  <-  paste0("../model-input/ml-models/", algo, "_",model_features,
                            "_", gsub(" +", "", use_dataset), "_learner_",name_tsk,"_",model_date,".rda")
    load(learner_file)
    
    # get predictions
    pred_values = as.data.table(predict(learner, ts, predict_type = "prob"))
    setnames(pred_values, c("1", "0"), c("prob.1", "prob.0"))
    pred_values$timeslice = name_tsk
    pred_values[, csn:= ts$csn]
    pred_values[, extract_dttm := time_of_extract]
    pred_values[, algo := algo]
    pred_values[, dataset := use_dataset]
    pred_values[, features := model_features]
    pred_values[, model_date := model_date]
    
    preds_all_models <- bind_rows(preds_all_models, pred_values)
    
  }
  
}


# just use one model to make predictions for app for now
preds_all_ts = preds_all_models[algo == algo_for_app & dataset == model_period_]



# Predict distribution ----------------------------------------------------

print("Creating aggregate predictions")

# get characteristics of report time
# changing this so that report is not clocked forward until a couple of hours after the extract time
get_report = case_when(as.numeric(substr(time_of_extract, 12,13)) < 7 ~ "6:00",
                       as.numeric(substr(time_of_extract, 12,13)) < 13 ~ "12:00",
                       as.numeric(substr(time_of_extract, 12,13)) < 17 ~ "16:00",
                       TRUE ~ "22:00")

is_weekend = if_else(weekdays(time_of_extract, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)

# time window until end of day
tw_end_of_day = 24 - hour(time_of_extract)
time_window_array_ = c(4, 8, tw_end_of_day)


# load file with max number of not-yet-arrived for current report time
poisson_file = "../model-input/poisson_not_yet_arrived.rda"
load(poisson_file)

# load results of poisson equations
poisson_results_file = paste0("../model-input/poisson_results_", poisson_results_date, ".rda")
load(poisson_results_file)

poisson_results_ = poisson_results[model_period == model_period_]
poisson_nya = poisson_not_yet_arrived_all[model_period == model_period_]

poisson_means = unique(get_poisson_means(time_of_extract, poisson_results_, poisson_nya, model_period_, 
                                  time_window_array_ ))


# load cox survival curve files
cox_surv_file =  paste0("../model-input/cox_surv_",cox_surv_date,".rda")
load(cox_surv_file)
cox_results_file =  paste0("../model-input/cox_results_",cox_surv_date,".rda")
load(cox_results_file)
cox_params_file = paste0("../model-input/scale_params_",cox_surv_date, ".rda")
load(cox_params_file)

base_prob = cox_surv[model_period == model_period_]
coefs = cox_results[model_period == model_period_]
scale_params_ = scale_params[model_period == model_period_]

setnames(coefs, "coefficents", "coef")

in_ED_data = get_cox_coef_by_csn(in_ED_all = in_ED_data, 
                                 coefs = coefs, 
                                 real_time = TRUE, 
                                 num_in_ED = NA, 
                                 model_period_, scale_params_)


# merge predictions with other variables
preds_all_ts = merge(preds_all_ts, unique(summ_now[, .(csn, sex, gt70 = age>70, gt65 = age>65)]), by = "csn")
preds_all_ts = merge(preds_all_ts, summ[, .(csn, first_ED_admission)], by = "csn")
preds_all_ts = merge(preds_all_ts, moves[is.na(discharge), .(csn, location)], by = "csn")
preds_all_ts$SDEC <- if_else(preds_all_ts$location %in% c("SDEC", "SDEC Waiting"), TRUE, FALSE)
preds_all_ts = merge(preds_all_ts, in_ED_data[, .(csn, epow)])


distr_all = get_prob_dist(preds_all_ts, poisson_not_yet_arrived = poisson_means, 
                          cox_results,  base_prob,
                          inc_nya = TRUE, only_nya = FALSE, time_of_extract, time_window_array_ )
distr_men = get_prob_dist(preds_all_ts[sex == "M"], poisson_not_yet_arrived = poisson_means, 
                          cox_results,   base_prob,
                          inc_nya = FALSE, only_nya = FALSE, time_of_extract, time_window_array_ )
distr_women = get_prob_dist(preds_all_ts[sex == "F"], poisson_not_yet_arrived = poisson_means, 
                            cox_results,  base_prob,
                            inc_nya = FALSE, only_nya = FALSE, time_of_extract, time_window_array_ )
distr_SDEC = get_prob_dist(preds_all_ts[(SDEC)], poisson_not_yet_arrived = poisson_means, 
                           cox_results,   base_prob, 
                           inc_nya = FALSE, only_nya = FALSE, time_of_extract, time_window_array_ )
distr_not_SDEC = get_prob_dist(preds_all_ts[(!SDEC)], poisson_not_yet_arrived = poisson_means, 
                               cox_results,  base_prob,
                               inc_nya = FALSE, only_nya = FALSE, time_of_extract, time_window_array_ )
distr_only_nya = get_prob_dist(preds_all_ts[(!SDEC)], poisson_not_yet_arrived = poisson_means, 
                               cox_results,   base_prob,
                               inc_nya = FALSE, only_nya = TRUE, time_of_extract, time_window_array_ ) 



# Get data for reference to previous day ------------------------------------------------------------------

print("Getting data on previous day's admissions")

# get patients who were in ED on the last reference day or the day before that (to allow for those admitted the day following admission)
# this may include inpatients who were rerouted to ED
# also note we are using inpatient class as the way of identifying patients who were admitted
# if these were inpatients only prior to ED, not after, they will ... ?

# # reference date is previous weekend for Sat or Sun and Friday for Monday
# if (weekdays(time_of_extract, abbreviate = TRUE) %in% c("Sat", "Sun")) {
#   search_string = substr(time_of_extract - days(7), 1,10)
# } else if (weekdays(time_of_extract, abbreviate = TRUE) %in% c("Mon")) {
#   search_string = substr(time_of_extract - days(3), 1,10)
# } else   {
#   search_string = substr(time_of_extract - days(1), 1,10)
# }

search_string = substr(time_of_extract - days(7), 1,10)


# start 48 hours before to allow for patients who were in ED for a long time
search_start = substr(as.POSIXct(search_string) - days(2), 1,10)

search_start = paste(search_start, ' 00:00:00')
search_string = paste(search_string, ' 23:59:59')

ref_day = date(search_string)
print(paste("Previous reference day for admissions:", date(search_string)))
print("Getting admissions for that day")
prev_day_moves = get_prior_adm(search_start, search_string, ctn)


# Get data for admissions so far today ------------------------------------

print("Getting data on admissions today so far")

search_string = substr(time_of_extract, 1, 19)
search_start = substr(time_of_extract- days(2), 1, 19)

print("Getting admissions so far today (Note: warnings may show here for patients with no location discharge so far")
today_moves = get_prior_adm(search_start, search_string, ctn)

# save table combining these
ref_moves = merge(prev_day_moves[!(not_tower), .(ward = department, num_admitted_last_ref_day = N)], 
                  today_moves[!(not_tower), .(ward = department, num_admitted_so_far_today = N)], 
                  by = "ward", all.x = TRUE) 
ref_moves[is.na(ref_moves)] <- 0
ref_moves = bind_rows(ref_moves, data.table(ward = "Total admissions", 
                                            num_admitted_last_ref_day = sum(ref_moves$num_admitted_last_ref_day), 
                                            num_admitted_so_far_today = sum(ref_moves$num_admitted_so_far_today)))


# Save predictions --------------------------------------------------------

print("Saving data to flow")

if (month(time_of_extract) %in% c(4, 5, 6, 7, 8, 9, 10))  {
  
  current_time = time_of_extract + hours(1)
  
} else {current_time = time_of_extract}


# prepare for save to schema

superset_summ = data.table(date_of_report = date(time_of_extract),
                           day_of_week = weekdays(time_of_extract), 
                           ref_day = ref_day, 
                           ref_day_of_week = weekdays(ref_day),
                           prepared_for_tower_report_at = get_report,
                           time_report_generated = substr(current_time, 12, 16),
                           num_patients_in_ED_and_SDEC = nrow(summ_now),
                           num_male = nrow(summ_now[sex == "M"]),
                           num_female = nrow(summ_now[sex == "F"]),
                           num_over_70 = nrow(summ_now[age > 70]),
                           num_included_in_predictions = nrow(summ_now[(!exclude)]),
                           num_under_18_excluded = nrow(summ_now[(under_18)]),
                           num_OTF_excluded = nrow(summ_now[(OTF_now)]),
                           training_dataset = model_period_,
                           ML_algo = algo_for_app,
                           model_date = model_date)

target_table <- 'ed_predictor_report_stats'
target_table_path <- DBI::Id(schema=target_schema, table=target_table)
DBI::dbWriteTable(ctn, name=target_table_path, value=superset_summ, overwrite = TRUE)


preds_to_save = merge(summ_now, preds_all_ts[, .(csn, timeslice, prob_adm = round(prob.1,2), extract_dttm,
                                                 algo, dataset, model_date)], all.x = TRUE)
preds_to_save[, time_since_arrival := round(as.numeric(time_since_arrival),0)]
preds_to_save_all_models = merge(summ_now, preds_all_models[, .(csn, timeslice, prob_adm = prob.1, extract_dttm,
                                                                algo, dataset, features, model_date)], all.x = TRUE)
preds_to_save_all_models[, time_since_arrival := round(as.numeric(time_since_arrival),0)]
agg_preds_to_save = bind_rows(distr_all[, .(num_adm_pred, prob_this_num, time_window, inc_nya, dist, contrib = "All patients")],
                              distr_SDEC[, .(num_adm_pred, prob_this_num, time_window, inc_nya, dist, contrib = "Patients in SDEC")],
                              distr_not_SDEC[, .(num_adm_pred, prob_this_num, time_window, inc_nya, dist, contrib = "Patients elsewhere in ED")],
                              distr_only_nya[, .(num_adm_pred, prob_this_num, time_window, inc_nya, dist, contrib = "Patients not yet arrived")])
agg_preds_to_save$extract_dttm = time_of_extract
agg_preds_to_save[, description := case_when(time_window == 4 ~ "Num beds needed in 4 hours",
                                             time_window == 8 ~ "Num beds needed in 8 hours",
                                             TRUE ~ "Num beds needed by midnight")]

# write most recent predictions - csn level
target_table <- 'ed_predictor_csn'
target_table_path <- DBI::Id(schema=target_schema, table=target_table)
DBI::dbWriteTable(ctn, name=target_table_path, value=preds_to_save, overwrite = TRUE)

# append to history of all predictions - csn level - note that this saves predictions from all models
target_table <- 'ed_predictor_csn_audit'
target_table_path <- DBI::Id(schema=target_schema, table=target_table)
DBI::dbWriteTable(ctn, name=target_table_path, value=preds_to_save_all_models, append=TRUE, overwrite = FALSE)

# write most recent predictions - aggregate level
target_table <- 'ed_predictor_agg'
target_table_path <- DBI::Id(schema=target_schema, table=target_table)
DBI::dbWriteTable(ctn, name=target_table_path, value=agg_preds_to_save, overwrite = TRUE)

# append to history of all predictions - aggregate level
target_table <- 'ed_predictor_agg_audit'
target_table_path <- DBI::Id(schema=target_schema, table=target_table)
DBI::dbWriteTable(ctn, name=target_table_path, value=agg_preds_to_save, append=TRUE, overwrite = FALSE)

# write details on number admitted on last reference day
target_table <- 'ed_predictor_adm_ref_day'
target_table_path <- DBI::Id(schema=target_schema, table=target_table)
DBI::dbWriteTable(ctn, name=target_table_path, value=ref_moves, overwrite = TRUE)

# save number admitted on last ref for reference
ref_moves_audit = ref_moves[ward == "Total admissions"]
ref_moves_audit$extract_dttm = time_of_extract
ref_moves_audit[, ward := NULL]
target_table <- 'ed_predictor_adm_ref_day_audit'
target_table_path <- DBI::Id(schema=target_schema, table=target_table)
DBI::dbWriteTable(ctn, name=target_table_path, value=ref_moves_audit, append=TRUE, overwrite = FALSE)

# dbDisconnect(ctn)

# Create charts --------------------------------------------------------------

print("Creating charts")

chart_colours = c("#f10806", "#f8696b", "#fbbe1c",  "#94d353",   "#d7d7d7")

distr_all = format_dist_for_charts(distr_all, chart_colours, tw_end_of_day)
distr_not_SDEC = format_dist_for_charts(distr_not_SDEC, chart_colours, tw_end_of_day)
distr_SDEC = format_dist_for_charts(distr_SDEC, chart_colours, tw_end_of_day)
distr_only_nya = format_dist_for_charts(distr_only_nya, chart_colours, tw_end_of_day)
distr_both = format_gender_dist_for_charts(distr_men, distr_women, distr_all, chart_colours, tw_end_of_day)

# chart of overall predictions - discrete colour gradient
# colours from bed planning report from https://imagecolorpicker.com/en
# green - #94d353
# yellow - #fbbe1c
# red - #f10806
# pale red - #f8696b
# blue - #00b0f0
# dark grey - #848484
# light grey - #d7d7d7



# chart for admissions within time window (three and one format)

for (tw in c(4, 8, tw_end_of_day)) {

  time_window_ = tw
  mainplot = get_main_plot_for_time_window (time_window_, distr_all, chart_colours)
  subplots = get_subplots_for_time_window(time_window_, distr_SDEC, distr_not_SDEC, distr_only_nya)
  plot_tw_hours <- add_ggplot( plot_grid(mainplot + theme(legend.position = "none"), subplots, ncol = 2, rel_widths = c(1/4, 3/4)), width = 5, height = 2)
  name = paste0("plot_", tw, "_hours")
  assign(name, plot_tw_hours)

  if (tw %in% c (4,8)) {

    total_preds_text = paste0(
      "  ## Predicted number of beds needed in ", tw, " hours
  The figure below shows how the ", tw, " hour predictions are made up from patients in ED, in SDEC and those who have not yet arrived.
  \n\n "
    )

    name = paste0("total_preds_", tw, "_hours")
    assign(name, total_preds_text)

  } else {

    total_preds_text = paste0(
      "  ## Predicted number of beds needed by midnight
  The figure below shows how the by midnight predictions are made up from patients in ED, in SDEC and those who have not yet arrived.
  \n\n "
    )

    name = paste0("total_preds_by_midnight")
    assign(name, total_preds_text)

  }



}



# Send predictions --------------------------------------------------------

print("Composing and sending email")


proxy <- # withheld

# parameters for email


diff = ref_moves[ward == "Total admissions", num_admitted_last_ref_day] - ref_moves[ward == "Total admissions", num_admitted_so_far_today]

if (tw_end_of_day == 8) {
  prob_needing_diff = distr_all[num_adm_pred == diff & time_window_text == "In 8 hours", prob_discrete]
  
} else if (tw_end_of_day == 4)  {
  prob_needing_diff = distr_all[num_adm_pred == diff & time_window_text == "In 4 hours", prob_discrete]
  
} else {
  prob_needing_diff = distr_all[num_adm_pred == diff & time_window_text == "By midnight", prob_discrete]
}


if (diff > 0) {
  diff_text = paste0("The difference between these numbers is **", diff,"**. \n\n Below are predictions of the number of beds needed for emergency patients. These predictions were generated at ", 
                     substr(current_time,1, 16),      ". 
                   Our model suggests the probability of needing at least ", diff, " beds, for adult patients
                   who have not yet been assigned a bed, between now and midnight tonight is **", prob_needing_diff,
                     "**.")
  
} else {
  diff_text = "**The number of admissions so far today has already exceeded that number.** "
}


closing_text = "\n\n ## Further information
We have also created a dashboard where you can view patient-level detail. For access to this,
and **for more information about how these predictions were created, please contact Zella King on zella.king@ucl.ac.uk**. And please let us know
if a different way of presenting the information would be more useful to you."


if (distr_both[time_window %in% c(4, 8) & cdf <.9, .SD[which.max(cdf)]][,num_adm_pred] < 15) {
  cut_at = 15
} else {
  cut_at = 25
}


# Create a plot that shows aggregate demand by SDEC and non-SDEC without time window
distr_SDEC_no_tw = format_dist_for_charts(get_prob_dist(preds_all_ts[(SDEC)], poisson_not_yet_arrived = poisson_means, 
                                 cox_results,   base_prob, 
                                 inc_nya = FALSE, only_nya = FALSE, time_of_extract, time_window_array_ = NA),
                                 chart_colours, tw_end_of_day = NA)
distr_SDEC_no_tw$group = "Patients in SDEC"

distr_not_SDEC_no_tw = format_dist_for_charts(get_prob_dist(preds_all_ts[(!SDEC)], poisson_not_yet_arrived = poisson_means, 
                                     cox_results,  base_prob,
                                     inc_nya = FALSE, only_nya = FALSE, time_of_extract, time_window_array_ = NA),
                                chart_colours, tw_end_of_day = NA)
distr_not_SDEC_no_tw$group = "Patients elsewhere in ED"

plot_no_tw = bind_rows(distr_SDEC_no_tw[num_adm_pred <= cut_at], distr_not_SDEC_no_tw[num_adm_pred <= cut_at])  %>% 
  ggplot(aes(x = num_adm_pred, y = 1, fill = prob_discrete)) + geom_col() +
  facet_rep_wrap(group~., ncol = 2, repeat.tick.labels = TRUE) + 

  coord_cartesian(xlim = c(0,20), default = TRUE)  +

  coord_flip() +
  labs(title = paste("Beds needed at some point for patients currently in ED & SDEC"),
       subtitle = "Excludes beds for patients under 18, and patients marked as OTF",
       x = "Number of beds", 
       fill = "Probability") +
  theme_classic() + theme(text = element_text(size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "right") +
  scale_fill_manual(values = chart_colours) + 
  guides(fill = guide_legend(reverse=T))
plot_no_tw_preds <- add_ggplot(plot_no_tw, width = 5)


# Create a plot that shows agregate demand by sub group without time window

distr_subgroup_no_tw = data.table()


for (sex_ in c("M", "F")) {
  for (gt65_ in c(FALSE, TRUE)) {
    if (nrow(preds_all_ts[sex == sex_ & gt65 == gt65_]) > 0) {
      distr_subgroup = format_dist_for_charts(get_prob_dist(preds_all_ts[sex == sex_ & gt65 == gt65_], poisson_not_yet_arrived = poisson_means, 
                                     cox_results,   base_prob,
                                     inc_nya = FALSE, only_nya = FALSE, time_of_extract, time_window_array_ = NA),
                                     chart_colours, tw_end_of_day = NA)
      distr_subgroup = unique(distr_subgroup)
      distr_subgroup[, text_for_subgroup := case_when(sex_ == "M" & gt65_ ~ "Male > 65",
                                                      sex_ == "M" & !gt65_ ~ "Male <= 65",
                                                      sex_ == "F" & gt65_ ~ "Female > 65",
                                                      sex_ == "F" & !gt65_ ~ "Female <= 65")]
      distr_subgroup_no_tw = bind_rows(distr_subgroup_no_tw, distr_subgroup)
    }
  }
}



if (distr_subgroup_no_tw[cdf <.9, .SD[which.max(cdf)]][,num_adm_pred] < 15) {
  cut_at = 15
} else {
  cut_at = 25
}



plot_subgroup_no_tw = distr_subgroup_no_tw[num_adm_pred <=  cut_at]  %>% 
  # plot_gender = distr_both[time_window %in% c(4, 8) & num_adm_pred <=  cut_at]  %>% 
  ggplot(aes(x = num_adm_pred, y = 1, fill = prob_discrete)) + geom_col() +
  # scale_fill_gradient2(low = "yellow", high = "red", limits = c(0,1), breaks = seq(0,1, .25)) + 
  facet_rep_wrap(text_for_subgroup~., ncol = 4, repeat.tick.labels = TRUE) + 
  # this gets zero bar but then can't set scale - get around this by capping scale at 60
  # but also can't set access breaks
  coord_cartesian(xlim = c(0,20), default = TRUE)  +
  # scale_x_continuous(limits = c(0,20), breaks = seq(0,20,5)) +
  
  coord_flip() +
  labs(title = paste("Beds needed by sex and age group"),
       subtitle = "Excludes beds needed for patients under 18, and patients marked as OTF",
       x = "Number of beds", 
       fill = "Probability") +
  theme_classic() + theme(text = element_text(size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "right") +
  scale_fill_manual(values = chart_colours) + 
  guides(fill = guide_legend(reverse=T))

plot_subgroup_no_tw_preds <- add_ggplot(plot_subgroup_no_tw, width = 5)




intro_text = paste0(
  "There are currently **", 
  nrow(summ_now[!(location %in% c("SDEC", "SDEC Waiting"))]), "** patients in ED, and **",
  nrow(summ_now[location %in% c("SDEC", "SDEC Waiting")]), "** in SDEC. Of the ", 
  nrow(summ_now),
  " patients, **", 
  nrow(summ_now[sex == "F"]),
  "** are female and **",
  nrow(summ_now[sex == "M"]),
  "** are male. There are **",
  nrow(summ_now[age >= 65]),
  "** patients aged 65 or over and **",
  nrow(summ_now[(under_18)]),
  "** are under 18. The predictions below exclude these ",
  nrow(summ_now[(under_18)]),
  " under 18s, and also exclude **",
  nrow(summ_now[(OTF_now)]),
  "** marked as OTF. \n\n


  Last ", weekdays(ref_day),", **", ref_moves[ward == "Total admissions", num_admitted_last_ref_day], "**
  patients were admitted. So far today, **", ref_moves[ward == "Total admissions", num_admitted_so_far_today], "**
  have been admitted. ", diff_text, " \n\n

  "
)



total_preds_no_tw_text = paste0(
  "  ## How many beds will be needed for patients who are in ED and SDEC now? 
The figure below shows predictions for number of beds needed for 
adult patients who are currently in ED and SDEC. It excludes beds for patients who have not yet arrived. 
  The colour shows the probability that each number of beds will be needed. 
**Please bear in mind that these predictions are provisional.** 
  \n\n "
)

subgroup_preds_text = paste0(
  " \n\n  ## How many male/female and under/over 65 beds will be needed? 
The figure below shows predictions for number of beds needed by males and females aged over 65, and those who are 65 or under. 
 It excludes beds for patients who have not yet arrived.
  \n\n "
)

total_preds_in_tw_text = paste0(
  "  ## When will emergency patients be admitted? 
Based on past patterns for the time it takes patients to be admitted, and past numbers of new arrivals at ED,
the figures below show predictions for how many patients will be admitted from ED within 4 hours and 8 hours from now.
  The right hand side of each figure shows a breakdown of the overall figure into patients currently in ED, patients currently in SDEC, and patients who have not yet arrived. 
  \n\n "
)


email2 <- compose_email(
  body = md(c(
    "# Predicted admissions from ED and SDEC", intro_text, 
    total_preds_no_tw_text,   plot_no_tw_preds, "\n\n ",
    subgroup_preds_text, plot_subgroup_no_tw_preds, "\n\n ",
    total_preds_in_tw_text, plot_4_hours, plot_8_hours, closing_text
  ))
)
email2

to_emails = strsplit(Sys.getenv("TO_EMAILS"), ",")[[1]]

subject = paste("ED predictions at ", current_time)

email2 %>% smtp_send(
  from = "zella.king@ucl.ac.uk",
  to = to_emails,
  subject = subject,
  credentials = creds(
    host = "UMAILRELAY1",
    port = 25,
    use_ssl = FALSE),
)

