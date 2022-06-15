library(dplyr)
library(data.table)
library(tidyverse)
library(lubridate)

# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")

# Load data ---------------------------------------------------------------



# add config file that will load any parameters needed
source("EDcrowding/predict-admission/code/config.R")

# add utils file which will set training, val and test set date ranges
source("EDcrowding/predict-admission/code/utils.R")

if (summ_file_date_for_checking_real_time > summ_file_date) {
  
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date_for_checking_real_time,".rda"))
} else {
  
  load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
}





# Get admitted patients only ----------------------------------------------


admitted = summ[adm %in% c("direct_adm", "indirect_adm")]

# temp deletion of admitted with NA in first_outside_proper_admission
admitted = admitted[!is.na(first_outside_proper_admission)]

# round down duration to 2 decimal places to reduce the number of times to compute survival analysis
admitted[, duration := round(difftime(left_ED, first_ED_admission, units = "hours"), digits = 2)]
admitted[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]

admitted[, quarter := factor(case_when( month(first_ED_admission) <= 3 ~ 1,
                                        month(first_ED_admission) <= 6 ~ 2, 
                                        month(first_ED_admission) <= 9 ~ 3, 
                                        month(first_ED_admission) <= 12 ~ 4))]
# admitted[, year := factor(year(first_ED_admission))]
admitted[, weekend := factor(if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]
# the lab closes at 10 pm 
# admitted[, night := factor(ifelse(hour(first_ED_admission) < 22 & hour(first_ED_admission) > 7, 0, 1))]

# # code below gets the same results as the section below that
# admitted[, get_report := case_when(as.numeric(substr(first_ED_admission, 12,13)) <= 5 ~ "6:00",
#                        as.numeric(substr(first_ED_admission, 12,13)) <= 11 ~ "12:00",
#                        as.numeric(substr(first_ED_admission, 12,13)) <= 15 ~ "16:00",
#                        as.numeric(substr(first_ED_admission, 12,13)) <= 21 ~ "22:00",
#                        TRUE ~ "6:00")]


admitted[, arrival_time := factor(case_when(hour(first_ED_admission) >= 22 | hour(first_ED_admission) < 6 ~ "22:00-06:00",
                                            hour(first_ED_admission) >= 6 & hour(first_ED_admission) < 12 ~ "06:00-12:00",
                                            hour(first_ED_admission) >= 12 & hour(first_ED_admission) < 16 ~ "12:00-16:00",
                                            hour(first_ED_admission) >= 16 & hour(first_ED_admission) < 22 ~ "16:00-22:00"))]




# Calculate historical admissions by day ---------------------------------------------


# rolling average
adm_day = admitted[, .N, by = date(first_outside_proper_admission)] 
setorderv(adm_day, "date")
adm_day[, day_of_week := weekdays(date)]
adm_day[, six_wk_moving := frollmean(N, 6), by = .(day_of_week)]

# last week
adm_day[, last_wk := lag(N), by = .(day_of_week)]

# last reference day
# reference date is previous weekend for Sat or Sun and Friday for Monday

# adm_day[, last_ref_day := case_when(day_of_week == "Monday" ~ lag(N, 3),
#                                     day_of_week == "Saturday" ~ lag(N, 7),
#                                     TRUE ~ lag(N))]

adm_day[, last_ref_day := lag(N, 7)]

adm_day[, weekend := if_else(day_of_week %in% c("Saturday", "Sunday"), "Weekends", "Weekdays")]



# Calculate admissions by report time -----------------------------

# Not sure why I did this, but leaving code here in case useful

time_of_report = c(6, 12, 16, 22)

adm_report_time = data.table()

for (j in 1:nrow(adm_day)) {
  
  if (j %% 100 == 0) { print(paste("Processed", j, "dates"))}

  for (i in 1:4) {

    for (time_window_ in c(4, 8))  {

      time_window_start = as.POSIXct(adm_day$date[j]) + hours(time_of_report[i])
      time_window_end = time_window_start + hours(time_window_)
      
      if (dst(time_window_start)) { time_window_start = time_window_start - hours(1) }
      if (dst(time_window_end)) { time_window_end = time_window_end - hours(1) }



      num_adm_in_time_window = admitted[first_outside_proper_admission > time_window_start &
                                          first_outside_proper_admission <= time_window_end, .N]
      
      num_adm_so_far_today = admitted[first_outside_proper_admission > adm_day$date[j] &
                                        first_outside_proper_admission <= time_window_start, .N]
      
      num_adm_by_midnight = admitted[first_outside_proper_admission > time_window_start &
                                                    first_outside_proper_admission <= adm_day$date[j+1], .N]

      num_adm = data.table(date = adm_day$date[j],
                           time_of_report = time_window_start,
                           time_window = time_window_,
                           num_adm = num_adm_in_time_window, 
                           num_adm_so_far_today = num_adm_so_far_today,
                           num_adm_by_midnight = num_adm_by_midnight)

      adm_report_time = bind_rows(adm_report_time, num_adm)

    }
  }
}


adm_report_time[, report_time := substr(time_of_report, 12,16)]
adm_report_time[, report_time := case_when(report_time %in% c("05:00", "07:00") ~ "06:00",
                                              report_time %in% c("11:00", "13:00") ~ "12:00",
                                              report_time %in% c("15:00", "17:00") ~ "16:00",
                                              report_time %in% c("21:00", "23:00") ~ "22:00",
                                              TRUE ~ report_time)]



# Calc UCH method ---------------------------------------------------------


adm_report_time = merge(adm_report_time, adm_day[, .(date, last_ref_day, six_wk_moving)], by = "date", all.x = TRUE)

adm_report_time[, uch_metric := last_ref_day - num_adm_so_far_today]
adm_report_time[, moving_avg_less_adm_so_far := six_wk_moving - num_adm_so_far_today]

# Save data ---------------------------------------------------------------

outFile = (paste0("EDcrowding/predict-admission/model-output/benchmarks_by_day_",today(),".rda"))
save(adm_day, file = outFile)

outFile = (paste0("EDcrowding/predict-admission/model-output/benchmarks_by_report_time_",today(),".rda"))
save(adm_report_time, file = outFile)



# Charts ------------------------------------------------------------------

load("EDcrowding/predict-admission/model-output/benchmarks_by_day_2021-08-17.rda")
load("EDcrowding/predict-admission/model-output/benchmarks_by_report_time_2021-08-17.rda")


# adm_day[date < "2021-05-01"] %>% ggplot(aes(x = N)) + 
#   geom_histogram( binwidth = 2, alpha = 0.6 , color="#e9ecef") +
#   labs(title = "Histogram of daily admissions from ED, 1 May 2019 - 30 April 2021", 
#        x = "Number of admissions")

# compare rolling 6 with
adm_day


p = adm_report_time[date < "2021-05-01"] %>% ggplot(aes(x = num_adm)) +
  geom_histogram( binwidth = 2, alpha = 0.6 , color="#e9ecef") +
  labs(title = "Histogram of admissions from ED, 1 May 2019 - 30 April 2021", 
       subtitle = "Subplots show four report times, and two time windows of 4 and 8 hours ",
       x = "Number of admissions within time window") +
  facet_grid(report_time~time_window) 

plot_chart(p, "Paper-hist-num-adm-by-tw-and-report-time")

