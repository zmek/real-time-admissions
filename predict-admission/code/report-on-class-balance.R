# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)



# Report on class balance -------------------------------------------------


load(paste0("EDcrowding/predict-admission/data-raw/dm_",dm_file_date,".rda"))
load(paste0("EDcrowding/flow-mapping/data-raw/summ_", summ_file_date,".rda"))
summ[, model_period := if_else(first_ED_admission < covid_start, "Pre Covid", "Post Covid")]

dm = merge(dm, summ[, .(csn, model_period)], by = "csn")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")



class_balance = data.table()
for (ts_ in timeslices) {
  ts_bal = dm[duration > as.numeric(ts_), .N, by = .(model_period, adm)]
  ts_bal[, timeslice := ts_]
  class_balance = bind_rows(class_balance, ts_bal)
}

odds = class_balance %>% pivot_wider(names_from = adm, values_from = N) %>% mutate(odds = `1`/`0`)

class_balance[, sum(N), by = .(model_period, timeslice)] 

write.csv(class_balance[, sum(N), by = .(timeslice, model_period )], file = paste0("EDcrowding/dissertation/data-output/class_balance_", model_file_date,".csv"),
          row.names = FALSE)


# Pre covid
p_pre = class_balance %>% filter(model_period == "Pre") %>% 
  ggplot(aes(x = timeslice, y = N, fill = as.factor(adm))) + geom_bar(stat= "identity", position = "stack") + 
  theme(legend.position = "none") +
  labs(title = "Class balance for timeslices",
       subtitle = "Pre Covid")
odds_pre = odds %>% filter(model_period == "Pre")
for (i in 1:nrow(odds)) {
  p_pre = p_pre +  annotate(geom = "text", x = odds_pre$timeslice[i], y = 10000, label = round(odds_pre$odds[i],2), size = 4)
}
p_pre = p_pre + annotate(geom = "text", x = odds_pre$timeslice[1], y = 20000, label = "Odds of admission", size = 4, hjust = 0.1)
# p_pre

# Post covid
p_post = class_balance %>% filter(model_period == "Post") %>% 
  ggplot(aes(x = timeslice, y = N, fill = as.factor(adm))) + geom_bar(stat= "identity", position = "stack")  + 
  theme(legend.position = "bottom") +
  labs(subtitle = "Post Covid",
       fill = "Admitted")
  
odds_post = odds %>% filter(model_period == "Post")
for (i in 1:length(timeslices)) {
  p_post = p_post +  annotate(geom = "text", x = odds_post$timeslice[i], y = 10000, label = round(odds_post$odds[i],2), size = 4)
}
p_post = p_post + annotate(geom = "text", x = odds_post$timeslice[1], y = 20000, label = "Odds of admission", size = 4, hjust = 0.1)
# p_post

# library(gridExtra)
grid.arrange(p_pre, p_post, nrow = 2)



