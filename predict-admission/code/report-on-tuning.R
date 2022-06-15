# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)


# Load tuning results  ---------------------------------------------------------------

model_date <- "2021-05-24"
model_features = "alop"
use_dataset = "Post"

scores_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features, "_scores_",today(),".rda")
load(scores_file)
scores <- data.table(scores)
scores[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]


scores_long_nroundsfile <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features, "_scores_long_nrounds", model_date,".rda")
load(scores_long_nroundsfile)





# Report on class balance -------------------------------------------------


load("EDcrowding/predict-admission/data-raw/dm_2021-05-19.rda")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")

class_balance = data.table()
for (ts_ in timeslices) {
  ts_bal = dm[duration > as.numeric(ts_), .N, by = .(a_epoch, adm)]
  ts_bal[, timeslice := ts_]
  class_balance = bind_rows(class_balance, ts_bal)
}

odds = class_balance %>% pivot_wider(names_from = adm, values_from = N) %>% mutate(odds = `1`/`0`)

# Pre covid
p_pre = class_balance %>% filter(a_epoch == "Pre") %>% 
  ggplot(aes(x = timeslice, y = N, fill = as.factor(adm))) + geom_bar(stat= "identity", position = "stack") + 
  theme(legend.position = "none") +
  labs(title = "Class balance for timeslices",
       subtitle = "Pre Covid")
odds_pre = odds %>% filter(a_epoch == "Pre")
for (i in 1:nrow(odds)) {
  p_pre = p_pre +  annotate(geom = "text", x = odds_pre$timeslice[i], y = 10000, label = round(odds_pre$odds[i],2), size = 4)
}
p_pre = p_pre + annotate(geom = "text", x = odds_pre$timeslice[1], y = 20000, label = "Odds of admission", size = 4, hjust = 0.1)
# p_pre

# Post covid
p_post = class_balance %>% filter(a_epoch == "Post") %>% 
  ggplot(aes(x = timeslice, y = N, fill = as.factor(adm))) + geom_bar(stat= "identity", position = "stack")  + 
  theme(legend.position = "bottom") +
  labs(subtitle = "Post Covid",
       fill = "Admitted")
  
odds_post = odds %>% filter(a_epoch == "Post")
for (i in 1:length(timeslices)) {
  p_post = p_post +  annotate(geom = "text", x = odds_post$timeslice[i], y = 10000, label = round(odds_post$odds[i],2), size = 4)
}
p_post = p_post + annotate(geom = "text", x = odds_post$timeslice[1], y = 20000, label = "Odds of admission", size = 4, hjust = 0.1)
# p_post

# library(gridExtra)
grid.arrange(p_pre, p_post, nrow = 2)




# Evaluate nrounds --------------------------------------------------------


print ("Best nrounds results:")

print(scores[tsk_ids == "val" & tuning_round == "nrounds" & model_features == model_features & dataset == use_dataset
             , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, nrounds)])

scores_long_nrounds = data.table(scores[tsk_ids == "val" & tuning_round == "nrounds" & model_features == model_features, 
                                        .(dataset, timeslice, logloss, prauc, auc, nrounds)] %>%
                                   pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))

order_scores = c("dataset", "timeslice", "metric", "nrounds")
setorderv(scores_long_nrounds, order_scores)
scores_long_nrounds[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
scores_long_nrounds[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
scores_long_nrounds[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
scores_long_nrounds[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good


scores_long_nrounds %>%  filter(metric == "logloss") %>%
  ggplot(aes(x = nrounds, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice) +
  labs(y = "Score", title = "Results of tuning nrounds of XGBoost for each timeslice: logloss",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")

scores_long_nrounds %>%  filter(metric == "prauc") %>%
  ggplot(aes(x = nrounds, y = score)) + geom_line() + geom_point( aes(col = gt0.005)) + facet_grid(dataset ~ timeslice) +
  labs(y = "Score", title = "Results of tuning nrounds of XGBoost for each timeslice: prauc",
       col = "Greater than 0.05 increase in area under precision recall curve") +
  theme(legend.position = "bottom")


# explore best nrounds which offer logloss delta of more than 0.05
scores_long_nrounds[(lt0.005 | is.na(lt0.005)) & metric == "logloss", .SD[which.min(score)], 
                    by = .(dataset, timeslice)]

# this suggests I can cap nrounds at 30 




# Evaluate nrounds --------------------------------------------------------


print ("Best nrounds results:")

print(scores[tsk_ids == "val" & tuning_round == "nrounds" & model_features == model_features & dataset == use_dataset
             , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, nrounds)])

scores_long_nrounds = data.table(scores[tsk_ids == "val" & tuning_round == "nrounds" & model_features == model_features, 
                                        .(dataset, timeslice, logloss, prauc, auc, nrounds)] %>%
                                   pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))

order_scores = c("dataset", "timeslice", "metric", "nrounds")
setorderv(scores_long_nrounds, order_scores)
scores_long_nrounds[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
scores_long_nrounds[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
scores_long_nrounds[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
scores_long_nrounds[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good


scores_long_nrounds %>%  filter(metric == "logloss") %>%
  ggplot(aes(x = nrounds, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice) +
  labs(y = "Score", title = "Results of tuning nrounds of XGBoost for each timeslice: logloss",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")

scores_long_nrounds %>%  filter(metric == "prauc") %>%
  ggplot(aes(x = nrounds, y = score)) + geom_line() + geom_point( aes(col = gt0.005)) + facet_grid(dataset ~ timeslice) +
  labs(y = "Score", title = "Results of tuning nrounds of XGBoost for each timeslice: prauc",
       col = "Greater than 0.05 increase in area under precision recall curve") +
  theme(legend.position = "bottom")


# explore best nrounds which offer logloss delta of more than 0.05
scores_long_nrounds[(lt0.005 | is.na(lt0.005)) & metric == "logloss", .SD[which.min(score)], 
                    by = .(dataset, timeslice)]

# this suggests I can cap nrounds at 30 



# Evaluate tune trees -----------------------------------------------------

print(scores[tsk_ids == "val" & tuning_round %in% c("tune_trees") & model_features == model_features
             , .SD[which.min(logloss)], by = list(dataset, timeslice)][,.(dataset, timeslice, tuning_round, max_depth, min_child_weight, logloss)])

# # difficult to look at two increments at once
# 
# scores_long_tune_trees = data.table(scores[tsk_ids == "val" & tuning_round == "tune_trees" & model_features == model_features, 
#                                         .(dataset, timeslice, logloss, prauc, auc, max_depth, min_child_weight)] %>%
#                                    pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))
# 
# scores_long_tune_trees[, params := paste(max_depth, "|", min_child_weight)]
# 
# order_scores = c("dataset", "timeslice", "metric", "score")
# setorderv(scores_long_tune_trees, order_scores)
# scores_long_tune_trees[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
# scores_long_tune_trees[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
# scores_long_tune_trees[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
# scores_long_tune_trees[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good
# 
# 
# scores_long_tune_trees %>%  filter(metric == "logloss") %>%
#   ggplot(aes(x = params, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice)
# 
# # now saving scores file as scores_with_full_tune_trees
# 
# scores_file <- paste0("EDcrowding/predict-admission/data-output/xgb_",model_features, "_scores_with_full_tune_trees_",today(),".rda")
# save(scores, file = scores_file)


# Evaluate max_depth ------------------------------------------------------


# trying just max_depth
scores_long_max_depth = data.table(scores[tsk_ids == "val" & tuning_round == "max_depth" & model_features == model_features,
                                           .(dataset, timeslice, logloss, prauc, auc, max_depth)] %>%
                                      pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))


order_scores = c("dataset", "timeslice", "metric", "max_depth")
setorderv(scores_long_max_depth, order_scores)
scores_long_max_depth[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
scores_long_max_depth[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
scores_long_max_depth[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
scores_long_max_depth[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good


scores_long_max_depth[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]

scores_long_max_depth %>%  filter(metric == "logloss") %>%
  ggplot(aes(x = max_depth, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice)  +
  labs(y = "Score", title = "Results of tuning max depth of XGBoost for each timeslice: logloss",
       subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


scores_long_max_depth %>%  filter(metric == "prauc") %>%
  ggplot(aes(x = max_depth, y = score)) + geom_line() + geom_point( aes(col = gt0.005)) + facet_grid(dataset ~ timeslice)  +
  labs(y = "Score", title = "Results of tuning max depth of XGBoost for each timeslice: prauc",
       subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


# explore best max_depth which offer logloss delta of more than 0.05
scores_long_max_depth[(lt0.005 | is.na(lt0.005)) & metric == "logloss", .SD[which.min(score)], 
                    by = .(dataset, timeslice)]



# Evalute min_child_weight ------------------------------------------------


scores_long_min_child_weight = data.table(scores[tsk_ids == "val" & tuning_round == "min_child_weight" & model_features == model_features,
                                                 .(dataset, timeslice, logloss, prauc, auc, min_child_weight)] %>%
                                            pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))


order_scores = c("dataset", "timeslice", "metric", "min_child_weight")
setorderv(scores_long_min_child_weight, order_scores)
scores_long_min_child_weight[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
scores_long_min_child_weight[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
scores_long_min_child_weight[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
scores_long_min_child_weight[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good


scores_long_min_child_weight[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]

scores_long_min_child_weight %>%  filter(metric == "logloss") %>%
  ggplot(aes(x = min_child_weight, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice)  +
  scale_x_continuous(breaks = c(3,4,5)) +
  labs(y = "Score", title = "Results of tuning min_child_weight of XGBoost for each timeslice: logloss",
       subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


scores_long_min_child_weight %>%  filter(metric == "prauc") %>%
  ggplot(aes(x = min_child_weight, y = score)) + geom_line() + geom_point( aes(col = gt0.005)) + facet_grid(dataset ~ timeslice)  +
  scale_x_continuous(breaks = c(3,4,5)) +
  labs(y = "Score", title = "Results of tuning min_child_weight of XGBoost for each timeslice: prauc",
       subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


# explore best min_child_weight which offer logloss delta of more than 0.05
scores_long_min_child_weight[(lt0.005 | is.na(lt0.005)) & metric == "logloss", .SD[which.min(score)], 
                      by = .(dataset, timeslice)]




# Evaluate samples --------------------------------------------------------

scores_long_subsample = data.table(scores[tsk_ids == "val" & tuning_round == "subsample" & model_features == model_features,
                                                 .(dataset, timeslice, logloss, prauc, auc, subsample)] %>%
                                            pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))


order_scores = c("dataset", "timeslice", "metric", "subsample")
setorderv(scores_long_subsample, order_scores)
scores_long_subsample[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
scores_long_subsample[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
scores_long_subsample[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
scores_long_subsample[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good


scores_long_subsample[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]

scores_long_subsample %>%  filter(metric == "logloss") %>%
  ggplot(aes(x = subsample, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice)  +
  labs(y = "Score", title = "Results of tuning subsample of XGBoost for each timeslice: logloss",
       # subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


scores_long_subsample %>%  filter(metric == "prauc") %>%
  ggplot(aes(x = subsample, y = score)) + geom_line() + geom_point( aes(col = gt0.005)) + facet_grid(dataset ~ timeslice)  +
  labs(y = "Score", title = "Results of tuning subsample of XGBoost for each timeslice: prauc",
       # subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


# explore best subsample which offer logloss delta of more than 0.05
scores_long_subsample[(lt0.005 | is.na(lt0.005)) & metric == "logloss", .SD[which.min(score)], 
                             by = .(dataset, timeslice)]



# Evaluate colsamples --------------------------------------------------------

scores_long_colsample_bytree = data.table(scores[tsk_ids == "val" & tuning_round == "colsample_bytree" & model_features == model_features,
                                          .(dataset, timeslice, logloss, prauc, auc, colsample_bytree)] %>%
                                     pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))


order_scores = c("dataset", "timeslice", "metric", "colsample_bytree")
setorderv(scores_long_colsample_bytree, order_scores)
scores_long_colsample_bytree[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
scores_long_colsample_bytree[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
scores_long_colsample_bytree[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
scores_long_colsample_bytree[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good


scores_long_colsample_bytree[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]

scores_long_colsample_bytree %>%  filter(metric == "logloss") %>%
  ggplot(aes(x = colsample_bytree, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice)  +
  labs(y = "Score", title = "Results of tuning colsample_bytree of XGBoost for each timeslice: logloss",
       # subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


scores_long_colsample_bytree %>%  filter(metric == "prauc") %>%
  ggplot(aes(x = colsample_bytree, y = score)) + geom_line() + geom_point( aes(col = gt0.005)) + facet_grid(dataset ~ timeslice)  +
  labs(y = "Score", title = "Results of tuning colsample_bytree of XGBoost for each timeslice: prauc",
       # subtitle = "Memory problems with Pre + Post datasets means this did not finish",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")


# explore best colsample_bytree which offer logloss delta of more than 0.05
scores_long_colsample_bytree[(lt0.005 | is.na(lt0.005)) & metric == "logloss", .SD[which.min(score)], 
                      by = .(dataset, timeslice)]


# Looking at scores for these combined

scores[tsk_ids == "val"  & model_features == model_features & dataset == use_dataset & tuning_round == "tune_samples"]

# Looking at improvement on validation set with tuning --------------------

s = data.table(scores[tsk_ids == "val"  & model_features == model_features ])

s1 = s[dataset == "Pre", .SD[which.min(logloss)], by = list(timeslice, tuning_round)] %>%
  filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
  ggplot(aes(x = 
               # tuning_round, 
               factor(tuning_round,
                        levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr", "final_preds")),
             y = logloss,
             group = "tuning_round")) +
  geom_line() + geom_point() + facet_grid(. ~ timeslice) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title = "Pre Covid: XGBoost Log loss values after each round of tuning (scores on validation set)",
       x = "Tuning round",
       y = "Log loss value") + theme_grey(base_size = 14) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 


s2 = s[dataset == "Pre", .SD[which.max(prauc)], by = list(timeslice, tuning_round)] %>%
  filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
  ggplot(aes(x = factor(tuning_round,
                        levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr", "final_preds")),
             y = prauc,
             group = "tuning_round")) +
  geom_line() + geom_point() + facet_grid(. ~ timeslice) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title = "Pre Covid: XGBoost PRAUC scores after each round of tuning (scores on validation set)",
       x = "Tuning round",
       y = "Area under PR curve") + theme_grey(base_size = 14) +
  theme(axis.text.x=element_text(angle=45,hjust=1))

library(gridExtra)
grid.arrange(s1, s2, nrow = 2)

s3 = s[dataset == "Post", .SD[which.min(logloss)], by = list(timeslice, tuning_round)] %>%
  filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
  ggplot(aes(x = 
               # tuning_round, 
               factor(tuning_round,
                      levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr", "final_preds")),
             y = logloss,
             group = "tuning_round")) +
  geom_line() + geom_point() + facet_grid(. ~ timeslice) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title = "Post Covid: XGBoost Log loss values after each round of tuning (scores on validation set)",
       x = "Tuning round",
       y = "Log loss value") + theme_grey(base_size = 14) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 


s4 = s[dataset == "Post", .SD[which.max(prauc)], by = list(timeslice, tuning_round)] %>%
  filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
  ggplot(aes(x = factor(tuning_round,
                        levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr", "final_preds")),
             y = prauc,
             group = "tuning_round")) +
  geom_line() + geom_point() + facet_grid(. ~ timeslice) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title = "Post Covid: XGBoost PRAUC scores after each round of tuning (scores on validation set)",
       x = "Tuning round",
       y = "Area under PR curve") + theme_grey(base_size = 14) +
  theme(axis.text.x=element_text(angle=45,hjust=1))


grid.arrange(s3, s4, nrow = 2)

