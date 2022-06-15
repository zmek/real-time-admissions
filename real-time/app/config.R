
# Set configuration for programme -----------------------------------------

# set date of ML models to use
model_details = data.table(algo = c(rep("xgb", 4)),
                           epoch = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                           model_date = c("2021-07-27", "2021-07-26", "2021-07-27", "2021-07-26")) 
model_features = "alop"

model_period_ = "Post-SDEC"
algo_for_app = "xgb"


# Output schema
target_schema <- 'flow'

# Date of cox regression files 
cox_surv_date <- "2021-09-16"

# Date of poisson results files
poisson_results_date = "2021-09-15"

# Adjustment for Cox regression coeffient ---------------------------------

pow_adjustment = data.table(model_period = c("Pre", "Post", "Pre + Post", "Post-SDEC"),
                            adjustment = c(0.3, 0, 0.3, 0.2))
