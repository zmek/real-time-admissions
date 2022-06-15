# About this folder

This is a series of files to train models to predict admissions from ED. It starts by making individual level predictions and then moves to aggregate predictions.

There are several shared files that are loaded in most of these scripts. 

real-time/app/utils.R - In order to prepare for real-time predictions, most functions are in a shared utils file, which can be accesssed by the real-time script, to ensure that data cleaning steps are processed consistently across training ML models and running them in production.

predict-admissions/code/utils.R - contains functions that used by more than one of the scripts in this folder, but not by the real-time app

predict-admissions/code/config.R - sets parameters like dates of training and test sets, and which version of files to load. Currently set up to run of four model periods (Pre-Covid, Post-Covid, Pre + Post Covid and post-SDEC)

## Files that need to be run once with new data import from Star

Before running any of the below, the scripts in the folder called flow-mapping need to be run, and summ_file_date parameter updated in config.R After that, from this folder, run:

### save benchmarks.R

Calculates how well the UCH method of predicting admissions, or the six-week rolling average, would have performed. Used by evaluate-distributions.R

### calc-not-yet-arrived.R

Creates an empirical distribution of mean number of patients who have not yet arrived, but will be admitted within a time window, varying by week/weekend, time of day (based on four time points of interest), pre/post/pre + post Covid, time window for arrival

Output
- poisson-means-not-yet-arrived.R

This is used by generate-prob-dist-and-pt-estimates.R and also by the real-time predictions app
Output is saved to real-time/model-input for use by the real-time app


### calc-time-to-admission-using survival-analysis-exponential-tail.R

Creates a survival curve for the probability of patient staying in ED. Uses only admitted patients

Inputs
- summ
- num_in_ED (which is created by script flow-mapping/prepare-ED-data-for-ML.R)

Output
- cox_results for the various model periods (pre-Covid, post-Covid, post-SDEC and combined)
- cox_surv_curv  - survival curves  for each model period; these include an exponential tail to allow for any possible length of stay prior to the point of prediction

Note that the cox equation's coefficients are used by the script  generate-prob-dist-and-pt-estimates.R to make predictions in a function called get_cox_coef_by_csn() [Note this function can be found in the utils file]

This is used by generate-prob-dist-and-pt-estimates.R and also by the real-time predictions app.

Output is saved to real-time/model-input for use by the real-time app


### calc-time-to-admission-using-six-week-rolling-survival-analysis-exponential-tail.R

This script does the same as the script above, but uses a sliding window from the previous six weeks to generate survival curves, and Cox results


### get-obs-lab-data-from-star.R

Retrieves data from Star for observations and pathology and consults

Output
- obs_raw
- lab_orders_raw 
- lab_results_raw
- consults_raw

### clean_obs_data.R

Cleans non-numeric fields to make them numeric (e.g. separates blood pressure into systolic and diastolic)

Names of flowsheet measurements are cleaned

Input
- obs_raw 
- summ (to get time of arrival and discharge from ED)

Output
- obs_real - all data from selected flowsheet measurements in long matrix
- obs_to_include (needed for real-time prediction)

Note that the output files exclude any visits that did not have obs measurements


### clean_lab_data.R

Input
- lab_orders_raw 
- lab_results_raw
- summ

Output
- lab_orders_real
- lab_results_real
- lab_orders_to_include (needed for real-time prediction)

Note that the output files exclude any visits that did not have lab measurements

### clean_consults_data.R

Input
- consults_raw
- summ

Output
- consults_real

## Machine learning

Now that the data is ready, the following files can be run; 

### 1. generate_timeslices.R

Input
- lab_orders_real
- lab_results_real
- obs_real
- consults_real
- moves
- summ

Output - a series of design matrices for input into the ML

Once a new set of design matrices is ready, predict-admissions/code/config.R needs to be updated with the new dm_file_date so that scripts pick up the correct output

### 1a. generate-dm-without-timeslice.R

As above but output is a single design matrix (for comparative purposes, not used in the final analysis)

### 2.  run-ML.R

Because the Covid outbreak happened during the period of this research, we conducted various analyses of different model periods. These were:

- pre-Covid up to 19 March 2020
- during Covid from 19 March 2020 up to the end of the study period
- Pre + Post Covid - continuous 
- post-SDEC - the period from December 2020 after the SDEC unit was introduced. 

Before running, set the required dates for training, validation and test sets for each model period as parameters in config.R. Note that this split is done chronologically.

Each time this is run, a new scores file is created with today's date, plus (if final_preds is set to TRUE), a new file with feature importances also with today's date. The only exception is when making predictions on the test set. In that case the existing scores file is loaded to obtain optimal parameters, and test set scores are appended to it. 

This script must be run once for each model period. After running, update config.R with model_output_date_Post [or other model period as appropriate]

Many of the functions used in for model preparation and training are defined in predict-admission/code/utils.R

Parameters to set

- model_period - choose one of "Pre", "Post", "Pre + Post" or "Post-SDEC"
- model_features = "alop"- each of a, l, o and p specific the inclusion of feature sets (a = admission characteristics, l = location, o = observations, p = pathology

Tuning parameters to set

- base_model - if TRUE using XGBoost defaults
- tune_nrounds
- tune_trees 
- tune_gamma - note that after various experiments we didn't tune gamma as tuning showed no variation
- recal_nr - after tuning trees, recalibration of nrounds
- tune_samples 
- reduce_lr - reduce learning rate
- final_preds - generate final predictions

For these last two, the scores_file is updated rather than new scores file created. In these cases don't update config.R with today's date
final_preds_on_test_set = TRUE
final_preds_pre_on_post_covid_test_set = FALSE #  not yet copied from run-ML-for-dissertation.R

Runs XGBoost

Inputs
- design matrices from generate-timeslices.R

Outputs
- scores for all tuning rounds
- saved learners for MLR
- save features used in each model (needed for real-time prediction)
- feature importances for each model


### 2a.   run-ML-without-timeslices.R

Runs XGBoost on a single model (no timeslices) as is as above other than the input.

Input
- dm_single from generate-dm-without-timeslice.R

### 2b. run-RF.R

Same as run-ML.R in how it creates and saves scores and feature importances files

Has additional processing of missing values, and saves medians for later imputation on test set

Parameters to set

- model_period - choose one of "Pre", "Post", "Pre + Post" or "Post-SDEC"
- model_features = "alop"- each of a, l, o and p specific the inclusion of feature sets (a = admission characteristics, l = location, o = observations, p = pathology

Note that tuning of the RF models was done elsewhere for another project (code not shown here) and the best-performing parameters, 
are brought across into this script without tuning. 

Runs Random Forest

Inputs
- design matrices from generate-timeslices.R

Outputs
- scores for all tuning rounds
- saved learners for MLR
- save features used in each model 
- feature importances for each model

After running, update config.R with model_output_date_Pre_rf or other model period as appropriate

### 2c. run-LR.R

Same as run-ML.R in how it creates and saves scores and feature importances (aka coefficients) files

Runs Logistic Regression

Inputs
- design matrices from generate-timeslices.R

Outputs
- scores for all tuning rounds
- saved learners for MLR
- save features used in each model
- coefficients for each model

After running, update config.R with model_output_date_Pre_lr or other model period as appropriate

### 3. generate-prob-dist-and-pt-estimates-using-survival-analysis.R

Takes the saved learners from run-ML.R, and generates a variety of probability distributions based on an input set of dates and times (typically the dates and times of reporting points of interest in the validation or test set). These distributions are aggregated from individual probabilities to make predictions about the number of patients to be admitted, either at any point, or within a specified time window. This can include

- just the patients in ED at each time point of interest
- also the patients who have not yet arrived at the time point, but who might arrive and be admitted within the time window

In order to calculate admissions within time window, a binomial approach is used assuming independent probabilities of being admitted, and being admitted within a time window

Inputs
- design matrices from generate-timeslices.R
- saved learners for MLR
- poisson means (generated by file calc-not-yet-arrived.R or by other method)
- time to admission (generated by calc-time-to-admission-using-survival-analysis-exponential-tail.R)

Functions in this script
- make_predictions - each individual gets a predicted prob of admission 
- get_cox_coef_by_csn - each individual has appended to their row the numbers in ED at the time they arrived; from these numbers, combined with coefficients from the cox regression model (created by Calc-time-to-admission-using survival-analysis.R) each patient is assigned a variable called epow. This variable will be used to squeeze or stretch their survival curve later [Note, this function is in real-time/app/utils.R]
- get_poisson_means - each time point has a poisson mean for the not-yet-arrived patients, which is calculated by retrieving the saved poisson equations and generating a predicted mean [Note, this function is in real-time/app/utils.R]
- get-prob-dist - the main event of the script; taking each time point, it returns a predicted distribution for numbers of admissions in different time windows. For predictions without a time window, it simply takes the patients in ED and uses a probability generating function to turn this into a distribution for each time point. To predict within time window, it starts with a base probability of remaining in the ED (which is generated by the Calc-time-to-admission-using survival-analysis.R) and then stretches or squeezes it for each patient depending on the state of ED when they arrived. This gives a probability of being admitted within the time window, which is (assuming a joint distribution of independent events) then multiplied by the probability of admission to predict a distribution within time window. Finally, a poisson distribution is used to predict patients who have not yet arrived [Note, this function is in real-time/app/utils.R]

Outputs
- predicted aggregated number of admissions (in the form of a probability distribution), plus actual number of admissions at that time point

### 4. Plot figures for paper-revision-2.R 

Produces all the figures shown in the paper except Figure 1. Note this file has not been commented, but is included for completeness. 

### 5. Plot one time point.R 

Produces Figure 1 in the paper. Note this file has not been commented, but is included for completeness. 

## Other scripts that may be useful

### report-on-tuning.R

Prints charts showing outputs from tuning process set in run-ML.R

### report-on-class-balance.R

Shows timeslice class balance

### plot-train-val-test-split.R

Shows chronological split into training, validation and test sets for each model period. 