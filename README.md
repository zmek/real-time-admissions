# EDcrowding

Welcome to the repo for real-time prediction of admissions at UCLH. 

The project has a number of elements 

1. Extract and clean data 
2. Train ML models to predict individual probability of admission
3. Aggregate the individual probabilities into a predicted distribution for number of beds needed
4. Run a ML pipeline in real-time to retrive patients current in ED, make predictions about individual probability of admission, aggregate these into predictions for number of beds needed, save predictions and email them to bed planners
5. Prepare figures for publication in an academic manuscript

See the current version of the manuscript in pre-print form at: https://www.medrxiv.org/content/10.1101/2022.03.07.22271999v1 

This repo is organised into the following folders

* flow-mapping - for extract from Star and initial processing
* predict-admission - scripts for running machine learning and all output files
* real-time - for real-time prediction including input needed for models in real-time

Each folder has its own README to explain what it does

Each folder is structured as follows (but note that data-raw and data-output are ignored by git and model-output and media have been emptied):
* code - contains all scripts used in the final analysis
* data-raw - contains datasets created by the scripts (ignored by git so not visible outside the UCH environment)
* data-output - contains outpout from ML models, feature lists (ignored by git so not visible outside the UCH environment)
* model-output - contains results like predicted probability distributions and evalutaion results
* media - output as charts