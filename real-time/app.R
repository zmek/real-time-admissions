# Demo script that predicts next vital sign
rlang::inform('--- Script to generate real-time predictions from ED')

# ****
# TODO
# ****

# load files
setwd("//uclcmddprafss21/Home/zelking1/Documents/EDcrowding/real-time")
source('app/libraries.R')
# source('app/config.R')
source('app/utils.R')

# Data pipeline with data dependencies
# check these with a call to `exists()` before you run the code
source('app/wrangle_pts.R')
source('app/wrangle_obs.R')

assertthat::assert_that(exists("dtpts"))
assertthat::assert_that(exists("dtobs"))
source('app/wrangle.R')

assertthat::assert_that(exists("mdt"))
source('app/model.R')

assertthat::assert_that(exists("mdt"))
source('app/write.R')

rlang::inform('--- script completed')