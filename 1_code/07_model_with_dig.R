# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Comparison of v6 model to a model with days_in_game as a feature

## Run utils
source('1_code/utils/00_run_utils.R')
library(dplyr)

## Define stuff
project <- 'DA'
platform <- 'google_play'
config <- list(
  project = project,
  platform = platform,
  # dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-01-01_2019-03-31.rds',
  dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-04-01_2019-06-30.rds',
  sampleSize = 2000,
  cvRunCount = 1
)



### Load data
filePathData <- file.path(
  '0_data',
  config$dataFile
)
dfLoad <- loadRds(filePathData, 'dfLoad')

## Define the whole dataset
dat <- dfLoad[
  register_platform == 'google_play' &
    source == 'marketing',
  c(
    'player_id',
    'register_date',
    'country',
    'tier',
    'days_in_game',
    'dx_pay_count',
    'dx_revenue',
    'dx_session_count',
    'dx_session_time',
    'dx_session_days',
    'dx_login_count',
    'dx_gems_count',
    'dx_gems_spent',
    'dy_pay_count',
    'dy_revenue'
  )]
dat$dy_payer <- factor(dat$dy_pay_count > 0)



### Run cross-validation
set.seed(1)

for(i in 1:config$cvRunCount) {
  
  ## Sample the dataset
  samplePid <- sample(dat[days_in_game == 0, ]$player_id, config$sampleSize)
  sampleRows <- sample(config$sampleSize, 0.5*config$sampleSize)
  samplePidTrain <- samplePid[sampleRows]
  samplePidTest <- samplePid[-sampleRows]
  
  ## Train data
  datTrain <- dat[player_id %in% samplePidTrain, ]
  
  ## Test data
  datTest <- dat[player_id %in% samplePidTest, ]
  
}






