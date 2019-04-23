# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# This saves the sample of data to .csv
# It will be converted to .arff later for Weka


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

## Run utils
source('1_code/utils/00_run_utils.R')


## Load dataset and prepare for Weka
dfToExport <- data.table(
  readRDS(
    file.path('0_data', 'ga_972_v5_payer_dataset_SY_GP&iOS_d0.rds')
  )
)
dfToExport <- dfToExport[register_platform == 'google_play']
dfToExport$dy_payer <- dfToExport$dy_pay_count > 0
dfToExport <- dfToExport[, c(
  'player_id',
  'days_in_game',
  'source',
  'country',
  'tier',
  'dx_pay_count',
  'dx_revenue',
  'd0_session_count',
  'd0_session_time',
  'd0_login_count',
  'dx_gem_count',
  'dx_gem_spent',
  'dy_payer',
  'dy_pay_count',
  'dy_revenue'
)]


## Sample data
sampleSize <- 100000
randomSeed <- 1024
set.seed(randomSeed)
dfSampleToExport <- dfToExport[sample(nrow(dfToExport), sampleSize)]


## write the sample data to .csv
fwrite(dfSampleToExport, file.path('0_data', paste(hiveTable, '_SY_GP_d0_sample.csv', sep = '')))