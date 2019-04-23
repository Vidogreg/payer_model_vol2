# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# Evaluate logit model on v5 dataset


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

### Run utils
source('1_code/utils/00_run_utils.R')

### Load data
if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(
    file.path('0_data', 'ga_972_v5_payer_dataset_SY_GP&iOS_d0.rds')
  ))

