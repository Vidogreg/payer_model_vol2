# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Evaluate logit model on v5 dataset


# ---------------------------------------------------------------------------------------
# Main code
# ---------------------------------------------------------------------------------------

### Define config variables
project <- 'SY'
trainRegMonths <- c('2018-08-01', '2018-09-01', '2018-10-01')
testRegMonths <- c('2018-10-01', '2018-11-01', '2018-12-01')


### Run utils
source('1_code/utils/00_run_utils.R')


### Load data
fileName <- 'ga_972_v6_payer_dataset_' %+% project %+% '_GP_d0.rds'
if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(
    file.path('0_data', fileName)
  ))

## Define datasets
dat <- dfLoad[
  register_platform == 'google_play' &
    source == 'marketing',
  c(
    'player_id',
    'register_month',
    'tier',
    'dx_pay_count',
    'dx_revenue',
    'd0_session_count',
    'd0_session_time',
    'd0_login_count',
    'dy_pay_count'
  )]
dat$dy_payer <- dat$dy_pay_count > 0


### Cycle through months
for(i in 1:length(testRegMonths)) {
  
  trainRegMonth <- trainRegMonths[i]
  testRegMonth <- testRegMonths[i]
  
  ## Define train dataset
  datTrain <- dat[register_month == trainRegMonth, ]
  datTrain <- datTrain[, c('player_id', 'register_month', 'dy_pay_count') := NULL]
  
  ## Calculate optimal cut-off from full train dataset
  model <- glm(
    formula = dy_payer ~ .,
    data = datTrain,
    family = 'binomial'
  )
  datTrain$model_fit <- predict.glm(
    model, newdata = datTrain, type = 'response'
  )
  cutOffFull <- optimize(
    differenceFPFN,
    interval = c(0, 1),
    fit = datTrain$model_fit,
    ref = datTrain$dy_payer
  )$minimum
  
  print(c(
    train_reg_month = trainRegMonth,
    test_reg_month = testRegMonth,
    cut_off_full = cutOffFull
  ))
  
}

print(fileName)