# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# In this script, we train extension of v6 model. We added days_in_game as a feature.


# ---------------------------------------------------------------------------------------
# Main code
# ---------------------------------------------------------------------------------------

### Define config variables
trainRegDate <- c('2019-02-01', '2019-03-31')
testRegDate <- c('2019-05-01', '2019-05-31')
# trainRegDate <- c('2019-02-01', '2019-02-01')
# testRegDate <- c('2019-05-01', '2019-05-01')


### Run utils
source('1_code/utils/00_run_utils.R')


### Load data
fileName <- 'payer_model_DA_GP&iOS_mkt_2019-02-01_2019-05-31.rds'
if(!exists('dfLoad')) {
  dfLoad <- data.table(readRDS(
    file.path('0_data', fileName)
  ))
  print(fileName %+% ' loaded to memory')
}

## Define dataset - all
dat <- dfLoad[
  register_platform == 'google_play',
  c(
    'player_id',
    'register_date',
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
dat$dy_payer <- dat$dy_pay_count > 0

## Define train dataset
datTrain <- dat[register_date >= trainRegDate[1] & register_date <= trainRegDate[2], ]

## Define test dataset
datTest <- dat[register_date >= testRegDate[1] & register_date <= testRegDate[2], ]


### Train and evaluate model
modelFormula <- as.formula(
  'dy_payer ~ tier + days_in_game + dx_pay_count + dx_revenue + ' %+%
  'dx_session_count + dx_session_time + dx_session_days + dx_login_count + ' %+%
  'dx_gems_count + dx_gems_spent'
)

model <- glm(
  formula = modelFormula,
  data = datTrain,
  family = 'binomial'
)

## predict on test data
datTest$model_fit <- predict.glm(
  model, newdata = datTest, type = 'response'
)


### Print results
filePath <- file.path('2_output', '04_v7_dig_logit_model.pdf')
pdf(filePath)

modelEval <- evalLogitModel(
  ref = datTest$dy_payer,
  fit = datTest$model_fit
)

printOutput(list(
  days_in_game = '0-7',
  modelEval$auc,
  cutOff = modelEval$cutOff
))
printOutput(modelEval$confMatrix)

for(dig in 0:7) {
  
  modelEval <- evalLogitModel(
    ref = datTest[days_in_game == dig, ]$dy_payer,
    fit = datTest[days_in_game == dig, ]$model_fit
  )
  
  printOutput(list(
    days_in_game = dig,
    modelEval$auc,
    cutOff = modelEval$cutOff
  ))
  printOutput(modelEval$confMatrix)
  
  print(dig)
  
}

dev.off()