# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# Evaluate logit model on v5 dataset


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

## define config variables
trainRegMonth <- '2018-09-01'
testRegMonth <- '2018-10-01'


### Run utils
source('1_code/utils/00_run_utils.R')

### Load data
if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(
    file.path('0_data', 'ga_972_v5_payer_dataset_SY_GP&iOS_d1.rds')
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
    'dx_session_count',
    'dx_session_time',
    'dx_session_days',
    'dx_login_count',
    'dx_gem_count',
    'dx_gem_spent',
    'dy_pay_count'
  )]
dat$dy_payer <- dat$dy_pay_count > 0

datTrain <- dat[register_month == trainRegMonth, ]
datTrain <- datTrain[, c('player_id', 'register_month', 'dy_pay_count') := NULL]

datTest <- dat[register_month == testRegMonth, ]
datTest <- datTest[, c('player_id', 'register_month', 'dy_pay_count') := NULL]


## Calculate optimal cut-off from train dataset using CV
cutOffCVResult <- makeCrossValCutOff(10, datTrain)

print(cutOffCVResult$cutOffs)


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
  cut_off_full_0.9 = 0.9*cutOffFull,
  cut_off_full = cutOffFull,
  cut_off_full_1.1 = 1.1*cutOffFull
))


## Calculate confusion matrices
datTest$model_fit <- predict.glm(
  model, newdata = datTest, type = 'response'
)

co <- cutOffFull
# co <- cutOffCVResult$cutOffs[2]
confMatrix <- table(datTest$model_fit > co, datTest$dy_payer)

if(dim(confMatrix)[1] < 2) {
  if(row.names(confMatrix) == 'TRUE') {
    FP <- confMatrix[1, 1]
    TP <- confMatrix[1, 2]
    TN <- 0; FN <- 0
  }
  else {
    TN <- confMatrix[1, 1]
    FN <- confMatrix[1, 2]
    TP <- 0; FP <- 0
  }
} else {
  TN <- confMatrix[1, 1]
  FN <- confMatrix[1, 2]
  FP <- confMatrix[2, 1]
  TP <- confMatrix[2, 2]
}

print(list(
  used_cut_off = co,
  confusion_matrix = confMatrix,
  precision = TP/(TP + FP),
  TPR = TP/(TP + FN),
  FPR = FP/(TN + FP)
))