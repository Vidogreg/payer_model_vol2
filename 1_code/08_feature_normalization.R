# ---------------------------------------------------------------------------------------
# Objective
# ---------------------------------------------------------------------------------------
# Experiment with the standardization and normalization of the features.
# Does it influence the performance?
#
# ---------------------------------------------------------------------------------------
# Conclusion
# ---------------------------------------------------------------------------------------
# ???

## Run utils
source('1_code/utils/00_run_utils.R')

## Define stuff
project <- 'DA'
platform <- 'google_play'
config <- list(
  project = project,
  platform = platform,
  dig = 3,
  dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-04-01_2019-06-30.rds',
  sampleSize = 200000,
  testSampleSplit = 0.5,
  # cvRunCount = 1,
  seed = 1
)



### Load datasets
filePathData <- file.path(
  '0_data',
  config$dataFile
)
dfLoad <- loadRds(filePathData, 'dfLoad')

## Define the whole dataset
dat <- dfLoad[
  register_platform == config$platform &
    source == 'marketing' &
    days_in_game == config$dig,
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
dat$player_id <- as.integer(dat$player_id)

## Define the model
formulaString <- paste(
  'dy_payer ~',
  'dx_pay_count +',
  'dx_revenue +',
  'dx_session_count +',
  'dx_session_time +',
  'dx_session_days +',
  'dx_login_count +',
  'dx_gems_count +',
  'dx_gems_spent +',
  'tier'
)

## Sample data
set.seed(config$seed)
sampleRows <- sample(nrow(dat), config$sampleSize)
iHalf <- round(config$sampleSize/2)
trainRows <- sampleRows[1:iHalf]
testRows <- sampleRows[(iHalf + 1):config$sampleSize]

## Train data
datTrain <- dat[trainRows, ]

## Test data
datTest <- dat[testRows, ]



### Train the model
mod <- glm(
  formula = formulaString,
  data = datTrain,
  family = 'binomial'
)
datTrain$fit <- predict.glm(mod, newdata = datTrain, type = 'response')

## Calculate RCD-optimal cut-off
cutOff <- calculateCutOff(datTrain$dy_payer, datTrain$fit)



### Evaluate the model
datTest$fit <- predict.glm(mod, newdata = datTest, type = 'response')
datTest$prediction <- datTest$fit >= cutOff
modPred <- prediction(datTest$fit, datTest$dy_payer)
modAuc <- performance(modPred, 'auc')@y.values[[1]]
modRoc <- performance(modPred, 'tpr', 'fpr')
modConfMatrix <- table(datTest$prediction, datTest$dy_payer)

TP <- modConfMatrix[2, 2]
TN <- modConfMatrix[1, 1]
FP <- modConfMatrix[2, 1]
FN <- modConfMatrix[1, 2]

print(plot(modRoc))
print(list(
  rcd_optimal_confusion_matrix = modConfMatrix,
  sensitivity = TP/(TP + FN),
  precision = TP/(TP + FP),
  relative_count_difference = (FP + TP)/(FN + TP)
))