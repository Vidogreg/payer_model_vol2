# ---------------------------------------------------------------------------------------
# Objective
# ---------------------------------------------------------------------------------------
# Experiment with the standardization and normalization of the features.
# Does it influence the performance?
#
# ---------------------------------------------------------------------------------------
# Conclusion
# ---------------------------------------------------------------------------------------
conclusion <- paste(
  'Transformation by Box-Cox together with scaling seems to slightly improve',
  'the performance of the model. We should run CV to be sure, since',
  'this is only one sample.',
  'But transformation definitely has some potential, since we can see in the histograms',
  'that the transformed fit better distinguishes between positives and negatives.',
  sep = '\n'
)

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
  sampleSize = 800000,
  testSampleSplit = 0.5,
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
label <- 'dy_payer'
features <- c(
  'dx_pay_count',
  'dx_revenue',
  'dx_session_count',
  'dx_session_time',
  'dx_session_days',
  'dx_login_count',
  'dx_gems_count',
  'dx_gems_spent',
  'tier'
)
formulaString <- paste(label, '~', paste(features, collapse = ' + '))

## Sample data
set.seed(config$seed)
sampleRows <- sample(nrow(dat), config$sampleSize)
iHalf <- round(config$sampleSize/2)
trainRows <- sampleRows[1:iHalf]
testRows <- sampleRows[(iHalf + 1):config$sampleSize]

## Train data
datTrain <- dat[trainRows, ] %>% select(c(features, label))

## Test data
datTest <- dat[testRows, ] %>% select(c(features, label))



### Train the models
## Basic model
mod <- glm(
  formula = formulaString,
  data = datTrain,
  family = 'binomial'
)
cutOff <- calculateCutOff(
  datTrain$dy_payer,
  predict.glm(mod, newdata = datTrain, type = 'response')
)

## scaled model
datTrainScale <- predict(preProcess(datTrain, method = c('center', 'scale')), datTrain)
datTestScale <- predict(preProcess(datTest, method = c('center', 'scale')), datTest)
modScale <- glm(
  formula = formulaString,
  data = datTrainScale,
  family = 'binomial'
)
cutOffScale <- calculateCutOff(
  datTrainScale$dy_payer,
  predict.glm(modScale, newdata = datTrainScale, type = 'response')
)

## scaled and normalized model
datTrainTrans <- datTrain
datTestTrans <- datTest

for(f in features) {
  boxCoxObj <- BoxCoxTrans(datTrain[[f]] + 1)
  datTrainTrans[[f]] <- predict(boxCoxObj, datTrain[[f]] + 1)
  datTestTrans[[f]] <- predict(boxCoxObj, datTest[[f]] + 1)
  # print(boxCoxObj)
  
  x <- datTrainTrans[[f]]
  datTrainTrans[[f]] <- (x - mean(x))/sd(x)
  x <- datTestTrans[[f]]
  datTestTrans[[f]] <- (x - mean(x))/sd(x)
}

modTrans <- glm(
  formula = formulaString,
  data = datTrainTrans,
  family = 'binomial'
)
cutOffTrans <- calculateCutOff(
  datTrainTrans$dy_payer,
  predict.glm(modTrans, newdata = datTrainTrans, type = 'response')
)



### Evaluate the models
### And print the results
evalModel <- function(mod, cutOff, datTest, title = '', note = '') {
  datTestEval <- datTest
  datTestEval$fit <- predict.glm(mod, newdata = datTestEval, type = 'response')
  datTestEval$prediction <- datTestEval$fit >= cutOff
  modPred <- prediction(datTestEval$fit, datTestEval$dy_payer)
  modAuc <- performance(modPred, 'auc')@y.values[[1]]
  modRoc <- performance(modPred, 'tpr', 'fpr')
  modConfMatrix <- table(datTestEval$prediction, datTestEval$dy_payer)
  
  TP <- modConfMatrix[2, 2]
  TN <- modConfMatrix[1, 1]
  FP <- modConfMatrix[2, 1]
  FN <- modConfMatrix[1, 2]
  
  ## ROC curve
  par(pty = 's')
  print(plot(
    modRoc,
    main = paste(title, 'AUC =', round(modAuc, 3))
  ))
  par(pty = 'm')
  
  ## model performance with RCD-optimal cut-off
  printOutput(list(
    note = note,
    rcd_optimal_cut_off = cutOff,
    rcd_optimal_confusion_matrix = modConfMatrix,
    sensitivity = TP/(TP + FN),
    precision = TP/(TP + FP),
    relative_count_difference = (FP + TP)/(FN + TP)
  ))
  
  ## print downsampled plots
  datTestEvalDown <- data.table(
    downSample(
      x = datTestEval[, -ncol(datTestEval), with = F],
      y = datTestEval$dy_payer
    )
  )
  X <- datTestEvalDown[, c('fit')]
  y <- datTestEvalDown$dy_payer
  print(densityFeaturePlot(X, y))
  print(boxFeaturePlot(X, y))
  print(
    ggplot(datTestEvalDown %>% filter(dy_payer == TRUE), aes(x = fit)) +
      geom_histogram(bins = 40) + ggtitle('dy_payer == TRUE')
  )
  print(
    ggplot(datTestEvalDown %>% filter(dy_payer == FALSE), aes(x = fit)) +
      geom_histogram(bins = 40) + ggtitle('dy_payer == TRUE')
  )
}

filePathOutput <- file.path('2_output', '08_feature_transformation.pdf')
pdf(filePathOutput)
printOutput(config)
printOutput(cat(conclusion))

evalModel(
  mod, cutOff, datTest,
  'basic model', 'no scaling or tranformation'
)
evalModel(
  modScale, cutOffScale, datTestScale,
  'scaled model',
  'scaled/standardized features'
)
evalModel(
  modTrans, cutOffTrans, datTestTrans,
  'transformed model',
  'features transformed and scaled'
)

dev.off()



# ## Downsample train dataset
# datTrainDown <- data.table(
#   downSample(x = datTrain %>% select(features), y = datTrain$dy_payer)
# )
# setnames(datTrainDown, old = c('Class'), new = c('dy_payer'))
# 
# # for(f in features) {
# for(f in c('x')) {
#   print(
#     ggplot(datTrainDown, aes_string(x = f)) +
#       geom_histogram(bins = 30) +
#       ggtitle(f)
#   )
#   print(
#     ggplot(datTrainDown, aes_string(x = f)) +
#       geom_density() +
#       ggtitle(f)
#   )
#   print(
#     ggplot(datTrainDown, aes_string(sample = f)) +
#       geom_qq() +
#       ggtitle(f)
#   )
# }
# 
# f <- 'dx_session_count'
# ggplot(datTrainDown, aes_string(sample = f)) +
#   geom_qq() +
#   ggtitle(f)
# shapiro.test(datTrainDown[[f]])
# 
# boxCoxObj <- BoxCoxTrans(datTrainDown[[f]] + 1)
# datTrainDown$x <- predict(boxCoxObj, datTrainDown[[f]] + 1)
# 
# ggplot(datTrainDown, aes(sample = x)) +
#   geom_qq() +
#   ggtitle(paste(f, 'transformed'))
# shapiro.test(datTrainDown$x)



# apply(datTrain %>% select(features), 2, mean)
# apply(datTrain %>% select(features), 2, sd)
# apply(datTrainScaled %>% select(features), 2, mean)
# apply(datTrainScaled %>% select(features), 2, sd)
# apply(datTrainTrans %>% select(features), 2, mean)
# apply(datTrainTrans %>% select(features), 2, sd)