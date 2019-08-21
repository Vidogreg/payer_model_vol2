# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Comparison of v6 model to a model with days_in_game as a feature

## Run utils
source('1_code/utils/00_run_utils.R')
library(dplyr)
# library(profvis)

## Define stuff
project <- 'DA'
platform <- 'google_play'
config <- list(
  project = project,
  platform = platform,
  # dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-01-01_2019-03-31.rds',
  dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-04-01_2019-06-30.rds',
  sampleSize = 200000,
  testSampleSplit = 0.5,
  cvRunCount = 40,
  seed = 1024
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



### Run cross-validation
runCv <- function(dat, config) {
  
  cvResult <- data.frame(
    cv_run_id = 1:config$cvRunCount,
    train_positives = rep(NA, config$cvRunCount),
    test_positives = rep(NA, config$cvRunCount),
    dig_model_auc = rep(NA, config$cvRunCount),
    dig_model_rcd = rep(NA, config$cvRunCount),
    dig_model_sens = rep(NA, config$cvRunCount)
  )
  
  for(i in 1:config$cvRunCount) {
    
    ## Sample the dataset
    samplePid <- sample(dat[days_in_game == 0, ]$player_id, config$sampleSize)
    sampleRows <- sample(config$sampleSize, config$testSampleSplit*config$sampleSize)
    samplePidTrain <- samplePid[sampleRows]
    samplePidTest <- samplePid[-sampleRows]
    
    ## Train data
    datTrain <- dat[player_id %in% samplePidTrain, ]
    
    ## Test data
    datTest <- dat[player_id %in% samplePidTest, ]
    
    ## Train dig model
    modelDig <- glm(
      formula = paste(formulaString, '+ days_in_game'),
      data = datTrain,
      family = 'binomial'
    )
    datTrain$fit <- predict.glm(modelDig, newdata = datTrain, type = 'response')
    
    ## Calculate RCD-optimal cut-offs for each days_in_game with dig model
    digs <- 0:7
    cutOffs <- data.frame(
      days_in_game = digs,
      rcd_cut_off = sapply(
        digs,
        function(x) {
          calculateCutOff(
            datTrain[days_in_game == x]$dy_payer,
            datTrain[days_in_game == x]$fit
          )
        }
      )
    )
    
    ## Evaluate the dig model
    datTestEval <- data.table(left_join(datTest, cutOffs, by = c('days_in_game')))
    datTestEval$fit <- predict.glm(modelDig, newdata = datTest, type = 'response')
    datTestEval$prediction <- datTestEval$fit >= datTestEval$rcd_cut_off
    modelDigPred <- prediction(datTestEval$fit, datTestEval$dy_payer)
    modelDigAuc <- performance(modelDigPred, 'auc')@y.values[[1]]
    # modelDigRoc <- performance(modelDigPred, 'tpr', 'fpr')
    # plot(modelDigRoc)
    TP <- nrow(datTestEval[dy_payer == TRUE & prediction == TRUE, ])
    FP <- nrow(datTestEval[dy_payer == FALSE & prediction == TRUE, ])
    FN <- nrow(datTestEval[dy_payer == TRUE & prediction == FALSE, ])
    modelDigRcd <- (TP + FP)/(TP + FN)
    modelDigSens <- TP/(TP + FN)
    
    ## Write results
    cvResult$train_positives[i] <- nrow(datTrain[dy_payer == TRUE])
    cvResult$test_positives[i] <- nrow(datTest[dy_payer == TRUE])
    cvResult$dig_model_auc[i] <- modelDigAuc
    cvResult$dig_model_rcd[i] <- modelDigRcd
    cvResult$dig_model_sens[i] <- modelDigSens
    
    print(paste('CV', i, 'complete'))
    
  }
  
  cvResult
  
}

set.seed(config$seed)
cvResult <- runCv(dat, config)



### Print results
plotHistogram <- function(df, colName, bins = 16) {
  ggplot(df, aes_string(x = colName)) +
    geom_histogram(
      aes(y = ..density..),
      bins = bins, fill = 'white', color = 'black'
    ) +
    geom_density(alpha = 0.1, fill = 'blue', color = 'blue')
}

filePathOutput <- file.path('2_output', '07_model_with_dig.pdf')
pdf(filePathOutput)

printOutput(config)
printOutput(summary(cvResult), 0.5)
printOutput(cvResult, 0.5)

print(plotHistogram(cvResult, 'dig_model_auc'))
print(plotHistogram(cvResult, 'dig_model_rcd'))
print(plotHistogram(cvResult, 'dig_model_sens'))

dev.off()