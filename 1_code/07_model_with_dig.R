# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Comparison of v6 model to a model with days_in_game as a feature

## Run utils
source('1_code/utils/00_run_utils.R')
library(dplyr)
# library(profvis)

## Define stuff
project <- 'SY'
platform <- 'google_play'
# platform <- 'apple_appstore'
config <- list(
  project = project,
  platform = platform,
  # dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-01-01_2019-03-31.rds',
  dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-04-01_2019-06-30.rds',
  sampleSize = 200000,
  # sampleSize = 150000,
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
  register_platform == config$platform &
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
  'tier +',
  'days_in_game'
)



### Run cross-validation
runCv <- function(dat, config) {
  
  k <- config$cvRunCount
  digs <- 0:7
  digsCount <- length(digs)
  
  cvResult <- data.frame(
    cv_run_id = 1:k,
    train_positives = rep(NA, k),
    test_positives = rep(NA, k),
    dig_model_auc = rep(NA, k),
    dig_model_rcd = rep(NA, k),
    dig_model_sens = rep(NA, k),
    dig_model_prec = rep(NA, k)
  )
  
  cvResultDig <- data.frame(
    cv_run_id = rep(NA, k*digsCount),
    days_in_game = rep(NA, k*digsCount),
    train_positives = rep(NA, k*digsCount),
    test_positives = rep(NA, k*digsCount),
    dig_model_rcd = rep(NA, k*digsCount),
    dig_model_sens = rep(NA, k*digsCount),
    dig_model_prec = rep(NA, k*digsCount)
  )
  
  j <- 1
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
      formula = formulaString,
      data = datTrain,
      family = 'binomial'
    )
    datTrain$fit <- predict.glm(modelDig, newdata = datTrain, type = 'response')
    
    ## Calculate RCD-optimal cut-offs for each days_in_game with dig model
    
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
    
    ## Write results
    cvResult$train_positives[i] <- nrow(datTrain[dy_payer == TRUE])
    cvResult$test_positives[i] <- nrow(datTest[dy_payer == TRUE])
    cvResult$dig_model_auc[i] <- modelDigAuc
    cvResult$dig_model_rcd[i] <- (TP + FP)/(TP + FN)
    cvResult$dig_model_sens[i] <- TP/(TP + FN)
    cvResult$dig_model_prec[i] <- TP/(TP + FP)
    
    for(dig in digs) {
      
      TP <- nrow(datTestEval[days_in_game == dig & dy_payer == TRUE & prediction == TRUE, ])
      FP <- nrow(datTestEval[days_in_game == dig & dy_payer == FALSE & prediction == TRUE, ])
      FN <- nrow(datTestEval[days_in_game == dig & dy_payer == TRUE & prediction == FALSE, ])
      modelDigRcd <- (TP + FP)/(TP + FN)
      modelDigSens <- TP/(TP + FN)
      modelDigPrec <- TP/(TP + FP)
      
      cvResultDig$cv_run_id[j] <- i
      cvResultDig$days_in_game[j] <- dig
      cvResultDig$train_positives[j] <- nrow(datTrain[days_in_game == dig & dy_payer == TRUE])
      cvResultDig$test_positives[j] <- nrow(datTest[days_in_game == dig & dy_payer == TRUE])
      cvResultDig$dig_model_rcd[j] <- (TP + FP)/(TP + FN)
      cvResultDig$dig_model_sens[j] <- TP/(TP + FN)
      cvResultDig$dig_model_prec[j] <- TP/(TP + FP)
      
      j <- j + 1
      
    }
    
    print(paste('CV', i, 'complete'))
    
  }
  
  list(all = cvResult, by_dig = cvResultDig)
  
}

set.seed(config$seed)
result <- runCv(dat, config)



### Print results
plotHistogram <- function(df, colName, bins = 16) {
  ggplot(df, aes_string(x = colName)) +
    geom_histogram(
      aes(y = ..density..),
      bins = bins, fill = 'white', color = 'black'
    ) +
    geom_density(alpha = 0.1, fill = 'blue', color = 'blue')
}
plotBoxPlotDig <- function(df, yColName, yLab) {
  ggplot(result$by_dig, aes(x = as.factor(days_in_game))) +
    geom_boxplot(aes_string(y = yColName)) +
    xlab('days_in_game') +
    ylab(yLab)
}

filePathOutput <- file.path('2_output', '07_model_with_dig.pdf')
pdf(filePathOutput)

printOutput(config)
printOutput(summary(result$all), 0.5)
printOutput(result$all, 0.5)

print(plotHistogram(result$all, 'dig_model_auc'))
print(plotHistogram(result$all, 'dig_model_rcd'))
print(plotHistogram(result$all, 'dig_model_sens'))

print(
  ggplot(
    melt(
      dat %>%
        group_by(days_in_game) %>%
        summarize(
          'dy_conv' = mean(as.numeric(dy_payer) - 1),
          'dx_conv' = mean(if_else(dx_pay_count > 0, 1, 0))
        ),
      id.var = 'days_in_game'
    ),
    aes(x = days_in_game, y = value, col = variable)
  ) +
    geom_line(size = 1.2) +
    expand_limits(y = 0) +
    ggtitle('Changes in dx_payer and dy_payer with days_in_game')
)

print(plotBoxPlotDig(result$by_dig, 'dig_model_rcd', 'relative count difference'))
print(plotBoxPlotDig(result$by_dig, 'dig_model_sens', 'sensitivity'))
print(plotBoxPlotDig(result$by_dig, 'dig_model_prec', 'precision'))

dev.off()




# # exploration of changes in predictions by dig per player
# x <- datTestEval[, c('player_id', 'days_in_game', 'dx_pay_count', 'dy_payer', 'prediction')]
# x$dy_payer <- as.logical(x$dy_payer)
# x2 <- data.table(
#   x %>%
#     group_by(player_id) %>%
#     summarize(any_pay = any(as.logical(dy_payer), prediction))
# )
# x[player_id == sample(x2[any_pay == TRUE, ]$player_id, 1), ]