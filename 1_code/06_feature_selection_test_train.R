# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Goal of this script is to benchmark the minimal model.
# And to measure the impact of adding more features.


# ---------------------------------------------------------------------------------------
# Main code
# ---------------------------------------------------------------------------------------


### Define stuff
config <- list(
  dataFile = 'payer_model_DA_GP&iOS_mkt_2019-01-01_2019-03-31.rds',
  # dataFile = 'payer_model_DA_GP&iOS_mkt_2019-04-01_2019-06-30.rds',
  # dataFile = 'payer_model_SY_GP&iOS_mkt_2019-01-01_2019-03-31.rds',
  # dataFile = 'payer_model_SY_GP&iOS_mkt_2019-04-01_2019-06-30.rds',
  trainRegDate = c('2019-01-01', '2019-01-31'),
  testRegDate = c('2019-03-01', '2019-03-31')
  # trainRegDate = c('2019-01-10', '2019-01-14'),
  # testRegDate = c('2019-03-10', '2019-03-14')
  # trainRegDate = c('2019-04-01', '2019-04-30'),
  # testRegDate = c('2019-06-01', '2019-06-30')
)


### Run utils
source('1_code/utils/00_run_utils.R')
packageTest('corrplot')
# packageTest('PerformanceAnalytics')


### Load data
filePathData <- file.path(
  '0_data',
  config$dataFile
)
dfLoad <- loadRds(filePathData, 'dfLoad')

## Define dataset - all
dat <- dfLoad[
  register_platform == 'google_play' &
    source == 'marketing' &
    days_in_game == 0,
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

## Define train dataset
datTrain <- dat[
  register_date >= config$trainRegDate[1] &
    register_date <= config$trainRegDate[2], ]

## Define test dataset
datTest <- dat[
  register_date >= config$testRegDate[1] &
    register_date <= config$testRegDate[2], ]

## Calculate correlation matrix
corrMatrix <- cor(
  dat[, -c(
    'player_id',
    'register_date',
    'country',
    'days_in_game',
    'dy_payer'
  )]
)
corrP <- cor.mtest(corrMatrix)


### Train and evaluate models
## Define models
modelFormulas <- list(
  as.formula(paste(
    'dy_payer ~',
    'dx_revenue'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_pay_count'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_login_count'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_session_count'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_session_time'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_gems_count'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_gems_spent'
  )),
  
  as.formula(paste(
    'dy_payer ~',
    'dx_revenue +',
    'dx_login_count'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_revenue +',
    'dx_session_count'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_revenue +',
    'dx_session_time'
  )),
  
  as.formula(paste(
    'dy_payer ~',
    'dx_revenue +',
    'dx_session_time +',
    'dx_gems_count'
  )),
  as.formula(paste(
    'dy_payer ~',
    'dx_revenue +',
    'dx_session_time +',
    'dx_gems_spent'
  ))
  
  # as.formula(paste(
  #   'dy_payer ~',
  #   'tier +',
  #   'dx_pay_count +',
  #   'dx_revenue +',
  #   'dx_session_count +',
  #   'dx_session_time +',
  #   'dx_session_days +',
  #   'dx_login_count +',
  #   'dx_gems_count +',
  #   'dx_gems_spent'
  # ))
)

## Train models
models <- lapply(
  modelFormulas,
  function(f) {
    glm(
      formula = f,
      data = datTrain,
      family = 'binomial'
    )
  }
)

## evaluate on test data
modelEvals <- lapply(
  models,
  function(m) {
    evalLogitModel(
      ref = datTest$dy_payer,
      fit = predict.glm(m, newdata = datTest, type = 'response')
    )
  }
)

summaryText <- paste(
  'Summary for DA:\n',
  'Based on correlation matrix, these variables should not be together\n',
  '  dx_session_time, dx_session_count, dx_login_count\n',
  '  dx_pay_count, dx_revenue\n',
  '  dx_gems_spent, dx_gems_count\n',
  '  dx_gems_spent, dx_revenue\n',
  '  dx_gems_spent, dx_pay_count\n',
  'We will test 1-variable models\n',
  '  dx_revenue ~ dx_pay_count (we choose revenue)\n',
  '  dx_session_time > dx_session_count > dx_login_count (as expected)\n',
  '  dx_gems_count > dx_gems_spent\n',
  'Check session features with dx_revenue\n',
  '  dx_session_count ~> dx_session_time > dx_login_count (time is cont.)\n',
  'Check gem features with previous\n',
  '  dx_gems_spent = dx_gems_count (spent is cont.)\n'
)
# summaryText <- paste(
#   'Summary for DA:\n'
# )


### Print results
filePathOutput <- file.path('2_output', '06_feature_selection_test_train.pdf')
pdf(filePathOutput)

printOutput(config)
printOutput(filePathData)

corrplot(
  corrMatrix, tl.col = 'black', tl.srt = 30,
  order = 'hclust', method = 'ellipse', type = 'upper',
  p.mat = corrP$p, insig = 'p-value'
)
corrplot(
  corrMatrix, tl.col = 'black', tl.srt = 30,
  order = 'hclust', method = 'number', type = 'upper'
)

printOutput(modelFormulas)

## downsample for plots
datDownTest <- data.table(downSample(datTest, datTest$dy_payer))

for(l in 1:length(modelFormulas)) {
  plot.roc(
    modelEvals[[l]]$rocPlot,
    print.thres = TRUE,
    print.auc = TRUE,
    main = paste('ROC curve for model', l)
  )
  fitTemp <- predict.glm(models[[l]], newdata = datDownTest)
  print(densityFeaturePlot(
    x = fitTemp,
    y = datDownTest$dy_payer,
    main = paste('density of model', l, 'fit based on dy_payer'),
    xlim = c(min(fitTemp), quantile(fitTemp, 0.95))
  ))
  printOutput(summary(models[[l]]))
  printOutput(modelEvals[[l]][2:4])
  printOutput(modelEvals[[l]][5])
}

printOutput(cat(summaryText))

dev.off()