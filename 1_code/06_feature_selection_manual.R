# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Goal of this script is to benchmark the minimal model.
# And to measure the impact of adding more features.


# ---------------------------------------------------------------------------------------
# Main code
# ---------------------------------------------------------------------------------------


### Run utils
source('1_code/utils/00_run_utils.R')
packageTest('corrplot')
packageTest('gridExtra')


### Define stuff
project <- 'DA'
config <- list(
  dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-01-01_2019-03-31.rds',
  # dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-04-01_2019-06-30.rds',
  # trainRegDate = c('2019-01-01', '2019-01-02'),
  # testRegDate = c('2019-03-01', '2019-03-02')
  trainRegDate = c('2019-01-01', '2019-01-31'),
  testRegDate = c('2019-03-01', '2019-03-31')
  # trainRegDate = c('2019-04-01', '2019-04-30'),
  # testRegDate = c('2019-06-01', '2019-06-30')
)


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
features <- c(
  'dx_revenue',
  'dx_pay_count',
  'dx_login_count',
  'dx_session_count',
  'dx_session_time',
  'dx_gems_count',
  'dx_gems_spent',
  'tier'
)
modelMatrix <- switch(
  project,
  DA = matrix(
    c(
      # 1-variable models
      1, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 1,
      # 2-variable models (revenue + activity)
      1, 0, 1, 0, 0, 0, 0, 0,
      1, 0, 0, 1, 0, 0, 0, 0,
      1, 0, 0, 0, 1, 0, 0, 0,
      # 3-variable models(revenue + session_time + gems)
      1, 0, 0, 0, 1, 1, 0, 0,
      1, 0, 0, 0, 1, 0, 1, 0,
      # best model so far with tier
      1, 0, 0, 0, 1, 0, 0, 1,
      # full model for comparison
      1, 1, 1, 1, 1, 1, 1, 1
    ),
    nrow = length(features)
  ),
  SY = matrix(
    c(
      # 1-variable models
      1, 0, 0, 0, 0, 0, 0, 0,
      0, 1, 0, 0, 0, 0, 0, 0,
      0, 0, 1, 0, 0, 0, 0, 0,
      0, 0, 0, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 1, 0, 0, 0,
      0, 0, 0, 0, 0, 1, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 0,
      0, 0, 0, 0, 0, 0, 0, 1,
      # 2-variable models (gem_spent + revenue)
      1, 0, 0, 0, 0, 0, 1, 0,
      0, 1, 0, 0, 0, 0, 1, 0,
      # 2-variable models (revenue + activity)
      1, 0, 1, 0, 0, 0, 0, 0,
      1, 0, 0, 1, 0, 0, 0, 0,
      1, 0, 0, 0, 1, 0, 0, 0,
      # 3-variable models(gem_spent + revenue + activity)
      1, 0, 1, 0, 0, 0, 1, 0,
      1, 0, 0, 1, 0, 0, 1, 0,
      1, 0, 0, 0, 1, 0, 1, 0,
      # best model so far with tier
      1, 0, 0, 0, 1, 0, 1, 1,
      # full model for comparison
      1, 1, 1, 1, 1, 1, 1, 1
    ),
    nrow = length(features)
  )
)
rownames(modelMatrix) <- features
colnames(modelMatrix) <- paste('m', 1:ncol(modelMatrix), sep = '')
modelFormulas <- defineModelFormulas(modelMatrix)

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
modelCutOffs <- lapply(
  models,
  function(m) {
    calculateCutOff(datTrain$dy_payer, m$fitted.values)
  }
)
print(paste(length(models), "models trained with cut-offs"))

## evaluate on test data
modelEvals <- lapply(
  1:length(models),
  function(i) {
    evalLogitModel2(
      ref = datTest$dy_payer,
      fit = predict.glm(models[[i]], newdata = datTest, type = 'response'),
      cutOff = modelCutOffs[[i]]
    )
  }
)
print(paste(length(models), "models evaluated"))

modelSummary <- data.frame(
  model_id = 1:length(models),
  rank = sapply(models, function(m) {m$rank}),
  auc = sapply(modelEvals, function(m) {m$auc}),
  cut_off = unlist(modelCutOffs),
  rcd = sapply(modelEvals, function(m) {m$relativeCountDifference}),
  sensitivity = sapply(
    modelEvals,
    function(m) {
      t <- m$confMatrix$table
      t[2, 2]/(t[1, 2] + t[2, 2])
    }
  )
)


### Write summary
{
  summaryText <- switch(
    project,
    DA = paste(
      'THIS SUMMARY IS OUTDATED\n',
      'WE FOUND OUT THAT WE NEED CROSS-VALIDATION TO GET ROBUST RESULTS\n',
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
      '  dx_gems_spent = dx_gems_count (spent is cont.)\n',
      '  gems do not add anything and are correlated with revenue\n',
      'Check tier\n',
      '  it bends the ROC curve in a weird way but improves the model\n',
      'Comparison with the full model shows that it has slightly better performance.\n',
      'It should be compared with cross-validation whether this difference is robust.\n',
      'Best model so far has: dx_revenue, dx_session_time, tier'
    ),
    SY = paste(
      'THIS SUMMARY IS OUTDATED\n',
      'WE FOUND OUT THAT WE NEED CROSS-VALIDATION TO GET ROBUST RESULTS\n',
      'Summary for SY:\n',
      'Based on correlation matrix, these variables should not be together\n',
      '  dx_session_time, dx_session_count, dx_login_count\n',
      '  dx_pay_count, dx_revenue\n',
      '  dx_gems_spent, dx_gems_count\n',
      '  dx_gems_spent, dx_revenue\n',
      '  dx_gems_spent, dx_pay_count\n',
      'We will test 1-variable models\n',
      '  dx_revenue ~ dx_pay_count (we choose revenue)\n',
      '  dx_session_time > dx_session_count > dx_login_count (as expected)\n',
      '  dx_gems_spent > dx_gems_count\n',
      'Check session features with dx_revenue\n',
      '  dx_session_time > dx_session_count > dx_login_count (as expected)\n',
      'Check gem features with previous\n',
      '  dx_gems_spent > dx_gems_count\n',
      '  even though gems_spent is correlated with revenue it makes a better model\n',
      'Check tier\n',
      '  tier barely improve the model\n',
      'The full model is significantly better compared to reduced model.\n',
      'It should be compared with cross-validation whether this difference is robust.\n',
      'Best model so far has all variables.\n',
      'Maybe removing the features from the full model makes sense.\n',
      'Multicollinearity is present in the full model but is it a problem?\n',
      'dx_gem_spent is suspiciocly good, is it robust? We need CV...'
    )
  )
}


### Print results
filePathOutput <- file.path(
  '2_output',
  '06_feature_selection_manual_' %+% project %+% '_' %+%
    substr(config$trainRegDate[1], 6, 7) %+% '-' %+%
    substr(config$testRegDate[2], 6, 7) %+% '.pdf'
)
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

printOutput(modelMatrix)
printOutput(modelSummary)
grid.arrange(
  ggplot(modelSummary, aes(model_id, rank)) + geom_col(),
  ggplot(modelSummary, aes(model_id, rcd)) + geom_col(),
  ggplot(modelSummary, aes(model_id, auc)) + geom_col(),
  ggplot(modelSummary, aes(model_id, sensitivity)) + geom_col(),
  nrow = 2
)

printOutput(cat(summaryText))

for(l in 1:length(modelFormulas)) {
  plot.roc(
    modelEvals[[l]]$rocPlot,
    print.thres = TRUE,
    print.auc = TRUE,
    main = paste('ROC curve for model', l)
  )
  printOutput(summary(models[[l]]))
  printOutput(modelEvals[[l]][2:3])
  printOutput(list(optimal_cut_off = modelCutOffs[[l]]))
  printOutput(modelEvals[[l]][4])
}

dev.off()