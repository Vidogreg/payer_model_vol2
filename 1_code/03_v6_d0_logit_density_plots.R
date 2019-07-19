# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Plot feature  and fit densities


# ---------------------------------------------------------------------------------------
# Main code
# ---------------------------------------------------------------------------------------

### Define config variables
project <- 'DA'
trainRegMonth <- c('2018-10-01')
testRegMonth <- c('2018-12-01')
randomSeed <- 1024


### Run utils
source('1_code/utils/00_run_utils.R')


### Load data
fileName <- 'ga_972_v6_payer_dataset_' %+% project %+% '_GP_d0.rds'
if(!exists('dfLoad'))
  dfLoad <- data.table(readRDS(
    file.path('0_data', fileName)
  ))

## Define dataset
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

## Define train and test datasets
datTrain <- dat[register_month == trainRegMonth, ]
datTrain <- datTrain[, c('player_id', 'register_month', 'dy_pay_count') := NULL]
datTrain[, dy_payer := factor(dy_payer)]

datTest <- dat[register_month == testRegMonth, ]
datTest <- datTest[, c('player_id', 'register_month', 'dy_pay_count') := NULL]
datTest[, dy_payer := factor(dy_payer)]

## Train model
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

## Calculate fit on datTrain
datTest$model_fit <- predict.glm(
  model, newdata = datTest, type = 'response'
)

## down-sample datasets for plotting
set.seed(randomSeed)
datTrainDown <- data.table(
  downSample(x = datTrain, y = datTrain$dy_payer)
)
setnames(datTrainDown, old = c('Class'), new = c('dy_payer'))
datTestDown <- data.table(
  downSample(x = datTest, y = datTest$dy_payer)
)
setnames(datTestDown, old = c('Class'), new = c('dy_payer'))





pdf('zzz.pdf')

for(i in c(1, 4, 5, 6)) {
  p <- featurePlot(
    x = datTrainDown[, i, with = FALSE],
    y = datTrainDown$dy_payer,
    plot = 'density',
    scales = list(
      x = list(relation = 'free'),
      y = list(relation = 'free')
    ),
    adjust = 1.5,
    pch = '|',
    auto.key = list(columns = 2),
    lwd = 2
  )
  print(p)
}

p <- featurePlot(
  x = datTestDown[, 2],
  y = datTestDown$dy_payer,
  plot = 'density',
  scales = list(
    x = list(relation = 'free'),
    y = list(relation = 'free')
  ),
  adjust = 1.5,
  pch = '|',
  auto.key = list(columns = 2),
  ylim = c(0, 1.5),
  lwd = 2
)
print(p)

p <- featurePlot(
  x = datTestDown[, 3],
  y = datTestDown$dy_payer,
  plot = 'density',
  scales = list(
    x = list(relation = 'free'),
    y = list(relation = 'free')
  ),
  adjust = 1.5,
  pch = '|',
  auto.key = list(columns = 2),
  ylim = c(0, 0.3),
  lwd = 2
)
print(p)

p <- featurePlot(
  x = datTestDown[, 8],
  y = datTestDown$dy_payer,
  plot = 'density',
  scales = list(
    x = list(relation = 'free'),
    y = list(relation = 'free')
  ),
  adjust = 1.5,
  pch = '|',
  auto.key = list(columns = 2),
  ylim = c(0, 8),
  lwd = 2
)
print(p)

dev.off()