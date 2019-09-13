# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# Sample calculation of v7 model for comparison with spark production job

## Run utils
source('1_code/utils/00_run_utils.R')

## Define stuff
project <- 'DA'
# project <- 'SY'
# platform <- 'google_play'
platform <- 'apple_appstore'
config <- list(
  project = project,
  platform = platform,
  maxDigX = 6,
  dataFile = 'payer_model_' %+% project %+% '_GP&iOS_mkt_2019-06-01_2019-08-31.rds',
  trainRegDate = c('2019-06-23', '2019-08-03'),
  testRegDate = c('2019-08-25', '2019-08-31')
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
    source == 'marketing' &
    days_in_game <= config$maxDigX,
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

## Define train dataset
datTrain <- dat[
  register_date >= config$trainRegDate[1] &
    register_date <= config$trainRegDate[2], ]

## Define test dataset
datTest <- dat[
  register_date >= config$testRegDate[1] &
    register_date <= config$testRegDate[2], ]



### Train model
formulaString <- paste(
  'dy_payer ~',
  'tier +',
  'days_in_game +',
  'dx_pay_count +',
  'dx_revenue +',
  'dx_gems_count +',
  'dx_gems_spent +',
  'dx_session_count +',
  'dx_session_time +',
  'dx_session_days +',
  'dx_login_count'
)

mod <- glm(
  formula = formulaString,
  data = datTrain,
  family = 'binomial'
)
datTrain$fit <- predict.glm(mod, newdata = datTrain, type = 'response')

## Calculate RCD-optimal cut-offs
digs <- 0:config$maxDigX
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

print(summary(mod))
print(cutOffs)



### Calculate predictions
datTest$fit <- predict.glm(mod, newdata = datTest, type = 'response')



### Check results
print(
  datTest %>%
    filter(days_in_game == 6) %>%
    group_by(register_date) %>%
    summarize(
      count = n(),
      min(fit),
      max(fit),
      mean(fit),
      sum(fit)
    )
)
print(
  datTest %>%
    filter(days_in_game == 6) %>%
    filter(player_id %in% c(34708528, 34897520))
)