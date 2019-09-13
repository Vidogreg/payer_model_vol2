# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# Here, we load datasets from hive


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

## Run utils
source('1_code/utils/00_run_utils.R')


## Load additional packages
packageTest('DBI')
packageTest('odbc')
packageTest('plyr')


## Define variables
hiveTableFeat <- 'payer_model_features'
hiveTableLab <- 'payer_model_labels'
# project <- 'DA'
# project <- 'SY'
project <- 'TSM'
# registerDateFrom <- '2019-01-01'
# registerDateFrom <- '2019-04-01'
registerDateFrom <- '2019-06-01'
# registerDateTo <- '2019-03-31'
# registerDateTo <- '2019-06-30'
registerDateTo <- '2019-08-31'

con <- DBI::dbConnect(
  odbc::odbc(),
  dsn = 'dwh-prod-ht-hive',
  schema = 'mkt'
)


## Load from hive
print(paste(Sys.time(), hiveTableFeat, 'querying...'))
dfLoadFeat <- data.table(
  dbGetQuery(
    con,
    paste(
      'SELECT * ', 'FROM mkt.', hiveTableFeat,
      ' WHERE project = \'', project, '\' AND',
      ' register_platform IN (\'google_play\', \'apple_appstore\') AND',
      ' source = \'marketing\' AND',
      ' register_date >= \'', registerDateFrom, '\' AND',
      ' register_date <= \'', registerDateTo, '\'',
      # ' LIMIT 100',
      sep = ''
    )
  )
)
print(paste(Sys.time(), hiveTableFeat, 'loaded to memory'))

print(paste(Sys.time(), hiveTableLab, 'querying...'))
dfLoadLab <- data.table(
  dbGetQuery(
    con,
    paste(
      'SELECT player_id, days_in_game, dy_pay_count, dy_revenue, project ',
      'FROM mkt.', hiveTableLab,
      ' WHERE project = \'', project, '\' AND',
      ' register_date >= \'', registerDateFrom, '\' AND',
      ' register_date <= \'', registerDateTo, '\'',
      # ' LIMIT 100',
      sep = ''
    )
  )
)
print(paste(Sys.time(), hiveTableLab, 'loaded to memory'))

### simplify column names
colnames(dfLoadFeat) <- gsub('.*\\.', '', colnames(dfLoadFeat))
colnames(dfLoadLab) <- gsub('.*\\.', '', colnames(dfLoadLab))

### cast integer64 to integer
for (col in colnames(dfLoadFeat)) {
  if(class(dfLoadFeat[[col]]) == 'integer64')
    dfLoadFeat[[col]] <- as.integer(dfLoadFeat[[col]])
}
for (col in colnames(dfLoadLab)) {
  if(class(dfLoadLab[[col]]) == 'integer64')
    dfLoadLab[[col]] <- as.integer(dfLoadLab[[col]])
}


## Join features with labels
dfAll <- join(
  dfLoadFeat, dfLoadLab,
  by = c('project', 'player_id', 'days_in_game'),
  type = 'left'
)

dfFinal <- dfAll[, c(
  'project',
  'player_id',
  'register_date',
  'register_platform',
  'source',
  'country',
  'tier',
  'days_in_game',
  'dx_pay_count',
  'dx_revenue',
  'd0_session_count',
  'd1x_session_count',
  'dx_session_count',
  'd0_session_time',
  'd1x_session_time',
  'dx_session_time',
  'dx_session_days',
  'd0_login_count',
  'd1x_login_count',
  'dx_login_count',
  'dx_gems_count',
  'dx_gems_spent',
  'dy_pay_count',
  'dy_revenue'
)]


## save to rds file
fileName <- paste(
  'payer_model_', project, '_GP&iOS_mkt_',
  registerDateFrom, '_', registerDateTo, '.rds',
  sep = ''
)
saveRDS(dfFinal, file = file.path('0_data', fileName))
print(paste(Sys.time(), fileName, 'saved in hard drive'))