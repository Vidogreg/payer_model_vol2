# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# Here, we load v5 datasets from hive


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

## Run utils
source('1_code/utils/00_run_utils.R')


## Load additional packages
packageTest('DBI')
packageTest('odbc')


## Define variables
hiveTable <- 'ga_972_v5_payer_dataset'

con <- DBI::dbConnect(
  odbc::odbc(),
  dsn = 'dwh-prod-ht-hive',
  schema = 'vgregor'
)

query <- paste(
  'SELECT * ', 'FROM vgregor.', hiveTable,
  ' WHERE days_in_game = 0 AND project = \'SY\' AND',
  ' register_platform IN (\'google_play\', \'apple_appstore\')',
  # ' LIMIT 100',
  sep = ''
)


## Load dataset
### Query the hive database
print(paste(Sys.time(), hiveTable, 'querying...'))
dfLoad <- data.table(dbGetQuery(con, query))
print(paste(Sys.time(), hiveTable, 'loaded to memory'))

### simplify column names
colnames(dfLoad) <- gsub('.*\\.', '', colnames(dfLoad))

### cast integer64 to integer
for (col in colnames(dfLoad)) {
  if(class(dfLoad[[col]]) == 'integer64')
    dfLoad[[col]] <- as.integer(dfLoad[[col]])
}


## save to rds file
saveRDS(dfLoad, file = file.path('0_data', paste(hiveTable, '_SY_GP&iOS_d0.rds', sep = '')))
# saveRDS(dfLoad, file = file.path('0_data', paste(hiveTable, '_SY_GP&iOS_d1.rds', sep = '')))


## Notes
# Full dataset SY, 2018-05-01 : 2018-10-31
#   dataset has 18 098 720 rows
#      .rds has        309 MB
# We will save only filtered rows SY, dig <= 1, Google & Apple
#   dataset has  4 211 952 rows
#      .rds has         65 MB
# We split d0 and d1 to two files (29 MB + 34 MB)