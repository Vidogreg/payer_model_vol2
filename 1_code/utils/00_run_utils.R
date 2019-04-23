# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# This file runs all utils scripts and load common variables and packages


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

## Set working directory
PROJECT <- 'payer_model_vol2'
PROJECT_DIR <- file.path(
  'C:/Users/vgregor/OneDrive - PXFD',
  '_PIXEL FEDERATION/GA issues/Games General/GA-972 Payer model'
)
setwd(file.path(PROJECT_DIR, PROJECT))


## Source utils folder
fileArray <- list.files('1_code/utils')[-1]
for(fileName in fileArray) {
  source(paste('1_code/utils/', fileName, sep = ''))
}


## load useful packages
packageTest('data.table')
packageTest('caret')
packageTest('pROC')