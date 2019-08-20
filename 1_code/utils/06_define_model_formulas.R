# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# this utils function takes a matrix defining the models and
# returns the list of model formulas


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

defineModelFormulas <- function(modelMatrix) {
  
  features <- rownames(modelMatrix)
  
  modelBooleanMatrix <- matrix(
    as.logical(modelMatrix),
    ncol = ncol(modelMatrix)
  )
  
  result <- list()
  
  for(j in 1:ncol(modelMatrix)) {
    result[[j]] <- as.formula(paste(
      'dy_payer~',
      paste(features[modelBooleanMatrix[, j]], collapse = '+')
    ))
  }
  
  result
  
}