# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# utils for calculating optimal cut-off directly or by CV


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

## difference of FP and FN - this will be minimized 
differenceFPFN <- function(cutOff, fit, ref, printCM = FALSE) {
  confMatrix <- table(fit > cutOff, ref)
  if(printCM) print(confMatrix)
  if(dim(confMatrix)[1] < 2) {
    if(row.names(confMatrix) == "TRUE") confMatrix[1, 1]
    else confMatrix[1, 2]
  } else abs(confMatrix[1, 2] - confMatrix[2, 1])
}

## calculation of optimal cut-off using k-fold CV
makeCrossValCutOff <- function(foldCount, data) {
  
  if('mod_fit' %in% colnames(data)) data$mod_fit <- NULL
  if('model_fit' %in% colnames(data)) data$model_fit <- NULL
  foldsIndex <- createFolds(data$dy_payer, k = foldCount)
  
  for(k in 1:foldCount) {
    dfTrain <- data[-foldsIndex[[k]], ]
    mod <- glm(
      formula = dy_payer ~ .,
      data = dfTrain,
      family = 'binomial'
    )
    
    dfVal <- data[foldsIndex[[k]], ]
    dfVal[, mod_fit := predict.glm(mod, newdata = dfVal, type = 'response')]
    
    cutOffOptimal <- optimize(
      differenceFPFN,
      interval = c(0, 1),
      fit = dfVal$mod_fit,
      ref = dfVal$dy_payer
    )$minimum
    
    if(k == 1) {
      cvResult <- NULL
      cvResult <- data.frame(matrix(
        c(
          mod$coefficients,
          cutOffOptimal,
          rep(0, (foldCount - 1)*(length(mod$coefficients) + 1))
        ),
        nrow = foldCount, byrow = TRUE
      ))
      colnames(cvResult) <- c(names(mod$coefficients), "optimal_cutoff")
    } else {
      cvResult[k, ] <- c(mod$coefficients, cutOffOptimal)
    }
    
    print('Fold #' %+% k %+% ' DONE')
  }
  
  cutOffs <- c(
    quantile(cvResult$optimal_cutoff, probs = 0.1),
    mean(cvResult$optimal_cutoff),
    quantile(cvResult$optimal_cutoff, probs = 0.9)
  )
  names(cutOffs) <- c('cut_off_cv_0.1q', 'cut_off_cv_avg', 'cut_off_cv_0.9q')
  
  list(
    cvResult = cvResult,
    cutOffs = cutOffs
  )
  
}