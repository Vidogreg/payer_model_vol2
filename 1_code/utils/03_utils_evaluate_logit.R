# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# utils for quick evaluating of logit model
# it calculates ROC curve, AUC, optimal cut-off and confucion matrix


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

evalLogitModel <- function(ref, fit) {
  
  refFactor <- factor(ref)
  if(length(levels(refFactor)) > 1) {
    rocObj <- roc(
      response = refFactor,
      predictor = fit,
      levels = c(FALSE, TRUE),
      direction = '<',
      auc = TRUE
    )
    
    rocPlot <- plot.roc(
      rocObj,
      print.thres = TRUE,
      print.auc = TRUE,
      main = 'ROC curve'
    )
    
    cutOffOptimal <- optimize(
      differenceFPFN,
      interval = c(0, 1),
      fit = fit,
      ref = ref
    )$minimum
    
    pred <- factor(fit >= cutOffOptimal, levels = c(FALSE, TRUE))
    rcd <- length(pred[pred == TRUE])/length(ref[ref == TRUE])
    
    confMatrix <- confusionMatrix(
      data = pred,
      reference = refFactor,
      positive = 'TRUE'
    )
  } else {
    rocObj <- NULL
    rocPlot <- list(auc = NULL)
    cutOffOptimal <- NULL
    rcd <- NULL
    confMatrix <- NULL
  }
  
  list(
    rocPlot = rocPlot,
    auc = rocObj$auc,
    cutOff = cutOffOptimal,
    relativeCountDifference = rcd,
    confMatrix = confMatrix
  )
  
}


evalLogitModel2 <- function(ref, fit, cutOff) {
  
  refFactor <- factor(ref)
  if(length(levels(refFactor)) > 1) {
    rocObj <- roc(
      response = refFactor,
      predictor = fit,
      levels = c(FALSE, TRUE),
      direction = '<',
      auc = TRUE
    )
    
    rocPlot <- plot.roc(
      rocObj,
      print.thres = TRUE,
      print.auc = TRUE,
      main = 'ROC curve'
    )
    
    pred <- factor(fit >= cutOff, levels = c(FALSE, TRUE))
    rcd <- length(pred[pred == TRUE])/length(ref[ref == TRUE])
    
    confMatrix <- confusionMatrix(
      data = pred,
      reference = refFactor,
      positive = 'TRUE'
    )
  } else {
    rocObj <- NULL
    rocPlot <- list(auc = NULL)
    rcd <- NULL
    confMatrix <- NULL
  }
  
  list(
    rocPlot = rocPlot,
    auc = rocObj$auc,
    relativeCountDifference = rcd,
    confMatrix = confMatrix
  )
  
}