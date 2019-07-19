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
    
    confMatrix <- confusionMatrix(
      data = pred,
      reference = refFactor,
      positive = 'TRUE'
    )
  } else {
    rocObj <- NULL
    rocPlot <- list(auc = NULL)
    cutOffOptimal <- NULL
    confMatrix <- NULL
  }
  
  list(
    rocPlot = rocPlot,
    auc = rocObj$auc,
    cutOff = cutOffOptimal,
    confMatrix = confMatrix
  )
  
}