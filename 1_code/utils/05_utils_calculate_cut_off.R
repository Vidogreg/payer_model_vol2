# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# utils for calculating RCD-optimal cut-off
# it should be used with the training data


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

calculateCutOff <- function(ref, fit) {
  
  optimize(
    differenceFPFN,
    interval = c(0, 1),
    fit = fit,
    ref = ref
  )$minimum
  
}