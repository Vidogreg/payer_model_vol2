# ---------------------------------------------------------------------------------------
# Introduction
# ---------------------------------------------------------------------------------------
# We observe how the sensitivity of classification model affects the count
# of predicted positives/count of actual positives (when cut-off is optimized for it)


# ---------------------------------------------------------------------------------------
# Main code
# ---------------------------------------------------------------------------------------

### Run utils
source('1_code/utils/00_run_utils.R')
packageTest('extraDistr')


### Define stuff
randomSeed <- 1
N <- 500000
conv <- 0.02
sensitivities <- c(conv, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
# sensitivities <- c(0.1, 0.4)
sampleSizes <- c(100, 500, 1000, 2000, 10000)
# sampleSizes <- c(100, 500)
sampleCount <- 500

configVals <- list(
  randomSeed = randomSeed,
  N = N,
  conv = conv,
  sensitivities = sensitivities,
  sampleSizes = sampleSizes,
  sampleCount = sampleCount
)

makePred <- function(x, sens, fpr) {
  if(x == 0) rbern(1, fpr) else rbern(1, sens)
}


### Simulation
filePath <- file.path('2_output', '05_count_prediction_by_sensitivity.pdf')
pdf(filePath)
printOutput(
  configVals
)

for (sampleSize in sampleSizes) {
  
  set.seed(randomSeed)
  result <- data.frame(
    sensitivity = c(sapply(sensitivities, FUN = function(x) {rep(x, sampleCount)})),
    error = rep(NaN, sampleCount*length(sensitivities))
  )
  rowIndex <- 1
  print('sample size: ' %+% sampleSize)
  
  for (j in 1:length(sensitivities)) {
    
    sens <- sensitivities[j]
    
    # Calculate population and predictions
    fpr <- conv*(1 - sens)/(1 - conv)
    population <- data.frame(
      id = 1:N,
      payer = rbern(N, conv)
    )
    population$prediction <- sapply(
      population$payer,
      makePred,
      sens = sens,
      fpr = fpr
    )
    
    # population confusion table
    print('sensitivity: ' %+% sens)
    print(table(population$payer, population$prediction))
    
    # Calculate samples from the population
    errorCounts <- rep(NaN, sampleCount)
    
    for(i in 1:sampleCount) {
      popSampleIndex <- sort(sample(population$id, sampleSize, replace = FALSE))
      popSample <- population[popSampleIndex, ]
      result[rowIndex, 2] <- sum(popSample$prediction) - sum(popSample$payer)
      rowIndex <- rowIndex + 1
    }
    
  }
  
  # Print resulting boxplot
  p <- boxplot(
    error ~ sensitivity,
    result,
    main = 'Error of predicted count of payers, players per sample: ' %+% sampleSize
  )
  cat('\n\n\n')
  
}
dev.off()



# ## theoretical confusion table
# theorTP <- round(N*conv*sens)
# theorTN <- round(N*(1 - 2*conv + conv*sens))
# theorFP <- round(N*conv*(1 - sens))
# theorFN <- round(theorFP)
# 
# theor <- data.frame(
#   payer = c(
#     rep(0, theorTN + theorFP),
#     rep(1, theorFN + theorTP)
#   ),
#   prediction = c(
#     rep(0, theorTN),
#     rep(1, theorFP),
#     rep(0, theorFN),
#     rep(1, theorTP)
#   )
# )
# print(table(theor$payer, theor$prediction))