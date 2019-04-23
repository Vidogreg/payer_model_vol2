# -----------------------------------------------------------------------------
# Introduction
# -----------------------------------------------------------------------------
# This includes the most basic utils in for this project


# -----------------------------------------------------------------------------
# Main code
# -----------------------------------------------------------------------------

## Operator for pasting strings together
'%+%' <- function(a, b) paste(a, b, sep = '')


## Checks whether a package is installed before loading
packageTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE))
      stop('Package not found')
  }
}


## Prints console output into a plot
printOutput <- function(output, cex = 0.7) {
  tmp <- capture.output(output)
  plot.new()
  text(0, 1, paste(tmp, collapse='\n'), adj = c(0,1), family = 'mono', cex = cex)
  box()
}