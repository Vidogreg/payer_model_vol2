## Function for plotting box plot
boxFeaturePlot <- function(x, y, ...) {
  featurePlot(
    x = x,
    y = y,
    plot = 'box',
    scales = list(
      y = list(relation = 'free')
    ),
    ...
  )
}

## Function for plotting density plot
densityFeaturePlot <- function(x, y, ...) {
  featurePlot(
    x = x,
    y = y,
    plot = 'density',
    scales = list(
      x = list(relation = 'free'),
      y = list(relation = 'free')
    ),
    adjust = 1.5,
    pch = '|',
    auto.key = list(columns = 2),
    ...
  )
}