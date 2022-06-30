# author: Colin Tang
# simple function to make histogram plots from csv's (default) where
# each parameter number defines the column for the value.
#

library("ggplot2")

ggEasy.histogram<-function(inputFile, xval = 1, yaxis = "count", sep = ",", binNum = 30, graphColor = "#46ACC8", 
                           outputName = "HistogramPlot.pdf", print = FALSE, print.height = 5, print.width = 7) {
  # flexible inputs
  headers <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(headers == xval))
  
  g <- ggplot(inputData, aes(inputData[,xval]))
  if (yaxis == "count") {
    g <- g + geom_histogram(aes(y=..count..), alpha=0.7, color = "black", fill = graphColor, size = 0.25, binwidth = max(inputData[,xval])/binNum)
  } else {
    g <- g + geom_density(alpha=0.4, color = "black", fill = graphColor, size = 0.25) +
      geom_histogram(aes(y=..density..), color = "black", fill = graphColor, alpha=0.7, size = 0.25, binwidth = max(inputData[,xval])/binNum)
  }
  g <- g + theme_bw() + xlab(colnames(inputData)[xval]) + scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}
