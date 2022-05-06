library(ggplot2)

ggEasy.histogram<-function(inputFile, xval = 1, yaxis = "count", sep = ",", binNum = 30, graphColor = "#46ACC8",outputName = "HistogramPlot.pdf", height = 5, width = 7) {
  inputData <- read.csv(inputFile, sep = sep, header = T)
  g <- ggplot(inputData, aes(inputData[,xval]))
  if (yaxis == "count") {
    g <- g + geom_histogram(aes(y=..count..), alpha=0.7, color = "black", fill = graphColor, size = 0.25, binwidth = max(inputData[,xval])/binNum)
  } else {
    g <- g + geom_density(alpha=0.4, color = "black", fill = graphColor, size = 0.25) +
      geom_histogram(aes(y=..density..), color = "black", fill = graphColor, alpha=0.7, size = 0.25, binwidth = max(inputData[,xval])/binNum)
  }
  g <- g + theme_bw() + xlab(colnames(inputData)[xval]) + scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  g
  ggsave(outputName, width = width, height = height)
}
