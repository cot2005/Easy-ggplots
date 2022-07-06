# author: Colin Tang
# simple function to make histogram plots from csv's (default) where
# each parameter number defines the column for the value.
#

library("ggplot2")

ggEasy.histogram<-function(df, xval = 1, yaxis = "count", fill = NA, sep = ",", alpha = 0.5, binNum = 30, bars = FALSE,
                           outputName = "histogramPlot.pdf", print = FALSE, print.height = 5, print.width = 7) {
  # flexible inputs
  headers <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(headers == xval))
  fill <- ifelse(is.numeric(fill), fill, which(headers == fill))
  if (is.na(fill)) {
    g <- ggplot(df, aes(df[,xval]))
  } else {
    g <- ggplot(df, aes(x = df[,xval], fill = df[,fill])) + guides(fill = guide_legend(title=headers[fill]))
  }
  if (yaxis == "count") {
    g <- g + geom_histogram(aes(y=..count..),  position="identity", alpha=alpha, color = "black", size = 0.25, binwidth = max(df[,xval])/binNum)
  } else {
    g <- g + geom_density(alpha=alpha, color = "black", size = 0.25)
    if (bars == TRUE) {
      g <- g + geom_histogram(aes(y=..density..), position="identity", color = "black", alpha=alpha, size = 0.25, binwidth = max(df[,xval])/binNum)
    }
  }
  g <- g + theme_bw() + xlab(colnames(df)[xval]) + scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
  
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}
