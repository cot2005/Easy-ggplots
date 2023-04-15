# author: Colin Tang
# simple function to make histogram plots from csv's (default) where
# each parameter number defines the column for the value.
#

library("ggplot2")
library("wesanderson")

ggEasy.histogram<-function(df, xval = 1, yaxis = "count", color = NA, sep = ",", legend = TRUE,
                           alpha = 0.5, binNum = 30, density.bars = FALSE, line.size = 0.25, 
                           palette = wes_palette(palette.wes_palette, length(unique(df[,color])), type = "continuous"),
                           palette.wes_palette = "Darjeeling1",
                           outputName = "histogramPlot.pdf", print = FALSE, print.height = 5, print.width = 7) {
  df <- as.data.frame(df)
  # flexible inputs
  headers <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(headers == xval))
  color <- ifelse(is.numeric(color), color, which(headers == color))
  
  # adds filler column for NA purposes
  if (is.na(color)) {
    df$FillerColor <- "filler"
    color <- ncol(df)
  }
    
  # constructs ggplot object
  g <- ggplot(df)
  
  g <- ggplot(df, aes(x = df[,xval], fill = factor(df[,color]), group = factor(df[,color])))
  if (yaxis == "count") {   # count histogram
    g <- g + geom_histogram(aes(y=..count..),  position="identity", 
                            alpha = alpha, size = line.size, color = "black",
                            binwidth = max(df[,xval], na.rm = T)/binNum)
  } else {   # assumes density if not count
    g <- g + geom_density(alpha=alpha, color = "black",
                          size = line.size)
    if (density.bars == TRUE) {
      g <- g + geom_histogram(aes(y=..density..), position="identity", 
                              color = "black", alpha=alpha, size = line.size, 
                              binwidth = max(df[,xval], na.rm = T)/binNum)
    }
  }
  g <- g + theme_bw() + scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
    xlab(headers[xval]) + 
    scale_fill_manual(values = palette)
  
  if ("FillerColor" %in% colnames(df)) {
    g <- g + theme(legend.position = "none")
  } else {
    g <- g + guides(fill = guide_legend(title = headers[color]))
  }
  
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}
