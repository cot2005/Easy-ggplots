# author: Colin Tang
# simple function to make box plots from csv's (default) where
# each parameter number defines the column for the value.
#

library("ggplot2")

ggEasy.boxplot4<-function(inputFile, xval = 1, yval = 2, shape = 3, fill = 4, sep = ",", 
                          outputName = "boxPlot.pdf", height = 5, width = 6, facet = FALSE) {
  df <- read.csv(inputFile, stringsAsFactors = T, sep = sep)
  header <- colnames(df)
  for (i in c(xval, shape, fill)) {
    df[,i] <- as.factor(df[,i])
  }
  g <- ggplot(df, aes(x = df[,xval], y = df[,yval], fill = df[,fill]))
  if (facet == TRUE) {   # adds facet wrapper
    g <- g + facet_grid(cols = vars(df[,fill]))
  }
  g + geom_boxplot(alpha = 0.5, color = "black") + geom_point(aes(group = df[,fill], shape = df[,shape]), alpha = 0.8, size = 2.5, position=position_jitterdodge()) + 
    scale_shape_manual(values = 21:(20 + length(levels(df[,shape])))) + theme_bw() + labs(x = header[xval], y = header[yval]) + 
    guides(shape = guide_legend(title=header[shape]), fill = guide_legend(title=header[fill]))
  ggsave(outputName, height = height, width = width)
}

ggEasy.boxplot3<-function(inputFile, xval = 1, yval = 2, fill = 3, sep = ",", 
                          outputName = "boxPlot.pdf", height = 5, width = 6, facet = FALSE) {
  df <- read.csv(inputFile, stringsAsFactors = T, sep = sep)
  header <- colnames(df)
  for (i in c(xval, fill)) {
    df[,i] <- as.factor(df[,i])
  }
  g <- ggplot(df, aes(x = df[,xval], y = df[,yval], fill = df[,fill]))
  if (facet == TRUE) {   # adds facet wrapper
    g <- g + facet_grid(cols = vars(df[,fill]))
  }
  g + geom_boxplot(alpha = 0.5, color = "black") + 
    geom_point(aes(group = df[,fill]), shape = 21, alpha = 0.8, size = 2.5, position=position_jitterdodge()) + 
    theme_bw() + labs(x = header[xval], y = header[yval]) + guides(fill = guide_legend(title=header[fill]))
  ggsave(outputName, height = height, width = width)
}
