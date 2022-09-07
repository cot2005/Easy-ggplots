# author: Colin Tang
# simple function to make box plots from csv's (default) where
# each parameter number defines the column for the value.
#

library("ggplot2")

ggEasy.scatter4<-function(inputFile, xval = 1, yval = 2, shape = 3, fill = 4, sep = ",", 
                          outputName = "scatterPlot.pdf", height = 5, width = 6, facet = FALSE) {
  df <- read.csv(inputFile, stringsAsFactors = T, sep = sep)
  header <- colnames(df)
  for (i in c(shape, fill)) {
    df[,i] <- as.factor(df[,i])
  }
  g <- ggplot(df, aes(x = df[,xval], y = df[,yval], group = df[,fill], fill = df[,fill], shape = df[,shape]))
  g + geom_point(alpha = 0.8, size = 2.5) + 
    scale_shape_manual(values = 21:(20 + length(levels(df[,shape])))) +
    theme_bw() + labs(x = header[xval], y = header[yval]) +
    guides(fill = guide_legend(title=header[fill], override.aes = list(shape = 21, colour = "black")), shape = guide_legend(title=header[shape]))
  ggsave(outputName, height = height, width = width)
}
  
  
ggEasy.scatter3<-function(inputFile, xval = 1, yval = 2, fill = 3,sep = ",", outputName = "scatterPlot.pdf", height = 5, width = 6) {
  df <- read.csv(inputFile, stringsAsFactors = T, sep = sep)
  header <- colnames(df)
  df[,fill] <- as.factor(df[,fill])
  g <- ggplot(df, aes(x = df[,xval], y = df[,yval], group = df[,fill], fill = df[,fill]))
  g + geom_point(alpha = 0.8, size = 2.5, shape = 21) +
    theme_bw() + labs(x = header[xval], y = header[yval]) +
    guides(fill = guide_legend(title=header[fill], override.aes = list(shape = 21, colour = "black")))
  ggsave(outputName, height = height, width = width)
}
