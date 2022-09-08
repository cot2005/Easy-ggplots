# author: Colin Tang
# simple function to make box plots from csv's (default) where
# each parameter number defines the column for the value.
#

library("ggplot2")

ggEasy.scatter4<-function(df, xval = 1, yval = 2, shape = 3, color = 4, facet = FALSE, point.size = 2.5, alpha = 0.8,
                          palette = wes_palette("Darjeeling1", length(unique(df[,color])), type = "continuous"),
                          outputName = "scatterPlot.pdf", print = FALSE, print.height = 5, print.width = 6) {
  df <- as.data.frame(df)
  # flexible inputs
  header <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(header == xval))
  yval <- ifelse(is.numeric(yval), yval, which(header == yval))
  color <- ifelse(is.numeric(color), color, which(header == color))
  shape <- ifelse(is.numeric(shape), shape, which(header == shape))

  g <- ggplot(df, aes(x = as.numeric(df[,xval]), y = as.numeric(df[,yval]), group = as.factor(df[,color]), color = as.factor(df[,color]), shape = as.factor(df[,shape])))
  g <- g + geom_point(alpha = alpha, size = point.size) + 
    theme_bw() + labs(x = header[xval], y = header[yval]) + 
    guides(color = guide_legend(title=header[color]), shape = guide_legend(title=header[shape])) + 
    scale_color_manual(values = palette)
  if (print == TRUE) {
    ggsave(outputName, width = print.width, height = print.height)
  }
  return(g)
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
