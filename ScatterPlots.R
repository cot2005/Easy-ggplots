# author: Colin Tang
# simple function to make scatter plots from data frames
# each graphing variable can be specified by the column index or the string of the column name.
#

library("ggplot2")
library("wesanderson")

ggEasy.scatter<-function(df, xval = 1, yval = 2, shape = NA, shape.selection = 16,color = NA, color.selection = "black",facet = NA, point.size = 2.5, alpha = 0.8,
                          palette = wes_palette("Darjeeling1", length(unique(df[,color])), type = "continuous"),
                          outputName = "scatterPlot.pdf", print = FALSE, print.height = 5, print.width = 6) {
  df <- as.data.frame(df)
  # flexible inputs
  header <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(header == xval))
  yval <- ifelse(is.numeric(yval), yval, which(header == yval))
  color <- ifelse(is.numeric(color), color, which(header == color))
  shape <- ifelse(is.numeric(shape), shape, which(header == shape))
  
  g <- ggplot(df, aes(x = as.numeric(df[,xval]), y = as.numeric(df[,yval]), color = as.factor(df[,color]), shape = as.factor(df[,shape]))) +
    theme_bw() + labs(x = header[xval], y = header[yval])
  # adds proper point aesthetics
  if (is.na(shape) && is.na(color)) {
    g <- g + geom_point(alpha = alpha, size = point.size, shape = shape.selection, color = color.selection)
  } else if (is.na(shape)){
    g <- g + geom_point(alpha = alpha, size = point.size, shape = shape.selection) +
      guides(color = guide_legend(title=header[color])) + scale_color_manual(values = palette)
  } else if (is.na(color)){
    g <- g + geom_point(alpha = alpha, size = point.size, color = color.selection) +
      guides(shape = guide_legend(title=header[shape]))
  } else {
    g <- g + geom_point(alpha = alpha, size = point.size) +
      guides(color = guide_legend(title=header[color]), shape = guide_legend(title=header[shape])) +
      scale_color_manual(values = palette)
  }
  if (!is.na(facet)) {
    g <- g + facet_wrap(df[,facet])
  }
  if (print == TRUE) {
    ggsave(outputName, width = print.width, height = print.height)
  }
  return(g)
}
