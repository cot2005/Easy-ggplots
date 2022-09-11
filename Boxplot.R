# author: Colin Tang
# simple function to make box plots from csv's (default) where
# each parameter number defines the column for the variable to be plotted. NULL shape or fill variables will
# adjust the output graph. 
#

library("ggplot2")

ggEasy.boxplot<-function(inputFile, xval = 1, yval = 2, shape = NULL, fill = NULL, facetGroup = NULL, sep = ",", 
                         angle.X = TRUE, box.width = 1, point.size = 2.5,
                          outputName = "boxPlot.pdf", height = 5, width = 6) {
  df <- read.csv(inputFile, stringsAsFactors = T, sep = sep, check.names = F)
  header <- colnames(df)
  for (i in c(xval, shape, fill)) {
    df[,i] <- as.factor(df[,i])
  }
  if (is.null(fill) == TRUE && is.null(shape) == TRUE) {   # 2 var
    g <- ggplot(df, aes(x = df[,xval], y = df[,yval]))
    g <- g + geom_boxplot(alpha = 0.5, color = "black", width = box.width) + 
      geom_point(aes(group = df[,xval]), shape = 21, alpha = 0.8, size = point.size) + 
      theme_bw() + labs(x = header[xval], y = header[yval])
  } else if (is.null(fill) == TRUE) {   # 3 var null fill/ no grouping
    g <- ggplot(df, aes(x = df[,xval], y = df[,yval]))
    g <- g + geom_boxplot(alpha = 0.5, color = "black", width = box.width) + 
      geom_point(aes(shape = df[,shape]), alpha = 0.8, size = point.size, position=position_jitterdodge(dodge.width = box.width, jitter.width = box.width/2)) + 
      scale_shape_manual(values = 21:(20 + length(levels(df[,shape])))) + theme_bw() + labs(x = header[xval], y = header[yval]) + 
      guides(shape = guide_legend(title=header[shape]))
  } else if (is.null(shape) == TRUE) {   # 3 var null shape
    g <- ggplot(df, aes(x = df[,xval], y = df[,yval], fill = df[,fill]))
    g <- g + geom_boxplot(alpha = 0.5, color = "black", width = box.width) + 
      geom_point(aes(group = df[,fill]), shape = 21, alpha = 0.8, size = point.size, position=position_jitterdodge(dodge.width = box.width, jitter.width = box.width/2)) + 
      theme_bw() + labs(x = header[xval], y = header[yval]) + guides(fill = guide_legend(title=header[fill]))
  } else {   # all 4 var
    g <- ggplot(df, aes(x = df[,xval], y = df[,yval], fill = df[,fill]))
    g <- g + geom_boxplot(alpha = 0.5, color = "black", width = box.width) + 
      geom_point(aes(group = df[,fill], shape = df[,shape]), alpha = 0.8, size = point.size, position=position_jitterdodge(dodge.width = box.width, jitter.width = box.width/2)) + 
      scale_shape_manual(values = 21:(20 + length(levels(df[,shape])))) + theme_bw() + labs(x = header[xval], y = header[yval]) + 
      guides(shape = guide_legend(title=header[shape]), fill = guide_legend(title=header[fill]))
  }
  if (angle.X == TRUE) {   # angles X axis labels
    g <- g + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  }
  if (is.null(facetGroup) == FALSE) {   # adds facet wrapper
    g <- g + facet_wrap(~get(colnames(df)[facetGroup]))))
  }
  print(g)
  ggsave(outputName, height = height, width = width)
}
