# errortype: 
# cl = confidence limits
# sd = standard deviation
# sem = standard error of the mean
#

library(ggplot2)
library(wesanderson)
ggEasy.scatter.stat<-function(df, xval = 1, yval = 2, color = 3, errortype = "sd", connectLine = FALSE, 
                              error.width = 0.05, line.size = 0.5, point.size = 1,alpha = 0.8, 
                              palette = wes_palette(palette.wes_palette, length(unique(df[,color])), type = "continuous"),
                              palette.wes_palette = "Darjeeling1",
                              print = FALSE, outputName = "statScatterPlot.pdf", print.width = 8, print.height = 6) {
  df <- as.data.frame(df)
  # flexible inputs
  headers <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(headers == xval))
  yval <- ifelse(is.numeric(yval), yval, which(headers == yval))
  color <- ifelse(is.numeric(color), color, which(headers == color))
  
  g <- ggplot(df, aes(x = df[,xval], y = df[,yval], color = factor(df[,color]), group = factor(df[,color])))
  if (errortype == "cl") {
    g <- g + stat_summary(fun.data = mean_cl_boot, alpha = alpha, size = point.size)
  } else {
    g <- g + stat_summary(fun = mean, geom = "point", alpha = alpha, size = point.size)
    if (errortype == "sd") {
      g <- g + stat_summary(fun.data = mean_sdl, geom = "errorbar", width = error.width, size = error.width * 4, alpha = alpha)
    } else if (errortype == "sem") {
      g <- g + stat_summary(fun.data = mean_se, geom = "errorbar", width = error.width, size = error.width * 4, alpha = alpha)
    }
  }
  if (connectLine == TRUE) {   # adds mean line
    g <- g + stat_summary(fun = mean, geom = "line", size = line.size, alpha = alpha)
  }
  # adds theme settings
  g <- g + guides(color = guide_legend(title = colnames(df)[color])) + 
    labs(x = colnames(df)[xval], y = colnames(df)[yval]) + theme_bw() +
    scale_color_manual(values = palette)
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}



# sd = standard deviation
# sem = standard error of the mean
#

ggEasy.barplot.stat<-function(df, xval = 1, yval = 2, color = NA, shape = NA, group = color, errortype = "sd", allpoints = T,
                              error.width = 1, point.size = 1, alpha = 0.8, 
                              palette = wes_palette(palette.wes_palette, length(unique(df[,color])), type = "continuous"),
                              palette.wes_palette = "Darjeeling1",
                              lineweight.bar = 0.5, lineweight.error = lineweight.bar, bartype = "dodge",
                              print = FALSE, outputName = "statBarPlot.pdf",print.width = 8, print.height = 6) {
  df <- as.data.frame(df)
  # flexible inputs
  headers <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(headers == xval))
  yval <- ifelse(is.numeric(yval), yval, which(headers == yval))
  color <- ifelse(is.numeric(color), color, which(headers == color))
  shape <- ifelse(is.numeric(shape), shape, which(headers == shape))
  
  # adds filler column for NA purposes
  if (is.na(color) || is.na(group) || is.na(shape)) {
    df$FillerColor <- "filler"
  }
  
  g <- ggplot(df,aes(x = factor(df[,xval]), y = as.numeric(df[,yval]))) + 
    labs(x = colnames(df)[xval], y = colnames(df)[yval]) + theme_bw()
  
  # adds color
  if (is.na(color)) {   # adds filler column for NA color
    ncolor <- 1
    g <- g + aes(fill = FillerColor) + 
      guides(fill = "none")
  } else {
    ncolor <- length(unique(df[,color]))
    g <- g + aes(fill = factor(df[,color])) + 
      guides(fill = guide_legend(title = colnames(df)[color]))
  }
  # adds group
  if (!is.na(group)) {
    g <- g + aes(group = factor(df[,group]))
  }
  # adds shape
  if (!is.na(shape)) {
    g <- g + aes(shape = factor(df[,shape]))
  }
  # adds bars
  g <- g + stat_summary(fun="mean",position=bartype,geom="bar", width = error.width, alpha = alpha, 
                        color= "black", size = lineweight.bar) + 
    scale_fill_manual(values = palette)
  
  if (bartype == "dodge") {
    # adds errorbars
    if (errortype == "sd") {
      g <- g + stat_summary(fun.data=mean_sdl, position=position_dodge(error.width), geom="errorbar", width = error.width/2, alpha = alpha, color = "black", size = lineweight.error)
    } else if (errortype == "sem") {
      g <- g + stat_summary(fun.data=mean_se, position=position_dodge(error.width), geom="errorbar", width = error.width/2, alpha = alpha, color = "black", size = lineweight.error)
    }
    # adds points
    if (allpoints == TRUE) {
      if (is.na(group)) {
        group <- xval
      }
      if (is.na(shape)) {
        g <- g + geom_point(aes(group = factor(df[,group])), alpha = alpha, color = "black", size = point.size, 
                            position=position_jitterdodge(dodge.width = error.width, jitter.width = error.width/2)) + 
          guides(shape = "none")
      } else {
        g <- g + geom_point(aes(shape = factor(df[,shape]), group = factor(df[,group])), alpha = alpha, color = "black", size = point.size, 
                            position=position_jitterdodge(dodge.width = error.width, jitter.width = error.width/2)) + 
          guides(shape = guide_legend(title = colnames(df)[shape]))
        if (length(unique(df[,shape])) > 6) {
          g <- g + scale_shape_manual(values=seq(0,length(unique(df[,shape]))))
        }
      }
    }
  }
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}
