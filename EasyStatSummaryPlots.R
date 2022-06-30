# 
# errortype: 
# cl = confidence limits
# sd = standard deviation
# sem = standard error of the mean
#

library(ggplot2)
library(wesanderson)

ggEasy.scatter.stat<-function(df, xval = 1, yval = 2, color = 3, errortype = "sd", connectLine = FALSE, 
                              error.width = 0.15, line.size = 0.5, point.size = 1,alpha = 0.8, width = 8, height = 6,
                              wespalette = "Darjeeling1") {
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
    scale_color_manual(values = wes_palette(wespalette, length(unique(df[,color], type = "continuous"))))
  print(g)
  ggsave("statScatterPlot.pdf", plot = g, width = width, height = height)
}


# sd = standard deviation
# sem = standard error of the mean
ggEasy.barplot.stat<-function(df, xval = 1, yval = 2, color = 3, shape = NULL, group = color, errortype = "sd", allpoints = T,
                              error.width = 0.15, point.size = 1, alpha = 0.8, width = 8, height = 6,
                              wespalette = "Darjeeling1") {
  g <- ggplot(df,aes(x = factor(df[,xval]), y = df[,yval], color = factor(df[,color]), fill = factor(df[,color]), group = factor(df[,group]))) + 
    stat_summary(fun="mean",position="dodge",geom="bar", width = error.width, alpha = alpha)
  if (errortype == "sd") {
    g <- g + stat_summary(fun.data=mean_sdl, position=position_dodge(error.width), geom="errorbar", width = error.width/2, alpha = alpha, color = "black")
  } else if (errortype == "sem") {
    g <- g + stat_summary(fun.data=mean_se, position=position_dodge(error.width), geom="errorbar", width = error.width/2, alpha = alpha, color = "black")
  }
  if (allpoints == TRUE) {
    if (is.null(shape)) {
      g <- g + geom_jitter(alpha = alpha, width = error.width/4, size = point.size)
    } else {
      g <- g + geom_point(aes(group = df[,color], shape = factor(df[,shape])), alpha = alpha, color = "black", size = point.size, 
                          position=position_jitterdodge(dodge.width = error.width, jitter.width = error.width/2)) + 
        guides(shape = guide_legend(title = colnames(df)[shape]))
    }
  g <- g + guides(fill = guide_legend(title = colnames(df)[color]), color = "none") + 
    labs(x = colnames(df)[xval], y = colnames(df)[yval]) + theme_bw() +
    scale_fill_manual(values = wes_palette(wespalette, length(unique(df[,color], type = "continuous"))))
  print(g)
  ggsave("statBarPlot.pdf", plot = g, width = width, height = height)
}
