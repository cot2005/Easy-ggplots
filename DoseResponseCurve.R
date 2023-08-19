# author: Colin Tang
# simple function to make dose response curves from an input dataframe.
# each graphing variable can be specified by the column index or the string of the column name.
#
# will perform an 3 parameter fitting if drc.upper is set to a number. will perform 4 parameter fitting if drc.upper is set to NA (default)
# options for error.geom = pointrange (default), linerange, errorbar, NA

library(drc)
library(ggplot2)
library(dplyr)
library(wesanderson)

ggEasy.drc<-function(df, xval = 1, yval = 2, color = NA, group = NA, alpha = 0.8, linewidth = 0.5,
                     error.geom = "pointrange", error.width = 0.1, error.size = 0.5, error.func="mean_cl_boot",
                     drc.upper = NA, wespalette = "Darjeeling1",
                     palette = wes_palette(wespalette, length(unique(df[,color])), type = "continuous"),
                     outputName = "drcPlot.pdf", print = FALSE, print.height = 5, print.width = 6) {
  df <- as.data.frame(df)
  # flexible inputs
  header <- colnames(df)
  xval <- ifelse(is.numeric(xval), xval, which(header == xval))
  yval <- ifelse(is.numeric(yval), yval, which(header == yval))
  color <- ifelse(is.numeric(color), color, which(header == color))
  group <- ifelse(is.numeric(group), group, which(header == group))
  
  # adds filler column for NA purposes
  if (is.na(color) || is.na(group)) {
    df$FillerColor <- "filler"
  }
  
  g <- ggplot(df, aes(x = as.numeric(df[,xval]), y = as.numeric(df[,yval]))) + 
    labs(x = header[xval], y = header[yval])
  
  # adds color
  if (is.na(color)) {   # adds filler column for NA color
    color <- which(colnames(df) == "FillerColor")
    g <- g + aes(color = FillerColor) + 
      guides(color = "none")
  } else {
    g <- g + aes(color = factor(df[,color])) + 
      guides(color = guide_legend(title = header[color]))
  }
  # adds group
  if (!is.na(group)) {
    g <- g + aes(group = factor(df[,group]))
  }
    
  # adds drc
  if (is.na(drc.upper)) {   # 4 parameter
    g <- g + geom_smooth(method = "drm",
                         method.args = list(fct = LL.4(fixed = c(NA, NA, NA, NA), names=c("H","E0","top","EC50"))), 
                         se = FALSE, linewidth = linewidth, alpha = alpha)
  } else {   # 3 parameter
    g <- g + geom_smooth(method = "drm",
                         method.args = list(fct = LL.4(fixed = c(NA, NA, drc.upper, NA), names=c("H","E0","top","EC50"))), 
                         se = FALSE, linewidth = linewidth, alpha = alpha)
  }
  
  # adds error bars
  if (error.geom == "errorbar") {
    g <- g + stat_summary(geom = error.geom, width = error.width, size = error.size, fun.data = error.func, alpha = alpha)
  } else if (!is.na(error.geom)) {
    g <- g + stat_summary(geom = error.geom, size = error.size, fun.data = error.func, alpha = alpha)
  }
  
  # adds theme aesthetics
  g <- g + theme_bw() + scale_color_manual(values = palette)
  
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}
