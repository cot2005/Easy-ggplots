# generic xy scatter and correlation plot function
library(ggplot2)

ggcorPlot<-function(df, x = 1, y = 2, cortest = "pearson", width = 5, height = 5, outputfile = "corplot.pdf") {
  df <- df[,c(x,y)]
  cols <- colnames(df)
  #puts label in upper left quadrant
  xlabcoord <- (max(df[,1]) - min(df[,1])) * 0.15 + min(df[,1])
  ylabcoord <- (max(df[,2]) - min(df[,2])) * 0.95 + min(df[,2])
  corr <- format(cor.test(df[,1], df[,2], method = cortest)$p.value, digits = 3)
  annotation <- paste(cortest, ", p = ", corr, sep = "")
  h <- ggplot(df, aes(x = df[,1], y = df[,2]))
  h + geom_point(shape = 16, alpha = 0.7, color = "black", size = 3) +
    theme_bw() + labs(x = cols[1], y = cols[2]) +  annotate(geom = "text", x = xlabcoord, y = ylabcoord, label = annotation, size = 5, hjust = 0) + 
    theme(text = element_text(size = 12))
  ggsave(outputfile, width = width, height = height)
}
