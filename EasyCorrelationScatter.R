ggEasyCorPlot<-function(df, xval = 1, yval = 2, fill = 3, label = 4, facetGroup = NULL, cortest = "pearson", point.size = 2.5, width = 8, height = 8, outputfile = "facet_corplot.pdf") {
  cols <- colnames(df)
  #puts correlation result in upper left quadrant
  xlabcoord <- (max(df[,xval]) - min(df[,xval])) * 0.1 + min(df[,xval])
  ylabcoord <- (max(df[,yval]) - min(df[,yval])) * 0.95 + min(df[,yval])

  g <- ggplot(df, aes(x = df[,xval], y = df[,yval], label = df[,label]))
  g <- g + geom_point(aes(fill = df[,fill]), alpha = 0.8, size = point.size, shape = 21) + theme(legend.position = "none") + 
    theme_bw() + guides(fill = guide_legend(title=cols[fill])) + geom_text_repel(size = 3, box.padding = 0.5, max.overlaps = 100) + labs(x = cols[xval], y = cols[yval]) 
    #guides(fill = guide_legend(title=cols[fill])) #+ scale_y_continuous(limits = c(0,1.50),breaks = seq(0, 1.50, by = 0.25)) 
  # calculates correlations
  corrList <- c()
  if (is.null(facetGroup) == FALSE) {   # adds facet wrapper
    uniqFacets <- unique(df[,facetGroup])
    for (i in uniqFacets) {
      tempdf <- subset(df, df[,facetGroup] == i)
      corr <- cor.test(tempdf[,xval], tempdf[,yval], method = cortest)
      corrText <- paste(cortest, ", (r = ", format(corr$estimate, digits = 3), "; p = ", format(corr$p.value, digits = 3), ")",sep = "")
      corrList <- append(corrList, corrText)
    }
    labeldf <- data.frame(x = uniqFacets, label = corrList)
    colnames(labeldf)[1] <- cols[facetGroup]
    g <- g + facet_wrap(~get(cols[facetGroup])) + geom_text(x = xlabcoord, y = ylabcoord, aes(label = label), data = labeldf)
  } else {
    corr <- cor.test(df[,xval], df[,yval], method = cortest)
    annotation <- paste(cortest, ", (r = ", format(corr$estimate, digits = 3), "; p = ", format(corr$p.value, digits = 3), ")",sep = "")
    g <- g + annotate(geom = "text", x = xlabcoord, y = ylabcoord, label = annotation, size = 5, hjust = 0)
  }
  print(g)
  ggsave(outputfile, width = width, height = height)
}
