
library(umap)
library(ggplot2)
library(wesanderson)
library(ggrepel)
library(dplyr)

ggEasy.umap<-function(df, labelColumn = ncol(df), point.size = 2, alpha = 0.8, subsample = NA, 
                      palette = wes_palette("Darjeeling1", numLabels, type = "continuous"),
                      n_neighbors = 15, min_distance = 0.1, returnDF = FALSE, 
                      clusterLabel = FALSE, labelSize = 3, removeGrid = TRUE,
                      print = FALSE, outputName = "umap.pdf", print.width = 7, print.height = 6) {
  # subsets data
  if (is.numeric(subsample)) {
    subrows <- sample(1:nrow(df), subsample)
    df <- df[subrows,]
  }
  
  # flexible inputs
  headers <- colnames(df)
  labelColumn <- ifelse(is.numeric(labelColumn), labelColumn, which(headers == labelColumn))
  
  # separates labels and gets stats
  labels <- df[,labelColumn]
  df <- df[,-labelColumn]
  df <- as.matrix(as.data.frame(df))
  numLabels <- length(unique(labels))
  
  # performs clustering
  custom.config <- umap.defaults
  custom.config$n_neighbors <- n_neighbors
  custom.config$min_dist <- min_distance
  
  umapData <- umap(df, config = custom.config)
  
  # reattaches labels
  umap.df <- data.frame(UMAP1 = umapData$layout[,1], UMAP2 = umapData$layout[,2], 
                        Label = labels)
  combinedValues <- c(umap.df[,1], umap.df[,2])
  umap.max <- max(combinedValues)
  umap.max <- umap.max * ifelse(umap.max > 0, 1.1, 0.9)
  umap.min <- min(combinedValues) * 0.9
  umap.min <- umap.min * ifelse(umap.min < 0, 1.1, 0.9)
  
  #performs plotting
  g <- ggplot(umap.df, aes(UMAP1, UMAP2, color = Label)) + 
    geom_point(size = point.size, alpha = alpha) + 
    theme_bw() + scale_color_manual(values = palette)
  
  # removes grid
  if (removeGrid) {
    g <- g + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  }
  
  # adds cluster labels
  if (clusterLabel) {
    labelDF <- umap.df %>% 
      group_by(Label) %>% 
      slice_sample(n = 1)
    g <- g + geom_label_repel(data = labelDF, aes(x = UMAP1, y = UMAP2, label = Label), size = labelSize)
  }
  
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  if (returnDF) {
    return(list(g, umap.df))
  } else {
    return(g)
  }
}
