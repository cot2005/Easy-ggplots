library(umap)
library(ggplot2)
library(wesanderson)

ggEasy.umap<-function(df, labelColumn = ncol(df), point.size = 2, alpha = 0.8, subsample = NA, 
         palette = wes_palette("Darjeeling1", numLabels, type = "continuous"),
         n_neighbors = 15, min_distance = 0.1,
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
  df <- matrix(as.numeric(df[,-labelColumn]), 
               nrow = nrow(df), ncol = ncol(df), byrow = TRUE)
  numLabels <- length(unique(labels))
  shapeCol <- ifelse(numLabels > 3, NA, 3)

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
  g <- ggEasy.scatter(umap.df, xval = 1, yval = 2, color = 3, shape = shapeCol, point.size = point.size, 
                      alpha = alpha, palette = palette) + coord_cartesian(xlim = c(umap.min,umap.max), ylim = c(umap.min,umap.max)) + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}

