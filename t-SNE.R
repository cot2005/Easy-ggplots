library(Rtsne)
library(ggplot2)
library(wesanderson)

ggEasy.tsne<-function(df, labelColumn = ncol(df), max_iter = 500, exaggeration_factor = 50, perplexity = 30,
                      learning = 200, subsample = NA, point.size = 2, alpha = 0.8,
                      palette =  wes_palette("Darjeeling1", numLabels, type = "continuous"),
                      outputName = "tsne.pdf", print = FALSE, print.height = 5, print.width = 6) {
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
  numLabels <- length(unique(labels))
  
  # using tsne
  #set.seed(1) # for reproducibility
  tsne <- Rtsne(df, dims = 2, perplexity=perplexity, verbose=F, max_iter=max_iter, exaggeration_factor=exaggeration_factor,
                eta=learning, check_duplicates = F)
  tsne.df <- data.frame(dim1 = tsne$Y[,1], dim2 = tsne$Y[,2], Label = as.factor(labels))
  g <- ggEasy.scatter(tsne.df, xval = 1, yval = 2, color = 3, shape = 3, point.size = point.size, 
                       alpha = alpha, palette = palette) + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  if (print == TRUE) {
    ggsave(outputName, plot = g, width = print.width, height = print.height)
  }
  return(g)
}
