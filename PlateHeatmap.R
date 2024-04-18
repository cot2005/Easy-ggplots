# author: Colin Tang
# simple function to make heatmaps of plates from an input dataframe to QC results.
# each graphing variable can be specified by the column index or the string of the column name.
#
# returns a ggplot object that can be further modified
#

library(dplyr)
library(ggplot2)
library(wesanderson)
library(stringr)

ggEasy.plateheatmap<-function(df, wellRow = 2, valueRow = 3, platetype = 96,
                              point.weight = 0.25, wespalette = "Darjeeling1",
                              palette = wes_palette(wespalette, 3, type = "continuous"),
                              outputName = "platePlot.pdf", print = FALSE, print.height = 5, print.width = 7) {
  df <- as.data.frame(df)
  # flexible inputs
  header <- colnames(df)
  wellRow <- ifelse(is.numeric(wellRow), wellRow, which(header == wellRow))
  valueRow <- ifelse(is.numeric(valueRow), valueRow, which(header == valueRow))
  
  # trims df
  colnames(df)[c(wellRow, valueRow)] <- c("well", "Signal")
  df <- df %>% 
    mutate(row = str_extract(well, "[A-P]"), 
           column = as.numeric(str_extract(well, "[0-9]+")))  
  
  # makes plate index
  if (platetype == 96) {
    plateIndex <- unlist(lapply(LETTERS[1:8], function(x) {paste(x,1:12, sep = "")}))
    plate.breaks <- seq(1, 12, by = 1)
    plate.lims <- c(1,12)
  } else if (platetype == 384) {
    plateIndex <- unlist(lapply(LETTERS[1:16], function(x) {paste(x,1:24, sep = "")}))
    plate.breaks <- seq(1, 24, by = 1)
    plate.lims <- c(1,24)
  } else {
    print("Plate Type not supported")
  }
  
  allData <- data.frame(row = str_extract(plateIndex, "[A-P]"), 
                        column = as.numeric(str_extract(plateIndex, "[0-9]+")))  %>% 
    left_join(df, by = c("row", "column"))
  
  platePlot <- allData %>% 
    ggplot(aes(x = column, y = row, fill = Signal)) + geom_point(size = 7, shape = 21, stroke=point.weight) + 
    scale_y_discrete(limits = rev) + 
    scale_x_continuous(position = "top", breaks = plate.breaks, limits = plate.lims) + 
    scale_fill_gradientn(colors = palette) + 
    theme_bw() + theme(panel.grid.minor = element_blank(), 
                       axis.title.x = element_blank(), 
                       axis.title.y = element_blank())
  # saves graph
  if (print) {
    ggsave(outputName, plot = platePlot, width = print.width, height = print.height)
  }
  
  return(platePlot)
}
