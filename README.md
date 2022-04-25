# Easy-ggplots
Functions for making simple ggplots from 2-4 variable datasets. Each variable input is declared by the column integer containing the desired data to be plotted for each plotting feature. The grouping is determined by the fill parameter. The facet parameter is provided for the boxplot functions as a boolean to separate the groups into individual plots. Shape and Fill variables are defaulted to NULL to allow for 2 variable boxplots.

*Fill/grouping is limited to 5.

Example:
```
inputFile = file name for input data file
xval, yval, shape, fill = column number in the input data containing each variable to be plotted.
angle.X = boolean for 45 degree angle x axis labels
box.width = width of boxes in box plot (default 1)
point.size = size of points
sep = separator for the input datafile (default ",")
outputName = output graph name (default "boxPlot.pdf")
height = graph height
width = graph width
facet = boolean to facet grid the graphs

ggEasy.boxplot("testData.csv", xval = 3, yval = 6, shape = 2, fill = 4, facet = FALSE)
```
