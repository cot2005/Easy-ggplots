# Easy-ggplots
Functions for making simple ggplots from 3 and 4 variable datasets. Each variable input is declared by the column integer containing the desired data to be plotted for each plotting feature. The grouping is determined by the fill parameter. The facet parameter is provided for the boxplot functions as a boolean to separate the groups into individual plots. 

*Fill and grouping is limited to 5.

Example:
```
inputFile = file name for input data file
xval, yval, shape, fill = column number in the input data containing each variable to be plotted.
sep = separator for the input datafile
outputName = output graph name "myPlot.pdf"
height = graph height
width = graph width
facet = boolean to facet grid the graphs


ggEasy.boxplot4("testData.csv", xval = 3, yval = 6, shape = 2, fill = 4, facet = FALSE)
```
