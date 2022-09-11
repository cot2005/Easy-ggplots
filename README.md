# Easy-ggplots
Functions for making simple ggplot objects and plots from multi-variable datasets. Each variable input is declared by the column integer or column name string containing the desired data to be plotted. The grouping is typically determined by the color parameter but can be modified with the group input. Shape and color variables can be NA'd to make simpler plots.

Example:
```
df = data frame
xval, yval, shape, fill, color, group = column number in the input data containing each variable to be plotted.
shape.selection = integer of custom shape to use if shape is not a variable (default = 16).
color.selection = string of custom color to use if color is not a variable.
angle.X = boolean for 45 degree angle x axis labels.
box.width = width of boxes in box plot (default 1).
point.size = size of points.
alpha = point transparency
outputName = output graph name with file extension (default "boxPlot.pdf")
print = boolean for exporting graph as a file
print.height = graph height
print.width = graph width
facet = boolean to facet grid the graphs
palette = palette of colors to use in graphs

ggEasy.boxplot(iris, xval = 3, yval = 6, shape = 2, fill = 4, facet = FALSE)
```
