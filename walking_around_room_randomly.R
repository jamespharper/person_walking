# Title: Analyze Categorical Data, Two Way
# Desription: Analyze a categorical dataset using frequency tables and mosaic
#             plots with chi squared, p-values and Cramer's v to test for
#             statistical significance
# Author: James Harper, PE, ENV SP
# Date Created: February 4, 2018
# -----------------------------------------------------------------------------

# Define area, starting loc, number of steps to take, and directions
area = c(10, 10)
start = c(0, 0)
steps = 100
directions = c("N", "S", "E", "W", "NW", "NE", "SW", "SE")

# Initialize loc and path variables
loc = data.frame(x = start[1], y = start[2])

for (step in 1:steps) {
  direction = sample(directions, 1, replace = TRUE); direction
  if (direction == "N") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)], 
                                y = loc$y[length(loc$y)] + 1))
  } else if (direction == "S") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)], 
                                y = loc$y[length(loc$y)] - 1))
  } else if (direction == "E") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)] + 1, 
                                y = loc$y[length(loc$y)]))
  } else if (direction == "W") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)] - 1, 
                                y = loc$y[length(loc$y)]))
  } else if (direction == "NW") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)] - 1, 
                                y = loc$y[length(loc$y)] + 1))
  } else if (direction == "NE") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)] + 1, 
                                y = loc$y[length(loc$y)] + 1))
  } else if (direction == "SW") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)] - 1, 
                                y = loc$y[length(loc$y)] - 1))
  } else if (direction == "SE") {
    loc = rbind(loc, data.frame(x = loc$x[length(loc$x)] + 1, 
                                y = loc$y[length(loc$y)] - 1))
  }
}

# Plot path
plot(loc$x, loc$y, type = "l")
