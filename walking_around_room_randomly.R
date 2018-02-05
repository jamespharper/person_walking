# Title: Analyze Categorical Data, Two Way
# Desription: Analyze a categorical dataset using frequency tables and mosaic
#             plots with chi squared, p-values and Cramer's v to test for
#             statistical significance
# Author: James Harper, PE, ENV SP
# Date Created: February 4, 2018
# -----------------------------------------------------------------------------

# Clear environment and console
rm(list = ls())
cat("\014")

# Define functions
ChangeLoc = function(direction) {
  if (direction == "N") {
    return(c(0, 1))
  } else if (direction == "S") {
    return(c(0, -1))
  } else if (direction == "E") {
    return(c(1, 0))
  } else if (direction == "W") {
    return(c(-1, 0))
  } else if (direction == "NW") {
    return(c(-1, 1))
  } else if (direction == "NE") {
    return(c(1, 1))
  } else if (direction == "SW") {
    return(c(-1, -1))
  } else if (direction == "SE") {
    return(c(1, -1))
  }
}

# Define area, starting loc, number of steps to take, and directions
area = data.frame(x.min = -10, x.max = 10, y.min = -10, y.max = 10)
start = c(0, 0)
steps = 100
directions = c("N", "S", "E", "W", "NW", "NE", "SW", "SE")

# Initialize loc and path variables
loc = data.frame(x = start[1], y = start[2])

for (step in 1:steps) {
  # print(paste("step =", step))
  check.boundary = 0
  while (check.boundary == 0) {
    direction = sample(directions, 1, replace = TRUE)
    change.loc = ChangeLoc(direction)
    # print(paste("change.loc =", change.loc))
    if (loc$x[length(loc$x)] + change.loc[1] >= area$x.min &&
        loc$x[length(loc$x)] + change.loc[1] <= area$x.max &&
        loc$y[length(loc$y)] + change.loc[2] >= area$y.min &&
        loc$y[length(loc$y)] + change.loc[2] <= area$y.max) {
      check.boundary = 1
    }
  }
  loc = rbind(loc, data.frame(x = loc$x[length(loc$x)] + change.loc[1], 
                              y = loc$y[length(loc$y)] + change.loc[2]))
}

# Plot path
plot(loc$x, loc$y, type = "l", 
     xlim = c(area$x.min, area$x.max),
     ylim = c(area$y.min, area$y.max))
