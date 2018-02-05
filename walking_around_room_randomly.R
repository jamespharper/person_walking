# Title: Analyze Categorical Data, Two Way
# Desription: Analyze a categorical dataset using frequency tables and mosaic
#             plots with chi squared, p-values and Cramer's v to test for
#             statistical significance
# Author: James Harper, PE, ENV SP
# Date Created: February 4, 2018
# -----------------------------------------------------------------------------


# Define area, starting position, number of steps to take, and directions
area = c(10, 10)
start = c(0, 0)
steps = 10
directions = c("N", "S", "E", "W")

# Initialize position and path variables
position = start
path = data.frame(x = start[1], y = start[2])

for (step in 1:steps) {
  
  # Take a step
  # direction = sample(directions, 1, replace = TRUE); direction
  direction = "N"
  if (direction == "N") {
    position = position + c(0, 1)
    rbind(path, position)
  }
  
}

# Plot path
plot(path[[]][1], path[[]][2])
