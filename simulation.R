source('graphics.R')
source('movement.R')
source('taxic.R')

# Robot position and orientation
robot <- data.frame(x=4,y=0,theta=pi/2)

# World object
# Grid and robot size parameters
world.halfSquareSide <- .5
world.robotDiam <- halfSquareSide
# World dimension
world.xDim <- 5
world.yDim <- 5
# Epsilon for position comparison
world.eps <- 1e-10
# Interesting places
# x=c(0, 0, 3, 4)
# y=c(0, 4, 0, 4)
# label=c('Y', 'R', 'B', 'G')
x <- c(0)
y <- c(4)
label <- c('G')
world.places=data.frame(x,y,label)
# Walls
world.walls <- Lines(list(
            Line(rbind(c(1,0) - halfSquareSide, c(1,2) - halfSquareSide)),
            Line(rbind(c(3,0) - halfSquareSide, c(3,2) - halfSquareSide)),
            Line(rbind(c(2,5) - halfSquareSide, c(2,3) - halfSquareSide)),
            Line(rbind(c(0,0) - halfSquareSide, c(0,5) - halfSquareSide)),
            Line(rbind(c(5,0) - halfSquareSide, c(5,5) - halfSquareSide)),
            Line(rbind(c(0,0) - halfSquareSide, c(5,0) - halfSquareSide)),
            Line(rbind(c(0,5) - halfSquareSide, c(5,5) - halfSquareSide))
            ),
            "walls")

draw(robot,world)

# Set a goal - TODO: pick from places
goal <- data.frame(x=0, y=4)
# Plot opt.
saveBasePlot(world)
quartz("Maze", 5, 5, antialias = T)
# While the robot has not reach the goal
while (!(dist(robot,goal)< eps)){
  # Get affordances
  posActions <- possibleActions(robot, world)
  # Get taxic values
  tVals <- taxicVals(robot,posActions, world, goal)
  # Select maximum value action
  action <- posActions[match(max(tVals), tVals)]
  # Move the robot according to picked action
  robot <- move(robot, action)
  # Draw the world
  print(draw(robot, world),newpage=F)
  print(robot)
}