source('graphics.R')
source('movement.R')
source('taxic.R')

robot <- data.frame(x=0,y=4,theta=pi/2)

world.halfSquareSide <- .5
world.robotDiam <- halfSquareSide

world.xDim <- 5
world.yDim <- 5

# Epsilon for position comparison
world.eps <- 1e-10

x=c(0, 0, 3, 4)
y=c(0, 4, 0, 4)
label=c('Y', 'R', 'B', 'G')
world.places=data.frame(x,y,label)

world.walls <- Lines(list(
            Line(rbind(c(1,0) - halfSquareSide, c(1,2) - halfSquareSide)),
            Line(rbind(c(2,5) - halfSquareSide, c(2,3) - halfSquareSide)),
            Line(rbind(c(0,0) - halfSquareSide, c(0,5) - halfSquareSide)),
            Line(rbind(c(5,0) - halfSquareSide, c(5,5) - halfSquareSide)),
            Line(rbind(c(0,0) - halfSquareSide, c(5,0) - halfSquareSide)),
            Line(rbind(c(0,5) - halfSquareSide, c(5,5) - halfSquareSide))
            ),
            "walls")

draw(robot,world)

goal <- data.frame(x=4, y=4)
while (!(dist(robot,goal)< eps)){
  dev.flush()
  
  posActions <- possibleActions(robot, world)
#   print(posActions)
  action <- selectAction(robot,posActions, world, goal)
#   print(action)
  robot <- move(robot, action)
  #draw(robot,walls) 
  print(draw(robot, world),newpage=F)
#   print(robot)
#   Sys.sleep(.1)
}

draw(robot,world)