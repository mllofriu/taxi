library('sp')
library('rgeos')

initWorld <- function(){
  # World object
  world <- list()
  class(world) <- "world"
  # Grid and robot size parameters
  world$halfSquareSide <- .5
  world$robotDiam <- world$halfSquareSide
  # World dimension
  world$xDim <- 10
  world$yDim <- 10
  # Epsilon for position comparison
  world$eps <- 1e-10
  # Interesting places
  label=c('Y', 'R', 'B', 'G')
  x <- c(0,0,6,9)
  y <- c(0,9,0,9)
  world$places=data.frame(x,y,label)
  # Walls
  world$walls <- Lines(list(
    Line(rbind(c(2,0) - world$halfSquareSide, c(2,4) - world$halfSquareSide)),
    Line(rbind(c(6,0) - world$halfSquareSide, c(6,4) - world$halfSquareSide)),
    Line(rbind(c(4,10) - world$halfSquareSide, c(4,6) - world$halfSquareSide)),
    Line(rbind(c(0,0) - world$halfSquareSide, c(0,10) - world$halfSquareSide)),
    Line(rbind(c(10,0) - world$halfSquareSide, c(10,10) - world$halfSquareSide)),
    Line(rbind(c(0,0) - world$halfSquareSide, c(10,0) - world$halfSquareSide)),
    Line(rbind(c(0,10) - world$halfSquareSide, c(10,10) - world$halfSquareSide))
  ),
  "walls")
  
  world
}
