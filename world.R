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
  world$xDim <- 20
  world$yDim <- 20
  # Epsilon for position comparison
  world$eps <- 1e-10
  # Interesting places
  label=c('Y', 'R', 'B', 'G')
  x <- c(0,0,12,19)
  y <- c(0,19,0,19)
  world$places=data.frame(x,y,label)
  # Walls
  world$walls <- Lines(list(
    Line(rbind(c(4,0) - world$halfSquareSide, c(4,8) - world$halfSquareSide)),
    Line(rbind(c(12,0) - world$halfSquareSide, c(12,8) - world$halfSquareSide)),
    Line(rbind(c(8,20) - world$halfSquareSide, c(8,12) - world$halfSquareSide)),
    Line(rbind(c(0,0) - world$halfSquareSide, c(0,20) - world$halfSquareSide)),
    Line(rbind(c(20,0) - world$halfSquareSide, c(20,20) - world$halfSquareSide)),
    Line(rbind(c(0,0) - world$halfSquareSide, c(20,0) - world$halfSquareSide)),
    Line(rbind(c(0,20) - world$halfSquareSide, c(20,20) - world$halfSquareSide))
  ),
  "walls")
  
  world
}
