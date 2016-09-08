#' @export
manhattanD = function(srcX, srcY, destX, destY) {
  return (abs(srcX - destX) + abs(srcY - destY))
}

#' @export
findPackage = function(car, packages) {
  min = 20
  pack = 1
  curr = 1
  for(row in 1:nrow(packages)) {
    if (packages[curr, 5] == 0) {
      dist = manhattanD(car$x, car$y, packages[curr, 1], packages[curr, 2]) 
      if (dist < min) {
        pack = curr
        min = dist
      }
    }
    curr = curr + 1
  }
  
  return (pack)
}

#findPath = function()

cost = function(roads, car) {
  
}

#' @export
aStar <- function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }
  
  print(paste("Closest: ", findPackage(car, packages)))
  
  #print(packages)
  
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}