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
  
  return (c(packages[pack, 1], packages[pack, 2]))
}

#findPath = function()

findNodes = function(x, y) {
  dir = c(right = T, left = T, up = T, down = T)
  if (x == 1) { dir$left = F }
  if (x == 10) { dir$right = F }
  if (y == 1) { dir$down = F }
  if (y == 10) { dir$up = F }
  return (dir)
}

nodeVal = function(roads, srcX, srxY, destX, destY) {
  cost = 0
  heuristic = manhattanD(, , , )
  
  return c(cost, heuristic)
}

updateMatrix = function(roads, mat, frontier, src, curr, dest, path) {
  node = nodeVal(roads, curr[1], curr[2], dest[1], dest[2])
  entry = (mat[curr[1], curr[2]])
  entry$h = node$heuristic
  #entry$g = node$cost
  entry$g = path + 1
  entry$arrow = c(src[1], src[2])
  
  insertFrontier(frontier, entry$g + entry$h, srcX, srcY)
}

insertFrontier = function(frontier, f, x, y) {
  i = 1
  insert = F
  for(e in frontier) {
    if (f < e$f) {
      append(c(f, x, y), frontier, i)
      insert = T
    }
    i = i + 1
  }
  
  if (!insert) {
    append(c(f, x, y), frontier)
  }
}

traverseArrow = function(mat, dest) {
  prev = curr = mat[dest[1], dest[2]]
  while (T) {
    arr = curr$arrow
    if (arr$x == 0) {
      return (prev)
    }
    prev = curr
    curr = mat[arr$x, arr$y]
  }
  return (c())
}

aStar = function(roads, car, x, y) {
  entry <- c(g <- 0, h <- 0, closed <- F, arrow <- c(0, 0))
  mat = matrix(c(rep(entry, 100)), nrow <- 10, ncol <- 10)
  frontier <- c() #c(f, x, y)
  
  path <- 0
  currX = car$x
  currY = car$y
  
  while (currX != x & currY != y) {
    dir = findNodes(currX, currY)
    
    if (dir$right) {updateMatrix(roads, mat, frontier, c(currX, currY), c(currX+1, currY), c(x, y), path) }
    if (dir$left) {updateMatrix(roads, mat, frontier, c(currX, currY), c(currX-1, currY), c(x, y), path) }
    if (dir$up) {updateMatrix(roads, mat, frontier, c(currX, currY), c(currX, currY+1), c(x, y), path) }
    if (dir$down) {updateMatrix(roads, mat, frontier, c(currX, currY), c(currX, currY-1), c(x, y), path) }
    
    new = frontier[1]
    currX = new$x
    currY = new$y
    frontier[-1]
    
    path <- path + 1
  }
  
  nextNode = traverseArrow(mat, c(x, y))
  if (nextNode[1] > car$x) { return 6 }
  if (nextNode[1] < car$x) { return 4 }
  if (nextNode[2] > car$y) { return 8 }
  if (nextNode[2] < car$y) { return 2 }
}

#' @export
funcerino <- function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }
  
  pack = findPackage(car, packages)
  car$nextMove = aStar(roads, car, pack[1], pack[2])
  
  #print(paste("Closest: ", findPackage(car, packages)))
  #print(packages)
  
  #car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  #if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}