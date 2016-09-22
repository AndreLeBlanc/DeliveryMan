# Calculates the Manhattan distance between two points.
manhattanD = function(srcX, srcY, destX, destY) {
  return (abs(srcX - destX) + abs(srcY - destY))
}

# Calculates the Euclidean distance between two points.
euclideanD = function(srcX, srcY, destX, destY) {
  a = abs(srcX - destX) ^ 2
  b = abs(srcY - destY) ^ 2
  c = sqrt(a + b)
  return (c)
}

# Find the closest package (smallest Manhattan distance).
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
  
  return (list(packages[pack, 1], packages[pack, 2]))
}

# Determine how many edges a node has in a dim x dim grid.
findNodes = function(x, y, dim) {
  dir = list(right = T, left = T, up = T, down = T)
  if (x == 1) { dir$left = F }
  if (x == dim) { dir$right = F }
  if (y == 1) { dir$down = F }
  if (y == dim) { dir$up = F }
  return (dir)
}

# Calculate the f(n) value for A* of a node.
nodeVal = function(roads, src, srcX, srcY, destX, destY) {
  if (src[[1]] < srcX) {
    cost = roads$hroads[srcY, src[[1]]]
  }
  else if (srcX < src[[1]]) {
    cost = roads$hroads[srcY, srcX]
  }
  else if (srcY < src[[2]]) {
    cost = roads$vroads[srcY, srcX]
  }
  else {
    cost = roads$vroads[src[[2]],srcX] 
  }
  heuristic = manhattanD(srcX, srcY, destX, destY)

  return (list(g = cost, h = heuristic))
}

# Update the frontier, visited set and matrix for graph search in A* with the f(n) value of a node.
updateMatrix = function(roads, mat, frontier, src, curr, dest, path) {
  if (!is.null(mat[[curr[[1]], curr[[2]]]])) {
    return (list(mat, frontier))
  }
  
  parentG = 0
  srcNode = mat[[src[[1]], src[[2]]]]
  if(!is.null(srcNode)) {
    parentG = srcNode$g
  }
  
  node = nodeVal(roads, src, curr[[1]], curr[[2]], dest[[1]], dest[[2]])
  #entry = list(g = path+node$g, h = node$h, closed = F, arrow = list(src[[1]], src[[2]]))
  entry = list(g = parentG+node$g, h = node$h, closed = F, arrow = list(src[[1]], src[[2]]))
  mat[curr[[1]], curr[[2]]] = list(entry)
  
  if (is.null(mat[[src[[1]], src[[2]]]])) {
    srcEntry = list(g = path, h = node$h, closed = T, arrow = list(0, 0))
    mat[src[[1]], src[[2]]] = list(srcEntry)
  }
  
  frontier = insertFrontier(frontier, entry$g + entry$h, curr[[1]], curr[[2]])
  return (list(mat, frontier))
}

# Insert a f(n) value of a node into the sorted frontier list.
insertFrontier = function(frontier, f, x, y) {
  i = 1
  insert = F
  len = length(frontier)
  
  while (i <= len) {
    e = frontier[[i]]
    if (f < e[[1]]) {
      frontier = append(frontier, list(list(f, x, y)), after = (i - 1))
      insert = T
      break
    }
    i = i + 1
  }

  if (!insert) {
    frontier = append(frontier, list(list(f, x, y)))
  }
  
  return (frontier)
}

# Traverse the graph from the destination to the start to determine the path the A* algorithm found.
traverseArrow = function(mat, dest) {
  prev = mat[[dest[[1]], dest[[2]]]]
  curr = prev
  arr = curr$arrow
  
  if (arr[[1]] == 0) {
    return (prev$arrow)
  }

  depth = 1  
  while (T) {
    temp = mat[[arr[[1]], arr[[2]]]]
    arr = temp$arrow
    if (arr[[1]] == 0) {
      if (depth == 1) {
        return (dest)
      }
      return (prev$arrow)
    }
    
    prev = curr
    curr = temp
    depth = depth + 1
  }
  
  return (NULL)
}

# Chose the direction for the car to move through A*.
makeChoice = function(car, mat, x, y) {
  nextNode = traverseArrow(mat, list(x, y))
  
  #node = mat[[nextNode[[1]], nextNode[[2]]]]
  #rand = runif(1)
  #if (node$g > 3 & rand[[1]] > 0.3) { return (5) }
  
  if (nextNode[[1]] > car$x) { return (6) }
  if (nextNode[[1]] < car$x) { return (4) }
  if (nextNode[[2]] > car$y) { return (8) }
  if (nextNode[[2]] < car$y) { return (2) }
}

# Perform the A* algorithm.
astar = function(roads, car, x, y, dim) {
  # Create road map and frontier nodes.
  len = length(roads$hroads)
  mat <- matrix((rep(list(), dim*dim)), nrow = dim, ncol = dim)
  frontier <- list()
  
  path <- 0
  currX = car$x
  currY = car$y
  
  # If we are at the destination already, then stay.
  if (currX == x & currY == y) { return (5) }
  
  # Otherwise execute the A star algorithm.
  while (currX != x | currY != y) {
    dir = findNodes(currX, currY, dim)
    
    # Updates matrix and frontier for every accesible node.
    if (dir$right) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX+1, currY), c(x, y), path)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    if (dir$left) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX-1, currY), c(x, y), path)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    if (dir$up) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX, currY+1), c(x, y), path)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    if (dir$down) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX, currY-1), c(x, y), path)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    
    # Get and remove fittest node in the frontier and update current nodes.
    new = frontier[[1]]
    currX = new[[2]]
    currY = new[[3]]
    frontier = frontier[-1]
    path = path + 1
  }

  # Traverse the road map from the destination to the source, extracting the first move.
  nextNode = makeChoice(car, mat, x, y)
  return (nextNode)
}

# Goes towards a destination with a package that is already picked up.
goToDest = function(car, roads, packages, dim) {
  car$nextMove = astar(roads, car, packages[car$load,3], packages[car$load,4], dim)
  return (car) 
}

# Finds a package and moves towards it.
goToPack = function(car, roads, packages, dim) {
  pack = findPackage(car, packages)
  car$nextMove = astar(roads, car, pack[[1]], pack[[2]], dim)
  return (car)
}

# Move the delivery man's car.
moveCar <- function(roads, car, packages, dim) {
  # If the car already has a package it goes and delivers it.
  if (car$load>0) {
    car = goToDest(car, roads, packages, dim)
  }
  # If the car doesn't have a package it goes and finds the nearest one. 
  else{
    car = goToPack(car, roads, packages, dim)
  }
  
  return (car)
}

# Run the Delivery Man game n times.
runNtimes <- function(n, fname="test.dat") {
  init = n
  i = 1
  sum = 0
  runs = list()
  
  print("STARTING")
  while (n > 0) {
    steps = runDeliveryMan(moveCar, 10, 2000, T, 0, 5)
    print(paste("RUN COMPLETE: ", i))
    
    n = n - 1
    i = i + 1
    runs = append(runs, steps)
    sum = sum + steps
  }
  print("DONE!")
  print(paste("Average no. steps = ", sum / init))
  lapply("vals", write, fname, append=TRUE, ncolumns=1000)
  lapply(runs, write, fname, append=TRUE, ncolumns=1000)
}

# Plot a histogram from data written to a file in runNtimes.
plotHist <- function(fname) {
  data <- read.csv(file=fname, sep=",", head=TRUE)
  hist(data$vals)
  hist(data$vals, main="Distribution of runs", xlab="Number of turns")
}

# Plot a boxgraph from data written to a file in runNtimes.
plotBox <- function(fname1, fname2=NULL, xlab=NULL) {
  data1 <- read.csv(file=fname1, sep=",", head=TRUE)
  if (is.null(fname2)) {
    boxplot(data1$vals, main='Distribution of runs', ylab='Number of turns')
    if (!is.null(names)) {
      boxplot(data1$vals, names=xlab, main='Distribution of runs', ylab='Number of turns')
    }
    else {
      boxplot(data1$vals, main='Distribution of runs', ylab='Number of turns')
    }
  }
  else {
    if (!is.null(names)) {
      data2 <- read.csv(file=fname2, sep=",", head=TRUE)
      boxplot(data1$vals, data2$vals, names=xlab, main='Distribution of runs', ylab='Number of turns')
    }
    else {
      data2 <- read.csv(file=fname2, sep=",", head=TRUE)
      boxplot(data1$vals, data2$vals, main='Distribution of runs', ylab='Number of turns')
    }
  }
}

# Calculate the standard deviation from data written to a file in runNtimes.
stdDeviation <- function(fname) {
  data <- read.csv(file=fname, sep=",", head=TRUE)
  print(sd(data$vals))
}

# Calculate the average from data written to a file in runNtimes.
average <- function(fname) {
  data <- read.csv(file=fname, sep=",", head=TRUE)
  print(mean(data$vals))
}