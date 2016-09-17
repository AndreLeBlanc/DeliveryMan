#' @export
manhattanD = function(srcX, srcY, destX, destY) {
  return (abs(srcX - destX) + abs(srcY - destY))
}

#' @export
euclideanD = function(srcX, srcY, destX, destY) {
  a = abs(srcX - destX) ^ 2
  b = abs(srcY - destY) ^ 2
  c = sqrt(a + b)
  return (c)
}

#' @export
findPackage = function(car, packages, man) {
  min = 20
  pack = 1
  curr = 1
  for(row in 1:nrow(packages)) {
    if (packages[curr, 5] == 0) {
      if (man) {
        dist = manhattanD(car$x, car$y, packages[curr, 1], packages[curr, 2])
      }
      else {
        dist = euclideanD(car$x, car$y, packages[curr, 1], packages[curr, 2])
      }
      if (dist < min) {
        pack = curr
        min = dist
      }
    }
    curr = curr + 1
  }
  
  return (list(packages[pack, 1], packages[pack, 2]))
}

findNodes = function(x, y, dim) {
  dir = list(right = T, left = T, up = T, down = T)
  if (x == 1) { dir$left = F }
  if (x == dim) { dir$right = F }
  if (y == 1) { dir$down = F }
  if (y == dim) { dir$up = F }
  return (dir)
}

nodeVal = function(roads, src, srcX, srcY, destX, destY, man) {
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
  if (man) {
    heuristic = manhattanD(srcX, srcY, destX, destY)
  }
  else {
    heuristic = euclideanD(srcX, srcY, destX, destY)
  }
  return (list(g = cost, h = heuristic))
}

updateMatrix = function(roads, mat, frontier, src, curr, dest, path, man) {
  if (!is.null(mat[[curr[[1]], curr[[2]]]])) {
    return (list(mat, frontier))
  }
  
  node = nodeVal(roads, src, curr[[1]], curr[[2]], dest[[1]], dest[[2]], man)
  entry = list(g = path+node$g, h = node$h, closed = F, arrow = list(src[[1]], src[[2]]))
  mat[curr[[1]], curr[[2]]] = list(entry)
  
  if (is.null(mat[[src[[1]], src[[2]]]])) {
    srcEntry = list(g = path, h = node$h, closed = T, arrow = list(0, 0))
    mat[src[[1]], src[[2]]] = list(srcEntry)
  }
  
  frontier = insertFrontier(frontier, entry$g + entry$h, curr[[1]], curr[[2]])
  return (list(mat, frontier))
}

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

makeChoice = function(car, mat, x, y) {
  nextNode = traverseArrow(mat, list(x, y))
  #if (nextNode[[1]] > car$x && nextNode[[1]] > x) { return (5) }
  if (nextNode[[1]] > car$x) { return (6) }
  #if (nextNode[[1]] < car$x && nextNode[[1]] < x) { return (5) }
  if (nextNode[[1]] < car$x) { return (4) }
  #if (nextNode[[2]] > car$y && nextNode[[2]] > y) { return (5) }
  if (nextNode[[2]] > car$y) { return (8) }
  #if (nextNode[[2]] < car$y && nextNode[[2]] < y) { return (5) }
  if (nextNode[[2]] < car$y) { return (2) }
}

aStar = function(roads, car, x, y, dim, man) {
  # Create road map and frontier nodes
  len = length(roads$hroads)
  mat <- matrix((rep(list(), dim*dim)), nrow = dim, ncol = dim)
  frontier <- list()
  
  path <- 0
  currX = car$x
  currY = car$y
  
  # If we are at the destination already, then stay
  if (currX == x & currY == y) { return (5) }
  
  # Otherwise execute the A star algorithm.
  while (currX != x | currY != y) {
    dir = findNodes(currX, currY, dim)
    
    # Updates matrix and frontier for every accesible node
    if (dir$right) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX+1, currY), c(x, y), path, man)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    if (dir$left) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX-1, currY), c(x, y), path, man)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    if (dir$up) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX, currY+1), c(x, y), path, man)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    if (dir$down) {
      nodes = updateMatrix(roads, mat, frontier, c(currX, currY), c(currX, currY-1), c(x, y), path, man)
      mat = nodes[[1]]
      frontier = nodes[[2]]
    }
    
    # Get and remove fittest node in the frontier and update current nodes
    new = frontier[[1]]
    currX = new[[2]]
    currY = new[[3]]
    frontier = frontier[-1]
    path = path + 1
  }

  # Traverse the road map from the destination to the source, extracting the first move
  nextNode = makeChoice(car, mat, x, y)
  return (nextNode)
}

# Goes towards a destination with a package that is already picked up.
goToDest = function(car, roads, packages, dim, manH) {
  car$nextMove = aStar(roads, car, packages[car$load,3], packages[car$load,4], dim, manH)
  return (car) 
}

# finds a package and moves towards it
goToPack = function(car, roads, packages, dim, man, manH) {
  pack = findPackage(car, packages, man)
  car$nextMove = aStar(roads, car, pack[[1]], pack[[2]], dim, manH)
  return (car)
}

#' @export
funcerino <- function(roads,car,packages, dim, man, manH) {
  # If the car already has a package it goes and delivers it.
  if (car$load>0) {
    car = goToDest(car, roads, packages, dim, manH)
  }
  # If the car doesn't have a package it goes and finds the nearest one. 
  else{
    car = goToPack(car, roads, packages, dim, man, manH)
  }
  
  return (car)
}

#' @export
runNtimes <- function(n) {
  print("STARTING")
  i = 1
  while (n > 0) {
    runDeliveryMan(funcerino, 10, 2000, T, 0, 5, T, T)
    print(paste("RUN COMPLETE: ", i))
    n = n - 1
    i = i + 1
  }
  print("DONE!!!")
}