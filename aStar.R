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
  
  return (list(packages[pack, 1], packages[pack, 2]))
}

findNodes = function(x, y) {
  dir = list(right = T, left = T, up = T, down = T)
  if (x == 1) { dir$left = F }
  if (x == 10) { dir$right = F }
  if (y == 1) { dir$down = F }
  if (y == 10) { dir$up = F }
  return (dir)
}

nodeVal = function(roads, srcX, srxY, destX, destY) {
  cost = 0
  heuristic = manhattanD(srcX, srxY, destX, destY)
  return (list(g = cost, h = heuristic))
}

updateMatrix = function(roads, mat, frontier, src, curr, dest, path) {
  if (!is.null(mat[[curr[[1]], curr[[2]]]])) {
    return (list(mat, frontier))
  }
  
  node = nodeVal(roads, curr[[1]], curr[[2]], dest[[1]], dest[[2]])
  entry = list(g = path+1, h = node$h, closed = F, arrow = list(src[[1]], src[[2]]))
  
  # currNode = mat[curr[[1]], curr[[2]]]
  # if (currNode != NULL) {
    # if (currNode$g > entry$g) {
    #   mat[curr[[1]], curr[[2]]] = list(entry)
    # }
  # }
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
    i = i - 1 # debug
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

aStar = function(roads, car, x, y) {
  # Create road map and frontier nodes
  mat <- matrix((rep(list(), 100)), nrow = 10, ncol = 10)
  frontier <- list()
  
  path <- 0
  currX = car$x
  currY = car$y

  while (currX != x | currY != y) {
    dir = findNodes(currX, currY)
    
    # Updates matrix and frontier for every accesible node
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
    
    # Get and remove fittest node in the frontier and update current nodes
    new = frontier[[1]]
    currX = new[[2]]
    currY = new[[3]]
    frontier = frontier[-1]
    path = path + 1
  }

  # Traverse the road map from the destination to the source, extracting the first move
  nextNode = traverseArrow(mat, list(x, y))
  if (nextNode[[1]] > car$x) { return (6) }
  if (nextNode[[1]] < car$x) { return (4) }
  if (nextNode[[2]] > car$y) { return (8) }
  if (nextNode[[2]] < car$y) { return (2) }
}

#' @export
funcerino <- function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
    pack = findPackage(car)
    car$nextMove = aStar(roads, car, packages[car$load,3], packages[car$load,4])
  }
  else{
    pack = findPackage(car, packages)
    car$nextMove = aStar(roads, car, pack[[1]], pack[[2]])
  }
  
  return (car)
}