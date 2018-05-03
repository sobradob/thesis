norm_vec <- function(x) sqrt(sum(x^2))
new_point <- function(p0, p1, di) { # Finds point in distance di from point p0 in direction of point p1
  v = p1 - p0
  u = v / norm_vec(v)
  return (p0 + u * di)
}

findPoints <- function(river, M) {
  
  result = river[1,,drop=FALSE] 
  # for all subsequent points p1, p2 in this data.frame norm_vec(p2 - p1) = M at all times
  equidistantPoints = river[1,,drop=FALSE] 
  river = tail(river, n = -1)
  accDist = 0
  
  
  while (length(river) > 0) {
    point = river[1,]
    lastPoint = result[1,]
    
    dist = norm_vec(point - lastPoint)    
    
    if ( accDist + dist > M ) {
      np = new_point(lastPoint, point, M - accDist)
      equidistantPoints = rbind(np, equidistantPoints) # add np to equidistantPoints
      result = rbind(np, result) # add np to result
      accDist = 0 # reset accDist
    } else {
      #move point from river to result  
      river = tail(river, n = -1)
      result = rbind(point, result)    
      #update accDist  
      accDist = accDist + dist
    }
  }
  allPoints = result[NROW(result):1,] # reverse result
  return(list(newPoints = equidistantPoints, allPoints = allPoints))
}
