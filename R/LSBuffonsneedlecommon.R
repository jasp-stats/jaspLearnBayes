



#input$obs: number of  observations
#input$k: number of crosses
simulate_throws <- function(observations, lengthRatio){
  
  d <- 5 # distance of two adjacent lines
  l <- lengthRatio * d / 100 # length of needle
  
  #seed1 <- runif(1, 10, 100)
  #seed <- input$goButton[[1]] + seed1
  
  seed <- runif(1, 10, 100)
  
  # initialization
  xe <- vector("numeric", observations)
  xs <- vector("numeric", observations)
  ye <- vector("numeric", observations)
  ys <- vector("numeric", observations)
  
  # simulation for the location of midpoint (x,y) and the angle (alpha)    
  set.seed(seed)
  mNx <<- runif(observations, -10, 10)
  set.seed(seed + 1)
  mNy <<- runif(observations, -10, 10)
  set.seed(seed + 2)
  alpha <<- runif(observations, 0, pi)
      
  ind1 <- which(alpha > pi/2)
  ind2 <- which(alpha <= pi/2)
  
  xe[ind1] <- mNx[ind1] - l/2 * cos(alpha[ind1]) # right endpoint
  xs[ind1] <- mNx[ind1] + l/2 * cos(alpha[ind1]) # left endpoint    
  
  ye[ind1] <- mNy[ind1] - l/2 * sin(alpha[ind1]) # right endpoint
  ys[ind1] <- mNy[ind1] + l/2 * sin(alpha[ind1]) # left endpoint

  xe[ind2] <- mNx[ind2] + l/2 * cos(alpha[ind2]) # right endpoint
  xs[ind2] <- mNx[ind2] - l/2 * cos(alpha[ind2]) # left endpoint
      
  ye[ind2] <- mNy[ind2] + l/2 * sin(alpha[ind2]) # right endpoint
  ys[ind2] <- mNy[ind2] - l/2 * sin(alpha[ind2]) # left endpoint 
  
  #xe1 <- mNx + l/2 * abs(cos(alpha))
  #xs1 <- mNx - l/2 * abs(cos(alpha))
  #ye1 <- mNy + l/2 * abs(sin(alpha))
  #ys1 <- mNy - l/2 * abs(sin(alpha))
  
  ## calculate number of intersections,  plot with 10 x 10 size and d = 5
  k11 <- sum(ye[ye > ys] > -10 & ys[ye > ys] < -10) #first line, alpha < pi/2
  k12 <- sum(ys[ye < ys] > -10 & ye[ye < ys] < -10) #first line, when alpha > pi/2
  
  k21 <- sum(ye[ye > ys] > -5 & ys[ye > ys] < -5)
  k22 <- sum(ys[ye < ys] > -5 & ye[ye < ys] < -5)
  k31 <- sum(ye[ye > ys] >  0 & ys[ye > ys] < 0)
  k32 <- sum(ys[ye < ys] >  0 & ye[ye < ys] < 0)
  k41 <- sum(ye[ye > ys] >  5 & ys[ye > ys] < 5)
  k42 <- sum(ys[ye < ys] >  5 & ye[ye < ys] < 5)
  k51 <- sum(ye[ye > ys] >  10 & ys[ye > ys] < 10)
  k52 <- sum(ys[ye < ys] >  10 & ye[ye < ys] < 10)
      
  k <- sum(k11, k12, k21, k22, k31, k32, k41, k42, k51, k52)

  return(list(k = k, xs = xs, xe = xe, ys = ys, ye = ye))
}

simulate_throws(10,100)["k"]

