



#input$obs: number of  observations
#input$k: number of crosses
#observations = 10
#lengthRatio = 80
simulateThrows <- function(observations, lengthRatio){
  
  d <- 5 # distance of two adjacent lines
  l <- lengthRatio * d / 100 # length of needle
  
  #seed1 <- runif(1, 10, 100)
  #seed <- input$goButton[[1]] + seed1
  
  #seed <- runif(1, 10, 100)
  
  # initialization

  
  xe <- numeric(observations)
  xs <- numeric(observations)
  ye <- numeric(observations)
  ys <- numeric(observations)
  
  # simulation for the location of midpoint (x,y) and the angle (alpha)    
  #set.seed(seed)
  mNx <- runif(observations, -10, 10)
  #set.seed(seed + 1)
  mNy <- runif(observations, -10, 10)
  #set.seed(seed + 2)
  alpha <- runif(observations, 0, pi)
      
  ind1 <- which(alpha > pi/2)
  ind2 <- which(alpha <= pi/2)
  
  xe[ind1] <- mNx[ind1] - l/2 * cos(alpha[ind1]) # x for right endpoint
  xs[ind1] <- mNx[ind1] + l/2 * cos(alpha[ind1]) # x for left endpoint    
  
  ye[ind1] <- mNy[ind1] - l/2 * sin(alpha[ind1]) # y for right endpoint
  ys[ind1] <- mNy[ind1] + l/2 * sin(alpha[ind1]) # y for left endpoint

  xe[ind2] <- mNx[ind2] + l/2 * cos(alpha[ind2]) # right endpoint
  xs[ind2] <- mNx[ind2] - l/2 * cos(alpha[ind2]) # left endpoint
      
  ye[ind2] <- mNy[ind2] + l/2 * sin(alpha[ind2]) # right endpoint
  ys[ind2] <- mNy[ind2] - l/2 * sin(alpha[ind2]) # left endpoint 
  
  

  
  ## my method
  ## calculate number of intersections,  plot with 10 x 10 size and d = 5
  
  k1 <- (ye + 10)*(ys + 10) < 0
  k2 <- (ye + 5)*(ys + 5) < 0
  k3 <- (ye + 0)*(ys + 0) < 0
  k4 <- (ye - 5)*(ys - 5) < 0
  k5 <- (ye - 10)*(ys - 10) < 0
  # count crosses
  k <- sum(k1,k2,k3,k4,k5)
  
  # record data for all crosses
  
  indexCrosses = k1+k2+k3+k4+k5
  
  xsCrosses = xs[indexCrosses==1]
  xeCrosses = xe[indexCrosses==1]
  ysCrosses = ys[indexCrosses==1]
  yeCrosses = ye[indexCrosses==1]


  return(list(k = k, 
              xs = xs, xe = xe, ys = ys, ye = ye,
              xsCrosses = xsCrosses, xeCrosses = xeCrosses,
              ysCrosses = ysCrosses, yeCrosses = yeCrosses))
}

#simulate_throws(10,80)["xsCrosses"]

