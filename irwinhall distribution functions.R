dirwinhall <- Vectorize(function(x, n, theta){
  # function to calculate density of sum of n unif(0, theta) RVs
  # inputs  : x - value at which to calculate density
  #         : n - number of unif RVs to sum
  #         : theta - population maximum
  # outputs : numeric value that is the density at x
  
  if(x/theta < 0) return(0)
  if(x/theta > n) return(0)
  if(x/theta >= 0 & x/theta <= n){
    X  <-  floor(x/theta)
    r <- seq(from = 0,  to = X)
    s <-  (-1)^r * choose(n, r)*(x/theta-r)^(n-1)/factorial(n-1)
    return(sum(s)/theta)
  }
})

pirwinhall <- Vectorize(function(q, n, theta){
  # function to calculate cumulative density of sum of n unif(0, theta) RVs
  # inputs  : q - quantile
  #         : n - number of unif RVs to sum
  #         : theta - population maximum
  # outputs : numeric value that is the cumulative density at x
  
  if(q/theta < 0) return(0)
  if(q/theta > n) return(1)
  if(q/theta >= 0 & q/theta <= n){
    X  <-  floor(q/theta)
    r <- seq(from = 0,  to = X)
    s <-  (-1)^r * choose(n, r)*(q/theta-r)^(n)/factorial(n)
    return(sum(s))
  }
})

pirwinhall_zero <- Vectorize(function(q, n, theta, p){
  # function to calculate cumulative density of sum of n unif(0, theta) RVs
  # inputs  : q - quantile
  #         : n - number of unif RVs to sum
  #         : theta - population maximum
  # outputs : numeric value that is the cumulative density at x
  
  if(q/theta < 0) return(0)
  if(q/theta > n) return(1)
  if(q/theta >= 0 & q/theta <= n){
    X  <-  floor(q/theta)
    r <- seq(from = 0,  to = X)
    s <-  (-1)^r * choose(n, r)*(q/theta-r)^(n)/factorial(n)
    return(sum(s) - p)
  }
})

qirwinhall <- Vectorize(function(p, n, theta){
  # function to calculate quantile of sum of n unif(0, theta) RVs
  # inputs  : x - probability
  #         : n - number of unif RVs to sum
  #         : theta - population maximum
  # outputs : numeric value that is the cumulative density at x
  
  tmp <- uniroot(pirwinhall_zero, n = n, theta = theta, p = p, lower = 0, upper = n*theta, tol = .000000001)
  return(tmp[[1]])
  
})

################
### EXAMPLES ###
################
n <- 4
theta <- 2


# density function
tmp <- replicate(1000000, sum(runif(n, 0, theta)))
hist(tmp, breaks = "FD", freq = F)
curve(dirwinhall(x, n, theta), add = T, n = 1001, col = 2, lwd = 2)

# distribution function
tmp2 <- sample(tmp, 10000)
plot(ecdf(tmp2))
curve(pirwinhall(q = x, n, theta), add = T, n = 1001, col = 2, lwd = 2, lty = 2)

# quantile function
qirwinhall(p = .025, n = 4, theta = 2)
pirwinhall(q = 1.760223, n = 4, theta = 2)



