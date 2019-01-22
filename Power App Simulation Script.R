##################################
### SIMULATE POWER FOR UNIFORM ###
##################################

theta.not <- 3
alpha <- .05
n <- 1
k1 <- theta.not*(alpha/2)^(1/n)
k2 <- theta.not*(1 - alpha/2)^(1/n)

test_func <- function(sample, statistic = 'max'){
  if(statistic == 'max'){
    stat <- max(sample)
    return(ifelse(stat < k1 | stat > k2, 1, 0))
  }
}

#theta is less than k1
theta <- .05
mean(replicate(100000, test_func(runif(n, 0, theta))))

#theta is between k1 and k2
theta <- .10
mean(replicate(100000, test_func(runif(n, 0, theta))))

theta <- 1.5
mean(replicate(100000, test_func(runif(n, 0, theta))))

theta <- 2.8
mean(replicate(100000, test_func(runif(n, 0, theta))))

theta <- k2
mean(replicate(100000, test_func(runif(n, 0, theta))))


#theta is between k2 and theta not
theta <- 2.96
mean(replicate(1000000, test_func(runif(n, 0, theta))))

#theta is greater than theta not
theta <- 3.5
mean(replicate(100000, test_func(runif(n, 0, theta))))
