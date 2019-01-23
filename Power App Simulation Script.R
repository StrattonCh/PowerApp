##################################
### SIMULATE POWER FOR UNIFORM ###
##################################

########################
## MAX - NOT EQUAL TO ##
########################
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

########################
## MIN - GREATER THAN ##
########################
theta.not <- 3
alpha <- .05
n <- 1
k <- theta.not*(1 - alpha^(1/n))

test_func <- function(sample, statistic = 'min'){
  if(statistic == 'min'){
    stat <- min(sample)
    return(ifelse(stat > k, 1, 0))
  }
}

#theta is less than k
theta <- 2.80
mean(replicate(100000, test_func(runif(n, 0, theta))))

#theta is greater than k
theta <- 4
mean(replicate(100000, test_func(runif(n, 0, theta))))

#####################
## MIN - LESS THAN ##
#####################
theta.not <- 3
alpha <- .05
n <- 1
k <- theta.not*(1 - (1 - alpha)^(1/n))

test_func <- function(sample, statistic = 'min'){
  if(statistic == 'min'){
    stat <- min(sample)
    return(ifelse(stat < k, 1, 0))
  }
}

#theta is less than k
theta <- .10
mean(replicate(100000, test_func(runif(n, 0, theta))))

#theta is greater than k
theta <-1
mean(replicate(100000, test_func(runif(n, 0, theta))))

########################
## MIN - NOT EQUAL TO ##
########################
theta.not <- 3
alpha <- .05
n <- 1
k1.min <- theta.not*(1 - (1 - alpha/2)^(1/n))
k2.min <- theta.not*(1 - (alpha/2)^(1/n))

k1.max <- theta.not*(alpha/2)^(1/n)
k2.max <- theta.not*(1 - alpha/2)^(1/n)

test_func <- function(sample, statistic = 'min'){
  if(statistic == 'min'){
    stat <- min(sample)
    return(ifelse(stat < k1.min | stat > k2.min, 1, 0))
  }
  if(statistic == "max"){
    stat <- min(sample)
    return(ifelse(stat < k1.max | stat > k2.max, 1, 0))
  }
}

#theta is less than k1
theta <- .05
mean(replicate(100000, test_func(runif(n, 0, theta))))
mean(replicate(100000, test_func(runif(n, 0, theta), statistic = 'max')))

#theta is between k1 and k2
theta <- .10
mean(replicate(100000, test_func(runif(n, 0, theta))))
mean(replicate(100000, test_func(runif(n, 0, theta), statistic = 'max')))

theta <- 1.5
mean(replicate(100000, test_func(runif(n, 0, theta))))
mean(replicate(100000, test_func(runif(n, 0, theta), statistic = 'max')))

theta <- 2.8
mean(replicate(100000, test_func(runif(n, 0, theta))))
mean(replicate(100000, test_func(runif(n, 0, theta), statistic = 'max')))

theta <- k2
mean(replicate(100000, test_func(runif(n, 0, theta))))
mean(replicate(100000, test_func(runif(n, 0, theta), statistic = 'max')))


#theta is between k2 and theta not
theta <- 2.96
mean(replicate(1000000, test_func(runif(n, 0, theta))))
mean(replicate(100000, test_func(runif(n, 0, theta), statistic = 'max')))

#theta is greater than theta not
theta <- 3.5
mean(replicate(100000, test_func(runif(n, 0, theta))))
mean(replicate(100000, test_func(runif(n, 0, theta), statistic = 'max')))
