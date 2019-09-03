
exp_samp_max_pwrfunc_greater <- function(theta, alpha, theta_not, n){
  #function to calculate power function of sample max from exponential(theta) distribution
  1 - (1 - exp(theta_not*log(1 - (1 - alpha)^(1/n)) / (theta) ))^n
}
exp_samp_max_pwrfunc_less <- function(theta, alpha, theta_not, n){
  #function to calculate power function of sample max from exponential(theta) distribution
  (1 - exp(theta_not*log(1 - (alpha)^(1/n)) / (theta) ))^n
}
exp_samp_max_pwrfunc_noteqto <- function(theta, alpha, theta_not, n){
  #function to calculate power function of sample max from exponential(theta) distribution
  1 - (1 - exp(theta_not*log(1 - (1 - alpha/2)^(1/n)) / (theta) ))^n + (1 - exp(theta_not*log(1 - (alpha/2)^(1/n)) / (theta) ))^n
}


norm_samp_min_pwrfunc_greater <- function(theta, alpha, sigma, theta_not, n){
  #function to calculate power of sample min of normal(theta, sigma^2) distribution
  k <- qnorm(1 - alpha^(1/n), theta_not, sigma)
  (1 - pnorm(k, theta, sigma))^n
}
norm_samp_min_pwrfunc_less <- function(theta, alpha, sigma, theta_not, n){
  #function to calculate power of sample min of normal(theta, sigma^2) distribution
  k <- qnorm(1 - (1 - alpha)^(1/n), theta_not, sigma)
  1 - (1 - pnorm(k, theta, sigma))^n
}
norm_min_cdf <- function(x, theta, sigma, n){
  #function to calculate cdf of sample min from normal(theta, sigma^2)
  1 - (1 - pnorm(x, theta, sigma))^n
}
norm_samp_min_pwrfunc_noteqto <- function(theta, alpha, sigma, theta_not, n){
  #function to calculate power of sample min of normal(theta, sigma^2) distribution
  k1 <- qnorm(1 - (1 - alpha/2)^(1/n), theta_not, sigma)
  k2 <- qnorm(1 - (alpha/2)^(1/n), theta_not, sigma)
  1 - norm_min_cdf(k2, theta, sigma, n) + norm_min_cdf(k1, theta, sigma, n)
}
norm_max_cdf <- function(x, theta, sigma, n){
  #function to calculate the cdf of the sample max from normal(theta, sigma^2)
  (pnorm(x, theta, sigma))^n
}
norm_samp_max_pwrfunc_greater <- function(theta, alpha, sigma, theta_not, n){
  #function to calculate power of sample max of normal(theta, sigma^2) distribution
  k <- qnorm((1 - alpha)^(1/n), theta_not, sigma)
  1 - norm_max_cdf(k, theta, sigma, n)
}
norm_samp_max_pwrfunc_less <- function(theta, alpha, sigma, theta_not, n){
  #function to calculate power of sample max of normal(theta, sigma^2) distribution
  k <- qnorm(alpha^(1/n), theta_not, sigma)
  norm_max_cdf(k, theta, sigma, n)
}
norm_samp_max_pwrfunc_noteqto <- function(theta, alpha, sigma, theta_not, n){
  #function to calculate power of sample max of normal(theta, sigma^2) distribution
  k1 <- qnorm((alpha/2)^(1/n), theta_not, sigma)
  k2 <- qnorm((1 -alpha/2)^(1/n), theta_not, sigma)
  1 - norm_max_cdf(k2, theta, sigma, n) + norm_max_cdf(k1, theta, sigma, n)
}

unif_samp_max_pwrfunc_greater <- Vectorize(function(theta, alpha, theta_not, n){
  #function to calculate power of sample max of unif(0,theta)
  k <- theta_not*(1 - alpha)^(1/n)
  if(is.na(theta)){
    return(NA)
  }
  if(theta < k){
    return(0)
  }
  if(k <= theta){
    return(1 - k^n/theta^n)
  }
}, SIMPLIFY = TRUE)

unif_samp_max_pwrfunc_less <- Vectorize(function(theta, alpha, theta_not, n){
  #function to calculate power of sample max of unif(0,theta)
  k <- theta_not*(alpha)^(1/n)
  if(is.na(theta)){
    return(NA)
  }
  if(theta < k){
    return(1)
  }
  if(k <= theta){
    return(k^n/theta^n)
  }
})

unif_samp_max_pwrfunc_noteqto <- Vectorize(function(theta, alpha, theta_not, n){
  #function to calculate power of sample max of unif(0,theta)
  k1 <- theta_not*(alpha/2)^(1/n)
  k2 <- theta_not*(1 - alpha/2)^(1/n)
  if(is.na(theta)){
    return(NA)
  }
  if(theta <= k1){
    return(1)
  }
  if(k1 < theta & theta <= k2){
    return(alpha*theta_not^n / (2*theta^n))
  }
  if(k2 <= theta){
    return(1 - theta_not^n * (1 - alpha/2) / theta^n + alpha*theta_not^n / (2*theta^n))
  }
})

unif_samp_min_pwrfunc_greater <- Vectorize(function(theta, alpha, theta_not, n){
  #function to calculate power of sample min of unif(0,theta)
  k <- theta_not*(1 - alpha^(1/n))
  if(is.na(theta)){
    return(NA)
  }
  if(theta <= k){
    return(0)
  }
  if(theta > k){
    return((1 - k/theta)^n)
  }
})

unif_samp_min_pwrfunc_less <- Vectorize(function(theta, alpha, theta_not, n){
  #function to calculate power of sample min of unif(0,theta)
  k <- theta_not*(1 - (1 - alpha)^(1/n))
  if(is.na(theta)){
    return(NA)
  }
  if(theta <= k){
    return(1)
  }
  if(theta > k){
    return(1 - (1 - k/theta)^n)
  }
})

unif_samp_min_pwrfunc_noteqto <- Vectorize(function(theta, alpha, theta_not, n){
  #function to calculate power of sample min of unif(0,theta)
  k1 <- theta_not*(1 - (1 - alpha/2)^(1/n))
  k2 <- theta_not*(1 - (alpha/2)^(1/n))
  if(is.na(theta)){
    return(NA)
  }
  if(theta <= k1){
    return(1)
  }
  if(k1 < theta & theta <= k2){
    return(1 - (1 - k1/theta)^n)
  }
  if(theta > k2){
    return(1 + (1 - k2/theta)^n - (1 - k1/theta)^n)
  }
})

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
  
  tmp <- uniroot(pirwinhall_zero, n = n, theta = theta, p = p, lower = 0, upper = n*theta, tol = .0001, extendInt = "yes")
  return(tmp[[1]])
  
})

unif_sum_pwrfunc_greater <- function(theta, alpha, theta_not, n){
  
  if(is.na(theta[length(theta)])){
    return(NA)
  }
  k <- qirwinhall(p = 1 - alpha, n = n, theta = theta_not)
  return(1 - pirwinhall(q = k, n = n, theta = theta))
}

unif_sum_pwrfunc_less <- function(theta, alpha, theta_not, n){
  
  if(is.na(theta[length(theta)])){
    return(NA)
  }
  k <- qirwinhall(p = alpha, n = n, theta = theta_not)
  return(pirwinhall(q = k, n = n, theta = theta))
}

unif_sum_pwrfunc_noteqto <- function(theta, alpha, theta_not, n){
  
  if(is.na(theta[length(theta)])){
    return(NA)
  }
  k1 <- qirwinhall(p = alpha/2, n = n, theta = theta_not)
  k2 <- qirwinhall(p = 1 - alpha/2, n = n, theta = theta_not)
  return(1 - pirwinhall(q = k2, n = n, theta = theta) + pirwinhall(q = k1, n = n, theta = theta))
}

unif_norm_approx_greater <- function(theta, alpha, theta_not, n){
  k1 <- qnorm(1 - alpha, n*theta_not/2, sqrt(n * theta_not^2 / 12))
  
  return(1 - pnorm(k1, n*theta/2, sqrt(n * theta^2 / 12)))
}

unif_norm_approx_less <- function(theta, alpha, theta_not, n){
  k1 <- qnorm(alpha, n*theta_not/2, sqrt(n * theta_not^2 / 12))
  
  return(pnorm(k1, n*theta/2, sqrt(n * theta^2 / 12)))
}

unif_norm_approx_noteq <- function(theta, alpha, theta_not, n){
  k1 <- qnorm(alpha/2, n*theta_not/2, sqrt(n * theta_not^2 / 12))
  k2 <- qnorm(1-alpha/2, n*theta_not/2, sqrt(n*theta_not^2 / 12))
  
  return(1 - pnorm(k2, n*theta/2, sqrt(n * theta^2 / 12)) + pnorm(k1, n*theta/2, sqrt(n * theta^2 / 12)))
}

##############################
### SAMPLING DISTRIBUTIONS ###
##############################


# exponential
exp.samp <- function(statistic, alternative, theta, theta.not, n, alpha){

  if(statistic == "sum"){
    if(alternative == 'Not equal to'){
      k1 <- qgamma(alpha/2, n, 1/theta.not)
      k2 <- qgamma(1 - alpha/2, n, 1/theta.not)
      power <- 1 - pgamma(k2, n, 1/theta) + pgamma(k1, n, 1/theta)
      upper.power <- power - pgamma(k1, n, 1/theta)
      lower.power <- pgamma(k1, n, 1/theta)
      
      
      upper.x <- max(c(2*n*theta, 2*n*theta.not, k2 + 1))
      
      plot(1, type = "n", las = 1,
           xlim = c(0.001, upper.x),
           ylim = c(0, max(
             c(max(dgamma(seq(0, 2*n*theta), n, 1 / theta)),
               max(dgamma(seq(0, 2*n*theta.not), n, 1 / theta.not)))
           )),
           xlab = "T(x)",
           ylab = bquote(f[Sigma(X[i])](x)),
           main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      
      (null.curve <- curve(dgamma(x, n, 1 / theta.not), add = T, n = 1000, lty = 2))
      (alt.curve <- curve(dgamma(x, n, 1/theta), add = T, n = 1000))
      
      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      if(lower.power < alpha/2){
        polygon(x = c(0.001, 0.001, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, 0,
                      dgamma(null.curve$x[which(null.curve$x < k1)], n, 1/theta.not), 0),
                col = "red")
        polygon(x = c(0.001, 0.001, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, 0,
                      dgamma(alt.curve$x[which(alt.curve$x < k1)], n, 1/theta), 0),
                col = "grey")
        text(x = k1, y = dgamma(k1, n, 1/theta.not), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 2)
        
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], upper.x),
                y = c(0, dgamma(k2, n, 1/theta),
                      dgamma(alt.curve$x[which(alt.curve$x > k2)], n, 1/theta), 0),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], upper.x),
                y = c(0, dgamma(k2, n, 1/theta.not),
                      dgamma(null.curve$x[which(null.curve$x > k2)], n, 1/theta.not), 0),
                col = "red")
        text(x = k2, y = dgamma(k2, n, 1/theta), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
      } else{
        polygon(x = c(0.001, 0.001, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, 0,
                      dgamma(alt.curve$x[which(alt.curve$x < k1)], n, 1/theta), 0),
                col = "grey")
        polygon(x = c(0.001, 0.001, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, 0,
                      dgamma(null.curve$x[which(null.curve$x < k1)], n, 1/theta.not), 0),
                col = "red")
        text(x = k1, y = dgamma(k1, n, 1/theta), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 2)
        
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], k2),
                y = c(0, dgamma(k2, n, 1/theta.not),
                      dgamma(null.curve$x[which(null.curve$x > k2)], n, 1/theta.not), 0),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], k2),
                y = c(0, dgamma(k2, n, 1/theta),
                      dgamma(alt.curve$x[which(alt.curve$x > k2)], n, 1/theta), 0),
                col = "grey")
        text(x = k2, y = dgamma(k2, n, 1/theta.not), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
      }
      
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
    } else if(alternative == 'Greater than'){
      
      g.star <- qgamma(alpha, n, 1 / theta.not, lower.tail = F)
      upper.x <- max(c(2*n*theta, 2*n*theta.not, g.star + 1))
      
      plot(1, type = "n", las = 1,
           xlim = c(0.001, upper.x),
           ylim = c(0, max(
             c(max(dgamma(seq(0, 2*n*theta), n, 1 / theta)),
               max(dgamma(seq(0, 2*n*theta.not), n, 1 / theta.not)))
           )),
           xlab = "T(x)",
           ylab = bquote(f[Sigma(X[i])](x)),
           main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(dgamma(x, n, 1 / theta), add = T, n = 1000))
      (null.curve <- curve(dgamma(x, n, 1 / theta.not), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")
      if(1 - pchisq(theta.not*qchisq(1 - alpha, 2*n)/theta, 2*n) >= alpha ) {
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], upper.x),
                y = c(0, dgamma(g.star, n, 1 / theta),
                      dgamma(alt.curve$x[which(alt.curve$x > g.star)], n, 1/theta), 0),
                col = "grey")
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], upper.x),
                y = c(0, dgamma(g.star, n, 1 / theta.not),
                      dgamma(null.curve$x[which(null.curve$x > g.star)], n, 1/theta.not), 0),
                col = "red")
      } else {
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], upper.x),
                y = c(0, dgamma(g.star, n, 1 / theta.not),
                      dgamma(null.curve$x[which(null.curve$x > g.star)], n, 1/theta.not), 0),
                col = "red")
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], upper.x),
                y = c(0, dgamma(g.star, n, 1 / theta),
                      dgamma(alt.curve$x[which(alt.curve$x > g.star)], n, 1/theta), 0),
                col = "grey")
      }
      
      power <- round(1 - pchisq(theta.not*qchisq(1 - alpha, 2*n)/theta, 2*n), 3)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = dgamma(g.star, n, 1 / theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 2)
    } else if(alternative == 'Less than'){
      plot(1, type = "n", las = 1,
           xlim = c(0.001, max(c(2*n*theta, 2*n*theta.not))),
           ylim = c(0, max(
             c(max(dgamma(seq(0, 2*n*theta), n, 1 / theta)),
               max(dgamma(seq(0, 2*n*theta.not), n, 1 / theta.not)))
           )),
           xlab = "T(x)",
           ylab = bquote(f[Sigma(X[i])](x)),
           main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(dgamma(x, n, 1 / theta), add = T, n = 1000))
      (null.curve <- curve(dgamma(x, n, 1 / theta.not), add = T, lty = 2, n = 1000))
      g.star <- qgamma(alpha, n, 1 / theta.not, lower.tail = T)
      abline(v = g.star, lty = 4, col = "red")
      if(pchisq(theta.not*qchisq(alpha, 2*n)/theta, 2*n) >= alpha ) {
        polygon(x = c(0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, dgamma(alt.curve$x[which(alt.curve$x < g.star)], n, 1/theta),
                      dgamma(g.star, n, 1 / theta), 0),
                col = "grey")
        polygon(x = c(0, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                y = c(0, dgamma(null.curve$x[which(null.curve$x < g.star)], n, 1/theta.not),
                      dgamma(g.star, n, 1 / theta.not), 0),
                col = "red")
      } else {
        polygon(x = c(0, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                y = c(0, dgamma(null.curve$x[which(null.curve$x < g.star)], n, 1/theta.not),
                      dgamma(g.star, n, 1 / theta.not), 0),
                col = "red")
        polygon(x = c(0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, dgamma(alt.curve$x[which(alt.curve$x < g.star)], n, 1/theta),
                      dgamma(g.star, n, 1 / theta), 0),
                col = "grey")
      }
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(pchisq(theta.not*qchisq(alpha, 2*n)/theta, 2*n), 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      #text(x = 100, y = .001, labels = paste("Power = ", dgamma(theta, n, 1 / theta)))
      text(x = g.star, y = dgamma(g.star, n, 1 / theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 4)
    } 
  }
  
  if(statistic == "Sample Minimum"){
    if(alternative == "Not equal to"){
      
      k1 <- qexp(alpha/2, n/theta.not)
      k2 <- qexp(1 - alpha/2, n/theta.not)
      power <- 1 - pexp(k2, n/theta) + pexp(k1, n/theta)
      upper.power <- power - pexp(k1, n/theta)
      lower.power <- pexp(k1, n/theta)
      
      plot(1, type = "n", las = 1,
           xlim = c(0.001, max(c(5*theta/n, 5*theta.not/n))), #5 times gives at least 96% of sampling dist by Chebychevs
           ylim = c(0, max(
             c(max(dexp(seq(0, 5*theta/n), n / theta)),
               max(dexp(seq(0, 5*theta.not/n), n / theta.not)))
           )),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      
      (null.curve <- curve(dexp(x, n / theta.not), add = T, n = 1000, lty = 2))
      (alt.curve <- curve(dexp(x, n/theta), add = T, n = 1000))
      
      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      if(lower.power < alpha/2){
        polygon(x = c(0.001, 0.001, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, 0,
                      dexp(null.curve$x[which(null.curve$x < k1)], n/theta.not), 0),
                col = "red")
        polygon(x = c(0.001, 0.001, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, 0,
                      dexp(alt.curve$x[which(alt.curve$x < k1)], n/theta), 0),
                col = "grey")
        text(x = k1, y = dexp(k1, n/theta), labels = bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 4)

        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], k2),
                y = c(0, dexp(k2, n/theta),
                      dexp(alt.curve$x[which(alt.curve$x > k2)], n/theta), 0),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], k2),
                y = c(0, dexp(k2, n/theta.not),
                      dexp(null.curve$x[which(null.curve$x > k2)], n/theta.not), 0),
                col = "red")
        text(x = k2, y = dexp(k2, n/theta), labels = bquote('crit val'~'='~.(round(k2, 4))), col = "red", pos = 4)
      } else{
        polygon(x = c(0.001, 0.001, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, 0,
                      dexp(alt.curve$x[which(alt.curve$x < k1)], n/theta), 0),
                col = "grey")
        polygon(x = c(0.001, 0.001, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, 0,
                      dexp(null.curve$x[which(null.curve$x < k1)], n/theta.not), 0),
                col = "red")
        text(x = k1, y = dexp(k1, n/theta), labels = bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 4)

        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], k2),
                y = c(0, dexp(k2, n/theta.not),
                      dexp(null.curve$x[which(null.curve$x > k2)], n/theta.not), 0),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], k2),
                y = c(0, dexp(k2, n/theta),
                      dexp(alt.curve$x[which(alt.curve$x > k2)], n/theta), 0),
                col = "grey")
        text(x = k2, y = dexp(k2, n/theta), labels = bquote('crit val'~'='~.(round(k2, 4))), col = "red", pos = 4)
      }
      
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      
    } else if(alternative == 'Less than'){
      plot(1, type = "n", las = 1,
           xlim = c(0.001, max(c(5*theta/n, 5*theta.not/n))), #5 times gives at least 96% of sampling dist by Chebychevs
           ylim = c(0, max(
             c(max(dexp(seq(0, 5*theta/n), n / theta)),
               max(dexp(seq(0, 5*theta.not/n), n / theta.not)))
           )),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(dexp(x, n / theta), add = T, n = 1000))
      (null.curve <- curve(dexp(x, n / theta.not), add = T, lty = 2, n = 1000))
      g.star <- qexp(alpha, n / theta.not, lower.tail = T)
      abline(v = g.star, lty = 4, col = "red")
      
      if(1 - exp(theta.not*log(1 - alpha)/theta) >= alpha ) {
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, dexp(0, n/ theta), dexp(alt.curve$x[which(alt.curve$x < g.star)], n /theta),
                      dexp(g.star, n/ theta), 0), col = "grey")
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                y = c(0, dexp(0, n/theta.not), dexp(null.curve$x[which(null.curve$x < g.star)], n / theta.not),
                      dexp(g.star, n / theta.not), 0), col = "red")
      } else {
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                y = c(0, dexp(0, n/theta.not), dexp(null.curve$x[which(null.curve$x < g.star)], n / theta.not),
                      dexp(g.star, n / theta.not), 0), col = "red")
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, dexp(0, n/ theta), dexp(alt.curve$x[which(alt.curve$x < g.star)], n /theta),
                      dexp(g.star, n/ theta), 0), col = "grey")
      }
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(1 - exp(theta.not*log(1 - alpha)/theta), 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = dgamma(g.star, n, 1 / theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 4))), col = "red", pos = 4)
    } else if(alternative == 'Greater than'){
      plot(1, type = "n", las = 1,
           xlim = c(0.001, max(c(5*theta/n, 5*theta.not/n))), #5 times gives at least 96.5% of sampling dist by Chebychevs
           ylim = c(0, max(
             c(max(dexp(seq(0, 5*theta/n), n / theta)),
               max(dexp(seq(0, 5*theta.not/n), n / theta.not)))
           )),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(dexp(x, n / theta), add = T, n = 1000))
      (null.curve <- curve(dexp(x, n / theta.not), add = T, lty = 2, n = 1000))
      g.star <- qexp(alpha, n / theta.not, lower.tail = F)
      abline(v = g.star, lty = 4, col = "red")
      if(exp(theta.not*log(alpha)/theta) >= alpha ) {
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                y = c(0, dexp(g.star, n/ theta),
                      dexp(alt.curve$x[which(alt.curve$x > g.star)], n /theta), 0),
                col = "grey")
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                y = c(0, dexp(g.star, n / theta.not),
                      dexp(null.curve$x[which(null.curve$x > g.star)], n / theta.not), 0),
                col = "red")
      } else {
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                y = c(0, dexp(g.star, n / theta.not),
                      dexp(null.curve$x[which(null.curve$x > g.star)], n / theta.not), 0),
                col = "red")
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                y = c(0, dexp(g.star, n/ theta),
                      dexp(alt.curve$x[which(alt.curve$x > g.star)], n /theta), 0),
                col = "grey")
      }
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(exp(theta.not*log(alpha)/theta), 2)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      
      text(x = g.star, y = dexp(g.star, n / theta.not), labels =  bquote('crit val'~'='~.(round(g.star,4))), col = "red", pos = 2)
    }

  }
  
  if(statistic == 'Sample Maximum'){
    
    if(alternative == 'Not equal to'){
      
      k1 <- -theta.not * log(1 - (alpha/2) ^ (1/n))
      k2 <- -theta.not * log(1 - (1 - alpha/2) ^ (1/n))
      power <- 1 - (1 - exp(-k2/theta))^n + (1 - exp(-k1/theta))^n
      upper.power <- power - (1 - exp(-k1/theta))^n
      lower.power <- (1 - exp(-k1/theta))^n
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n){
        (1 - exp(-x/theta))^n - y
      }
      sampdist <- function(x, theta, n){
        n*(1 - exp(-x/theta))^(n-1) * exp(-x/theta) * 1/theta
      }
      power.func <- function(theta){
        1 - (1 - exp(k2/theta))^n + (1 - exp(k1/theta))^n
      }

      # plotting limits
      upper.x <- max(c(
        uniroot(cdf, y = .999, theta = theta, n = n, lower = 0, upper = 5*n*theta, extendInt = "yes")$root,
        uniroot(cdf, y = .999, theta = theta.not, n = n, lower = 0, upper = 5*n*theta.not, extendInt = "yes")$root
      ))
      nullmax <- max(sapply(seq(from = 0, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n)))
      altmax <- max(sapply(seq(from = 0, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(0.001, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(n)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      
      (alt.curve <- curve(sampdist(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n), add = T, lty = 2, n = 1000))

      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      if(lower.power < alpha/2){
        polygon(x = c(0.001, 0.001, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, 0,
                      sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        polygon(x = c(0.001, 0.001, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, 0,
                      sampdist(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")
        text(x = k1, y = sampdist(k1, theta, n), labels = bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 4)

        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], k2),
                y = c(0, sampdist(k2, theta, n),
                      sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n), 0),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], k2),
                y = c(0, sampdist(k2, theta.not, n),
                      sampdist(null.curve$x[which(null.curve$x > k2)], theta.not, n), 0),
                col = "red")
        text(x = k2, y = sampdist(k2, theta, n), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
      } else{
        polygon(x = c(0.001, 0.001, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, 0,
                      sampdist(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")
        polygon(x = c(0.001, 0.001, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, 0,
                      sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        text(x = k1, y = sampdist(k1, theta, n), labels = bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 4)

        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], k2),
                y = c(0, sampdist(k2, theta.not, n),
                      sampdist(null.curve$x[which(null.curve$x > k2)], theta.not, n), 0),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], k2),
                y = c(0, sampdist(k2, theta, n),
                      sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n), 0),
                col = "grey")
        text(x = k2, y = sampdist(k2, theta.not, n), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
      }

      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      
    } 
    
    if(alternative == 'Less than'){
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n){
        (1 - exp(-x/theta))^n - y
      }
      sampdist <- function(x, theta, n){
        n*(1 - exp(-x/theta))^(n-1) * exp(-x/theta) * 1/theta
      }
      
      # crit val
      g.star <- -theta.not*log(1 - alpha^(1/n))
      
      # plotting limits
      upper.x <- max(c(
        uniroot(cdf, y = .99, theta = theta, n = n, lower = 0, upper = 5*n*theta)$root,
        uniroot(cdf, y = .99, theta = theta.not, n = n, lower = 0, upper = 5*n*theta.not)$root
      ))
      nullmax <- max(sapply(seq(from = 0, to = upper.x, length.out = 5000), FUN = function(x) sampdist(x, theta.not, n)))
      altmax <- max(sapply(seq(from = 0, to = upper.x, length.out = 5000), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(0.001, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(n)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")

      # polygons
      if(exp_samp_max_pwrfunc_less(theta, alpha, theta.not, n) >= alpha){
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta, n),
                      sampdist(g.star, theta, n), 0), col = "grey")
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, sampdist(0, theta.not, n), sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta.not, n),
                      sampdist(g.star, theta.not, n), 0), col = "red")
      } else{
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, sampdist(0, theta.not, n), sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta.not, n),
                      sampdist(g.star, theta.not, n), 0), col = "red")
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta, n),
                      sampdist(g.star, theta, n), 0), col = "grey")
      }
      
      # legend
      power <- round(exp_samp_max_pwrfunc_less(theta, alpha, theta.not, n), 3)
      legend("topright",
           legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
           lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = sampdist(g.star, theta.not, n), labels =  bquote('crit val'~'='~.(round(g.star, 4))), col = "red", pos = 4)
    }
    
    if(alternative == 'Greater than'){
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n){
        (1 - exp(-x/theta))^n - y
      }
      sampdist <- function(x, theta, n){
        n*(1 - exp(-x/theta))^(n-1) * exp(-x/theta) * 1/theta
      }
      
      # crit val
      g.star <- -theta.not*log(1 - (1 - alpha)^(1/n))
      
      # plotting limits
      upper.x <- max(c(
        uniroot(cdf, y = .999, theta = theta, n = n, lower = 0, upper = 5*n*theta, extendInt = "yes")$root,
        uniroot(cdf, y = .999, theta = theta.not, n = n, lower = 0, upper = 5*n*theta.not, extendInt = "yes")$root
      ))
      nullmax <- max(sapply(seq(from = 0, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n)))
      altmax <- max(sapply(seq(from = 0, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(0.001, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(n)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")
      
      # polygons
      if(exp_samp_max_pwrfunc_greater(theta, alpha, theta.not, n) >= alpha){
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                y = c(0, sampdist(g.star, theta, n), sampdist(alt.curve$x[which(alt.curve$x > g.star)], theta, n), 0),
                col = "grey")
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                y = c(0, sampdist(g.star, theta.not, n),
                      sampdist(null.curve$x[which(null.curve$x > g.star)], theta.not, n), 0),
                col = "red")
      } else{
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                y = c(0, sampdist(g.star, theta.not, n),
                      sampdist(null.curve$x[which(null.curve$x > g.star)], theta.not, n), 0),
                col = "red")
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                y = c(0, sampdist(g.star, theta, n), sampdist(alt.curve$x[which(alt.curve$x > g.star)], theta, n), 0),
                col = "grey")
      }
      
      # legend
      power <- round(exp_samp_max_pwrfunc_greater(theta, alpha, theta.not, n), 3)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = sampdist(g.star, theta.not, n), labels =  bquote('crit val'~'='~.(round(g.star, 4))), col = "red", pos = 2)
    }
    
  }
}

# normal
norm.samp <- function(statistic, alternative, theta, theta.not, n, alpha, sigma){
  
  if(statistic == "sum"){
    
    if(alternative == 'Not equal to'){
      
      k1 <- qnorm(alpha/2, n*theta.not, sqrt(n)*sigma)
      k2 <- qnorm(1- alpha/2, n*theta.not, sqrt(n)*sigma)
      power <- 1 - pnorm(k2, n*theta, sqrt(n)*sigma) + pnorm(k1, n*theta, sqrt(n)*sigma)
      upper.power <- power - pnorm(k1, n*theta, sqrt(n)*sigma)
      lower.power <- pnorm(k1, n*theta, sqrt(n)*sigma)
      
      # plotting limits
      
      if(is.na(theta)){
        upper.x <- max(c(
          n*theta.not + 5*sqrt(n)*sigma,
          n*theta + 5*sqrt(n)*sigma
        ))
        lower.x <- min(c(
          n*theta.not - 5*sqrt(n)*sigma,
          n*theta - 5*sqrt(n)*sigma
        ))
      } else{
        upper.x <- max(c(
          n*theta.not + 5*sqrt(n)*sigma,
          n*theta + 5*sqrt(n)*sigma
        ))
        lower.x <- min(c(
          n*theta.not - 5*sqrt(n)*sigma,
          n*theta - 5*sqrt(n)*sigma
        ))
      }
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, theta.not*n, sqrt(n)*sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, theta*n, sqrt(n)*sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[Sigma(X[i])](x)),
           main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(dnorm(x, n*theta, sqrt(n)*sigma), add = T, n = 1000))
      (null.curve <- curve(dnorm(x, n*theta.not, sqrt(n)*sigma), add = T, lty = 2, n = 1000))
      
      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      if(lower.power < alpha/2){
        polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                y = c(dnorm(null.curve$x[which(null.curve$x < k1)], n*theta.not, sqrt(n)*sigma), 
                      dnorm(k1, n*theta.not, sqrt(n)*sigma), 0),
                col = "red")
        polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                y = c(dnorm(null.curve$x[which(null.curve$x < k1)], n*theta, sqrt(n)*sigma), 
                      dnorm(k1, n*theta, sqrt(n)*sigma), 0),
                col = "grey")
        text(x = k1, y = dnorm(k1, n*theta.not, sqrt(n)*sigma), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)

        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                y = c(0, dnorm(k2, n*theta, sqrt(n)*sigma),
                      dnorm(alt.curve$x[which(alt.curve$x > k2)], n*theta, sqrt(n)*sigma)),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                y = c(0, dnorm(k2, n*theta.not, sqrt(n)*sigma),
                      dnorm(null.curve$x[which(null.curve$x > k2)], n*theta.not, sqrt(n)*sigma)),
                col = "red")
        text(x = k2, y = dnorm(k2, n*theta.not, sqrt(n)*sigma), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
      } else{
        polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                y = c(dnorm(null.curve$x[which(null.curve$x < k1)], n*theta, sqrt(n)*sigma), 
                      dnorm(k1, n*theta, sqrt(n)*sigma), 0),
                col = "grey")
        polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                y = c(dnorm(null.curve$x[which(null.curve$x < k1)], n*theta.not, sqrt(n)*sigma), 
                      dnorm(k1, n*theta.not, sqrt(n)*sigma), 0),
                col = "red")
        text(x = k1, y = dnorm(k1, n*theta.not, sqrt(n)*sigma), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
        
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                y = c(0, dnorm(k2, n*theta.not, sqrt(n)*sigma),
                      dnorm(null.curve$x[which(null.curve$x > k2)], n*theta.not, sqrt(n)*sigma)),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                y = c(0, dnorm(k2, n*theta, sqrt(n)*sigma),
                      dnorm(alt.curve$x[which(alt.curve$x > k2)], n*theta, sqrt(n)*sigma)),
                col = "grey")
        text(x = k2, y = dnorm(k2, n*theta.not, sqrt(n)*sigma), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)

      }

      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      
    } 
    
    if(alternative == 'Less than'){
      
      # crit val
      g.star <- qnorm(alpha, n*theta.not, sqrt(n)*sigma)
      
      # plotting limits
      upper.x <- max(c(
        n*theta.not + 5*sqrt(n)*sigma,
        n*theta + 5*sqrt(n)*sigma
      ))
      
      lower.x <- min(c(
        n*theta.not - 5*sqrt(n)*sigma,
        n*theta - 5*sqrt(n)*sigma
      ))
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, theta.not*n, sqrt(n)*sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, theta*n, sqrt(n)*sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[Sigma(X[i])](x)),
           main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(dnorm(x, n*theta, sqrt(n)*sigma), add = T, n = 1000))
      (null.curve <- curve(dnorm(x, n*theta.not, sqrt(n)*sigma), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")
      
      # polygons
      if(pnorm(qnorm(alpha, n*theta.not, sqrt(n)*sigma), n*theta, sqrt(n)*sigma) >= alpha){
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(dnorm(alt.curve$x[which(alt.curve$x < g.star)], n*theta, sqrt(n)*sigma),
                      dnorm(g.star, n*theta, sqrt(n)*sigma), 0), col = "grey")
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(dnorm(alt.curve$x[which(alt.curve$x < g.star)], n*theta.not, sqrt(n)*sigma),
                      dnorm(g.star, n*theta.not, sqrt(n)*sigma), 0), col = "red")
      } else{
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(dnorm(alt.curve$x[which(alt.curve$x < g.star)], n*theta.not, sqrt(n)*sigma),
                      dnorm(g.star, n*theta.not, sqrt(n)*sigma), 0), col = "red")
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(dnorm(alt.curve$x[which(alt.curve$x < g.star)], n*theta, sqrt(n)*sigma),
                      dnorm(g.star, n*theta, sqrt(n)*sigma), 0), col = "grey")
      }
      
      # legend
      power <- round(pnorm(qnorm(alpha, n*theta.not, sqrt(n)*sigma), n*theta, sqrt(n)*sigma), 2)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = dnorm(g.star, n*theta.not, sqrt(n)*sigma), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 4)
    }
    
    if(alternative == 'Greater than'){
      
      # crit val
      g.star <- qnorm(1 - alpha, n*theta.not, sqrt(n)*sigma)
      
      # plotting limits
      upper.x <- max(c(
        n*theta.not + 5*sqrt(n)*sigma,
        n*theta + 5*sqrt(n)*sigma
      ))
      
      lower.x <- min(c(
        n*theta.not - 5*sqrt(n)*sigma,
        n*theta - 5*sqrt(n)*sigma
      ))
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, theta.not*n, sqrt(n)*sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, theta*n, sqrt(n)*sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[Sigma(X[i])](x)),
           main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(dnorm(x, n*theta, sqrt(n)*sigma), add = T, n = 1000))
      (null.curve <- curve(dnorm(x, n*theta.not, sqrt(n)*sigma), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")

      # polygons
      if(1 - pnorm(qnorm(1 - alpha, n*theta.not, sqrt(n)*sigma), n*theta, sqrt(n)*sigma) >= alpha){
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                y = c(0, dnorm(g.star, theta*n, sqrt(n)*sigma), dnorm(alt.curve$x[which(alt.curve$x > g.star)], n*theta, sqrt(n)*sigma)),
                col = "grey")
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)]),
                y = c(0, dnorm(g.star, n*theta.not, sqrt(n)*sigma),
                      dnorm(null.curve$x[which(null.curve$x > g.star)], n*theta.not, sqrt(n)*sigma)),
                col = "red")
      } else{
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)]),
                y = c(0, dnorm(g.star, n*theta.not, sqrt(n)*sigma),
                      dnorm(null.curve$x[which(null.curve$x > g.star)], n*theta.not, sqrt(n)*sigma)),
                col = "red")
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                y = c(0, dnorm(g.star, theta*n, sqrt(n)*sigma), dnorm(alt.curve$x[which(alt.curve$x > g.star)], n*theta, sqrt(n)*sigma)),
                col = "grey")
      }
      
      # legend
      power <- round(1 - pnorm(qnorm(1 - alpha, n*theta.not, sqrt(n)*sigma), n*theta, sqrt(n)*sigma), 2)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = dnorm(g.star, n*theta.not, sqrt(n)*sigma), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 2)
    }
  } 
  
  if(statistic == "Sample Minimum"){
    
    if(alternative == 'Not equal to'){
      
      k1 <- qnorm(1 - (1 - alpha/2) ^ (1/n), theta.not, sigma)
      k2 <- qnorm(1 - (alpha/2) ^ (1/n), theta.not, sigma)
      power <- 1 - norm_min_cdf(k2, theta, sigma, n) + norm_min_cdf(k1, theta, sigma, n)
      upper.power <- power - norm_min_cdf(k1, theta, sigma, n)
      lower.power <- norm_min_cdf(k1, theta, sigma, n)
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n, sigma = sigma){
        1 - (1 - pnorm(x, theta, sigma))^n - y
      }
      sampdist <- function(x, theta, n, sigma = sigma){
        n * (1 - pnorm(x, theta, sigma))^(n-1) * dnorm(x, theta, sigma)
      }

      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .9999, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .9999, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n, sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n, sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n, sigma), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n, sigma), add = T, lty = 2, n = 1000))
      
      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      if(lower.power < alpha/2){
        polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n, sigma), 
                      sampdist(k1, theta.not, n, sigma), 0),
                col = "red")
        polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta, n, sigma), 
                      sampdist(k1, theta, n, sigma), 0),
                col = "grey")
        text(x = k1, y = sampdist(k1, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
        
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                y = c(0, sampdist(k2, theta, n, sigma),
                      sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n, sigma)),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                y = c(0, sampdist(k2, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > k2)], theta.not, n, sigma)),
                col = "red")
        text(x = k2, y = sampdist(k2, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
      } else{
        polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta, n, sigma), 
                      sampdist(k1, theta, n, sigma), 0),
                col = "grey")
        polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n, sigma), 
                      sampdist(k1, theta.not, n, sigma), 0),
                col = "red")
        text(x = k1, y = sampdist(k1, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
        
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                y = c(0, sampdist(k2, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > k2)], theta.not, n, sigma)),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                y = c(0, sampdist(k2, theta, n, sigma),
                      sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n, sigma)),
                col = "grey")
        text(x = k2, y = sampdist(k2, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
        
      }
      
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      
      
      
    } 
    
    if(alternative == 'Less than'){
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n, sigma = sigma){
        1 - (1 - pnorm(x, theta, sigma))^n - y
      }
      sampdist <- function(x, theta, n, sigma = sigma){
        n * (1 - pnorm(x, theta, sigma))^(n-1) * dnorm(x, theta, sigma)
      }
      
      # crit val
      g.star <- qnorm(1 - (1 - alpha)^(1/n), theta.not, sigma)
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .9999, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .9999, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n, sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n, sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n, sigma), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n, sigma), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")
      
      # polygons
      if(norm_samp_min_pwrfunc_less(theta, alpha, sigma, theta.not, n) >= alpha){
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta, n, sigma),
                      sampdist(g.star, theta, n, sigma), 0), col = "grey")
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta.not, n, sigma),
                      sampdist(g.star, theta.not, n, sigma), 0), col = "red")
      } else{
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta.not, n, sigma),
                      sampdist(g.star, theta.not, n, sigma), 0), col = "red")
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta, n, sigma),
                      sampdist(g.star, theta, n, sigma), 0), col = "grey")
      }
      
      # legend
      power <- round(norm_samp_min_pwrfunc_less(theta, alpha, sigma, theta.not, n), 3)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = sampdist(g.star, theta.not, n, sigma), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 4)
    }
    
    if(alternative == 'Greater than'){
      
      # sampling distribution functions
      # sampling distribution functions
      cdf <- function(x, y, theta, n, sigma = sigma){
        1 - (1 - pnorm(x, theta, sigma))^n - y
      }
      sampdist <- function(x, theta, n, sigma = sigma){
        n * (1 - pnorm(x, theta, sigma))^(n-1) * dnorm(x, theta, sigma)
      }
      
      # crit val
      g.star <- qnorm(1 - alpha^(1/n), theta.not, sigma)
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .9999, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .9999, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n, sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n, sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n, sigma), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n, sigma), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")
      
      # polygons
      if(norm_samp_max_pwrfunc_greater(theta, alpha, sigma, theta.not, n) >= alpha){
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta, n, sigma), sampdist(alt.curve$x[which(alt.curve$x > g.star)], theta, n, sigma)),
                col = "grey")
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > g.star)], theta.not, n, sigma)),
                col = "red")
      } else{
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > g.star)], theta.not, n, sigma)),
                col = "red")
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta, n, sigma), sampdist(alt.curve$x[which(alt.curve$x > g.star)], theta, n, sigma)),
                col = "grey")
      }
      
      # legend
      power <- round(norm_samp_min_pwrfunc_greater(theta, alpha, sigma, theta.not, n), 3)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = sampdist(g.star, theta.not, n, sigma), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 2)
    }
    
  } 
  
  if(statistic == "Sample Maximum"){
    
    if(alternative == 'Not equal to'){
      
      k1 <- qnorm((alpha/2)^(1/n), theta.not, sigma)
      k2 <- qnorm((1 - alpha/2)^(1/n), theta.not, sigma)
      power <- 1 - norm_max_cdf(k2, theta, sigma, n) + norm_max_cdf(k1, theta, sigma, n)
      upper.power <- power - norm_max_cdf(k1, theta, sigma, n)
      lower.power <- norm_max_cdf(k1, theta, sigma, n)
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n, sigma = sigma){
        (pnorm(x, theta, sigma))^n - y
      }
      sampdist <- function(x, theta, n, sigma = sigma){
        n* (pnorm(x, theta, sigma))^(n-1) * dnorm(x, theta, sigma)
      }
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .9999, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .9999, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n, sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n, sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(n)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n, sigma), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n, sigma), add = T, lty = 2, n = 1000))
      
      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      if(lower.power < alpha/2){
        polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n, sigma), 
                      sampdist(k1, theta.not, n, sigma), 0),
                col = "red")
        polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta, n, sigma), 
                      sampdist(k1, theta, n, sigma), 0),
                col = "grey")
        text(x = k1, y = sampdist(k1, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
        
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                y = c(0, sampdist(k2, theta, n, sigma),
                      sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n, sigma)),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                y = c(0, sampdist(k2, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > k2)], theta.not, n, sigma)),
                col = "red")
        text(x = k2, y = sampdist(k2, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
      } else{
        polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta, n, sigma), 
                      sampdist(k1, theta, n, sigma), 0),
                col = "grey")
        polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                y = c(sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n, sigma), 
                      sampdist(k1, theta.not, n, sigma), 0),
                col = "red")
        text(x = k1, y = sampdist(k1, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
        
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                y = c(0, sampdist(k2, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > k2)], theta.not, n, sigma)),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                y = c(0, sampdist(k2, theta, n, sigma),
                      sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n, sigma)),
                col = "grey")
        text(x = k2, y = sampdist(k2, theta.not, n, sigma), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
        
      }
      
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      
      
      
    } 
    
    if(alternative == 'Less than'){
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n, sigma = sigma){
        (pnorm(x, theta, sigma))^n - y
      }
      sampdist <- function(x, theta, n, sigma = sigma){
        n* (pnorm(x, theta, sigma))^(n-1) * dnorm(x, theta, sigma)
      }
      
      # crit val
      g.star <- qnorm(alpha^(1/n), theta.not, sigma)
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .9999, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .9999, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))

      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n, sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n, sigma)))
      upper.y <- max(c(nullmax, altmax))

      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(n)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n, sigma), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n, sigma), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")
      
      # polygons
      if(norm_samp_max_pwrfunc_less(theta, alpha, sigma, theta.not, n) >= alpha){
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta, n, sigma),
                      sampdist(g.star, theta, n, sigma), 0), col = "grey")
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta.not, n, sigma),
                      sampdist(g.star, theta.not, n, sigma), 0), col = "red")
      } else{
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta.not, n, sigma),
                      sampdist(g.star, theta.not, n, sigma), 0), col = "red")
        polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                y = c(sampdist(alt.curve$x[which(alt.curve$x < g.star)], theta, n, sigma),
                      sampdist(g.star, theta, n, sigma), 0), col = "grey")
      }
      
      # legend
      power <- round(norm_samp_max_pwrfunc_less(theta, alpha, sigma, theta.not, n), 3)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = sampdist(g.star, theta.not, n, sigma), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 4)
    }
    
    if(alternative == 'Greater than'){
      
      # sampling distribution functions
      cdf <- function(x, y, theta, n, sigma = sigma){
        (pnorm(x, theta, sigma))^n - y
      }
      sampdist <- function(x, theta, n, sigma = sigma){
        n* (pnorm(x, theta, sigma))^(n-1) * dnorm(x, theta, sigma)
      }
      
      # crit val
      g.star <- qnorm((1 - alpha)^(1/n), theta.not, sigma)
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .9999, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .9999, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, sigma = sigma, lower = theta - 5*n*sigma, upper = theta + 5*n*sigma, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, sigma = sigma, lower = theta.not - 5*n*sigma, upper = theta.not + 5*n*sigma, extendInt = "yes")$root
      ))
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n, sigma)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n, sigma)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(n)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist(x, theta, n, sigma), add = T, n = 1000))
      (null.curve <- curve(sampdist(x, theta.not, n, sigma), add = T, lty = 2, n = 1000))
      abline(v = g.star, lty = 4, col = "red")
      
      # polygons
      if(norm_samp_max_pwrfunc_greater(theta, alpha, sigma, theta.not, n) >= alpha){
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta, n, sigma), sampdist(alt.curve$x[which(alt.curve$x > g.star)], theta, n, sigma)),
                col = "grey")
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > g.star)], theta.not, n, sigma)),
                col = "red")
      } else{
        polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta.not, n, sigma),
                      sampdist(null.curve$x[which(null.curve$x > g.star)], theta.not, n, sigma)),
                col = "red")
        polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                y = c(0, sampdist(g.star, theta, n, sigma), sampdist(alt.curve$x[which(alt.curve$x > g.star)], theta, n, sigma)),
                col = "grey")
      }
      
      # legend
      power <- round(norm_samp_max_pwrfunc_greater(theta, alpha, sigma, theta.not, n), 3)
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = g.star, y = sampdist(g.star, theta.not, n, sigma), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 2)
    }
  }
    
}

# uniform
unif.samp <- function(statistic, alternative, theta, theta.not, n, alpha, norm.approx = F){
  
  if(statistic == "sum"){
    
    if(norm.approx){
      
      if(alternative == 'Not equal to'){
        k1 <- qnorm(alpha/2, n * theta.not / 2, sqrt(n * theta.not^2 / 12))
        k2 <- qnorm(1 - alpha/2, n * theta.not / 2, sqrt(n * theta.not^2 / 12))
        power <- 1 - pnorm(k2, n*theta/2, sqrt(n * theta^2 / 12)) + pnorm(k1, n*theta/2, sqrt(n * theta^2 / 12))
        upper.power <- power - pnorm(k1, n*theta/2, sqrt(n * theta^2 / 12))
        lower.power <- pnorm(k1, n*theta/2, sqrt(n * theta^2 / 12))
        
        
        # plotting limit
        lower.x <- max(
          0, 
          min(c(
            n*theta.not/2 - 3*sqrt(theta.not^2 * n / 12),
            n*theta/2 - 3*sqrt(theta^2 * n /12)
          ))
        )
        upper.x <- max(c(
          n*theta.not/2 + 3*sqrt(theta.not^2 * n / 12),
          n*theta/2 + 3*sqrt(theta^2 * n /12)
        ))
        
        nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, n*theta.not/2, sqrt(n * theta.not^2/12))))
        altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, n*theta/2, sqrt(n*theta^2/12))))
        upper.y <- max(c(nullmax, altmax))
        
        # plot
        title <- bquote(atop("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2)), 
                             "based on a normal approximation"))
        plot(1, type = "n", las = 1,
             xlim = c(lower.x, upper.x),
             ylim = c(0, upper.y),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = title)
        
        (alt.curve <- curve(dnorm(x, n*theta/2, sqrt(n*theta^2/12)), add = T, n = 1000))
        (null.curve <- curve(dnorm(x, n*theta.not/2, sqrt(n*theta.not^2/12)), add = T, lty = 2, n = 1000))
        
        abline(v = k1, lty = 4, col = "red")
        abline(v = k1, lty = 4, col = "red")
        
        
        
        
        
        
        
        
        
        
        if(lower.power < alpha/2){
          polygon(x = c(lower.x, lower.x, null.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x < k1)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
          polygon(x = c(lower.x, lower.x, alt.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x < k1)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
          text(x = k1, y = dnorm(k1, n*theta.not/2, sqrt(n*theta.not^2/12)), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
          
          polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], upper.x),
                  y = c(0, dnorm(k2, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x > k2)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
          polygon(x = c(k2, k2, null.curve$x[which(alt.curve$x > k2)], upper.x),
                  y = c(0, dnorm(k2, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x > k2)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
          text(x = k2, y = dirwinhall(k2, n, theta.not), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
        } else{
          polygon(x = c(lower.x, lower.x, alt.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x < k1)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
          polygon(x = c(lower.x, lower.x, null.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x < k1)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
          text(x = k1, y = dnorm(k1, n*theta.not/2, sqrt(n*theta.not^2/12)), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
          
          polygon(x = c(k2, k2, null.curve$x[which(alt.curve$x > k2)], upper.x),
                  y = c(0, dnorm(k2, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x > k2)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
          polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], upper.x),
                  y = c(0, dnorm(k2, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x > k2)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
          text(x = k2, y = dirwinhall(k2, n, theta.not), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
        }
        
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        
      } 
      
      if(alternative == 'Less than'){
        
        # crit val
        k1 <- qnorm(alpha, n * theta.not / 2, sqrt(n * theta.not^2 / 12))
        
        # plotting limit
        lower.x <- max(
          0, 
          min(c(
            n*theta.not/2 - 3*sqrt(theta.not^2 * n / 12),
            n*theta/2 - 3*sqrt(theta^2 * n /12)
          ))
        )
        upper.x <- max(c(
          n*theta.not/2 + 3*sqrt(theta.not^2 * n / 12),
          n*theta/2 + 3*sqrt(theta^2 * n /12)
        ))
        
        nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, n*theta.not/2, sqrt(n * theta.not^2/12))))
        altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, n*theta/2, sqrt(n*theta^2/12))))
        upper.y <- max(c(nullmax, altmax))
        
        # plot
        title <- bquote(atop("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2)), 
                             "based on a normal approximation"))
        plot(1, type = "n", las = 1,
             xlim = c(lower.x, upper.x),
             ylim = c(0, upper.y),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = title)
        
        (alt.curve <- curve(dnorm(x, n*theta/2, sqrt(n*theta^2/12)), add = T, n = 1000))
        (null.curve <- curve(dnorm(x, n*theta.not/2, sqrt(n*theta.not^2/12)), add = T, lty = 2, n = 1000))
        abline(v = k1, lty = 4, col = "red")
        
        
        # polygons
        power <- round(unif_norm_approx_less(theta, alpha, theta.not, n), 3)
        if(power >= alpha){
          polygon(x = c(lower.x, lower.x, alt.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x < k1)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
          polygon(x = c(lower.x, lower.x, null.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x < k1)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
        } else{
          polygon(x = c(lower.x, lower.x, null.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x < k1)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
          polygon(x = c(lower.x, lower.x, alt.curve$x[which(alt.curve$x < k1)], k1),
                  y = c(0, dnorm(lower.x, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x < k1)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
        }
        
        # legend
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        text(x = k1, y = dnorm(k1, n*theta.not/2, sqrt(n * theta.not^2/12)), labels =  bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 2)
        
      }
      
      if(alternative == 'Greater than'){
        # crit val
        k1 <- qnorm(1 - alpha, n * theta.not / 2, sqrt(n * theta.not^2 / 12))
        
        # plotting limit
        lower.x <- max(
          0, 
          min(c(
            n*theta.not/2 - 3*sqrt(theta.not^2 * n / 12),
            n*theta/2 - 3*sqrt(theta^2 * n /12)
          ))
        )
        upper.x <- max(c(
          n*theta.not/2 + 3*sqrt(theta.not^2 * n / 12),
          n*theta/2 + 3*sqrt(theta^2 * n /12)
        ))

        nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, n*theta.not/2, sqrt(n * theta.not^2/12))))
        altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dnorm(x, n*theta/2, sqrt(n*theta^2/12))))
        upper.y <- max(c(nullmax, altmax))
        
        # plot
        title <- bquote(atop("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2)), 
                             "based on a normal approximation"))
        plot(1, type = "n", las = 1,
             xlim = c(lower.x, upper.x),
             ylim = c(0, upper.y),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = title)
        
        (alt.curve <- curve(dnorm(x, n*theta/2, sqrt(n*theta^2/12)), add = T, n = 1000))
        (null.curve <- curve(dnorm(x, n*theta.not/2, sqrt(n*theta.not^2/12)), add = T, lty = 2, n = 1000))
        abline(v = k1, lty = 4, col = "red")
        

        # polygons
        power <- round(unif_norm_approx_greater(theta, alpha, theta.not, n), 3)
        if(power >= alpha){
          polygon(x = c(k1, k1, alt.curve$x[which(alt.curve$x > k1)], upper.x),
                  y = c(0, dnorm(k1, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x > k1)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
          polygon(x = c(k1, k1, null.curve$x[which(alt.curve$x > k1)], upper.x),
                  y = c(0, dnorm(k1, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x > k1)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
        } else{
          polygon(x = c(k1, k1, null.curve$x[which(alt.curve$x > k1)], upper.x),
                  y = c(0, dnorm(k1, n*theta.not/2, sqrt(n*theta.not^2/12)), dnorm(null.curve$x[which(alt.curve$x > k1)],n*theta.not/2, sqrt(n*theta.not^2/12)), 0), col = "red")
          polygon(x = c(k1, k1, alt.curve$x[which(alt.curve$x > k1)], upper.x),
                  y = c(0, dnorm(k1, n*theta/2, sqrt(n*theta^2/12)), dnorm(alt.curve$x[which(alt.curve$x > k1)],n*theta/2, sqrt(n*theta^2/12)), 0), col = "grey")
        }

        # legend
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        text(x = k1, y = dnorm(k1, n*theta.not/2, sqrt(n * theta.not^2/12)), labels =  bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 2)
      }
      
    } else{
      
      if(alternative == 'Not equal to'){
        
        k1 <- qirwinhall(alpha/2, n, theta.not)
        k2 <- qirwinhall(1 - alpha/2, n, theta.not)
        power <- 1 - pirwinhall(q = k2, n = n, theta = theta) + pirwinhall(q = k1, n = n, theta = theta)
        upper.power <- power - pirwinhall(q = k1, n = n, theta = theta)
        lower.power <- pirwinhall(q = k1, n = n, theta = theta)
        
        # plotting limit
        upper.x <- max(c(
          qirwinhall(.999, n, theta),
          qirwinhall(.999, n, theta.not)
        ))
        
        lower.x <- min(c(
          qirwinhall(.001, n, theta),
          qirwinhall(.001, n, theta.not)
        ))
        
        nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dirwinhall(x, n, theta.not)))
        altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dirwinhall(x, n, theta)))
        upper.y <- max(c(nullmax, altmax))
        
        # plot
        plot(1, type = "n", las = 1,
             xlim = c(lower.x, upper.x),
             ylim = c(0, upper.y),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
        (alt.curve <- curve(dirwinhall(x, n, theta), add = T, n = 1000))
        (null.curve <- curve(dirwinhall(x, n, theta.not), add = T, lty = 2, n = 1000))
        
        abline(v = k1, lty = 4, col = "red")
        abline(v = k2, lty = 4, col = "red")
        
        if(lower.power < alpha/2){
          polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                  y = c(dirwinhall(null.curve$x[which(null.curve$x < k1)], n, theta.not), 
                        dirwinhall(k1, n, theta.not), 0),
                  col = "red")
          polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                  y = c(dirwinhall(null.curve$x[which(null.curve$x < k1)], n, theta), 
                        dirwinhall(k1, n, theta), 0),
                  col = "grey")
          text(x = k1, y = dirwinhall(k1, n, theta), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
          
          polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                  y = c(0, dirwinhall(k2, n, theta),
                        dirwinhall(alt.curve$x[which(alt.curve$x > k2)], n, theta)),
                  col = "grey")
          polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                  y = c(0, dirwinhall(k2, n, theta.not),
                        dirwinhall(null.curve$x[which(null.curve$x > k2)], n, theta.not)),
                  col = "red")
          text(x = k2, y = dirwinhall(k2, n, theta.not), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
        } else{
          polygon(x = c(alt.curve$x[which(alt.curve$x < k1)], k1, k1),
                  y = c(dirwinhall(null.curve$x[which(null.curve$x < k1)], n, theta), 
                        dirwinhall(k1, n, theta), 0),
                  col = "grey")
          polygon(x = c(null.curve$x[which(null.curve$x < k1)], k1, k1),
                  y = c(dirwinhall(null.curve$x[which(null.curve$x < k1)], n, theta.not), 
                        dirwinhall(k1, n, theta.not), 0),
                  col = "red")
          text(x = k1, y = dirwinhall(k1, n, theta), labels = bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
          
          polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)]),
                  y = c(0, dirwinhall(k2, n, theta.not),
                        dirwinhall(null.curve$x[which(null.curve$x > k2)], n, theta.not)),
                  col = "red")
          polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)]),
                  y = c(0, dirwinhall(k2, n, theta),
                        dirwinhall(alt.curve$x[which(alt.curve$x > k2)], n, theta)),
                  col = "grey")
          text(x = k2, y = dirwinhall(k2, n, theta.not), labels = bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 4)
        }
        
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(power, 3)))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        
      } 
      
      if(alternative == 'Less than'){
        
        # crit val
        g.star <- qirwinhall(alpha, n, theta.not)
        
        # plotting limit
        upper.x <- max(c(
          qirwinhall(.999, n, theta),
          qirwinhall(.999, n, theta.not)
        ))
        
        lower.x <- min(c(
          qirwinhall(.001, n, theta),
          qirwinhall(.001, n, theta.not)
        ))
        
        nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dirwinhall(x, n, theta.not)))
        altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dirwinhall(x, n, theta)))
        upper.y <- max(c(nullmax, altmax))
        
        # plot
        plot(1, type = "n", las = 1,
             xlim = c(lower.x, upper.x),
             ylim = c(0, upper.y),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
        (alt.curve <- curve(dirwinhall(x, n, theta), add = T, n = 1000))
        (null.curve <- curve(dirwinhall(x, n, theta.not), add = T, lty = 2, n = 1000))
        abline(v = g.star, lty = 4, col = "red")
        
        # polygons
        if(unif_sum_pwrfunc_less(theta, alpha, theta.not, n) >= alpha){
          polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(dirwinhall(alt.curve$x[which(alt.curve$x < g.star)], n, theta),
                        dirwinhall(g.star, n, theta), 0), col = "grey")
          polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(dirwinhall(alt.curve$x[which(alt.curve$x < g.star)], n, theta.not),
                        dirwinhall(g.star, n, theta.not), 0), col = "red")
        } else{
          polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(dirwinhall(alt.curve$x[which(alt.curve$x < g.star)], n, theta.not),
                        dirwinhall(g.star, n, theta.not), 0), col = "red")
          polygon(x = c(alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(dirwinhall(alt.curve$x[which(alt.curve$x < g.star)], n, theta),
                        dirwinhall(g.star, n, theta), 0), col = "grey")
        }
        
        # legend
        power <- round(unif_sum_pwrfunc_less(theta, alpha, theta.not, n), 3)
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        text(x = g.star, y = dirwinhall(g.star, n, theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 4)
        
      }
      
      if(alternative == 'Greater than'){
        # crit val
        g.star <- qirwinhall(1 - alpha, n, theta.not)
        
        # plotting limit
        upper.x <- max(c(
          qirwinhall(.999, n, theta),
          qirwinhall(.999, n, theta.not)
        ))
        
        lower.x <- min(c(
          qirwinhall(.001, n, theta),
          qirwinhall(.001, n, theta.not)
        ))
        
        nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dirwinhall(x, n, theta.not)))
        altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) dirwinhall(x, n, theta)))
        upper.y <- max(c(nullmax, altmax))
        
        # plot
        plot(1, type = "n", las = 1,
             xlim = c(lower.x, upper.x),
             ylim = c(0, upper.y),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
        (alt.curve <- curve(dirwinhall(x, n, theta), add = T, n = 1000))
        (null.curve <- curve(dirwinhall(x, n, theta.not), add = T, lty = 2, n = 1000))
        abline(v = g.star, lty = 4, col = "red")
        
        # polygons
        if(unif_sum_pwrfunc_greater(theta, alpha, theta.not, n) >= alpha){
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], upper.x),
                  y = c(0, dirwinhall(g.star, n, theta), dirwinhall(alt.curve$x[which(alt.curve$x > g.star)], n, theta), 0), col = "grey")
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                  y = c(0, dirwinhall(g.star, n, theta.not), dirwinhall(alt.curve$x[which(alt.curve$x > g.star)], n, theta.not)), col = "red")
        } else{
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], upper.x),
                  y = c(0, dirwinhall(g.star, n, theta.not), dirwinhall(alt.curve$x[which(alt.curve$x > g.star)], n, theta.not), 0), col = "red")
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)]),
                  y = c(0, dirwinhall(g.star, n, theta), dirwinhall(alt.curve$x[which(alt.curve$x > g.star)], n, theta)), col = "grey")
        }
        
        # legend
        power <- round(unif_sum_pwrfunc_greater(theta, alpha, theta.not, n), 3)
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        text(x = g.star, y = dirwinhall(g.star, n, theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 2)
      }
      
    }
    
  }
  
  if(statistic == "Sample Minimum"){
    
    if(alternative == 'Not equal to'){
      
      # crit val
      k1 <- theta.not * (1 - (1 - alpha/2)^(1/n))
      k2 <- theta.not * (1 - (alpha/2)^(1/n))
      power <- round(unif_samp_min_pwrfunc_noteqto(theta, alpha, theta.not, n), 3)
      lower.power <- round(unif_samp_min_pwrfunc_less(theta, alpha/2, theta.not, n), 3)
      upper.power <- round(unif_samp_min_pwrfunc_greater(theta, alpha/2, theta.not, n), 3)
      
      # sampling distribution functions
      cdf <- Vectorize(function(x, y, theta, n){
        if(x <= theta){
          1 - (1 - x/theta)^n - y
        } else{
          1
        }
      })
      sampdist.vec <- Vectorize(function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (n/theta)*(1 - x/theta)^(n-1)
        } else{
          0
        }
      }, SIMPLIFY = TRUE)
      sampdist <- function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (n/theta)*(1 - x/theta)^(n-1)
        } else{
          0
        }
      }
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .999, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .999, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      # upper.x <- min(c(theta, upper.x))
      lower.x <- min(c(
        uniroot(cdf, y = .001, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .001, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist.vec(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist.vec(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      # polygons
      if(lower.power <= alpha/2){
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")
        
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta, n), sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n), 0),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x > k2)], theta.not, n), sampdist(upper.x, theta.not, n)),
                col = "red")
        
        text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 4)
        text(x = k2, y = sampdist(k2, theta.not, n), labels =  bquote('crit val'~'='~.(round(k2, 4))), col = "red", pos = 2)

      } else{
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta, n), sampdist.vec(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x > k2)], theta.not, n), 0),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta, n), sampdist.vec(alt.curve$x[which(alt.curve$x > k2)], theta, n), 0),
                col = "grey")
        
        text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 4)
        text(x = k2, y = sampdist(k2, theta.not, n), labels =  bquote('crit val'~'='~.(round(k2, 4))), col = "red", pos = 2)

      }
      
      # legend
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
    }
    
    if(alternative == 'Less than'){
      
      # crit val
      k1 <- theta.not * (1 - (1 - alpha)^(1/n))
      power <- round(unif_samp_min_pwrfunc_less(theta, alpha, theta.not, n), 3)
      
      # sampling distribution functions
      cdf <- Vectorize(function(x, y, theta, n){
        if(x <= theta){
          1 - (1 - x/theta)^n - y
        } else{
          1
        }
      })
      sampdist.vec <- Vectorize(function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (n/theta)*(1 - x/theta)^(n-1)
        } else{
          0
        }
      }, SIMPLIFY = TRUE)
      sampdist <- function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (n/theta)*(1 - x/theta)^(n-1)
        } else{
          0
        }
      }
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .999, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .999, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      # upper.x <- min(c(theta, upper.x))
      lower.x <- min(c(
        uniroot(cdf, y = .001, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .001, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      # upper.x <- 6
      # lower.x <- 0
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist.vec(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist.vec(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = k1, lty = 4, col = "red")
      
      # polygons
      if(power >= alpha){
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")

      } else{
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")

      }

      # legend
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 4)
      
    }
    
    if(alternative == 'Greater than'){
      
      # crit val
      k1 <- theta.not * (1 - alpha^(1/n))
      power <- round(unif_samp_min_pwrfunc_greater(theta, alpha, theta.not, n), 3)
      
      # sampling distribution functions
      cdf <- Vectorize(function(x, y, theta, n){
        if(x <= theta){
          1 - (1 - x/theta)^n - y
        } else{
          1
        }
      })
      sampdist.vec <- Vectorize(function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (n/theta)*(1 - x/theta)^(n-1)
        } else{
          0
        }
      }, SIMPLIFY = TRUE)
      sampdist <- function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (n/theta)*(1 - x/theta)^(n-1)
        } else{
          0
        }
      }
      
      # plotting limit
      upper.x <- max(c(
        uniroot(cdf, y = .999, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .999, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      # upper.x <- min(c(theta, upper.x))
      lower.x <- min(c(
        uniroot(cdf, y = .001, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .001, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      
      nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n)))
      altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist.vec(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist.vec(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = k1, lty = 4, col = "red")

      # polygons
      if(power >= alpha){
        polygon(x = c(k1, k1, alt.curve$x[which(alt.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta, n), sampdist(alt.curve$x[which(alt.curve$x > k1)], theta, n), 0),
                col = "grey")
        polygon(x = c(k1, k1, null.curve$x[which(null.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x > k1)], theta.not, n), sampdist(upper.x, theta.not, n)),
                col = "red")
      } else{
        polygon(x = c(k1, k1, null.curve$x[which(null.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta.not, n),
                      sampdist(null.curve$x[which(null.curve$x > k1)], theta.not, n), 0),
                col = "red")
        polygon(x = c(k1, k1, alt.curve$x[which(alt.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta, n), sampdist.vec(alt.curve$x[which(alt.curve$x > k1)], theta, n), 0),
                col = "grey")
      }
      # 
      # # legend
      legend("topright",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 4))), col = "red", pos = 2)
      
    }
    
  }
  
  if(statistic == "Sample Maximum"){
    
    if(alternative == 'Not equal to'){
      
      # crit val
      k1 <- theta.not * (alpha/2)^(1/n)
      k2 <- theta.not * (1 - (alpha/2))^(1/n)
      power <- round(unif_samp_max_pwrfunc_noteqto(theta, alpha, theta.not, n), 3)
      lower.power <- round(unif_samp_max_pwrfunc_less(theta, alpha/2, theta.not, n), 3)
      upper.power <- round(unif_samp_max_pwrfunc_greater(theta, alpha/2, theta.not, n), 3)
      
      # sampling distribution functions
      cdf <- Vectorize(function(x, y, theta, n){
        if(x < theta){
          (x/theta)^n - y
        } else{
          1 - y
        }
      })
      sampdist.vec <- Vectorize(function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (1/theta)^n * n * x^(n-1)
        } else{
          0
        }
      }, SIMPLIFY = TRUE)
      sampdist <- function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (1/theta)^n * n * x^(n-1)
        } else{
          0
        }
      }
      
      # plotting limit
      # upper.x <- max(c(
      #   uniroot(cdf, y = .9999, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
      #   uniroot(cdf, y = .9999, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      # ))
      upper.x <- max(c(theta, theta.not))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      
      # nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n)))
      # altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(sampdist(theta, theta, n), sampdist(theta.not, theta.not, n)))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist.vec(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist.vec(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = k1, lty = 4, col = "red")
      abline(v = k2, lty = 4, col = "red")
      
      # polygons
      if(lower.power <= alpha/2){
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")
        
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta, n), sampdist(alt.curve$x[which(alt.curve$x > k2)], theta, n), 0),
                col = "grey")
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x > k2)], theta.not, n), sampdist(upper.x, theta.not, n)),
                col = "red")
        
        text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
        text(x = k2, y = sampdist(k2, theta.not, n), labels =  bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 2)
        
      } else{
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta, n), sampdist.vec(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        
        polygon(x = c(k2, k2, null.curve$x[which(null.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x > k2)], theta.not, n), 0),
                col = "red")
        polygon(x = c(k2, k2, alt.curve$x[which(alt.curve$x > k2)], upper.x),
                y = c(0, sampdist(k2, theta, n), sampdist.vec(alt.curve$x[which(alt.curve$x > k2)], theta, n), 0),
                col = "grey")
        
        text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
        text(x = k2, y = sampdist(k2, theta.not, n), labels =  bquote('crit val'~'='~.(round(k2, 2))), col = "red", pos = 2)
        
      }
      
      # legend
      legend("topleft",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
    }
    
    if(alternative == 'Less than'){
      
      # crit val
      k1 <- theta.not * (alpha)^(1/n)
      power <- round(unif_samp_max_pwrfunc_less(theta, alpha, theta.not, n), 3)
      
      # sampling distribution functions
      cdf <- Vectorize(function(x, y, theta, n){
        if(x < theta){
          (x/theta)^n - y
        } else{
          1 - y
        }
      })
      sampdist.vec <- Vectorize(function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (1/theta)^n * n * x^(n-1)
        } else{
          0
        }
      }, SIMPLIFY = TRUE)
      sampdist <- function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (1/theta)^n * n * x^(n-1)
        } else{
          0
        }
      }
      
      # plotting limit
      # upper.x <- max(c(
      #   uniroot(cdf, y = .9999, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
      #   uniroot(cdf, y = .9999, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      # ))
      upper.x <- max(c(theta + .05, theta.not + .05))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))

      nullmax <- sampdist(theta.not, theta.not, n)
      altmax <- sampdist(theta, theta, n)
      upper.y <- max(c(nullmax, altmax))
      
      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist.vec(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist.vec(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = k1, lty = 4, col = "red")

      # polygons
      if(power >= alpha){
        upr <- min(k1, theta)
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < upr)], upr),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < upr)], theta, n), 0),
                col = "grey")
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")

      } else{
        polygon(x = c(0, 0, null.curve$x[which(null.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta.not, n), sampdist(null.curve$x[which(null.curve$x < k1)], theta.not, n), 0),
                col = "red")
        polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < k1)], k1),
                y = c(0, sampdist(0, theta, n), sampdist(alt.curve$x[which(alt.curve$x < k1)], theta, n), 0),
                col = "grey")

      }

      # legend
      legend("topleft",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 4)
      
    }
    
    if(alternative == 'Greater than'){
      
      # crit val
      k1 <- theta.not * (1 - alpha)^(1/n)
      power <- round(unif_samp_max_pwrfunc_greater(theta, alpha, theta.not, n), 3)
      
      # sampling distribution functions
      cdf <- Vectorize(function(x, y, theta, n){
        if(x < theta){
          (x/theta)^n - y
        } else{
          1 - y
        }
      })
      sampdist.vec <- Vectorize(function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (1/theta)^n * n * x^(n-1)
        } else{
          0
        }
      }, SIMPLIFY = TRUE)
      sampdist <- function(x, theta, n){
        # theta <- theta[length(theta)]
        if(x <= theta){
          (1/theta)^n * n * x^(n-1)
        } else{
          0
        }
      }
      
      # plotting limit
      # upper.x <- max(c(
      #   uniroot(cdf, y = .9999, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
      #   uniroot(cdf, y = .9999, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      # ))
      upper.x <- max(c(theta, theta.not))
      lower.x <- min(c(
        uniroot(cdf, y = .0001, theta = theta, n = n, lower = theta - 5*n, upper = theta + 5*n, extendInt = "yes")$root,
        uniroot(cdf, y = .0001, theta = theta.not, n = n, lower = theta.not - 5*n, upper = theta.not + 5*n, extendInt = "yes")$root
      ))
      
      # nullmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta.not, n)))
      # altmax <- max(sapply(seq(from = lower.x, to = upper.x, length.out = 500), FUN = function(x) sampdist(x, theta, n)))
      upper.y <- max(c(sampdist(theta, theta, n), sampdist(theta.not, theta.not, n)))

      # plot
      plot(1, type = "n", las = 1,
           xlim = c(lower.x, upper.x),
           ylim = c(0, upper.y),
           xlab = "T(x)",
           ylab = bquote(f[X[(1)]](x)),
           main = bquote("Sampling Distribution of T(X) ="~X[(n)]~"for"~theta~"="~.(round(theta, 2))~"and"~theta[0]~"="~.(round(theta.not,2))))
      (alt.curve <- curve(sampdist.vec(x, theta, n), add = T, n = 1000))
      (null.curve <- curve(sampdist.vec(x, theta.not, n), add = T, lty = 2, n = 1000))
      abline(v = k1, lty = 4, col = "red")

      # polygons
      if(power >= alpha){
        
        polygon(x = c(k1, k1, alt.curve$x[which(alt.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta, n), sampdist(alt.curve$x[which(alt.curve$x > k1)], theta, n), 0),
                col = "grey")
        polygon(x = c(k1, k1, null.curve$x[which(null.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta.not, n), sampdist.vec(null.curve$x[which(null.curve$x > k1)], theta.not, n), 0),
                col = "red")
      } else{
        polygon(x = c(k1, k1, null.curve$x[which(null.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta.not, n),
                      sampdist(null.curve$x[which(null.curve$x > k1)], theta.not, n), 0),
                col = "red")
        polygon(x = c(k1, k1, alt.curve$x[which(alt.curve$x > k1)], upper.x),
                y = c(0, sampdist(k1, theta, n), sampdist.vec(alt.curve$x[which(alt.curve$x > k1)], theta, n), 0),
                col = "grey")
      }

      # legend
      legend("topleft",
             legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
             lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
      text(x = k1, y = sampdist(k1, theta.not, n), labels =  bquote('crit val'~'='~.(round(k1, 2))), col = "red", pos = 2)
      
    }
    
  }
}






