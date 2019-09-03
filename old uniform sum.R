if(statistic == "Sum of the X's"){
  
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