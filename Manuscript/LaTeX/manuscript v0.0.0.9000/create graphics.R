################
### FIGURE 4 ### - height of 800, maintain aspect ratio
################
par(mfrow = c(3,1))
alpha <- 0.05
n <- 25
theta.not <- 3
sigma <- 1

# exp max
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
mtext("for samples from the Exponential distribution", cex = .75)
curve(exp_samp_max_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal max
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(norm_samp_max_pwrfunc_greater(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# uniform max
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0, 6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(n)']), las = 1)
mtext(bquote("for samples from the Uniform distribution"), cex = .75)
curve(unif_samp_max_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

################
### FIGURE 5 ### - 1200 x 800
################
par(mfrow = c(3,2))
alpha <- 0.05
n <- 25
theta.not <- 3
sigma <- 1

# uniform sum
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
mtext(bquote("for samples from the Uniform distribution"), cex = .75)
curve(unif_sum_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal sum
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(1 - pnorm(qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma) + pnorm(-qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# uniform min
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(1)']), las = 1)
mtext(bquote("for samples from the Uniform distribution"), cex = .75)
curve(unif_samp_min_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal min
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(norm_samp_min_pwrfunc_noteqto(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# uniform max
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0, 6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(n)']), las = 1)
mtext(bquote("for samples from the Uniform distribution"), cex = .75)
curve(unif_samp_max_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal max
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(norm_samp_max_pwrfunc_noteqto(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

################
### FIGURE 6 ###
################
par(mfrow = c(3,1))
alpha <- 0.05
n <- 25
theta.not <- 3

# exp greater
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
mtext("for samples from the Exponential distribution", cex = .75)
curve(1 - pchisq(theta.not*qchisq(1 - alpha, 2*n)/x, 2*n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# exp less
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(pchisq(theta.not*qchisq(alpha, 2*n)/x, 2*n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# exp not equal to
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0, 6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")


################
### FIGURE 8 ### - 1200 x 800
################
par(mfcol = c(3,2))
n <- 25
sigma <- 1

theta.not <- 3
alpha <- 0.05
# uniform sum
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal sum
alpha <- 0.15
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# uniform min
alpha <- 0.50
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(1)']), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

theta.not <- 6
# normal min
alpha <- 0.05
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,12), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# uniform max
alpha <- 0.15
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0, 12), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(n)']), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal max
alpha <- 0.50
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,12), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
mtext(bquote("for samples from the Exponential distribution"), cex = .75)
curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

################
### FIGURE 9 ### - 1200 x 800
################
par(mfcol = c(3,2))
alpha <- 0.05
theta.not <- 3
sigma <- 1

n <- 10
# uniform sum
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(1 - pnorm(qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma) + pnorm(-qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

n <- 50
# normal sum
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(1 - pnorm(qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma) + pnorm(-qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

n <- 500
# uniform min
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(1 - pnorm(qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma) + pnorm(-qnorm(1 - alpha/2) - (theta.not - x)*sqrt(n)/sigma), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal min
n <- 10
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(norm_samp_min_pwrfunc_noteqto(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# uniform max
n <- 50
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0, 6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(norm_samp_min_pwrfunc_noteqto(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

# normal max
n <- 500
plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
     xlim = c(0,6), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
mtext(bquote("for samples from the Normal distribution"), cex = .75)
curve(norm_samp_min_pwrfunc_noteqto(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
abline(h = alpha, lty = 2, col = "red")

