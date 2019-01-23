library(shiny)

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
})

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
  
  tmp <- uniroot(pirwinhall_zero, n = n, theta = theta, p = p, lower = 0, upper = n*theta, tol = .000000001)
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


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Dr. Jenny G's Power App"),
  
  # Widgets
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:23.5%;height: 90vh; overflow-y: auto;",
      selectInput(
        inputId = "distribution",
        label = h3("Population Distribution"),
        choices = c("Exponential", "Normal", "Uniform")
      ),
      selectInput(
        inputId = "statistic",
        label = h3("Test Statistic"),
        choices = c("Sum of the X's", "Sample Minimum", "Sample Maximum")
      ),
      numericInput(
        inputId = "theta.not",
        label = h3("Null Value"),
        value = 3
      ),
      conditionalPanel(
        condition = "input.distribution == 'Normal'",
        numericInput(
          inputId = "sigma",
          label = h3("Population Standard Deviation"),
          value = 1,
          min = 0
        )
      ),
      selectInput(
        inputId = "alternative",
        label = h3("Alternative Hypothesis"),
        choices = c("Greater than", "Less than", "Not equal to")
      ),
      numericInput(
        inputId = "alpha",
        label = h3("Alpha Level"),
        value = .05,
        min = 0,
        max = 1,
        step = 0.01
      ),
      numericInput(
        inputId = "sample.size",
        label = h3("Sample Size"),
        value = 25,
        step = 10,
        min = 0
      ),
      # conditionalPanel(
      #   condition = "typeof input.plot_click != 'undefined'",
      #   numericInput(
      #     inputId = "theta",
      #     label = h3("Theta"),
      #     value = NULL,
      #     step = 1,
      #     min = 0
      #   )
      # )
      numericInput(
        inputId = 'theta',
        label = h3("Theta"),
        value = NULL,
        step = 0.01
      )
    ),
    
    # Meat of the app
    mainPanel(
      withMathJax(),
      h1("Introduction"),
      p("Hello! This application is meant to help visualize power curves, sampling distributions, and the relationship
        between the two. It is meant to accompany the mean annual loss due to theft example in the notes. In this example,
        we consider a random sample from an Exponential(", HTML('&theta;'), ") distribution. The panel to the left will allow 
        you to explore the power curve for each of two different statistics, as well as how that curve changes based on 
        significance level, sample size and null value."),
      br(),
      p("This application will also allow you to explore how power is related to the sampling distribution of a test 
        statistic under the null and alternative hypotheses. To visualize these distributions for a specific value of",
        HTML('&theta;,'), "simply click the power curve at the desired value of", HTML('&theta;.'), "Have fun!"),
      br(),
      h3("Power Curve"),
      p(""),
      plotOutput(outputId = "powerPlot",
                 click = "plot_click",
                 hover = hoverOpts(id = "plot.hover", delay = 0, delayType = "debounce")
      ),
      br(),
      
      #add sampling distribution text for exponential
      conditionalPanel(
        condition = "input.distribution == 'Exponential'",
        h3("Sampling Distributions"),
        p("It can be shown that if \\(X_{i}\\) \\(\\overset{\\scriptscriptstyle \\text{iid}}{\\sim}\\) \\(Exp(\\)\\(\\theta)\\), 
        then \\(\\Sigma(X_{i})\\) ~ \\(Gamma(n, \\theta)\\) and \\(X_{(1)}\\) ~ \\(Exp(\\)\\(\\frac{\\theta}{n})\\). 
        These results are used to plot the sampling distributions for each statistic both under the null hypothesis and
        the true value of", HTML('&theta;.'), "The red area corresponds to the significance level and the gray area corresponds
        to the power; note the relationship between the sampling distribution under", HTML('&theta;<sub>0</sub>,'), "sampling
        distribution under", HTML('&theta;,') ,"significance level and power!"),
        conditionalPanel(condition = "typeof input.plot_click != 'undefined'",
                         plotOutput(outputId = "sampDist")
                         )
      )
      )
      )
      )

# Define server logic
server <- function(input, output, session) {
  
  #initialize reactive values
  val <- reactiveValues(theta = NULL)
  observeEvent(input$plot_click, {
    val$theta <- input$plot_click$x
    updateNumericInput(session, "theta", value = round(input$plot_click$x, 2))
  })
  observeEvent(input$theta, {
    val$theta <- input$theta
  })
  
  
  
    #Produce First Graph of Power Function
    output$powerPlot <- renderPlot({
      
      ################################
      ### EXPONENTIAL DISTRIBUTION ###
      ################################
      
      #sum, greater than
      if(input$distribution == "Exponential" & input$statistic == "Sum of the X's" & input$alternative == "Greater than" & input$theta.not > 0){
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 5*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
        curve(1 - pchisq(input$theta.not*qchisq(1 - input$alpha, 2*input$sample.size)/x, 2*input$sample.size), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = val$theta, y = 1 - pchisq(input$theta.not*qchisq(1 - input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size), pch = 16)
        abline(v = val$theta, col  = "gray")
        abline(h = 1 - pchisq(input$theta.not*qchisq(1 - input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(1 - pchisq(input$theta.not*qchisq(1 - input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size),3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      #sum, less than 
      if(input$distribution == "Exponential" & input$statistic == "Sum of the X's" & input$alternative == "Less than" & input$theta.not > 0){
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 3*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
        curve(pchisq(input$theta.not*qchisq(input$alpha, 2*input$sample.size)/x, 2*input$sample.size), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = val$theta, y = pchisq(input$theta.not*qchisq(input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size), pch = 16)
        abline(v = val$theta, col  = "gray")
        abline(h = pchisq(input$theta.not*qchisq(input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size), col  = "gray")
        if(!is.na(val$theta)){
          legend("topright",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(pchisq(input$theta.not*qchisq(input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size),3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #sum, not equal
      if(input$distribution == "Exponential" & input$statistic == "Sum of the X's" & input$alternative == "Not equal to" & input$theta.not > 0){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha

        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 3*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
        curve(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/x) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/x), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
                points(x = theta, y = 1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/theta) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/theta), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = 1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/theta) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/theta), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")),
                            bquote(theta~"="~.(round(val$theta,2))),
                            bquote(beta(theta)~"="~ .(round(1 - pgamma(qgamma(1 - alpha/2, n, 1/theta.not), n, 1/theta) + pgamma(qgamma(alpha/2, n, 1/theta.not), n, 1/theta),3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      

      #min, greater
      if(input$distribution == "Exponential" & input$statistic == "Sample Minimum" & input$alternative == "Greater than" & input$theta.not > 0){
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 5*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
        curve(exp(input$theta.not*log(input$alpha)/x), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = val$theta, y = exp(input$theta.not*log(input$alpha)/val$theta), pch = 16)
        abline(v = val$theta, col  = "gray")
        abline(h = exp(input$theta.not*log(input$alpha)/val$theta), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(exp(input$theta.not*log(input$alpha)/val$theta),3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #min, less
      if(input$distribution == "Exponential" & input$statistic == "Sample Minimum" & input$alternative == "Less than" & input$theta.not > 0){
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001,5*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
        curve(1 - exp(input$theta.not*log(1 - input$alpha)/x), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = val$theta, y = 1 - exp(input$theta.not*log(1 - input$alpha)/val$theta), pch = 16)
        abline(v = val$theta, col  = "gray")
        abline(h = 1 - exp(input$theta.not*log(1 - input$alpha)/val$theta), col  = "gray")
        if(!is.na(val$theta)){
          legend("topright",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(1 - exp(input$theta.not*log(1 - input$alpha)/val$theta),3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #min, not equal
      if(input$distribution == "Exponential" & input$statistic == "Sample Minimum" & input$alternative == "Not equal to" & input$theta.not > 0){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha

        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 3*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~~X[(1)]), las = 1)
        curve(1 - pexp(qexp(1 - alpha/2, n/theta.not), n/x) + pexp(qexp(alpha/2, n/theta.not), n/x), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")

        points(x = theta, y = 1 - pexp(qexp(1 - alpha/2, n/theta.not), n/theta) + pexp(qexp(alpha/2, n/theta.not), n/theta), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = 1 - pexp(qexp(1 - alpha/2, n/theta.not), n/theta) + pexp(qexp(alpha/2, n/theta.not), n/theta), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")),
                            bquote(theta~"="~.(round(val$theta,2))),
                            bquote(beta(theta)~"="~ .(round(1 - pexp(qexp(1 - alpha/2, n/theta.not), n/theta) + pexp(qexp(alpha/2, n/theta.not), n/theta),3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #max, greater than
      if(input$distribution == "Exponential" & input$statistic == "Sample Maximum" & input$alternative == "Greater than" & input$theta.not > 0){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 5*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
        
        curve(exp_samp_max_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = exp_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = exp(input$theta.not*log(input$alpha)/val$theta), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(exp_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #max, less than
      if(input$distribution == "Exponential" & input$statistic == "Sample Maximum" & input$alternative == "Less than" & input$theta.not > 0){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 5*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
        
        curve(exp_samp_max_pwrfunc_less(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = exp_samp_max_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = exp(input$theta.not*log(input$alpha)/val$theta), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(exp_samp_max_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #max, not equal
      if(input$distribution == "Exponential" & input$statistic == "Sample Maximum" & input$alternative == "Not equal to" & input$theta.not > 0){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, 5*input$theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
        
        curve(exp_samp_max_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = exp_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = exp(input$theta.not*log(input$alpha)/val$theta), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(exp_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }

      #make sure assumptions arent violated
      if(input$distribution == "Exponential" & input$theta.not <= 0) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        text(x = .5, y = .5, "Theta must be greater than 0!", col = "red")
      }
      
      if(input$distribution == "Exponential" & input$sample.size <= 0) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        text(x = .5, y = .5, "The sample size must be greater than 0!", col = "red")
      }
      
      if(input$distribution == "Exponential" & input$alpha <= 0 | input$alpha >= 1) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        text(x = .5, y = .5, "Alpha must be in (0,1)!", col = "red")
      }
      
      if(input$distribution == "Exponential" & input$sample.size %% 1 != 0) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        text(x = .5, y = .5, "The sample size must be an integer!", col = "red")
      }
      
      
      ###########################
      ### NORMAL DISTRIBUTION ###
      ###########################
      
      #sum of x's, greater than
      if(input$distribution == "Normal" & input$statistic == "Sum of the X's" & input$alternative == "Greater than"){
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(input$theta.not - 3*input$sigma, input$theta.not + 3*input$sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
        curve(1 - pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - x)*sqrt(input$sample.size))/input$sigma), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = val$theta, y = 1 - pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - val$theta)*sqrt(input$sample.size))/input$sigma), pch = 16)
        abline(v = val$theta, col  = "gray")
        abline(h = 1 - pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - val$theta)*sqrt(input$sample.size))/input$sigma), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(1 - pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - val$theta)*sqrt(input$sample.size))/input$sigma), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #sum of x's, less than
      if(input$distribution == "Normal" & input$statistic == "Sum of the X's" & input$alternative == "Less than"){
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(input$theta.not - 3*input$sigma, input$theta.not + 3*input$sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
        curve(pnorm(qnorm(input$alpha) + (input$theta.not - x)*sqrt(input$sample.size)/input$sigma), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = val$theta, y = pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - val$theta)*sqrt(input$sample.size))/input$sigma), pch = 16)
        abline(v = val$theta, col  = "gray")
        abline(h = pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - val$theta)*sqrt(input$sample.size))/input$sigma), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - val$theta)*sqrt(input$sample.size))/input$sigma), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #sum, not equal
      if(input$distribution == "Normal" & input$statistic == "Sum of the X's" & input$alternative == "Not equal to"){
         plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(input$theta.not - 3*input$sigma, input$theta.not + 3*input$sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
        curve(1 - pnorm(qnorm(1 - input$alpha/2) - (input$theta.not - x)*sqrt(input$sample.size)/input$sigma) + pnorm(-qnorm(1 - input$alpha/2) - (input$theta.not - x)*sqrt(input$sample.size)/input$sigma), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = val$theta, y = 1 - pnorm(qnorm(1 - input$alpha/2) - (input$theta.not - val$theta)*sqrt(input$sample.size)/input$sigma) + pnorm(-qnorm(1 - input$alpha/2) - (input$theta.not - val$theta)*sqrt(input$sample.size)/input$sigma), pch = 16)
        abline(v = val$theta, col  = "gray")
        abline(h = 1 - pnorm(qnorm(1 - input$alpha/2) - (input$theta.not - val$theta)*sqrt(input$sample.size)/input$sigma) + pnorm(-qnorm(1 - input$alpha/2) - (input$theta.not - val$theta)*sqrt(input$sample.size)/input$sigma), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(1 - pnorm(qnorm(1 - input$alpha/2) - (input$theta.not - val$theta)*sqrt(input$sample.size)/input$sigma) + pnorm(-qnorm(1 - input$alpha/2) - (input$theta.not - val$theta)*sqrt(input$sample.size)/input$sigma), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #min, greater than
      if(input$distribution == "Normal" & input$statistic == "Sample Minimum" & input$alternative == "Greater than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        sigma <- input$sigma
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(theta.not - 3*sigma, theta.not + 3*sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
        
        curve(norm_samp_min_pwrfunc_greater(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = norm_samp_min_pwrfunc_greater(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = norm_samp_min_pwrfunc_greater(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(norm_samp_min_pwrfunc_greater(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #min, less than
      if(input$distribution == "Normal" & input$statistic == "Sample Minimum" & input$alternative == "Less than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        sigma <- input$sigma
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(theta.not - 3*sigma, theta.not + 3*sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
        
        curve(norm_samp_min_pwrfunc_less(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = norm_samp_min_pwrfunc_less(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = norm_samp_min_pwrfunc_less(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(norm_samp_min_pwrfunc_less(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #min, not equal
      if(input$distribution == "Normal" & input$statistic == "Sample Minimum" & input$alternative == "Not equal to"){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        sigma <- input$sigma
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(theta.not - 3*sigma, theta.not + 3*sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
        
        curve(norm_samp_min_pwrfunc_noteqto(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = norm_samp_min_pwrfunc_noteqto(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = norm_samp_min_pwrfunc_noteqto(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(norm_samp_min_pwrfunc_noteqto(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #max, greater than
      if(input$distribution == "Normal" & input$statistic == "Sample Maximum" & input$alternative == "Greater than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        sigma <- input$sigma
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(theta.not - 3*sigma, theta.not + 3*sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
        
        curve(norm_samp_max_pwrfunc_greater(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = norm_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = norm_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(norm_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #max, less than
      if(input$distribution == "Normal" & input$statistic == "Sample Maximum" & input$alternative == "Less than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        sigma <- input$sigma
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(theta.not - 3*sigma, theta.not + 3*sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
        
        curve(norm_samp_max_pwrfunc_less(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = norm_samp_max_pwrfunc_less(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = norm_samp_max_pwrfunc_less(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(norm_samp_max_pwrfunc_less(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #max, not equal
      if(input$distribution == "Normal" & input$statistic == "Sample Maximum" & input$alternative == "Not equal to"){
        n <- input$sample.size
        theta.not <- input$theta.not
        theta <- val$theta
        alpha <- input$alpha
        sigma <- input$sigma
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(theta.not - 3*sigma, theta.not + 3*sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
        
        curve(norm_samp_max_pwrfunc_noteqto(theta = x, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = norm_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = norm_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(norm_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, sigma = sigma, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }

      
      ############################
      ### UNIFORM DISTRIBUTION ###
      ############################
      
      #sum, greater than
      if(input$distribution == "Uniform" & input$statistic == "Sum of the X's" & input$alternative == "Greater than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
        
        curve(unif_sum_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_sum_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_sum_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")

        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_sum_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #sum, less than
      if(input$distribution == "Uniform" & input$statistic == "Sum of the X's" & input$alternative == "Less than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
        
        curve(unif_sum_pwrfunc_less(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_sum_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_sum_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_sum_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #sum, not equal
      if(input$distribution == "Uniform" & input$statistic == "Sum of the X's" & input$alternative == "Not equal to"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
        
        curve(unif_sum_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_sum_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_sum_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_sum_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #min, greater than
      if(input$distribution == "Uniform" & input$statistic == "Sample Minimum" & input$alternative == "Greater than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(1)']), las = 1)
        
        curve(unif_samp_min_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_samp_min_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_samp_min_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #min, less than
      if(input$distribution == "Uniform" & input$statistic == "Sample Minimum" & input$alternative == "Less than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(1)']), las = 1)
        
        curve(unif_samp_min_pwrfunc_less(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_samp_min_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_samp_max_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_samp_min_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #min, not equal
      if(input$distribution == "Uniform" & input$statistic == "Sample Minimum" & input$alternative == "Not equal to"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(1)']), las = 1)
        
        curve(unif_samp_min_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_samp_min_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_samp_min_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_samp_min_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }
      
      #max, greater than
      if(input$distribution == "Uniform" & input$statistic == "Sample Maximum" & input$alternative == "Greater than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta

        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(n)']), las = 1)
        
        curve(unif_samp_max_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")

        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #max, less than
      if(input$distribution == "Uniform" & input$statistic == "Sample Maximum" & input$alternative == "Less than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta

        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(n)']), las = 1)
        
        curve(unif_samp_max_pwrfunc_less(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_samp_max_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_samp_max_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")

        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_samp_max_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #max, not equal 
      if(input$distribution == "Uniform" & input$statistic == "Sample Maximum" & input$alternative == "Not equal to"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(n)']), las = 1)
        
        curve(unif_samp_max_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.null(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }
      
    })

  
  #Plot the sampling distribution for chosen theta
  output$sampDist <- renderPlot({
    if(is.null(val$theta)){
      plot(1, type = "n")
    }
    
    ################################
    ### EXPONENTIAL DISTRIBUTION ###
    ################################

    if(input$distribution == "Exponential"){
      if(input$statistic == "Sum of the X's" & input$alternative == "Greater than" & input$theta.not >= 0 & input$sample.size >= 0 & input$alpha >= 0 & input$alpha <= 1) {
        #validate(need(!is.null(val$theta), message = "Please select a point on the power curve above to visualize the sampling distribution of the chosen statistic for a particular value of theta."))
        validate(need(!is.null(val$theta), message = ""))
        plot(1, type = "n", las = 1,
             xlim = c(0.001, max(c(2*input$sample.size*val$theta, 2*input$sample.size*input$theta.not))),
             ylim = c(0, max(
               c(max(dgamma(seq(0, 2*input$sample.size*val$theta), input$sample.size, 1 / val$theta)),
                 max(dgamma(seq(0, 2*input$sample.size*input$theta.not), input$sample.size, 1 / input$theta.not)))
             )),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(val$theta, 2))~"and"~theta[0]~"="~.(round(input$theta.not,2))))
        (alt.curve <- curve(dgamma(x, input$sample.size, 1 / val$theta), add = T, n = 1000))
        (null.curve <- curve(dgamma(x, input$sample.size, 1 / input$theta.not), add = T, lty = 2, n = 1000))
        g.star <- qgamma(input$alpha, input$sample.size, 1 / input$theta.not, lower.tail = F)
        abline(v = g.star, lty = 4, col = "red")
        if(1 - pchisq(input$theta.not*qchisq(1 - input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size) >= input$alpha ) {
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                  y = c(0, dgamma(g.star, input$sample.size, 1 / val$theta),
                        dgamma(alt.curve$x[which(alt.curve$x > g.star)], input$sample.size, 1/val$theta), 0),
                  col = "grey")
          polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                  y = c(0, dgamma(g.star, input$sample.size, 1 / input$theta.not),
                        dgamma(null.curve$x[which(null.curve$x > g.star)], input$sample.size, 1/input$theta.not), 0),
                  col = "red")
        } else {
          polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                  y = c(0, dgamma(g.star, input$sample.size, 1 / input$theta.not),
                        dgamma(null.curve$x[which(null.curve$x > g.star)], input$sample.size, 1/input$theta.not), 0),
                  col = "red")
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                  y = c(0, dgamma(g.star, input$sample.size, 1 / val$theta),
                        dgamma(alt.curve$x[which(alt.curve$x > g.star)], input$sample.size, 1/val$theta), 0),
                  col = "grey")
        }
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(1 - pchisq(input$theta.not*qchisq(1 - input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size), 2)))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        #text(x = 100, y = .001, labels = paste("Power = ", dgamma(val$theta, input$sample.size, 1 / val$theta)))
        text(x = g.star, y = dgamma(g.star, input$sample.size, 1 / input$theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 2)
      }
      
      if(input$statistic == "Sum of the X's" & input$alternative == "Less than" & input$theta.not >= 0 & input$sample.size >= 0 & input$alpha >= 0 & input$alpha <= 1){
        validate(need(!is.null(val$theta), message = "Please select a point on the power curve above to visualize the sampling distribution of the chosen statistic for a particular value of theta."))
        plot(1, type = "n", las = 1,
             xlim = c(0.001, max(c(2*input$sample.size*val$theta, 2*input$sample.size*input$theta.not))),
             ylim = c(0, max(
               c(max(dgamma(seq(0, 2*input$sample.size*val$theta), input$sample.size, 1 / val$theta)),
                 max(dgamma(seq(0, 2*input$sample.size*input$theta.not), input$sample.size, 1 / input$theta.not)))
             )),
             xlab = "T(x)",
             ylab = bquote(f[Sigma(X[i])](x)),
             main = bquote("Sampling Distribution of T(X) ="~Sigma(X[i])~"for"~theta~"="~.(round(val$theta, 2))~"and"~theta[0]~"="~.(round(input$theta.not,2))))
        (alt.curve <- curve(dgamma(x, input$sample.size, 1 / val$theta), add = T, n = 1000))
        (null.curve <- curve(dgamma(x, input$sample.size, 1 / input$theta.not), add = T, lty = 2, n = 1000))
        g.star <- qgamma(input$alpha, input$sample.size, 1 / input$theta.not, lower.tail = T)
        abline(v = g.star, lty = 4, col = "red")
        if(pchisq(input$theta.not*qchisq(input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size) >= input$alpha ) {
          polygon(x = c(g.star, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(0, dgamma(alt.curve$x[which(alt.curve$x < g.star)], input$sample.size, 1/val$theta),
                        dgamma(g.star, input$sample.size, 1 / val$theta), 0),
                  col = "grey")
          polygon(x = c(g.star, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                  y = c(0, dgamma(null.curve$x[which(null.curve$x < g.star)], input$sample.size, 1/input$theta.not),
                        dgamma(g.star, input$sample.size, 1 / input$theta.not), 0),
                  col = "red")
        } else {
          polygon(x = c(g.star, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                  y = c(0, dgamma(null.curve$x[which(null.curve$x < g.star)], input$sample.size, 1/input$theta.not),
                        dgamma(g.star, input$sample.size, 1 / input$theta.not), 0),
                  col = "red")
          polygon(x = c(g.star, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(0, dgamma(alt.curve$x[which(alt.curve$x < g.star)], input$sample.size, 1/val$theta),
                        dgamma(g.star, input$sample.size, 1 / val$theta), 0),
                  col = "grey")
        }
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(pchisq(input$theta.not*qchisq(input$alpha, 2*input$sample.size)/val$theta, 2*input$sample.size), 2)))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        #text(x = 100, y = .001, labels = paste("Power = ", dgamma(val$theta, input$sample.size, 1 / val$theta)))
        text(x = g.star, y = dgamma(g.star, input$sample.size, 1 / input$theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 4)
      }
      
      if(input$statistic == "Sample Minimum" & input$alternative == "Greater than" & input$theta.not >= 0 & input$sample.size >= 0 & input$alpha >= 0 & input$alpha <= 1){
        validate(need(!is.null(val$theta), message = "Please select a point on the power curve above to visualize the sampling distribution of the chosen statistic for a particular value of theta."))
        plot(1, type = "n", las = 1,
             xlim = c(0.001, max(c(5*val$theta/input$sample.size, 5*input$theta.not/input$sample.size))), #5 times gives at least 96.5% of sampling dist by Chebychevs
             ylim = c(0, max(
               c(max(dexp(seq(0, 5*val$theta/input$sample.size), input$sample.size / val$theta)),
                 max(dexp(seq(0, 5*input$theta.not/input$sample.size), input$sample.size / input$theta.not)))
             )),
             xlab = "T(x)",
             ylab = bquote(f[X[(1)]](x)),
             main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(val$theta, 2))~"and"~theta[0]~"="~.(round(input$theta.not,2))))
        (alt.curve <- curve(dexp(x, input$sample.size / val$theta), add = T, n = 1000))
        (null.curve <- curve(dexp(x, input$sample.size / input$theta.not), add = T, lty = 2, n = 1000))
        g.star <- qexp(input$alpha, input$sample.size / input$theta.not, lower.tail = F)
        abline(v = g.star, lty = 4, col = "red")
        if(exp(input$theta.not*log(input$alpha)/val$theta) >= input$alpha ) {
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                  y = c(0, dexp(g.star, input$sample.size/ val$theta),
                        dexp(alt.curve$x[which(alt.curve$x > g.star)], input$sample.size /val$theta), 0),
                  col = "grey")
          polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                  y = c(0, dexp(g.star, input$sample.size / input$theta.not),
                        dexp(null.curve$x[which(null.curve$x > g.star)], input$sample.size / input$theta.not), 0),
                  col = "red")
        } else {
          polygon(x = c(g.star, g.star, null.curve$x[which(null.curve$x > g.star)], g.star),
                  y = c(0, dexp(g.star, input$sample.size / input$theta.not),
                        dexp(null.curve$x[which(null.curve$x > g.star)], input$sample.size / input$theta.not), 0),
                  col = "red")
          polygon(x = c(g.star, g.star, alt.curve$x[which(alt.curve$x > g.star)], g.star),
                  y = c(0, dexp(g.star, input$sample.size/ val$theta),
                        dexp(alt.curve$x[which(alt.curve$x > g.star)], input$sample.size /val$theta), 0),
                  col = "grey")
        }
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(exp(input$theta.not*log(input$alpha)/val$theta), 2)))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        
        text(x = g.star, y = dexp(g.star, input$sample.size / input$theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 2)
      }
      
      if(input$statistic == "Sample Minimum" & input$alternative == "Less than" & input$theta.not >= 0 & input$sample.size >= 0 & input$alpha >= 0 & input$alpha <= 1){
        validate(need(!is.null(val$theta), message = "Please select a point on the power curve above to visualize the sampling distribution of the chosen statistic for a particular value of theta."))
        plot(1, type = "n", las = 1,
             xlim = c(0.001, max(c(5*val$theta/input$sample.size, 5*input$theta.not/input$sample.size))), #5 times gives at least 96% of sampling dist by Chebychevs
             ylim = c(0, max(
               c(max(dexp(seq(0, 5*val$theta/input$sample.size), input$sample.size / val$theta)),
                 max(dexp(seq(0, 5*input$theta.not/input$sample.size), input$sample.size / input$theta.not)))
             )),
             xlab = "T(x)",
             ylab = bquote(f[X[(1)]](x)),
             main = bquote("Sampling Distribution of T(X) ="~X[(1)]~"for"~theta~"="~.(round(val$theta, 2))~"and"~theta[0]~"="~.(round(input$theta.not,2))))
        (alt.curve <- curve(dexp(x, input$sample.size / val$theta), add = T, n = 1000))
        (null.curve <- curve(dexp(x, input$sample.size / input$theta.not), add = T, lty = 2, n = 1000))
        g.star <- qexp(input$alpha, input$sample.size / input$theta.not, lower.tail = T)
        abline(v = g.star, lty = 4, col = "red")
        
        if(1 - exp(input$theta.not*log(1 - input$alpha)/val$theta) >= input$alpha ) {
          polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(0, dexp(0, input$sample.size/ val$theta), dexp(alt.curve$x[which(alt.curve$x < g.star)], input$sample.size /val$theta),
                        dexp(g.star, input$sample.size/ val$theta), 0), col = "grey")
          polygon(x = c(0, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                  y = c(0, dexp(null.curve$x[which(null.curve$x < g.star)], input$sample.size / input$theta.not),
                        dexp(g.star, input$sample.size / input$theta.not), 0), col = "red")
        } else {
          polygon(x = c(0, null.curve$x[which(null.curve$x < g.star)], g.star, g.star),
                  y = c(0, dexp(null.curve$x[which(null.curve$x < g.star)], input$sample.size / input$theta.not),
                        dexp(g.star, input$sample.size / input$theta.not), 0), col = "red")
          polygon(x = c(0, 0, alt.curve$x[which(alt.curve$x < g.star)], g.star, g.star),
                  y = c(0, dexp(0, input$sample.size/ val$theta), dexp(alt.curve$x[which(alt.curve$x < g.star)], input$sample.size /val$theta),
                        dexp(g.star, input$sample.size/ val$theta), 0), col = "grey")
        }
        legend("topright",
               legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(round(1 - exp(input$theta.not*log(1 - input$alpha)/val$theta), 2)))),
               lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        text(x = g.star, y = dgamma(g.star, input$sample.size, 1 / input$theta.not), labels =  bquote('crit val'~'='~.(round(g.star, 2))), col = "red", pos = 4)
      }
      
      if(input$theta.not <= 0) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        #text(x = .5, y = .5, "Theta must be greater than 0!", col = "red")
      }
      
      if(input$sample.size <= 0) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        #text(x = .5, y = .5, "The sample size must be greater than 0!", col = "red")
      }
      
      if(input$alpha <= 0 | input$alpha >= 1) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        #text(x = .5, y = .5, "Alpha must be in (0,1)!", col = "red")
      }
      
      if(input$sample.size %% 1 != 0) {
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      }
    }
    

    ###########################
    ### NORMAL DISTRIBUTION ###
    ###########################
    
    if(input$distribution == "Normal"){
      NULL
    }
    
    ############################
    ### UNIFORM DISTRIBUTION ###
    ############################
    
    if(input$distribution == "Uniform"){
      NULL
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = T))

