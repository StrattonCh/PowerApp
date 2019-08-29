library(shiny)
# library(powerapp)
source("helpers.R")
# devtools::install_github('StrattonCh/powerapppackage')

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("The Power of Sampling Distributions"),
  
  # Widgets
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:23.5%;height: 90vh; overflow-y: auto;",
      
      conditionalPanel(
        "input.tabselected == 1",
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
            min = 0)
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
        numericInput(
          inputId = 'theta',
          label = h3("Theta"),
          value = NULL,
          step = 0.01
        )
      ),
      conditionalPanel(
        "input.tabselected == 2",
        selectInput(
          inputId = "dist2",
          label = h3("Population Distribution"),
          choices = c("Exponential" = 'exp',
                      "Normal" = 'norm', 
                      "Uniform" = 'unif')
        ),
        selectInput(
          inputId = "stat2",
          label = h3("Test Statistic"),
          choices = c("Sum of the X's" = 'sum', 
                      "Sample Minimum" = 'min',
                      "Sample Maximum" = 'max')
        )
      )
    ),
    
    # Meat of the app
    mainPanel(
      tabsetPanel(
        id = "tabselected",
        type = "tabs",
        tabPanel("Plots", value = 1,
                 withMathJax(),
                 h2("Introduction"),
                 p("Hello! This application is meant to help visualize power curves, sampling distributions, and the relationship
        between the two. The panel to the left will allow you to explore the power curve for each of three different population distributions,
        alternative hypotheses, and test statistics. Furthermore, you may investigate how those curves depend on 
        the significance level, sample size and null value."),
                 br(),
                 p("This application will also allow you to explore how power is related to the sampling distribution of a test 
        statistic under the null hypothesis and true value of",  HTML('&theta;.'), "To visualize these distributions for a specific value of",
                   HTML('&theta;,'), "simply click the power curve at the desired value of", HTML('&theta;, or type the value in the panel.'), "Have fun!"),
                 br(),
                 h3("Power Curve"),
                 conditionalPanel(
                   condition = "input.distribution == 'Exponential'",
                   h4("Exponential Distribution")
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'Normal'",
                   h4("Normal Distribution")
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'Uniform'",
                   h4("Uniform Distribution")
                 ),
                 p(""),
                 plotOutput(outputId = "powerPlot",
                            click = "plot_click",
                            hover = hoverOpts(id = "plot.hover", delay = 0, delayType = "debounce")
                 ),
                 br(),
                 
                 #add sampling distribution text for exponential
                 h3('Sampling Distribution'),
                 conditionalPanel(
                   condition = "input.distribution == 'Exponential'",
                   p("Below are the sampling distributions for chosen statistic under both the null hypothesis and
        the true value of", HTML('&theta;.'), "The red area corresponds to the significance level and the gray area corresponds
        to the power; note the relationship between the sampling distribution under", HTML('&theta;<sub>0</sub>,'), "sampling
        distribution under", HTML('&theta;,') ,"significance level and power!")
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'Normal'",
                   p("Below are the sampling distributions for chosen statistic under both the null hypothesis and
        the true value of", HTML('&theta;.'), "The red area corresponds to the significance level and the gray area corresponds
        to the power; note the relationship between the sampling distribution under", HTML('&theta;<sub>0</sub>,'), "sampling
        distribution under", HTML('&theta;,') ,"significance level and power!")
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'Uniform'",
                   p("Below are the sampling distributions for chosen statistic under both the null hypothesis and
        the true value of", HTML('&theta;.'), "The red area corresponds to the significance level and the gray area corresponds
        to the power; note the relationship between the sampling distribution under", HTML('&theta;<sub>0</sub>,'), "sampling
        distribution under", HTML('&theta;,') ,"significance level and power!")
                 ),
                 plotOutput(outputId = "sampDist")
        ),
        tabPanel("Derivations", value = 2,
                 tags$div(HTML("<script> type='text/x-mathjax-config'> MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]} });</script>")),
                 # # p(includeHTML("exp_sum.txt"))
                 withMathJax(uiOutput('deriv2'))
                 )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  #initialize reactive values
  val <- reactiveValues()
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
        abline(h = exp_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
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
        abline(h = exp_samp_max_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
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
        abline(h = exp_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(exp_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
    
    #make sure assumptions arent violated
    if(input$distribution == "Exponential" & input$theta.not <= 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The null value must be greater than 0!", col = "red")
    }
    
    if(input$distribution == "Exponential" & val$theta <= 0 & !is.na(val$theta)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Theta must be greater than 0! If you have previously clicked a negative value of theta, click this plot.", col = "red")
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
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(1 - pnorm(qnorm(1 - input$alpha, mean = 0, sd = 1) + ((input$theta.not - val$theta)*sqrt(input$sample.size))/input$sigma), 2)))),
                 pch = c(NA,NA), bty = "n")
        }
      }
      
      #sum of x's, less than
      if(input$distribution == "Normal" & input$statistic == "Sum of the X's" & input$alternative == "Less than"){
        n <- input$sample.size
        alpha <- input$alpha
        theta.not <- input$theta.not
        theta <- val$theta[length(val$theta)]
        sigma <- input$sigma
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(input$theta.not - 3*input$sigma, input$theta.not + 3*input$sigma), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
        curve(pnorm(qnorm(input$alpha) + (input$theta.not - x)*sqrt(input$sample.size)/input$sigma), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        points(x = theta, y = pnorm(qnorm(input$alpha) + (theta.not - theta)*sqrt(input$sample.size)/input$sigma), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = pnorm(qnorm(input$alpha) + (theta.not - theta)*sqrt(input$sample.size)/input$sigma), col  = "gray")
        if(!is.na(theta)){
          power <- pnorm(qnorm(input$alpha) + (input$theta.not - val$theta[length(val$theta)])*sqrt(input$sample.size)/input$sigma)
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(val$theta,2))), bquote(beta(theta)~"="~ .(round(power, 2)))),
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
    
    #make sure assumptions arent violated
    if(input$distribution == "Normal" & input$sigma <= 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Sigma must be greater 0!", col = "red")
    }

    if(input$distribution == "Normal" & input$sample.size <= 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be greater than 0!", col = "red")
    }
    
    if(input$distribution == "Normal" & input$alpha <= 0 | input$alpha >= 1) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Alpha must be in (0,1)!", col = "red")
    }
    
    if(input$distribution == "Normal" & input$sample.size %% 1 != 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be an integer!", col = "red")
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

        if(!is.na(val$theta)){
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
        
        if(!is.na(val$theta)){
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
        
        if(!is.na(val$theta)){
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
        abline(h = unif_samp_min_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.na(val$theta)){
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
        abline(h = unif_samp_min_pwrfunc_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.na(val$theta)){
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
        
        if(!is.na(val$theta)){
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
        theta <- val$theta[length(val$theta)]

        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0, 2*theta.not), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(n)']), las = 1)
        
        curve(unif_samp_max_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_samp_max_pwrfunc_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")

        if(!is.na(val$theta)){
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

        if(!is.na(val$theta)){
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
        
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_samp_max_pwrfunc_noteqto(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }
    
    
    #make sure assumptions arent violated
    if(input$distribution == "Uniform" & input$theta.not <= 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The null value must be greater than 0!", col = "red")
    }
    
    if(input$distribution == "Uniform" & val$theta <= 0 & !is.na(val$theta)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Theta must be greater than 0! If you have previously clicked a negative value of theta, click this plot.", col = "red")
    }
    
    if(input$distribution == "Uniform" & input$sample.size <= 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be greater than 0!", col = "red")
    }
    
    if(input$distribution == "Uniform" & input$alpha <= 0 | input$alpha >= 1) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Alpha must be in (0,1)!", col = "red")
    }
    
    if(input$distribution == "Uniform" & input$sample.size %% 1 != 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be an integer!", col = "red")
    }
    
    
    
      
    })

  
  #Plot the sampling distribution for chosen theta
  output$sampDist <- renderPlot({
    
    validate(need(val$theta, 'Click a point on the power plot above to visualize the sampling distribution!'))
    
    ################################
    ### EXPONENTIAL DISTRIBUTION ###
    ################################
    
    if(input$distribution == "Exponential"){
      
      if(val$theta <= 0 | input$alpha < 0 | input$alpha > 1 | input$sample.size <= 0 | input$theta.not <= 0){
        stop(message('Please specify appropriate parameter, alpha, and sample size values.'))
      }
      
      exp.samp(statistic = input$statistic, alternative = input$alternative, theta = val$theta[length(val$theta)], 
               theta.not = input$theta.not, n = input$sample.size, alpha = input$alpha)
      
    }
    
    ###########################
    ### NORMAL DISTRIBUTION ###
    ###########################
    
    if(input$distribution == "Normal"){
      
      if(input$alpha < 0 | input$alpha > 1 | input$sample.size <= 0 | input$sigma <= 0){
        stop(message('Please specify appropriate parameter, alpha, and sample size values.'))
      }
      
      norm.samp(statistic = input$statistic, alternative = input$alternative, theta = val$theta[length(val$theta)], 
                theta.not = input$theta.not, n = input$sample.size, alpha = input$alpha, sigma = input$sigma)
    }
    
    ############################
    ### UNIFORM DISTRIBUTION ###
    ############################
    
    if(input$distribution == "Uniform"){
      
      if(val$theta <= 0 | input$alpha < 0 | input$alpha > 1 | input$sample.size <= 0 | input$theta.not <= 0){
        stop(message('Please specify appropriate parameter, alpha, and sample size values.'))
      }
      
      unif.samp(statistic = input$statistic, alternative = input$alternative, theta = val$theta[length(val$theta)],
                theta.not = input$theta.not, n = input$sample.size, alpha = input$alpha)
      
      # plot(1)
    }

    
  })
  
  output$deriv2 <- renderUI({
    withMathJax(HTML(readLines(paste0(input$dist2, '_', input$stat2, '.txt'))))
  })

}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = T))

