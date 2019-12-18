library(shiny)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
source("helpers.R")

# Define UI for application
ui <- fluidPage(
  # theme = shinytheme("superhero"),
  # Application title
  titlePanel("The Power of Sampling Distributions"),
  setBackgroundColor("white"),
  # setBackgroundColor("#708090"),
  useShinyjs(),
  
  # Widgets
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:23.5%;height: 90vh; overflow-y: auto; background-color: white;",

      conditionalPanel(
        "input.tabselected == 'plots'",
        selectInput(
          inputId = "distribution",
          label = h3("Population Distribution"),
          choices = c("Exponential", "Normal", "Uniform")
        ),
        selectInput(
          inputId = "statistic",
          label = h3("Test Statistic"),
          choices = c("Sum of the X's" = "sum", "Sample Minimum", "Sample Maximum")
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
          step = 1,
          min = 0
        ),
        numericInput(
          inputId = 'theta',
          label = h3("Theta"),
          value = NULL,
          step = 0.01
        ),
        conditionalPanel(
          condition = "input.distribution == 'Uniform' & input.statistic == 'sum'",
          checkboxInput(
            inputId = "norm_approx",
            label = "Use normal approximation",
            value = FALSE)
        )
      ),
      conditionalPanel(
        "input.tabselected == 'deriv'",
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
        ),
        selectInput(
          inputId = "alt2",
          label = h3("Alternative Hypothesis"),
          choices = c("Greater than" = 'greater',
                      "Less than" = 'lesser',
                      "Not equal to" = 'not')
        )
      ),
      conditionalPanel(
        "input.tabselected == 'approx'",
        numericInput(
          inputId = "sim.reps",
          label = h3("Number of simulated samples"),
          value = 10000,
          step = 1000,
          min = 1
        ),
        numericInput(
          inputId = "sim.n",
          label = h3("Sample Size"),
          value = 25,
          step = 1,
          min = 1
        ),
        numericInput(
          inputId = "sim.theta",
          label = h3("Theta"),
          value = 3,
          min = 0
        ),
        checkboxInput(
          inputId = "sim.addcdf",
          label = 'Overlay normal CDF',
          value = F
        )
      ),
      conditionalPanel(
        "input.tabselected == 'sim'",
        numericInput(
          inputId = "sim2.reps",
          label = h3("Number of simulated samples"),
          value = 10000,
          step = 1000,
          min = 1
        ),
        numericInput(
          inputId = "sim2.n",
          label = h3("Sample Size"),
          value = 25,
          step = 1,
          min = 1
        ),
        numericInput(
          inputId = "sim2.theta0",
          label = h3("Null Value"),
          value = 3,
          min = 0
        ),
        numericInput(
          inputId = "sim2.alpha",
          label = h3("Alpha Level"),
          value = .05,
          min = 0,
          max = 1,
          step = 0.01
        ),
        selectInput(
          inputId = "sim2.alt",
          label = h3("Alternative Hypothesis"),
          choices = c("Greater than", "Less than", "Not equal to")
        ),
        numericInput(
          inputId = "sim2.theta",
          label = h3("Theta"),
          value = 3,
          min = 0
        ),
        actionButton(
          inputId = "sim2.go",
          label = "Simulate power!"
        )
      )
    ),
    
    # Meat of the app
    mainPanel(
      tabsetPanel(
        id = "tabselected",
        type = "tabs",
        tabPanel("Plots", value = "plots",
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
                 p(""),
                 plotOutput(outputId = "powerPlot",
                            click = "plot_click",
                            hover = hoverOpts(id = "plot.hover", delay = 0, delayType = "debounce")
                 ),
                 downloadButton('powerSave', 'Download this plot'),
                 uiOutput("warning"),
                 conditionalPanel(
                   condition = "!input.norm_approx & output.divStatus & input.distribution == 'Uniform' & input.statistic == 'sum'",
                   shinyjs::hidden(div(id = "warning_text", 
                                       uiOutput("warning2"),
                                       plotOutput("warning_plot"),
                                       uiOutput("warning3"),
                                       uiOutput("warning4")
                   ))
                 ),
                 
                 #add sampling distribution text for exponential
                 h3('Sampling Distribution'),
                 conditionalPanel(
                   condition = "input.distribution == 'Exponential'",
                   p("Below are the sampling distributions for the chosen statistic under both the null hypothesis and
        the true value of", HTML('&theta;.'), "The red area corresponds to the significance level and the gray area corresponds
        to the power; note the relationship between the sampling distribution under", HTML('&theta;<sub>0</sub>,'), "sampling
        distribution under", HTML('&theta;,') ,"significance level and power!")
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'Normal'",
                   p("Below are the sampling distributions for the chosen statistic under both the null hypothesis and
        the true value of", HTML('&theta;.'), "The red area corresponds to the significance level and the gray area corresponds
        to the power; note the relationship between the sampling distribution under", HTML('&theta;<sub>0</sub>,'), "sampling
        distribution under", HTML('&theta;,') ,"significance level and power!")
                 ),
                 conditionalPanel(
                   condition = "input.distribution == 'Uniform'",
                   p("Below are the sampling distributions for the chosen statistic under both the null hypothesis and
        the true value of", HTML('&theta;.'), "The red area corresponds to the significance level and the gray area corresponds
        to the power; note the relationship between the sampling distribution under", HTML('&theta;<sub>0</sub>,'), "sampling
        distribution under", HTML('&theta;,') ,"significance level and power!")
                 ),
                 plotOutput(outputId = "sampDist"),
                 downloadButton('sampSave', 'Download this plot')
        ),
        tabPanel("Derivations", value = "deriv",
                 tags$div(HTML("<script> type='text/x-mathjax-config'> MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]} });</script>")),
                 # # p(includeHTML("exp_sum.txt"))
                 withMathJax(uiOutput('deriv2'))
        ),
        tabPanel("Irwin-Hall Normal Approximation", value = "approx",
                 p('The purpose of this tab is to examine how well the normal distribution approximates the 
                   sampling distribution of the sum of uniform random variables. To do so, we will use simulation.
                   Use the side panel to simulate sample sums from the uniform distribution and calculate the 
                   empirical distribution function for these simulated samples. Then, overlay the normal 
                   distribution function to compare them.'),
                 plotOutput(outputId = "sim_hist", height = 800)
        ),
        tabPanel("Simulated Power", value = "sim",
                 p("The purpose of this tab is to calculate and visualize power for each alternative hypothesis for the 
                   sum of uniform random variables through simulation. To do so, choose the appropriate values from
                   the side panel and click 'Simulate power!' Please note that for large numbers of simulated samples,
                   it will take time to compute."),
                 plotOutput(outputId = "sim2_power", height = 800)
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
  p <- function(){
    ################################
    ### EXPONENTIAL DISTRIBUTION ###
    ################################
    
    #sum, greater than
    if(input$distribution == "Exponential" & input$statistic == "sum" & input$alternative == "Greater than" & input$theta.not > 0){
      
      theta <- val$theta[length(val$theta)]
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
    if(input$distribution == "Exponential" & input$statistic == "sum" & input$alternative == "Less than" & input$theta.not > 0){
      
      theta <- val$theta[length(val$theta)]
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
    if(input$distribution == "Exponential" & input$statistic == "sum" & input$alternative == "Not equal to" & input$theta.not > 0){
      n <- input$sample.size
      theta.not <- input$theta.not
      theta <- val$theta[length(val$theta)]
      alpha <- input$alpha
      
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
      
      theta <- val$theta[length(val$theta)]
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
      
      theta <- val$theta[length(val$theta)]
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
      
      if(is.na(theta)) {
        xlims <- c(0.00001, 2*input$theta.not)
      } else{
        xlims <- c(0.00001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~~X[(1)]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
      
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
      
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
      
      if(is.na(theta)) {
        xlims <- c(0.001, 2*input$theta.not)
      } else{
        xlims <- c(0.001, max(2*input$theta.not, theta))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = xlims, ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
      text(x = .5, y = .5, "The null value must be greater than 0.", col = "red")
    }
    
    if(input$distribution == "Exponential" & val$theta <= 0 & !is.na(val$theta)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Theta must be greater than 0. If you have previously clicked a negative value of theta, click this plot.", col = "red")
    }
    
    if(input$distribution == "Exponential" & input$sample.size <= 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be greater than 0.", col = "red")
    }
    
    if(input$distribution == "Exponential" & (input$alpha <= 0 | input$alpha >= 1)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Alpha must be in (0,1).", col = "red")
    }
    
    if(input$distribution == "Exponential" & input$sample.size %% 1 != 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be an integer.", col = "red")
    }
    
    
    ###########################
    ### NORMAL DISTRIBUTION ###
    ###########################
    
    #sum of x's, greater than
    if(input$distribution == "Normal" & input$statistic == "sum" & input$alternative == "Greater than"){
      theta <- val$theta
      theta.not <- input$theta.not
      n <- input$sample.size
      sigma <- input$sigma
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
    if(input$distribution == "Normal" & input$statistic == "sum" & input$alternative == "Less than"){
      n <- input$sample.size
      alpha <- input$alpha
      theta.not <- input$theta.not
      theta <- val$theta[length(val$theta)]
      sigma <- input$sigma
      
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
    if(input$distribution == "Normal" & input$statistic == "sum" & input$alternative == "Not equal to"){
      
      theta <- val$theta
      theta.not <- input$theta.not
      n <- input$sample.size
      sigma <- input$sigma
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~Sigma(X[i])), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
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
      
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
      
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 0*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 0*sigma,
          theta - 0*sigma
        ))
      }
      
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
      
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[(1)]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
      
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
      
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
      
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
      
      if(is.na(theta)){
        upper.x <- max(c(
          theta.not + 3*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma
        ))
      } else{
        upper.x <- max(c(
          theta.not + 3*sigma,
          theta + 0*sigma
        ))
        lower.x <- min(c(
          theta.not - 3*sigma,
          theta - 0*sigma
        ))
      }
      
      
      plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
           xlim = c(lower.x, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X[('n')]), las = 1)
      mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
      
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
    
    if(input$distribution == "Uniform" & input$alpha <= 0 | input$alpha >= 1) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Alpha must be in (0,1)!", col = "red")
    } else if(input$distribution == "Uniform" & input$theta.not <= 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The null value must be greater than 0!", col = "red")
    } else if(input$distribution == "Uniform" & input$sample.size <= 0){
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be greater than 0!", col = "red")
    } else{
      #sum, greater than
      if(input$distribution == "Uniform" & input$statistic == "sum" & input$alternative == "Greater than" & !(input$norm_approx)){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
      
      #sum, greater than, norm approx
      if(input$distribution == "Uniform" & input$statistic == "sum" & input$alternative == "Greater than" & input$norm_approx){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            theta.not + 3* sqrt(theta.not^2 / 12)
          ))
          lower.x <- max(c(
            0, 
            theta.not - 3* sqrt(theta.not^2 / 12)
          ))
        } else{
          upper.x <- max(c(
            theta.not + 3* sqrt(theta.not^2 / 12),
            theta + 0 * sqrt(theta^2 / 12)
          ))
          lower.x <- max(c(
            0, 
            min(c(
              theta.not - 3* sqrt(theta.not^2 / 12),
              theta - 0 * sqrt(theta^2 / 12)
            ))
          ))
        }
        
        
        title <- bquote("Power Function for T(X) ="~ Sigma(X['i']) ~ "based on a normal approximation")
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(lower.x, upper.x), ylim = c(0,1), main = title, las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
        curve(expr = unif_norm_approx_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_norm_approx_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_norm_approx_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_norm_approx_greater(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #sum, less than
      if(input$distribution == "Uniform" & input$statistic == "sum" & input$alternative == "Less than" & !(input$norm_approx)){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
      
      #sum, less than, norm approx
      if(input$distribution == "Uniform" & input$statistic == "sum" & input$alternative == "Less than" & input$norm_approx){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            theta.not + 3* sqrt(theta.not^2 / 12)
          ))
          lower.x <- max(c(
            0, 
            theta.not - 3* sqrt(theta.not^2 / 12)
          ))
        } else{
          upper.x <- max(c(
            theta.not + 3* sqrt(theta.not^2 / 12),
            theta + 0 * sqrt(theta^2 / 12)
          ))
          lower.x <- max(c(
            0, 
            min(c(
              theta.not - 3* sqrt(theta.not^2 / 12),
              theta - 0 * sqrt(theta^2 / 12)
            ))
          ))
        }
        
        
        title <- bquote("Power Function for T(X) ="~ Sigma(X['i']) ~ "based on a normal approximation")
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(lower.x, upper.x), ylim = c(0,1), main = title, las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
        curve(expr = unif_norm_approx_less(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_norm_approx_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_norm_approx_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_norm_approx_less(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
        
      }  
      
      #sum, not equal
      if(input$distribution == "Uniform" & input$statistic == "sum" & input$alternative == "Not equal to" & !(input$norm_approx)){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ Sigma(X['i'])), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
      
      #sum, not equal, norm approx
      if(input$distribution == "Uniform" & input$statistic == "sum" & input$alternative == "Not equal to" & input$norm_approx){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            theta.not + 3* sqrt(theta.not^2 / 12)
          ))
          lower.x <- max(c(
            0, 
            theta.not - 3* sqrt(theta.not^2 / 12)
          ))
        } else{
          upper.x <- max(c(
            theta.not + 3* sqrt(theta.not^2 / 12),
            theta + 0* sqrt(theta^2 / 12)
          ))
          lower.x <- max(c(
            0, 
            min(c(
              theta.not - 3* sqrt(theta.not^2 / 12),
              theta - 0* sqrt(theta^2 / 12)
            ))
          ))
        }
        
        
        title <- bquote("Power Function for T(X) ="~ Sigma(X['i']) ~ "based on a normal approximation")
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(lower.x, upper.x), ylim = c(0,1), main = title, las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
        curve(expr = unif_norm_approx_noteq(theta = x, alpha = alpha, theta_not = theta.not, n = n), add = T, n = 1000)
        abline(h = input$alpha, lty = 2, col = "red")
        
        points(x = theta, y = unif_norm_approx_noteq(theta = theta, alpha = alpha, theta_not = theta.not, n = n), pch = 16)
        abline(v = theta, col  = "gray")
        abline(h = unif_norm_approx_noteq(theta = theta, alpha = alpha, theta_not = theta.not, n = n), col  = "gray")
        
        if(!is.na(val$theta)){
          legend("topleft",
                 legend = c(expression(paste("Click Info")), bquote(theta~"="~.(round(theta,2))), bquote(beta(theta)~"="~ .(round(unif_norm_approx_noteq(theta = theta, alpha = alpha, theta_not = theta.not, n = n), 3)))),
                 pch = c(NA,NA), bty = "n")
        }
        
      }  
      
      #min, greater than
      if(input$distribution == "Uniform" & input$statistic == "Sample Minimum" & input$alternative == "Greater than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(1)']), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(1)']), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(1)']), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(n)']), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~ X['(n)']), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        
        
        plot(1, type = "n", xlab = expression(theta), ylab = expression(beta(theta)),
             xlim = c(0.001, upper.x), ylim = c(0,1), main = bquote("Power Function for T(X) ="~X['(n)']), las = 1)
        mtext(bquote("for samples from the" ~ .(input$distribution) ~ 'distribution'))
        
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
      
    }
    
    #make sure assumptions arent violated
    if(input$distribution == "Uniform" & val$theta <= 0 & !is.na(val$theta)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "Theta must be greater than 0! If you have previously clicked a negative value of theta, click this plot.", col = "red")
    }
    
    if(input$distribution == "Uniform" & input$sample.size %% 1 != 0) {
      plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
      text(x = .5, y = .5, "The sample size must be an integer!", col = "red")
    }
    
  }
  
  output$powerPlot <- renderPlot({
    p()
  })
  
  # download plot
  output$powerSave <- downloadHandler(
    filename = function() {paste0(input$distribution, '_', input$statistic, '_power_', Sys.time(), '.png')},
    content = function(file) {
      grDevices::png(file = file, width = 1918, height = 800, pointsize = 22)
      p()
      dev.off()
    }
  )

  #Plot the sampling distribution for chosen theta
  p2 <- function(){
    
    validate(need(val$theta, 'Click a point on the power plot above to visualize the sampling distribution!'))
    
    ################################
    ### EXPONENTIAL DISTRIBUTION ###
    ################################
    
    if(input$distribution == "Exponential"){
      
      if(val$theta <= 0 | input$alpha <= 0 | input$alpha >= 1 | input$sample.size <= 0 | input$theta.not <= 0 | input$sample.size %% 1 != 0){
        
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        text(x = .5, y = .5, "Please specify appropriate parameter, alpha, and sample size values.", col = "red")
        
        # stop(message('Please specify appropriate parameter, alpha, and sample size values.'))
      } else{
        
        exp.samp(statistic = input$statistic, alternative = input$alternative, theta = val$theta[length(val$theta)], 
                 theta.not = input$theta.not, n = input$sample.size, alpha = input$alpha, distribution = input$distribution)
        
      }
      
      
    }
    
    ###########################
    ### NORMAL DISTRIBUTION ###
    ###########################
    
    if(input$distribution == "Normal"){
      
      if(input$alpha <= 0 | input$alpha >= 1 | input$sample.size <= 0 | input$sample.size %% 1 != 0 | input$sigma <= 0){
        
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        text(x = .5, y = .5, "Please specify appropriate parameter, alpha, and sample size values.", col = "red")
        
        # stop(message('Please specify appropriate parameter, alpha, and sample size values.'))
      } else{
        
        norm.samp(statistic = input$statistic, alternative = input$alternative, theta = val$theta[length(val$theta)], 
                  theta.not = input$theta.not, n = input$sample.size, alpha = input$alpha, sigma = input$sigma, distribution = input$distribution)
        
      }
      
    }
    
    ############################
    ### UNIFORM DISTRIBUTION ###
    ############################
    
    if(input$distribution == "Uniform"){
      
      if(val$theta <= 0 | input$alpha <= 0 | input$alpha >= 1 | input$sample.size <= 0 | input$theta.not <= 0 | input$sample.size %% 1 != 0){
        plot(1, type = "n", xlab = "", ylab = "", main = "", xlim = c(0,1), ylim = c(0,1), axes = F)
        text(x = .5, y = .5, "Please specify appropriate parameter, alpha, and sample size values.", col = "red")
      } else{
        unif.samp(statistic = input$statistic, alternative = input$alternative, theta = val$theta[length(val$theta)],
                  theta.not = input$theta.not, n = input$sample.size, alpha = input$alpha, norm.approx = input$norm_approx, distribution = input$distribution)
      }
      
    }
    
    
  }
  output$sampDist <- renderPlot({
    p2()
  })
  
  # download plot
  output$sampSave <- downloadHandler(
    filename = function() {paste0(input$distribution, '_', input$statistic, '_samp_', Sys.time(), '.png')},
    content = function(file) {
      png(file = file, width = 1918, height = 800, pointsize = 22)
      p2()
      dev.off()
    }
  )
  
  output$deriv2 <- renderUI({
    withMathJax(HTML(readLines(paste0(input$dist2, '_', input$stat2, '_', input$alt2, '.txt'))))
  })
  
  
  # logical flag to denote numeric instability
  output$warning <- renderUI(
    if(input$distribution == "Uniform" & input$statistic == "sum" & !(input$norm_approx)){
      
      # message <- paste0("Warning: due to numerical instability in the Irwin-Hall distribution function, this power function
      #     (and the resulting sampling distributions) may exhibit some strange behavior or fail to plot.
      #     This problem is exacerbated and more noticable as the sample size increases; see Alberto (2019) for
      #     greater detail. For even moderately large sample sizes, we recommend using the Central Limit Theorem
      #     to approximate the sampling distribution and power curve. To do so, check the 'Use normal approximation'
      #     box in the side panel. For sample sizes of even four or greater, the Central Limit Theorem provides a
      #     reasonably good approximation to the sampling distribution. Use the Irwin-Hall Normal Approximation tab
      #     to further investigate this relationship.")
      if(rv$clicks %% 2 == 1){
        txt <- "Click here to hide warning information."
      } else{
        txt <- "Warning: Something is wrong! Click here to learn more."
      }
      
      if(input$alternative == "Greater than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        tmp <- curve(unif_sum_pwrfunc_greater(theta = x, alpha = alpha, theta_not = theta.not, n = n), from = 0.001, to = upper.x, n = 1000)

        if(!(all(tmp$y == cummax(tmp$y))) | all(tmp$y == 1) | any(is.na(tmp$y))){
          actionLink(inputId = "warning_link", label = txt, style = "color:red; font-weight:bold")
        } 
        
      } else if(input$alternative == "Less than"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        tmp <- curve(unif_sum_pwrfunc_less(theta = x, alpha = alpha, theta_not = theta.not, n = n), from = 0.001, to = upper.x, n = 1000)
        
        if(!(all(tmp$y == cummin(tmp$y))) | all(tmp$y == 1) | any(is.na(tmp$y))){
          actionLink(inputId = "warning_link", label = txt, style = "color:red; font-weight:bold")
        }
      } else if(input$alternative == "Not equal to"){
        n <- input$sample.size
        theta.not <- input$theta.not
        alpha <- input$alpha
        theta <- val$theta
        
        if(is.na(theta)){
          upper.x <- max(c(
            2*theta.not
          ))
        } else{
          upper.x <- max(c(
            2*theta.not,
            theta
          ))
        }
        tmp <- curve(unif_sum_pwrfunc_noteqto(theta = x, alpha = alpha, theta_not = theta.not, n = n), from = 0.001, to = upper.x, n = 1000)
        
        min.x <- which(tmp$y == min(tmp$y))
        lwr.y <- tmp$y[1:min.x]
        upr.y <- tmp$y[(min.x + 1) : length(tmp$y)]
        
        mono <- all(c(
          all(lwr.y == cummin(lwr.y)),
          all(upr.y == cummax(upr.y))
        ))
        
        if(!(mono) | all(tmp$y == 1) | any(is.na(tmp$y))){
          actionLink(inputId = "warning_link", label = txt, style = "color:red; font-weight:bold")
        }
      }

    } else{
      NULL
    }
  )
  
  output$divStatus <- reactive({
    !(input$sample.size %in% 1:6)
  })
  outputOptions(output, "divStatus", suspendWhenHidden = FALSE)

  # additional warning message
  output$warning2 <- renderUI(

    HTML(
    "<ul> 
    <li> <b> Numerical Instability: </b> This power function (and the resulting sampling distributions) exhibits
    strange oscillating behavior due to numerical instability in the Irwin-Hall distribution function. 
    This problem is exacerbated and more noticable as the sample size increases; see Alberto (2019) for greater 
    detail. The plot below allows you to better visualize the numerical instability. </li>
    </ul>")
  )
  
  # warning plot
  output$warning_plot <- renderPlot({
    
    if(input$alternative == "Greater than"){
      n <- input$sample.size
      theta.not <- input$theta.not
      alpha <- input$alpha
      theta <- val$theta
      
      # identify problem region
      
      if(is.na(theta)){
        upper.x <- max(c(
          2*theta.not
        ))
      } else{
        upper.x <- max(c(
          2*theta.not,
          theta
        ))
      }
      
      test.x <- seq(.001, upper.x, length.out = 1000)
      test.y <- unif_sum_pwrfunc_greater(test.x, alpha, theta.not, n)
      
      
      lims <- range(test.x[which(test.y != cummax(test.y))])
      # lims <- test.x[c(max(0.001, min(which(test.y < 0))), max(which(test.y < 0)))]
      
      if(any(is.na(lims)) | any(is.infinite(lims))) {
        plot(1, type = "n", axes = F, xlab = "", ylab = "", main = "")
        text(1, 1, labels = "Due to the numerical instability, the CDF takes on values of negative infinity and positive infinity.", col = "red")
      } else{
        plot.x <- seq(lims[1], lims[2], length.out = 1000)
        plot.y <- unif_sum_pwrfunc_greater(theta = plot.x, alpha = alpha, theta_not = theta.not, n = n)
        
        plot(plot.y ~ plot.x, type = "l", xlab = expression(theta), ylab = expression(beta(theta)),
             main = bquote("Power Function for T(X) ="~ Sigma(X['i']) ~ "for" ~ theta ~ "in (" * .(round(lims[1], 2)) * "," ~ .(round(lims[2], 2)) * ")"), las = 1)
      }
      
    }
    
    if(input$alternative == "Less than"){
      n <- input$sample.size
      theta.not <- input$theta.not
      alpha <- input$alpha
      theta <- val$theta
      
      # identify problem region
      
      if(is.na(theta)){
        upper.x <- max(c(
          2*theta.not
        ))
      } else{
        upper.x <- max(c(
          2*theta.not,
          theta
        ))
      }
      
      test.x <- seq(.001, upper.x, length.out = 1000)
      test.y <- unif_sum_pwrfunc_less(test.x, alpha, theta.not, n)
      
      lims <- range(test.x[which(test.y != cummin(test.y))])
      # lims <- test.x[c(max(0.001, min(which(test.y < 0))), max(which(test.y < 0)))]
      
      if(any(is.na(lims)) | any(is.infinite(lims))) {
        plot(1, type = "n", axes = F, xlab = "", ylab = "", main = "")
        text(1, 1, labels = "Due to the numerical instability, the CDF takes on values of negative infinity and positive infinity.", col = "red")
      } else{
        plot.x <- seq(lims[1], lims[2], length.out = 1000)
        plot.y <- unif_sum_pwrfunc_less(theta = plot.x, alpha = alpha, theta_not = theta.not, n = n)
        
        plot(plot.y ~ plot.x, type = "l", xlab = expression(theta), ylab = expression(beta(theta)),
             main = bquote("Power Function for T(X) ="~ Sigma(X['i']) ~ "for" ~ theta ~ "in (" * .(round(lims[1], 2)) * "," ~ .(round(lims[2], 2)) * ")"), las = 1)
      }
    }
    
    if(input$alternative == "Not equal to"){
      n <- input$sample.size
      theta.not <- input$theta.not
      alpha <- input$alpha
      theta <- val$theta
      
      # identify problem region
      
      if(is.na(theta)){
        upper.x <- max(c(
          2*theta.not
        ))
      } else{
        upper.x <- max(c(
          2*theta.not,
          theta
        ))
      }
      
      test.x <- seq(.001, upper.x, length.out = 1000)
      test.y <- unif_sum_pwrfunc_noteqto(test.x, alpha, theta.not, n)
      min.x <- which(test.y == min(test.y))
      
      lwr.test.x <- test.x[1:min.x]
      lwr.test.y <- test.y[1:min.x]
      lims.lwr <- range(lwr.test.x[which(lwr.test.y != cummin(lwr.test.y))])
      
      upr.test.x <- test.x[(min.x + 1) : length(test.x)]
      upr.test.y <- test.y[(min.x + 1) : length(test.x)]
      lims.upr <- range(upr.test.x[which(upr.test.y != cummax(upr.test.y))])
      
      if(all(is.infinite(lims.upr))){
        lims <- lims.lwr
      } else{
        lims <- c(min(lims.lwr), max(lims.upr))
      }
      
      if(any(is.na(lims)) | any(is.infinite(lims))) {
        plot(1, type = "n", axes = F, xlab = "", ylab = "", main = "")
        text(1, 1, labels = "Due to the numerical instability, the CDF takes on values of negative infinity and positive infinity.", col = "red")
      } else{
        plot.x <- seq(lims[1], lims[2], length.out = 1000)
        plot.y <- unif_sum_pwrfunc_noteqto(theta = plot.x, alpha = alpha, theta_not = theta.not, n = n)
        
        plot(plot.y ~ plot.x, type = "l", xlab = expression(theta), ylab = expression(beta(theta)),
             main = bquote("Power Function for T(X) ="~ Sigma(X['i']) ~ "for" ~ theta ~ "in (" * .(round(lims[1], 2)) * "," ~ .(round(lims[2], 2)) * ")"), las = 1)
      }
    }
    
  })
  
  # additional warning
  output$warning3 <- renderUI(
    
    HTML(
    "<ul> 
    <li> <b> Central Limit Theorem: </b> One option to remedy this numerical instability issue is to use the
    Central Limit Theorem to approximate the sampling distribution of the sum of uniform random variables. 
    For sample sizes of even four or greater, the Central Limit Theorem provides a reasonably good 
    approximation to the sampling distribution. Use the 'Irwin-Hall Normal Approximation' tab to further
    investigate this relationship. To use the CLT to approximate the power function and sampling 
    distributions, check the 'Use normal approximation' box on the side panel. </li>
    </ul>")
  )
  
  # additional warning
  output$warning4 <- renderUI(
    
    HTML(
      "<ul> 
    <li> <b> Simulation: </b> Another option often used in practice is to simulate the power.
    To do so, we generate draws from the sampling distribution of the sum of uniform random variables under both 
    the null value and true value of theta. We then choose our critical value(s) such that we reject with 
    probability alpha when the null hypothesis is true by pulling quantiles from the empirical CDF of the simulated 
    sampling distribution under the null value. Finally, we calculate the proportion of simulated statistics
    under theta that were as or more extreme than these critical values, which becomes our estimate of the power. 
    To see this in action, click the 'Simulated Power' tab. </li>
    </ul>")
  )
  
  # toggleable action link
  rv <- reactiveValues(clicks = 0)
  observeEvent(input$warning_link, {
    shinyjs::toggle("warning_text")
    rv$clicks <- rv$clicks + 1
    if(rv$clicks %% 2 == 1){
      txt <- "Click here to hide warning information."
    } else{
      txt <- "Warning: Something is wrong! Click here to learn more."
    }
    updateActionButton(session, "warning_link", label = txt)
  })
  
  # normal approx tab
  output$sim_hist <- renderPlot({
    par(mfrow = c(2,1))
    sims <- input$sim.reps
    n <- input$sim.n
    theta <- input$sim.theta
    
    tmp <- replicate(sims, sum(runif(n, 0, theta)))
    hist(tmp, 
         xlab = bquote(Sigma(X[i])),
         main = bquote('Approximate sampling distribution of' ~ Sigma(X[i]) ~ 'for' ~ theta ~ '=' ~ .(round(theta, 2)))
    )
    
    plot(ecdf(tmp), main = bquote("Empirical CDF of simulated samples"),
         ylab = bquote(F[n](x)))
    
    if(input$sim.addcdf){
      curve(pnorm(x, n*theta / 2 , sqrt(n * theta^2 / 12)), lty = 2, col = "red", add = T)
      tmp2 <- ecdf(tmp)
      ks <- max(abs(tmp2(tmp) - pnorm(tmp, n*theta/2, sqrt(n*theta^2/12))))
      
      legend("bottomright", legend =  c(expression(paste('Empirical CDF')), 'Normal CDF', bquote('Kolmogorov-Smirnov distance =' ~ .(round(ks, 6)))),
             lty = c(1,2, NA), col = c(1:2, NA), bty = "n")
    }
    
  })
  
  # hide norm approx tab if uniform sum isn't selected
  observe({
    req(input$distribution)
    req(input$statistic)
    if(input$distribution == "Uniform" & input$statistic == "sum"){
      showTab(inputId = "tabselected", target = 'approx')
      showTab(inputId = "tabselected", target = 'sim')
    } else {
      hideTab(inputId = "tabselected", target = 'approx')
      hideTab(inputId = "tabselected", target = "sim")
    }
  })
  
  # simulated power tab
  null.samps <- reactiveValues(data = NULL)
  alt.samps <- reactiveValues(data = NULL)
  
  observeEvent(input$sim2.go, {
    null.samps$data <- replicate(input$sim2.reps, sum(runif(input$sim2.n, 0, input$sim2.theta0)))
    alt.samps$data <- replicate(input$sim2.reps, sum(runif(input$sim2.n, 0, input$sim2.theta)))
  })
  
  output$sim2_power <- renderPlot({
    input$sim2.go
    if(is.null(null.samps$data) | is.null(alt.samps$data)){
      return()
    } else{
      isolate({
        
        par(mfrow = c(2,1))
        sims <- input$sim2.reps
        n <- input$sim2.n
        theta <- input$sim2.theta
        theta0 <- input$sim2.theta0
        alpha <- input$sim2.alpha
        length.out <- 100
        
        null.dens <- density(null.samps$data)
        alt.dens <- density(alt.samps$data)
        
        if(input$sim2.alt == "Greater than"){
          # power curve
          sims <- replicate(100000, sum(runif(n, 0, 1)))
          if(is.na(theta)){
            upper.x <- max(c(
              2*theta0
            ))
          } else{
            upper.x <- max(c(
              2*theta0,
              theta
            ))
          }
          xlims <- c(0, upper.x)
          sim.x <- seq(0, upper.x, length.out = length.out)
          
          sims.mat <- matrix(rep(sims, length.out), ncol = length.out)
          theta.mat <- matrix(rep(sim.x, each = length(sims)), ncol = length.out)
          theta0.sims <- sims * theta0
          prod.mat <- sims.mat * theta.mat
          k1 <- quantile(theta0.sims, 1 - alpha)
          power <- apply(prod.mat, 2, FUN = function(x) length(which(x >= k1)) / length(x))
          
          plot(1, type = "n", xlim = xlims, ylim = c(0,1),
               ylab = bquote(beta(theta)),
               xlab = bquote(theta),
               main = bquote("Simulated Power Function for T(X) =" ~ Sigma(X[i])))
          lines(power ~ sim.x)
          abline(h = alpha, lty = 2, col = 2)
          
          # sampling distributions
          xlims <- c(
            min(c(min(null.dens$x), min(alt.dens$x))),
            max(c(max(null.dens$x), max(alt.dens$x)))
          )
          ylims <- c(
            0, 
            max(c(max(null.dens$y), max(alt.dens$y)))
          )
          plot(1, type = "n", xlim = xlims, ylim = ylims,
               ylab = bquote(f[Sigma(X[i])](x)),
               xlab = "T(X)",
               main = bquote("Simulated Sampling Distribution for T(X) =" ~ Sigma(X[i]) ~ "for" ~ theta ~
                               "=" ~ .(round(theta, 2)) ~ "and" ~ theta[0] ~ "=" ~ .(round(theta0, 2))))
          lines(null.dens, lty = 2)
          lines(alt.dens, lty = 1)
          
          # polygons
          abline(v = k1, lty = 2, col = 2)
          theta.sims <- sims * theta
          power <- length(which(theta.sims >= k1)) / length(theta.sims)
          
          
          which.null.x <- which(null.dens$x >= k1)
          which.alt.x <- which(alt.dens$x >= k1)
          which.null.y <- null.dens$y[which.null.x]
          which.alt.y <- alt.dens$y[which.alt.x]
          
          if(max(which.alt.y) >= max(which.null.y)){
            polygon(
              x = c(k1, alt.dens$x[which.alt.x]),
              y = c(0, alt.dens$y[which.alt.x]),
              col = "grey"
            )
            polygon(
              x = c(k1, null.dens$x[which.null.x]),
              y = c(0, null.dens$y[which.null.x]),
              col = "red"
            )
            text(k1, which.null.y[1], labels = bquote("T(X) =" ~ .(round(k1, 2))), col = 2, pos = 4)
          } else{
            polygon(
              x = c(k1, null.dens$x[which.null.x]),
              y = c(0, null.dens$y[which.null.x]),
              col = "red"
            )
            polygon(
              x = c(k1, alt.dens$x[which.alt.x]),
              y = c(0, alt.dens$y[which.alt.x]),
              col = "grey"
            )
            text(k1, which.null.y[1], labels = bquote("T(X) =" ~ .(round(k1, 2))), col = 2, pos = 4)
          }
          
          legend("topright",
                 legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
                 lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
          
          

        }
        
        if(input$sim2.alt == "Less than"){
          
          
          # power curve
          sims <- replicate(100000, sum(runif(n, 0, 1)))
          if(is.na(theta)){
            upper.x <- max(c(
              2*theta0
            ))
          } else{
            upper.x <- max(c(
              2*theta0,
              theta
            ))
          }
          xlims <- c(0, upper.x)
          sim.x <- seq(0, upper.x, length.out = length.out)
          
          sims.mat <- matrix(rep(sims, length.out), ncol = length.out)
          theta.mat <- matrix(rep(sim.x, each = length(sims)), ncol = length.out)
          theta0.sims <- sims * theta0
          prod.mat <- sims.mat * theta.mat
          k1 <- quantile(theta0.sims, alpha)
          power <- apply(prod.mat, 2, FUN = function(x) length(which(x <= k1)) / length(x))
          
          plot(1, type = "n", xlim = xlims, ylim = c(0,1),
               ylab = bquote(beta(theta)),
               xlab = bquote(theta),
               main = bquote("Simulated Power Function for T(X) =" ~ Sigma(X[i])))
          lines(power ~ sim.x)
          abline(h = alpha, lty = 2, col = 2)
          
          # sampling distributions
          xlims <- c(
            min(c(min(null.dens$x), min(alt.dens$x))),
            max(c(max(null.dens$x), max(alt.dens$x)))
          )
          ylims <- c(
            0, 
            max(c(max(null.dens$y), max(alt.dens$y)))
          )
          plot(1, type = "n", xlim = xlims, ylim = ylims,
               ylab = bquote(f[Sigma(X[i])](x)),
               xlab = "T(X)",
               main = bquote("Simulated Sampling Distribution for T(X) =" ~ Sigma(X[i]) ~ "for" ~ theta ~
                               "=" ~ .(round(theta, 2)) ~ "and" ~ theta[0] ~ "=" ~ .(round(theta0, 2))))
          lines(null.dens, lty = 2)
          lines(alt.dens, lty = 1)
          
          # polygons
          abline(v = k1, lty = 2, col = 2)
          theta.sims <- sims * theta
          power <- length(which(theta.sims <= k1)) / length(theta.sims)
          
          
          which.null.x <- which(null.dens$x <= k1)
          which.alt.x <- which(alt.dens$x <= k1)
          which.null.y <- null.dens$y[which.null.x]
          which.alt.y <- alt.dens$y[which.alt.x]
          
          if(max(which.alt.y) >= max(which.null.y)){
            polygon(
              x = c(alt.dens$x[which.alt.x], k1),
              y = c(alt.dens$y[which.alt.x], 0),
              col = "grey"
            )
            polygon(
              x = c(null.dens$x[which.null.x], k1),
              y = c(null.dens$y[which.null.x], 0),
              col = "red"
            )
            text(k1, which.null.y[length(which.null.y)], labels = bquote("T(X) =" ~ .(round(k1, 2))), col = 2, pos = 4)
          } else{
            polygon(
              x = c(null.dens$x[which.null.x], k1),
              y = c(null.dens$y[which.null.x], 0),
              col = "red"
            )
            polygon(
              x = c(alt.dens$x[which.alt.x], k1),
              y = c(alt.dens$y[which.alt.x], 0),
              col = "grey"
            )
            text(k1, which.null.y[length(which.null.y)], labels = bquote("T(X) =" ~ .(round(k1, 2))), col = 2, pos = 4)
          }
          
          legend("topright",
                 legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
                 lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
        
        }
        
        if(input$sim2.alt == "Not equal to"){
          
          
          # power curve
          sims <- replicate(100000, sum(runif(n, 0, 1)))
          if(is.na(theta)){
            upper.x <- max(c(
              2*theta0
            ))
          } else{
            upper.x <- max(c(
              2*theta0,
              theta
            ))
          }
          xlims <- c(0, upper.x)
          sim.x <- seq(0, upper.x, length.out = length.out)
          
          sims.mat <- matrix(rep(sims, length.out), ncol = length.out)
          theta.mat <- matrix(rep(sim.x, each = length(sims)), ncol = length.out)
          theta0.sims <- sims * theta0
          prod.mat <- sims.mat * theta.mat
          k1 <- quantile(theta0.sims, alpha/2)
          k2 <- quantile(theta0.sims, 1 - alpha/2)
          power <- apply(prod.mat, 2, FUN = function(x) length(which(x <= k1 | x >= k2)) / length(x))
          
          plot(1, type = "n", xlim = xlims, ylim = c(0,1),
               ylab = bquote(beta(theta)),
               xlab = bquote(theta),
               main = bquote("Simulated Power Function for T(X) =" ~ Sigma(X[i])))
          lines(power ~ sim.x)
          abline(h = alpha, lty = 2, col = 2)
          
          # sampling distributions
          xlims <- c(
            min(c(min(null.dens$x), min(alt.dens$x))),
            max(c(max(null.dens$x), max(alt.dens$x)))
          )
          ylims <- c(
            0, 
            max(c(max(null.dens$y), max(alt.dens$y)))
          )
          plot(1, type = "n", xlim = xlims, ylim = ylims,
               ylab = bquote(f[Sigma(X[i])](x)),
               xlab = "T(X)",
               main = bquote("Simulated Sampling Distribution for T(X) =" ~ Sigma(X[i]) ~ "for" ~ theta ~
                               "=" ~ .(round(theta, 2)) ~ "and" ~ theta[0] ~ "=" ~ .(round(theta0, 2))))
          lines(null.dens, lty = 2)
          lines(alt.dens, lty = 1)

          # polygons
          abline(v = k1, lty = 2, col = 2)
          abline(v = k2, lty = 2, col = 2)
          theta.sims <- sims * theta
          power <- length(which(theta.sims <= k1 | theta.sims >= k2)) / length(theta.sims)

          which.null.x.lower <- which(null.dens$x <= k1)
          which.alt.x.lower <- which(alt.dens$x <= k1)
          which.null.y.lower <- null.dens$y[which.null.x.lower]
          which.alt.y.lower <- alt.dens$y[which.alt.x.lower]
          
          which.null.x.upper <- which(null.dens$x >= k2)
          which.alt.x.upper <- which(alt.dens$x >= k2)
          which.null.y.upper <- null.dens$y[which.null.x.upper]
          which.alt.y.upper <- alt.dens$y[which.alt.x.upper]

          if(max(which.alt.y.lower) >= max(which.null.y.lower)){
            polygon(
              x = c(alt.dens$x[which.alt.x.lower], k1),
              y = c(alt.dens$y[which.alt.x.lower], 0),
              col = "grey"
            )
            polygon(
              x = c(null.dens$x[which.null.x.lower], k1),
              y = c(null.dens$y[which.null.x.lower], 0),
              col = "red"
            )
            text(k1, which.null.y.lower[length(which.null.y.lower)], labels = bquote("T(X) =" ~ .(round(k1, 2))), col = 2, pos = 4)
            
            polygon(
              x = c(k2, null.dens$x[which.null.x.upper]),
              y = c(0, null.dens$y[which.null.x.upper]),
              col = "red"
            )
            polygon(
              x = c(k2, alt.dens$x[which.alt.x.upper]),
              y = c(0, alt.dens$y[which.alt.x.upper]),
              col = "grey"
            )
            text(k2, which.null.y.upper[1], labels = bquote("T(X) =" ~ .(round(k2, 2))), col = 2, pos = 4)
          } else{
            polygon(
              x = c(null.dens$x[which.null.x.lower], k1),
              y = c(null.dens$y[which.null.x.lower], 0),
              col = "red"
            )
            polygon(
              x = c(alt.dens$x[which.alt.x.lower], k1),
              y = c(alt.dens$y[which.alt.x.lower], 0),
              col = "grey"
            )
            text(k1, which.null.y.lower[length(which.null.y.lower)], labels = bquote("T(X) =" ~ .(round(k1, 2))), col = 2, pos = 4)
            
            polygon(
              x = c(k2, alt.dens$x[which.alt.x.upper]),
              y = c(0, alt.dens$y[which.alt.x.upper]),
              col = "grey"
            )
            polygon(
              x = c(k2, null.dens$x[which.null.x.upper]),
              y = c(0, null.dens$y[which.null.x.upper]),
              col = "red"
            )
            text(k2, which.null.y.upper[1], labels = bquote("T(X) =" ~ .(round(k2, 2))), col = 2, pos = 4)

          }

          legend("topright",
                 legend = c(expression(paste("Sampling Distribution Under ",theta)), bquote("Sampling Distribution Under"~theta[0]), bquote(alpha), bquote(beta(theta)~"="~.(power))),
                 lty = c(1,2,NA,NA), pch = c(NA, NA, 15, 15), col = c(1,1,"red","gray") , bty = "n")
          
        }
        
      })
      
    }
  
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = T))

