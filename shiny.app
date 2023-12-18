library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)
library(R.utils)

# define characteristic functions 
# sourced from https://en.wikipedia.org/wiki/Characteristic_function_(probability_theory) 
degenerate_char <- function(t, a = 1, ...) { exp(1i * t * a)}
bern_char <- function(t, p = .5, ...) {1 - p + p * exp( 1i * t ) }
binomial_char <- function(t, p = 0.5, n = 10, ...) { (1 - p + p * exp( 1i * t ))^n }
poisson_char <- function(t, lambda = 0.3, ...) { exp(lambda * (exp(1i * t) - 1 ))}
negbinom_char <- function(t, r = .5, p = .5, ...) { (p / (1 - exp(1i * t * p * exp(1i * t))))^r }
unif_cont_char <- function(t, a = 0, b = 1, ...) { (exp(1i * t * b) - exp(1i * t * a)) / (1i * t * (b - a)) }
laplace_char <- function(t, mu = 1, b = 5, ...) { exp(1i * t * mu) / (1 + b^2 * t^2) }
normal_char <- function(t, mu = 1, sigma_sqrd = 1, ...) {  exp(1i * t * mu - .5 * sigma_sqrd * t^2) }
chi_sqrd_char <- function(t, k = 1, ...) { (1-2 * 1i * t)^{-k / 2} }
cauchy_char <- function(t, mu = 1, theta = 1, ...) { exp(1i * t * mu - theta * abs(t)) }
gamma_char <- function(t, k = 10, theta = 1.1, ...) { (1 - 1i * t * theta)^{-k} }
exponential_char <- function(t, lambda = 1, ...) { (1-1i*t*lambda^{-1})^{-1} }

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Characteristic Functions"),
  sidebarLayout(

    # Sidebar with a slider input
    sidebarPanel(
      h3("Definition."),
      withMathJax(p("The ", 
      # link to wikipedia article for characteristic function
      htmltools::a(
        src = "https://en.wikipedia.org/wiki/Characteristic_function_(probability_theory)",
        "characteristic function"),
      " of a random variable X is defined as \\(\\phi_X(t) = \\mathbb{E}[e^{itX}]\\). This is basically the Fourier transform of the probability density function.")),

      selectInput("char_fn",
                  "Choice of Distribution",
                  choices = c(
                    "Degenerate",
                    "Bernoulli",
                    "Binomial",
                    "Poisson",
                    "Negative Binomial",
                    "Uniform (Continuous)",
                    "Laplace",
                    "Normal",
                    "Chi Squared",
                    "Cauchy",
                    "Gamma",
                    "Exponential"
                  )),

      # additional options specific to each of the distributions
      h4("Distribution Specific Parameters"),
      uiOutput("char_fn_options"),

      # scale for t 
      h4("Range for t values"),
      # sliderInput("t_scale", "t:", 1, min = -1000, max = 1000, value = c(-10, 10))
      shinyWidgets::sliderTextInput("t_scale","t:",
                            choices=c(-1000, 100, -10, -5, -2, -1, -0.5, 0.5, 1, 2, 5, 10, 100, 1000),
                            selected=c(-10, 10), grid = T),

      # resolution of t 
      h4("Resolution for t values"),
      shinyWidgets::sliderTextInput("t_res", "Resolution:", selected = 10000, choices = c(1000, 10000, 50000, 100000)),

      # button to reset all inputs
      actionButton("reset", "Reset")
    ),

    # Show a plot of the generated distribution; 
    # make the plot quite big so that the 3d plot is visible
    mainPanel(
      plotlyOutput("charPlot", height = "800px")
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # create a reactive function that will output the additional options
  output$char_fn_options <- renderUI({
    switch(input$char_fn, 
      "Degenerate" = list(
        # description of the Degenerate distribution at a = a
        p("The Degenerate distribution is a point mass at a."),
        sliderInput("a", "a", 1, min = -100, max = 100, animate=TRUE)
      ),
      "Bernoulli" = list(
        p("The Bernoulli distribution is a binary distribution that has outcome 1 with probability p and outcome 0 with probability 1-p."),
        sliderInput("p", "p", 0.5, min = 0.01, max = 1, animate = TRUE)
      ),
      "Binomial" = list(
        p("The Binomial distribution is the sum of n independent Bernoulli trials."),
        numericInput("n", "n", 10, min = 1, step = 1),
        sliderInput("p", "p", 0.5, min = .0, max = 1, step = .001, animate = TRUE)
      ),
      "Poisson" = list(
        p("The Poisson distribution is a discrete distribution that is often used to model the number of events that occur in a given time interval."),
        sliderInput("lambda", "lambda", value = 10, min = 0.01, max = 1000, animate = TRUE)
      ),
      "Negative Binomial" = list(
        p("The Negative Binomial distribution is the number of Bernoulli trials until the rth success."),
        sliderInput("r", "r", 10, min = 1, max = 100, step = 1, animate = TRUE),
        sliderInput("p", "p", 0.5, min = .0, max = 1, step = .001, animate = TRUE)
      ),
      "Uniform (Continuous)" = list(
        p("The Uniform distribution is a continuous distribution that is constant between a and b."),
        sliderInput("a", "a", 0, min = -100, max = 100, animate = TRUE),
        sliderInput("b", "b", 1, min = -100, max = 100, animate = TRUE)
      ),
      "Laplace" = list(
        p("The Laplace distribution, also called the double-exponential, is a continuous distribution that can be thought of as two symmetric exponentials about mu with rate parameter b."),
        # sliders for mu and b
        sliderInput("mu", "mu", 0, min = -100, max = 100, animate = TRUE),
        sliderInput("b", "b", 5, min = 0.01, max = 100, animate = TRUE)
      ),
      "Normal" = list(
        p("The Normal distribution, one of the most important distributions in statistics, is a continuous distribution centered at mu with variance sigma_sqrd."),
        # sliders for mu and sigma_sqrd
        sliderInput("mu", "mu", 0, min = -100, max = 100, animate = TRUE),
        sliderInput("sigma_sqrd", "sigma_sqrd", 1, min = 0.01, max = 100, animate = TRUE)
      ),
      "Chi Squared" = list(
        p("The Chi Squared distribution is the sum of the squares of k independent standard normal random variables."),
        sliderInput("k", "k", 1, min = 0.01, max = 100, animate = TRUE)
      ),
      "Cauchy" = list(
        p("The Cauchy distribution is a continuous distribution that is symmetric about mu and has scale parameter theta. It is often used as a counterexample to the Central Limit Theorem."),
        # sliders for mu and theta 
        sliderInput("mu", "mu", 0, min = -100, max = 100, animate = TRUE),  
        sliderInput("theta", "theta", 1, min = 0.01, max = 100, animate = TRUE)
      ),
      "Gamma" = list(
        p("The Gamma distribution is a continuous distribution that is often used to model waiting times.It has parameters k and theta that represent the shape and scale, respectively."),
        # sliders for k and theta
        sliderInput("k", "k", 10, min = 0.01, max = 100, animate = TRUE),  
        sliderInput("theta", "theta", 1.1, min = 0.01, max = 100, animate = TRUE)
      ),
      "Exponential" = list(
        p("The Exponential distribution is a continuous distribution that is often used to model waiting times. It has a single parameter lambda that represents the rate."),
        sliderInput("lambda", "lambda", 0.1, min = 0.01, max = 100, animate = TRUE)
      )
    )
  })

  output$charPlot <- renderPlotly({

    char_fn <- switch(input$char_fn, 
      "Degenerate" = degenerate_char,
      "Bernoulli" = bern_char,
      "Binomial" = binomial_char,
      "Poisson" = poisson_char,
      "Negative Binomial" = negbinom_char,
      "Uniform (Continuous)" = unif_cont_char,
      "Laplace" = laplace_char,
      "Normal" = normal_char,
      "Chi Squared" = chi_sqrd_char,
      "Cauchy" = cauchy_char,
      "Gamma" = gamma_char,
      "Exponential" = exponential_char
    )

    # add appropriate req statements depending on the input$char_fn using a switch statement
    switch(input$char_fn, 
      "Degenerate" = req(input$a),
      "Bernoulli" = req(input$p),
      "Binomial" = req(input$n, input$p),
      "Poisson" = req(input$lambda),
      "Negative Binomial" = req(input$r, input$p),
      "Uniform (Continuous)" = req(input$a, input$b),
      "Laplace" = req(input$mu, input$b),
      "Normal" = req(input$mu, input$sigma_sqrd),
      "Chi Squared" = req(input$k),
      "Cauchy" = req(input$mu, input$theta),
      "Gamma" = req(input$k, input$theta),
      "Exponential" = req(input$lambda)
    )

    # plot the characteristic function
    req(input$t_scale)
    req(input$t_res)
    char_fn_plotter(char_fn, range = seq(input$t_scale[1], input$t_scale[2], length.out = input$t_res),
      # all of the possible parameters
      n = input$n,
      p = input$p,
      lambda = input$lambda,
      b = input$b,
      sigma_sqrd = input$sigma_sqrd,
      k = input$k,
      theta = input$theta,
      mu = input$mu,
      a = input$a,
      r = input$r
      ) %>% 
      layout(title = paste(input$char_fn, "Characteristic Function"))
  })

  # reset all inputs
  observeEvent(input$reset, {
    updateSelectInput(session, "char_fn", selected = NULL)
    updateSliderInput(session, "a", value = 1)
    updateSliderInput(session, "p", value = 0.5)
    updateSliderInput(session, "n", value = 10)
    updateSliderInput(session, "lambda", value = if (input$char_fn == 'Poisson') 10 else 0.3)
    updateSliderInput(session, "r", value = 0.5)
    updateSliderInput(session, "b", value = 5)
    updateSliderInput(session, "mu", value = 1)
    updateSliderInput(session, "sigma_sqrd", value = 1)
    updateSliderInput(session, "k", value = 1)
    updateSliderInput(session, "theta", value = 1)
    shinyWidgets::updateSliderTextInput(session, "t_scale", selected = c(-10, 10))
    shinyWidgets::updateSliderTextInput(session, "t_res", selected = 10000)
  })
}

# Complete app with UI and server components
shinyApp(ui, server)
