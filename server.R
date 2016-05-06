library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  # rounding
  r <- 4
  
  # DISTRIBUTIONS
  
  distribution <- reactive({
    ifelse(input$dist_type=="continuous",
           input$continuous_dist,
           input$discrete_dist)
  })
  
  #parameters
  
  # continuous
  mean <- reactive({if(!is.na(input$mean)) input$mean else 0})
  sd <- reactive({if(!is.na(input$sd)) input$sd else 1})
  df1 <- reactive({if(!is.na(input$df1)) input$df1 else 3})
  df2 <- reactive({if(!is.na(input$df2)) input$df2 else 2})
  
  # discrete
  probs <- reactive({if(!is.na(input$p)) input$p else 0.5})
  n <- reactive({if(!is.na(input$n)) input$n else 1})
  lambda <- reactive({if(!is.na(input$lambda)) input$lambda else 3})
  
  # probability type choice 
  # p(X<=x) = "cum", P(X > x) = "reverse_cum", P(x0 < X <= x) = "range"
  ptype <- reactive({
    ifelse(input$dist_type=="continuous",
           input$continuous_ptype,
           input$discrete_ptype)
  })
  

  # COMPUTATIONS 
  
  # some helper functions
  
  # update an input (p, x, or x0)
  updateNum <- function(id,x) {
    x <- round(x,r)
    updateNumericInput(session=session, 
                       inputId=paste0(id,input$dist_type), value=x)
  }
  
  # check if user wants to adjust x or x0
  which_closer <- function(x) {
    type <- input$dist_type
    ifelse((x-input[[paste0("x",type)]])**2 < (x - input[[paste0("x0",type)]])**2, 
           "x","x0")
  }
  
  # quantiles
  get_q <- function(p) {
    q <- switch(distribution(),
                "normal" = qnorm(p, mean(), sd()),
                "t" = qt(p, df1()),
                "F"= qf(p, df1(), df2()),
                "chi-squared"= qchisq(p, df1()),
                "binomial" = qbinom(p, n(), probs()),
                "poisson" = qpois(p, lambda())
    )
    q  
  }
  
  # cumulative probabilities
  get_p <- function(q) {
    p <- switch(distribution(),
                "normal" = pnorm(q, mean(), sd()),
                "t" = pt(q, df1()),
                "F"= pf(q, df1(), df2()),
                "chi-squared"=pchisq(q, df1()),
                "binomial" = pbinom(q, n(), probs()),
                "poisson" = ppois(q, lambda())
    )
    p
  }
  
  # compute the probability specified by x and x0
  compute_p <- function(x,x0) {
    p1 <- get_p(x)
    p0 <- get_p(x0)
    p <- switch(ptype(),
                cum = p1,
                reverse_cum = 1- p1,
                range = p1 - p0)
    abs(p)
  }
  
  
  
  # Update x and x0 based on user clicking the plot
  observe({
    type <- input$dist_type
    click <- switch(type,
                    "continuous" = input$continuous_click$x,
                    "discrete" = input$discrete_click$x
    )
    if(!is.null(click)) {
      click <- ifelse(type=="discrete", floor(click), click)
      if(input[[paste0(type,"_ptype")]]=="range") {
        updateNum(which_closer(click), click)
      } 
      else {
        updateNum("x", click)
      }
    }
  })
  
  #  info from user
  Q <- reactiveValues(x=1, x0=0, p=NULL)
  
  # update x
  observe({
    type <- input$dist_type
    
    x <- input[[paste0("x", type)]]
    x <- ifelse(is.na(x), Q$x, x)
    
    if(x != Q$x) {
      if(x < Q$x0) {
        Q$x0 <- x - 0.5
      } 
      
      Q$x <- x
    }
  })
  
  # update x0
  observe({
    type <- input$dist_type
    
    x0 <- input[[paste0("x0", type)]]
    x0 <- ifelse(is.na(x0), min(Q$x,Q$x0), x0)
    
    if(x0 != Q$x0) {
      
      if(Q$x < x0) {
        x0 <- Q$x - 0.5
      } 
      Q$x0 <- x0
      
    }
    
  })
  
  # update p
  observe({
    Q$p <- round(compute_p(Q$x, Q$x0),r)
    updateNum("p", Q$p)
  })
  
  # use p inputs to update x and x0
  observe({
    
    p <- input[[paste0("p",input$dist_type)]]
    p_current <- isolate(Q$p)
    update_q <- p != p_current
    update_q <- ifelse(is.na(update_q),F,update_q)
    
    
    if(update_q) {
      switch(ptype(),
             range = {
               p <- (1-p)/2
               x <- get_q(1-p)
               x0 <- get_q(p)
               updateNum("x", x)
               updateNum("x0", x0)
             },
             cum = {
               x <- get_q(p)
               updateNum("x", x)
             },
             reverse_cum ={
               x <- get_q(1-p)
               updateNum("x", x)
             }
      )
    }
    
  })
  
  # PLOTS
  
  # x values for plotting
  x <- reactive({
    switch(distribution(),
           "normal" = seq(-4*sd()+mean(),4*sd()+mean(),length=200),
           "t" = seq(-4,4,length=200),
           "F" = seq(0.001,9,length=200),
           "chi-squared" = seq(0.001,3*df1(),length=200),
           "binomial" = 0:n(),
           "poisson" = 0:(round(4*lambda()))
    )
  })
  
  # probability density values for plotting
  fx <- reactive({
    function(x) {
      
      switch(distribution(),
             "normal" = dnorm(x, mean(),sd()),
             "t" = dt(x, df1()),
             "F" = df(x, df1(), df2()),
             "chi-squared" = dchisq(x, df1()),
             "binomial"=dbinom(x, n(), probs()),
             "poisson" = dpois(x, lambda())
      )
    }
  })
  
  
  
  # x values for highlighted probability regions
  hx <- reactive({
    switch(ptype(),
           cum = x()[x() <= Q$x],
           reverse_cum = x()[x() > Q$x],
           range = x()[x() > Q$x0 & x() <= Q$x]
    )
  })
  
  
  # density plot
  
  output$continuous_plot <- renderPlot({
    x <- x()
    density <- fx()(x)
    
    # density curve
    plot(x, density, 
         type='l', ylim=c(0,input$ylim),
         main=paste("The density of the",input$continuous_dist, "distribution"))
    
    # probability area 
    polygon(c( min(hx()), hx(), max(hx()) ),
            c( 0, fx()(hx()) , 0 ), 
            col = "grey92")
  })
  
  output$discrete_plot <- renderPlot({
    x <- x()
    probability <- fx()(x)
    chosen <- x %in% hx()
    colors = c("grey90","grey60")[chosen+1]
    plot(x, probability, xaxt="n",
         type= "h", lwd=3, ylim=c(0,input$discrete_ylim), col=colors,
         main=paste("The point probabilities of the",input$discrete_dist, "distribution"))
    axis(1, at = x)
  })
  
  
})