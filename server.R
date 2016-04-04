library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  # parameters from user
  # continuous
  mean <- reactive({if(!is.na(input$mean)) input$mean else 0})
  sd <- reactive({if(!is.na(input$sd)) input$sd else 1})
  df1 <- reactive({if(!is.na(input$df1)) input$df1 else 3})
  df2 <- reactive({if(!is.na(input$df2)) input$df2 else 2})
  # discrete
  probs <- reactive({if(!is.na(input$p)) input$p else 0.5})
  n <- reactive({if(!is.na(input$n)) input$n else 1})
  lambda <- reactive({if(!is.na(input$lambda)) input$lambda else 3})
  
  # x values for plotting
  x <- reactive({
    if(input$dist_type=="continuous") {
    switch(input$continuous_dist,
           "normal" = seq(-4*sd()+mean(),4*sd()+mean(),length=200),
           "t" = seq(-4,4,length=200),
           "F" = seq(0.001,9,length=200),
           "chi-squared" = seq(0.001,3*df1(),length=200)
           )
    } else{
      switch(input$discrete_dist,
             "binomial" = 0:n(),
             "poisson" = 0:(round(4*lambda()))
      )
    }
  })
  
  # probability density values for plotting
  fx <- reactive({
    function(x) {
      if(input$dist_type=="continuous") {
      switch(input$continuous_dist,
             "normal" = dnorm(x, mean(),sd()),
             "t" = dt(x, df1()),
             "F" = df(x, df1(), df2()),
             "chi-squared" = dchisq(x, df1()))
      } else {
        switch(input$discrete_dist,
               "binomial"=dbinom(x, n(), probs()),
               "poisson" = dpois(x, lambda())
        )
      }
    }
  })
  
  # get probability region info from user
  Q <- reactiveValues(x=1, x0=0)
  
  # some helper functions
  updateNum <- function(id,x,type) {
    updateNumericInput(session=session, inputId=paste0(id,type), value=x)
  }
  which_closer <- function(x,type) {
    ifelse((x-input[[paste0("x",type)]])**2 < (x - input[[paste0("x0",type)]])**2, 
           "x","x0")
  }

  
  # Update Q values using both numeric input and user clicking the plot
  observe({
    type <- input$dist_type
    click <- switch(type,
                    "continuous" = input$continuous_click$x,
                    "discrete" = input$discrete_click$x
    )
    if(!is.null(click)) {
      click <- ifelse(type=="discrete",floor(click),click)
      if(input[[paste0(type,"_ptype")]]=="range") {
        updateNum(which_closer(click,type), click, type)
      } else {
        updateNum("x", click, type)
      }
    }
    x <- input[[paste0("x",type)]]
    x0 <- input[[paste0("x0",type)]]
    
    x <- ifelse(is.na(x), 0, x) 
    x0 <- ifelse(is.na(x0), min(Q$x,0), x0)
    
    if(x < x0) {
      updateNum("x0", x - 0.5, type)
    }
    
    Q$x <- x
    Q$x0 <- x0
    
  })
  
  # x values for highlighted probability regions
  hx <- reactive({
    type <- ifelse(input$dist_type=="continuous",
                   input$continuous_ptype,
                   input$discrete_ptype)
    switch(type,
           cum = x()[x() <= Q$x],
           reverse_cum = x()[x() > Q$x],
           range = x()[x() > Q$x0 & x() <= Q$x]
    )
  })
  
  #probabilities
  
  get_p <- function(q) {
    if(input$dist_type=="continuous") {
      switch(input$continuous_dist,
             "normal" = pnorm(q, mean(), sd()),
             "t" = pt(q, df1()),
             "F"= pf(q, df1(), df2()),
             "chi-squared"=pchisq(q, df1())
      )
    } else {
      switch(input$discrete_dist,
             "binomial" = pbinom(q, n(), probs()),
             "poisson" = ppois(q, lambda())
      )
    }
  }
  
  p <- reactive({
    get_p(Q$x)
  }) 
  p0 <- reactive({
    get_p(Q$x0)
  })
  
  
  # outputs
  
  # calculated probability
  output$continuous_prob <- renderText({
    p <- switch(input$continuous_ptype,
                cum = p(),
                reverse_cum = 1-p(),
                range = p()-p0())
    paste("<p id = 'prob_output'>The probability corresponding to your current selection is <b>",
          round(abs(p),5),"</b></p>")
  })
  
  output$discrete_prob <- renderText({   
    p <- switch(input$discrete_ptype,
                cum = p(),
                reverse_cum = 1-p(),
                range = p()-p0())
    paste("<p id = 'prob_output'>The probability corresponding to your current selection is <b>",
          round(abs(p),5),"</b></p>")
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