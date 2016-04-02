library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  # parameters from user 
  mean <- reactive({if(!is.na(input$mean)) input$mean else 0})
  sd <- reactive({if(!is.na(input$sd)) input$sd else 1})
  df1 <- reactive({if(!is.na(input$df1)) input$df1 else 3})
  df2 <- reactive({if(!is.na(input$df2)) input$df2 else 2})
  
  # x values for plotting
  x <- reactive({
    switch(input$dist,
           "normal" = seq(-4*sd()+mean(),4*sd()+mean(),length=200),
           "t" = seq(-4,4,length=200),
           "F" = seq(0.001,9,length=200),
           "chi-squared" = seq(0.001,3*df1(),length=200))
  })
  
  # probability density values for plotting
  fx <- reactive({
    function(x) {
      switch(input$dist,
             "normal" = dnorm(x, mean(),sd()),
             "t" = dt(x, df1()),
             "F" = df(x, df1(), df2()),
             "chi-squared" = dchisq(x, df1()))
    }
  })
  
  # get probability region info from user
  Q <- reactiveValues(x=1, x0=0)
  
  # some helper functions
  updateNum <- function(id,x) {
    updateNumericInput(session=session, inputId=id, label = NULL, value=x)
  }
  which_closer <- function(x) {
    ifelse((x-input$x)**2 < (x - input$x0)**2, "x","x0")
  }
  
  # Update Q values using both numeric input and user clicking the plot
  observe({
    click <- input$plot_click$x
    if(!is.null(click)) {
      if(input$ptype=="range") {
        updateNum(which_closer(click),click)
        if(input$x < input$x0) updateNum("x0",input$x)
      } else {
        updateNum("x",click)
      }
    }
    
    Q$x <- input$x
    Q$x0 <- input$x0
    
  })
  
  # x values for highlighted probability regions
  hx <- reactive({      
    switch(input$ptype,
           cum = x()[x() <= Q$x],
           reverse_cum = x()[x() > Q$x],
           range = x()[x() > Q$x0 & x() <= Q$x]
    )
  })
  
  #probabilities  
  p <- reactive({
    q <- Q$x
    switch(input$dist,
           "normal" = pnorm(q, mean(), sd()),
           "t" = pt(q, df1()),
           "F"= pf(q, df1(), df2()),
           "chi-squared"=pchisq(q, df1()))
  }) 
  p0 <- reactive({
    q <- Q$x0
    switch(input$dist,
           "normal" = pnorm(q, mean(), sd()),
           "t" = pt(q, df1()),
           "F"= pf(q,df1(), df2()),
           "chi-squared"=pchisq(q, df1()))
  })
  
  
  # outputs
  
  # calculated probability
  output$prob <- renderText({   
    p <- switch(input$ptype,
                cum = p(),
                reverse_cum = 1-p(),
                range = p()-p0())
    paste("<p id = 'prob_output'>The probability corresponding to your current selection is <b>",
          round(abs(p),5),"</b></p>")
  })
  
  # density plot
  
  output$plot <- renderPlot({
    
    # density curve
    plot(x(), fx()(x()), 
         type='l', ylim=c(0,input$ylim),
         main=paste("The density of the",input$dist, "distribution"))
    
    # probability area 
    polygon(c( min(hx()), hx(), max(hx()) ),
            c( 0, fx()(hx()) , 0 ), 
            col = "grey92")
  })
  
  
  
})