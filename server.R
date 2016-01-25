library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # parameters from user
  mean <- reactive({
    if(!is.na(input$mean)) input$mean else 0
  })
  sd <- reactive({
    if(!is.na(input$sd)) input$sd else 1
  })
  df1 <- reactive({
    if(!is.na(input$df1)) input$df1 else 3
  })
  df2 <- reactive({
    if(!is.na(input$df2)) input$df2 else 2
  })
  
  # x  
  x <- reactive({
    switch(input$dist,
           "normal" = seq(-4*sd()+mean(),4*sd()+mean(),length=200),
           "t" = seq(-4,4,length=200),
           "F" = seq(0,9,length=200),
           "chi-squared" = seq(0,3*df1(),length=200)
    )
  })
  
  # fx
  fx <- reactive({
    function(x) {
      switch(input$dist,
             "normal" = dnorm(x, mean(),sd()),
             "t" = dt(x, df1()),
             "F" = df(x, df1(), df2()),
             "chi-squared" = dchisq(x, df1())
      )
    }
  })

  
  # highlighted region ranges
    hx <- reactive({      
    switch(input$ptype,
           cum = x()[x() <= input$q],
           reverse_cum = x()[x() > input$q],
           range = x()[x() <= input$q & x() > input$q0]
    )
  })
  
  #probability
  p <- reactive({
    q <- input$q
    switch(input$dist,
           "normal" = pnorm(q, mean(), sd()),
           "t" = pt(q, df1()),
           "F"= pf(q, df1(), df2()),
           "chi-squared"=pchisq(q, df1())
    )
  }) 
  p0 <- reactive({
    q <- input$q0
    switch(input$dist,
           "normal" = pnorm(q, mean(), sd()),
           "t" = pt(q, df1()),
           "F"= pf(q,df1(), df2()),
           "chi-squared"=pchisq(q, df1())
    )
  })
  
  #probability output
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
    
    # density
    plot(x(), fx()(x()), 
         type='l', ylim=c(0,input$ylim),
         main=paste("The density of the",input$dist, "distribution"))
    
    # p area 
    polygon(c( min(hx()), hx(), max(hx()) ),
            c( 0, fx()(hx()) , 0 ), 
            col = "grey")
  })
  
  
  
})