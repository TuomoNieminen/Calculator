# Layout is three responsive panels:
# 1: distribution choice 2: probability choice 3: a plot output

fluidRow(
  
  # distribution and parameter choices
  
  column(3, br(),
         div(style="padding:15px; background-color:#F5F5FF; border-radius:10px;",
             
             # distribution type
             radioButtons("continuous_dist", "distribution:",
                          c("Normal" = "normal",
                            "t" = "t",
                            "Chi-squared" = "chi-squared",
                            "F" = "F")),
             
             # parameters for the chosen distribution
             h5("parameters:"),
             
             conditionalPanel(condition="input.continuous_dist=='normal'",
                              numericInput("mean","mean",
                                           value=0),
                              numericInput("sd","standard deviation",
                                           value=1)
             ),
             conditionalPanel(condition="input.continuous_dist!='normal'",
                              numericInput("df1","degrees of freedom",
                                           value=3)),
             conditionalPanel(condition="input.continuous_dist=='F'",
                              numericInput("df2", "degrees of freedom 2",
                                           value=3))
         )),
  
  # probability choice and output

  
column(3, br(),
         div(style="padding:15px; background-color:#F6F0FF; border-radius:10px;",

             radioButtons("continuous_ptype","choose one",
                          c("P(X <= x)"="cum",
                            "P(X > x)"="reverse_cum",
                            "P(x0 < X <= x)"="range")
                          ),
             br(),
             
             p("Adjust either to compute the other:"),
             
             numericInput("pcontinuous","current probability",value=NA,step=0.01,min=0,max=1),
             conditionalPanel(condition="input.continuous_ptype=='range'",
                              numericInput("x0continuous","current x0 point (lower)",
                                           value=0, step=0.1)),
             numericInput("xcontinuous","current x point", 
                          value=1, step=0.1)
         )),
  
  # plot output
  column(6,
         p(align="center","You can click on the plot to calculate probabilities"),
         plotOutput("continuous_plot", click="continuous_click"),
         
         fluidRow(column(2),
                  column(10,
                         sliderInput("ylim","Adjust the y axis",
                                     min = 0, max=2,value=0.4,
                                     ticks=F, step=0.02)
                  ))
  )
)