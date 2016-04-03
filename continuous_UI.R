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
             htmlOutput("continuous_prob"),               
             radioButtons("continuous_ptype","probability",
                          c("P(X =< x)"="cum",
                            "P(X > x)"="reverse_cum",
                            "P(x0 =< X =< x)"="range")),
             br(),             
             conditionalPanel(condition="input.continuous_ptype=='range'",
                              numericInput("x0continuous","select the x0 point",
                                           value=0, step=0.1)),
             numericInput("xcontinuous","select the x point", 
                          value=1, step=0.1)
         )),
  
  # plot output
  column(6,
         p(align="center","Click on the plot to define probabilities"),
         plotOutput("continuous_plot", click="continuous_click"),
         
         fluidRow(column(2),
                  column(10,
                         sliderInput("ylim","Adjust the y axis",
                                     min = 0, max=2,value=0.4,
                                     ticks=F, step=0.02)
                  ))
  )
)