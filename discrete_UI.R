# Layout is three responsive panels:
# 1: distribution choice 2: probability choice 3: a plot output

fluidRow(
  
  # distribution and parameter choices
  
  column(3, br(),
         div(style="padding:15px; background-color:#F5F5FF; border-radius:10px;",
             
             # distribution type
             radioButtons("discrete_dist", "distribution:",
                          c("Binomial" = "binomial",
                            "Poisson" = "poisson")),
             
             # parameters for the chosen distribution
             h5("parameters:"),
             
             conditionalPanel(condition="input.discrete_dist=='binomial'",
                              numericInput("n","trials",
                                           value=10, step=1),
                              numericInput("p","probs",
                                           value=0.5, step=0.05)
             ),
             conditionalPanel(condition="input.discrete_dist=='poisson'",
                              numericInput("lambda","lambda",
                                           value=3, step=0.5))
         )),
  
  # probability choice and output
  
  column(3, br(),
         div(style="padding:15px; background-color:#F6F0FF; border-radius:10px;",
             radioButtons("discrete_ptype","choose one",
                          c("P(X =< x)"="cum",
                            "P(X > x)"="reverse_cum",
                            "P(x0 < X =< x)"="range")),
             br(),
             
             p("Adjust either to compute the other:"),
             
             numericInput("pdiscrete","current probability",value=NA,step=0.01,min=0,max=1),
             conditionalPanel(condition="input.discrete_ptype=='range'",
                              numericInput("x0discrete","current x0 point (lower)",
                                           value=0)),
             numericInput("xdiscrete","current x point", value=5)
         )),
  
  # plot output
  column(6,
         p(align="center","You can click on the plot to calculate probabilities"),
         plotOutput("discrete_plot", click="discrete_click"),
         
         fluidRow(column(2),
                  column(10,
                         sliderInput("discrete_ylim","Adjust the y axis",
                                     min = 0, max=1, value=0.6,
                                     ticks=F, step=0.02)
                  ))
  )
)