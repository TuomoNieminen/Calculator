library(shiny)

# Define UI for random distribution calculator 
shinyUI(fluidPage(
  
  # Title
  br(),
  fluidRow(
           column(11,
              h2("Probability calculator", align="center",style="color: #7BB0BC;")),
           column(1, tags$a(img(id ="github", src = 'github.PNG'), 
                            href = "https://github.com/TuomoNieminen/Calculator"))),
  
  tabsetPanel(id="dist_type",
    tabPanel("Continuous distributions",value="continuous",
             source("ui_continuous.R", local=T)$value
             ),
    tabPanel("Discrete distributions",value="discrete",
             source("ui_discrete.R", local=T)$value)
  ),
  
hr(),
div(align = "center", "Tuomo Nieminen 2016"),
br()
)
)
