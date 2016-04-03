library(shiny)

# Define UI for random distribution calculator 
shinyUI(fluidPage(
  
  # Title
  h2("Probability calculator", align="center",style="color: #7BB0BC;"),
  
  tabsetPanel(id="dist_type",
    tabPanel("Continuous distributions",value="continuous",
             source("continuous_UI.R", local=T)$value
             ),
    tabPanel("Discrete distributions",value="discrete",
             source("discrete_UI.R", local=T)$value)
  )
  

)
)
