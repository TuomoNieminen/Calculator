# Calculator
R Shiny implementation of a statistics probability calculator

Shiny is an R library for creating interactive web applications. 
This simple app allows the user to calculate probabilities or quantiles from well known probability distributions. 
It also includes the possibility to explore the distributions graphically.  

The app is hosted [here](https://tuomonieminen.shinyapps.io/Calculator/) and can also used locally from R:

```
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
runGitHub("Calculator","TuomoNieminen")
```
