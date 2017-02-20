
![](https://github.com/TuomoNieminen/Calculator/blob/master/app.PNG)


# About  

Welcome to the Calculator repository. This simple interactive app allows the user to calculate probabilities or quantiles from well known probability distributions. A unique aspect is a possibility to explore the distributions graphically and define the desired probabilities by interacting with the graphics.

R is a programming language for data analysis. Shiny is an R library for creating interactive web applications. This repository includes the codes for an R Shiny implementation of a statistics probability calculator.

# Usage

The app is hosted [here](https://tuomonieminen.shinyapps.io/Calculator/) and can also used locally from R:

```
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
runGitHub("Calculator","TuomoNieminen")
```
