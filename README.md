# Calculator
R Shiny implementation of a probability calculator

Shiny is an R library for creating interactive web applications. 
This simple app allows the user to calculate probabilities from well known probability distributions. 
It also includes the possibility to explore the distributions graphically.  

The app can be run from R:

```
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
runGitHub("Calculator","TuomoNieminen")
```
