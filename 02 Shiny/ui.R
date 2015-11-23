#ui.R 

library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

# Application title
headerPanel("Alaska Oil Well Scatterplot"),

# Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("KPI1", 
                "KPI_Low_Max_value:", 
                min = 1,
                max = 40000, 
                value = 40000),
    sliderInput("KPI2", 
                "KPI_Medium_Max_value:", 
                min = 40000,
                max = 80000, 
                value = 80000)
  ),

# Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
    #plotOutput("distTable")
  )
))
