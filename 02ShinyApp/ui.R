#ui.R

require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barchart", tabName = "barchart", icon = icon("th")),
      menuItem("Scatter Plot", tabName = "scatterplot", icon = icon("th")),
      menuItem("Table", tabName = "table", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "crosstab",
              actionButton(inputId = "light", label = "Light"),
              actionButton(inputId = "dark", label = "Dark"),
              sliderInput("KPI1", "KPI_Low_Max_value:", 
                          min = 1, max = 40000,  value = 40000),
              sliderInput("KPI2", "KPI_Medium_Max_value:", 
                          min = 40000, max = 80000,  value = 80000),
              textInput(inputId = "title", 
                        label = "Crosstab Title",
                        value = "BALTIMORE SALARY"),
              actionButton(inputId = "clicks1",  label = "Click me"),
              plotOutput("distPlot1")
      ),
      
      # Second tab content
      tabItem(tabName = "barchart",
              plotOutput("distPlot2")
      ),
      
      # Second tab content
      tabItem(tabName = "scatterplot",
              plotOutput("distPlot3")
      ),
      
      # Fifth tab content
      tabItem(tabName = "table",
              dataTableOutput("table")
      )
    )
  )
)