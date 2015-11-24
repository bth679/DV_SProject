#ui.R
require(shiny)
require(shinydashboard)
require(leaflet)


dashboardPage( skin = "red",
  dashboardHeader(title = "Shiny Project - Texas vs. Baltimore Salaries",
                  titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Scatter Plot", tabName = "scatterplot", icon = icon("th")),
      menuItem("Barchart", tabName = "barchart", icon = icon("bar-chart-o")),
      menuItem("Crosstab", tabName = "crosstab", icon = icon("dashboard"))
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
              plotOutput("distPlot2"),
              h1("Analysis: "),
              p("In this plot, we try to discover the average annual salary in each department. From the plot, we see that the jobs in Texas Department of Criminal Justice seems to get paid higher than the other departments. However, its average isn't as high as jobs are in Texas Department of Transportation. Also, there are more jobs offered in Texas Department of Criminal Justice than the others. The red reference lines represents the average of annual salary in each Department.")
      ),
      
      # Third tab content
      tabItem(tabName = "scatterplot",
              plotOutput("distPlot3"),
              h1("Analysis: "),
              p("From this scatterplot, we can tell that except Police Department, Fire Department, Waste Water, Law Department are clustering and do not have a clear relationship between Annual Salary and Gross Salary, other roles in the following agencies, Circuit Court, City Council, Community Relations, Company Audits, Company Controllers/ Communications, DFW-Administration etc. all share a high positive correlation coefficient and can be accurately modeled with a linear model.")
      )
    )
  )
)