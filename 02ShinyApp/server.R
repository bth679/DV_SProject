# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)

shinyServer(function(input, output) {
  
  ##############Code for Crosstab#############
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  labelsize2 <- reactive({input$labelsize2})
  labelsize3 <- reactive({input$labelsize3})

  BALTIMORE_SALARY <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BALTIMORE_SALARY"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pp9774', PASS='orcl_pp9774', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  df1 <- eventReactive(input$clicks1, {df <- BALTIMORE_SALARY %>% group_by(AGENCY, JOBTITLE) %>% mutate(KPI = ifelse(ANNUALSALARY <= KPI_Low_Max_value(), '03 Low', ifelse(ANNUALSALARY <= KPI_Medium_Max_value(), '02 Medium', '01 High')))})

  output$distPlot1 <- renderPlot(height=400, width=700,{          
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() + 
      scale_y_discrete() +
      labs(title='Baltimore Salaries Crosstab\n') +
      labs(x=paste("AGENCY"), y=paste("JOBTITLE")) +
      
      layer(data=df1(), 
            mapping=aes(x=AGENCY, y=JOBTITLE, label=ANNUALSALARY), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=AGENCY, y=JOBTITLE, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=rv$alpha), 
            position=position_identity()
      )
    plot

  }) 
  
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
  
  #############Code for Barchart################
  # Begin code for Second Tab:
  TEXAS_SALARIES <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from TEXAS_SALARIES"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_bth679', PASS='orcl_bth679', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  df2 <- TEXAS_SALARIES %>% group_by(DEPARTMENT) %>% mutate(AVERAGE = mean(ANNUAL_SALARY)) %>% mutate(REF_LINE = mean(AVERAGE))
  
  output$distPlot2 <- renderPlot(height=400, width=800, {
    plot1 <- ggplot() + 
      coord_cartesian() + 
      coord_flip() +
      facet_wrap("DEPARTMENT", ncol=1) +
      labs(title='Texas Salary Barchart\nANNUAL SALARY, JOB TITLE') +
      labs(x=paste("TITLE"), y=paste("ANNUAL_SALARY")) +
      layer(data=df2, 
            mapping=aes(x=TITLE, y=ANNUAL_SALARY),
            stat="identity",
            stat_params=list(),
            geom="bar",
            position=position_identity()
      ) +
      layer(data=df2, 
            mapping=aes(yintercept = REF_LINE), 
            geom="hline",
            geom_params=list(colour="red")
      )
    plot1
  })
  ############Code for Scatterplot##################
  # Begin code for Third Tab:
  
  df3 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from baltimore_salary"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pp9774', PASS='orcl_pp9774', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  output$distPlot3 <- renderPlot(height=400, width=800, {
    plot3 <- ggplot(data = df3, aes(x = ANNUALSALARY, y = GROSSPAY, color = AGENCY), width = 300, height = 300) + geom_point() + labs(title='Baltimore Salary Scatterplot\nANNUAL SALARY, GROSS PAY')
    plot3
  })

})
  
  