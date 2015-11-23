# server.R
require(jsonlite)
require(RCurl)
require(ggplot2)
require(dplyr)
require(reshape2)
require(shiny)

shinyServer(function(input, output) {
  
  KPI_Low_Max_value <- reactive({input$KPI1})     
  KPI_Medium_Max_value <- reactive({input$KPI2})
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.50 })
  observeEvent(input$dark, { rv$alpha <- 0.75 })
  
  df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                                                 "select color, clarity, sum_price, round(sum_carat) as sum_carat, kpi as ratio, 
                                                                                 case
                                                                                 when kpi < "p1" then \\\'03 Low\\\'
                                                                                 when kpi < "p2" then \\\'02 Medium\\\'
                                                                                 else \\\'01 High\\\'
                                                                                 end kpi
                                                                                 from (select color, clarity, 
                                                                                 sum(price) as sum_price, sum(carat) as sum_carat, 
                                                                                 sum(price) / sum(carat) as kpi
                                                                                 from diamonds
                                                                                 group by color, clarity)
                                                                                 order by clarity;"
                                                                                 ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_UTEid', PASS='orcl_UTEid', 
                                                                                                   MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_Low_Max_value(), p2=KPI_Medium_Max_value()), verbose = TRUE)))
  })
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title=isolate(input$title)) +
      labs(x=paste("COLOR"), y=paste("CLARITY")) +
      layer(data=df1(), 
            mapping=aes(x=COLOR, y=CLARITY, label=SUM_PRICE), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=COLOR, y=CLARITY, fill=KPI), 
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
  
  # Begin code for Second Tab:
  TEXAS_SALARIES <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from TEXAS_SALARIES"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_bth679', PASS='orcl_bth679', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  df2 <- TEXAS_SALARIES %>% group_by(DEPARTMENT) %>% mutate(AVERAGE = mean(ANNUAL_SALARY)) %>% mutate(REF_LINE = mean(AVERAGE))
  
  output$distPlot2 <- renderPlot(height=500, width=1000, {
    plot1 <- ggplot() + 
      coord_cartesian() + 
      coord_flip() +
      facet_wrap("DEPARTMENT", ncol=1) +
      #labs(title='Diamonds Barchart\nAVERAGE_PRICE, WINDOW_AVG_PRICE, ') +
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
  
  # Begin code for Third Tab:
  
  df3 <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from alaska_oilwell"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_bth679', PASS='orcl_bth679', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
  output$distPlot3 <- renderPlot(height=500, width=1000, {
    plot3 <- ggplot(data = df3, aes(x = LONGITUDE, y = LATITUDE, color = OPERATOR_NAME), width = 300, height = 300) + geom_point() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
    plot3
  })
  
})