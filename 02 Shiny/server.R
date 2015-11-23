# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    KPI_Low_Max_value = 40000;
    KPI_Medium_Max_value = 80000;
    
    BALTIMORE_SALARY <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from BALTIMORE_SALARY"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_pp9774', PASS='orcl_pp9774', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
    
    df <- BALTIMORE_SALARY %>% group_by(AGENCY, JOBTITLE) %>% mutate(average = mean(ANNUALSALARY)) %>% mutate(KPI = ifelse(average <= KPI_Low_Max_value, '03 Low', ifelse(average <= KPI_Medium_Max_value, '02 Medium', '01 High')))
    
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title='Baltimore Salaries Crosstab\n') +
      labs(x=paste("AGENCY"), y=paste("JOBTITLE")) +
      
      layer(data=df, 
            mapping=aes(x=AGENCY, y=JOBTITLE, label=average), 
            stat="identity", 
            stat_params=list(), 
            geom="text",size = 3,
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      
      layer(data=df, 
            mapping=aes(x=AGENCY, y=JOBTITLE, fill=KPI), 
            stat="identity", 
            stat_params=list(), 
            geom="tile",
            geom_params=list(alpha=0.50), 
            position=position_identity()
      )
    
        return(plot)
  }) # output$distPlot
})
