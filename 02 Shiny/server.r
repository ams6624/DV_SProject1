# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)
require(DT)
require(dashboard)

shinyServer(function(input, output) {
  kpi_region <- reactive({input$region})
  
  kpi_input <- reactive({input$KPI1}) 
  input_states <- reactive({input$states})
  
  rv <- reactiveValues(alpha = 0.50)
  observeEvent(input$light, { rv$alpha <- 0.40 })
  observeEvent(input$dark, { rv$alpha <- 0.85 })
  
  medicare <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from medicare"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_alm3657', PASS='orcl_alm3657', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
  
#m5 <- medicare %>% group_by(STATE) %>% distinct(CLAIM_TYPE) %>% select(STATE, AVGSPENDPEREPISODE_ST_, AVGSPENDPEREPISODE_NAT_, CLAIM_TYPE) %>% filter(STATE %in%(c("TX", "CA", "WA", "KY", "FL"))) %>% mutate(kpi= ifelse( AVGSPENDPEREPISODE_ST_ <= kpi_input, 'Lower', 'Higher')) 
  
  df1 <- eventReactive(input$clicks1, {m5 <- medicare %>% group_by(STATE) %>% distinct(CLAIM_TYPE) %>% select(STATE, AVGSPENDPEREPISODE_ST_, AVGSPENDPEREPISODE_NAT_, CLAIM_TYPE) %>% filter(STATE %in%(c("TX", "CA", "WA", "KY", "FL"))) %>% mutate(kpi= ifelse( AVGSPENDPEREPISODE_ST_<= kpi_input() , 'Lower', 'Higher'))})
  
  output$distPlot1 <- renderPlot({             
    plot <- ggplot() + 
      coord_cartesian() + 
      scale_x_discrete() +
      scale_y_discrete() +
      labs(title=isolate(input$title)) +
      labs(x=paste("CLAIM TYPE"), y=paste("STATE")) +
      layer(data=df1(), 
            mapping=aes(x=CLAIM_TYPE, y=STATE, label=AVGSPENDPEREPISODE_ST_), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black"), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=CLAIM_TYPE, y=STATE, label=AVGSPENDPEREPISODE_NAT_), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", vjust=2), 
            position=position_identity()
      ) +
      layer(data=df1(), 
            mapping=aes(x=CLAIM_TYPE, y=STATE, fill=kpi), 
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
  
  
  df2 <- eventReactive(input$clicks2,{med2 <- medicare %>% select(STATE, AVGSPENDPEREPISODE_ST_, HOSPITAL_NAME)  %>% group_by(STATE) %>% summarize(num_hospitals = n_distinct(HOSPITAL_NAME), avgSpending = sum(AVGSPENDPEREPISODE_ST_))%>% arrange(desc(STATE)) %>%  mutate(reg= ifelse(STATE %in%(c( "WA", "OR","ID", "MT", "WY", "CA","NV", "CO","AZ", "NM", "UT")),"west", ifelse(STATE %in%(c("ND", "SD","NE", "MN", "IA", "WI","IL", "MO","KS", "OK","AR","LA", "TX")),"central", "east"))) %>% filter(reg %in% kpi_region())})
  
  
  output$distPlot2 <- renderPlot(height=500, width=800, {
    plot1 <- ggplot(data = df2(), 
                    mapping = aes(x=STATE, y=avgSpending, fill = num_hospitals))+
      scale_fill_gradient(
        low = "red",
        high = "blue") +
      geom_bar(position = "dodge", stat="identity") +
      labs(title='Medicare Spending per State') +
      labs(x=paste("State"), y=paste("Average Spending per State")) +
      layer(geom = "bar",
            position = "dodge",
            stat = "identity",
            data = df2(),
            mapping = aes(x =STATE))+
      geom_hline(yintercept = 2570966)+
      coord_flip()+
      layer(data=df2(), 
            mapping=aes(x=STATE, y=avgSpending, fill = num_hospitals, label = num_hospitals), 
            stat="identity", 
            stat_params=list(), 
            geom="text",
            geom_params=list(colour="black", hjust=-0.5), 
            position=position_identity()
      )
    plot1
  })
  
  # Begin code for Third Tab:
  
  df3 <- eventReactive(input$clicks3,{med3 <- medicare %>% filter(STATE %in% input_states())})

  
  output$distPlot3 <- renderPlot(height=300, width=500, {
    plot3 <- ggplot(data=df3(), aes(x=PERCENTSPEND_HOSP_, y=AVGSPENDINGPEREPISODE_HOSPI_)) +
      geom_point( size=4, shape=21, fill="blue", colour = "blue") +
      labs(title='Hospital Spending') +
      labs(x=paste("Percent Spending"), y=paste("Avg Spending per Episode"))
  
    plot3
  })
  
  
})
