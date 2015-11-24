#ui.R
require(shiny)
require(shinydashboard)
require(leaflet)

dashboardPage(
  dashboardHeader(title = "Medicare Data"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crosstab", tabName = "crosstab", icon = icon("th")),
      menuItem("Barchart", tabName = "barchart", icon = icon("bar-chart-o")),
      menuItem("ScatterPlot", tabName = "scatterplot", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
              tabName = "crosstab",
              actionButton(inputId = "light", label = "Light"),
              actionButton(inputId = "dark", label = "Dark"),
              sliderInput("KPI1", "KPI_maxAvg_State_Spending:", 
                          min = 1, max = 3000,  value = 3000),
              textInput(inputId = "title", 
                        label = "Crosstab",
                        value = "CrossTab Medicare Data"),
              actionButton(inputId = "clicks1",  label = "Click me"),
              plotOutput("distPlot1")
      ),
      
      # Second tab content
      tabItem(tabName = "barchart",
              actionButton(inputId = "clicks2",  label = "Click me"),
              checkboxGroupInput(inputId = "region", label = "Region", choices = c("central" = "central", "east" = "east", "west" = "west"), selected = "central"),
              plotOutput("distPlot2")
              
      ),
      
      # Third tab content
      tabItem(tabName = "scatterplot",
              fluidRow(
                box(actionButton(inputId = "clicks3",  label = "Click me"),
              checkboxGroupInput(inputId = "states", label = "Region", choices = c("AK","AL","AZ","AR", "CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY"), selected = "CA")),
              
              box(title = "Scatter Plot", background = "light-blue",plotOutput("distPlot3"))
      )
    )
  )
)
)
