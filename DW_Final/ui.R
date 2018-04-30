library(shiny)
library(shinydashboard)
library(leaflet)

animationOptions(interval = 800, loop = TRUE)

header <- dashboardHeader(
  title = "Citibike"
)

body <- dashboardBody(
  
  tabItems(
    
    # First tab content 
    tabItem(tabName = "History",
            fluidRow(
              column(width = 9,
                     box(width = NULL,
                         tags$style(type = "text/css", "#map {height:calc(100vh - 130px) !important;}"),
                         leafletOutput("map")
                     )
              ),
              
              column(width = 3,
                     box(width = NULL,
                         
                         h3("Station Explorer"),
                         
                         checkboxGroupInput("sex", "Gender", 
                                            choices = c("Female" = 2, "Male" = 1, "Unkown" = 0),
                                            selected = c(2, 1, 0)
                         ),
                         
                         checkboxGroupInput("user", "User Type", 
                                            choices = c("Customer", "Subscriber"),
                                            selected = c("Customer", "Subscriber")
                         ),
                         
                         sliderInput("hrs", h3("Hour Range"), min = 0, max = 23, 
                                     value = c(0, 23), animate = TRUE),
                         
                         checkboxInput("routes", "Show Most Popular Routes", value = FALSE),
                         
                         dateRangeInput("dates", h3("Date range"), min = '2013-07-01', max = '2017-12-31',
                                        start = '2017-08-01', end = '2017-08-01'),
                         
                         plotOutput("roseplot", height = 200)       
                     )
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "Realtime",
            fluidRow(
              box(background = "maroon",
                  radioButtons("type", "Type", inline = T,
                               choices = c("Capacity" = "capacity",
                                           "Avaliable Bikes" = "num_bikes_available")
                  )
              )
            ),
            
            fluidRow(
              column(width = 12,
                     box(width = NULL,
                         tags$style(type = "text/css", "#realtime {height:calc(100vh - 80px) !important;}"),
                         leafletOutput("realtime")
                     )
              )
            )
    )
  )
)

sider <- dashboardSidebar(
  collapsed = T,
  sidebarMenu(
    menuItem("History", tabName = "History", icon = icon("bicycle")),
    menuItem("Realtime", tabName = "Realtime", icon = icon("bolt"))
  )
)

dashboardPage(
  skin = "red",
  header,
  sider,
  body
)