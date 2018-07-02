#### Load Packages ####
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(maptools)
library(leaflet)
library(sp)
library(httr)
library(jsonlite)

#### Load Dataset ####
load("test.RData")

#### Build UI ####
# Break each element in part and build them up is good in syntax and good for adjustment
box111 <- box(width = NULL,
              tags$style(type = "text/css", "#map {height:calc(100vh - 130px) !important;}"),
              leafletOutput("map")
)

box112 <- box(width = NULL,
              h3("Station Explorer"),
              
              checkboxGroupInput("sex", "Gender", 
                                 choices = c("Female" = 2, "Male" = 1, "Unkown" = 0),
                                 selected = c(2, 1, 0)
              ),
              
              checkboxGroupInput("user", "User Type", 
                                 choices = c("Customer", "Subscriber"),
                                 selected = c("Customer", "Subscriber")
              ),
              
              sliderInput("hrs", h3("Hour Range"), 
                          min = 0, max = 23, 
                          value = c(0, 23), animate = TRUE),
              animationOptions(interval = 800, loop = TRUE),
              
              checkboxInput("routes", "Show Most Popular Routes", value = FALSE),
              
              dateRangeInput("dates", h3("Date range"), 
                             min = '2013-07-01', max = '2017-12-31',
                             start = '2017-08-01', end = '2017-08-01'),
              
              plotOutput("roseplot", height = 200)       
)

box211 <- box(background = "maroon",
              radioButtons("type", "Type", inline = T,
                           choices = c("Capacity" = "capacity",
                                       "Avaliable Bikes" = "num_bikes_available")
              )
)

box221 <- box(width = NULL,
              tags$style(type = "text/css", "#realtime {height:calc(100vh - 80px) !important;}"),
              leafletOutput("realtime")
)

tab1 <- tabItem(tabName = "History",
                fluidRow(column(width = 9, box111), column(width = 3, box112)
                )
)

tab2 <- tabItem(tabName = "Realtime",
                fluidRow(box211
                ),
                fluidRow(column(width = 12, box221)
                )
)

menu1 <- menuItem("History", tabName = "History", icon = icon("bicycle"))
menu2 <- menuItem("Realtime", tabName = "Realtime", icon = icon("bolt"))

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Citibike"),
                    dashboardSidebar(collapsed = T, sidebarMenu(menu1, menu2)),
                    dashboardBody(tabItems(tab1, tab2))
)

#### Build Function ####
points_to_line <- function(data, lng, lat, id_field) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c("lng", "lat")
  
  # Split into a list by ID field
  paths <- sp::split(data, data[[id_field]])
  
  sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
  
  for (p in 2:length(paths)) {
    id <- paste0("line", as.character(p))
    l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
    sp_lines <- spRbind(sp_lines, l)
  }
  
  return(sp_lines)
  
}

#### Define Server Logic ####
server <- function(input, output) {
  
  #### Filter Data ####
  df_map <- reactive({
    df2 <- citibike_df %>% 
      filter(starttime >= input$dates[1] & stoptime <= input$dates[2] &
               hour >= input$hrs[1] & hour <= input$hrs[2] &
               gender %in% input$sex & usertype %in% input$user)
    
    nstart <- df2 %>% 
      group_by(start.station.latitude, start.station.longitude) %>% summarise(nstart = n())
    
    nend <- df2 %>%  
      group_by(end.station.latitude, end.station.longitude) %>% summarise(nend = n())
    
    dfmap <- 
      full_join(nstart, nend, by = c("start.station.latitude" = "end.station.latitude",
                                     "start.station.longitude" = "end.station.longitude")) %>%
      na.omit() %>% mutate(abs_change_perc = round(100*(nend-nstart)/nstart, 1)) 
    
    dfmap$rank <- dfmap$abs_change_perc %>% rank(ties.method = "first") 
    
    return(dfmap)
  })
  
  #### Aggregated Data for Routes ####
  df_route <- reactive({
    df2 <- citibike_df %>% 
      filter(starttime >= input$dates[1] & stoptime <= input$dates[2] &
               hour >= input$hrs[1] & hour <= input$hrs[2] &
               gender %in% input$sex & usertype %in% input$user)
    
    if(input$routes == TRUE){
      y <- df2 %>%  
        group_by(start.station.latitude, start.station.longitude, 
                 end.station.latitude, end.station.longitude) %>% 
        summarise(nroute = n()) %>% arrange(desc(nroute)) %>% 
        head(100) %>% ungroup() %>% mutate(route_id = rownames(.)) %>% 
        gather(measure, val, -route_id) %>% group_by(route_id) %>%
        do(data.frame(   lat = c(.[["val"]][.[["measure"]] == "start.station.latitude"],
                                 .[["val"]][.[["measure"]] == "end.station.latitude"]),
                         lng = c(.[["val"]][.[["measure"]] == "start.station.longitude"],
                                 .[["val"]][.[["measure"]] == "end.station.longitude"]))) %>% 
        as.data.frame() %>% points_to_line("lng", "lat", "route_id") 
      
      return(y)
    } 
  })
  
  #### Interactive Map One ####
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.960, lat = 40.753, zoom = 13) 
  })
  
  # Incremental changes to the map
  observe({
    pal <- colorNumeric(palette=colorRampPalette(c("#FF6666", "#ffa500","#66CC99"))(3000),
                        domain = df_map()$rank)
    
    #Circles for staions
    leafletProxy("map", data = df_map()) %>% clearShapes() %>%
      addCircles(~start.station.longitude, ~start.station.latitude,
                 stroke = FALSE, fillOpacity = 1, color = ~pal(rank), radius = 70, 
                 popup = ~paste('Number of Trip Starts Here: ', nstart, '<br/>',
                                'Number of Trip End Here: ', nend, '<br/>',
                                'Rate of Bike Changes: ', abs_change_perc,' %', '<br/>') 
      ) 
    
    if (input$routes==TRUE) {
      leafletProxy("map", data = df_map()) %>% 
        addPolylines(data = df_route(),color="#56B4E9",weight = 2, opacity = 1)
      #Add polyline for routes
    } 
  })
  
  #### Get Realtime Data from JSON ####
  realtime <- reactive({
    temp <- "http://gbfs.citibikenyc.com/gbfs/gbfs.json" %>% GET() %>% content(as = "text") %>% 
      fromJSON() %>% .[[3]] %>% .[[1]] %>% .[[1]] %>% .[str_detect(.[[1]],"station"), 2] %>% 
      map(GET) %>% map(content, as = "text") %>% map(fromJSON) %>% map(~.[[3]]) %>% map(~.[[1]]) %>% 
      plyr::join_all() %>% select(name, lat, lon, input$type)
    colnames(temp)[4] <- "num"
    return(temp)
  })
  
  #### Interactive Map Two ####
  # Create the map
  output$realtime <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.960, lat = 40.753, zoom = 13) 
  })
  
  # Incremental changes to the map
  observe({
    pal <- colorNumeric(palette=colorRampPalette(c("#FF6666","#66CC99"))(3), domain = realtime()$num)
    
    #circles for staions
    leafletProxy("realtime", data = realtime()) %>% clearShapes() %>%
      addCircles(~lon, ~lat,
                 stroke = FALSE, fillOpacity = 1, color = ~pal(num), radius = 70, 
                 popup = ~paste('<b><font color="Red">', 'Station: ', name, '</font></b><br/>',
                                'Numbers: ', num, '<br/>') 
      ) 
  })
  
  #### Roseplot ####
  rose <- reactive({
    df2 <- citibike_df %>% 
      filter(starttime >= input$dates[1] & stoptime <= input$dates[2] &
               hour >= input$hrs[1] & hour <= input$hrs[2] &
               gender %in% input$sex & usertype %in% input$user)
    
    #Side absolut panel charts
    df_hr <- df2 %>% group_by(hour) %>% summarise(ntrip = n())
    return(df_hr)
  })
  
  observe({
    output$roseplot <- renderPlot({
      p <- ggplot(rose(), aes(hour, ntrip)) +
        geom_bar(aes(fill = ntrip), stat = "Identity") + coord_polar() + 
        scale_fill_gradientn(colours = rainbow(7)) + theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) 
      print(p)
    })
  })
}

#### Run the Application ####
shinyApp(ui = ui, server = server)

