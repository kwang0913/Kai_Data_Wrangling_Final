library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(maptools)
library(leaflet)
library(sp)
library(httr)
library(jsonlite)

points_to_line <- function(data, long, lat, id_field) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c("long", "lat")
  
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

function(input, output) {
  
  #### Filter Data ####
  df_map <- reactive({
    df2 <- citibike_df %>% 
      filter(starttime >= input$dates[1] & stoptime <= input$dates[2] &
               hour >= input$hrs[1] & hour <= input$hrs[2] &
               gender %in% input$sex & usertype %in% input$user)
    
    nstart <- df2 %>%  group_by(start.station.latitude, start.station.longitude) %>% summarise(nstart = n())
    nend <- df2 %>%  group_by(end.station.latitude, end.station.longitude) %>% summarise(nend = n())
    dfmap <- full_join(nstart, nend, by = c("start.station.latitude" = "end.station.latitude",
                                            "start.station.longitude" = "end.station.longitude")) %>%
      na.omit() %>% 
      mutate(abs_change_perc = round(100*(nend-nstart)/nstart, 1)) 
    dfmap$rank <- dfmap$abs_change_perc %>% rank(ties.method = "first") 
    
    return(dfmap)
  })
  
  df_route <- reactive({
    df2 <- citibike_df %>% 
      filter(starttime >= input$dates[1] & stoptime <= input$dates[2] &
               hour >= input$hrs[1] & hour <= input$hrs[2] &
               gender %in% input$sex & usertype %in% input$user)
    
    #aggregated data for routes
    if(input$routes == TRUE){
      y <- df2 %>%  
        group_by(start.station.latitude, start.station.longitude, 
                 end.station.latitude, end.station.longitude) %>% 
        summarise(nroute = n()) %>% 
        arrange(desc(nroute)) %>% 
        head(100) %>% ungroup() %>% mutate(route_id = rownames(.)) %>% 
        gather(measure, val, -route_id) %>% group_by(route_id) %>%
        do(data.frame(   lat=c(.[["val"]][.[["measure"]] == "start.station.latitude"],
                               .[["val"]][.[["measure"]] == "end.station.latitude"]),
                         long = c(.[["val"]][.[["measure"]] == "start.station.longitude"],
                                  .[["val"]][.[["measure"]] == "end.station.longitude"]))) %>% 
        as.data.frame() %>% 
        points_to_line("long", "lat", "route_id") 
      
      return(y)
    } 
  })
  
  #### Interactive Map one ####
  
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
    
    #circles for staions
    leafletProxy("map", data = df_map()) %>%
      clearShapes() %>%
      addCircles(~start.station.longitude, ~start.station.latitude,
                 stroke = FALSE, fillOpacity = 1, color = ~pal(rank), radius = 70, 
                 popup = ~paste('Number of Trip Starts Here: ', nstart, '<br/>',
                                'Number of Trip End Here: ', nend, '<br/>',
                                'Rate of Bike Changes: ', abs_change_perc,' %', '<br/>') 
      ) 
    
    if (input$routes==TRUE) {
      leafletProxy("map", data = df_map()) %>% 
        addPolylines(data = df_route(),color="#56B4E9",weight = 2, opacity = 1)
      #add polyline for routes
    } 
  })
  
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
    leafletProxy("realtime", data = realtime()) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat,
                 stroke = FALSE, fillOpacity = 1, color = ~pal(num), radius = 70, 
                 popup = ~paste('<b><font color="Red">', 'Station: ', name, '</font></b><br/>',
                                'Numbers: ', num, '<br/>') 
      ) 
  })
  
  # roseplot
  rose <- reactive({
    df2 <- citibike_df %>% 
      filter(starttime >= input$dates[1] & stoptime <= input$dates[2] &
               hour >= input$hrs[1] & hour <= input$hrs[2] &
               gender %in% input$sex & usertype %in% input$user)
    
    #side absolut panel charts
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



