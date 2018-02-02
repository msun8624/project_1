library(googleVis)
library(leaflet)
library(shiny)
library(maps)


source('./project1/helper.R')

shinyServer(function(input, output, session){
    
    output$mymap <- renderLeaflet({   
        leaflet(data = states) %>% addProviderTiles("Esri.WorldStreetMap") %>% 
            setView(lng = -93.85, lat = 37.45, zoom = 4)  %>%
            clearMarkers() %>%
            clearShapes() %>%
            addPolygons(
                fillColor = ~pal(density),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.5) %>%
            addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL, position = 'bottomright')
    })
    
    observeEvent(input$Hour, {
        accidents_state <- update_input(input$Hour, input$State, accidents)
        
        
        leafletProxy("mymap") %>%
            clearMarkers() %>%
            addCircleMarkers(data = accidents_state, ~Longitude, ~Latitude, radius = 3,
                             stroke = F, fillOpacity = 0.2, fillColor = 'red',
                             label = paste0('County: ', accidents_state$County, 
                                            '. Hourly Accidents in ', accidents_state$State_ab,   
                                            ': ', accidents_state$N_hour_state)) %>%
            setView(lng = -93.85, lat = 37.45, zoom = 4)
    })

    observeEvent(input$State, {
        accidents_state <- update_input(input$Hour, input$State, accidents)
        
        
        leafletProxy("mymap") %>%
            clearMarkers() %>%
            addCircleMarkers(data = accidents_state, ~Longitude, ~Latitude, radius = 3,
                             stroke = F, fillOpacity = 0.2, fillColor = 'red',
                             label = paste0('County: ', accidents_state$County, 
                                            '. Hourly Accidents in ', accidents_state$State_ab,   
                                            ': ', accidents_state$N_hour_state)) %>%
            setView(lng = -93.85, lat = 37.45, zoom = 4)
    })
    
    
    #reactive for infobox on Tab 1
#   accidents_state <- reactive({
#       accidents_updated <- update_input(input$Hour, input$State, accidents)
#       accidents_updated
#   })
    
    # show statistics using infoBox
   output$maxBox <- renderInfoBox({
       accidents_state <- update_input(input$Hour, input$State, accidents)
       max_state <- accidents_state$State[1]
       max_value <- accidents_state$N_hour_state[1]
       max_value_state <- accidents_state$N_state[1]
       max_density <- accidents_state$Density_state[1]
       infoBox(paste('MAX:',max_state), 
               value = tags$p(HTML(paste('Hr-Qty.:',max_value, br(), 'D-Qty:',max_value_state, br(),
                                    'Density:', max_density)), style = 'font-size: 80%;'), 
               icon = icon("ambulance"))
   })
   
   output$minBox <- renderInfoBox({
       accidents_state <- update_input(input$Hour, input$State, accidents)
       min_state <- accidents_state$State[nrow(accidents_state)]
       min_value <- accidents_state$N_hour_state[nrow(accidents_state)]
       min_value_state <- accidents_state$N_state[nrow(accidents_state)]
       min_density <- accidents_state$Density_state[nrow(accidents_state)]
       infoBox(paste('MIN:',min_state), 
               value = tags$p(HTML(paste('Hr-Qty.:',min_value, br(), 'D-Qty:',min_value_state, br(),
                                    'Density:', min_density)), style = 'font-size: 80%;'), 
               icon = icon("ambulance"))
   })
    
   output$avgBox <- renderInfoBox({
       accidents_state <- update_input(input$Hour, input$State, accidents)
       avg_state <- 'AVG: NATIONAL'
       avg_value <- accidents_state$N_hour_nation[1]
       avg_value_nation <- accidents_state$N_nation[1]
       avg_density <- accidents_state$Density_nation[1]
       infoBox(avg_state, 
               value = tags$p(HTML(paste('Hr-Qty.:',avg_value, br(), 'D-Qty:',avg_value_nation, br(),
                                    'Density:',avg_density)), style = 'font-size: 80%;'), 
               icon = icon("ambulance"))
   })
   
   # Render a barplot for tab 5
   output$qtyPlot <- renderPlot({
       qty_vs_density <- accidents %>%
           select(State, N_state, Density_state, Density_nation) %>%
           group_by(State) %>% distinct() %>% arrange(desc(N_state)) 
       
       g <- ggplot(qty_vs_density, aes(x=factor(State,  levels = unique(State)), y=N_state+Density_state))
       g + geom_col(aes(fill=State)) + guides(fill=F) + 
           theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 6.5)) +
           ylab('No. of Accidents') +
           xlab('US States') + 
           ggtitle('US Fatal Accidents Distribution - Quantity vs Density') +
           theme(plot.title = element_text(hjust = 0.5)) +
           geom_col(aes(x=factor(State,  levels = unique(State)),y=Density_state),
                    fill = 'black', alpha = 0.9, size = 0.25) + 
           geom_hline(aes(yintercept =Density_nation), 
                      linetype = 'solid', color = 'red', size = 0.5) +
           annotate('text', x='Vermont', y = 480, label = 'National Density\nLine (pp3m) - Red', 
                    size = 2.4, color = 'red') 
       
   })
   
   output$trendPlot <- renderPlot({
       collision_qty <- collision_total %>% 
           group_by(Hour,Collision_manner) %>% summarise(Num_collisions =n())
       
       g <- ggplot(collision_qty, aes(x=Hour, y=Num_collisions))
       g + geom_area(aes(fill = Collision_manner)) +
           ylab('No. of Accidents') +
           xlab('Hour') + 
           ggtitle('Collision Manner Time Distribution by Hour') +
           theme(plot.title = element_text(hjust = 0.5, size = 13)) +
           guides(fill = guide_legend('Collision Manner')) +
           theme(legend.title = element_text(size = 9), 
                 legend.position = 'bottom',
                 legend.key.size = unit(0.3,'cm'),
                 legend.key.height = unit(0.35,'cm'),
                 legend.text = element_text(size = 8))
   })
   
   output$roadPlot <- renderPlot({
       collision_rdeparture <- accidents %>%  group_by(Collision_manner, Road_departure) %>% 
           summarise(Num_collisions = n()) %>% group_by(Collision_manner) %>%
           mutate(Total = sum(Num_collisions)) %>%
           arrange(Total, Num_collisions) 
       
       g <- ggplot(collision_rdeparture, aes(x = factor(Collision_manner, 
                                                        levels = unique(Collision_manner)), y = Num_collisions))
       g + geom_bar(aes(fill = Road_departure), stat = 'identity', position = 'dodge') +
           theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8.5)) +
           ylab('No. of Accidents') +
           xlab('Collision Manner') + 
           ggtitle('Impact of Road Departure on Fatal Accidents') +
           theme(plot.title = element_text(hjust = 0.5, size = 11)) +
           guides(fill = guide_legend('Road Departure')) +
           theme(legend.title = element_text(size = 8), 
                 legend.position = 'bottom',
                 legend.key.size = unit(0.2,'cm'),
                 legend.text = element_text(size = 7))
   })
   
   output$drunkenPlot <- renderPlot({
       collision_drunken <- accidents %>% group_by(Collision_manner, Drunken) %>%
           summarise(Num_collisions = n()) %>% group_by(Collision_manner) %>% 
           mutate(Total = sum(Num_collisions)) %>%
           arrange(Total, Num_collisions) 
       
       g <- ggplot(collision_drunken, aes(x = factor(Collision_manner, 
                                                     levels = unique(Collision_manner)), y = Num_collisions))
       g + geom_bar(aes(fill = Drunken), stat = 'identity', position = 'dodge') +
           theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8.5)) +
           ylab('No. of Accidents') +
           xlab('Collision Manner') + 
           ggtitle('Impact of Drunken on Fatal Accidents') +
           theme(plot.title = element_text(hjust = 0.5, size = 11)) +
           guides(fill = guide_legend('Drunken - BAC')) +
           theme(legend.title = element_text(size = 8), 
                 legend.position = 'bottom',
                 legend.key.size = unit(0.2,'cm'),
                 legend.text = element_text(size = 7))
   })
   
   output$weatherPlot <- renderPlot({
       collision_weather <- accidents %>% group_by(Weather, Collision_manner) %>%
           summarise(Num_collisions = n()) %>% group_by(Weather) %>% 
           mutate(Total = sum(Num_collisions)) %>%
           arrange(Total, Num_collisions) 
       
       top_5_weather <- accidents %>% select(Weather, Collision_manner) %>%
           group_by(Weather) %>% summarise(Num_collisions = n()) %>%
           distinct() %>% top_n(5, wt = Num_collisions)
       
       collision_weather <- collision_weather %>% filter(Weather %in% top_5_weather$Weather)
       
       g <- ggplot(collision_weather, aes(x = factor(Weather, 
                                                     levels = unique(Weather)), y = Num_collisions))
       g + geom_bar(aes(fill = Collision_manner), stat = 'identity', position = 'dodge') +
           theme(axis.text.x = element_text(angle = 50, hjust = 1, size = 8.5)) +
           ylab('No. of Accidents') +
           xlab('Weather') + 
           ggtitle('Impact of Weather on Fatal Accidents - Top 5') +
           theme(plot.title = element_text(hjust = 0.5, size = 11)) +
           guides(fill = guide_legend('Collision Manner')) +
           theme(legend.title = element_text(size = 8), 
                 legend.position = 'bottom',
                 legend.key.size = unit(0.2,'cm'),
                 legend.key.height = unit(0.25,'cm'),
                 legend.text = element_text(size = 7))
   })
   
   output$stationPlot <- renderPlot({
       collision_station <- collision_total %>% 
           filter(Collision_manner=='Stationary collision') %>%
           group_by(Hour, Road_function) %>%
           summarise(Num_collisions =n())
       
       g <- ggplot(collision_station, aes(x=Hour, y=Num_collisions))
       g + geom_area(aes(fill = Road_function)) +
           ylab('No. of Accidents') +
           xlab('Hour') + 
           ggtitle('Trend for Collision Manner - Stationary Collision') +
           theme(plot.title = element_text(hjust = 0.5, size = 11)) +
           guides(fill = guide_legend('Road Function')) +
           theme(legend.title = element_text(size = 8), 
                 legend.position = 'bottom',
                 legend.key.size = unit(0.2,'cm'),
                 legend.key.height = unit(0.25,'cm'),
                 legend.text = element_text(size = 7))
   })
   
   output$anglePlot <- renderPlot({
       collision_angle <- collision_total %>% 
           filter(Collision_manner=='Angle') %>%
           group_by(Hour, Road_function) %>%
           summarise(Num_collisions =n())
       
       g <- ggplot(collision_angle, aes(x=Hour, y=Num_collisions))
       g + geom_area(aes(fill = Road_function)) +
           ylab('No. of Accidents') +
           xlab('Hour') + 
           ggtitle('Trend for Collision Manner - Angle') +
           theme(plot.title = element_text(hjust = 0.5, size = 11)) +
           guides(fill = guide_legend('Road Function')) +
           theme(legend.title = element_text(size = 8), 
                 legend.position = 'bottom',
                 legend.key.size = unit(0.2,'cm'),
                 legend.key.height = unit(0.25,'cm'),
                 legend.text = element_text(size = 7))
   })
   
   output$headonPlot <- renderPlot({
       collision_headon <- collision_total %>% 
           filter(Collision_manner=='Head-On') %>%
           group_by(Hour, Road_function) %>%
           summarise(Num_collisions =n())
       
       g <- ggplot(collision_headon, aes(x=Hour, y=Num_collisions))
       g + geom_area(aes(fill = Road_function)) +
           ylab('No. of Accidents') +
           xlab('Hour') + 
           ggtitle('Trend for Collision Manner - Head-On') +
           theme(plot.title = element_text(hjust = 0.5, size = 11)) +
           guides(fill = guide_legend('Road Function')) +
           theme(legend.title = element_text(size = 8), 
                 legend.position = 'bottom',
                 legend.key.size = unit(0.2,'cm'),
                 legend.key.height = unit(0.25,'cm'),
                 legend.text = element_text(size = 7))
   })
})