library(googleVis)
library(leaflet)
library(shiny)
library(shinydashboard)
library(maps)


shinyUI(dashboardPage(
    dashboardHeader(title = "Fatal Vehicle Accidents"),
    dashboardSidebar(
        
        sidebarUserPanel("Matthew Sun",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Analysis", tabName = "analysis", icon = icon("bar-chart", lib = "font-awesome")),
            menuItem("Cause", tabName = "cause", icon = icon("bar-chart", lib = "font-awesome")),
            menuItem("Insight", tabName = "insight", icon = icon("bar-chart", lib = "font-awesome"))
        ),
        
        sliderInput("Hour",
                    label = "Select Hour to Display",
                    min = 0,
                    max = 23,
                    value = c(1, 3),
                    step = 1,
                    animate = T),
        
        selectizeInput('State',
                       label = 'Select State to Display',
                       choices = c('All', state.name),
                       selected = 'New York',
                       multiple = T,
                       options = list(
                           'plugins' = list('remove_button'),
                           'create' = T, 'persist' = F)
                       ),
        
        div(style="text-align:center","The dataset covers the year 2015"),
        div(style="text-align:center","Sources: ftp.nhtsa.dot.gov, www.gsa.gov")
    ),
    
    dashboardBody(
#       tags$head(
#           tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
#       ),
        
        tags$head(tags$style(HTML('
                                  .user-panel>.info{
                                  background:none ;
                                  }
                                  '))),   
        
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(infoBoxOutput("maxBox"),
                             infoBoxOutput("minBox"),
                             infoBoxOutput("avgBox")),
                    fluidRow(box(leafletOutput("mymap"), width = 12, height = 435)),
                    h4('- The above map consists of fatal vehicle accidents collected during 2015 in US. It interactively visualize the accidents distribution by state and by hour.')
            ),
          
            tabItem(tabName = "analysis",
                    fluidRow(box(plotOutput("qtyPlot"), width = 12)),
                    h4('- The above bar chart compares the accidents volume with the accidents density. The volume is normalized by the population of each state.'),
                    br(),
                    fluidRow(box(plotOutput("trendPlot"), width = 12)),
                    h4('- The above area chart visualize the collision manner time distribution during the 24 hour period. Each hour data is aggregated over one year.'),
                    br()
            ),
            
            tabItem(tabName = "cause",
                    fluidRow(
                        box(plotOutput("roadPlot"), width = 6),
                        box(plotOutput("drunkenPlot"), width = 6)
                    ),
                    fluidRow(
                        box(plotOutput("weatherPlot"), width = 6),
                        box(width = 6, height = 425,
                            tags$ul(tags$li(tags$h3('These three bar charts demonstrate the relationship between what and how for the fatal accidents'))),
                            tags$ul(tags$li(tags$h3('What - what caused the accidents.'))),
                            tags$ul(tags$li(tags$h3('How - how vehicle was impacted when accidents happened.'))))
                    )
            ),
            
            tabItem(tabName = "insight",
                    fluidRow(
                        box(plotOutput("stationPlot"), width = 6),
                        box(plotOutput("anglePlot"), width = 6)
                    ),
                    fluidRow(
                        box(plotOutput("headonPlot"), width = 6),
                        box(width = 6, height = 425,
                            tags$ul(tags$li(tags$h3('Three area charts explore the trend of three most common vehicle collision manners with time.'))),
                            tags$ul(tags$li(tags$h3('The amazing finding is majority of the fatal accidents happened in clear weather and the other vehicle involved was in stationary position.'))),
                            tags$ul(tags$li(tags$h3('This trend fits well with the AI future - autopilot of the vehicles.'))))
                    )
            )
        )
    )
))