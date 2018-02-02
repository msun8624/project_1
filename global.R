library(data.table)
library(geojsonio)
library(leaflet)


setwd('~/NYC_data_science/projects/')
accidents <- fread(file = './project1/vehicle_accidents.csv')
collision_total <- accidents %>% filter(Hour < 24) %>%
    select(Hour, Collision_manner, Road_function) 
states <- geojson_read('./project1/us_states_accidents_density.json', what = "sp")
bins <- c(100, 200, 300, 400, 500, 600, 700)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)