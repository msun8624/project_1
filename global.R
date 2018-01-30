library(data.table)

setwd('~/NYC_data_science/projects/')
accidents <- fread(file = './project1/vehicle_accidents.csv')
collision_total <- accidents %>% filter(Hour < 24) %>%
    select(Hour, Collision_manner, Road_function) 