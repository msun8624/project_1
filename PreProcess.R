# generate data
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(stringr)

# read accident.csv file
accident1 <- read.csv('./datasets/us-vehicle-accidents/join_table/accident.csv', 
                      stringsAsFactors = F)
# read US GLC codes
city_codes <- read.csv('./datasets/us-vehicle-accidents/us-GLC-codes/GLCs_for_the_USA_and_DC.csv',
                       stringsAsFactors = F)
# read US FIPS codes
county_codes <- read.csv('./datasets/us-traffic/us-geocodes/us_fips_codes.csv',
                         stringsAsFactors = F)
# change column names
county_t <- county_codes %>% select(STATE.NAME = State, 
                                    COUNTY.NAME = County.Name,
                                    STATE = FIPS.State,
                                    COUNTY = FIPS.County)
# populate US State names
accident1 <- accident1 %>% left_join(., 
                        county_t %>% select(STATE.NAME, STATE) %>% distinct(),
                        by = 'STATE')
# populate US county names for each State
accident1 <- accident1 %>% left_join(., county_t %>% select(STATE, COUNTY, COUNTY.NAME),
                        by = c('STATE', 'COUNTY'))
# populate state names abbreviation 
city_t <- city_codes %>% select(STATE = State.Code,
                      STATE_AB = State.Abbreviation,
                      COUNTY = County.Code,        
                      CITY = City.Code,
                      CITY.NAME = City.Name.County.Name)
accident1 <- accident1 %>% left_join(., city_t %>% select(STATE, STATE_AB) %>%
                                         distinct(), by = 'STATE')
# populate available city names
accident1 <- accident1 %>%  left_join(., city_t %>% select(., -STATE_AB), 
                                      by = c('STATE', 'COUNTY', 'CITY'))

accident1 <- accident1 %>% select(ST_CASE, STATE = STATE.NAME, STATE_AB,
                                   COUNTY = COUNTY.NAME, CITY = CITY.NAME, 
                                   DAY,  MONTH, YEAR, DAY_WEEK, HOUR, MINUTE, 
                                   LATITUDE, LONGITUDE = LONGITUD, WEATHER1)

# populate weather condition
weather_df <- data.frame(WEATHER1 = c(0:8, 10:12, 98, 99), 
                         WEATHER = c('No Adserve', 'Clear',
                                     'Rain', 'Sleet', 'Snow',
                                     'Fog','Severe Crosswinds',
                                     'Blowing Sand', 'Other',
                                     'Cloudy','Blowing Snow',
                                     'Freezing Rain','Not Reported',
                                     'Unknown'))
accident1 <- accident1 %>% left_join(., weather_df, by = "WEATHER1")

# remove weather1
accident1 <- accident1 %>% select(., -WEATHER1) 

# populate state income and population
state_pop <- read.csv('./datasets/us-statistics/us-population-by-state-and-count.csv',
                      stringsAsFactors = F)
state_income <- read.csv('./datasets/us-statistics/kaggle_income.csv',
                      stringsAsFactors = F)
colnames(state_pop) <- toupper(colnames(state_pop))
accident1 <- accident1 %>% left_join(state_pop, by = 'STATE')

state_income <- state_income %>% 
    select(STATE = State_Name, COUNTY = County, MEAN = Mean) %>%
    group_by(STATE) %>% summarise(INCOME = floor(mean(MEAN)))

accident1 <- accident1 %>% left_join(state_income, by = 'STATE')

# populate accidents table
accidents <- read.csv('./datasets/us-vehicle-accidents/join_table/ACC_AUX.CSV',
                      stringsAsFactors = F)
accidents <- accidents %>% select(ST_CASE, LAND_USE = A_RU,
                                  INTERSTATE = A_INTER, JUNCTION = A_JUNC,
                                  TRAFFICWAY = A_RELRD, ROAD_FUNCTION = A_ROADFC,
                                  COLLISION_MANNER = A_MANCOL, TIME_OF_DAY = A_TOD,
                                  DAY_OF_WEEK = A_DOW, DRUNKEN = A_POSBAC,
                                  ROAD_DEPARTURE = A_RD, VEHICLES_INVOLVED = A_CT)

# remove strings after symbol defined in sep
extract_str <- function(str_x, sep = '/') {
    extract_partial <- function(str_x) {
        if(str_detect(str_x, sep)){
            return(str_sub(str_x, 1, str_locate(str_x,sep)[1]-1))
        } else {
            return(str_x)
        }
    }
    extract_partial
}
accidents$TRAFFICWAY <- sapply(accidents$TRAFFICWAY, extract_str(sep = '/'))
accidents$ROAD_FUNCTION <- sapply(accidents$ROAD_FUNCTION, extract_str(sep = ','))
accidents$ROAD_DEPARTURE <- sapply(accidents$ROAD_DEPARTURE, extract_str(sep = '-'))
accidents$VEHICLES_INVOLVED <- sapply(accidents$VEHICLES_INVOLVED, extract_str(sep = '-'))

# replace long descriptions with short phrases
replace_str <- function(str_x, pattern, replacement){
    replace_partial <- function(str_x){
        return(str_replace(str_x, pattern, replacement))
    }
    replace_partial
}

accidents$COLLISION_MANNER <- sapply(accidents$COLLISION_MANNER, 
               replace_str(pattern = 'Not Collision with Motor Vehicle in Transport',
                           replacement = 'Stationary collision'))

replace_phrases <- function(str_x) {
    pattern1 <- 'Driver With Positive BAC Testing Crash'
    pattern2 <- 'All Drivers With ZERO BAC Testing Crash'
    pattern3 <- 'Unknown BAC Crash'
    replacement1 <- 'Positive BAC'
    replacement2 <- 'ZERO BAC'
    replacement3 <- 'Unknown BAC'
    if(str_detect(str_x, pattern1)) {
        return(str_replace(str_x, pattern1, replacement1))
    } else if( str_detect(str_x, pattern2)) {
        return(str_replace(str_x, pattern2, replacement2))
    } else if( str_detect(str_x, pattern3)) {
        return(str_replace(str_x, pattern3, replacement3))
    }
}

accidents$DRUNKEN <- sapply(accidents$DRUNKEN, replace_phrases)

# generate final accidents data frame
accidents <- accidents %>% left_join(accident1, by = 'ST_CASE')
accidents <- accidents %>% select(-ST_CASE)
colnames(accidents) <- tolower(colnames(accidents))
accidents <- accidents %>% filter(longitude < 0) %>% 
                      select(state, state_ab, population, income,
                             county, city, longitude, latitude,
                             year, month,day, hour, minute, time_of_day,
                             day_week, day_of_week, collision_manner,
                             land_use, road_function, interstate, junction,
                             trafficway, collision_manner, vehicles_involved,
                             drunken, road_departure, weather) %>%
                      group_by(state) %>% mutate(n_state = length(state), 
                             density_state = round(n_state*1000000/population)) %>%
                      ungroup()
density_mean_tb <- accidents  %>% select(state, population, n_state) %>% 
                   distinct() %>% mutate(n_nation = sum(n_state),
                                         population_nation = sum(population),
                                         density_nation = round(n_nation*1000000/ population_nation)) %>%
                   select(-population, -n_state, -population_nation)
accidents <- accidents %>% left_join(., density_mean_tb, by = 'state')

colnames(accidents) <- str_to_title(colnames(accidents))
write.csv(accidents, file = './project1/vehicle_accidents.csv', row.names = F)