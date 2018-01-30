library(dplyr)

update_input <- function(input_hour, input_state, accidents_dt) {
    if(length(input_hour)>1){
        hour_range = seq(from = input_hour[1], to = input_hour[2])
    } else {
        hour_range = input_hour
    }
    
    if(length(input_state) > 1) {
        if('All' %in% input_state) {
            state_range = state.name
        } else {
            state_range = input_state
        }
    } else if(input_state == 'All') {
        state_range = state.name
    } else {
        state_range = input_state
    }
    
    accidents_update <- accidents_dt %>% filter(State %in% state_range  & Hour %in% hour_range) %>%
        select(State, State_ab, Longitude, Latitude, County, Hour, N_state, N_nation, 
               Density_state, Density_nation) %>% group_by(State) %>%
        mutate(N_hour_state = length(State), Ratio_state = N_hour_state/N_state) %>%
        ungroup()
    
    accidents_temp <- accidents_update %>% select(State, N_hour_state, N_nation) %>%
        distinct() %>% mutate(N_hour_nation = sum(N_hour_state), 
                              Ratio_nation = N_hour_nation/N_nation) %>%
        select(-N_nation, -N_hour_state)
    accidents_update <- accidents_update %>% left_join(., accidents_temp, by = 'State') %>%
        group_by(State) %>% arrange(desc(N_hour_state))
    
    return(accidents_update)
}