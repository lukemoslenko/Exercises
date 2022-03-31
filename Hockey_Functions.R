### 
## Hockey Functions
## Luke Moslenko
## March 27th 2022
###


nhldataclean <- function(dataframe){
  
  train_pbp <- dataframe
  
  fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")
  
  normal_strength_state <- c("5v5", "5v4", "4v5", "5v3", "3v5", "4v3", "3v4", "6v5", "5v6")
  
  
  ## is the event from the home team
  is_home <- function(dataframe){
    dataframe$is_home <- ifelse(dataframe$event_team == 
                                  dataframe$home_name, 1 , 0)
    return(dataframe)
  }
  
  ## is the event a goal
  is_goal <- function(dataframe){
    dataframe$is_goal <- ifelse(dataframe$event_type == "GOAL", 1, 0)
    return(dataframe)
  }
  
  
  ## Filter out shootout & Additional Overtimes
  train_pbp <- train_pbp[train_pbp$period < 5,]
  
  ## Determine if event is by the home team
  train_pbp <- is_home(train_pbp)
  ## Determine if event is a goal
  train_pbp <- is_goal(train_pbp)
  
  ## Set up time difference between events
  train_pbp <- train_pbp %>% group_by(game_id) %>%
    arrange(event_idx, .by_group = TRUE) %>%
    mutate(time_diff = game_seconds - lag(game_seconds))
  
  
  train_pbp$time_diff[is.na(train_pbp$time_diff)] <- 0
  train_pbp$is_home[is.na(train_pbp$is_home)] <- 0
  
  ## Determine if shot is a rebound
  train_pbp$is_rebound <- ifelse(train_pbp$time_diff <= 2 & 
                                   train_pbp$event_type %in% fenwick_events &
                                   train_pbp$event_team == 
                                   lag(train_pbp$event_team),
                                 1, 0)
  
  train_pbp$is_rebound[is.na(train_pbp$is_rebound)] <- 0
  
  ## Determine if shot is on the rush
  train_pbp$is_rush <- ifelse(train_pbp$time_diff > 4 &
                                lag(abs(train_pbp$x_fixed)) < 60 &
                                train_pbp$event_type %in% fenwick_events,
                              1, 0)
  
  train_pbp$is_rush[is.na(train_pbp$is_rush)] <- 0
  
  
  ## Set up secondary type and strengh state as factors
  train_pbp$secondary_type <- as.factor(train_pbp$secondary_type)
  train_pbp$strength_state <- as.factor(train_pbp$strength_state)
  train_pbp$event_goalie_name  <- as.factor(train_pbp$event_goalie_name)
  train_pbp$event_goalie_name <- ifelse(is.na(train_pbp$event_goalie_name), "EMPTYNET", as.factor(train_pbp$event_goalie_name))
  
  
  ## Filter out unusual strength states
  train_pbp <- train_pbp %>% filter(strength_state %in% normal_strength_state)
  
  
  ## remove NAs from coordinates data
  train_pbp <- filter(train_pbp, x != 'NA' & y != 'NA') 
  
  return(train_pbp)
  
}


TOI_calc <- function(dataframe){
  
  game_data <- train_pbp
  
  D <- game_data %>%
    filter(time_diff >0) %>%
    select(time_diff, event_team, event_team_type, event_type, secondary_type, home_on_1:home_on_6, away_on_1:away_on_6)
  
  D_pivot <-  D %>% 
    pivot_longer(cols = home_on_1:away_on_6, names_to = "on_ice", values_to = "event_player_1_name")
  
  TOI_player <- D_pivot %>% 
    group_by(event_player_1_name) %>%
    summarize(TOI = sum(time_diff) / 60)
  
  TOI_player$TOI60 <- TOI_player$TOI/60
  
  
  ## Filter for minimum 100 minutes
  TOI_player <- TOI_player %>% 
    filter(TOI > 100)
  
  
  return(TOI_player)
}
