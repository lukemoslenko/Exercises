
## Hockey Stats Learning

## Load Packages

library(tidyverse)

## load data
game_data <- train_pbp


## Which pp generated the best shot rate in each game?
    ## 1) Filter down to 5v5 events with game time only
    ## 2) corsis events or events with time only

PP <- game_data %>%
  filter(game_strength_state %in% c("5v4", "4v5") & 
           (event_length > 0 | event_type %in% c("SHOT", "GOAL", "MISS", "BLOCK")))


## Create a new set of PP variable to have that data in the same place
#The game_strengh_state variable always uses the home team's strengh state first

PP <- PP %>%
  mutate(PP_1 = ifelse(game_strength_state == "5v4", home_on_1, away_on_1),
         PP_2 = ifelse(game_strength_state == "5v4", home_on_2, away_on_2),
         PP_3 = ifelse(game_strength_state == "5v4", home_on_3, away_on_4),
         PP_4 = ifelse(game_strength_state == "5v4", home_on_4, away_on_4),
         PP_5 = ifelse(game_strength_state == "5v4", home_on_5, away_on_5),
         PP_6 = ifelse(game_strength_state == "5v4", home_on_6, away_on_6),
         PP_goalie = ifelse(game_strength_state == "5v4", home_goalie, away_goalie),
         PP_team = ifelse(game_strength_state == "5v4", home_team, away_team))

## Create a new PP_line variable that has the on ice players
PP <- PP %>% 
  unite(PP_line, PP_1:PP_6, sep = "-", remove = FALSE)

## Create a function to 1) remove the goalie from the powerplay and 2) remove special characters

create_line <- function(line, goalie) {
  line <- str_replace_all(line, goalie, "")
  line <- str_replace_all(line, c("--" = "-", "^-" = "", "-$" = ""))
}

## PP_line without goalie
PP <- PP %>%
  mutate(PP_line = create_line(PP_line, PP_goalie))

## Create a corsi for (CF) Variable to identify shot attempts
PP <- PP %>%
  mutate(CF = ifelse(event_type %in% c("GOAL", "SHOT", "MISS", "BLOCK") &
    event_team == PP_team, 1, 0))

##Group by power play line, summarize the TOI & CF
# Filter down to powerplay lines that played at least two mintes
#create a corsi per 60 minutesvariable and sort

PP_group <- PP %>% 
  group_by(PP_team, PP_line) %>%
  summarize(TOI = sum(event_length) / 60,
            CF = sum(CF)) %>%
    filter(TOI >= 1) %>%
    mutate(CF_60 = (CF * 60 / TOI)) %>% 
          arrange(desc(CF_60))


# Which Defensemen played the more 5v5 minutes ----------------------------

D <- game_data %>%
  filter(time_diff >0) %>%
  select(time_diff, event_team, event_team_type, event_type, secondary_type, home_on_1:home_on_6, away_on_1:away_on_6)

D_pivot <-  D %>% 
  pivot_longer(cols = home_on_1:away_on_6, names_to = "on_ice", values_to = "event_player_1_name")

D_player <- D_pivot %>% 
  group_by(event_player_1_name) %>%
  summarize(TOI = sum(time_diff) / 60)




