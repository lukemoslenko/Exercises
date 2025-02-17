
#### Hockey Data Analytics ####
### ES 8913 Final Project ###
### Luke Moslenko ###

# Source Functions and Packages --------------------------------------------------------
## Load Packages
library(hockeyR)
library(tidyverse)
library(lubridate)
library(sportyR)
library(pROC)


## Functions
source("C:/Users/user/Documents/Grad school/Winter 2022/Data Science/Exercises/Hockey_Functions.R")

# ## Load Play by Play Data by Season -------------------------------------

## 2021-2022 Season
pbp_21_22 <- load_pbp("2021-2022")

# 2020 - 2021 season
pbp_20_21 <- load_pbp("2020-2021")

# 2019 - 2020 season
pbp_19_20 <- load_pbp("2019-2020")

# 2018 - 2019 season
pbp_18_19 <- load_pbp("2018-2019")


# Training Data Set -------------------------------------------------------

## Combine 2018-19 & 19-20 seasons for training data
train_pbp <- rbind(pbp_18_19, pbp_19_20)

train_pbp <- nhldataclean(train_pbp)


### Filtering Data

fenwick_events <- c("MISSED", "SHOT", "GOAL") 

## filtering to fenwick events
Train_Fenwick_Data <- filter(train_pbp, event_type %in% fenwick_events) %>%
    filter(Train_Fenwick_Data, !is.na(secondary_type))  ## Remove NAs from shot type

## Remove NAs from event goalie
Train_Fenwick_Data$event_goalie_name <- ifelse(is.na(Train_Fenwick_Data$event_goalie_name), "EMPTYNET", as.factor(Train_Fenwick_Data$event_goalie_name))


# Test Data ---------------------------------------------------------------

Test_Fenwick_Data <- nhldataclean(pbp_20_21)

Test_Fenwick_Data <- filter(Test_Fenwick_Data, event_type %in% fenwick_events) 

Test_Fenwick_Data <-  filter(Test_Fenwick_Data, !is.na(secondary_type))## Remove NAs from shot type

## Remove NAs from event goalie
Test_Fenwick_Data$event_goalie_name <- ifelse(is.na(Test_Fenwick_Data$event_goalie_name), "EMPTYNET", as.factor(Test_Fenwick_Data$event_goalie_name))

Test_Fenwick_Data <- nhldataclean(Test_Fenwick_Data)

## Write or load data for next stage
#write.csv(Train_Fenwick_Data, file = "Train_Fenwick_data_v1.csv")

Train_Fenwick_Data <-  read.csv("Train_Fenwick_data_v1.csv")


# Setting model up --------------------------------------------------------

## xGmodel
xGmodel <- glm(is_goal ~ poly(shot_distance, 3, raw = TRUE) + 
                 poly(shot_angle, 3, raw = TRUE) + secondary_type + 
                 strength_state +
                 is_rebound +
                 is_rush +
                 event_goalie_name,
               data = Train_Fenwick_Data, 
               family = binomial(link = 'logit'))




# Save / Load Model -------------------------------------------------------


save(xGmodel, file = "xGmodel.rda")

## Load model
load("~/Grad school/Winter 2022/Data Science/Exercises/xGmodel.rda") 


# Model Evaluation --------------------------------------------------------

## Likelihood Ratio Test

## Simpler Model
xGmodel_2 <- glm(is_goal ~ poly(shot_distance, 3, raw = TRUE) + 
                   poly(shot_angle, 3, raw = TRUE),
                 data = Train_Fenwick_Data, 
                 family = binomial(link = 'logit'))

## compare deviance
anova(xGmodel, xGmodel_2, test = "Chisq")

xGmodel_1 <- step(xGmodel)

## H-L Test
pR2(xGmodel)


# Model Prediction --------------------------------------------------------


## Predict expected goals
xG <- predict(xGmodel, Test_Fenwick_Data , type = "response")

g <- roc(is_goal ~ xG, data = Test_Fenwick_Data)

g


# Visualization of xG on ice rink -----------------------------------------

## Assign Expected Goals to each X,Y coordinate on the rink
avg_xG_by_coord <- Train_Fenwick_Data %>% group_by(x_fixed, y_fixed) %>%
  subset(event_goalie_name < 112) %>%
 summarise(xg = mean(xG))


## Get abs of x coord so it is all the offensive zone
avg_xG_by_coord$abs_x <- abs(avg_xG_by_coord$x_fixed)


## Plot xG by coordinates
ggplot(avg_xG_by_coord, aes(abs_x, y_fixed, fill = xg)) + geom_raster() +
  scale_fill_gradient(low = 'white', high = 'black')+
  geom_vline(xintercept = 0, color = 'red') +
  geom_vline(xintercept = 25, color = 'blue') +
  geom_vline(xintercept = 88, color = 'red') +
  xlab('X Coordinates') + ylab('Y Coordinates') +
  labs(title = 'Average xG Value by Coordinate',
       subtitle = "Half of an NHL Rink",
       caption =  " * Excludes empty net goals",
       fill = "Expected Goals") +
  geom_tile(aes(x = 89.665, y = 0, width = 3.33, height = 6, fill = 1))


# Individual Game ---------------------------------------------------------

## How to filter for a specific game
  ## Select date & Add home team three letter abbreviation
game <- pbp_20_21 %>%
  filter(game_date == "2021-01-15" & home_abbreviation == "TBL")

## Split season by game
pbp_20_21$game_id <- as.factor(pbp_20_21$game_id)


## Setting up a shot visualization for a specific game

## Get the image of the team's logos
team_logos_colors <- team_logos_colors

team_logos <- team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbreviation) | team_abbr == unique(game$away_abbreviation)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)
## Set the transparency
transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}
## Filter events for just shots
shots <- game %>% filter(event_type %in% fenwick_events) %>%
  # adding team colors
  left_join(team_logos, by = c("event_team_abbr" = "team_abbr"))


# Shot plot on Rink -------------------------------------------------------

geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
  ) +
  geom_point(
    data = shots,
    aes(x_fixed, y_fixed, shape = secondary_type),
    size = 5,
      color =  ifelse(shots$event_type == "GOAL", "forestgreen" , "black"),
    stroke = 1.2
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
    subtitle = glue::glue(
      "{unique(game$game_date)}\n
    {unique(shots$away_abbreviation)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbreviation)}"
    ),
    caption = "data from hockeyR | plot made with sportyR", 
    shape = "Shot type"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )


# Game Expected Goals per Player ------------------------------------------

## Clean game data
game <- nhldataclean(game)

game <- game %>% filter(event_type %in% fenwick_events)

game$xG <- predict(xGmodel, game, type = "response")

## Group events by player and team, calculate expected and difference between prediction and reality
xg_player_game <- game %>%
  group_by(event_player_1_name, event_team) %>%
  summarise( xG = sum(xG), Goals = sum(is_goal), Difference = sum(is_goal) - sum(xG))

## Round Expected Goals to three digits
xg_player_game$xG <- round(xg_player_game$xG, digits = 3)

## Calculate cumulative xG for label positioning
xg_player_game <- xg_player_game %>%
  group_by(event_team) %>%
  mutate(pos = (cumsum(xG))) 

## Stacked xG per game
ggplot(xg_player_game, aes(x = event_team, y = xG, fill = event_player_1_name, group = event_team)) +
  geom_col(position = "stack", show.legend = FALSE, colour = "black" ) +
  geom_text(aes(y = pos, label = event_player_1_name), vjust = 1.15, colour = "black", size =3.5) +
  labs(title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
       subtitle = glue::glue( "{unique(game$game_date)}\n  {unique(game$away_abbreviation)} {unique(game$away_final)} - {unique(game$home_final)} {unique(game$home_abbreviation)}"),
       x = NULL, y = "Expected Goals") +
  theme(legend.position = "bottom")+
  theme(axis.title = element_text(size = 10)) 



# Cumulative xG over a game -----------------------------------------------

## Calculating cumulative xG per team
game <- game %>%
  group_by(event_team) %>%
  mutate(cumulative_xG = (cumsum(xG))) 


## Plot Cumulative xG over time
xg_time <- ggplot(game, aes(x = game_seconds, y = cumulative_xG, group = event_team, colour = event_team)) +
  geom_line(size = 1.5) +
  geom_point(data = game %>% filter(event_type == "GOAL"), size = 4.5) +
  geom_vline(xintercept = c(1200,2400,3600), color = "black" ,alpha = 0.4) +
  labs(title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
       subtitle = glue::glue( "{unique(game$game_date)}\n  {unique(game$away_abbreviation)} {unique(game$away_final)} - {unique(game$home_final)} {unique(game$home_abbreviation)}"),
       color = "Team", x = "Game Seconds", y = "Cumulative Expected Goals") +
  theme(legend.position = "bottom")


# Season Player & Team Summary Stats ---------------------------------------------

## Expected Goals by Player
xg_player_glm <- Test_Fenwick_Data %>%
  group_by(event_player_1_name) %>%
  summarise(xG = sum(xG), Goals = sum(is_goal), Difference = (sum(is_goal) - sum(xG)), eventteam = event_team)

xg_player_glm <- unique(xg_player_glm)

## Visualization Goals vs Expected Goals
ggplot(data = xg_player_glm, aes(x = Goals, y = xG)) +
  geom_point() + 
  labs(title = 'Expected Goals vs Goals by Player (GLM)',
       subtitle = "Training Dataset: 2018-2019 & 2019-2020 season", 
       x = "Goals Scored", 
       y = "Expected Goals") +
  stat_smooth(method = glm) 
  
## Expected Goals by Team

xg_team <- Train_Fenwick_Data %>%
  group_by(event_team) %>%
  summarise( xG = sum(xG), Goals = sum(is_goal), Difference = sum(xG) - sum(is_goal))

arrange(xg_team, desc(Difference))

## Plot Team Goals vs Expected Goals
ggplot(aes(x = xG, y = Goals), data = xg_team) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  labs(title = 'Expected Goals vs Goals by Team')


# Time on Ice  + Financial Value Calculation -------------------------------------------------


##Calculate total time on ice (Need data with all events = pbp_**_**)
nhldataclean(pbp_20_21)

player_TOI <- TOI_calc(pbp_20_21)

train_pbp <- nhldataclean(pbp_20_21)

## Join TOI to xG for each player
XG_TOI <- left_join(player_TOI, xg_player_glm)

## Load Salary Data From CapFriendly
salary_data <- read.csv("player_salary_2021.csv")

## Join Salary Data to xG and TOI data
XG_TOI <- right_join(XG_TOI, salary_data, by = "event_player_1_name", drop = TRUE)
  
  ## ##
  #grepl()
  


## Calculate xG per 60 mins of Ice (Standardization)
XG_TOI$xG_per60 <- XG_TOI$xG / XG_TOI$TOI60

## Calculate dollar per xG
XG_TOI$dollar_perxg60 <- XG_TOI$CAP.HIT/ XG_TOI$xG_per60

XG_TOI_calc$dollar_perxg <- XG_TOI$CAP.HIT/ XG_TOI$xG 

## Calculate dollar per real goal
XG_TOI$dollars_pergoal <- XG_TOI$CAP.HIT / XG_TOI$Goals

## Position classifier
defense <-c("LD", "RD", "LD/RD")
leftwinger <- c("LW","LW, RW", "LW, C", "LW, C, RW")
rightwinger <- c("RW", "RW, LW", "RW, C", "RW, C, LW")
center <- c("C", "C, LW", "C, RW", "C, LW, RW", "C, RW, LW")

## Make Position a Factor
XG_TOI$Position <- ifelse(XG_TOI$POS %in% defense, "Defense", ifelse(XG_TOI$POS %in% leftwinger, "Leftwinger", ifelse(XG_TOI$POS %in% rightwinger, "Rightwinger", ifelse(XG_TOI$POS %in% center, "Center", NA))))

XG_TOI$Position <-as.factor(XG_TOI$Position)

XG_TOI <- drop_na(XG_TOI)

XG_TOI <- filter(XG_TOI, XG_TOI$Position != "Defense") 

#write.csv(XG_TOI, file = "XG_TOI_calc.csv")
XG_TOI_calc <- read.csv("XG_TOI_calc.csv")

## Visualizations
ggplot(XG_TOI, aes(x = xG, y = dollar_perxg, shape = Position)) +
  geom_point(size = 2.75) +
  labs(x = "Expected Goals", 
       y = "Dollars per Expected Goal")+
  scale_y_continuous(labels = scales::comma, breaks = c(100000,200000, 300000, 400000, 500000, 1000000, 2000000, 3000000, 4000000)) 
  


ggplot() +
  geom_image(x = xG, y = )


XG_TOI_calc_test <- filter(XG_TOI_calc, XG_TOI_calc$dollar_perxg > 750000)
XG_TOI_calc_test$eventteam <- as.factor(XG_TOI_calc_test$eventteam)

t <- XG_TOI_calc_test %>%
  group_by(eventteam) %>%
  summarise(mdpg = mean(dollars_pergoal, na.rm = TRUE))

t <- drop_na(t)

write.csv(t, file = "t.csv")

t <- read.csv("t.csv")

t$playoffs <- as.factor(t$playoffs)

t <- t[order(t$mdpg),]

ggplot(t, aes(x = reorder(eventteam, +mdpg), y = mdpg, fill = playoffs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(label = t$Rank, vjust = -0.6) +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = 452777) +
  geom_text(aes(x = 19.9,y = 435000, label = "Average cost per goal: Salary Cap"), size = 3) +
  geom_vline(xintercept = 16.5) +
  geom_text(aes(x = 16.8, label = "Playoff cutoff", y = 800000), angle = 90, size = 3) +
  labs(x = "NHL Team", 
       y = "Average Dollars per Expected Goal")

## Summarize by team
bad_deal <- filter(XG_TOI_calc_test ,XG_TOI_calc_test$dollars_pergoal > 452777)



bad_deal %>%
  count(eventteam)

good_deal <- filter(XG_TOI_calc_test ,XG_TOI_calc_test$dollars_pergoal <= 452777)

good_deal %>%
  count(eventteam)

# Predicting the Outcome of Games with Model ------------------------------

## Filter for regular season games
regular_season <- subset(Train_Fenwick_Data, Train_Fenwick_Data$season_type == "R")

## Split data into list of games
season_split <- split(regular_season, f = regular_season$game_id)


# Apply Expected Goal model to predict xG of each team in each game

xG_gameresults <- list()

for(i in 1:length(season_split)){
  
  game <- season_split[[i]]  
  
  game <- nhldataclean(game)
  
  game <- filter(game, event_type %in% fenwick_events)
  
  game <- filter(game, !is.na(secondary_type))
  
  ##
  game$xG <- predict(xGmodel, game, type = "response")
  
  game$xG <- round(game$xG, digits = 2)
  
  game <- game %>%
    group_by(event_team) %>%
    mutate(cumulative_xG = cumsum(xG))
  
  xg_team_game <- game %>%
    group_by(event_team) %>%
    summarise( xG = sum(xG), Goals = sum(is_goal), Difference = sum(is_goal) - sum(xG)) 
  
  xG_gameresults[[i]] <- xg_team_game
  
}

## Do a check to see if prediction is correct
xG_gamecheck <- list()
for(j in 1:length(xG_gameresults)){ 
  
  gm <- xG_gameresults[[j]]
  
  gm <- gm[order(gm$xG),]
  
  gm <- gm %>%
    mutate(result = ifelse(gm$xG[1] < gm$xG[2] & gm$Goals[1] < gm$Goals[2] || gm$Goals[1] == gm$Goals[2] & (gm$xG[1] + 1.35)  > gm$xG[2], 1, 0))
  
  xG_gamecheck[[j]] <- gm
}

## Convert list to a dataframe
xG_gamecheck <- do.call(rbind.data.frame, xG_gamecheck)

## Sum of Correct Prediction
sum(xG_gamecheck$result)

save(xG_gamecheck, file = "xG_gamecheck.rda")

## Visualization of correct prediction of time

ggplot(xG_gamecheck, aes(x= event_team, y = result)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  geom_hline(yintercept = 152) +
  labs(x = "NHL Team", 
       y = "Sum of Correct Predicitions", 
       caption = "2018-2019 & 2019-2020 Regular Seasons") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,152))
  




# Other Visualizations ----------------------------------------------------

## Coordinates for specific player
player_name <- ("Zach.Hyman")

## Player heatmap:
player_xg <- game %>% filter(event_player_1_name == player_name)

## Get abs of x coord
player_xg$abs_x <- abs(player_xg$x_fixed)


## Plot xG by cooridinates

ggplot(player_xg, aes(abs_x, y_fixed, colour = xG)) + geom_point() +
  scale_color_gradient(low = "lightgrey", high = "black")+
  geom_vline(xintercept = 0, color = 'red') +
  geom_vline(xintercept = 25, color = 'blue') +
  geom_vline(xintercept = 88, color = 'red') +
  xlab('X Coordinates') + ylab('Y Coordinates') +
  labs(title = 'Average xG Value by Coordinate',
       subtitle = player_name) +
  geom_tile(aes(x = 89.665, y = 0, width = 3.33, height = 6))+
  geom_blank(data = game, )

## Shot Heat Map 
ggplot(player_xg, aes(x = abs_x, y = y_fixed)) +
  geom_density2d_filled()+
  geom_vline(xintercept = 0, color = 'red') +
  geom_vline(xintercept = 25, color = 'blue') +
  geom_vline(xintercept = 88, color = 'red') +
  xlab('X Coordinates') + ylab('Y Coordinates') +
  labs(title = 'Shot Heatmap',
       subtitle = player_name) +
  geom_tile(aes(x = 89.665, y = 0, width = 3.33, height = 6), color = "white") +
  theme(legend.position = "none")

ggplot(Test_Fenwick_Data, aes(x = shot_distance, y = xG, shape = secondary_type)) +
         geom_point() +
  labs(x = "Shot Distance (ft)", 
       y = "Expected Goal", 
       title = "Expected Goals by Shot Distance", 
       shape = "Shot Type") +
  scale_shape_manual(values = c(15,16,17,18,22,23,24))


k <- lm(is_goal ~ xG, data = Test_Fenwick_Data)
