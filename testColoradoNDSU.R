
library(cfbfastR)
library(cfbplotR)
library(tidyverse)
library(gt)

team = "Colorado"
year = 2024
week = 1

#Getting pbp data
pbpForEpa = cfbd_pbp_data(year = year, team = team, week = week, epa_wpa = T) %>%
  filter(down > 0,
         !str_detect(play_text, "Kneel"),
         (rush == 1 | pass == 1))

#Getting gameid for the game
gameid = pbpForEpa$game_id[1]

#Getting advanced game log data
advancedBox = cfbd_game_box_advanced(gameid, long = F) 

#Getting game team stats
teamStats = cfbd_game_team_stats(year = year, game_id = gameid)

teamStats = teamStats %>% separate_wider_delim(completion_attempts, delim = "-", names = c("completions", "attempts"))

teamStats = teamStats %>% separate_wider_delim(third_down_eff, delim = "-", names = c("thirdDownCompleted", "thirdDownAttempts"))


#Seperating the team stats df with home and away
homeTeamStats = teamStats %>% filter(home_away == "home")
awayTeamStats = teamStats %>% filter(home_away == "away")

#Seperating the advanced box df with home and away
homeAdvBox = advancedBox %>% filter(team == pbpForEpa$home[1])
awayAdvBox = advancedBox %>% filter(team == pbpForEpa$away[1])

#Calculating EPA things
homePBP = pbpForEpa %>%
  filter(pos_team == pbpForEpa$home[1])

awayPBP = pbpForEpa %>%
  filter(pos_team == pbpForEpa$away[1])

homePBPpass = homePBP %>%
  filter(pass == 1)

awayPBPpass = awayPBP %>%
  filter(pass == 1)

homePBPrush = homePBP %>%
  filter(rush == 1)

awayPBPrush = awayPBP %>%
  filter(rush == 1)

homeEPAperPlay = sum(homePBP$EPA, na.rm = T) / nrow(homePBP)
awayEPAperPlay = sum(awayPBP$EPA, na.rm = T) / nrow(awayPBP)

homeEPAperPass = sum(homePBPpass$EPA, na.rm = T) / nrow(homePBPpass)
awayEPAperPass = sum(awayPBPpass$EPA, na.rm = T) / nrow(awayPBPpass)

homeEPAperRush = sum(homePBPrush$EPA, na.rm = T) / nrow(homePBPrush)
awayEPAperRush = sum(awayPBPrush$EPA, na.rm = T) / nrow(awayPBPrush)

#Creating the data frame for the gt table
dfForGt = data.frame(
  "Stat" = c("Score",
             "EPA / Play",
             "Yards / Play",
             "Success Rate",
             "EPA / Pass",
             "Yards / Completion",
             "Completion %",
             "Attempts",
             "Yards",
             "EPA / Rush",
             "Yards / Rush",
             "Stuff Rate",
             "Attempts",
             "Yards",
             "Third Down %"
  ),
  home = c(homeTeamStats$points,
           homeEPAperPlay,
           as.numeric(homeTeamStats$total_yards) / homeAdvBox$ppa_plays, 
           homeAdvBox$success_rates_overall_total,
           homeEPAperPass,
           as.numeric(homeTeamStats$net_passing_yards) / as.numeric(homeTeamStats$completions),
           as.numeric(homeTeamStats$completions) / as.numeric(homeTeamStats$attempts),
           homeTeamStats$attempts,
           homeTeamStats$net_passing_yards,
           homeEPAperRush,
           homeTeamStats$yards_per_rush_attempt,
           homeAdvBox$rushing_stuff_rate,
           homeTeamStats$rushing_attempts,
           homeTeamStats$rushing_yards,
           as.numeric(homeTeamStats$thirdDownCompleted) / as.numeric(homeTeamStats$thirdDownAttempts)),
  away = c(awayTeamStats$points,
           awayEPAperPlay,
           as.numeric(awayTeamStats$total_yards) / awayAdvBox$ppa_plays,
           awayAdvBox$success_rates_overall_total,
           awayEPAperPass,
           as.numeric(awayTeamStats$net_passing_yards) / as.numeric(awayTeamStats$completions),
           as.numeric(awayTeamStats$completions) / as.numeric(awayTeamStats$attempts),
           awayTeamStats$attempts,
           awayTeamStats$net_passing_yards,
           awayEPAperRush,
           awayTeamStats$yards_per_rush_attempt,
           awayAdvBox$rushing_stuff_rate,
           awayTeamStats$rushing_attempts,
           awayTeamStats$rushing_yards,
           as.numeric(awayTeamStats$thirdDownCompleted) / as.numeric(awayTeamStats$thirdDownAttempts))
)

colnames(dfForGt)[2] <- pbpForEpa$home[1]
colnames(dfForGt)[3] <- pbpForEpa$away[1]

dfForGt[,2] = as.numeric(dfForGt[,2])
dfForGt[,3] = as.numeric(dfForGt[,3])




#Getting data for team stats 
combined_data_team_stats <- data.frame()
weeksForPercentiles = c(1:15)

for (weeks in weeksForPercentiles) {
  temp = cfbd_game_team_stats(
    year = 2023,
    week = weeks,
    season_type = "regular",
    team = NULL,
    conference = NULL,
    game_id = NULL,
    rows_per_team = 1
  )
  
  combined_data_team_stats <- rbind(combined_data_team_stats, temp)
}

combined_data_team_stats[78] = sapply(combined_data_team_stats[78], as.numeric)
combined_data_team_stats = as.data.frame(combined_data_team_stats)

#Getting data for adv stats
combined_data_adv_stats <- data.frame()
weeksForPercentiles = c(1:15)
weekData = data.frame()

for (weeks in weeksForPercentiles) {
  temp = cfbd_game_info(
    year = year,
    week = weeks,
    season_type = "regular",
    division = "fbs",
  )
  
  weekData = rbind(weekData, temp)
  
  gameIds = weekData$game_id
}

for (game in gameIds) {
  temp = cfbd_game_box_advanced(
    game_id = game,
  )
  
  combined_data_adv_stats <- rbind(combined_data_adv_stats, temp)
}

combined_data_adv_stats <- do.call(rbind, lapply(gameIds, function(game) {
  cfbd_game_box_advanced(game_id = game)
}))



#Getting play by play data
pbp2023 = cfbd_pbp_data(year = year, epa_wpa = T) %>%
  filter(down > 0,
         !str_detect(play_text, "Kneel"))



dfForGt %>%
  gt() %>%
  cols_move(
    columns = 1,
    after = 2
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>% 
  opt_table_font(
    font = "Coolvetica",
  ) %>%
  tab_row_group("Efficiency", c(2:4, 15)) %>%
  tab_row_group("Passing", c(5:9)) %>%
  tab_row_group("Rushing", c(10:14)) %>%
  row_group_order(groups = c(NA, "Efficiency", "Passing", "Rushing")) %>%
  tab_style(
    style = list(
      cell_text(align = "center")  # Align text to the center
    ),
    locations = cells_row_groups()
  ) %>%
  data_color(
    columns = c(2,3),
    rows = Stat == "EPA / Play",
    colors = scales::col_quantile(
      palette = "Greens",
      domain = combined_data_adv_stats$ppa_overall_total,
      na.color = "#FFFFFF00"
    )
  ) %>%
  data_color(
    columns = c(2,3),
    rows = Stat == "Yards / Play",
    colors = scales::col_quantile(
      palette = "Greens",
      domain = combined_data_adv_stats$ppa_overall_total,
      na.color = "#FFFFFF00"
    )
  )
