
# Loading Packages --------------------------------------------------------

library(tidyverse)
library(janitor)

# Loading Data ------------------------------------------------------------

scores_dat <- read_csv("data/unprocessed/nfl-scores-and-betting-data/spreadspoke_scores.csv")


# Wrangling ---------------------------------------------------------------

scores_dat <- scores_dat %>%
  mutate(team_away = if_else(team_away == "New England Patriots" & schedule_season < 1971,
                             "Boston Patriots", team_away),
         winning_team = if_else(score_home > score_away, team_home,
                                if_else(score_home < score_away, team_away, "Tie")),
         losing_team = if_else(score_home > score_away, team_away,
                                if_else(score_home < score_away, team_home, "Tie")))

wins_dat <- scores_dat %>%
  filter(!(winning_team == "Tie")) %>%
  group_by(winning_team, schedule_season) %>%
  count() %>%
  mutate(wins = n) %>%
  select(-n)

losses_dat <- scores_dat %>%
  filter(!(losing_team == "Tie")) %>%
  group_by(losing_team, schedule_season) %>%
  count() %>%
  mutate(losses = n) %>%
  select(-n)

# For some reason, these are not the same length. Must investigate

tibble(win_test = (wins_dat %>% group_by(schedule_season) %>% count())$n,
       loss_test = (losses_dat %>% group_by(schedule_season) %>% count())$n,
       is_equal = if_else(win_test == loss_test, 1, 0)) %>%
  count(is_equal)

 # So there are five seasons that are not the same for some reason

tibble(season = (scores_dat %>% group_by(schedule_season) %>% count())$schedule_season,
       win_test = (wins_dat %>% group_by(schedule_season) %>% count())$n,
       loss_test = (losses_dat %>% group_by(schedule_season) %>% count())$n,
       is_equal = if_else(win_test == loss_test, 1, 0)) %>%
  filter(is_equal == 0)

# Now that we know the years, lets investigate

loss_szns <- (scores_dat %>%
  filter(schedule_season == 1972 | schedule_season == 1976 | schedule_season == 1982 |
           schedule_season == 2008 | schedule_season == 2017) %>%
  group_by(schedule_season, losing_team) %>% count() %>%
  mutate(team_year = str_c(losing_team, "_", schedule_season)))$team_year

win_szns <- (scores_dat %>%
    filter(schedule_season == 1972 | schedule_season == 1976 | schedule_season == 1982 |
             schedule_season == 2008 | schedule_season == 2017) %>%
    group_by(schedule_season, winning_team) %>% count() %>%
    mutate(team_year = str_c(winning_team, "_", schedule_season)))$team_year

tibble(loss_szns) %>%
  mutate(test = if_else(loss_szns %in% win_szns, 1, 0)) %>%
  filter(test == 0)

tibble(win_szns) %>%
  mutate(test = if_else(win_szns %in% loss_szns, 1, 0)) %>%
  filter(test == 0)

# OH, I GET IT!! These are the seasons where a team won no games or went undefeated. Must account for that before I can join tibbles

losses_dat <- losses_dat %>% ungroup() %>%
  add_row(losing_team = "Miami Dolphins", schedule_season = 1972, losses = 0) %>%
  rename(team = losing_team)

wins_dat <- wins_dat %>% ungroup() %>%
  add_row(winning_team = "Tampa Bay Buccaneers", schedule_season = 1976, wins = 0) %>%
  add_row(winning_team = "Baltimore Colts", schedule_season = 1982, wins = 0) %>%
  add_row(winning_team = "Detroit Lions", schedule_season = 2008, wins = 0) %>%
  add_row(winning_team = "Cleveland Browns", schedule_season = 2017, wins = 0) %>%
  rename(team = winning_team)

# Now we can cbind them

records_dat <- left_join(wins_dat, losses_dat) %>%
  mutate(win_pct = wins / (wins + losses)) %>%
  group_by(team) %>%
  mutate(prev_win_pct = lag(win_pct)) # This is useful for joining, because we want previous season data

# Now must inspect the NAs and see which we can fix (some teams relocate, rebrand, etc)

records_dat %>%
  filter(is.na(prev_win_pct), schedule_season > 1966)

# We can imput data for the cardinals, ravens, colts, chargers, raiders, cardinals again, rams, oilers, and titans


records_dat$prev_win_pct[1] = 0.4375000 # 1993 Phoenix Cardinals --> 1994 Arizona Cardinals
records_dat$prev_win_pct[98] = 0.3125000 # 1995 Cleveland Browns --> 1996 Baltimore Ravens
records_dat$prev_win_pct[342] = NA # 1999 browns technically an expansion team, cant use 1995 data for prev
records_dat$prev_win_pct[626] = 0.4375000 # 1983 Baltimore Colts --> 1984 Indianapolis Colts
records_dat$prev_win_pct[741] = 0.3125000 # 2016 San Diego Chargers --> 2017 Los Angeles Chargers
records_dat$prev_win_pct[744] = 0.4375000 # 1981 Oakland Raiders --> 1982 Los Angeles Raiders
records_dat$prev_win_pct[1124] = 0.5625000 # 1994 Los Angeles Raiders --> 1995 Oakland Raiders
records_dat$prev_win_pct[898] = 0.14285714 # 1970 Boston Patriots --> 1971 New England Patriots
records_dat$prev_win_pct[1203] = 0.5000000 # 1987 St. Louis Cardinals --> 1988 Phoenix Cardinals
records_dat$prev_win_pct[1434] = 0.2500000 # 1994 Los Angeles Rams --> 1995 St. Louis Rams
records_dat$prev_win_pct[786] = 0.4375000 # 2015 St. Louis Rams --> 2016 Los Angeles Rams
records_dat$prev_win_pct[1498] = 0.5000000 # 1996 Houston Oilers --> 1997 Tennessee Oilers
records_dat$prev_win_pct[1500] = 0.5000000 # 1998 Tennessee Oilers --> 1999 Tennessee Titans

records_dat %>%
  filter(is.na(prev_win_pct), schedule_season > 1966)

scores_dat <- left_join(scores_dat, records_dat, by = c("team_home" = "team", "schedule_season" = "schedule_season")) %>% 
  rename(home_prev_win_pct = prev_win_pct) %>%
  select(-wins, - losses, -win_pct) %>%
  left_join(records_dat, by = c("team_away" = "team", "schedule_season" = "schedule_season")) %>%
  rename(away_prev_win_pct = prev_win_pct) %>%
  select(-wins, -losses, -win_pct)

# Finally, I have the previous season data in there! Now I need to find a way to get a cumulative running total of current season record

# Fix Dates and other things
scores_dat <- scores_dat %>%
  mutate(schedule_date = as.Date(schedule_date, "%m/%d/%Y"),
         home_team_year = str_c(schedule_season, " ", team_home),
         away_team_year = str_c(schedule_season, " ", team_away),
         home_win = if_else(team_home == winning_team, 1, 0),
         home_loss = if_else(team_home == winning_team, 0, 1),
         away_win = if_else(team_away == winning_team, 1, 0),
         away_loss = if_else(team_away == winning_team, 0, 1))

home_scores_dat <- scores_dat %>%
  group_by(home_team_year) %>%
  mutate(home_wins = cumsum(home_win),
         home_losses = cumsum(home_loss)) %>%
  ungroup() %>% group_by(away_team_year) %>%
  mutate(away_wins = cumsum(away_win),
         away_losses = cumsum(away_loss)) %>% ungroup()

home_scores_dat %>% filter(home_team_year == "2010 Green Bay Packers" | away_team_year == "2010 Green Bay Packers") %>%
  arrange(schedule_date)

team_ids <- unique(scores_dat$home_team_year)

(scores_dat %>%
  filter(home_team_year == team_ids[777] | away_team_year == team_ids[777]))

home_cum_rec_dat <- tibble()

for (i in 1:length(team_ids)) { # This loop gives the home teams record in their home games
  home_team_wins <- 0
  home_team_losses <- 0
  team_dat <- scores_dat %>% filter(home_team_year == team_ids[i])
  team <- team_ids[i]
  home_running_wins <- c()
  home_running_losses <- c()
  for (j in 1:nrow(team_dat)) {
    home_team_wins <- home_team_wins + team_dat$home_win[j]
    home_team_losses <- home_team_losses + team_dat$home_loss[j]
    home_running_wins <- c(home_running_wins, home_team_wins)
    home_running_losses <- c(home_running_losses, home_team_losses)
    if (length(home_running_wins) == nrow(team_dat)) {
      home_szn_dat <- tibble(team_dat$schedule_date, team_dat$schedule_week, team_dat$home_team_year, 
                             home_running_wins, home_running_losses)
    }
  }
  home_cum_rec_dat <- bind_rows(home_cum_rec_dat, home_szn_dat)
}

home_cum_rec_dat <- home_cum_rec_dat %>%
  rename(schedule_date = `team_dat$schedule_date`,
         schedule_week = `team_dat$schedule_week`,
         home_team_year = `team_dat$home_team_year`) %>%
  mutate(team = home_team_year)

away_cum_rec_dat <- tibble()

for (i in 1:length(team_ids)) { # This loop gives the away teams record in their away games
  away_team_wins <- 0
  away_team_losses <- 0
  team_dat <- scores_dat %>% filter(away_team_year == team_ids[i])
  team <- team_ids[i]
  away_running_wins <- c()
  away_running_losses <- c()
  for (j in 1:nrow(team_dat)) {
    away_team_wins <- away_team_wins + team_dat$away_win[j]
    away_team_losses <- away_team_losses + team_dat$away_loss[j]
    away_running_wins <- c(away_running_wins, away_team_wins)
    away_running_losses <- c(away_running_losses, away_team_losses)
    if (length(away_running_wins) == nrow(team_dat)) {
      away_szn_dat <- tibble(team_dat$schedule_date, team_dat$schedule_week, team_dat$away_team_year, 
                             away_running_wins, away_running_losses)
    }
  }
  away_cum_rec_dat <- bind_rows(away_cum_rec_dat, away_szn_dat)
}

away_cum_rec_dat <- away_cum_rec_dat %>%
  rename(schedule_date = `team_dat$schedule_date`,
         schedule_week = `team_dat$schedule_week`,
         team = `team_dat$away_team_year`)

cum_rec_dat <- tibble()

for (i in 1:length(team_ids)) {
  cum_rec_dat <- bind_rows(cum_rec_dat, bind_rows(home_cum_rec_dat, away_cum_rec_dat) %>% filter(team == team_ids[i]) %>% arrange(schedule_date) %>%
    fill(home_running_wins, home_running_losses, away_running_wins, away_running_losses) %>%
    mutate_if(~ any(is.na(.)), funs(ifelse(is.na(.), 0, .))) %>%
    mutate(home_team_wins = home_running_wins + away_running_wins,
           home_team_losses = home_running_losses + away_running_losses))
}

cum_rec_dat <- cum_rec_dat %>% select(-contains("running"))

home_game_rec_dat <- cum_rec_dat %>% filter(team == home_team_year) %>% select(-team)
away_game_rec_dat <- cum_rec_dat %>% filter(team != home_team_year) %>% 
  rename(away_team_year = team,
         away_team_wins = home_team_wins,
         away_team_losses = home_team_losses) %>%
  select(-home_team_year)

left_join(scores_dat, home_game_rec_dat) %>% 
  left_join(away_game_rec_dat, by = c("schedule_date", "away_team_year"))

# After all this time, I realize the data has a mistake. From 1966 through 69, the Patriots are called Boston for home games
# and New England for away games. Super wierd, but I went back up and fixed it above now. But this was where I realized.

scores_dat <- left_join(scores_dat, home_game_rec_dat) %>% 
  left_join(away_game_rec_dat, by = c("schedule_date", "away_team_year"))

scores_dat <- scores_dat %>%
  mutate(pre_home_team_wins = if_else(home_win == 1, home_team_wins - 1, home_team_wins),
         pre_home_team_losses = if_else(home_loss == 1, home_team_losses - 1, home_team_losses),
         pre_away_team_wins = if_else(away_win == 1, away_team_wins - 1, away_team_wins),
         pre_away_team_losses = if_else(away_loss == 1, away_team_losses - 1, away_team_losses)) %>%
  rename(post_home_team_wins = home_team_wins,
         post_home_team_losses = home_team_losses,
         post_away_team_wins = away_team_wins,
         post_away_team_losses = away_team_losses)

scores_dat %>% filter(schedule_season == 2010) %>% view() # Testing based on prior knowledge

scores_dat <- scores_dat %>%
  rename(schedule_week = schedule_week.x) %>%
  select(-schedule_week.y)
  
scores_dat <- scores_dat %>%
  mutate(pre_home_win_pct = pre_home_team_wins / (pre_home_team_wins + pre_home_team_losses),
         pre_away_win_pct = pre_away_team_wins / (pre_away_team_wins + pre_away_team_losses),
         score_sum = score_home + score_away)

# Need to fix spread favorite IDs to match team names, otherwise nothing will work

scores_dat %>% group_by(team_favorite_id) %>%
  count() %>% print(n = 50)

scores_dat %>% filter(team_favorite_id == "PICK") %>%
  select(team_home, team_away, spread_favorite, schedule_season)

scores_dat <- scores_dat %>%
  mutate(team_favorite_id = if_else(
    team_favorite_id == "ARI" & schedule_season < 1988, "St. Louis Cardinals", if_else(
      team_favorite_id == "ARI" & schedule_season >= 1988 & schedule_season < 1994, "Phoenix Cardinals", if_else(
        team_favorite_id == "ARI" & schedule_season >= 1994, "Arizona Cardinaks", if_else(
          team_favorite_id == "ATL", "Atlanta Falcons", if_else(
            team_favorite_id == "BAL", "Baltimore Ravens", if_else(
              team_favorite_id == "BUF", "Buffalo Bills", if_else(
                team_favorite_id == "CAR", "Carolina Panthers", if_else(
                  team_favorite_id == "CHI", "Chicago Bears", if_else(
                    team_favorite_id == "CIN", "Cincinnati Bengals", if_else(
                      team_favorite_id == "CLE", "Cleveland Browns", if_else(
                        team_favorite_id == "DAL", "Dallas Cowboys", if_else(
                          team_favorite_id == "DEN", "Denver Broncos", if_else(
                            team_favorite_id == "DET", "Detroit Lions", if_else(
                              team_favorite_id == "GB", "Green Bay Packers", if_else(
                                team_favorite_id == "HOU", "Houston Texans", if_else(
                                  team_favorite_id == "IND" & schedule_season < 1984, "Baltimore Colts", if_else(
                                    team_favorite_id == "IND" & schedule_season >= 1984, "Indianapolis Colts", if_else(
                                      team_favorite_id == "JAX", "Jacksonville Jaguars", if_else(
                                        team_favorite_id == "KC", "Kansas City Chiefs", if_else(
                                          team_favorite_id == "LAC" & schedule_season < 2017, "San Diego Chargers", if_else(
                                            team_favorite_id == "LAC" & schedule_season >= 2017, "Los Angeles Chargers", if_else(
                                              team_favorite_id == "LAR" & schedule_season < 1995, "Los Angeles Rams", if_else(
                                                team_favorite_id == "LAR" & schedule_season >= 1995 & schedule_season < 2016, "St. Louis Rams", if_else(
                                                  team_favorite_id == "LAR" & schedule_season >= 2016, "Los Angeles Rams", if_else(
                                                    team_favorite_id == "MIA", "Miami Dolphins", if_else(
                                                      team_favorite_id == "MIN", "Minnesota Vikings", if_else(
                                                        team_favorite_id == "NE" & schedule_season < 1971, "Boston Patriots", if_else(
                                                          team_favorite_id == "NE" & schedule_season >= 1971, "New England Patriots", if_else(
                                                            team_favorite_id == "NO", "New Orleans Saints", if_else(
                                                              team_favorite_id == "NYG", "New York Giants", if_else(
                                                                team_favorite_id == "NYJ", "New York Jets", if_else(
                                                                  team_favorite_id == "OAK" & schedule_season < 1982, "Oakland Raiders", if_else(
                                                                    team_favorite_id == "OAK" & schedule_season >= 1982 & schedule_season < 1995, "Oakland Raiders", if_else(
                                                                      team_favorite_id == "OAK" & schedule_season >= 1995, "Oakland Raiders", if_else(
                                                                        team_favorite_id == "PHI", "Philadelphia Eagles", if_else(
                                                                          team_favorite_id == "PIT", "Pittsburgh Steelers", if_else(
                                                                            team_favorite_id == "SEA", "Seattle Seahawks", if_else(
                                                                              team_favorite_id == "SF", "San Francisco 49ers", if_else(
                                                                                team_favorite_id == "TB", "Tampa Bay Buccaneers", if_else(
                                                                                  team_favorite_id == "TEN" & schedule_season < 1997, "Houston Oilers", if_else(
                                                                                    team_favorite_id == "TEN" & schedule_season >= 1997 & schedule_season < 1999, "Tennessee Oilers", if_else(
                                                                                      team_favorite_id == "TEN" & schedule_season >= 1999, "Tennessee Titans", if_else(
                                                                                        team_favorite_id == "WAS", "Washington Redskins", team_favorite_id
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ) 
          )    
        )
      )
    )
  ))

scores_dat <- scores_dat %>%
  mutate(spread_result = if_else(
    team_home == team_favorite_id, score_away - score_home, if_else(
      team_away == team_favorite_id, score_home - score_away, score_home - score_away
    )
  ))

scores_dat <- scores_dat %>%
  mutate(home_favored = if_else(team_favorite_id == team_home, 1, 0),
         favorite_hit = if_else(spread_result < spread_favorite, 1, 0),
         underdog_hit = if_else(spread_result > spread_favorite, 1, 0),
         push = if_else(spread_result == spread_favorite, 1, 0),
         bet_result_on_favorite = if_else(favorite_hit == 1, 1, if_else(underdog_hit == 1, -1, if_else(push == 1, 0, push))))

scores_dat <- scores_dat %>%
  mutate(schedule_week = if_else(schedule_week == "WildCard", "Wildcard", if_else(
    schedule_week == "SuperBowl", "Superbowl", schedule_week)))

# MY DATA IS FINALLY READY!!!!!!! NOW I CAN EXPLORE

write_csv(scores_dat, path = str_c("C:\\Users\\Isaac\\Documents\\Stats 301\\Gage_Isaac_301_3_Project\\final-project-isaacgage\\data\\processed\\",
                                   "scores_processed.csv"))
