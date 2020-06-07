
# Load Packages -----------------------------------------------------------

library(tidyverse)

# Read in Data ------------------------------------------------------------

train_dat <- read_csv("data/processed/training_data.csv") %>%
  mutate(result = case_when(bet_result_on_favorite == 1 ~ "FAV",
                            bet_result_on_favorite == -1 ~ "DOG",
                            bet_result_on_favorite == 0 ~ "PUSH"),
         home_favorite = if_else(team_home == team_favorite_id, "Yes", "No")) %>%
  mutate(pre_home_win_pct = if_else(is.nan(pre_home_win_pct), 0, pre_home_win_pct),
         pre_away_win_pct = if_else(is.nan(pre_away_win_pct), 0, pre_away_win_pct),
         home_prev_win_pct = if_else(is.na(home_prev_win_pct), 0.1875, home_prev_win_pct),
         away_prev_win_pct = if_else(is.na(away_prev_win_pct), 0.1875, away_prev_win_pct)) %>%
  select(schedule_week, schedule_playoff, spread_favorite, stadium, home_prev_win_pct,
         away_prev_win_pct, pre_home_win_pct, pre_away_win_pct, home_favorite, result) %>%
  filter(!is.na(result))

test_dat <- read_csv("data/processed/test_data.csv") %>%
  mutate(result = case_when(bet_result_on_favorite == 1 ~ "FAV",
                            bet_result_on_favorite == -1 ~ "DOG",
                            bet_result_on_favorite == 0 ~ "PUSH"),
         home_favorite = if_else(team_home == team_favorite_id, "Yes", "No")) %>%
  mutate(pre_home_win_pct = if_else(is.nan(pre_home_win_pct), 0, pre_home_win_pct),
         pre_away_win_pct = if_else(is.nan(pre_away_win_pct), 0, pre_away_win_pct),
         home_prev_win_pct = if_else(is.na(home_prev_win_pct), 0.1875, home_prev_win_pct),
         away_prev_win_pct = if_else(is.na(away_prev_win_pct), 0.1875, away_prev_win_pct)) %>%
  select(schedule_week, schedule_playoff, spread_favorite, stadium, home_prev_win_pct,
         away_prev_win_pct, pre_home_win_pct, pre_away_win_pct, home_favorite, result) %>%
  filter(!is.na(result)) %>%
  mutate(stadium = if_else(stadium == "Dolphin Stadium", "Hard Rock Stadium", stadium)) # Fix factor level that gave issues

# Build Model -------------------------------------------------------------

# Previous methods not working, so switched to new package

tr_control <- caret::trainControl(method = "cv", number = 10)

fit <- caret::train(result ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = tr_control,
             metric     = "Accuracy",
             data       = train_dat)

fit

# So out of 1 through 10, 9 is the best k value

fit2 <- caret::train(result ~ .,
                    method     = "knn",
                    tuneGrid   = expand.grid(k = 11:25),
                    trControl  = tr_control,
                    metric     = "Accuracy",
                    data       = train_dat)


fit2

# The best here is 17, but still not as good as 9. Let's try one more time

fit3 <- caret::train(result ~ .,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = seq(30, 100, by = 5)),
                     trControl  = tr_control,
                     metric     = "Accuracy",
                     data       = train_dat)

fit3

# We have a new best! k = 100. I guess let's see if it keeps climbing

fit4 <- caret::train(result ~ .,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = seq(105, 150, by = 5)),
                     trControl  = tr_control,
                     metric     = "Accuracy",
                     data       = train_dat)

fit4

# New best, but seems to have peaked. Let's zoom in

set.seed(1968) # Set seed so final selection is reproducible

fit5 <- caret::train(result ~ .,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = 101:109),
                     trControl  = tr_control,
                     metric     = "Accuracy",
                     data       = train_dat)

fit5

# So, let's use 102 for our final k value, and apply it to the test set

knn_predict <- predict(fit5, newdata = test_dat)
knn_predict

save(knn_predict, file = "knn_preds.rda")

test_dat %>%
  mutate(pred = as.factor(knn_predict),
         wrong = if_else(pred != result, 1, 0)) %>%
  summarise(misclass_rate = mean(wrong))

knn_misclass <- test_dat %>%
  mutate(pred = as.factor(knn_predict),
         wrong = if_else(pred != result, 1, 0)) %>%
  summarise(misclass_rate = mean(wrong))

# Hey, not terrible!

save(knn_misclass, file = "knn_misclass.rda")
