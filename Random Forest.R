
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(ranger)
library(vip)
library(pdp)
library(modelr)

# Read in Training Data ---------------------------------------------------

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

# Define Misclass Helper Function -----------------------------------------

misclass_ranger <- function(model, test){
  # Check if test is a tibble
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  # Make predictions
  preds <- predict(model, test)$predictions
  # Compute misclass rate
  misclass <- mean(test$result != preds)
  return(misclass)
}


# Cross Validate to Select Optimal mtry -----------------------------------

set.seed(1919)

train_models <- train_dat %>%
  crossv_kfold(k = 10, id = "fold") %>%
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>%
  crossing(mtry = 1:(ncol(train_dat) - 1)) %>%
  mutate(model = map2(.x = train, .y = mtry,
                      .f = function(x, y) ranger(result ~ ., 
                                                 mtry = y, 
                                                 data = x, 
                                                 splitrule = "gini",
                                                 importance = "impurity")),
         train_misclass = map2(model, train, misclass_ranger),
         test_misclass = map2(model, test, misclass_ranger), 
         oob_misclass = map(.x = model, 
                            .f = function(x) x[["prediction.error"]])
  ) 

train_grouped <- train_models %>%
  group_by(mtry) %>%
  summarise(oob_mean_misclass = mean(unlist(oob_misclass)),
            test_mean_misclass = mean(unlist(test_misclass)))

ggplot(train_grouped) + 
  geom_line(aes(mtry, oob_mean_misclass, color = "OOB Error")) +
  geom_line(aes(mtry, test_mean_misclass, color = "Test Error")) + 
  labs(x = "mtry", y = "Misclassification Rate") + 
  scale_color_manual("", values = c("purple", "blue", "red")) + 
  theme_bw()

# Based on this, our best model appears to be where mtry = 2, as this minimizes test error and has low OOB error

best_rf_model <- train_models %>%
  filter(mtry == 2) %>%
  select(model) %>%
  pluck("model", 1)

# Test Model on Test Set --------------------------------------------------

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
  filter(!is.na(result))

misclass_ranger(best_rf_model, test_dat)

rf_misclass <- misclass_ranger(best_rf_model, test_dat)

save(rf_misclass, file = "rf_misclass.rda")

# Alright then, pretty unexciting. At least its below 50%.

vip(best_rf_model) # Wow, this is fascinating

