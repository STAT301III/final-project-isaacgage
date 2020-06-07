
# Loading Packages --------------------------------------------------------

library(tidyverse)
library(modelr)
library(ranger)
library(vip)
library(pdp)
library(xgboost)
library(onehot)

# Load in Data ------------------------------------------------------------

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
  filter(!is.na(result))

train_dat_dummied <- train_dat %>% select(-result) %>% fastDummies::dummy_cols(remove_selected_columns = TRUE)
test_dat_dummied <- test_dat %>% select(-result) %>% fastDummies::dummy_cols(remove_selected_columns = TRUE)

common_cols <- intersect(colnames(train_dat_dummied), colnames(test_dat_dummied))

train_dat_fixed <- train_dat_dummied %>%
  select(all_of(common_cols)) %>%
  mutate(result = train_dat$result) %>%
  mutate_if(is.character, as.factor)

test_dat_fixed <- test_dat_dummied %>%
  select(all_of(common_cols)) %>%
  mutate(result = test_dat$result) %>%
  mutate_if(is.character, as.factor)

setdiff(colnames(train_dat_fixed), colnames(test_dat_fixed))

# Create Helper Functions for Formatting ----------------------------------

xgb_matrix <- function(dat, outcome){
  
  # Sanitize input: check that dat is a tibble
  if(!is_tibble(dat)){
    
    dat <- as_tibble(dat)
    
  }
  
  # Sanitize input: check that data has factors, not characters
  dat_types <- dat %>% map_chr(class)
  
  outcome_type <- class(dat[[outcome]])
  
  if("character" %in% dat_types){
    
    # If we need to re-code, leave that outside of the function
    print("You must encode characters as factors.")
    return(NULL)
    
  } else {

      lab <- case_when(dat[[outcome]] == "DOG" ~ 0,
                       dat[[outcome]] == "FAV" ~ 1,
                       dat[[outcome]] == "PUSH" ~ 2)
    
    # Make our DMatrix
    mat <- dat %>% dplyr::select(-outcome) %>%
      onehot::onehot(max_levels = 89) %>% # use onehot to encode variables
      predict(dat) # get OHE matrix
    
    return(xgb.DMatrix(data = mat, 
                       label = lab))
    
  }
  
}

xg_error <- function(model, test_mat, metric = "misclass"){
  
  # Get predictions and actual values
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  
  if(metric == "mse"){
    
    # Compute MSE if that's what we need
    err <- mean((preds - vals)^2)
    
  } else if(metric == "misclass") {
    
    # Otherwise, get the misclass rate
    err <- mean(preds != vals)
    
  }
  
  return(err)
}


# Build Model -------------------------------------------------------------

# xg_class <- train_dat_fixed %>%
#   crossv_kfold(k = 5, id = "fold") %>%
#   mutate(
#     train = map(train, as_tibble),
#     test = map(test, as_tibble)
#   ) %>%
#   crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>%
#   mutate(
#     train_mat = map(train, xgb_matrix, outcome = "result"),
#     test_mat = map(test, xgb_matrix, outcome = "result"),
#     xg_model = map2(.x = train_mat, .y = learn_rate,
#                     .f = function(x, y) xgb.train(params = list(eta = y,
#                                                                 depth = 10, # tree depth, can tune
#                                                                 objective = "multi:softmax",
#                                                                 num_class = 3),
#                                                   data = x,
#                                                   nrounds = 500, # 500 trees, can be tuned
#                                                   silent = TRUE)),
#     # Get training and test error
#     xg_train_misclass = map2(xg_model, train_mat, xg_error, metric = "misclass"),
#     xg_test_misclass = map2(xg_model, test_mat, xg_error, metric = "misclass")
#   )
# save(xg_class, file = "boosted_output.rda") # Save so I don't have to keep waiting for that to rerun

load("boosted_output.rda")

xg_grouped <- xg_class %>%
  group_by(learn_rate) %>%
  summarise(xg_test_misclass = mean(unlist(xg_test_misclass)))

ggplot(xg_grouped) + 
  geom_line(aes(learn_rate, unlist(xg_test_misclass)))

# It appears that a very small learn rate is the best

xg_grouped %>%
  arrange(xg_test_misclass)

# Indeed, the best learn_rate is 0.0000000001

# Apply to Test Set -------------------------------------------------------

train_matrix <- xgb_matrix(train_dat_fixed, "result")

best_xg_model <- xgb.train(params = list(eta = 0.0000000001, depth = 10, objective = "multi:softmax",
                                         num_class = 3),
                           data = train_matrix, nrounds = 500, silent = TRUE)

test_matrix <- xgb_matrix(test_dat_fixed, "result")

xg_test_misclass <- xg_error(best_xg_model, test_matrix, metric = "misclass")

save(xg_test_misclass, file = "boosted_misclass.rda")

