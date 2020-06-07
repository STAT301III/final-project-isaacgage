
# Loading Packages --------------------------------------------------------

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

lambda_grid <- 10^seq(-2, 10, length = 200)

ridge_cv <- train_dat %>% 
  glmnetUtils::cv.glmnet(
    formula = result ~ ., 
    data = ., 
    alpha = 0, 
    nfolds = 10,
    lambda = lambda_grid,
    family = "multinomial"
  )

ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_1se <- ridge_cv$lambda.1se

lasso_cv <- train_dat %>% 
  glmnetUtils::cv.glmnet(
    formula = result ~ ., 
    data = ., 
    alpha = 1, 
    nfolds = 10,
    lambda = lambda_grid,
    family = "multinomial"
  )

lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_min <- lasso_cv$lambda.min

glmnet <- tibble(
  train = train_dat %>% list(),
  test  = test_dat %>% list()
) %>%
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(result ~ ., data = .x,
                                    alpha = 0, lambda = ridge_lambda_min,
                                    family = "multinomial")),
    ridge_1se = map(train, ~ glmnetUtils::glmnet(result ~ ., data = .x,
                                    alpha = 0, lambda = ridge_lambda_1se,
                                    family = "multinomial")),
    lasso_min = map(train, ~ glmnetUtils::glmnet(result ~ ., data = .x,
                                    alpha = 1, lambda = lasso_lambda_min,
                                    family = "multinomial")),
    lasso_1se = map(train, ~ glmnetUtils::glmnet(result ~ ., data = .x,
                                    alpha = 1, lambda = lasso_lambda_1se,
                                    family = "multinomial"))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

# Helper function to pull prediction

pred_puller <- function(input_tibble) {
  preds <- as.vector(input_tibble %>%
    mutate(pred = case_when(
      DOG.s0 > FAV.s0 & DOG.s0 > PUSH.s0 ~ "DOG",
      FAV.s0 > DOG.s0 & FAV.s0 > PUSH.s0 ~ "FAV",
      PUSH.s0 > DOG.s0 & PUSH.s0 > FAV.s0 ~ "PUSH"
    )) %>% select(pred))
  return(preds)
}

glmnet_misclass <- glmnet %>%
  mutate(pred_scores = map2(fit, test, predict),
         pred_scores = map(pred_scores, as_tibble),
         pred = map(pred_scores, pred_puller),
         misclass = map2_dbl(test, pred, ~ mean(.y != .x$result))) %>%
  unnest(misclass) %>%
  select(method, misclass)

glmnet_misclass # So ridge min is the best

ridge_misclass <- glmnet_misclass %>%
  filter(method == "ridge_min") %>%
  select(misclass)

save(ridge_misclass, file = "ridge_misclass.rda")

ridge_pred_scores <- glmnet %>%
  mutate(pred_scores = map2(fit, test, predict),
         pred_scores = map(pred_scores, as_tibble)) %>%
  filter(method == "ridge_min") %>%
  pluck("pred_scores", 1)

save(ridge_pred_scores, file = "ridge_pred_scores.rda")
