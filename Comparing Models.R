
# Loading Packages --------------------------------------------------------

library(tidyverse)

# Loading Misclasses ------------------------------------------------------

load("knn_misclass.rda")
load("multi_logit_misclass.rda")
load("ridge_misclass.rda")
load("boosted_misclass.rda")
load("rf_misclass.rda")


# Make Table --------------------------------------------------------------

knn_misclass <- knn_misclass %>% pluck("misclass_rate", 1)
multi_log_reg_misclass <- multi_log_reg_misclass %>% pluck("misclass", 1)
ridge_misclass <- ridge_misclass %>% pluck("misclass")

tibble(model = c("KNN", "Multinomial Logit", "Random Forest", "Ridge", "Boosted Tree"),
       misclass_rate = c(knn_misclass, multi_log_reg_misclass, rf_misclass, ridge_misclass, xg_test_misclass)) %>%
  arrange(misclass_rate) %>% knitr::kable()

# Let's Investigate the Best Model, Ridge ---------------------------------

load("ridge_pred_scores.rda")

pred_puller <- function(input_tibble) {
  preds <- as.vector(input_tibble %>%
                       mutate(pred = case_when(
                         DOG.s0 > FAV.s0 & DOG.s0 > PUSH.s0 ~ "DOG",
                         FAV.s0 > DOG.s0 & FAV.s0 > PUSH.s0 ~ "FAV",
                         PUSH.s0 > DOG.s0 & PUSH.s0 > FAV.s0 ~ "PUSH"
                       )) %>% select(pred))
  return(preds)
}

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

preds <- pred_puller(ridge_pred_scores)

push_adjusted_misclass <- test_dat %>%
  mutate(pred = preds$pred) %>%
  filter(result != "PUSH") %>%
  summarise(push_adj_misclass = mean(pred != result)) %>% pluck("push_adj_misclass", 1)

test_scores <- test_dat %>%
  mutate(dog_score = ridge_pred_scores$DOG.s0,
         fav_score = ridge_pred_scores$FAV.s0,
         push_score = ridge_pred_scores$PUSH.s0,
         fav_win_pct = ifelse(home_favorite == "Yes", pre_home_win_pct,
           ifelse(home_favorite == "No" & spread_favorite < 0, pre_away_win_pct, NA)),
         dog_win_pct = ifelse(home_favorite == "Yes", pre_away_win_pct,
                              ifelse(home_favorite == "No" & spread_favorite < 0, pre_home_win_pct, NA)),
         diff_win_pct = fav_win_pct - dog_win_pct,
         pred = preds$pred,
         correct = ifelse(pred == result, "Correct", "Incorrect"))
  
ggplot(ridge_pred_scores) +
  geom_density(aes(DOG.s0, fill = "Underdog"), alpha = 0.5) +
  geom_density(aes(FAV.s0, fill = "Favorite"), alpha = 0.5) +
  labs(
    x = "Prediction Score",
    y = "Density",
    title = "Comparing the Distributions of Scores for Underdogs and Favorites",
    fill = "Team Type"
  ) +
  scale_fill_manual(values = c("red", "green"), ) +
  theme_minimal()

ggplot(test_scores, aes(spread_favorite, dog_score)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    y = "Underdog Prediction Score",
    x = "Spread",
    title = "Spread vs Underdog Prediction Score"
  ) +
  theme_minimal()

ggplot(test_scores %>% filter(!is.na(diff_win_pct)), aes(diff_win_pct, dog_score)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(
    y = "Underdog Prediction Score",
    x = "Difference in Win Percentages",
    title = "Win Percentage Difference vs. Underdog Prediction Score"
  ) +
  theme_minimal()

ggplot(test_scores, aes(dog_score)) +
  geom_density(aes(fill = correct), alpha = 0.3) +
  scale_fill_manual(values = c("green", "red")) +
  labs(
    x = "Underdog Prediction Score",
    y = "Density",
    fill = "Prediction",
    title = "Density of Correct vs. Incorrect Predictions by Underdog Score"
  ) +
  theme_minimal()
