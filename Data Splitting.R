
# Loading Packages --------------------------------------------------------

library(tidyverse)
library(skimr)

# Loading Dataset ---------------------------------------------------------

scores_dat <- read_csv(file = "data/processed/scores_processed.csv")

# Splitting Data ----------------------------------------------------------

set.seed(1919)

train_dat <- scores_dat %>% sample_frac(0.85)
test_dat <- scores_dat %>% setdiff(train_dat)

# Writing the CSVs --------------------------------------------------------

write_csv(train_dat, path = str_c("C:\\Users\\Isaac\\Documents\\Stats 301\\Gage_Isaac_301_3_Project\\final-project-isaacgage\\data\\processed\\",
                                  "training_data.csv"))

write_csv(test_dat, path = str_c("C:\\Users\\Isaac\\Documents\\Stats 301\\Gage_Isaac_301_3_Project\\final-project-isaacgage\\data\\processed\\",
                                 "test_data.csv"))

