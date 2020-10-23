library(tidyverse)
library(tidymodels)

options(tidymodels.dark = TRUE)

# Raw Data ----------------------------------------------------------------



pbp_db <- tbl(DBI::dbConnect(RSQLite::SQLite(), "pbp_db.sqlite"), "pbp_clean_2000-2019")


raw_plays <- pbp_db %>%
  filter(
    play_type %in% c("run", "pass"),
    penalty == 0,
    qtr <= 4,
    season_type == "REG",
    season >= 2017,
    down %in% c(1:3),
    !is.na(yardline_100)
  ) %>% 
  select(
    game_id, posteam,
    play_type, yards_gained, ydstogo, down, game_seconds_remaining, 
    yardline_100, qtr, posteam, posteam_score, defteam, defteam_score, 
    score_differential, shotgun, no_huddle, posteam_timeouts_remaining, 
    defteam_timeouts_remaining, wp, goal_to_go, half_seconds_remaining
  ) %>%
  collect() 


# Feature Engineering -----------------------------------------------------



all_plays <- raw_plays %>%
  group_by(game_id, posteam) %>%
  mutate(
    run = if_else(play_type == "run", 1, 0),
    pass = if_else(play_type == "pass", 1, 0),
    total_runs = if_else(play_type == "run", cumsum(run) - 1, cumsum(run)),
    total_pass = if_else(play_type == "pass", cumsum(pass) - 1, cumsum(pass)),
    previous_play = if_else(posteam == lag(posteam),
                            lag(play_type), "First play of Drive"
    ),
    previous_play = if_else(is.na(previous_play),
                            replace_na("First play of Drive"), previous_play
    )
  ) %>%
  ungroup() %>%
  mutate_at(vars(
    play_type, shotgun, no_huddle,
    posteam_timeouts_remaining, defteam_timeouts_remaining, 
    previous_play, goal_to_go
  ), as.factor) %>%
  mutate(
    down = factor(down, levels = c(1, 2, 3), ordered = TRUE),
    qtr = factor(qtr, levels = c(1, 2, 3, 4), ordered = TRUE), 
    in_red_zone = if_else(yardline_100 <= 20, 1, 0),
    in_fg_range = if_else(yardline_100 <= 35, 1, 0),
    two_min_drill = if_else(half_seconds_remaining <= 120, 1, 0)
  ) %>% 
  mutate(
    in_red_zone = factor(if_else(yardline_100 <= 20, 1, 0)),
    in_fg_range = factor(if_else(yardline_100 <= 35, 1, 0)),
    two_min_drill = factor(if_else(half_seconds_remaining <= 120, 1, 0))
  ) %>%
  select(-run, -pass)


# Split -------------------------------------------------------------------



split_pbp <- initial_split(all_plays, 0.75, strata = play_type)

split_pbp

# separate the training data
train_data <- training(split_pbp)

# separate the testing data
test_data <- testing(split_pbp)


# Create Recipe -----------------------------------------------------------

pbp_rec <- recipe(play_type ~ ., data = train_data)  %>%
  step_rm(half_seconds_remaining) %>% # remove
  step_string2factor(posteam, defteam) %>%  # convert to factors
  # ignore these vars for train/test, but include in data as ID
  update_role(yards_gained, game_id, new_role = "ID") %>% 
  # removes vars that have large absolute correlations w/ other vars
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors())  # remove zero-variance predictors

tune_pbp_rf <- rand_forest(
  mtry = tune(), # add placeholder for tune
  trees = 100,
  min_n = tune() # add placeholder for tune
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")


# Create Workflow ---------------------------------------------------------

tune_rf_wf <- workflow() %>% 
  add_recipe(pbp_rec) %>% 
  add_model(tune_pbp_rf)

set.seed(37)

pbp_folds <- vfold_cv(train_data, v = 10)

tune_res <- tune_grid(
  tune_rf_wf,
  resamples = pbp_folds,
  grid = 15, # 15 combos of model parameters
  control = control_grid(verbose = TRUE)
)

tune_res %>% 
  write_rds("tune-rds-output.rds")
