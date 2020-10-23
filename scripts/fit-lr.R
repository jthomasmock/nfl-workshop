library(tidyverse)
library(tidymodels)

set.seed(37)

## ---- data-in
seasons <- 2016:2019

pbp_raw <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

## ---- clean-data
season_df <- filter(pbp_raw, 
                 season_type == 'REG' & 
                   down %in% c(1,2,3) &
                   !is.na(qb_dropback) &
                   !is.na(score_differential)) %>%
  mutate(qb_dropback = factor(qb_dropback),
         off_to = if_else(posteam_type == 'away', away_timeouts_remaining, home_timeouts_remaining),
         def_to = if_else(posteam_type == 'away', home_timeouts_remaining, away_timeouts_remaining)) %>%
  dplyr::select(qb_dropback, down, ydstogo, yardline_100, score_differential, qtr, half_seconds_remaining, off_to, def_to)

## ---- feat-engineering

season_df <- pbp_raw %>% 
  select(
    game_id, game_date, game_seconds_remaining, season_type, week, season,
    play_type, yards_gained, ydstogo, down, yardline_100, qtr, posteam, 
    posteam_score, defteam, defteam_score, score_differential, shotgun, 
    no_huddle, posteam_timeouts_remaining, defteam_timeouts_remaining, penalty, 
    wp, goal_to_go, half_seconds_remaining
  ) %>%
  filter(
    play_type %in% c("run", "pass"),
    penalty == 0,
    season_type == "REG",
    down %in% c(1:3),
    !is.na(score_differential)
  ) %>%
  mutate(
    in_red_zone = if_else(yardline_100 <= 20, 1, 0),
    in_fg_range = if_else(yardline_100 <= 35, 1, 0),
    two_min_drill = if_else(half_seconds_remaining <= 120, 1, 0)
  ) %>%
  select(-penalty, -season_type, -half_seconds_remaining)

all_plays <- season_df %>%
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
    play_type, season, posteam, defteam, shotgun, down, qtr, no_huddle,
    posteam_timeouts_remaining, defteam_timeouts_remaining, in_red_zone,
    in_fg_range, previous_play, goal_to_go, two_min_drill
  ), as.factor) %>%
  select(-run, -pass)

set.seed(37)

split_pbp <- initial_split(all_plays, 0.75, strata = play_type)
split_pbp
# separate the training data
train_data <- training(split_pbp)
# separate the testing data
test_data <- testing(split_pbp)

train_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))

test_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))

pbp_rec <- recipe(play_type ~ ., data = train_data) %>% 
  # ignore these vars for train/test, but include in data as ID
  update_role(game_id, game_date, yards_gained, new_role = "ID") %>% 
  # removes vars that have large absolute correlations w/ other vars
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors()) # remove zero-variance predictors

lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% # parsnip model
  add_recipe(pbp_rec) 

pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data)

pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob")) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(test_data %>% select(play_type, down, ydstogo, posteam))


pbp_pred_lr %>% 
  # get Area under Curve
  roc_auc(truth = play_type, 
          .pred_pass)

pbp_pred_lr %>% 
  # collect and report metrics
  metrics(truth = play_type, 
          .pred_class)

pbp_fit_lr

pbp_pred_lr

sum_fit <- pbp_pred_lr  %>%
  filter(ydstogo <= 15) %>% 
  group_by(down, ydstogo) %>%
  summarize(
    .pred_pass = mean(.pred_pass),
    n = n()
    )

pbp_pred_lr  %>%
  filter(between(ydstogo, 1, 15)) %>% 
  filter(posteam == "NE", down %in% c(1:3)) %>% 
  group_by(down, ydstogo) %>%
  summarize(
    .pred_pass = mean(.pred_pass),
    n = n()
  )

all_plays %>% 
  filter(between(ydstogo, 1, 15)) %>% 
  mutate(pass = if_else(play_type == "pass", 1, 0)) %>% 
  group_by(posteam, down, ydstogo) %>% 
  summarize(
    n = n(),
    pct_pass = mean(pass)
    ) %>% 
  mutate(posteam = as.character(posteam),
         down = as.double(down)) %>% 
  ungroup() %>% 
  left_join(
    pbp_pred_lr  %>%
      filter(between(ydstogo, 1, 15)) %>% 
      mutate(posteam = as.character(posteam),
             down = as.double(down)) %>% 
      filter(down %in% c(1:3)) %>% 
      group_by(posteam, down, ydstogo) %>%
      summarize(
        .pred_pass = mean(.pred_pass)
      )
  ) %>% 
  ggplot(aes(x = pct_pass, y=.pred_pass)) +
  geom_point()

all_plays %>% 
  filter(between(ydstogo, 1, 15)) %>% 
  mutate(pass = if_else(play_type == "pass", 1, 0)) %>% 
  group_by(down, ydstogo) %>% 
  summarize(
    n = n(),
    pct_pass = mean(pass)
  ) %>% 
  mutate(down = as.double(down)) %>% 
  ungroup() %>% 
  left_join(
    pbp_pred_lr  %>%
      filter(between(ydstogo, 1, 15)) %>% 
      mutate(posteam = as.character(posteam),
             down = as.double(down)) %>% 
      filter(down %in% c(1:3)) %>% 
      group_by(down, ydstogo) %>%
      summarize(
        .pred_pass = mean(.pred_pass)
      )
  ) %>% 
  ggplot(aes(x = pct_pass, y=.pred_pass)) +
  geom_point()


pbp_pred_lr %>% 
  mutate(qb_dropback = if_else(play_type == "pass", 1, 0)) %>% 
  mutate(pred_rounded = round(.pred_pass,1)) %>%
  group_by(pred_rounded) %>% 
  summarise(mean_prediction = mean(.pred_pass),
            mean_actual = mean(as.numeric(qb_dropback)),
            n = n()) %>%
  ggplot(aes(x = pred_rounded, y = mean_actual)) +
  geom_abline() +
  geom_point(aes(size = n)) +
  labs(
    x = "Predicted Prob",
    y = "Observed"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.1), limits = c(0,1), 
    labels = scales::percent_format(accuracy = 1)
    ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1), limits = c(0,1), 
    labels = scales::percent_format(accuracy = 1)
  )


pbp_pred_lr %>% 
  filter(ydstogo <= 15) %>% 
  ggplot(aes(x = ydstogo, y = .pred_pass, color = down, group = down)) +
  geom_point(data = sum_fit, aes(size = n)) +
  geom_smooth() +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1), 
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
    ) +
  scale_x_continuous(breaks = seq(1, 15, by = 1)) +
  geom_hline(yintercept = 0.5, color = "black", linetype = "dashed", alpha = 0.5) +
  theme_minimal()


pbp_pred_lr %>% 
  filter(posteam == "NE")
