## ---- read-data-and-filter
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

## ---- feature-engineer

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

## ---- split-data

set.seed(20201024)

split_pbp <- initial_split(all_plays, 0.75, strata = play_type)

split_pbp

# separate the training data
train_data <- training(split_pbp)

# separate the testing data
test_data <- testing(split_pbp)

## ---- count-plays

train_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))

test_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))

## ---- create-recipe

pbp_rec <- recipe(play_type ~ ., data = train_data)  %>%
  step_rm(half_seconds_remaining) %>% # remove
  step_string2factor(posteam, defteam) %>%  # convert to factors
  # ignore these vars for train/test, but include in data as ID
  update_role(yards_gained, game_id, new_role = "ID") %>% 
  # removes vars that have large absolute correlations w/ other vars
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors())  # remove zero-variance predictors

## ---- other1

set.seed(20201024)

## ---- lr-mod-fit

lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% # parsnip model
  add_recipe(pbp_rec)

pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data)

## ---- lr-pred

pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(test_data %>% select(play_type)) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob"))
  

pbp_pred_lr %>% 
  # get Area under Curve
  roc_auc(truth = play_type, 
          .pred_pass)

pbp_pred_lr %>% 
  # collect and report metrics
  metrics(truth = play_type, 
          .pred_class)

pbp_pred_lr %>% 
  accuracy(truth = play_type, .pred_class)

pbp_pred_lr %>% 
  # calculate ROC curve
  roc_curve(truth = play_type, 
            estimate = .pred_pass) %>% 
  # ggplot2 autoplot for AB line 
  # and the path of ROC curve
  autoplot()


# RF model ----------------------------------------------------------------

rf_mod <- rand_forest(trees = 100) %>% 
  set_engine("ranger", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")

rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>%  # New model
  add_recipe(pbp_rec)    # Same recipe

## ---- fit-rf-wflow

set.seed(20201024)
pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest

## ---- compare-preds

# Get predictions and check metrics
pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(play_type)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))

pbp_pred_rf %>% # Random Forest predictions
  metrics(truth = play_type, .pred_class)

pbp_pred_lr %>% # Logistic Regression predictions
  metrics(truth = play_type, .pred_class)

pbp_pred_lr %>% 
  accuracy(truth = play_type, .pred_class)

pbp_pred_lr %>% 
  conf_mat(truth = play_type, .pred_class)

pbp_pred_rf %>% 
  conf_mat(truth = play_type, .pred_class)


pbp_pred_lr %>% 
  roc_auc(truth = factor(play_type), .pred_pass)

pbp_fit_rf %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)

## ---- create-roc-curves

roc_rf <- pbp_pred_rf %>% 
  roc_curve(truth = factor(play_type), .pred_pass) %>% 
  mutate(model = "Random Forest")

roc_lr <- pbp_pred_lr %>% 
  roc_curve(truth = factor(play_type), .pred_pass) %>% 
  mutate(model = "Logistic Regression")

full_plot <- bind_rows(roc_rf, roc_lr) %>% 
  # Note that autoplot() would also work here!
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  scale_color_manual(values = c("#374785", "#E98074")) +
  theme(legend.position = "top")

full_plot
