## ----setup, include = FALSE, eval = TRUE-----------------------------------------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(vip)
library(usemodels)
library(xaringan)
library(gt)

knitr::read_chunk("tidymodel_script.R")

## ---- cache=TRUE, message = FALSE, warning=FALSE---------------------------------------------------------------------------------------------------
# get all weekly QBR for 2020 season
basic_data <- crossing(
  season = 2020, week = 1:6
  ) %>% 
  pmap_dfr(espnscrapeR::get_nfl_qbr)

basic_plot <- basic_data %>% 
  ggplot(
    aes(x = total_epa, y = qbr_total)
    ) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    x = "EPA", y = "QBR", 
    title = "EPA is correlated with QBR"
    )


## ---- message = FALSE, warning=FALSE, echo = FALSE, fig.dim=c(5,5)---------------------------------------------------------------------------------
basic_plot


## --------------------------------------------------------------------------------------------------------------------------------------------------
# fit a basic linear model
basic_lm <- lm(qbr_total~total_epa, data = basic_data)


## --------------------------------------------------------------------------------------------------------------------------------------------------
basic_lm


## --------------------------------------------------------------------------------------------------------------------------------------------------
summary(basic_lm)


## --------------------------------------------------------------------------------------------------------------------------------------------------
broom::tidy(basic_lm)


## --------------------------------------------------------------------------------------------------------------------------------------------------
broom::glance(basic_lm)


## ---- echo=FALSE, out.width="60%"------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("https://themockup.blog/posts/2020-05-01-tidy-long-models/distill-preview.png")


## ---- echo = FALSE, out.width="25%"----------------------------------------------------------------------------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/tidymodels.svg")


## ---- echo = FALSE, out.width = "55%"--------------------------------------------------------------------------------------------------------------
knitr::include_graphics("https://bradleyboehmke.github.io/HOML/02-modeling-process_files/figure-html/modeling-process-roc-1.png")


## ----read-data-and-filter, cache = TRUE, echo = FALSE----------------------------------------------------------------------------------------------
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

## ---- skip-in-rmd

## IF DOING LOCALLY WITHOUT PBP DATABASE

raw_plays <- read_rds(url("https://github.com/jthomasmock/nfl-workshop/blob/master/raw_plays.rds?raw=true"))

## ---- size=1---------------------------------------------------------------------------------------------------------------------------------------
glimpse(raw_plays)


## ----read-data-and-filter, cache = TRUE------------------------------------------------------------------------------------------------------------
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


## ----feature-engineer------------------------------------------------------------------------------------------------------------------------------
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


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## split_data <- initial_split(data, 0.75)
## 
## train_data <- training(split_data)
## test_data <- testing(split_data)


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## model_recipe <- recipe(pred ~ predictors, data = train_data)


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Choose a model and an engine
## lr_mod <- logistic_reg(mode = "classification") %>%
##   set_engine("glm")


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Combine the model and recipe to the workflow
## lr_wflow <- workflow() %>%
##   add_recipe(model_recipe) %>%
##   add_model(lr_mod)
## 
## # Fit/train the model
## model_fit <- lr_wflow %>%
##   fit(data = train_data)


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Get predictions
## pred_lr <- predict(pbp_fit_lr, test_data)
## 
## # Check metrics
## pred_lr %>%
##   metrics(truth = pred, .pred_class) %>%
##   bind_cols(select(test_data, pred)) %>%
##   bind_cols(predict(fit_lr, test_data, type = "prob"))


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Split
## split_pbp <- initial_split(all_plays, 0.75, strata = play_type)
## 
## # Split into test/train
## train_data <- training(split_pbp)
## test_data <- testing(split_pbp)


## ---- eval =FALSE----------------------------------------------------------------------------------------------------------------------------------
## pbp_rec <- recipe(play_type ~ ., data = train_data)  %>%
##   step_rm(half_seconds_remaining) %>% # remove
##   step_string2factor(posteam, defteam) %>%  # convert to factors
##   update_role(yards_gained, game_id, new_role = "ID") %>%  # add as ID
##   step_corr(all_numeric(), threshold = 0.7) %>% # remove auto-correlated
##   step_center(all_numeric()) %>%  # substract mean from numeric
##   step_zv(all_predictors())  # remove zero-variance predictors
## 
## # Choose a model and an engine
## lr_mod <- logistic_reg(mode = "classification") %>%
##   set_engine("glm")


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Combine the model and recipe to the workflow
## lr_wflow <- workflow() %>%
##   add_recipe(pbp_rec) %>%
##   add_model(lr_mod)
## 
## # Fit/train the model
## pbp_fit_lr <- lr_wflow %>%
##   fit(data = train_data)


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Get predictions
## pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>%
##   bind_cols(test_data %>% select(play_type)) %>%
##   bind_cols(predict(pbp_fit_lr, test_data, type = "prob"))
## 
## # Check metrics
## pbp_pred_lr %>%
##   metrics(truth = play_type, .pred_class)


## --------------------------------------------------------------------------------------------------------------------------------------------------
split_pbp <- initial_split(all_plays, 0.75, strata = play_type)

split_pbp

# separate the training data
train_data <- training(split_pbp)

# separate the testing data
test_data <- testing(split_pbp)


## ----split and train ratio-------------------------------------------------------------------------------------------------------------------------
train_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))


## ----print test ratio------------------------------------------------------------------------------------------------------------------------------
test_data %>% 
  count(play_type) %>% 
  mutate(ratio = n/sum(n))


## ----create-recipe---------------------------------------------------------------------------------------------------------------------------------
pbp_rec <- recipe(play_type ~ ., data = train_data)  %>%
  step_rm(half_seconds_remaining) %>% # remove
  step_string2factor(posteam, defteam) %>%  # convert to factors
  # ignore these vars for train/test, but include in data as ID
  update_role(yards_gained, game_id, new_role = "ID") %>% 
  # removes vars that have large absolute correlations w/ other vars
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors())  # remove zero-variance predictors


## ---- echo=FALSE, out.width= "25%"-----------------------------------------------------------------------------------------------------------------

knitr::include_graphics("https://raw.githubusercontent.com/tidymodels/parsnip/master/man/figures/logo.png")


## --------------------------------------------------------------------------------------------------------------------------------------------------
# Note that mode = "classification" is the default here anyway!
lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")


## ----echo=FALSE, out.width = "25%"-----------------------------------------------------------------------------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/tidymodels/workflows/master/man/figures/logo.png")


## --------------------------------------------------------------------------------------------------------------------------------------------------
lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% # parsnip model
  add_recipe(pbp_rec)   # recipe from recipes


## --------------------------------------------------------------------------------------------------------------------------------------------------
pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data) # fit the model against the training data


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------
pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(test_data %>% select(play_type)) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob"))


## --------------------------------------------------------------------------------------------------------------------------------------------------
pbp_pred_lr %>% 
  # get Area under Curve
  roc_auc(truth = play_type, 
          .pred_pass)

pbp_pred_lr %>% 
  # collect and report metrics
  metrics(truth = play_type, 
          .pred_class)


## ---- fig.dim = c(5,5)-----------------------------------------------------------------------------------------------------------------------------
pbp_pred_lr %>% 
  # calculate ROC curve
  roc_curve(truth = play_type, .pred_pass) %>% 
  autoplot()


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Split
## split_pbp <- initial_split(all_plays, 0.75, strata = play_type)
## 
## # Split into test/train
## train_data <- training(split_pbp)
## test_data <- testing(split_pbp)


## ---- eval =FALSE----------------------------------------------------------------------------------------------------------------------------------
## pbp_rec <- recipe(play_type ~ ., data = train_data)  %>%
##   step_rm(half_seconds_remaining) %>% # remove
##   step_string2factor(posteam, defteam) %>%  # convert to factors
##   update_role(yards_gained, game_id, new_role = "ID") %>%  # add as ID
##   step_corr(all_numeric(), threshold = 0.7) %>% # remove auto-correlated
##   step_center(all_numeric()) %>%  # substract mean from numeric
##   step_zv(all_predictors())  # remove zero-variance predictors
## 
## # Choose a model and an engine
## lr_mod <- logistic_reg(mode = "classification") %>%
##   set_engine("glm")


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Combine the model and recipe to the workflow
## lr_wflow <- workflow() %>%
##   add_recipe(pbp_rec) %>%
##   add_model(lr_mod)
## 
## # Fit/train the model
## pbp_fit_lr <- lr_wflow %>%
##   fit(data = train_data)


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## # Get predictions
## pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>%
##   bind_cols(test_data %>% select(play_type)) %>%
##   bind_cols(predict(pbp_fit_lr, test_data, type = "prob"))
## 
## # Check metrics
## pbp_pred_lr %>%
##   metrics(truth = play_type, .pred_class)


## --------------------------------------------------------------------------------------------------------------------------------------------------
rf_mod <- rand_forest(trees = 100) %>% 
  set_engine("ranger", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")

rf_wflow <- workflow() %>% 
  add_recipe(pbp_rec) %>% # Same recipe
  add_model(rf_mod)     # New model
  

pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest

# Get predictions and check metrics
pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(play_type)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))


## ----compare_metrics call--------------------------------------------------------------------------------------------------------------------------
pbp_pred_rf %>% # Random Forest predictions
  metrics(truth = play_type, .pred_class)

pbp_pred_lr %>% # Logistic Regression predictions
  metrics(truth = play_type, .pred_class)


## ---- fig.dim=c(10,6)------------------------------------------------------------------------------------------------------------------------------
pbp_fit_rf %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)


## --------------------------------------------------------------------------------------------------------------------------------------------------
roc_rf <- pbp_pred_rf %>% 
  roc_curve(truth = play_type, .pred_pass) %>% 
  mutate(model = "Ranger")

roc_lr <- pbp_pred_lr %>% 
  roc_curve(truth = play_type, .pred_pass) %>% 
  mutate(model = "Logistic Regression")

full_plot <- bind_rows(roc_rf, roc_lr) %>% 
  # Note that autoplot() works here!
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  scale_color_manual(
    values = c("#374785", "#E98074")
    ) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())
  



## ---- fig.dim=c(6,6)-------------------------------------------------------------------------------------------------------------------------------
full_plot


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------
calibration_plot <- pbp_pred_rf %>% 
  mutate(
    pass = if_else(play_type == "pass", 1, 0),
    pred_rnd = round(.pred_pass, 1)
    ) %>% 
  group_by(pred_rnd) %>% 
  summarize(
    mean_pred = mean(.pred_pass),
    mean_obs = mean(pass),
    n = n()
    ) %>% 
  ggplot(aes(x = mean_pred, y = mean_obs)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size = n)) +
  theme_minimal() +
  labs(
    x = "Predicted Pass", 
    y = "Observed Pass"
    ) +
  coord_cartesian(
    xlim = c(0,1), ylim = c(0, 1)
    )


## ---- echo = FALSE, fig.dim=c(7,7)-----------------------------------------------------------------------------------------------------------------
calibration_plot


## ---- echo = FALSE, out.width="45%"----------------------------------------------------------------------------------------------------------------
knitr::include_graphics("images/resample-pic.png")


## --------------------------------------------------------------------------------------------------------------------------------------------------
vfold_cv(train_data, v = 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------
vfold_cv(train_data, v = 10, repeats = 5)


## ---- eval = FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------
## set.seed(20201024)
## # Create 10 folds and 5 repeats
## pbp_folds <- vfold_cv(train_data, v = 10, repeats = 5)
## 
## pbp_folds


## ---- echo = FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------
pbp_folds <- read_rds("pbp_folds.rds")
pbp_folds


## ---- eval = FALSE, message=FALSE------------------------------------------------------------------------------------------------------------------
## keep_pred <- control_resamples(save_pred = TRUE, verbose = TRUE)
## set.seed(20201024)
## # Fit resamples
## rf_res <- fit_resamples(rf_wflow, resamples = pbp_folds, control = keep_pred)
## 
## rf_res


## ---- echo = FALSE, message = FALSE----------------------------------------------------------------------------------------------------------------
rf_res <- read_rds("rf_res.rds")
rf_res


## --------------------------------------------------------------------------------------------------------------------------------------------------
# Naive Model on Testing Data
rf_compare_df <- bind_rows(
  accuracy(pbp_pred_rf, 
    truth = play_type, .pred_class),
  roc_auc(pbp_pred_rf, 
    truth = play_type, .pred_pass)
  )


## ---- fig.dim = c(4,4)-----------------------------------------------------------------------------------------------------------------------------
combo_plot <- rf_res %>% 
  collect_metrics(summarize = FALSE) %>% 
  ggplot(aes(x = .metric, y = .estimate)) +
  geom_jitter(width = 0.2) +
  geom_boxplot(width = 0.3, alpha = 0.5) +
  geom_point(
    data = rf_compare_df, #
    color = "red",
    size = 3 
    )


## ---- fig.dim=c(6,6), echo = FALSE-----------------------------------------------------------------------------------------------------------------
combo_plot


## --------------------------------------------------------------------------------------------------------------------------------------------------
assess_res <- collect_predictions(rf_res)

assess_res


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------
res_calib_plot <- assess_res %>% 
  mutate(
    pass = if_else(play_type == "pass", 1, 0),
    pred_rnd = round(.pred_pass, 2)
    ) %>% 
  group_by(pred_rnd) %>%
  summarize(
    mean_pred = mean(.pred_pass),
    mean_obs = mean(pass),
    n = n()
    ) %>% 
  ggplot(aes(x = mean_pred, y = mean_obs)) +
  geom_abline(linetype = "dashed") +
  geom_point(aes(size = n), alpha = 0.5) +
  theme_minimal() +
  labs(
    x = "Predicted Pass", 
    y = "Observed Pass"
    ) +
  coord_cartesian(
    xlim = c(0,1), ylim = c(0, 1)
    )


## ---- echo = FALSE, fig.dim=c(7,7)-----------------------------------------------------------------------------------------------------------------
res_calib_plot


## ---- echo = FALSE, out.width="25%"----------------------------------------------------------------------------------------------------------------
knitr::include_graphics("https://raw.githubusercontent.com/rstudio/hex-stickers/master/SVG/tune.svg")


## --------------------------------------------------------------------------------------------------------------------------------------------------
tune_pbp_rf <- rand_forest(
  mtry = tune(), # add placeholder for tune
  trees = 100,
  min_n = tune() # add placeholder for tune
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

tune_rf_wf <- workflow() %>% 
  add_recipe(pbp_rec) %>% 
  add_model(tune_pbp_rf)


## --------------------------------------------------------------------------------------------------------------------------------------------------
tune_rf_wf


## ---- eval = FALSE---------------------------------------------------------------------------------------------------------------------------------
## set.seed(37)
## 
## pbp_folds <- vfold_cv(train_data, v = 5)
## 
## tic()
## tune_res <- tune_grid(
##   tune_rf_wf,
##   resamples = pbp_folds,
##   grid = 15, # 15 combos of model parameters
##   control = control_grid(verbose = TRUE)
## )
## toc()
## # 1478.385 sec elapsed


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
tune_res <- read_rds("tuned_rf.rds")


## --------------------------------------------------------------------------------------------------------------------------------------------------
tune_res


## --------------------------------------------------------------------------------------------------------------------------------------------------
# Essentially the same as tune_res[[".metrics"]][[1]]
tune_res %>% 
  pluck(".metrics", 3)




## ---- fig.dim = c(8,10)----------------------------------------------------------------------------------------------------------------------------
plot_tuned <- tune_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry:min_n) %>%
  pivot_longer(mtry:min_n,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x", ncol = 1) +
  labs(x = NULL, y = "AUC")


## ---- fig.dim=c(4,6), echo = FALSE-----------------------------------------------------------------------------------------------------------------
plot_tuned


## ---- fig.dim = c(4,4)-----------------------------------------------------------------------------------------------------------------------------
plot_tuned <- tune_res %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry:min_n) %>%
  pivot_longer(mtry:min_n,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x", ncol = 1) +
  labs(x = NULL, y = "AUC")


## ---- fig.dim=c(4,6)-------------------------------------------------------------------------------------------------------------------------------
plot_tuned +
  scale_y_continuous(limits = c(0.75, 0.85))


## --------------------------------------------------------------------------------------------------------------------------------------------------
# Which 5x were best?
show_best(tune_res, "roc_auc", n = 5)

# Select the best
best_fit_auc <- select_best(tune_res, "roc_auc")

# Select wflow for the model with best hyperparams
rf_tuned <- finalize_workflow(
  rf_wflow,
  parameters = best_fit_auc
)


## --------------------------------------------------------------------------------------------------------------------------------------------------
rf_tuned_fit <- last_fit(rf_tuned, split_pbp)

rf_tuned_fit %>%  # tuned model metrics
  collect_metrics()


## --------------------------------------------------------------------------------------------------------------------------------------------------
rf_compare_df # naive model metrics

