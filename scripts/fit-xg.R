xg_mod <- boost_tree(trees = 100) %>% 
  set_engine("xgboost", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")
xg_wflow <- workflow() %>% 
  add_model(xg_mod) %>%  # New model
  add_recipe(pbp_rec)    # Same recipe
pbp_fit_xg <- xg_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest
# Get predictions and check metrics
pbp_pred_xg <- predict(pbp_fit_xg, test_data) %>% 
  bind_cols(test_data %>% select(play_type)) %>% 
  bind_cols(predict(pbp_fit_xg, test_data, type = "prob"))

pbp_pred_xg %>% # Random Forest predictions
  metrics(truth = play_type, .pred_class)

pbp_pred_rf %>% # Random Forest predictions
  metrics(truth = play_type, .pred_class)


pbp_pred_lr %>% # Logistic Regression predictions
  metrics(truth = play_type, .pred_class)



# OS FOOTBALL -------------------------------------------------------------

post16 <- filter(pbp_raw, 
                 season_type == 'REG' & 
                   down %in% c(1,2,3) &
                   !is.na(qb_dropback) &
                   !is.na(score_differential),
                 qb_scramble == 0) %>%
  mutate(qb_dropback = factor(qb_dropback),
         off_to = if_else(posteam_type == 'away', away_timeouts_remaining, home_timeouts_remaining),
         def_to = if_else(posteam_type == 'away', home_timeouts_remaining, away_timeouts_remaining)) %>%
  dplyr::select(qb_dropback, down, ydstogo, yardline_100, score_differential, qtr, half_seconds_remaining, off_to, def_to)

dat_split <- initial_split(post16)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

qb_recipe <- recipe(qb_dropback ~ down + 
                      ydstogo + 
                      yardline_100 + 
                      score_differential + 
                      qtr + 
                      half_seconds_remaining +
                      off_to +
                      def_to,
                    data = dat_train)

qb_model <- boost_tree(
    mtry = tune(),
    trees = 2000, 
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),                    
    sample_size = tune()
    ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

qb_folds <- vfold_cv(dat_train)

qb_workflow <- workflow() %>%
  add_recipe(qb_recipe) %>%
  add_model(qb_model)

xgb_grid <- grid_latin_hypercube(
  finalize(mtry(), dat_train),
  min_n(),
  tree_depth(),
  learn_rate(),
  loss_reduction(),
  sample_size = sample_prop(),
  size = 30
)

xgb_res <- tune_grid(
  qb_workflow,
  resamples = qb_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

best_auc <- select_best(xgb_res, "roc_auc")

qb_xgb <- finalize_workflow(
  qb_workflow,
  parameters = best_auc
)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC") +
  theme_minimal()

final_mod <- last_fit(qb_xgb, dat_split)

collect_metrics(final_mod)

final_mod

xgb_res %>% write_rds("xgb-cv.rds")

final_mod %>%
  collect_predictions() %>%
  mutate(pred_rounded = round(.pred_1,1)) %>%
  group_by(pred_rounded) %>%
  summarise(mean_prediction = mean(.pred_1),
            mean_actual = mean(as.numeric(qb_dropback) - 1),
            n = n(),
            se = sd(as.numeric(qb_dropback) - 1 - .pred_1)/sqrt(n)) %>%
  ggplot(aes(x = pred_rounded, y = mean_actual)) +
  geom_abline() +
  geom_point(aes(size = n)) +
  theme_minimal() +
  xlab('Predicted Probability') +
  ylab('Actual Probability') +
  ggtitle('Calibration Plot, Test Data') +
  ylim(0,1) +
  xlim(0,1)

xg_mod <- boost_tree(trees = 1000) %>% 
  set_engine("xgboost") %>%     # Parallelize
  set_mode("classification")

xg_wflow <- workflow() %>% 
  add_recipe(qb_recipe) %>% 
  add_model(xg_mod)

pbp_fit_xg <- xg_wflow %>% # New workflow
  fit(data = dat_train)   # Fit the Random Forest

# Get predictions and check metrics
pbp_pred_xg <- predict(pbp_fit_xg, dat_test) %>% 
  bind_cols(dat_test %>% select(qb_dropback)) %>% 
  bind_cols(predict(pbp_fit_xg, dat_test, type = "prob"))

pbp_pred_xg %>% # Random Forest predictions
  metrics(truth = qb_dropback, .pred_class)


pbp_pred_xg %>% 
  mutate(pred_rounded = round(.pred_1,1)) %>%
  group_by(pred_rounded) %>% 
  summarise(mean_prediction = mean(.pred_1),
            mean_actual = mean(as.numeric(qb_dropback))-1,
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
