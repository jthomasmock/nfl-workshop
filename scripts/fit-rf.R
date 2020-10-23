rf_mod <- rand_forest(trees = 100) %>% 
  set_engine("ranger", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")
rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>%  # New model
  add_recipe(pbp_rec)    # Same recipe
pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest
# Get predictions and check metrics
pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(play_type)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))

pbp_pred_rf %>% # Random Forest predictions
  metrics(truth = play_type, .pred_class)

pbp_pred_lr %>% # Logistic Regression predictions
  metrics(truth = play_type, .pred_class)

pbp_fit_rf %>%
  pull_workflow_fit() %>% 
  vip::vip(num_features = 20)


pbp_pred_rf %>% 
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
