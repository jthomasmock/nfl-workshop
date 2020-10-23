library(tidyverse)
library(espnscrapeR)
library(broom)
library(ggtext)
library(gt)

all_qbr <- 2006:2019 %>% 
  map_dfr(get_nfl_qbr)

all_wins <- 2006:2019 %>% 
  map_dfr(get_nfl_standings)

all_wins

all_qbr %>% 
  filter(season == 2019) %>% 
  mutate(above_50 = if_else())
  group_by(name) %>% 
  mutate(
    )


comb_data <- all_wins %>% 
  select(season = year, team = abb_name, wins, win_percent) %>% 
  left_join(
    all_qbr %>% 
      select(season, team, name, qbr_total, total_epa),
    by = c("season", "team")
  ) %>% 
  filter(!is.na(name)) 

sum_data <- comb_data %>% 
  group_by(wins) %>% 
  summarise(
    win_percent = median(win_percent),
    qbr_total = median(qbr_total),
    n= n()
    )

sum_data %>% 
  arrange(desc(win_percent)) %>% 
  gt() %>% 
  fmt_percent(
    columns = vars(win_percent),
    decimals = 1
    ) %>% 
  fmt_number(
    columns = vars(qbr_total),
    decimals = 1
    ) %>% 
  data_color(
    columns = vars(qbr_total), 
    colors = scales::col_numeric(palette = c("#f9f9ff", "#4C4CFF"), domain = NULL)
  )

comb_data %>% 
  ggplot(aes(x = qbr_total, y = win_percent)) +
  geom_point(alpha = 0.5) +
  geom_point(data = sum_data, color = "red", size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  geom_abline(intercept = 0,  color = "black", size = 1, alpha = 0.5, slope = 0.01) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(limits = c(0, 100))

lm(win_percent ~ qbr_total, data = comb_data) %>% 
  broom::glance()

all_off <- 2006:2019 %>% 
  map_dfr(scrape_team_stats_nfl)

long_off_stats <- all_off %>% 
  select(team, pass_att:sacks, season,-pass_long, -pass_comp) %>% 
  mutate(season2 = season + 1) %>% 
  pivot_longer(
    cols = c(-team, -season, -season2), 
    names_to = "metric", 
    values_to = "value") %>% 
  bind_rows(
  all_qbr %>% 
  select(season, team, qbr_total, total_epa) %>% 
  mutate(season2 = season + 1) %>% 
  pivot_longer(
    cols = c(qbr_total, total_epa),
    names_to = "metric", 
    values_to = "value"
    ) %>% 
    left_join(all_wins %>% 
                select(team = abb_name, team_name)) %>% 
    select(team = team_name, season, season2, metric, value) %>% 
    distinct(team, season, metric, .keep_all = TRUE)
  )

join_years <- long_off_stats %>% 
  inner_join(long_off_stats, by = c("season2" = "season", "team", "metric")) %>% 
  select(everything(), -season2.y)

join_years

nest_off_data <- join_years %>% 
  nest(data = c(-metric))

nest_off_data 

tidy_off_models <- nest_off_data %>% 
  mutate(
    fit = map(data, ~ lm(value.y ~ value.x, data = .x)),
    tidy_output = map(fit, glance)
  )

tidy_off_models

off_lm_output <- tidy_off_models %>% 
  hoist(tidy_output, r.squared = "r.squared") %>% 
  arrange(desc(r.squared))

off_lm_output

off_stability <- off_lm_output %>% 
  select(metric, r.squared) %>% 
  mutate(metric_label = glue::glue("{metric} (R^2 = {round(r.squared, 3)})"))

off_stability

(off_plot <- long_off_stats %>% 
    inner_join(long_off_stats, by = c("season2" = "season", "team", "metric")) %>% 
    mutate(metric = factor(metric,
                           levels = pull(off_stability, metric),
                           labels = pull(off_stability, metric_label))) %>% 
    ggplot(aes(x = value.x, y = value.y)) +
    geom_point(color = "black", alpha = 0.5) +
    geom_smooth(method = "lm", color = "#ff2b4f", se = FALSE) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    geom_abline(intercept = 0, slope = 1, color = "grey", size = 1, alpha = 0.5) +
    facet_wrap(~metric, scales = "free") +
    labs(x = "\nYear Y Metric", 
         y = "Year Y + 1 Metric\n",
         title = "Offense Stats - 2000-2019",
         subtitle = "Linear model fit comparing Year and Year + 1",
         caption = "Plot: @thomas_mock | Data: espnscrapeR")  +
    theme_minimal() +
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_textbox(face = "bold", color = "white")))



# PFR Data ----------------------------------------------------------------
# Weekly Wins vs EPA

c(2006:2019)


get_nfl_qbr()

all_qbr_week <- crossing(
  season = 2006:2019,
  week = 1:17
) %>% 
  pmap_dfr(get_nfl_qbr)


weekly_wins <- scrape_nfl_weekly_standings(season = 2006, tidy = TRUE)

weekly_wins <- 2006:2019 %>% 
  map_dfr(~scrape_nfl_weekly_standings(.x, tidy = TRUE))

weekly_wins %>% 
  group_by(season, team) %>% 
  count(winner)

all_qbr_week

all_teams <- get_nfl_teams()

comb_wins <- all_qbr_week %>% 
  left_join(
    all_teams %>% 
      select(team = team_short_name, full_name),
    by = "team"
  ) %>% 
  select(season, game_week, team_abb = team, team = full_name, everything()) %>% 
  left_join(
    weekly_wins %>% 
      select(season, game_week = week, team, points, yards, turnovers, winner),
    by = c("season", "team", "game_week")
  ) 

stand_2019 <- get_nfl_standings()

team_df <- get_nfl_teams() %>% 
  select(team_abb = team_short_name, logo) %>% 
  mutate(team_abb = if_else(team_abb == "LV", "OAK", team_abb))


# Expected Wins by QBR ----------------------------------------------------



comb_wins %>% 
  filter(season >= 2019) %>%
  filter(str_length(team_abb) <= 3) %>% 
  mutate(exp_wins = qbr_total/100) %>% 
  group_by(season, team_abb) %>% 
  summarise(exp_wins = sum(exp_wins)) %>% 
  left_join(stand_2019 %>% 
              select(team_abb = abb_name, wins)) %>%
  left_join(team_df, by = "team_abb") %>% 
  ungroup() %>% 
  mutate(diff = exp_wins - wins) %>% 
  arrange(desc(exp_wins)) %>% 
  select(season, team_abb, logo, everything()) %>% 
  gt() %>% 
  cols_label(
    season = "Season",
    team_abb = "Team",
    logo = "",
    exp_wins = "Exp Wins",
    wins = "Actual Wins",
    diff = "Difference"
  ) %>% 
  fmt_number(
    columns = c(4, 6),
    decimals = 1
  ) %>% 
  data_color(
    columns = 6,
    colors = scales::col_numeric(palette = c("#af8dc3", "#f7f7f7", "#7fbf7b"), domain = c(-5, 5))
  ) %>% 
  text_transform(
    locations = cells_body(vars(logo)),
    fn = function(x){
      web_image(url = x, height = 30)
    }
  )




# Comb Wins by Week -------------------------------------------------------



comb_wins %>% 
  mutate(round_qbr = 1 * qbr_total %/% 1,
         round_epa = 0.5 * total_epa %/% 0.5) %>% 
  group_by(round_qbr) %>%
  # group_by(round_epa) %>% 
  summarize(win_pct = mean(winner, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(x = round_qbr, y = win_pct)) +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_abline(intercept = 0, slope = 0.01, size = 1, color = "black", alpha = 0.5)

comb_epa_sum <- comb_wins %>% 
  mutate(round_qbr = 1 * qbr_total %/% 1,
         round_epa = 0.5 * total_epa %/% 0.5) %>% 
  group_by(round_epa) %>%
  summarize(win_pct = mean(winner, na.rm = TRUE),
            n = n())

comb_epa_sum %>% 
  ggplot(aes(x = round_epa, y = win_pct)) +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = .1)
  ) +
  scale_x_continuous(
    breaks = seq(-5, 15, by = 2.5)
  ) +
  scale_size(name = "Games") +
  geom_abline(intercept = 0, slope = 0.05, size = 1, color = "black", alpha = 0.5) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(family = "Chivo"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size= 24, face = "bold"),
    plot.subtitle = element_markdown(size= 18),
    plot.caption = element_markdown(size = 16, hjust = 1),
    legend.position = c(0.15, 0.75),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
  ) +
  labs(
    x = "\nTotal Expected Points Added", y = "Observed Win Rate",
    title = "Win Percentage vs Expected Points Added",
    subtitle = "Binned in 1 Point QBR Increments, R^2 : 0.95",
    caption = "<br>**Data**: ESPN + PFR<br>**Plot**: @thomas_mock"
  )

qbr_win_plot <- comb_wins %>% 
  mutate(round_qbr = 1 * qbr_total %/% 1,
         round_epa = 0.1 * total_epa %/% 0.1) %>% 
  group_by(round_qbr) %>%
  summarize(win_pct = mean(winner, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(x = round_qbr, y = win_pct)) +
  geom_point(aes(size = n), alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", color = "red") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = .1)
    ) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10)
  ) +
  scale_size(name = "Games") +
  geom_abline(intercept = 0, slope = 0.01, size = 1, color = "black", alpha = 0.5) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    text = element_text(family = "Chivo"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    plot.title = element_text(size= 24, face = "bold"),
    plot.subtitle = element_markdown(size= 18),
    plot.caption = element_markdown(size = 16, hjust = 1),
    legend.position = c(0.15, 0.75),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
  ) +
  labs(
    x = "\nQBR", y = "Observed Win Rate",
    title = "Win Percentage vs QBR Score",
    subtitle = "Binned in 1 Point QBR Increments, R^2 : 0.95",
    caption = "<br>**Data**: ESPN + PFR<br>**Plot**: @thomas_mock"
    )

qbr_win_plot


# Density plot of EPA/QBR -------------------------------------------------

comb_wins %>% 
  filter(!is.na(winner)) %>% 
  ggplot(aes(x = qbr_total, y = factor(winner))) +
  ggridges::geom_density_ridges(quantiles = TRUE)
  mutate(round_qbr = 1 * qbr_total %/% 1,
         round_epa = 0.1 * total_epa %/% 0.1) %>% 
  group_by(round_qbr) %>%
  summarize(win_pct = mean(winner, na.rm = TRUE),
            n = n()) %>% 
  

tab_out <- comb_wins %>% 
  mutate(round_qbr = 10 * qbr_total %/% 10,
         round_epa = 0.1 * total_epa %/% 0.1) %>% 
  group_by(round_qbr) %>%
  # group_by(round_epa) %>% 
  summarize(win_pct = mean(winner, na.rm = TRUE),
            n = n()) %>% 
  arrange(desc(round_qbr)) %>% 
  select(win_pct, round_qbr, n) %>% 
  gt() %>% 
  cols_label(
    win_pct = "Won Game",
    round_qbr = "QBR bin",
    n = "Games"
  ) %>% 
  fmt_percent(vars(win_pct), decimals = 1) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "transparent",
      weight = 3
    ),
    locations = cells_body(
      rows = nrow(.[["_data"]])
    )
  ) %>% 
  cols_width(
    1 ~ px(100),
    2 ~ px(100),
    3 ~ px(100)
  ) %>% 
  data_color(
    columns = vars(win_pct, round_qbr),
    colors = scales::col_numeric(palette = c("#f9f9ff", "#4C4CFF"), domain = NULL)
  ) %>% 
  tab_header(
    title = md("**QBR vs Win Percentage**"),
    subtitle = "Data from 2006-19"
  ) %>% 
  tab_source_note(
    source_note = md("**Data:** ESPN & PFR | **Table:** @thomas_mock")
  ) %>% 
  tab_footnote(
    "QBR binned into 10 point segments",
    locations = cells_column_labels(2)
  ) %>% 
  tab_options(
    heading.align = "left",
    heading.title.font.size = px(24),
    heading.subtitle.font.size = px(20),
    heading.border.bottom.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    column_labels.font.weight = "bold",
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "transparent"
  ) %>% 
  opt_table_font(font = google_font("Chivo"))

tab_out

summary_fun <- function(data){
  data %>% 
    group_by(value) %>% 
    summarize(n = n(), win_pct = mean(winner, na.rm = TRUE)) 
}

comb_wins %>% 
  mutate(
    qbr = 1 * qbr_total %/% 1,
    epa = 0.1 * total_epa %/% 0.1
    ) %>% 
  select(winner, qbr, epa) %>% 
  pivot_longer(cols = c(qbr, epa), names_to = "metric") %>% 
  group_nest(metric) %>% 
  mutate(
    data = map(data, summary_fun),
    fit = map(data, ~ lm(win_pct ~ value, data = .x)),
    tidy_output = map(fit, glance)
    ) %>% 
  hoist(tidy_output, r.squared = "r.squared")



sum_win_qbr <- comb_wins %>% 
  mutate(round_qbr = 1 * qbr_total %/% 1,
         round_epa = 0.5 * total_epa %/% 0.5) %>% 
  group_by(round_qbr) %>%
  summarize(
    win_pct = mean(winner, na.rm = TRUE),
    n = n()
  )

sum_win_epa <- comb_wins %>% 
  mutate(round_qbr = 1 * qbr_total %/% 1,
         round_epa = 0.5 * total_epa %/% 0.5) %>% 
  group_by(round_epa) %>% 
  summarize(win_pct = mean(winner, na.rm = TRUE),
            n = n())

lm(win_pct ~ round_qbr, data = sum_win_qbr) %>% 
  glance()

lm(win_pct ~ round_epa, data = sum_win_epa) %>% 
  glance()

comb_wins %>% 
  filter(!is.na(winner)) %>% 
  mutate(winner = factor(winner, levels = c(0, 1), labels = c("Lost", "Won"))) %>% 
  ggplot(aes(x = qbr_total, y = winner, group = winner, fill = factor(stat(quantile)))) +
  ggridges::stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE,
    color = "white", size = 1
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_fill_manual(
    name = "Quartiles", 
    values = c("#003399", "#ff2b4f", "#fcab27", "#88398a")
    ) +
  guides(fill = guide_legend(override.aes = list(linetype = 0))) +
  ggridges::theme_ridges() +
  theme(
    text= element_text(family = "Chivo"),
    axis.title.x = element_text(hjust = 0, size = 18),
    axis.text.y = element_text(size = 24, vjust = -0.5, margin = margin(r = -45)),
    axis.text = element_text(size = 16),
    plot.title = element_text(size= 20),
    plot.subtitle = element_text(size= 18),
    plot.caption = element_text(size = 16),
    panel.grid.major.y = element_line(color = "white", size = 1),
    legend.position = c(0.1, 0.8),
    legend.background = element_rect(fill = "white"),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
    ) +
  
  labs(x = "QBR", y = "",
       title = "QBR distribution in Wins vs Losses",
       subtitle = "Game-level QBR from 2006-19",
       caption = "Data: ESPN | Plot: @thomas_mock")


# 2020 Data ---------------------------------------------------------------

qbr_week_2020 <- crossing(
  season = 2020,
  week = 1:6
) %>% 
  pmap_dfr(get_nfl_qbr)


weekly_wins_2020 <- scrape_nfl_weekly_standings(season = 2020, tidy = TRUE) %>% 
  filter(yards >= 1) %>% 
  mutate(winner = if_else(is.na(winner), 0.5, winner))


weekly_wins_2020 %>% 
  group_by(season, team) %>% 
  count(winner) %>% 
  print(n = 50) %>% 


all_teams <- get_nfl_teams()

comb_win_2020 <- qbr_week_2020 %>% 
  left_join(
    all_teams %>% 
      select(team = team_short_name, full_name),
    by = "team"
  ) %>% 
  select(season, game_week, team_abb = team, team = full_name, everything()) %>% 
  left_join(
    weekly_wins_2020 %>% 
      select(season, game_week = week, team, points, yards, turnovers, winner),
    by = c("season", "team", "game_week")
  ) 

comb_win_2020 %>% 
  group_by(team_abb) %>% 
  mutate(wins = sum(winner)) %>% 
  group_by(team_abb, headshot_href) %>% 
  summarize(exp_wins = sum(qbr_total/100),
            wins = wins) %>% 
  distinct(team_abb, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(diff = wins - exp_wins) %>% 
  arrange(desc(exp_wins)) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(headshot_href)),
    fn = function(x){
      web_image(url = x, height = 30)
    }
  ) %>% 
  fmt_number(
    columns = c(3,5),
    decimals = 1
  )

#############
# https://www.pro-football-reference.com/years/2019/opp.htm
# NET YARDS/ATTEMPT
# YDS/ATT
# YDS/play
# 

team_def <- 2006:2019 %>% 
  map_dfr(~scrape_team_stats_nfl(.x, role = "defense"))

lm(td_pct ~ yds_att, data = team_def %>% 
     mutate(td_pct = pass_td/pass_att)) %>% 
  glance()

%>% 
  ggplot(aes(x = td_pct, y = yds_att)) + 
  geom_point()

