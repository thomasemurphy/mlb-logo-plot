library(ggplot2)
library(baseballr)
library(tidyverse)
library(lubridate)
library(mlbplotR)

setwd('baseball')

season <- 2024

team_hitting_stats <- baseballr::mlb_teams_stats(
  stat_type = 'season',
  stat_group = 'hitting',
  sport_ids = 1,
  season = season
  )

team_pitching_stats <- baseballr::mlb_teams_stats(
  stat_type = 'season',
  stat_group = 'pitching',
  sport_ids = 1,
  season = season
)

team_stats <- team_hitting_stats %>%
  left_join(
    team_pitching_stats,
    by = 'team_name',
    suffix = c('_off', '_def')
  )

teams_colors_logos <- mlbplotR::load_mlb_teams()

team_stats %>%
  # select(team_name, avg, obp, slg, ops) %>%
  arrange(desc(slg_off)) %>%
  print(n = 30)

to_plot <- team_stats %>%
  left_join(
      teams_colors_logos %>% select(team_name, team_abbr),
      by = 'team_name'
      ) %>%
  mutate(
    rpg_off = runs_off / games_played_off,
    rpg_def = runs_def / games_played_def
    )

median_rpg_def <- median(to_plot$rpg_def)
median_rpg_off <- median(to_plot$rpg_off)

mid_rpg_off <- (max(to_plot$rpg_off) + min(to_plot$rpg_off)) / 2
mid_rpg_def <- (max(to_plot$rpg_def) + min(to_plot$rpg_def)) / 2

x_axis_limits <- c(
  median_rpg_off - (max(to_plot$rpg_off) - median_rpg_off) - .3,
  max(to_plot$rpg_off) + .3
)

y_axis_limits <- c(
  max(to_plot$rpg_def) + .1,
  median_rpg_def - (max(to_plot$rpg_def) - median_rpg_def) - .1
)

text_df_y <- data.frame(
  y = c(4, 5, 6),
  x = median_rpg_off,
  label = c(
    '4',
    '5',
    '6'
  )
)

text_df_x <- data.frame(
  x = c(4, 5, 6),
  y = median_rpg_def,
  label = c(
    '4',
    '5',
    '6'
  )
)

ggplot(
  to_plot,
  aes(
    x = rpg_off,
    y = rpg_def)
  ) +
  geom_mlb_logos(
    aes(
      team_abbr = team_abbr,
    ),
    width = 0.055,
    alpha = 0.8
    ) +
  # geom_point() +
  geom_hline(
    yintercept = median_rpg_def,
    linewidth = 0.15,
    alpha = 0.3
    ) +
  geom_vline(
    xintercept = median_rpg_off,
    linewidth = 0.15,
    alpha = 0.3
    ) +
  scale_y_reverse(
    # limits = y_axis_limits
    limits = c(6.2, 3.5)
  ) +
  scale_x_continuous(
    # limits = x_axis_limits
    limits = c(3, 6.1)
  ) +
  geom_text(
    data = text_df_y,
    mapping = aes(x = x, y = y, label = label),
    size = 24,
    hjust = 'center',
    vjust = 'center',
    color = '#999999',
    alpha = 0.2
    ) +
  geom_text(
    data = text_df_x,
    mapping = aes(x = x, y = y, label = label),
    size = 24,
    hjust = 'center',
    vjust = 'center',
    color = '#999999',
    alpha = 0.2
  ) +
  annotate(
    'text',
    x = median_rpg_off + .08,
    y = 6.15,
    label = 'runs allowed per game',
    color = '#999999',
    alpha = 0.7,
    size = 4,
    hjust = 'left',
    vjust = 'bottom'
  ) +
  annotate(
    'text',
    x = 6,
    y = median_rpg_def + .18,
    label = 'runs scored\nper game',
    color = '#999999',
    alpha = 0.7,
    size = 4,
    hjust = 'center',
    vjust = 'top'
  ) +
  theme_void()