library("tidyverse")
source("tour_de_france_2017/helpers.R")

# read the files (one file per stage)
files <- list.files(path = "tour_de_france_2017/data",
                    pattern = "^stage_[0-9]{1,2}\\.csv")

stages <- map(files,
              ~ read_delim(paste0("tour_de_france_2017/data/", .x),
                           delim = ";",
                           col_types = cols(position = "i",
                                            rider = "c",
                                            result = "c"))) %>%
  set_names(gsub("\\.csv", "", files))

# clean the files:
# - split the rider column in three separate columns: rider, country and team
# - get the time of the previous rider when it's missing for the current rider
# (the time is missing when riders are equally-ranked)
clean_files <- function(df) {
  df %>%
    mutate(rider = sub("(.*) \\((.*?)\\) (.*)", "\\1;\\2;\\3", rider)) %>%
    separate(rider, sep = ";", into = c("rider", "country", "team")) %>%
    mutate(result = case_when(
      stringr::str_count(result, ":") == 0 ~ NA_character_,
      TRUE ~ result)) %>%
    fill(result)
}

stages <- map(stages, clean_files)

# get the results of the riders in seconds
result_sec <- function(df) {
  gc_1st <- df %>%
    filter(position == 1) %>%
    pull(result) %>%
    lubridate::hms() %>%
    lubridate::period_to_seconds()
  
  df %>%
    mutate(result_sec = case_when(
      position == 1 ~ gc_1st,
      TRUE ~ lubridate::period_to_seconds(gc_1st + lubridate::hms(result))))
}

stages <- map(stages, result_sec)

# compute the gap between one rider (Christopher Froome by default)
# and the others
time_to <- function(df, to = "Christopher Froome") {
  rider_sec <- df %>%
    filter(rider == to) %>%
    pull(result_sec)
  
  df %>%
    mutate(time_to = result_sec - rider_sec)
}

stages <- map(stages, time_to)

# add the stage name (stage_xx) and bind the list of data frames into one
df <- map2_df(stages, gsub("\\.csv", "", files), ~ mutate(.x, stage = .y))

# add the final position in the gc for each rider
final_gc <- stages[[length(stages)]] %>%
  select(final_position = position, rider)

df <- left_join(df, final_gc, by = "rider")

# the profile of each stage
stages_profiles <- data_frame(stage = unique(df[["stage"]]),
                              profile = c("ITT", "flat", "hilly", "flat",
                                          "mountain", "flat", "flat",
                                          "mountain", "mountain", "flat",
                                          "flat", "mountain", "mountain",
                                          "hilly", "hilly", "flat",
                                          "mountain", "mountain", "flat",
                                          "ITT", "flat"))

# plot the time difference between C. Froome and the rest of the top 10
top_10 <- final_gc %>%
  filter(final_position %in% seq_len(10)) %>%
  pull(rider)

g <- df %>%
  filter(rider %in% top_10 & rider != "Christopher Froome") %>%
  mutate(rider = forcats::fct_reorder(rider, final_position)) %>%
  ggplot(aes(x = stage, y = time_to, group = rider, color = rider)) +
  geom_line(size = 0.85) +
  geom_point(size = 3) +
  geom_text(data = stages_profiles, aes(x = stage, y = 0,
                                        label = paste0("(", profile, ")")),
            vjust = 8, color = "grey30", size = 3, inherit.aes = FALSE) +
  scale_x_discrete(labels = paste0("Stage ", seq_len(length(stages)))) +
  labs(title = "A close race until the final stages",
       subtitle = paste0("Time difference (in seconds) between Christopher ",
                         "Froome and the rest of the top 10\n"),
       caption = "data: cyclingnews.com",
       x = "",
       y = "time difference (seconds)") +
  grey_theme() +
  theme(legend.position = "right",
        legend.title = element_blank())

# turn off the clipping to allow the geom_text to be "outside" of the plot
gg <- ggplotGrob(g)
gg$layout$clip[gg$layout$name == "panel"] = "off"
grid::grid.draw(gg)

# save the plot
# ggsave("tour_de_france_2017/plots/time_difference.png", gg,
#        width = 16, height = 9, dpi = 150)

# plot the rank of the riders of the final top 10 along the race
g <- df %>%
  filter(rider %in% top_10) %>%
  mutate(rider = forcats::fct_reorder(rider, final_position)) %>%
  ggplot(aes(x = stage, y = position, group = rider, color = rider)) +
  geom_line(size = 0.85) +
  geom_point(size = 3) +
  geom_text(data = stages_profiles, aes(x = stage, y = 0,
                                        label = paste0("(", profile, ")")),
            vjust = 8, color = "grey30", size = 3, inherit.aes = FALSE) +
  scale_x_discrete(labels = paste0("Stage ", seq_len(length(stages)))) +
  labs(title = paste0("Christopher Froome never left the top 6 ",
                      "of the general classification"),
       subtitle = "Rank of the riders of the final top 10 after each stage",
       caption = "data: cyclingnews.com",
       x = "",
       y = "position") +
  grey_theme() +
  theme(legend.position = "right",
        legend.title = element_blank())

gg <- ggplotGrob(g)
gg$layout$clip[gg$layout$name == "panel"] = "off"
grid::grid.draw(gg)

# save the plot
# ggsave("tour_de_france_2017/plots/rank_top10.png", gg,
#        width = 16, height = 9, dpi = 150)

# same type of plot for the 167 riders who finished the race
g <- ggplot(data = df, aes(x = stage, y = position, group = rider)) +
  geom_line(color = "grey60", size = 0.65) +
  geom_text(data = stages_profiles, aes(x = stage, y = 0,
                                        label = paste0("(", profile, ")")),
            vjust = 6, color = "grey30", size = 3, inherit.aes = FALSE) +
  scale_x_discrete(labels = paste0("Stage ", seq_len(length(stages))),
                   expand = c(0.02, 0)) +
  scale_y_continuous(breaks = c(seq(0, 175, 25), 196), expand = c(0.02, 0)) +
  labs(title = "Climbs do almost all the differences",
       subtitle = "Rank of the riders after each stage",
       caption = "data: cyclingnews.com",
       x = "",
       y = "position") +
  grey_theme() +
  theme(panel.grid.major = element_line(color = "grey80", size = 0.25))

gg <- ggplotGrob(g)
gg$layout$clip[gg$layout$name == "panel"] = "off"
grid::grid.draw(gg)

# save the plot
# ggsave("tour_de_france_2017/plots/rank_all.png", gg,
#        width = 16, height = 9, dpi = 150)

# get a list of the teams
teams <- unique(df[["team"]])

# create a list of colors
teams_colors <- c("#00ABDF", "#E2000F", "#E31E40", "#13459F", "#FDCE02",
                  "#78BD1A", "#000000", "#81BE41", "#3FBCCE", "#D0DE46",
                  "#012559", "#692E1E", "#000000", "#E31D1A", "#020100",
                  "#343233", "#008A2A", "#0B4EB3", "#45484A", "#0100D9",
                  "#E61244", "#123474")

draw_team_ranks <- function(team_filter, team_color) {
  ggplot(data = df, aes(x = stage, y = position, group = rider)) +
    geom_line(color = "grey60", size = 0.65) +
    geom_line(data = filter(df, team == team_filter),
              size = 1.2, color = team_color) +
    scale_x_discrete(labels = paste0("Stage ", seq_len(length(stages))),
                     expand = c(0.02, 0)) +
    scale_y_continuous(breaks = c(seq(0, 175, 25), 196), expand = c(0.02, 0)) +
    labs(title = team_filter,
         x = "",
         y = "position") +
    grey_theme() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 1))
}

teams_ranks <- map2(teams, teams_colors, draw_team_ranks) %>%
  set_names(teams)

# let's look at AG2R La Mondiale et FDJ
ag2r_fdj <- gridExtra::grid.arrange(teams_ranks[["AG2R La Mondiale"]],
                                    teams_ranks[["FDJ"]],
                                    ncol = 2)

# save the plot
# ggsave("tour_de_france_2017/plots/ag2r_fdj.png", ag2r_fdj,
#        width = 16, height = 9, dpi = 150)

# a sort of a radar plot for each stage

# the function below is adapted from this blog post:
# http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html

coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  if (theta == "x") {
    r <- "y"
  } else {
    r <- "x"
  }
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

draw_radar <- function(df, stage, top = 5, ymin = -10, ymax = 200) {
  add_zero <- function(x) {
    if (x < 10) {
      paste0("0", x)
    } else {
      as.character(x)
    }
  }
  
  stages_ <- map_chr(seq_len(stage), ~ add_zero(.x))
  
  ggplot() +
    geom_polygon(data = df %>%
                   filter(stage %in% paste0("stage_",
                                            stages_[1:(length(stages_) - 1)]) &
                            rider != "Christopher Froome") %>%
                   filter(final_position %in% seq_len(top)) %>%
                   arrange(final_position),
                 aes(x = rider, y = time_to, group = stage),
                 color = "grey60", fill = NA, size = 1, show.legend = FALSE) +
    geom_polygon(data = df %>%
                   filter(stage == paste0("stage_",
                                          stages_[length(stages_)]) &
                            rider != "Christopher Froome") %>%
                   filter(final_position %in% seq_len(top)) %>%
                   arrange(final_position),
                 aes(x = rider, y = time_to, group = stage),
                 color = "#000000", fill = NA, size = 1, show.legend = FALSE) +
    scale_x_discrete(limits = setdiff(top_10,
                                      "Christopher Froome")[1:(top - 1)]) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(title = "Christopher Froome and his opponents",
         subtitle = paste0("After stage ", stage,
                          " (", stages_profiles[stage, "profile"], ")"),
         caption = "data: cyclingnews.com",
         x = "", y = "") +
    coord_radar() +
    grey_theme() +
    theme(strip.text.x = element_text(size = rel(2)),
          axis.text.x = element_text(size = rel(2)),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
}

stages_radar <- map(seq_len(length(stages)),
                    ~ draw_radar(df = df, stage = .x,
                                 top = 5, ymin = -10, ymax = 200))

# save the plots
for (i in seq_along(stages_radar)) {
  ggsave(paste0("tour_de_france_2017/plots/radar_", i, ".png"),
         stages_radar[[i]], width = 16, height = 9, dpi = 150)
}
