# a grey and minimalist theme
theme_minimal_grey <- function(base_size = 14,
                               axis_title_size = 16,
                               axis_text_size = 10,
                               title_size = 24,
                               subtitle_size = 16) {
  bg_color <- "#fcfcfc"
  bg_rect <- element_rect(fill = bg_color, color = bg_color)
  
  theme_minimal(base_size = base_size) +
    theme(axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = axis_text_size, color = "grey30"),
          axis.ticks = element_blank(),
          plot.background = bg_rect,
          panel.background = bg_rect,
          panel.border = element_blank(),
          panel.grid.major = element_line(color = "grey80", size = 0.25),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = title_size, color = "black"),
          plot.subtitle = element_text(size = subtitle_size, color = "black"))
}

# a dark and minimalist theme
theme_minimal_dark <- function(base_size = 14,
                               axis_title_size = 16,
                               axis_text_size = 10,
                               title_size = 24,
                               subtitle_size = 16) {
  bg_color <- "grey10"
  bg_rect <- element_rect(fill = bg_color, color = bg_color)
  
  theme_minimal(base_size = base_size) +
    theme(axis.title = element_text(size = axis_title_size),
          axis.text = element_text(size = axis_text_size, color = "grey60"),
          axis.ticks = element_blank(),
          plot.background = bg_rect,
          panel.background = bg_rect,
          panel.border = element_blank(),
          panel.grid.major = element_line(color = "grey20", size = 0.25),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = title_size, color = "grey70"),
          plot.subtitle = element_text(size = subtitle_size, color = "grey70"),
          plot.caption = element_text(color = "grey70"))
}
