personal_theme <- function(base_size = 15) {
  theme_bw() +
    theme(
      axis.text.x       = element_text(size = 0.8 * base_size, color = "black"),
      axis.text.y       = element_text(size = 0.8 * base_size, color = "black"),
      axis.ticks.length = unit(0.15, "cm"),
      axis.title.x      = element_text(size = base_size, margin = margin(t = 30, b = 5), hjust = 0.5, face = 2),
      axis.title.y      = element_text(size = base_size, margin = margin(r = 30), vjust = 1, face = 2),
      legend.text       = element_markdown(size = 0.8 * base_size),
      legend.title      = element_text(size = 0.8 * base_size, face = 2),
      legend.key.size   = unit(base_size, "pt"),
      legend.key.width  = unit(base_size, "pt"),
      legend.position   = "right",
      panel.border      = element_rect(colour = "black", linewidth = 0.75, fill = NA),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      strip.text.x      = element_text(size = base_size, color = "black"),
      plot.margin       = margin(t = 0.5, r = 0.5, b = 1, l = 0.5, "cm"),
      plot.caption      = element_text(size = 0.8 * base_size, color = "black", hjust = 0.2, vjust = 0.2),
      plot.caption.position = "plot",
      plot.title        = element_text(size = base_size * 1.2, margin = margin(b = 30), face = 2)
    )
};
