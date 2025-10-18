
.pt <- 2.845276

# Default theme for all plots
theme_set(
  theme_minimal(base_size = 14, base_family = "Tenorite") %+replace%
    theme(
      legend.position  = "top",
      legend.box      = "horizontal",
      legend.direction= "horizontal",
      complete         = TRUE
    )
)

# Custom theme for presentation plots
.theme_ppt <- function() {
  theme(
    plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, color = 'gray30'),
    panel.grid.minor = element_blank()
  )
}