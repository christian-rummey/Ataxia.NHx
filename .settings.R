# Project Settings for Ataxia Natural History Analyses
# Source this file at the beginning of analysis scripts

# Load configuration
source('config/parameters.R')

# Load shared libraries
source('lib/data-prep.R')
source('lib/models.R')
source('lib/plotting.R')
source('lib/powerpoint.R')

# Constants
.pt <- 2.845276

# Color Palettes ----

# Age group colors
.colors.age_group <- c(
  "all"    = "#000000", # black for "all"
  "<8y"    = "#E41A1C", # red
  "8-11y"  = "#377EB8", # blue
  "12-15y" = "#4DAF4A", # green
  "16-24y" = "#984EA3", # purple
  "25-40y" = "#FF7F00", # orange
  ">40y"   = "#666666"  # dark gray
)

# Study colors
.colors.study <- c(
  "all"     = "#000000", # black for "all"
  "CRCSCA"  = "#E41A1C", # red
  "EUROSCA" = "#377EB8", # blue
  "UNIFAI"  = "#4DAF4A"  # green
)

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