library(dplyr)
library(tidyr)
library(stringr)
library(curl)
library(ggplot2)
library(ggimage)
library(ggdist)
library(ggrepel)
library(ggtext)

# ---- Global Theme ----

theme_set(
  theme_minimal() +
    theme(
      plot.title.position = "plot",
      plot.title = element_markdown(family = "Roboto Condensed", size = 14),
      plot.margin = margin(25, 25, 25, 25),
      axis.title.x = element_markdown(family = "Roboto Condensed", hjust = .5, size = 12),
      axis.title.y = element_markdown(family = "Roboto Condensed", hjust = .5, size = 12),
      axis.text = element_markdown(family = "Roboto Condensed", hjust = .5, size = 8),
      legend.position = "top",
      panel.grid.minor = element_blank(),
      text = element_text(family = "Roboto Condensed")
    )
)

# ---- Data Preparation ----

tuesdata <- tidytuesdayR::tt_load(2025, week = 13)
pokemon_df <- tuesdata$pokemon_df |>
  filter(!is.na(url_icon)) |>
  mutate(
    total = hp + attack + defense + special_attack + special_defense + speed,
    url_icon = str_c("https:", url_icon)
  ) |>
  select(pokemon, url_icon, hp, attack, defense, special_attack, special_defense, speed, total) |>
  pivot_longer(
    cols = -c(pokemon, url_icon),
    names_to = "status",
    values_to = "value"
  ) |>
  mutate(local_path = str_c("images/", pokemon, ".png"))

# ---- Z-score Normalization and Outlier Detection ----

pokemon_df <- pokemon_df |>
  group_by(status) |>
  mutate(
    value = (value - mean(value)) / sd(value),
    outlier = value > mean(value) + 3 * sd(value)
  ) |>
  ungroup()

# ---- Image Downloading for Outliers ----

dir.create("images", showWarnings = FALSE)
no_ssl_handle <- curl::new_handle(ssl_verifyhost = 0, ssl_verifypeer = 0)

Map(
  function(url, path) {
    try(curl::curl_download(url, path, mode = "wb", handle = no_ssl_handle), silent = TRUE)
  },
  pokemon_df |> filter(outlier) |> pull(url_icon),
  pokemon_df |> filter(outlier) |> pull(local_path)
)

# ---- Helper Functions ----

capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))
}

prettify_label <- function(x) {
  x <- gsub("_", " ", x)
  ifelse(tolower(x) == "hp", "HP", capitalize_first(x))
}

# ---- Color Palette & Order ----

my_palette <- c(
  hp = "#EB4934",
  attack = "#F57F20",
  defense = "#F4E240",
  special_attack = "#659BFF",
  special_defense = "#8ED674",
  speed = "#F07CBA",
  total = "#A2DEF6"
)

desired_order <- c("hp", "attack", "defense", "special_attack", "special_defense", "speed", "total")

# ---- Plot ----
pokemon_df |>
  ggplot(aes(x = value, y = status)) +
  stat_gradientinterval(alpha = 0, aes(fill = status)) +
  ggimage::geom_image(
    data = pokemon_df |> filter(outlier),
    aes(image = local_path),
    size = 0.1
  ) +
  ggrepel::geom_text_repel(
    data = pokemon_df |> filter(outlier),
    aes(label = capitalize_first(pokemon)),
    nudge_y = -0.4,
    size = 2,
    segment.colour = NA,
    point.padding = 0,
    box.padding = 0.1
  ) +
  guides(color = "none", fill = "none") 


pokemon_df |>
  ggplot(aes(x = value, y = status)) +
  stat_gradientinterval(alpha = 0, aes(fill = status)) +
  ggimage::geom_image(
    data = pokemon_df |> filter(outlier),
    aes(image = local_path),
    size = 0.1
  ) +
  ggrepel::geom_text_repel(
    data = pokemon_df |> filter(outlier),
    aes(label = capitalize_first(pokemon)),
    nudge_y = -0.4,
    size = 2,
    segment.colour = NA,
    point.padding = 0,
    box.padding = 0.1
  ) +
  scale_y_discrete(
    labels = prettify_label,
    limits = rev(desired_order)
  ) +
  scale_color_manual(values = my_palette) +
  scale_fill_manual(values = my_palette) +
  guides(color = "none", fill = "none") +
  labs(
    x = "Status Value (z-score)",
    y = NULL,
    title = "**Pokémon Stat Distribution**",
    subtitle = 'Only outliers (> 3 SD) are shown as images'
  ) +
  theme(panel.grid.major.y = element_blank())
