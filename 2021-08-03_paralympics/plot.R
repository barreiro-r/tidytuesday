library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(gifski)
library(gganimate)
library(transformr)
library(showtext)
library(ggTimeSeries)
library(hrbrthemes)

# Loading data
tuesdata <- tidytuesdayR::tt_load('2021-08-03')
athletes <- tuesdata$athletes

# Main palette
ring_colors <- c(
  'Europe' = '#3582C4',
  'Asia' = '#F2B44D',
  'Africa' = '#000000',
  'Oceania' = '#4AA45A',
  'America' = '#DC4352'
  )

# Creating matrix all countries vs all years
world_raw <- ne_countries(scale = "small", returnclass = "sf") %>% 
  select(brk_a3, continent, geometry) %>% 
  mutate(continent = if_else(str_detect(continent,"America"),"America", continent))

all_abb <- world_raw %>% pull(brk_a3) %>% unique()

participating_abb <-
  athletes %>%
  filter(!is.na(abb) & !is.na(year) & str_detect(abb,'^[A-Z][A-Z][A-Z]$') == 1) %>% # Filtering NAs and misplaced fields
  distinct(abb, year) %>% mutate(is_in = 1) %>% pivot_wider(names_from = 'year', values_from = 'is_in')

# From matrix (wider) to longer format
participating_abb <-
  tibble(abb = all_abb) %>% left_join(participating_abb) %>%
  replace(is.na(.),0) %>% pivot_longer(cols = -abb, values_to = "is_in", names_to = "year") %>%
  rename(brk_a3 = 'abb')

world <- 
  world_raw %>% left_join(participating_abb, by = 'brk_a3') %>%
  filter(!is.na(year)) %>%
  mutate(year_int = as.integer(year))


# Sub plot
data2plot_sub <-
  participating_abb %>%
  left_join(ne_countries(scale = "small", returnclass = "sf") %>% select(brk_a3, continent)) %>%
  select(-geometry) %>%
  filter(is_in == 1) %>%
  count(year, continent)

data2plot_sub_leg <-
  data2plot_sub %>%
  group_by(year) %>%
  summarise(n = sum(n)/2) %>%
  mutate(continent = NA)

gsub <-
  ggplot(data = data2plot_sub, aes(x = year, y = n, fill = continent)) +
    stat_steamgraph(aes(group = continent)) +
    scale_fill_manual(values = ring_colors) +
    theme_ipsum_rc(grid="X") +
    geom_text(data = data2plot_sub_leg, aes(label = n *2), vjust = -.5, size = 3) +
    labs(x = NULL, y = NULL, fill = NULL, title = "Overall number of countries") +
    coord_cartesian(ylim = c(-40,40)) +
    theme(axis.text.y = element_blank(), 
          legend.position = 'NA',
          axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 7))

# Main plot
my_animation <-
  ggplot(data = world %>% mutate(is_in = if_else(is_in == 1, continent, "not_in"))) +
  geom_sf(aes(fill = is_in, group = continent),show.legend = FALSE, color = "white") +
  theme_void() +
  transition_states(as.integer(year) , state_length = 30, transition_length = 10) +
  annotation_custom(ggplotGrob(gsub), xmin = -200, xmax = -90, ymin = -50, ymax = 15) +
  scale_fill_manual( values = c(ring_colors, not_in = 'grey80')) +
  theme(text=element_text(family="Roboto Condensed"),
        plot.title = element_text(face = "bold", size = 12),
        plot.margin=unit(c(30,30,30,30),"pt")) +
  labs(
    title = "Countries on the Paralympics",
    subtitle = 'From 1980 and 2016, the number of countries competing at the paralympics doubled!\n\nYear: {as.integer(closest_state)}'
  )

# Plotting!
anim_save("tidytuesday_2021-08-03.gif", my_animation, width = 4500 / 4.5, height = 2600 / 4.5)


