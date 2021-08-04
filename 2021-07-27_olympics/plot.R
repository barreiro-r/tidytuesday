library(tidyverse)
library(ggsci)

my_link  <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv'

# filter NAs
olympics <- 
  read_csv(my_link, col_types = cols()) %>%
  filter(!is.na(year) & !is.na(age)) %>%
  filter(season == "Summer")

# Get athletes with most participation
most_recurrent_athletes <-
  olympics %>% filter(season == "Summer") %>% distinct(name,year) %>%
  count(name, sort = TRUE) %>% head(12)

my_data2plot <-
  olympics %>% filter(name %in% most_recurrent_athletes$name) %>%
  distinct(name,year,sport)

# Define plot order
my_order <-
  my_data2plot %>%
  group_by(name) %>%
  summarise(first_year = min(year), last_year = max(year)) %>%
  arrange(first_year,desc(last_year)) %>% pull(name)

# Get first and last participation age
my_age_labels <-
  olympics %>% filter(name %in% most_recurrent_athletes$name) %>%
  distinct(name,year,age) %>%
  group_by(name) %>%
  summarise(first_year = min(year), last_year = max(year), min_age = min(age), max_age = max(age)) %>%
  unite(first_year,min_age, col = "first", sep = "_") %>%
  unite(last_year, max_age, col = "last", sep = "_") %>%
  select(name,first,last) %>%
  gather(value = "year_age", key = 'time', -name) %>%
  select(-time) %>%
  separate(year_age, into = c('year','age'), sep = '_', convert = TRUE)

# Get plot limits
min_year <- my_data2plot %>% pull(year) %>% min()
max_year <- my_data2plot %>% pull(year) %>% max()

# Plot!
my_plot <-
  my_data2plot %>%
  ggplot(aes(x = year, y = name, color = sport)) +
  geom_point(size = 5) +
  geom_line(size = 5.8, alpha = 0.7, show.legend  = FALSE) +
  geom_text(data = my_age_labels, aes(label = age), color = 'white', size = 2.6) +
  scale_y_discrete(limits = my_order %>% rev) +
  scale_x_continuous(breaks = seq(min_year, max_year + 8, 8)) +
  scale_color_d3() +
  theme_minimal() +
  labs(x = NULL, y = NULL, color = NULL, 
       title = "How far can you go?", subtitle = "Top 12 athletes with most olympic participation") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    plot.title = element_text(face = 'bold'))

ggsave(filename = 'my_plot.pdf',
       plot     = my_plot,
       device   = 'pdf',
       width    = 6.98,
       height   = 3.72)




