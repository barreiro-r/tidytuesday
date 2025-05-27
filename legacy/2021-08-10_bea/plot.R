library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(glue)

# Loading data
tuesdata <- tidytuesdayR::tt_load('2021-08-10')

# by category
data2plot <-
  tuesdata$chain_investment %>% 
  filter(category %in% c('Federal', 'Private') & !str_detect(meta_cat, 'Total'))
  
p1 <-  
  data2plot %>%
  ggplot(aes(x = year, y = gross_inv_chain, fill = category, color = category))  +
  facet_wrap(~meta_cat, scales = 'free_y') +
  geom_area(alpha = 0.5, position = 'identity') +
  ggsci::scale_color_jco() +
  ggsci::scale_fill_jco() +
  labs(title = NULL, x = NULL, y = NULL) + 
  coord_cartesian(expand = c(0,0)) + 
  theme_minimal() +
  scale_y_continuous(label = scales::comma) + 
  theme(legend.position = 'none')


# total infra
data2plot_2 <-
  tuesdata$chain_investment %>% 
  filter(category %in% c('Federal', 'Private') & str_detect(meta_cat, 'Total infra'))

p2 <-
  data2plot_2 %>%
  ggplot(aes(x = year, y = gross_inv_chain, fill = category, color = category))  +
  geom_area(alpha = 0.5, position = 'identity') +
  ggsci::scale_color_jco() +
  ggsci::scale_fill_jco() +
  labs(title = "U.S. infrastructure <b style='color:#006AB3'>Federal</b> and <b style='color:#EABC00'>Private</b> investment ", subtitle = 'Total infrastructure', color = NULL, fill = NULL, x = NULL, y = "Gross Investment (chained 2021 dollars)") + 
  coord_cartesian(expand = c(0,0)) + 
  scale_y_continuous(label = scales::comma) + 
  theme_minimal() +
  theme(
    legend.position = 'none',
    plot.title = element_textbox_simple(),
    legend.key.size = unit(.4, "cm")
  ) +
  theme(legend.title.align = 0.5)

# merging
final_plot <-
  cowplot::plot_grid(p2,p1, ncol = 2)


# plotting
pdf(
  file = 'final_plot.pdf',
  width = 13.78,
  height = 4.33)
final_plot
dev.off()
