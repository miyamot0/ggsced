library(ggsced)
library(tidyverse)
library(ggh4x)

data_set = Gilroyetal2015

data_labels = data_set %>%
  select(Participant, Condition) %>%
  filter(Participant == "Andrew") %>%
  unique() %>%
  mutate(x = c(2.5, 9, 18.5, 25.5),
         y = 100)

y_mult = .05
x_mult = .02

p = ggplot(data_set, aes(Session, Responding,
                         group = Condition)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(data = data_labels,
            mapping = aes(x, y,
                          label = Condition),
            hjust = 0.5,
            vjust = 0.0625) +
  scale_y_continuous(name = "Percentage Accuracy",
                     limits = c(0, 102.5),
                     breaks = (0:4) * 25,
                     expand = expansion(mult = y_mult)) +
  scale_x_continuous(breaks = c(1:27),
                     limits = c(1, 27),
                     expand = expansion(mult = x_mult)) +
  facet_grid2(Participant ~ .,
              remove_labels = "x",
              axes = "x") +
  theme(
    text = element_text(size = 14,
                        color = 'black'),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  gg_sced_style_x(x_mult, lwd = 2) +
  gg_sced_style_y(y_mult, lwd = 2)

staggered_pls = list(
  '1' = c(4.5,  11.5, 18.5),
  '2' = c(13.5, 20.5, 23.5),
  '3' = c(23.5, 23.5, 23.5)
)

#svg(filename = 'figs/GilroyEtAl2015.svg',
#    width = 8,
#    height = 6)

gg_sced(p, legs = staggered_pls)

#dev.off()
