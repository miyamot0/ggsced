rm(list = ls())

library(ggsced)
library(tidyverse)
library(ggh4x)

data <- Gilroyetal2021

y_mult = .05
x_mult = .02

data_labels = data %>%
  select(Participant, Condition) %>%
  filter(Participant == "John") %>%
  unique() %>%
  mutate(x = c(2, 5, 8, 11, 14, 18),
         Label = gsub("2", "", Condition),
         y = 20)

p = ggplot(data, aes(Session, Responding,
                 group = Condition)) +

  geom_line() +
  geom_point(size = 2.5,
             pch = 21,
             fill = 'black') +

  geom_line(mapping = aes(Session, Reinforcers),
            lty = 2) +
  geom_point(mapping = aes(Session, Reinforcers),
             size = 2.5,
             pch = 24,
             fill = 'white') +

  geom_text(data = data_labels,
            mapping = aes(x, y,
                          label = Label),
            family = "Times New Roman",
            inherit.aes = FALSE) +

  scale_x_continuous(breaks = c(1:25),
                     limits = c(1, 25),
                     expand = expansion(mult = x_mult)) +
  facet_grid2(Participant ~ .,
              scales = "free_y",
              remove_labels = "x",
              axes = "x")  +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(breaks = c(0, 10, 20),
                         limits = c(0, 20),
                         expand = expansion(mult = y_mult)),
      scale_y_continuous(breaks = c(0, 5, 10),
                         limits = c(0, 10),
                         expand = expansion(mult = y_mult)),
      scale_y_continuous(breaks = c(0, 10, 20),
                         limits = c(0, 20),
                         expand = expansion(mult = y_mult))
    )
  ) +

  theme(
    text = element_text(family = "Times New Roman", size = 14,
                        color = 'black'),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  gg_sced_style_x(x_mult, lwd = 2) +
  gg_sced_style_y(y_mult, lwd = 2)

staggered_pls = list(
  '1' = c(3.5,   3.5,   3.5),
  '2' = c(6.5,   6.5,   8.5),
  '3' = c(9.5,   9.5,  11.5),
  '4' = c(12.5,  16.5,  16.5),
  '5' = c(15.5,  22.5,  19.5)
)

offsets_pls = list(
  '1' = c(0, 0, 0),
  '2' = c(0, 0, 0),
  '3' = c(0, 0, 0),
  '4' = c(0, 0, 0),
  '5' = c(-1, 0, 0)
)

#svg(filename = 'figs/GilroyEtAl2021.svg',
#    width = 8,
#    height = 6)

gg_sced(p, legs = staggered_pls, offs = offsets_pls)

#dev.off()
