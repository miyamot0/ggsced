rm(list = ls())

library(ggsced)
library(tidyverse)
library(ggh4x)

data <- Gilroyetal2021

y_mult = .05
x_mult = .02

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

  # geom_text(data = data_labels,
  #           mapping = aes(x, y,
  #                         label = Label),
  #           inherit.aes = FALSE) +

  # geom_segment(data = series_labels,
  #              aes(x = x0, y, xend = x1, yend = y),
  #              arrow = arrow(length = unit(0.25, "cm"))) +


  #
  # geom_text(data = participant_labels,
  #           mapping = aes(x, y,
  #                         label = Participant),
  #           inherit.aes = FALSE,
  #           hjust = 1,
  #           vjust = 0) +

  scale_x_continuous(breaks = c(1:25),
                     limits = c(1, 25),
                     expand = expansion(mult = x_mult)) +
  facet_grid2(Participant ~ .,
              scales = "free_y",
              remove_labels = "x",
              axes = "x")  +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(name = "Frequency",
                         breaks = c(0, 10, 20),
                         limits = c(0, 20),
                         expand = expansion(mult = y_mult)),
      scale_y_continuous(name = "Frequency",
                         breaks = c(0, 5, 10),
                         limits = c(0, 10),
                         expand = expansion(mult = y_mult)),
      scale_y_continuous(name = "Frequency",
                         breaks = c(0, 10, 20),
                         limits = c(0, 20),
                         expand = expansion(mult = y_mult))
    )
  ) +

  theme(
    text = element_text(size = 14,
                        color = 'black'),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  ) +
  ggsced_style_x(x_mult, lwd = 2) +
  ggsced_style_y(y_mult, lwd = 2)

simple_facet_labels_df = ggsced_facet_labels(p, y = 20)
simple_facet_labels_df[2, "Responding"] <- 10
simple_facet_labels_df[3, "Responding"] <- 8

p <- p + geom_text(data = simple_facet_labels_df,
                   hjust = 1,
                   vjust = 0.5,
                   mapping = aes(label = label))

simple_condition_labels_df = ggsced_condition_labels(p)
simple_condition_labels_df$label = gsub("2", "", simple_condition_labels_df$label)

p <- p + geom_text(data = simple_condition_labels_df,
                   mapping = aes(label = label),
                   hjust = 0.5,
                   vjust = 0.5)

staggered_pls = list(
  '1' = c(3.5,   3.5,   3.5),
  '2' = c(6.5,   6.5,   8.5),
  '3' = c(9.5,   9.5,  11.5),
  '4' = c(12.5,  16.5,  16.5),
  '5' = c(15.5,  22.5,  19.5)
)

offsets_pls = list(
  '1' = c(F, F, F),
  '2' = c(F, F, F),
  '3' = c(F, F, F),
  '4' = c(F, F, F),
  '5' = c(T, F, F)
)

ggsced(p, legs = staggered_pls, offs = offsets_pls)
