library(ggsced)
library(tidyverse)
library(ggh4x)

data_set = Gilroyetal2015

y_mult = .05
x_mult = .02

p = ggplot(data_set, aes(Session, Responding, group = Condition)) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(name = "Percentage Accuracy",
                     limits = c(0, 100),
                     breaks = (0:4) * 25,
                     expand = expansion(mult = c(y_mult))) +
  scale_x_continuous(breaks = c(1:27),
                     limits = c(1, 27),
                     expand = expansion(mult = c(x_mult))) +
  facet_grid2(Participant ~ .,
              remove_labels = "x",
              axes = "x") +
  theme(text = element_text(size = 14, color = 'black'),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  ggsced_style_x(x_mult, lwd = 2) +
  ggsced_style_y(y_mult, lwd = 2)

simple_facet_labels_df = ggsced_facet_labels(p)

p <- p + geom_text(data = simple_facet_labels_df,
                   mapping = aes(label = label),
                   hjust = 1,
                   vjust = 0)

simple_condition_labels_df = ggsced_condition_labels(p)
simple_condition_labels_df[2, "Session"] <- 25.5
simple_condition_labels_df[4, "Session"] <- 9

p <- p + geom_text(data = simple_condition_labels_df,
                   mapping = aes(label = label),
                   hjust = 0.5,
                   vjust = 0.25)

staggered_pls = list('1' = c(4.5,  11.5, 18.5),
                     '2' = c(13.5, 20.5, 23.5),
                     '3' = c(23.5, 23.5, 23.5))

# png(filename = 'figs/GilroyEtAl2015.png',
#     units = "in",
#     res = 600,
#     width = 8,
#     height = 6)

ggsced(p, legs = staggered_pls)

#dev.off()
