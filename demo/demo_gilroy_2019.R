library(ggsced)
library(tidyverse)
library(ggh4x)

current_data <- Gilroyetal2019Tx
current_data$Condition <- paste0(current_data$Condition, current_data$PhaseNum)
current_data$Function <- current_data$Participant

y_mult = .05
x_mult = .02

phase_annos = data.frame(
  Function = c("Attention", "Attention",
               "Demand", "Demand", "Demand"),
  PL = c(25.5, 26.5,
         34.5, 37.5, 41.5),
  lty = c(1, 1,
          1, 1, 1)
)

p = ggplot(current_data, aes(Session, CTB,
                         group = Condition)) +
  geom_line() +
  geom_point(pch = 21, fill = 'white', size = 2) +

  geom_line(aes(Session, FCR), lty = 2) +
  geom_point(aes(Session, FCR), pch = 21, fill = 'black', size = 2) +

  geom_point(aes(Session, FCR2), pch = 23, fill = 'black', size = 2) +
  geom_line(aes(Session, FCR2), lty = 2) +

  geom_vline(data = phase_annos,
             mapping = aes(xintercept = PL),
             lty = 1,
             lwd = 0.33) +

  scale_y_continuous(name = "Percentage Accuracy",
                     breaks = 0:3,
                     limits = c(0, 3),
                     expand = expansion(mult = y_mult)) +
  scale_x_continuous(breaks = (0:10) * 10,
                     limits = c(0, 100),
                     expand = expansion(mult = x_mult)) +
  facet_grid2(Function ~ .,
              remove_labels = "x",
              axes = "x") +
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
  '1' = c(13.5, 20)
)

gg_sced(p, legs = staggered_pls)
