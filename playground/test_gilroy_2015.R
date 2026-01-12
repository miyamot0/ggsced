

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

participant_labels = data_set %>%
  select(Participant) %>%
  unique() %>%
  mutate(x = rep(27, 3),
         y = 0)

y_mult = .05
x_mult = .02

generate_facet_labels = function(plt, y = 0, x = NULL, hjust = 1, vjust = 0) {
  lcl_bld <- ggplot2::ggplot_build(p)
  lcl_plot_data = lcl_bld$plot$data

  max_x = max(lcl_bld$layout$panel_params[[1]]$x$breaks)
  min_y = y

  if (!is.null(x)) max_x = x

  facet_name = names(lcl_bld$layout$facet_params$rows)[1]
  unique_names = unique(lcl_plot_data[, facet_name])

  x_col = gsub("~", "", deparse(lcl_bld$plot$mapping[['x']]))
  y_col = gsub("~", "", deparse(lcl_bld$plot$mapping[['y']]))
  g_col = gsub("~", "", deparse(lcl_bld$plot$mapping[['group']]))

  tag_frm = list()
  tag_frm[[facet_name]] = unique_names
  tag_frm[['label']] = as.character(unique_names)
  tag_frm[[x_col]] = rep(max_x, length(unique_names))
  tag_frm[[y_col]] = rep(min_y, length(unique_names))
  tag_frm[[g_col]] = rep(1, length(unique_names))

  tag_frm_df = as.data.frame(tag_frm)

  p = p +
    geom_text(data = tag_frm_df,
              mapping = aes(label = label),
              hjust = hjust,
              vjust = vjust)

  invisible(p)
}

generate_condition_labels = function(plt, y = NULL, x = NULL, hjust = 0.5, vjust = 1) {
  lcl_bld <- ggplot2::ggplot_build(p)
  lcl_plot_data = lcl_bld$plot$data

  max_y = max(lcl_bld$layout$panel_params[[1]]$y$breaks)

  if (!is.null(y)) max_y = y

  if (!is.null(x)) max_x = x

  facet_name = names(lcl_bld$layout$facet_params$rows)[1]
  unique_names = unique(lcl_plot_data[, facet_name])

  x_col = gsub("~", "", deparse(lcl_bld$plot$mapping[['x']]))
  y_col = gsub("~", "", deparse(lcl_bld$plot$mapping[['y']]))
  g_col = gsub("~", "", deparse(lcl_bld$plot$mapping[['group']]))

  levels_of_facet = levels(lcl_bld$plot$data[, facet_name])
  levels_of_grp = levels(lcl_bld$plot$data[, g_col])

  lcl_data_internal = lcl_bld$data[[1]]
  lcl_data_internal = lcl_data_internal[lcl_data_internal$PANEL == 1, ]

  facet_name = names(lcl_bld$layout$facet_params$rows)[1]
  unique_names = unique(lcl_plot_data[, facet_name])

  splits_by_g <- by(lcl_data_internal, list(lcl_data_internal$group), function(df_g) {
    lcl_x_min = min(df_g$x)
    lcl_x_max = max(df_g$x)

    current_lvl = as.numeric(unique(df_g$group))
    x_set = mean(c(lcl_x_min, lcl_x_max))

    list(x = x_set,
         label = levels_of_grp[current_lvl],
         group = 1,
         y = max_y)
  })

  df_splits <- do.call(rbind, splits_by_g)
  df_splits_df = as.data.frame(df_splits)

  tag_frm = list()
  tag_frm[['label']] = as.character(df_splits_df$label)
  tag_frm[[x_col]] = as.numeric(df_splits_df$x)
  tag_frm[[y_col]] = as.numeric(df_splits_df$y)
  tag_frm[[g_col]] = rep(1, nrow(df_splits_df))
  tag_frm[[facet_name]] = rep(levels_of_facet[1], nrow(df_splits_df))

  tag_frm_df = as.data.frame(tag_frm)

  p = p +
    geom_text(data = tag_frm_df,
              mapping = aes(label = label),
              hjust = hjust,
              vjust = vjust)

  invisible(p)
}

p = ggplot(data_set, aes(Session, Responding,
                         group = Condition)) +
  geom_line() +
  geom_point(size = 3) +
  # geom_text(data = data_labels,
  #           mapping = aes(x, y,
  #                         label = Condition),
  #           hjust = 0.5,
  #           vjust = 0.275) +
  scale_y_continuous(name = "Percentage Accuracy",
                     limits = c(0, 100),
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
  ggsced_style_x(x_mult, lwd = 2) +
  ggsced_style_y(y_mult, lwd = 2)

get_phase_labels = function(plt) {
  lcl_bld <- ggplot2::ggplot_build(p)
  lcl_grobs <- ggplot2::ggplotGrob(p)
  lcl_data <- str(lcl_bld$plot$data)
  lcl_panels <- ggsced_get_panels(lcl_grobs)

  #naming_top_y <- max(frst_pnl$y.range)

  #frst_pnl = lcl_bld$layout$panel_params[[1]]

  #print(frst_pnl)

  lcl_data = lcl_bld$data[[1]]
  lcl_plot_data = lcl_bld$plot$data[[1]]

  print(lcl_data)

  groups = unique(lcl_data$group)

  print(groups)



  ## Need FIRST panel

  # first_pnl_data = lcl_data[lcl_data$PANEL == 1, ]
  #
  # print(first_pnl_data)

  # x_col = dplyr::as_label(tst_bld$plot$mapping$x)
  # y_col = dplyr::as_label(tst_bld$plot$mapping$y)
  # grouped_col = dplyr::as_label(tst_bld$plot$mapping$group)
  #
  # print(x_col)
  # print(y_col)
  # print(grouped_col)

  #lcl_levels = levels(lcl_bld$plot$data[, grouped_col])

  ## OLDD

  #print(tst_bld$plot)

  #print(lcl_data)

  # grouped_vars <- by(lcl_data, list(lcl_data$group), function(df) {
  #   min_x = min(df$x)
  #   max_x = max(df$x)
  #   grp_label = lcl_levels[unique(df$group)]
  #
  #   str(df$x)
  #
  #   temp_list = list()
  #   temp_list[[x_col]] = mean(c(min_x, max_x))
  #   temp_list[[y_col]] = top_y
  #   temp_list[[grouped_col]] = 1
  #   temp_list[['label']] = lcl_levels[unique(df$group)]
  #
  #   temp_list
  #
  #   # list(group = unique(df$group),
  #   #      y = top_y,
  #   #      label = grp_label,
  #   #      x = mean(c(min_x, max_x)))
  # })
  #
  # positions_grped <- do.call(rbind, grouped_vars)
  #
  # positions_grped
}

p = generate_facet_labels(p)

p = generate_condition_labels(p)

staggered_pls = list(
  '1' = c(4.5,  11.5, 18.5),
  '2' = c(13.5, 20.5, 23.5),
  '3' = c(23.5, 23.5, 23.5)
)

p = ggsced(p, legs = staggered_pls)
