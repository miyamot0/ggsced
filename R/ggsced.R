
#' gg_sced_scale_units
#'
#' ...
#'
#' @param session_value Session associated with phase-change line
#' @param domain_size Total size of domain per panel specification
#'
#' @return A proportional value given value in a panel domain
#'
gg_sced_scale_units <- function(session_value, domain_size) {
  (session_value - domain_size[1]) / (domain_size[2] - domain_size[1])
}

#' gg_sced_style_y
#'
#' ...
#'
#' @param expansion Percentage of axis to pad (top/bottom) to break axis
#' @param lwd Width of axis line (clipping on)
#' @param col Color of axis drawn (default = 'black')
#'
#' @return Annotation to simulate a disconnected y-axis via expansion
#' @export
#'
gg_sced_style_y <- function(expansion = 0.00, lwd = 2, col = "black") {
  annotation_custom(
    grid::linesGrob(x = c(0, 0),
                    y = grid::unit(c(expansion, 1 - expansion), "npc"),
                    gp = grid::gpar(lwd = lwd, col = col))
  )
}

#' gg_sced_style_x
#'
#' ...
#'
#' @param expansion Percentage of axis to pad (top/bottom) to break axis
#' @param lwd Width of axis line (clipping on)
#' @param col Color of axis drawn (default = 'black')
#'
#' @return Annotation to simulate a disconnected y-axis via expansion
#' @export
#'
gg_sced_style_x <- function(expansion = 0.00, lwd = 2, col = "black") {
  annotation_custom(
    grid::linesGrob(x = grid::unit(c(expansion, 1 - expansion), "npc"),
                    y = c(0, 0),
                    gp = grid::gpar(lwd = lwd, col = col))
  )
}

#' gg_sced_get_panels
#'
#' ...
#'
#' @param ggplot_grobs ...
#'
#' @return ...
#'
gg_sced_get_panels <- function(ggplot_grobs) {
  # TODO: Assert types

  lcl_panels = ggplot_grobs$layout
  lcl_panels = lcl_panels[grepl("^panel", ggplot_grobs$layout$name),]

  return(lcl_panels)
}

#' gg_sced_name_dogleg
#'
#' ...
#'
#' @param panel ...
#' @param index ...
#' @param n_leg ...
#'
#' @return ...
#'
gg_sced_name_dogleg <- function(panel, index, n_leg) {
  # TODO: Assert types

  paste0(panel$name, "-phase.change-", index, '-leg-', n_leg)
}

#' gg_sced_name_dogleg_lateral
#'
#' ...
#'
#' @param panel ...
#' @param index ...
#' @param n_leg ...
#'
#' @return ...
#'
gg_sced_name_dogleg_lateral <- function(panel, index, n_leg) {
  # TODO: Assert types

  paste0(panel$name, "-phase.change-", index, "-leg.lateral-", n_leg)
}

#' gg_sced
#'
#' ...
#'
#' @param plt ...
#' @param legs ...
#' @param offs ...
#'
#' @return ...
#' @export
#'
gg_sced <- function(plt, legs,
                    offs = NULL,
                    verbose = TRUE) {
  if (is.null(plt)) stop('Error: Plot object undefined.')

  if (is.null(legs)) stop('Error: Phase change listings undefined.')

  # Actual GG object to reference
  lcl_ggplot_build <- ggplot_build(plt)

  # Grobs to be drawn on grid
  lcl_ggplot_grobs <- ggplotGrob(plt)

  # Grobs specific to data to be annotated
  lcl_panels <- gg_sced_get_panels(lcl_ggplot_grobs)

  print(lcl_panels)

  # Number of panels as per the drawn figure
  lcl_n_panels = nrow(lcl_panels)

  # Assert: Must be uniform length legs
  leg_lengths = unlist(lapply(legs, function(vec) {
    # Assert: All elements must be numeric
    if (is.numeric(vec) == FALSE) {
      stop('Error: Vectors in list are not numeric in type.')
    }

    length(vec)
  }), use.names = FALSE)

  if (length(unique(leg_lengths)) > 1) {
    stop('Error: Vectors in list are not of uniform length')
  }

  grid::grid.newpage()

  n_leg = 0

  for (pl in legs) {
    n_leg = n_leg + 1

    for (row in seq_len(lcl_n_panels)) {
      lcl_panel = lcl_panels[row,]

      has_more_rows = row < lcl_n_panels

      params = lcl_ggplot_build$layout$panel_params[[row]]
      x_range <- params$x.range

      x_lvl = pl[row]

      npc_x <- gg_sced_scale_units(x_lvl, x_range)

      message(paste("Draw", row, "of", lcl_n_panels, "panels, x = ", x_lvl))
      message(paste("npc_x = ", npc_x))

      dynamic_b = ifelse(has_more_rows == TRUE,
                         lcl_panels[row + 1, "t"] - 1,
                         lcl_panel$t)

      dynamic_offs = ifelse(is.null(offs) == FALSE,
                            offs[[n_leg]][row],
                            0)

      draw_short = ifelse(dynamic_offs == 0, FALSE, TRUE)

      if (draw_short == TRUE) {

        dynamic_b = dynamic_b - 2

        # Note: This is the full segment
        main_segment_name = gg_sced_name_dogleg(lcl_panel, row, n_leg)
        main_segment = grid::segmentsGrob(x0 = unit(npc_x, "npc"),
                                          x1 = unit(npc_x, "npc"),
                                          y0 = unit(1, "npc"),
                                          y1 = unit(0, "npc"),
                                          name = main_segment_name)

        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment,
                                                    t = lcl_panel$t,
                                                    l = lcl_panel$l,
                                                    #Note: this should connect to the upper portion
                                                    b = dynamic_b,
                                                    clip = 'off',
                                                    z = 1000,
                                                    name = main_segment_name)

        main_segment_pre = grid::segmentsGrob(x0 = unit(npc_x, "npc"),
                                              x1 = unit(npc_x, "npc"),
                                              y0 = unit(1, "npc"),
                                              y1 = unit(0.5, "npc"),
                                              name = paste(main_segment_name, 'pre'))

        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment_pre,
                                                    t = dynamic_b + 1,
                                                    l = lcl_panel$l,
                                                    clip = 'off',
                                                    z = 1000,
                                                    name = paste(main_segment_name, 'pre'))


        if (has_more_rows == TRUE) {
          main_segment_lateral_name = gg_sced_name_dogleg_lateral(lcl_panel, row, n_leg)

          npc_x2 <- gg_sced_scale_units(pl[row + 1], x_range)

          main_segment_post = grid::segmentsGrob(x0 = unit(npc_x2, "npc"),
                                                 x1 = unit(npc_x2, "npc"),
                                                 y0 = unit(0.5, "npc"),
                                                 y1 = unit(0, "npc"),
                                                 name = paste(main_segment_name, 'post'))

          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      main_segment_post,
                                                      t = dynamic_b + 1,
                                                      l = lcl_panel$l,
                                                      clip = 'off',
                                                      z = 1000,
                                                      name = paste(main_segment_name, 'post'))

          lateral_segment2 = grid::segmentsGrob(x0 = unit(npc_x, "npc"),
                                                x1 = unit(npc_x2, "npc"),
                                                y0 = unit(0.5, "npc"),
                                                y1 = unit(0.5, "npc"),
                                                name = paste0(main_segment_lateral_name, 'asdf'))

          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      lateral_segment2,
                                                      t = lcl_panels[row + 1,]$t - 2,
                                                      l = lcl_panels[row + 1,]$l,
                                                      clip = 'off',
                                                      z = 1000,
                                                      name = paste0(main_segment_lateral_name, 'asdf'))
        }

      } else {

        # Note: This is the full segment
        main_segment_name = gg_sced_name_dogleg(lcl_panel, row, n_leg)
        main_segment = grid::segmentsGrob(x0 = unit(npc_x, "npc"),
                                          x1 = unit(npc_x, "npc"),
                                          y0 = unit(1, "npc"),
                                          y1 = unit(0, "npc"),
                                          name = main_segment_name)

        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment,
                                                    t = lcl_panel$t,
                                                    l = lcl_panel$l,
                                                    b = dynamic_b,
                                                    clip = 'off',
                                                    z = 1000,
                                                    name = main_segment_name)

        if (has_more_rows == TRUE) {
          main_segment_lateral_name = gg_sced_name_dogleg_lateral(lcl_panel, row, n_leg)

          npc_x2 <- gg_sced_scale_units(pl[row + 1], x_range)

          lateral_segment = grid::segmentsGrob(x0 = unit(npc_x, "npc"),
                                               x1 = unit(npc_x2, "npc"),
                                               y0 = unit(1, "npc"),
                                               y1 = unit(1, "npc"),
                                               name = main_segment_lateral_name)

          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      lateral_segment,
                                                      t = lcl_panels[row + 1,]$t,
                                                      l = lcl_panels[row + 1,]$l,
                                                      clip = 'off',
                                                      z = 1000,
                                                      name = main_segment_lateral_name)

        }
      }
    }
  }

  grid::grid.draw(lcl_ggplot_grobs)

  invisible()
}
