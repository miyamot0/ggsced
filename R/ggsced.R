##
## Copyright 2026 Shawn Gilroy, Louisiana State University
##
## This file is part of ggsced.
##
## ggsced is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## ggsced is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with ggsced  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.

#' ggsced
#'
#' Core exported function to facilitate the drawing of phase change lines atop a ggplot object. Primarily designed to be used *after* the plot is finalized, with the lines being the last element drawn at the highest z index (i.e., atop all elements).
#'
#' @param plt ggplot object as typically designed/printed in userspace
#' @param legs list of 'legs' to be drawn
#' @param offs TODO mapping of lines that require minor offset
#' @param verbose Optional ability to view process output (debugging primarily)
#'
#' @return Finalized figure with respective phase change lines embedded.
#' @export
#' @importMethodsFrom assert assert
#' @importMethodsFrom ggplot2 ggplot_build
#' @importMethodsFrom ggplot2 ggplotGrob
#' @importMethodsFrom grid grid.newpage
#' @importMethodsFrom grid segmentsGrob
#' @importMethodsFrom gtable add_grob
#' @importMethodsFrom grid grid.draw
#'
ggsced <- function(plt, legs,
                   offs = NULL,
                   verbose = FALSE) {

  assert::assert(!is.null(plt),
                 ("ggplot" %in% class(plt)),
                 msg = "Plot object must be a valid ggplot object.")

  assert::assert(!is.null(legs),
                 is.list(legs),
                 msg = "Phase change points must be a valid ordered list.")

  # TODO: Testthis
  # Actual GG object to reference
  lcl_ggplot_build <- ggplot2::ggplot_build(plt)

  # TODO: Testthis
  # Grobs to be drawn on grid
  lcl_ggplot_grobs <- ggplot2::ggplotGrob(plt)

  # TODO: Testthis
  # Grobs specific to data to be annotated
  lcl_panels <- ggsced_get_panels(lcl_ggplot_grobs)

  # TODO: Testthis
  # Number of panels as per the drawn figure
  lcl_n_panels = nrow(lcl_panels)

  # Assert: Must be uniform length legs
  leg_lengths = unlist(lapply(legs, function(vec) {
    assert::assert(all(is.numeric(vec)),
                   msg = "Phase change points must be of numeric type.")

    length(vec)
  }), use.names = FALSE)

  assert::assert(length(unique(leg_lengths)) == 1,
                 msg = "Phase change vectors in list are not of a uniform length.")

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

      npc_x <- ggsced_scale_units(x_lvl, x_range)

      ggsced_output_console(paste("Draw", row, "of", lcl_n_panels,
                                   "panels, x = ", x_lvl),
                             verbose)

      ggsced_output_console(paste("npc_x = ", npc_x),
                             verbose)

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
        main_segment_name = ggsced_name_dogleg(lcl_panel, row, n_leg)
        main_segment = grid::segmentsGrob(x0 = grid::unit(npc_x, "npc"),
                                          x1 = grid::unit(npc_x, "npc"),
                                          y0 = grid::unit(1, "npc"),
                                          y1 = grid::unit(0, "npc"),
                                          name = main_segment_name)

        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment,
                                                    t = lcl_panel$t,
                                                    l = lcl_panel$l,
                                                    #Note: this should connect to the upper portion
                                                    b = dynamic_b,
                                                    #clip = 'off',
                                                    z = 1000,
                                                    name = main_segment_name)

        main_segment_pre = grid::segmentsGrob(x0 = grid::unit(npc_x, "npc"),
                                              x1 = grid::unit(npc_x, "npc"),
                                              y0 = grid::unit(1, "npc"),
                                              y1 = grid::unit(0.5, "npc"),
                                              name = paste(main_segment_name, 'pre'))

        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment_pre,
                                                    t = dynamic_b + 1,
                                                    l = lcl_panel$l,
                                                    #clip = 'off',
                                                    z = 1000,
                                                    name = paste(main_segment_name, 'pre'))


        if (has_more_rows == TRUE) {
          main_segment_lateral_name = ggsced_name_dogleg_lateral(lcl_panel, row, n_leg)

          npc_x2 <- ggsced_scale_units(pl[row + 1], x_range)

          main_segment_post = grid::segmentsGrob(x0 = grid::unit(npc_x2, "npc"),
                                                 x1 = grid::unit(npc_x2, "npc"),
                                                 y0 = grid::unit(0.5, "npc"),
                                                 y1 = grid::unit(0, "npc"),
                                                 name = paste(main_segment_name, 'post'))

          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      main_segment_post,
                                                      t = dynamic_b + 1,
                                                      l = lcl_panel$l,
                                                      #clip = 'off',
                                                      z = 1000,
                                                      name = paste(main_segment_name, 'post'))

          lateral_segment2 = grid::segmentsGrob(x0 = grid::unit(npc_x, "npc"),
                                                x1 = grid::unit(npc_x2, "npc"),
                                                y0 = grid::unit(0.5, "npc"),
                                                y1 = grid::unit(0.5, "npc"),
                                                name = paste0(main_segment_lateral_name, 'asdf'))

          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      lateral_segment2,
                                                      t = lcl_panels[row + 1,]$t - 2,
                                                      l = lcl_panels[row + 1,]$l,
                                                      #clip = 'off',
                                                      z = 1000,
                                                      name = paste0(main_segment_lateral_name, 'asdf'))
        }

      } else {

        # Note: This is the full segment
        main_segment_name = ggsced_name_dogleg(lcl_panel, row, n_leg)
        main_segment = grid::segmentsGrob(x0 = grid::unit(npc_x, "npc"),
                                          x1 = grid::unit(npc_x, "npc"),
                                          y0 = grid::unit(1, "npc"),
                                          y1 = grid::unit(0, "npc"),
                                          name = main_segment_name)

        lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                    main_segment,
                                                    t = lcl_panel$t,
                                                    l = lcl_panel$l,
                                                    b = dynamic_b,
                                                    #clip = 'off',
                                                    z = 1000,
                                                    name = main_segment_name)

        if (has_more_rows == TRUE) {
          main_segment_lateral_name = ggsced_name_dogleg_lateral(lcl_panel, row, n_leg)

          npc_x2 <- ggsced_scale_units(pl[row + 1], x_range)

          lateral_segment = grid::segmentsGrob(x0 = grid::unit(npc_x, "npc"),
                                               x1 = grid::unit(npc_x2, "npc"),
                                               y0 = grid::unit(1, "npc"),
                                               y1 = grid::unit(1, "npc"),
                                               name = main_segment_lateral_name)

          lcl_ggplot_grobs <- gtable::gtable_add_grob(lcl_ggplot_grobs,
                                                      lateral_segment,
                                                      t = lcl_panels[row + 1,]$t,
                                                      l = lcl_panels[row + 1,]$l,
                                                      #clip = 'off',
                                                      z = 1000,
                                                      name = main_segment_lateral_name)

        }
      }
    }
  }

  final_plot = grid::grid.draw(lcl_ggplot_grobs)

  invisible(final_plot)
}
