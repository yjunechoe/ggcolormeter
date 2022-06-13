#' Colormeter legend coordinate system
#'
#' @description Due to the peculiar nature of the colormeter legend (e.g., dynamic size and positioning
#' of color bars/arcs depending on the number of breaks), many parts of the legend are drawn in a
#' mini isolated coordinate space. You can inspect this coordinate space with `debug = TRUE`.
#'
#' The coordinate space defaults to the x-y range of the legend "data", which considers the
#' polar-transformed positioning of the color arcs/bars, the labels, and the dashboard circle.
#'
#' @keywords internal
#' @name legend-coords
NULL

#' A color legend in the style of a dashboard meter
#'
#' @inheritParams ggplot2::guide_colorbar
#' @inheritParams ggplot2::guide_colorsteps
#' @param legend_size Size of the legend box.
#' @param legend_padding Spacing between the color meter and the legend boundary.
#' @param title_position <[`legend-coords`][legend-coords]> 2-length vector for the x/y-position of the legend title.
#' @param arc_range <[`legend-coords`][legend-coords]> 2-length vector for the start and end angles of the color meter.
#' @param arc_radius <[`legend-coords`][legend-coords]> Radius of the color meter.
#' @param arc_width <[`legend-coords`][legend-coords]> Width of the arcs in the color meter.
#' @param arc_gap <[`legend-coords`][legend-coords]> Gap between arcs in the color meter.
#' @param arc_rounding <[`legend-coords`][legend-coords]> Rounding of arcs in the color meter.
#' @param label_radius <[`legend-coords`][legend-coords]> Radius of the labels.
#' @param dashboard_radius <[`legend-coords`][legend-coords]> Radius of the dashboard background.
#' @param dashboard_color Dashboard background color.
#' @param dashboard_fill Dashboard background fill.
#' @param dashboard_linewidth Dashboard background line width.
#' @param dashboard_linetype Dashboard background line type.
#' @param clip_dashboard Whether the dashboard circle should clip to the legend boundary.
#' @param close_dashboard Whether the dashboard should be closed where it meets the legend boundary.
#' @param frame_color Color of the frame drawn around the arcs.
#' @param frame_linewidth Width of the frame drawn around the arcs.
#' @param frame_linetype Line type of the frame drawn around the arcs.
#' @param aspect.ratio Aspect ratio for the legend.
#' @param debug If `TRUE`, axes and origin for <[`legend-coords`][legend-coords]> are drawn over the legend for debugging.
#' @param ... Ignored.
#'
#' @export
#'
guide_colormeter <- function(title = ggplot2::waiver(), title.theme = NULL, label.theme = NULL,
                             legend_size = unit(5, "lines"), legend_padding = unit(c(1.2, 1, 0.3, 1), "lines"),
                             title_position = c(0, 0), arc_range = c(-4/7 * pi, 4/7 * pi),
                             arc_radius = 1, arc_width = arc_radius/4, arc_gap = arc_radius/5, arc_rounding = 0,
                             label_radius = arc_radius * 1.25, dashboard_radius = label_radius * 1.2,
                             dashboard_color = "black", dashboard_fill = NA,
                             dashboard_linewidth = 0.5, dashboard_linetype = 1,
                             clip_dashboard = TRUE, close_dashboard = clip_dashboard,
                             frame_color = NA, frame_linewidth = 0.5, frame_linetype = 1,
                             aspect.ratio = 1, show.limits = NULL, debug = FALSE,
                             reverse = FALSE, available_aes = c("colour", "color", "fill"),
                             ...) {
  guide <- ggplot2::guide_colorsteps(
    even.steps = TRUE, ticks = FALSE, show.limits = show.limits,
    title = title, title.theme = title.theme, label.theme = label.theme,
    frame.colour = frame_color, frame.linewidth = frame_linewidth, frame.linetype = frame_linetype,
    reverse = reverse, available_aes = available_aes
  )
  guide$legend_size <- legend_size
  guide$legend_padding <- legend_padding
  guide$title_position <- title_position
  guide$arc_range <- arc_range
  guide$arc_radius <- arc_radius
  guide$arc_width <- arc_width
  guide$arc_gap <- arc_gap
  guide$arc_rounding <- arc_rounding
  guide$label_radius <- label_radius
  guide$clip_dashboard <- clip_dashboard
  guide$close_dashboard <- close_dashboard
  guide$dashboard_radius <- dashboard_radius
  guide$dashboard_color <- dashboard_color
  guide$dashboard_fill <- dashboard_fill
  guide$dashboard_linewidth <- dashboard_linewidth
  guide$dashboard_linetype <- dashboard_linetype
  guide$aspect.ratio <- aspect.ratio
  guide$debug <- debug
  class(guide) <- c("colormeter", class(guide))
  guide
}

#' @importFrom ggplot2 guide_gengrob
#' @export
#' @method guide_gengrob colormeter
guide_gengrob.colormeter <- function(guide, theme) {

  n <- nrow(guide$bar)
  n_breaks <- n * 2 - 1

  arc_range <- guide$arc_range
  arc_gap <- guide$arc_gap

  bar_widths <- rep(pi/n_breaks, n_breaks)
  bar_widths[c(FALSE, TRUE)] <- bar_widths[1] * arc_gap
  bar_widths <- bar_widths * abs(diff(arc_range))/sum(bar_widths)
  bar_breaks <- (arc_range[1] + cumsum(c(0, bar_widths)))

  arcs <- data.frame(
    start = bar_breaks[-length(bar_breaks)],
    end = bar_breaks[-1],
    x0 = 0,
    y0 = 0,
    r0 = guide$arc_radius - guide$arc_width,
    r = guide$arc_radius,
    PANEL = 1,
    group = 1
  )

  arc_data <- arcPaths(arcs, 360)
  arc_data$group <- match(arc_data$group, unique(arc_data$group))
  arc_data <- arc_data[arc_data$group %% 2 == 1,]

  label <- guide$key$.label
  if (any(vapply(label, is.call, logical(1)))) {
    label <- lapply(label, function(l) {
      if (is.call(l))
        substitute(expression(x), list(x = l))
      else l
    })
    label <- do.call(c, label)
  }
  if (!isTRUE(guide$show.limits)) {
    label <- c("", label, "")
  }
  label_radius <- guide$label_radius
  if (!length(label_radius) %in% c(1, length(label))) {
    stop("Length of `label_radius` must be 1 or equal to the number of labels")
  }
  label_data <- radial_transform001(label_radius, seq(arc_range[1], arc_range[2], length.out = length(label)))
  label.theme <- guide$label.theme %||% ggplot2::calc_element("legend.text", theme)
  label_grob <- grid::textGrob(
    label = label,
    rot = label.theme$angle,
    gp = grid::gpar(
      fontsize = label.theme$size,
      fontfamily = label.theme$family,
      fontface = label.theme$face,
      col = label.theme$colour,
      lineheight = label.theme$lineheight
    ),
    x = label_data$x, y = label_data$y,
    hjust = label.theme$hjust %||% guide$label.hjust %||% 0.5,
    vjust = label.theme$vjust %||% guide$label.vjust %||% 0.5,
    default.units = "native",
    name = "guide.label"
  )

  # TODO: secondary labels at bar midpoints

  # label_grob <- ggplot2:::element_grob(
  #   element = label.theme, label = c("", label, ""),
  #   x = grid::unit(label_data$x, "native"),
  #   y = grid::unit(label_data$y, "native"),
  #   hjust = label.theme$title.hjust %||% 0.5,
  #   vjust = label.theme$title.vjust %||% 0.5,
  #   margin_x = FALSE, margin_y = TRUE
  # )

  arc_grob <- shapeGrob(
    arc_data$x, arc_data$y,
    default.units = 'native',
    id = arc_data$group,
    expand = 0,
    radius = guide$arc_rounding,
    gp = grid::gpar(
      col = guide$frame.colour,
      fill = guide$bar$colour,
      lwd = guide$frame.linewidth * ggplot2::.pt,
      lty = guide$frame.linetype
    )
  )

  legend_expansion <- c(0, 0, 0, 0)
  # TODO: wrap up legend_expansion is experimental
  # legend_expansion <- rep_len(guide$legend_expansion, 4)
  xrange <- range(label_data$x) * (legend_expansion[c(4, 2)] + 1) # legend_expansion[c(4, 2)]
  yrange <- range(label_data$y) * (legend_expansion[c(3, 1)] + 1) # legend_expansion[c(3, 1)]
  size <- guide$legend_size
  width <- size * abs(diff(xrange))/2 * sum(legend_expansion[c(2, 4)], 1)
  height <- size * abs(diff(yrange))/2 * guide$aspect.ratio * sum(legend_expansion[c(1, 3)], 1)
  width <- grid::convertUnit(width, "cm")
  height <- grid::convertUnit(height, "cm")

  legend_vp <- grid::viewport(xscale = xrange, yscale = yrange, width = width, height = height,
                              gp = grid::gpar(lineheight = label.theme$lineheight))

  colormeter_grob <- grid::grobTree(arc_grob, label_grob, vp = legend_vp)

  background_grob <- ggplot2::element_render(theme, "legend.background")

  title.theme <- guide$title.theme %||% ggplot2::calc_element("legend.title", theme)
  title.theme$hjust <- title.theme$hjust + 0.5
  title.hjust <- guide$title.hjust %||% title.theme$hjust
  title.vjust <- guide$title.vjust %||% title.theme$vjust %||% 0.5
  title_grob <- ggplot2::element_grob(title.theme, label = guide$title,
                                      hjust = title.hjust, vjust = title.vjust,
                                      margin_x = TRUE, margin_y = TRUE)
  title_grob$name <- "guide.title"
  title_grob$vp <- legend_vp
  title_grob$children[[1]]$x <- grid::unit(guide$title_position[1], "native")
  title_grob$children[[1]]$y <- grid::unit(guide$title_position[2], "native")


  legend_padding <- rep_len(guide$legend_padding, 4)
  if (!grid::is.unit(legend_padding)) {legend_padding <- grid::unit(legend_padding, "lines")}

  dashboard_data <- radial_transform001(guide$dashboard_radius, seq(0, 2 * pi, length.out = 360))
  dashboard_gp <- grid::gpar(col = guide$dashboard_color, fill = guide$dashboard_fill,
                             lwd = guide$dashboard_linewidth, lty = guide$dashboard_linetype)
  dashboard_grob <- grid::polygonGrob(
    x = grid::unit(dashboard_data$x, "native"),
    y = grid::unit(dashboard_data$y, "native") * (1 + (guide$aspect.ratio - 1)/2),
    gp = dashboard_gp, vp = legend_vp
  )

  gt <- gtable::gtable(widths = width, heights = height)
  gt <- gtable::gtable_add_padding(gt, padding = legend_padding)

  gt <- gtable::gtable_add_grob(gt, background_grob, name = "background",
                                clip = "off", t = 1, r = 3, b = 3, l = 1)
  gt <- gtable::gtable_add_grob(gt, dashboard_grob, name = "dashboard",
                                clip = guide$clip_dashboard, t = 1, r = 3, b = 3, l = 1)
  gt <- gtable::gtable_add_grob(gt, colormeter_grob, name = "colormeter",
                                clip = "off", t = 2, r = 2, b = 2, l = 2)
  gt <- gtable::gtable_add_grob(gt, title_grob,
                                name = "title", clip = "off", t = 2, r = 2, b = 2, l = 2)
  gt <- gtable::gtable_add_grob(gt, grid::editGrob(background_grob, gp = grid::gpar(fill = NA)), name = "outline",
                                clip = "off", t = 1, r = 3, b = 3, l = 1)

  if (guide$close_dashboard) {
    dashboard_border_gp <- dashboard_gp
    dashboard_border_gp$fill <- NA
    dashboard_border <- grid::rectGrob(
      width = grid::convertUnit(sum(gt$widths), "cm"),
      height = grid::convertUnit(sum(gt$heights), "cm"),
      gp = dashboard_border_gp,
      vp = grid::editViewport(legend_vp, mask = dashboard_grob)
    )
    gt <- gtable::gtable_add_grob(gt, dashboard_border, name = "dashboard_close",
                                  clip = "off", t = 1, r = 3, b = 3, l = 1)
  }

    if (guide$debug) {
    gt <- gtable::gtable_filter(gt, "background", invert = TRUE)
    gt$grobs[[which(gt$layout$name == "outline")]]$gp <- grid::gpar(lty = 5, fill = NA, col = "turquoise")
    xaxis <- grid::xaxisGrob(vp = legend_vp)
    yaxis <- grid::yaxisGrob(vp = legend_vp)
    origin <- grid::pointsGrob(0, 0, default.units = "native", vp = legend_vp)
    gt <- gtable::gtable_add_grob(gt, grid::grobTree(xaxis, yaxis, origin, gp = grid::gpar(col = "steelblue")),
                                  name = "axes", clip = "off", t = 2, r = 2, b = 2, l = 2)
  }

  gt

}
