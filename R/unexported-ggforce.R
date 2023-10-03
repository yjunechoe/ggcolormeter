# nocov start
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

#' @importFrom grid is.unit grob gpar polygonGrob unit
shapeGrob <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL,
                      id.lengths = NULL, expand = 0, radius = 0,
                      default.units = 'npc', name = NULL, gp = gpar(),
                      vp = NULL) {
  if (as.numeric(expand) == 0 && as.numeric(radius) == 0) {
    grob <- polygonGrob(
      x = x, y = y, id = id, id.lengths = id.lengths,
      default.units = default.units, name = name, gp = gp, vp = vp
    )
    return(grob)
  }
  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }
  if (!is.unit(expand)) {
    expand <- unit(expand, default.units)
  }
  if (!is.unit(radius)) {
    radius <- unit(radius, default.units)
  }
  if (as.numeric(radius) < 0) {
    stop('radius must be positive', call. = FALSE)
  }
  if (is.null(id)) {
    if (is.null(id.lengths)) {
      id <- rep(1, length(x))
    } else {
      id <- rep(seq_along(id.lengths), id.lengths)
      if (length(id) != length(x)) {
        stop('id.lengths must sum up to the number of points', call. = FALSE)
      }
    }
  }
  x <- x[order(id)]
  y <- y[order(id)]
  grob(
    x = x, y = y, id = id, expand = expand, radius = radius, name = name,
    gp = gp, vp = vp, cl = 'shape'
  )
}

#' @importFrom grid makeContent convertX convertY convertWidth gpar polygonGrob nullGrob unit
#' @importFrom polyclip polyoffset polylineoffset
#' @keywords internal
#' @export
#' @method makeContent shape
makeContent.shape <- function(x) {
  id.length <- lengths(split(seq_along(x$id), x$id))
  type <- ifelse(id.length == 1, 'point',
                 ifelse(id.length == 2, 'line', 'polygon'))
  x_new <- convertX(x$x, 'mm', TRUE)
  x_new <- split(x_new, x$id)
  y_new <- convertY(x$y, 'mm', TRUE)
  y_new <- split(y_new, x$id)
  polygons <- Map(list, x = x_new, y = y_new)
  poly <- split(polygons, type)
  expand <- convertWidth(x$expand, 'mm', TRUE)
  radius <- convertWidth(x$radius, 'mm', TRUE)
  expand <- expand - radius
  if (expand != 0) {
    if (!is.null(poly$polygon)) {
      poly$polygon <- lapply(poly$polygon, polyoffset, delta = expand,
                             jointype = 'miter', miterlim = 1000)
    }
    if (expand > 0) {
      if (!is.null(poly$line)) {
        poly$line <- lapply(poly$line, polylineoffset, delta = expand,
                            jointype = 'square', endtype = 'opensquare')
      }
      poly$point <- pointoffset(poly$point, expand, type = 'square')
    }
  }
  if (radius != 0) {
    if (!is.null(poly$polygon)) {
      not_empty <- lengths(poly$polygon) != 0
      poly$polygon[not_empty] <- lapply(poly$polygon[not_empty], polyoffset,
                                        delta = radius, jointype = 'round')
    }
    if (expand > 0) {
      if (!is.null(poly$line)) {
        not_empty <- lengths(poly$line) != 0
        poly$line[not_empty] <- lapply(poly$line[not_empty], polyoffset,
                                       delta = radius, jointype = 'round')
      }
      if (!is.null(poly$point)) {
        not_empty <- lengths(poly$point) != 0
        poly$point[not_empty] <- lapply(poly$point[not_empty], polyoffset,
                                        delta = radius, jointype = 'round')
      }
    } else {
      if (!is.null(poly$line)) {
        poly$line <- lapply(poly$line, polylineoffset, delta = radius,
                            jointype = 'round', endtype = 'openround')
      }
      poly$point <- pointoffset(poly$point, radius, type = 'circle')
    }
  }
  polygons[type == 'polygon'] <- lapply(poly$polygon, function(d) if (length(d) == 0) list() else d[[1]])
  polygons[type == 'line'] <- lapply(poly$line, function(d) if (length(d) == 0) list() else d[[1]])
  polygons[type == 'point'] <- lapply(poly$point, function(d) if (length(d) == 0) list() else d[[1]])
  x$id <- rep(seq_along(polygons), sapply(polygons, function(p) length(p$x)))
  x_new <- unlist(lapply(polygons, `[[`, 'x'))
  y_new <- unlist(lapply(polygons, `[[`, 'y'))
  if (length(x_new) == 0) return(nullGrob())
  x$x <- unit(x_new, 'mm')
  x$y <- unit(y_new, 'mm')
  x$cl <- 'polygon'
  class(x)[1] <- 'polygon'
  x
}

pointoffset <- function(A, delta, type) {
  if (length(A) == 0) return(A)
  switch(
    type,
    square = {
      square <- list(x = c(-delta, -delta, delta, delta),
                     y = c(-delta, delta, delta, -delta))
      x <- split(rep(sapply(A, `[[`, 'x'), each = 4) + square$x,
                 rep(seq_along(A), each = 4))
      y <- split(rep(sapply(A, `[[`, 'y'), each = 4) + square$y,
                 rep(seq_along(A), each = 4))
      lapply(Map(list, x = x, y = y), list)
    },
    circle = {
      detail <- 100
      radi <- seq(0, 2 * pi, length.out = detail + 1)[-(detail + 1)]
      circle <- list(x = cos(radi) * delta, y = sin(radi) * delta)
      x <- split(rep(sapply(A, `[[`, 'x'), each = detail) + circle$x,
                 rep(seq_along(A), each = detail))
      y <- split(rep(sapply(A, `[[`, 'y'), each = detail) + circle$y,
                 rep(seq_along(A), each = detail))
      lapply(Map(list, x = x, y = y), list)
    }
  )
}

radial_transform001 <- function(r, a) {data.frame(x = r * sin(a), y = r * cos(a))}

# Modified
arcPaths <- function(data, n) {
  data <- data[data$start != data$end, ]
  data$nControl <- ceiling(n / (2 * pi) * abs(data$end - data$start))
  data$nControl[data$nControl < 3] <- 3
  extraData <- !names(data) %in% c('r0', 'r', 'start', 'end', 'group')
  data$group <- make.unique(as.character(data$group))
  paths <- lapply(seq_len(nrow(data)), function(i) {
    path <- data.frame(
      a = seq(data$start[i], data$end[i], length.out = data$nControl[i]),
      r = data$r[i]
    )
    if ('r0' %in% names(data)) {
      if (data$r0[i] != 0) {
        path <- rbind(
          path,
          data.frame(a = rev(path$a), r = data$r0[i])
        )
      } else {
        path <- rbind(
          path,
          data.frame(a = data$start[i], r = 0)
        )
      }
    }
    path$group <- data$group[i]
    path$index <- seq(0, 1, length.out = nrow(path))
    path <- cbind(path, data[rep(i, nrow(path)), extraData, drop = FALSE])
  })
  paths <- do.call(rbind, paths)
  paths <- cbind(
    paths[, !names(paths) %in% c('r', 'a')],
    radial_transform001(paths$r, paths$a)
  )
  paths$x <- paths$x + paths$x0
  paths$y <- paths$y + paths$y0
  if ('explode' %in% names(data)) {
    exploded <- data$explode != 0
    if (any(exploded)) {
      exploder <- radial_transform001(
        data$explode[exploded],
        data$start[exploded] + (data$end[exploded] - data$start[exploded]) / 2
      )
      explodedPaths <- paths$group %in% which(exploded)
      exploderInd <- as.integer(factor(paths$group[explodedPaths]))
      paths$x[explodedPaths] <-
        paths$x[explodedPaths] + exploder$x[exploderInd]
      paths$y[explodedPaths] <-
        paths$y[explodedPaths] + exploder$y[exploderInd]
    }
  }
  paths[, !names(paths) %in% c('x0', 'y0', 'exploded')]
}
# nocov end
