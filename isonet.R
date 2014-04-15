library(ggplot2)
library(mapdata)
library(rgeos)
library(deldir)
library(igraph)
library(sp)
library(plyr)
library(ggmap)
library(akima)

make.isonet <- function(lon, lat, factor, level, map = NULL) {
    lon <- lon + rnorm(length(lon), 0, 0.01)
    lat <- lat + rnorm(length(lat), 0, 0.01)

    d <- deldir(lon, lat)$delsgs
    edges <- NULL

    for (i in 1:nrow(d)) {
        a <- d[i,]$ind1
        b <- d[i,]$ind2
        if (factor[a] == level &
            factor[b] == level) {
            edges <- rbind(edges, d[i,])
        }
    }

    plot <- ggplot(data.frame(lon = lon, lat = lat, factor = factor),
                   aes(x = lon, y = lat))
    if (!is.null(map)) {
        plot <- plot + geom_polygon(aes(x = long, y = lat, group = group),
                                    data = map, color = "grey50", fill = NA)
    }

    plot <- plot +
        geom_point(aes(color = factor), size = 2, alpha = 0.7) +
        geom_segment(aes(x1, y1, xend = x2, yend = y2),
                     data = edges, alpha = 0.7)



    plot
}

make.isonet2 <- function(lon, lat, factor, level,
                         tol = 0.5, bothCliques = 2, oneClique = 3,
                         map = NULL) {
    ## TODO: not happy with the jittering here
    lon <- lon + rnorm(length(lon), 0, 0.01)
    lat <- lat + rnorm(length(lat), 0, 0.01)

    dd <- deldir(lon, lat)
    voronoi <- dd$dirsgs
    voronoi$clique <- NA
    delaunay <- dd$delsgs
    sel.edges <- NULL
    cliques <- NULL

    ## Find neighbors who are connected by edges in the Delaunay triangulation
    for (i in 1:nrow(delaunay)) {
        a <- delaunay[i,]$ind1
        b <- delaunay[i,]$ind2
        if ((factor[a] == level && factor[b] == level) ||
            (factor[a] != level && factor[b] != level)) {
            cliques <- rbind(cliques, data.frame(a = a, b = b))
        }
    }
    graph <- graph.edgelist(as.matrix(cliques))
    cl <- igraph::clusters(graph)

    ## Find Voronoi tesselation edges which differentiate between in- and
    ## out-group points, and save them to be drawn
    for (i in 1:nrow(voronoi)) {
        a <- voronoi[i,]$ind1
        b <- voronoi[i,]$ind2
        if (((factor[a] == level && factor[b] != level) ||
             (factor[a] != level && factor[b] == level)) &&
            ## TODO: make 2 and 3 customizable
            (isTRUE(cl$csize[cl$membership[a]] > oneClique) ||
             isTRUE(cl$csize[cl$membership[b]] > oneClique)) &&
            (isTRUE(cl$csize[cl$membership[a]] > bothCliques) &&
             isTRUE(cl$csize[cl$membership[b]] > bothCliques))) {
            sel.edges <- rbind(sel.edges, voronoi[i,])
        }
    }

    ## Get the map bounding region
    if (is.null(map)) {
        map <- map_data("worldHires")
    }
    bounding.polygons <- SpatialPolygons(dlply(map, .(group), function (x) {
        if (nrow(x) < 2) stop("aieee")
        Polygons(list(Polygon(x[,c("long","lat")])), as.character(x$group[1]))
    }))
    bounding <- gBuffer(gSimplify(bounding.polygons, 1))

    edgelines <- SpatialLines(apply(sel.edges, 1, function (x) {
            Lines(Line(matrix(c(x["x1"], x["y1"], x["x2"], x["y2"]),
                              nrow = 2, byrow = TRUE)),
                  ID = paste0(as.character(x["ind1"]), as.character(x["ind2"])))
        }))

    edgelines <- gIntersection(edgelines, bounding)
    edgelines <- gLineMerge(edgelines)
    edgelines <- gSimplify(edgelines, tol, TRUE)


    ## edgelines2 <- Reduce(rbind, lapply(edgelines@lines[[1]]@Lines, function (x) {
    ##     coords <- x@coords
    ##     spl <- spline.poly(coords, 1000)
    ##     data.frame(long = spl[,1], lat = spl[,2], group = floor(runif(1, 0, 1e9)))
    ## }), NULL)

    edgelines <- fortify(SpatialLinesDataFrame(edgelines, data = data.frame(foo = 1)))

    return (edgelines)
}

plot.isonet <- function (edgelines, map = NULL) {
    if (!is.null(map)) {
        plot <- list(
            geom_path(aes(x = long, y = lat, group = group), data = edgelines),
            theme_nothing(legend = TRUE),
            geom_polygon(aes(x = long, y = lat, group = group),
                         data = map, color = "grey50", fill = NA))
    } else {
        plot <- list(
            geom_path(aes(x = long, y = lat, group = group), data = edgelines),
            theme_nothing(legend = TRUE))
    }

    plot
}

## failed experiment, see stat below instead
geom_isogloss <- function (mapping = NULL, data = NULL, stat = "identity",
                           position = "identity",
                           factor, level,
                           tol = 0.5, bothCliques = 2, oneClique = 3,
                           map = NULL, color = "black", linetype = 1, size = 1,
                           alpha = 1,
                           ...) {
    rg <- geom_path(mapping = mapping, data = data, stat = stat,
                    position = position, ...)

    rg$geom <- proto(rg$geom, {
        draw <- function(., data, ...) {
            lines <- make.isonet2(data$x, data$y, data$factor, level = .$lvl,
                                  tol = .$tol, bothCliques = .$bC,
                                  oneClique = .$oC, map = .$map)
            draw.data <- data.frame(
                x = lines$long,
                y = lines$lat,
                group = lines$group,
                colour = .$color,
                linetype = .$linetype,
                size = .$size)
            .super$draw(., draw.data, ...)
        }
    })
    ## We might like to just use a closure here to access these vars inside
    ## the scope of draw.  But proto screws with the scope chain in ways that
    ## make that impossible
    rg$geom$lvl <- level
    ## TODO: this copying should be automated via "..."
    rg$geom$oC <- oneClique
    rg$geom$bC <- bothCliques
    rg$geom$tol <- tol
    rg$geom$map <- map
    ## TODO: default_aes...
    rg$geom$size <- size
    rg$geom$linetype <- linetype
    rg$geom$color <- color
    rg$geom$alpha <- alpha

    rg
}

stat_isogloss <- function (mapping = NULL, data = NULL,
                           geom = "path", position = "identity",
                           factor = NULL, level,
                           tol = 0.5, bothCliques = 2, oneClique = 3,
                           map = NULL,
                           ...) {
  StatIsogloss$new(mapping = mapping, data = data, geom = geom,
                   position = position, factor = factor, level = level,
                   tol = tol, bothCliques = bothCliques, oneClique = oneClique,
                   map = map,
                   ...)
}

StatIsogloss <- proto(ggplot2:::Stat, {
  objname <- "isogloss"

  default_geom <- function(.) GeomPath2
  ## TODO: for some reason, group does not work...
  default_aes <- function(.) aes(x = ..x.., y = ..y.., group = ..group..)

  calculate_groups <- function(., data, scales, factor, level, tol,
                               bothCliques, oneClique, map) {

      iso <- make.isonet2(data$x, data$y, data$factor, level = level,
                          tol = tol, bothCliques = bothCliques,
                          oneClique = oneClique, map = map)

      colnames(iso) <- revalue(colnames(iso), c("long" = "x", "lat" = "y"))
      iso$group <- as.factor(iso$group)
      print(iso$group)
      iso
  }
})

## For debugging...
GeomPath2 <- proto(ggplot2:::Geom, {
  objname <- "path"

  draw_groups <- function(., data, ...) {
      print(data)
      ## .$draw(data, ...)
      do.call(grid::gList, dlply(data, .(group), function (d) .$draw(d, ...)))
  }

  draw <- function(., data, scales, coordinates, arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, ..., na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      message("geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?")
    }

    keep <- function(x) {
      # from first non-missing to last non-missing
      first <- match(FALSE, x, nomatch = 1) - 1
      last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
      c(
        rep(FALSE, first),
        rep(TRUE, last - first),
        rep(FALSE, length(x) - last))
    }
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    missing <- !complete.cases(data[c("x", "y", "size", "colour",
      "linetype")])
    kept <- ave(missing, data$group, FUN=keep)
    data <- data[kept, ]
    # must be sorted on group
    data <- arrange(data, group)

    if (!all(kept) && !na.rm) {
      warning("Removed ", sum(!kept), " rows containing missing values",
        " (geom_path).", call. = FALSE)
    }

    munched <- coord_munch(coordinates, data, scales)

    # Silently drop lines with less than two points, preserving order
    rows <- ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- ddply(munched, .(group), function(df) {
      data.frame(
        solid = identical(unique(df$linetype), 1),
        constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
      )
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      stop("geom_path: If you are using dotted or dashed lines",
        ", colour, size and linetype must be constant over the line",
        call.=FALSE)
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    print(group_diff)
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)
    print(constant)

    if (!constant) {
      with(munched,
        segmentsGrob(
          x[!end], y[!end], x[!start], y[!start],
          default.units="native", arrow = arrow,
          gp = gpar(
            col = alpha(colour, alpha)[!end], fill = alpha(colour, alpha)[!end],
            lwd = size[!end] * .pt, lty = linetype[!end],
            lineend = lineend, linejoin = linejoin, linemitre = linemitre
          )
        )
      )
    } else {
      id <- match(munched$group, unique(munched$group))
      with(munched,
        polylineGrob(
          x, y, id = id,
          default.units = "native", arrow = arrow,
          gp = gpar(
            col = alpha(colour, alpha)[start], fill = alpha(colour, alpha)[start],
            lwd = size[start] * .pt, lty = linetype[start],
            lineend = lineend, linejoin = linejoin, linemitre = linemitre)
        )
      )
    }
  }

  draw_legend <- function(., data, ...) {
    data$arrow <- NULL
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
      ggname(.$my_name(), segmentsGrob(0.1, 0.5, 0.9, 0.5, default.units="npc",
      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt,
        lty=linetype, lineend="butt")))
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA)
  guide_geom <- function(.) "path"

})

## from: http://gis.stackexchange.com/a/24929
spline.poly <- function(xy, vertices, k=3, ...) {
    # Assert: xy is an n by 2 matrix with n >= k.

    # Wrap k vertices around each end.
    n <- dim(xy)[1]
    if (k >= 1) {
        data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
    } else {
        data <- xy
    }

    # Spline the x and y coordinates.
    data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
    x <- data.spline$x
    x1 <- data.spline$y
    x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y

    # Retain only the middle part.
    cbind(x1, x2)[k < x & x <= n+k, ]
}

## with(mergers.geo2, make.isonet(lon, lat, cot.caught, "no", md)) +
## scale_color_discrete("Has merger?") + ggtitle("cot~caught") + coord_map() +
## theme_nothing(legend = TRUE)


make.graphs <- function () {
pdf("cot-caught.pdf")

print(with(mergers.geo2, make.isonet2(lon, lat, cot.caught, "no", map = md)) + scale_color_discrete("Has merger?") + ggtitle("cot~caught") + coord_map() + theme_nothing(legend = TRUE))

dev.off()

pdf("don-dawn.pdf")
print(with(mergers.geo2, make.isonet2(lon, lat, don.dawn, "no", map = md)) + scale_color_discrete("Has merger?") + ggtitle("Don~Dawn") + coord_map() + theme_nothing(legend = TRUE))
dev.off()

pdf("pin-pen.pdf")
print(with(mergers.geo2, make.isonet2(lon, lat, pin.pen, "yes", map = md)) + scale_color_discrete("Has merger?") + ggtitle("pin~pen") + coord_map() + theme_nothing(legend = TRUE))
dev.off()

pdf("whine-wine.pdf")
print(with(mergers.geo2, make.isonet2(lon, lat, whine.wine, "no", map = md)) + scale_color_discrete("Has merger?") + ggtitle("whine~wine") + coord_map() + theme_nothing(legend = TRUE))
dev.off()
}
