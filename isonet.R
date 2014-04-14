library(ggplot2)
library(mapdata)
library(rgeos)
library(deldir)
library(igraph)
library(sp)
library(plyr)
library(ggmap)

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

make.isonet2 <- function(lon, lat, factor, level, tol = 0.5, map = NULL) {
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
            (isTRUE(cl$csize[cl$membership[a]] > 3) ||
             isTRUE(cl$csize[cl$membership[b]] > 3)) &&
            (isTRUE(cl$csize[cl$membership[a]] > 2) &&
             isTRUE(cl$csize[cl$membership[b]] > 2))) {
            sel.edges <- rbind(sel.edges, voronoi[i,])
        }
    }

    ## Get the map bounding region
    if (is.null(map)) {
        map <- map_data("world")
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

    ## edgelines <- SpatialLines(dlply(sel.edges, .(clique), function (y) {
    ##     l <- apply(y, 1, function (x) {
    ##         Line(matrix(c(x["x1"], x["y1"], x["x2"], x["y2"]),
    ##                     nrow = 2, byrow = TRUE))
    ##     })
    ##     Lines(l, ID = paste0(as.character(x["ind1"]), as.character(x["ind2"])))
    ## }))

    edgelines <- gIntersection(edgelines, bounding)
    el2 <<- edgelines
    edgelines <- gSimplify(edgelines, tol)

    edgelines <- ldply(edgelines@lines[[1]]@Lines, function (x) {
        data.frame(x1 = x@coords[1,1],
                   y1 = x@coords[1,2],
                   x2 = x@coords[2,1],
                   y2 = x@coords[2,2])
    })

    plot <- ggplot(data.frame(lon = lon, lat = lat, factor = factor),
                   aes(x = lon, y = lat))
    if (!is.null(map)) {
        plot <- plot + geom_polygon(aes(x = long, y = lat, group = group),
                                    data = map, color = "grey50", fill = NA)
    }

    plot <- plot +
        geom_point(aes(color = factor), size = 2, alpha = 0.7) +
        geom_segment(aes(x1, y1, xend = x2, yend = y2),
                     data = edgelines, alpha = 0.7)



    plot
}

## with(mergers.geo2, make.isonet(lon, lat, cot.caught, "no", md)) +
## scale_color_discrete("Has merger?") + ggtitle("cot~caught") + coord_map() +
## theme_nothing(legend = TRUE)


make.graphs <- function () {
pdf("cot-caught.pdf")

print(with(mergers.geo2, make.isonet2(lon, lat, cot.caught, "no", md)) + scale_color_discrete("Has merger?") + ggtitle("cot~caught") + coord_map() + theme_nothing(legend = TRUE))

dev.off()

pdf("don-dawn.pdf")
print(with(mergers.geo2, make.isonet(lon, lat, don.dawn, "no", md)) + scale_color_discrete("Has merger?") + ggtitle("Don~Dawn") + coord_map() + theme_nothing(legend = TRUE))
dev.off()

pdf("pin-pen.pdf")
print(with(mergers.geo2, make.isonet(lon, lat, pin.pen, "yes", md)) + scale_color_discrete("Has merger?") + ggtitle("pin~pen") + coord_map() + theme_nothing(legend = TRUE))
dev.off()

pdf("whine-wine.pdf")
print(with(mergers.geo2, make.isonet(lon, lat, whine.wine, "no", md)) + scale_color_discrete("Has merger?") + ggtitle("whine~wine") + coord_map() + theme_nothing(legend = TRUE))
dev.off()
}

world <- map_data("worldHires")

wpolys <- dlply(world, .(group), function (x) {
    if (nrow(x) < 2) stop("aieee")
    Polygons(list(Polygon(x[,c("long","lat")])), as.character(x$group[1]))
})

wpolys <- SpatialPolygons(wpolys)

## TODO: union, simplify

wpolys <- gBuffer(wpolys)
