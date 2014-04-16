missing.pkgs <- setdiff(c("rgeos","ggmap","mapdata","ggplot2","deldir","igraph",
                           "sp","plyr"),
                         rownames(installed.packages()))

if (length(missing.pkgs) > 0) {
    install.packages(missing.pkgs)
}

library(ggplot2)
library(mapdata)
library(rgeos)
library(deldir)
library(igraph)
library(sp)
library(plyr)
library(ggmap)

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
        if (!is.na(factor[a]) && !is.na(factor[b]) &&
            ((factor[a] == level && factor[b] == level) ||
             (factor[a] != level && factor[b] != level))) {
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
        if (!is.na(factor[a]) && !is.na(factor[b]) &&
            ((factor[a] == level && factor[b] != level) ||
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
            theme_nothing(legend = TRUE),
            geom_polygon(aes(x = long, y = lat, group = group),
                         data = map, color = "grey50", fill = NA),
            geom_path(aes(x = long, y = lat, group = group), data = edgelines))
    } else {
        plot <- list(
            geom_path(aes(x = long, y = lat, group = group), data = edgelines),
            theme_nothing(legend = TRUE))
    }

    plot
}
