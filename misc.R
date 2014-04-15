## A failed experiment
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
