library(alphahull)
library(ggplot2)
library(stringr)
library(deldir)
library(igraph)

shinyServer(function (input, output) {

    ## A box to select from data frames in the current environment
    output$df.select <- renderUI({
        objs <- ls(.GlobalEnv)
        choices <- character()
        for (i in 1:length(objs)) {
            if ("data.frame" %in% class(get(objs[i], envir = .GlobalEnv))) {
                choices <- c(choices, objs[i])
            }
        }
        selectInput("df.name", "Data frame: ", choices = choices)
    })

    ## Select columns of the data frame for latitude, longitude, and variable
    ## of interest
    output$cols.select <- renderUI({
        df <- get(input$df.name, .GlobalEnv)
        cols <- colnames(df)
        lat.match <- grep("lat", cols, ignore.case = TRUE)
        if (!is.na(lat.match)) {
            lat.col <- cols[lat.match[1]]
        } else {
            lat.col <- NULL
        }

        lon.match <- grep("lon", cols, ignore.case = TRUE)
        if (!is.na(lon.match)) {
            lon.col <- cols[lon.match[1]]
        } else {
            lon.col <- NULL
        }

        fac.cols <- character()

        for (i in 1:length(cols)) {
            if ("factor" %in% class(df[[cols[i]]]) |
                "character" %in% class(df[[cols[i]]])) {
                fac.cols <- c(fac.cols, cols[i])
            }
        }

        div(
            selectInput("lat.col", "Latitude: ", choices = cols, selected = lat.col),
            selectInput("lon.col", "Longitude: ", choices = cols, selected = lon.col),
            selectInput("fac.col", "Grouping factor: ", choices = fac.cols,
                        selected = fac.cols[1]),
            tryCatch(selectInput("vala", "Column A:",
                                 choices = levels(as.factor(df[[input$fac.col]]))),
                     error = function (e) "Invalid")
            )
    })

    ## The graph
    output$graph <- renderPlot({
        df <- get(input$df.name, .GlobalEnv)
        ## shape <- as.data.frame(ashape(data[[input$lon.col]],
        ##                               data[[input$lat.col]],
        ##                               input$alpha)$edges)
        ## for (i in 1:nrow(shape)) {
        ##     if (!any(shape[i,"x1"] - shape[-i,"x2"] < 1e-7 &
        ##              shape[i,"y1"] - shape[-i,"y2"] < 1e-7)) {
        ##         shape[i,"x1"] <- NA
        ##     }
        ## }
        ## shape <- subset(shape, !is.na(x1))

        d <- deldir(df[[input$lon.col]], df[[input$lat.col]])$delsgs
        edges <- NULL

        for (i in 1:nrow(d)) {
            a <- d[i,]$ind1
            b <- d[i,]$ind2
            if ((df[a,input$fac.col] == input$vala & df[b,input$fac.col] == input$vala)# |
                ## (df[a,input$fac.col] != input$vala & df[b,input$fac.col] == input$vala)
                ) {
                edges <- rbind(edges, ## data.frame(a = a, b = b)
                               d[i,]
                               )
            }
        }
        edges[[input$fac.col]] <- rep(input$vala, length.out = nrow(edges))

        ## print(edges)

        ## graph <- graph.edgelist(as.matrix(edges))
        ## cl <- clusters(graph)
        ## print(cl)
        ## hulls <- NULL
        ## for (i in 1:cl$no) {
        ##     if (cl$csize[i] == 1) {
        ##         next
        ##     }
        ##     points <- df[cl$membership == i, c(input$lon.col, input$lat.col)]
        ##     hull <- chull(points)
        ##     hull <- points[hull,]
        ##     colnames(hull) <- c("x", "y")
        ##     hull$g <- i
        ##     hulls <- rbind(hulls, hull)
        ## }

        ## print(ggplot(hulls, aes(x, y, group = g)) + geom_polygon())

        print(ggplot(df, aes_string(x = input$lon.col, y = input$lat.col)) +
            geom_point(aes_string(color = input$fac.col)) +
            geom_segment(aes(x1,y1,xend=x2,yend=y2),
                         data = edges))
            ## geom_polygon(aes(x = x, y = y, group = g), data = hulls,
            ##             color = "black", fill = NA))
    })
})
