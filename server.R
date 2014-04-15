library(ggplot2)
library(plyr)
library(maps)
library(mapdata)

shinyServer(function (input, output, session) {

    ## A box to select from data frames in the current environment
    output$df.select <- renderUI({
        objs <- ls(.GlobalEnv)
        choices <- c("")
        for (i in 1:length(objs)) {
            if ("data.frame" %in% class(get(objs[i], envir = .GlobalEnv))) {
                choices <- c(choices, objs[i])
            }
        }
        selectInput("df.name", "Data frame: ", choices = choices)
    })

    observe({
        if (!is.null(input$df.name) && input$df.name != "") {
            df <- get(input$df.name, .GlobalEnv)
            cols <- colnames(df)

            fac.cols <- character()
            for (i in 1:length(cols)) {
                if ("factor" %in% class(df[[cols[i]]]) |
                    "character" %in% class(df[[cols[i]]])) {
                    fac.cols <- c(fac.cols, cols[i])
                }
            }

            num.cols <- character()
            for (i in 1:length(cols)) {
                if ("numeric" %in% class(df[[cols[i]]])) {
                    num.cols <- c(num.cols, cols[i])
                }
            }

            lat.match <- grep("lat", num.cols, ignore.case = TRUE)
            if (!is.na(lat.match)) {
                lat.col <- cols[lat.match[1]]
            } else {
                lat.col <- NULL
            }

            lon.match <- grep("lon", num.cols, ignore.case = TRUE)
            if (!is.na(lon.match)) {
                lon.col <- cols[lon.match[1]]
            } else {
                lon.col <- NULL
            }

            updateSelectInput(session, "lat.col", choices = num.cols)
            updateSelectInput(session, "lon.col", choices = num.cols)
            updateSelectInput(session, "fac.col", choices = fac.cols,
                              selected = fac.cols[1])
        }
    })

    observe(tryCatch({
        df <- get(input$df.name, .GlobalEnv)
        updateSelectInput(session, "vala",
                          choices = levels(as.factor(df[[input$fac.col]])))
    }, error = function (e) {
        FALSE
    }))

    mapdata <- reactive({
        mapname <- c("US (states)" = "state",
                     "World" = "worldHires",
                     "US (county)" = "county")[input$map.region]
        mapdata <- map_data(mapname, ifelse(input$country == "", ".", input$country))
    })

    dfr <- reactive({
        get(input$df.name, .GlobalEnv)
    })

    loncolR <- reactive({
        d <- dfr()
        d[[input$lon.col]]
    })

    latcolR <- reactive({
        d <- dfr()
        d[[input$lat.col]]
    })

    faccolR <- reactive({
        d <- dfr()
        d[[input$fac.col]]
    })

    inet <- reactive({
        isonet <- make.isonet2(loncolR(), latcolR(), faccolR(), input$vala,
                               tol = as.numeric(input$tol),
                               bothCliques = input$both.cliques,
                               oneClique = input$one.clique,
                               map = mapdata())
        isonet
    })
    ## The graph
    output$graph <- renderPlot({
        if (input$lon.col == "" || input$lat.col == "" || input$fac.col == "") {
            stop("Columns not properly specified")
        }

        plot <- ggplot(aes_string(x = input$lon.col, y = input$lat.col),
                       data = dfr())

        plot <- plot + plot.isonet(inet(), map = mapdata())
        plot <- plot + geom_point(aes_string(color = input$fac.col))
        plot <- plot + coord_map()

        ## if (input$riversp) {
        ##     rivers.dat <- map_data("rivers")
        ##     plot <- plot + geom_polygon(aes(x = long, y = lat, group = group),
        ##                                 data = rivers.dat, color = "blue", fill = "blue")
        ## }

        print(plot)

    })

    output$dl.code <- renderUI({
        if (input$data.file.name == "") {
            code("Enter a filename please.")
        } else {
            code(paste0("load(\"", input$data.file.name, ".RData\")"),
                 br(),
                 paste0(
                     "ggplot(aes(x = ",
                     input$lon.col, ", y = ",
                     input$lat.col, "), data = ",
                     input$df.name, ") +"), br(),
                 "    theme_nothing(legend = TRUE) +", br(),
                 paste0("    geom_polygon(aes(x = long, y = lat, group = group), data = map_data(\"",
                        c("US (states)" = "state",
                          "World" = "worldHires",
                          "US (county)" = "county")[input$map.region],
                        "\"), color = \"grey50\", fill = NA) +"), br(),
                 paste0("    geom_path(aes(x = long, y = lat, group = group), data = ",
                        input$data.file.name,
                        ") +"), br(),
                 paste0("    geom_point(aes(color = ", input$fac.col, ")) +"), br(),
                 "    coord_map()")
        }
    })

    output$dl.code.short <- renderUI({
        if (input$data.file.name == "") {
            code("Enter a filename please.")
        } else {
            code(paste0("geom_path(aes(x = long, y = lat, group = group), data = ",
                        input$data.file.name,
                        ")"))
        }
    })

    output$data.file.dl <- downloadHandler(
        filename = function () paste0(input$data.file.name, ".Rdata"),
        content = function(file) {
            local({
                assign(input$data.file.name, inet())
                save(list = c(input$data.file.name), file = file)
            })
        })
})
