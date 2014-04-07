shinyUI(fluidPage(
    sidebarLayout(
        sidebarPanel(
            uiOutput("df.select"),
            uiOutput("cols.select"),
            sliderInput("alpha", "Smoothing parameter: ", 0.01, 0.99,
                        step = 0.01, value = 0.5)
            ),
        mainPanel(plotOutput("graph"))
        )))
