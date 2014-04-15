shinyUI(pageWithSidebar(
    headerPanel("Isoglosses"),
        sidebarPanel(
            selectInput("fac.col", "Grouping factor: ", choices = c("")),
            selectInput("vala", "Column A: ", choices = c("")),
            sliderInput("both.cliques", "Discard regions smaller than: ",
                        min = 1, max = 9, value = 3, step = 1),
            sliderInput("one.clique", "Discard boundaries smaller than: ",
                        min = 1, max = 9, value = 5, step = 1),
            selectInput("tol", "Smooth isoglosses by: ",
                        choices = c(0, 0.1, 0.25, 0.5, 1, 2, 3, 5, 7.5, 10))
            ),
        mainPanel(
            tabsetPanel(
                tabPanel("Select data",
                         uiOutput("df.select"),
                         selectInput("lat.col", "Latitude: ", choices = c("")),
                         selectInput("lon.col", "Longitude: ", choices = c("")),
                         hr(),
                         selectInput("map.region", "Map region: ",
                                     choices = c(
                                         "US (states)", "World", "US (county)")),
                         textInput("country", "Country: ", "")#,
                         ## checkboxInput("riversp", "Include rivers?")
                         ),
                tabPanel("Graph", plotOutput("graph")),
                tabPanel("Download",
                         "Download this data file.  Then, from R, execute:",
                         br(),
                         uiOutput("dl.code"),
                         br(),
                         "If you are adding this isogloss to another map, the code to be added to the rest of the geoms is:",
                         br(),
                         uiOutput("dl.code.short"),
                         textInput("data.file.name", "Enter a file name:"),
                         downloadButton("data.file.dl", "Download"))
                ))))
