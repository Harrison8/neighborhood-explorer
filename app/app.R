library(dplyr)
library(tidyr)
library(magrittr)
library(dygraphs)
library(xts)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(scales)
library(leaflet)

ui <- fluidPage(

  useShinyjs(),

  theme = shinythemes::shinytheme("lumen"),

  tags$head(includeCSS("www/styles.css")),

  titlePanel("GLP Neighborhood Explorer"),

  sidebarLayout(

    sidebarPanel(

      width = 3,

      #h4("Legend"),
      #uiOutput("legend"),

      div(style="text-align: center;",
          radioGroupButtons("variable",
                            h4("Select a variable"),
                            direction = "vertical",
                            choices = c("bach_plus"))),

      #sliderInput("rolling_mean", "Rolling Mean", min = 1, max = 14, value = 1, step = 2),
      div(style="text-align: center;",
          radioGroupButtons("geography",
                            h4("Select a geography"),
                            direction = "vertical",
                            choices = c("Neighborhood"))),

      div(style="text-align: center;",
          sliderInput("year",
                      h4("Select a year"),
                      2000,
                      2016,
                      2016)),

    ),

    mainPanel(

      width = 9,

      fluidRow(

        column(width = 5,

        leafletOutput("map"),
        ),

        column(width = 6,
        dygraphOutput(outputId = "plot", height = "600px")
        )

      ),
      fluidRow(verbatimTextOutput("Click_text")),

    )
  )
)

server <- function(input, output) {

  # Return data frame based on geography
  this_data <- reactive({data_fxn(input)})

  # Return map object based on geography
  this_map <- reactive({map_fxn(input)})

  # Combine map with data
  final_map <- reactive({combine_map(this_data(), this_map(), input)})

  #Create basemap
  output$map <- renderLeaflet({

    leaflet(final_map()) %>%
      addTiles() %>%
      setView(lng = -85.7585, lat = 38.2527, zoom = 10)

  })

  #Update map based on changes
  observe({

    # import reactive map object
    map_obj <- final_map()

    # Organize object so most recently-clicked polygon is loaded last (on top)
    if(!is.null(input$map_shape_click$id)) {
      map_obj <-
        bind_rows(
          map_obj[map_obj$neighborhood != input$map_shape_click$id,],
          map_obj[map_obj$neighborhood == input$map_shape_click$id,]
        )
    }

    #Define palette based on data range
    color_vector <- RColorBrewer::brewer.pal(9, "BuPu")
    var_range <- range(map_obj[[input$variable]], na.rm = T)
    na_present <- any(is.na(map_obj[[input$variable]]))
    if(FALSE) var_range = c(var_range, NA_real_)

    pal <- leaflet::colorNumeric(
      palette = color_vector,
      domain = var_range)

    legend_title <- input$variable

    # Format legends using HTML
    labels <- sprintf("%s<br/>%s",
                      map_obj$line1,
                      map_obj$line3) %>%
      lapply(htmltools::HTML)

    if ("neighborhood" %in% names(map_obj)) {
      labels[[which(map_obj$neighborhood == "Airport")]] <-
        htmltools::HTML(sprintf("%s<br/>%s",
                                "Louisville International Airport",
                                "No residents"))}

    # Highlight clicked polygon using color and lineweight
    colors <- rep("#444444", 25)
    colors[map_obj$neighborhood == input$map_shape_click$id] = "#00a9b7"

    weights <- rep(1, 25)
    weights[map_obj$neighborhood == input$map_shape_click$id] = 3

    # Remove old legend, add legend, and add polygons
    map <- leafletProxy("map", data = map_obj) %>%
      clearControls() %>%
      addLegend(pal = pal, values = var_range, opacity = 0.7, title = legend_title, position = "bottomright") %>%
      addPolygons(
        color = colors, weight = weights, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~pal(var),
        label = labels,
        layerId = map_obj$neighborhood,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "font-family" = "Arial", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))

  })

  # Render dygraph
  output$plot <- renderDygraph({

    make_graph(this_data(), input)

  })

  observe({
    click<-input$map_shape_click
    if(is.null(click))
      return()
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    text2<-paste("You've selected point ", click$id)
    output$Click_text<-renderText({
      text2
    })
  })

}

shinyApp(ui = ui, server = server)

