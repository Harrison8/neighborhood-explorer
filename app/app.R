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
library(sf)
library(stringr)

ui <- fluidPage(

  useShinyjs(),

  theme = shinythemes::shinytheme("lumen"),

  tags$head(includeCSS("www/styles.css")),

  #titlePanel("GLP Neighborhood Explorer"),

  fluidRow(
    wellPanel(
      h4("Use the options below to show data on the map and graph.
         Click on a Council District or Neighborhorhood to view data over time compared to the city average.", align = "center"),
      h4("All numbers are five-year averages. For example, data for 2017  uses survey responses collected from 2015 to 2019.
         All numbers are estimates, and the shaded area on the line graph shows a 90% confidence interval.
         This data comes from the Census Bureau's American Community Survey.", align = "center"),
      h4("This is version 1.0 of this tool. Please get in touch with me at harrison@greaterlouisvilleproject.org
         to talk about how this can be more useful and what other kinds of information we can provide!", align = "center"))

  ),

  fluidRow(

    column(3,
           offset=1,
           wellPanel(
           div(style="text-align: center;",
               selectInput("variable",
                           h4("Select a variable"),
                           choices = education_variables,
                           selected = "assoc_plus")))),

    column(3,
           wellPanel(
           div(style="text-align: center;",
               radioGroupButtons("geography",
                                 h4("Select a geography"),
                                 direction = "vertical",
                                 choices = c(
                                   "Neighborhood",
                                   #"Zip Code",
                                   "Metro Council District"),
                                 selected = "Metro Council District",
                                 status = "primary")))),

    column(3,
           wellPanel(
           div(style="text-align: center;",
               sliderInput("year",
                           h4("Select a year to display on the map"),
                           min = 2008,
                           max = 2017,
                           value = 2017,
                           sep = ""))))

    # column(2,
    #        wellPanel(
    #          div(style="text-align: center;",
    #              radioGroupButtons("hide_lines",
    #                                label = h4("Show all areas on the graph?"),
    #                                choices = c("Show all",
    #                                            "Hide Unselected"),
    #                                status = "primary"))))

  ),

  hr(),

  fluidRow(

    column(width = 6,

           leafletOutput("map", height = "400px"),
    ),

    column(width = 5,
           dygraphOutput(outputId = "plot", height = "400px")
    )

  )

)


server <- function(input, output) {

  # Return data frame based on geography
  this_data <- reactive({map_data_fxn(input)})

  this_data_city <- reactive({city_data_fxn(input)})

  # Return map object based on geography
  this_map <- reactive({map_fxn(input)})

  # Combine map with data
  final_map <- reactive({combine_map(this_data(), this_map(), input)})

  #Create basemap
  output$map <- renderLeaflet({

    leaflet(final_map()) %>%
      addTiles() %>%
      setView(lng = -85.63, lat = 38.20, zoom = 10)

  })

  #Update map based on changes
  observe({

    # import reactive map object
    map_obj <- final_map()

    geog <- if_else(input$geography == "Neighborhood", "neighborhood", "district")

    # Organize object so most recently-clicked polygon is loaded last (on top)
    if(!is.null(input$map_shape_click$id)) {

      map_obj <-
        bind_rows(
          map_obj[map_obj[[geog]] != input$map_shape_click$id,],
          map_obj[map_obj[[geog]] == input$map_shape_click$id,]
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
                                "No residents"))
    }

    # Highlight clicked polygon using color and lineweight
    colors <- rep("#444444", 25)
    colors[map_obj[[geog]] == input$map_shape_click$id] = "#00a9b7"

    weights <- rep(1, 25)
    weights[map_obj[[geog]] == input$map_shape_click$id] = 3

    # Remove old legend, add legend, and add polygons
    map <- leafletProxy("map", data = map_obj) %>%
      clearControls() %>%
      addLegend(pal = pal, values = var_range, opacity = 0.7,
                title = names(education_variables)[education_variables == input$variable],
                position = "bottomright") %>%
      addPolygons(
        color = colors, weight = weights, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~pal(var),
        label = labels,
        layerId = map_obj[[geog]],
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "font-family" = "Arial", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))

  })

  # Render dygraph
  output$plot <- renderDygraph({

    make_graph(this_data(), this_data_city(), input)

  })

  # observe({
  #   click<-input$map_shape_click
  #   if(is.null(click))
  #     return()
  #   text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
  #   text2<-paste("You've selected point ", click$id)
  #   output$Click_text<-renderText({
  #     text2
  #   })
  # })

}

shinyApp(ui = ui, server = server)

