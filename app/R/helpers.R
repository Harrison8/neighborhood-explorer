load("R/education_data.RData")
load("R/map_data.RData")

data_fxn <- function(input) {

  output_df <- switch(input$geography, "Neighborhood" = degree_nh)

  output_df %<>%
    filter(sex == "total", race == "total", !is.na(neighborhood), neighborhood != "Airport")

  output_df %<>%
    group_by(neighborhood) %>%
    mutate(across(input$variable, ~ . - .[year == 2000])) %>%
    ungroup()

  output_df %<>% complete(year, neighborhood = "Airport")

  output_df

}

map_fxn <- function(input) {

  output_map <- switch(input$geography, "Neighborhood" = map_nh)

  output_map
}

combine_map <- function(data_object, map_object, input) {

  # Filter to year
  temp_df <- data_object %>%
    filter(year == input$year)

  # Bind data and rename variable
  map_object %<>% left_join(temp_df, by = "neighborhood")
  map_object$var <- map_object[[input$variable]]

  # Create hover text and format as HTML
  map_object %<>%
    mutate(
      line1 = paste0(neighborhood, " neighborhood"),
      line3 = paste0(input$variable, ": ", round(.data[[input$variable]], 2), "%"))

  map_object
}


make_map <- function(data_object, map_object, input) {

  #Define palette using color_style parameter
  color_vector <- RColorBrewer::brewer.pal(9, "BuPu")

  var_range <- range(map_object[[input$variable]], na.rm = T)

  na_present <- any(is.na(map_object[[input$variable]]))

  if(na_present) var_range = c(var_range, NA_real_)

  pal <- leaflet::colorNumeric(
      palette = color_vector,
      domain = var_range)


  #Create map title using legend_title parameter

  legend_title <- input$variable

  m <- leaflet() %>%
    addTiles()

  m <- m %>%
    addLegend(pal = pal, values = var_range, opacity = 0.7, title = legend_title, position = "bottomright")

  m <- m %>% addPolygons(
    data = map_object,
    color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.5,
    fillColor = ~pal(var),
    label = labels,
    layerId = c(1:25),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", "font-family" = "Arial", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))

  # Fix legend NA value
  #css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
  #html_fix <- htmltools::tags$style(type = "text/css", css_fix)
  #m %<>% htmlwidgets::prependContent(html_fix)

  m
}

make_graph <- function(data_object, input) {

  data_object %<>%
    select(year, neighborhood, bach_plus) %>%
    pivot_wider(names_from = neighborhood, values_from = bach_plus) %>%
    mutate(year = paste0("1/1/", year) %>% as.POSIXct(format = "%m/%d/%Y"))

  ts <- xts(x = data_object, order.by = data_object$year)

  output_graph <- dygraph(ts)

  if(is.null(input$map_shape_click$id)) {
    output_graph %<>%
      dyGroup(names(data_object)[2:26],
              color = rep("#7f7f7f", 25))
  } else {
    output_graph %<>%
    dySeries(input$map_shape_click$id, color = "#00a9b7", strokeWidth = 3) %>%
    dyGroup(setdiff(names(data_object)[2:26], input$map_shape_click$id),
              color = rep("#7f7f7f", 24))
  }

  output_graph %<>%
    dyLegend(show = "never") %>%
    dyAxis("x", axisLabelFormatter="function(d) { return d.getFullYear() }") %>%
    dyAxis("x", ticker="function(a, b, pixels, opts, dygraph, vals) {
                            return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)
                        }") %>%
    dyAxis("y", "Percent Change", valueRange = c(-5, 20))

  output_graph

}


test <- list(geography="Neighborhood", variable="bach_plus", year=2016)

make_map(data_fxn(test), map_fxn(test), test)

#make_graph(data_fxn(test), test)



