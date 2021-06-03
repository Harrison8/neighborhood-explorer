load("R/education_data.RData")
load("R/map_data.RData")
load("R/degree_district.RData")

degree_district$sex  <- "total"
degree_district$race <- "total"

education_variables <- c("No High School" = "no_hs",
                         "High School" = "hs",
                         "Some College" = "some_col",
                         "Associate" = "assoc",
                         "Bachelor's" = "bach",
                         "Graduate" = "grad",
                         "Associate's or higher" = "assoc_plus",
                         "Bachelor's or higher" = "bach_plus")

`%cols_in%` <- function (df, columns) columns[columns %in% names(df)]

pivot_vartype_wider <- function(df, variables) {
  df %>%
    pivot_longer(variables,
                 names_to = "variable",
                 values_to = "value") %>%
    pivot_wider(names_from = "var_type",
                values_from = "value")
}

pivot_vartype_longer <- function(df,
                                 names_from_col = "variable",
                                 values_from_col = "value") {
  df %>%
    pivot_longer(df %cols_in% c("estimate", "population", "MOE", "percent", "CI"),
                 names_to = "var_type",
                 values_to = "value") %>%
    pivot_wider(names_from = names_from_col, values_from = values_from_col)
}


# Summarise data for map
map_data_fxn <- function(input) {

  output_df <- switch(input$geography,
                      "Neighborhood" = degree_nh,
                      "Metro Council District" = degree_district)

  geog_var <- switch(input$geography,
                     "Neighborhood" = "neighborhood",
                     "Metro Council District" = "district")

  method = if_else(TRUE, "values", "percent_change")

  if(input$geography == "Neighborhood") {
    output_df %<>%
      filter(sex == "total", race == "total", !is.na(neighborhood), neighborhood != "Airport")
  }

  if (method == "percent_change") {
    output_df %<>%
      filter(year >=2008) %>%
      pivot_vartype_wider(output_df %cols_not_in% c("year", "sex", "race", "neighborhood", "district", "var_type")) %>%
      group_by(across(all_of(c(geog_var, "variable")))) %>%
      mutate(
        estimate = estimate - first(estimate),
        population,
        percent = percent - first(percent),
        overlap = pmax( 1 - (year-first(year)) / 5, 0),
        MOE = sqrt(1 - overlap) * sqrt( (MOE / 1.645) ^ 2 + (first(MOE) / 1.645) ^ 2) * 1.645,
        CI = MOE / population * 100) %>%
      ungroup() %>%
      pivot_vartype_longer()
  }


  if(input$geography == "Neighborhood") {
    output_df %<>% complete(year, neighborhood = "Airport")
  }

  output_df$var <- output_df[[input$variable]]

  output_df %<>% mutate(across(geog_var, ~as.character(.)))

  output_df
}

# Summarise data for the city
city_data_fxn <- function(input) {

  output_df <- switch(input$geography,
                      "Neighborhood" = degree_county,
                      "Metro Council District" = degree_county)

  geog_var <- switch(input$geography,
                     "Neighborhood" = "neighborhood",
                     "Metro Council District" = "district")

  method = if_else(TRUE, "values", "percent_change")

  output_df %<>%
    filter(sex == "total", race == "total", FIPS == "21111")

  if(geog_var == "district") output_df %<>% filter(year %in% 2008:2017)
  else output_df %<>% filter(year %in% c(2000, 2007:2017))

  if (method == "percent_change") {
    output_df %<>%
      filter(year >=2008) %>%
      pivot_vartype_wider(output_df %cols_not_in% c("FIPS", "year", "sex", "race", "neighborhood", "var_type")) %>%
      group_by(across(all_of(c("FIPS", "variable")))) %>%
      mutate(
        estimate = estimate - first(estimate),
        population,
        percent = percent - first(percent),
        overlap = pmax( 1 - (year-first(year)) / 5, 0),
        MOE = sqrt(1 - overlap) * sqrt( (MOE / 1.645) ^ 2 + (first(MOE) / 1.645) ^ 2) * 1.645,
        CI = MOE / population * 100) %>%
      ungroup() %>%
      pivot_vartype_longer()
  }

  output_df$var <- output_df[[input$variable]]

  output_df %<>%
    transmute(year,
              !!geog_var := "city",
              var_type, var)

  output_df

}

# Return map object
map_fxn <- function(input) {

  output_map <- switch(input$geography,
                       "Neighborhood" = map_nh,
                       "Metro Council District" = map_district)

  output_map
}

# Combine data and map to create a map object
combine_map <- function(data_object, map_object, input) {

  # Filter to year
  temp_df <- data_object %>%
    filter(year == input$year,
           var_type == "percent")

  # Bind data and rename variable
  if(input$geography == "Neighborhood") {
    map_object %<>% left_join(temp_df, by = "neighborhood")

    # Create hover text and format as HTML
    map_object %<>%
      mutate(
        line1 = paste0(neighborhood, " neighborhood"),
        line3 = paste0(names(education_variables)[education_variables == input$variable],
                       ": ",
                       round(.data[[input$variable]], 1), "%"))
  } else {
    map_object %<>%
      mutate(district = as.character(district)) %>%
      left_join(temp_df, by = "district")

    # Create hover text and format as HTML
    map_object %<>%
      mutate(
        line1 = paste0("District ", district),
        line3 = paste0(names(education_variables)[education_variables == input$variable],
                       ": ",
                       round(.data[[input$variable]], 2), "%"))
  }

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

make_graph <- function(data_object, city_input, input) {

  geog <- if_else("neighborhood" %in% names(data_object),
                  "neighborhood",
                  "district")

  data_object %<>%
    select_at(c("year", geog, "var_type", "var")) %>%
    bind_rows(city_input) %>%
    filter(var_type %in% c("percent", "CI")) %>%
    pivot_vartype_wider("var") %>%
    mutate(
      lower = percent - CI,
      upper = percent + CI) %>%
    select(-CI) %>%
    pivot_wider(names_from=geog, values_from = c(lower, percent, upper),
                names_glue = paste0("{", geog, "}.{.value}")) %>%
    mutate(year = paste0("1/1/", year) %>% as.POSIXct(format = "%m/%d/%Y"))


  # No selection made
  if(is.null(input$map_shape_click$id)) {

    pct_cols <- names(data_object)[str_detect(names(data_object), "percent")]

    data_object %<>% select(all_of(c("year", pct_cols)))

    max_value <- max(data_object[pct_cols], na.rm = TRUE)

    ts <- xts::xts(x = data_object, order.by = data_object$year)
    output_graph <- dygraph(ts,
                            main = names(education_variables)[education_variables == input$variable])

    output_graph %<>%
      dyGroup(pct_cols,
              color = rep("#bababa", length(pct_cols)))

    output_graph %<>% dyLegend(show = "never")

  # Selection made
  } else {

    input_id <- input$map_shape_click$id
    # Previuosly kept all percent columns
    # pct_cols <- names(data_object)[str_detect(names(data_object), "percent") &
    #                                !str_detect(names(data_object), input_id) &
    #                                !str_detect(names(data_object), "city") ]
    #
    # data_object %<>% select(all_of(c("year", pct_cols,
    #                                  "city.lower", "city.percent", "city.upper",
    #                                  paste0(input_id, c(".lower", ".percent", ".upper")))))

    data_object %<>% select(all_of(c("year",
                                     paste0(input_id, c(".lower", ".percent", ".upper")),
                                     "city.lower", "city.percent", "city.upper")))

    max_value <- max(data_object[c(paste0(input_id, c(".lower", ".percent", ".upper")),
                                   "city.lower", "city.percent", "city.upper")], na.rm = TRUE)

    ts <- xts::xts(x = data_object, order.by = data_object$year)
    output_graph <- dygraph(ts,
                            main = names(education_variables)[education_variables == input$variable])

    selected_label <- if_else(geog == "neighborhood", input_id, paste0("District ", input_id))

    output_graph %<>%
      # dyGroup(pct_cols,
      #           color = rep("#bababa", length(pct_cols))) %>%
      dySeries(c("city.lower", "city.percent", "city.upper"), label = "City Average", color = "#000000", strokeWidth = 3) %>%
      dySeries(paste0(input_id, c(".lower", ".percent", ".upper")), label = selected_label, color = "#00a9b7", strokeWidth = 3)

    output_graph %<>% dyLegend(show = "always")
  }

  output_graph %<>%
    dyLimit(0, strokePattern = "solid") %>%
    dyLegend(width = 400) %>%
    dyAxis("x",
           "Year",
           axisLabelFormatter="function(d) { return d.getFullYear() }",
           rangePad = 10,
           ticker = "function(a, b, pixels, opts, dygraph, vals) {
                       return Dygraph.getDateAxis(a, b, Dygraph.ANNUAL, opts, dygraph)
                     }") %>%
    dyAxis("y",
           "Percent",
           valueFormatter = "function(v){return (v*1).toFixed(1) + '%'}",
           valueRange = c(0, max_value + 10),
           rangePad = 10)

  output_graph

}

#
# data_fxn(test)
# data_fxn2(test)
#
# map_fxn
# combine_map
# make_map
#
# test <- list(geography = "Metro Council District",
#              variable = "no_hs",
#              year = 2017,
#              map_shape_click = list( id = "8"))
#
# make_graph(map_data_fxn(test), city_data_fxn(test), test)
# combine_map(data_fxn(test), map_fxn(test), test)
