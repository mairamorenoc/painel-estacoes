# -----------------------------------------------
# SERVER
# -----------------------------------------------

server <- function(input, output, session) {
  # LABEL LIST/DICTONARIES ----------------------

  # Label dictionary to naming stations
  station_labels <- c (
    "tb_estacao_1b" = "Merajuba/Mocajuba, PA",
    "mare" = "Complexo da Maré, RJ" # "unified" mare station label
  )
  
  # "Virtual" station map
  # Since "mare" is not a real DB table. Here It represents two actual tables
  station_table_map <- list(
    "mare" = c("tb_estacao_3", "tb_estacao_4") ## OBS é tab_estacao_4 ou tb_estacao_4?
  )
  
  # Exclusions moved outside observeEvent to make them Global
  # Remove unwanted sensors for ALL stations
  excluded_global <- c("1", "25", "26") ## Wifi, Bluetooth and Mobile data
  
  # Remove unwanted sensors for SPECIFIC real tables
  excluded_by_table <- list(
    "tb_estacao_3" = c("48", "49"),
    "tb_estacao_4" = c("23") ## Pressure sensor - already in main station
  )
  
  # Helper to get real DB table names from selected station
  get_station_tables <- function(station_id) {
    if (station_id %in% names(station_table_map)) {
      station_table_map[[station_id]]
    } else {
      station_id
    }
  }
  
  # Default sensors----------------------------------

  # Label dictionary to naming sensors (climate variables) on dropdown menu
  sensor_labels <- c(
    "8" = "Temperatura (°C)",
    "11" = "Umidade (%)",
    "18" = "Luminosidade (lux)",
    "19" = "UV (uv)",
    "22" = "Ponto de Orvalho (°C)",
    "23" = "Pressão (hPa)",
    "27" = "Sensação térmica (°C)",
    "28" = "Delta T (°C)",
    "34" = "Nivel de líquidos (mca)",
    "35" = "Chuva (mm)",
    "36" = "Vento (km/h)",
    "37" = "Rajada (km/h)",
    "347" = "Vento (km/h)",
    "348" = "Rajada (km/h)"
  ) ## OBS. Alguns sensores (48, 49) não estão na lista que o Rapha passou. Perguntar

  # Label dictionary to costumize sensor plottly plots (color, chart type)
  sensor_config <- list(
    # Temperature related
    "8" = list(type = "scatter", mode = "lines", color = "#e63946"), ## T°
    "27" = list(type = "scatter", mode = "lines", color = "#f4a261"), ## Sensacao termica

    # Delta T
    "28" = list(type = "scatter", mode = "lines", color = "#e5e619"), ## Delta T
    "22" = list(type = "scatter", mode = "lines", color = "#228b22"), ## Ponto de Orvalo (WTF is that??)

    # Pressure
    "23" = list(type = "scatter", mode = "lines", color = "#800080"), ## Pressao atmosferica

    # Humidity
    "11" = list(type = "scatter", mode = "lines", color = "#457b9d"), ## Umidade

    # Rain
    "35" = list(type = "bar", mode = NULL, color = "#2a9d8f"), ## Chuva (acumulada?)

    # Liquid level
    "34" = list(type = "bar", mode = NULL, color = "#00009c"), ## Nivel de liquidos

    # Wind (speed I think)
    "36" = list(type = "scatter", mode = "lines", color = "#264653"), ## Vento
    "347" = list(type = "scatter", mode = "lines", color = "#264653"), ## Vento - pq 2 cod ?

    # Wind gust
    "37" = list(type = "scatter", mode = "lines", color = "#19b2e6"), ## Rajadas de vento
    "348" = list(type = "scatter", mode = "lines", color = "#19b2e6"), ## Rajadas de vento - de novo pq 2 cod ?

    # Luminosity
    "18" = list(type = "scatter", mode = "lines", color = "#ffbe0b"), ## Chuva

    # UV (Index?)
    "19" = list(type = "scatter", mode = "lines", color = "#ff006e"), ## Uv

    # Missinmg codes
    "default" = list(type = "scatter", mode = "lines", color = "#333333") # see and plot those "unknown" sensors codes
  )

  # Sensor categories list schema (for dropdown, plots and so on)
  categories <- list(
    Temperatura = list(ids = c("8", "27"), unit = "°C"),
    "Delta T" = list(ids = c("28", "22"), unit = "°C"), ## OBS. Nomes sem espaço - nao esquecer
    Umidade = list(ids = c("11"), unit = "%"),
    Pressão = list(ids = c("23"), unit = "hPa"),
    Chuva = list(ids = c("35"), unit = "mm"),
    "Nivel de Liquidos" = list(ids = c("34"), unit = "mca"),
    Vento = list(ids = c("36", "347", "37", "348"), unit = "Km/h"),
    Luminosidade = list(ids = c("18"), unit = "lux"),
    UV = list(ids = c("19"), unit = "uv")
  )
  
  # Air Quality sensors---------------------
  
  # Label dictionary for air quality sensors
  airQuality_labels <- c(
    "70"   = "Ozônio (O3)",
    "71"  = "Material Particulado 2.5 (PM2.5)",
    "72"  = "Material Particulado 10 (PM10)",
    "73"  = "Dióxido de Enxofre (SO2)",
    "74"  = "Dióxido de Nitrogênio (NO2)",
    "75"  = "Monóxido de Carbono (CO)"
  )
  
  # Air quality sensors categories list schema (for dropdown, plots and so on)
  airQuality_cats <- list(
    "Material Particulado (PM)" = list(ids=c("71", "72"), unit="µg/m³"),
    "Ozônio (O3)" = list(ids=c("70"), unit="ppm"),
    "Dióxido de Enxofre (SO2)" = list(ids=c("73"), unit="ppm"),
    "Dióxido de Nitrogênio (NO2)" = list(ids=c("74"), unit="ppm"),
    "Monóxido de Carbono (CO)" = list(ids=c("75"), unit="ppm")
  )
  
  # Label dictionary to customize air quality sensor plottly plots (color, chart type)
  airQuality_config <- list(
    
    # Gases
    "70"  = list(type = "scatter", mode = "lines", color = "#800080"), ## O3
    "73" = list(type = "scatter", mode = "lines", color = "#00009c"), ## SO2
    "74" = list(type = "scatter", mode = "lines", color = "#457b9d"), ## NO2
    "75" = list(type = "scatter", mode = "lines", color = "#264653"), ## CO
    
    # Particulate Matter
    "71" = list(type = "scatter", mode = "lines", color = "#e5e619"), ## PM2.5
    "72" = list(type = "scatter", mode = "lines", color = "#228b22"), ## PM10
    
    # Missing codes
    "default" = list(type = "scatter", mode = "lines", color = "#333333") # see and plot those "unknown" sensors codes 
  )
  
  # Contextual Metadata ------------------------
  
  # Station metadata for sidebar contextual info
  station_meta <- list(
    "tb_estacao_1b" = list(
      text = "Localizada em  Merajuba/Mocajuba, Pará.", ## OBS. atualizar
      lat = -2.50974,
      lon = -49.46684
    ),
    "mare" = list(
      text = "Localizada na Casa das Mulheres da Maré.", 
      lat = -22.85172,
      lon = -43.24457
    ) ## Render RJ map when "mare" virtual st. is selected
  )
  

  # DB CONNECTION (via DuckDB) -----------------------------

  # con <- DBI::dbConnect(
  #   duckdb::duckdb(),
  #   file.path("data", "estacoes.duckdb")
  # )

  # DB CONNECTION (via Postgres) -----------------------------

  source("credentials.R")
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "observatorio",
    host = "psql.icict.fiocruz.br",
    port = 5432,
    user = user,
    password = password
  )

  # Postgres schema prefix to tables
  schema <- "estacoes"

  session$onSessionEnded(function() {
    DBI::dbDisconnect(con) ## Close DB conn on session ends
  })

  # INPUT PIPELINE -----------------------------------------------------

  # Available stations (tables)
  station_names <- c("tb_estacao_1b", "mare") # Using "mare" virtual st. instead of separated tables

  station_choices <- setNames(
    station_names,
    sapply(station_names, function(id) {
      if (id %in% names(station_labels)) {
        station_labels[id]
      } else {
        paste("Estação", id)
      }
    })
  )

  # Update and populate dropdown menu with the available retrieved stations
  updateSelectInput(
    session,
    "station",
    choices = station_choices, ## adds listed and unlisted stations to the dropdown
    selected = station_names[1] ## choose the fisrt table as default one
  ) 
  
  # Reactive expression to choose active sensor metadata according to selected station 
  sensor_meta <- reactive ({
    
    req(input$station)
    
    # Choose active metadata according to station
    if (input$station == "mare") {
      active_labels <- c(sensor_labels, airQuality_labels)
      active_categories <- c(categories, airQuality_cats)
      active_config <- c(sensor_config, airQuality_config)
    } else {
      active_labels <- sensor_labels
      active_categories <- categories
      active_config <- sensor_config
    }
    
    list(
      labels = active_labels,
      categories = active_categories,
      config = active_config
    )
    
  })
  # Input event-driven reactive logic for DATE calendar menu ------------
  observeEvent(
    input$station,
    {
      req(input$station)
      
      # Get real table names from selected station
      station_tables <- get_station_tables(input$station)
      
      # Get available dates from selected station/table(s)
      available_dates <- c()
      
      for (table_name in station_tables) {
        
        dates_df <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT DISTINCT DATE(time) as d FROM ",
            schema,
            ".",
            table_name,
            " ORDER BY d"
          )
        )
        
        available_dates <- c(available_dates, as.Date(dates_df$d))
      }
      
      available_dates <- sort(unique(available_dates))
      
      # Get latest available date
      latest_date <- max(available_dates, na.rm = TRUE)
      
      # Dynamic input calendar update
      updateDateInput(
        session,
        "selected_date",
        value = latest_date,
        min = min(available_dates),
        max = latest_date
      )
      
      # Input Reactive conditional logic for SENSOR dropdown menu ----------------
      
      # Detect available sensors in selected station/table(s)
      sensor_ids <- character(0)
      
      for (table_name in station_tables) {
        
        sensors_df <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT DISTINCT sensor FROM ",
            schema,
            ".",
            table_name,
            " ORDER BY sensor"
          )
        )
        
        table_sensor_ids <- as.character(sensors_df$sensor)
        
        # Get table-specific exclusions
        table_excluded <- excluded_by_table[[table_name]]
        
        if (is.null(table_excluded)) {
          table_excluded <- character(0)
        }
        
        # Apply global and table-specific exclusions
        table_sensor_ids <- table_sensor_ids[
          !table_sensor_ids %in% c(excluded_global, table_excluded)
        ]
        
        sensor_ids <- c(sensor_ids, table_sensor_ids)
      }
      
      sensor_ids <- unique(sensor_ids)
      
      # Get active sensor metadata from reactive expression
      meta <- sensor_meta()
      
      # Keep only categories that still have at least one available sensor
      available_categories <- Filter(
        function(cat) {
          any(cat$ids %in% sensor_ids)
        },
        meta$categories
      )
      
      # DEBUG: check available categories after filtering
      print(names(available_categories))
      
      # Get sensor ids from available categories
      categorized_ids <- unique(
        unlist(lapply(available_categories, function(i) i$ids))
      )
      
      # Get all unlisted sensors
      standalone_ids <- sensor_ids[!sensor_ids %in% categorized_ids]
      
      # Set category label choices for sensor categories dropdown menu
      category_choices <- setNames(
        paste0("cat_", names(available_categories)),
        names(available_categories)
      )
      
      # Set label choices for missing sensors in the sensor dropdown menu
      standalone_choices <- setNames(
        standalone_ids,
        sapply(standalone_ids, function(id) {
          if (id %in% names(meta$labels)) {
            meta$labels[id]
          } else {
            paste("Sensor", id)
          }
        })
      )
      
      # Put all dropdown label choices together in one vector
      all_choices <- c(category_choices, standalone_choices)
      
      # Update and populate sensor dropdown menu with categories and missing sensor label choices
      updateSelectInput(
        session,
        "sensor",
        choices = all_choices,
        selected = all_choices[1]
      )
    },
    ignoreInit = TRUE
  ) ## OBS. NAO ESQUECER: ignoreInit = TRUE → Run code ONLY when the user changes the input

  # PREPARING DATA FOR PLOTING -----------------------------------------------

  # Data pipeline to plotting MAIN charts -----------------------------------------

  # Reactive logic to fetch daily station data by sensor
  sensor_data <- reactive({
    
    req(input$station, input$selected_date)
    
    # Get real table names from selected station
    station_tables <- get_station_tables(input$station)
    
    # Get available dates from selected station/table(s)
    available_dates <- c()
    
    for (table_name in station_tables) {
      
      dates_df <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT DISTINCT DATE(time) as d FROM ",
          schema,
          ".",
          table_name
        )
      )
      
      available_dates <- c(available_dates, as.Date(dates_df$d))
    }
    
    available_dates <- sort(unique(available_dates))
    
    # Convert selected date as date object
    selected_date <- as.Date(input$selected_date)
    
    # Fallback logic for unavailable dates
    # if (!selected_date %in% available_dates) {
      # selected_date <- max(available_dates[available_dates <= selected_date])
    # } ## OBS. RETIRAR. Separation of conscerns → delegar para a UI
    
    # Define daily data for main chart plots
    start_time <- as.POSIXct(selected_date)
    end_time <- start_time + lubridate::days(1)
    
    # Fetch daily data from selected station/table(s)
    df_list <- list()
    
    for (table_name in station_tables) {
      
      # Original production syntax kept for compatibility.
      # Alternative syntax: DBI::Id(schema = schema, table = table_name)
      df_table <- dplyr::tbl(con, DBI::Id(schema, table_name)) |>
        dplyr::filter(
          time >= start_time,
          time < end_time
        ) |>
        dplyr::select(sensor, time, value) |>
        dplyr::arrange(sensor, time) |>
        dplyr::collect()
      
      table_excluded <- excluded_by_table[[table_name]]
      
      if (is.null(table_excluded)) {
        table_excluded <- character(0)
      }
      
      df_table <- df_table |>
        dplyr::filter(
          !sensor %in% as.integer(c(excluded_global, table_excluded))
        )
      
      df_list[[table_name]] <- df_table
    }
    
    df <- dplyr::bind_rows(df_list)
    
    df$time <- df$time - lubridate::hours(3)
    
    df
  })

  # Data pipeline to ploting KPI cards -------------------------------------------
  
  # KPI station schema 
  kpi_station_map <- c(
    "mare" = "tb_estacao_3" # Update kpi st. map to use mare tab
  ) ## OBS. Mapping vectors são melhores se precisar acrescentar mais uma estação com essa regra
  
  # Rule for choosing which station will be used on KPI Cards
  kpi_station <- reactive({
    
    req(input$station)
    
    if (input$station %in% names(kpi_station_map)) {
      kpi_station_map[[input$station]]
    } else {
      input$station
    }
    
  })
  
  # Fetch latest hour from KPI specific sensors
  latest_data <- reactive({
    req(kpi_station()) ## this will use reactive rule instead of input$station

    sensor_ids <- c(8, 23, 35, 36, 347) ## Predefined key indicators sensors
    sensor_sql <- paste(sensor_ids, collapse = ", ") ## Take all sensor IDs and combine them into one comma-separated string → ideal for SQL query
    
    # UPDATED DB query
    df <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT t.sensor, t.time, t.value
       FROM ", schema, ".", kpi_station(), " t
       INNER JOIN (
         SELECT sensor, MAX(time) AS max_time
         FROM ", schema, ".", kpi_station(), "
         WHERE sensor IN (", sensor_sql, ")
         GROUP BY sensor
       ) last
       ON t.sensor = last.sensor
       AND t.time = last.max_time
       WHERE t.sensor IN (", sensor_sql, ")"
      )
    ) ## Now this returns the last available value for each KPI sensor and not just the the last available according to time stamp
    
    df$time <- df$time - lubridate::hours(3) ## UTC to brasilia time-zone
    
    df
    
  })

  # Function to get each KPI sensor data
  get_sensor_value <- function(placeholder_id) {
    df <- latest_data()
    vals <- df$value[df$sensor == placeholder_id]
    
    if (length(vals) == 0) {
      return(NA_real_)
    } ## Adds defensive programming expression
    
    vals[1]
  } 

  # PLOTING DATA-----------------------------------------

  # Get station names for plots labeling
  station_nm <- reactive({
    req(input$station)

    if (input$station %in% names(station_labels)) {
      station_labels[[input$station]]
    } else {
      paste("Estação", input$station)
    }
  })
  
  # Render station info card on sidebar ---------------
  
  # Render station description text
  output$station_info <- renderText ({
    req(input$station)
    
    meta_st <- station_meta[[input$station]] ## Retrieves metadata of selected station from object list dictionary
    meta_st$text ## Selects text attribute for rendering
    
  }) ## OBS. Make this reactive later
  
  # Plot station location map
  output$station_map <- renderLeaflet({
    req(input$station)
    
    meta_map <- station_meta[[input$station]] 
    
    leaflet() %>%
      addTiles %>%
      addMarkers(
        lng = meta_map$lon,
        lat = meta_map$lat
      )
  })
  

  # Render KPI charts ----------------------------------

  # Temperature KPI Card
  output$temp_value <- renderUI({
    value <- get_sensor_value(8)
    tags$div(class = "kpi-value", paste0(value, " °C"))
  })

  # Rain KPI CArd
  output$rain_value <- renderUI({
    value <- get_sensor_value(35)
    tags$div(class = "kpi-value", paste0(value, " mm"))
  })

  # Pressure KPI Card
  output$pressure_value <- renderUI({
    value <- get_sensor_value(23)
    tags$div(class = "kpi-value", paste0(value, " hPa"))
  })

  # Wind KPI Card
  output$wind_value <- renderUI({
    req(input$station)

    df <- latest_data()
    req(nrow(df) > 0)

    # Get wind value (sensor 36 or 347)
    value <- df$value[df$sensor %in% c(36, 347)][1]

    req(!is.na(value))

    tags$div(
      class = "kpi-value",
      paste0(round(value, 1), " km/h")
    )
  })

  # Formatting KPI CARDS subtitles -------------------------

  formatted_time <- reactive({
    df <- latest_data()
    if (nrow(df) == 0) {
      return(NULL)
    }
    format(df$time[1], "%d-%m-%Y %H:%M") ## Brazilian time format
  })

  # Temperature
  output$temp_sub <- renderUI({
    tagList(
      station_nm(), ## OBS. NAO esquecer () -> is reactive!
      tags$br(),
      formatted_time()
    )
  })

  # Rain
  output$rain_sub <- renderUI({
    tagList(
      station_nm(),
      tags$br(),
      formatted_time()
    )
  })

  # Pressure
  output$pressure_sub <- renderUI({
    tagList(
      station_nm(),
      tags$br(),
      formatted_time()
    )
  })

  # Wind
  output$wind_sub <- renderUI({
    tagList(
      station_nm(),
      tags$br(),
      formatted_time()
    )
  })

  # Render MAIN charts -----------------------------------

  output$sensor_plot <- renderPlotly({
    # Input reactive conditional logic for outputs
    req(input$sensor)

    # Logic to avoid empty plot when no data
    df_all <- sensor_data() |> ## Execute sensor_data block code and pass it to a df
      dplyr::distinct(sensor, time, .keep_all = TRUE) ## Keep only 1 row for each sensor and time

    req(nrow(df_all) > 0) ## Run code only if there any rows on df
    
    # Get active sensor metadata from reactive expression
    meta <- sensor_meta()

    # Get selected sensor
    selected <- input$sensor

    # Conditional statement to get sensor labels --------------------------
    if (startsWith(selected, "cat_")) {
      # Get category name
      category_name <- sub("cat_", "", selected) ## remove category prefix

      # GEt sensors and units from the category
      sensor_ids <- meta$categories[[category_name]]$ids ## take categories names
      unit_label <- meta$categories[[category_name]]$unit ## take categories units

      # From all sensor data, picks ONLY the ones that are in category list and makes them integer
      df <- df_all |>
        dplyr::filter(sensor %in% as.integer(sensor_ids)) ## OBS. Nao esquecer converter para integer!

      # Define plot title as category name
      plot_title <- category_name

      # ELSE block for when missing sensors are selected
    } else {
      # Picks selected sensor
      df <- df_all |>
        dplyr::filter(sensor == as.integer(selected))

      # Define title dynamically using sensor naming labels dictionary
      plot_title <- if (selected %in% names(meta$labels)) {
        meta$labels[selected] ## for listed sensors
      } else {
        paste("Sensor", selected) ## for missing sensors
      }
      unit_label <- "" ## Leaves empty (for now)
    }

    # Placeholder plot to allow loop over sensors
    p <- plotly::plot_ly()

    # Plot charts ---------------------------------------------------------------------

    # Loop over sensors to get plotting config and plot charts
    for (sid in unique(df$sensor)) {
      sid_char <- as.character(sid) ## match string format of dictionary labels
      config <- meta$config[[sid_char]] ## get plot config from dictionary (sensor_config) - chart type, color, etc.
      if (is.null(config)) {
        config <- meta$config[["default"]]
      } ## use default plot config for missing/new sensors

      # Conditional logic for sensor naming
      sensor_name <- if (sid_char %in% names(meta$labels)) {
        meta$labels[sid_char] ## use label from dictionary (sensor_labels)
      } else {
        paste("Sensor", sid_char) ## use "Sensor"  as label
      }

      # Build the plot object
      p <- p |>
        plotly::add_trace(
          data = df |> dplyr::filter(sensor == sid),
          x = ~time,
          y = ~value,
          type = config$type,
          mode = config$mode,
          name = sensor_name,
          line = list(color = config$color),
          text = ~ paste0(
            "Hora: ",
            format(time, "%H:%M"),
            "h",
            "<br>Valor: ",
            value,
            " ",
            unit_label
          ),
          hoverinfo = "text",
          textposition = "none" ## Removes hover text from bar charts
        ) ## OBS. add_trace() method permite plotar different chart types on the same graph
    }

    # Special config for UV sensor plotting ----------------------------------------------------

    # Add background shapes ("risk bands") to the plot when UV selected
    if (selected == "19" || selected == "cat_UV") {
      p <- p |>
        layout(
          shapes = list(
            list(
              type = "rect",
              xref = "paper",
              x0 = 0,
              x1 = 1,
              y0 = 0,
              y1 = 3,
              fillcolor = "green",
              opacity = 0.1,
              line = list(width = 0)
            ), ## Baixo risco queimaduras
            list(
              type = "rect",
              xref = "paper",
              x0 = 0,
              x1 = 1,
              y0 = 3,
              y1 = 6,
              fillcolor = "yellow",
              opacity = 0.1,
              line = list(width = 0)
            ), ## Risco moderado
            list(
              type = "rect",
              xref = "paper",
              x0 = 0,
              x1 = 1,
              y0 = 6,
              y1 = 8,
              fillcolor = "orange",
              opacity = 0.1,
              line = list(width = 0)
            ), ## Alto risco
            list(
              type = "rect",
              xref = "paper",
              x0 = 0,
              x1 = 1,
              y0 = 8,
              y1 = 20,
              fillcolor = "red",
              opacity = 0.1,
              line = list(width = 0)
            ) ## Risco muito alto/extremo
          )
        )
    }

    # Final plot layout that will rendered ------------------------------------------------------
    p |>
      layout(
        title = paste(station_nm(), "|", plot_title),
        xaxis = list(
          title = "Hora",
          tickformat = "%H:%M\n%b %d"
        ),
        yaxis = list(
          title = "", ## Remove title for now
          automargin = TRUE
        ),

        legend = list(
          orientation = "h", ## place legend text horizontal
          x = 0,
          y = -0.25 ## place legend bellow and to the left of plotting area
        ),

        margin = list(l = 70, r = 20, t = 60, b = 80) ## b=80 to add more space
      )
  })
}
