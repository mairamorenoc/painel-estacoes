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
    "22" = "Ponto de orvalho (°C)",
    "23" = "Pressão (hPa)",
    "27" = "Sensação térmica (°C)",
    "28" = "Delta T (°C)",
    "34" = "Nivel do rio (mca)",
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
    "Nivel do rio" = list(ids = c("34"), unit = "mca"),
    Vento = list(ids = c("36", "347", "37", "348"), unit = "Km/h"),
    Luminosidade = list(ids = c("18"), unit = "lux"),
    UV = list(ids = c("19"), unit = "uv")
  )
  
  # Air Quality sensors---------------------
  
  # Label dictionary for air quality sensors
  airQuality_labels <- c(
    "70"   = "Ozônio (O3)",
    "71"  = "PM2.5",
    "72"  = "PM10",
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
  
  
  # CODE BLOCK (CREATED 23.06.26) FOR NEW STATISTICS TAB ---------
  # Populate station selector - Statistics tab
  observe({
    
    updateSelectInput(
      session,
      "stats_station",
      choices = station_choices,
      selected = unname(station_choices[1])
    )
  })
  
  # Populate sensor selector - Statistics tab
  observeEvent(input$stats_station, {
    
    req(input$stats_station)
    
    stats_station <- input$stats_station
    
    stats_tables <- if (stats_station == "mare") {
      c("tb_estacao_3", "tb_estacao_4")
    } else {
      stats_station
    }
    
    available_sensors <- c()
    
    for (table_name in stats_tables) {
      
      sensors_df <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT DISTINCT sensor
         FROM ", schema, ".", table_name, "
         ORDER BY sensor"
        )
      )
      
      available_sensors <- c(available_sensors, sensors_df$sensor)
    }
    
    available_sensors <- sort(unique(as.character(available_sensors)))
    
    allowed_labels <- sensor_labels
    
    if (stats_station == "mare") {
      allowed_labels <- c(sensor_labels, airQuality_labels)
    }
    
    available_sensors <- available_sensors[
      available_sensors %in% names(allowed_labels)
    ]
    
    if (length(available_sensors) == 0) {
      
      updateSelectInput(
        session,
        "stats_sensor",
        choices = character(0),
        selected = character(0)
      )
      
      return()
    }
    
    sensor_choices <- setNames(
      available_sensors,
      allowed_labels[available_sensors]
    )
    
    updateSelectInput(
      session,
      "stats_sensor",
      choices = sensor_choices,
      selected = available_sensors[1]
    )
  })
  
  # Calculate selected period - Statistics tab
  stats_period_range <- reactive({
    
    req(input$stats_base_date)
    req(input$stats_period)
    
    base_date <- as.Date(input$stats_base_date)
    
    if (input$stats_period == "day") {
      
      start_date <- base_date
      end_date <- base_date
      period_label <- "Dia"
      
    } else if (input$stats_period == "week") {
      
      # Monday to Sunday
      start_date <- base_date - lubridate::days(lubridate::wday(base_date, week_start = 1) - 1)
      end_date <- start_date + lubridate::days(6)
      period_label <- "Semana"
      
    } else {
      
      # Full calendar month
      start_date <- as.Date(format(base_date, "%Y-%m-01"))
      end_date <- seq(start_date, length = 2, by = "1 month")[2] - 1
      period_label <- "Mês"
    }
    
    list(
      start_date = start_date,
      end_date = end_date,
      start_time = as.POSIXct(start_date),
      end_time = as.POSIXct(end_date) + lubridate::days(1),
      label = period_label
    )
  })
  
  # Get selected sensor data - Statistics tab
  stats_sensor_data <- reactive({
    
    req(input$stats_station)
    req(input$stats_sensor)
    
    period <- stats_period_range()
    
    selected_sensor <- as.integer(input$stats_sensor)
    
    stats_tables <- if (input$stats_station == "mare") {
      c("tb_estacao_3", "tb_estacao_4")
    } else {
      input$stats_station
    }
    
    df_all <- data.frame()
    
    for (table_name in stats_tables) {
      
      df <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT sensor, time, value
         FROM ", schema, ".", table_name, "
         WHERE sensor = ", selected_sensor, "
         AND time >= '", period$start_time, "'
         AND time < '", period$end_time, "'
         ORDER BY time"
        )
      )
      
      # (UPDATED 18.07.26)
      if (nrow(df) > 0) {
        
        # Keep original date from queried timestamp for daily statistics
        # This avoids grouping rain data into the previous day after timezone adjustment
        df$stats_date <- as.Date(df$time)
        
        df$table_name <- table_name
        df_all <- dplyr::bind_rows(df_all, df)
      }
    }
    
    if (nrow(df_all) == 0) {
      return(df_all)
    }
    
    df_all$time <- df_all$time - lubridate::hours(3)
    
    df_all
  })
  
  # Update base date according to selected station and sensor - Statistics tab
  observeEvent(list(input$stats_station, input$stats_sensor), {
    
    req(input$stats_station)
    req(input$stats_sensor)
    
    selected_sensor <- as.integer(input$stats_sensor)
    
    stats_tables <- if (input$stats_station == "mare") {
      c("tb_estacao_3", "tb_estacao_4")
    } else {
      input$stats_station
    }
    
    available_times <- c()
    
    for (table_name in stats_tables) {
      
      time_df <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT MIN(time) AS min_time, MAX(time) AS max_time
         FROM ", schema, ".", table_name, "
         WHERE sensor = ", selected_sensor
        )
      )
      
      if (
        nrow(time_df) > 0 &&
        !is.na(time_df$min_time[1]) &&
        !is.na(time_df$max_time[1])
      ) {
        available_times <- c(
          available_times,
          time_df$min_time[1],
          time_df$max_time[1]
        )
      }
    }
    
    if (length(available_times) == 0) {
      
      updateDateInput(
        session,
        "stats_base_date",
        value = NULL,
        min = NULL,
        max = NULL
      )
      
      return()
    }
    
    available_times <- as.POSIXct(available_times)
    available_times <- available_times - lubridate::hours(3)
    
    min_date <- min(as.Date(available_times), na.rm = TRUE)
    max_date <- max(as.Date(available_times), na.rm = TRUE)
    
    updateDateInput(
      session,
      "stats_base_date",
      value = max_date,
      min = min_date,
      max = max_date
    )
  })
  
  # (UPDATED 18.07.2026) Helpers - Statistics tab
  
  # Return the user-facing label for any sensor ID
  get_sensor_label_by_id <- function(sensor_id) {
    
    sensor_id <- as.character(sensor_id)
    
    if (sensor_id %in% names(sensor_labels)) {
      return(unname(sensor_labels[sensor_id]))
    }
    
    if (sensor_id %in% names(airQuality_labels)) {
      return(unname(airQuality_labels[sensor_id]))
    }
    
    paste("Sensor", sensor_id)
  }
  
  # Return the measurement unit for any sensor ID
  get_sensor_unit_by_id <- function(sensor_id) {
    
    sensor_id <- as.character(sensor_id)
    sensor_label <- get_sensor_label_by_id(sensor_id)
    
    # Air quality sensors
    if (sensor_id %in% c("71", "72")) {
      return("µg/m³")
    }
    
    if (sensor_id %in% c("70", "73", "74", "75")) {
      return("ppm")
    }
    
    # Unit inside parentheses, e.g. "Temperatura (°C)"
    unit <- sub(".*\\((.*)\\).*", "\\1", sensor_label)
    
    if (unit != sensor_label) {
      return(unit)
    }
    
    ""
  }
  
  # Label for the sensor currently selected in the Statistics tab
  get_stats_sensor_label <- reactive({
    
    req(input$stats_sensor)
    
    get_sensor_label_by_id(input$stats_sensor)
  })
  
  # Unit for the sensor currently selected in the Statistics tab
  get_stats_sensor_unit <- reactive({
    
    req(input$stats_sensor)
    
    get_sensor_unit_by_id(input$stats_sensor)
  })
  
  format_stats_value <- function(value, unit = "", digits = 1) {
    
    if (is.na(value)) {
      return("—")
    }
    
    value <- round(value, digits)
    
    if (unit == "") {
      return(as.character(value))
    }
    
    paste(value, unit)
  }
  
  # (UPDATED 18.07.2026)
  # Monthly data for all sensors of the selected station ---------------------
  
  stats_month_station_data <- reactive({
    
    req(input$stats_station, input$stats_base_date)
    req(!is.na(input$stats_base_date))
    
    station_tables <- get_station_tables(input$stats_station)
    
    month_start <- lubridate::floor_date(
      as.Date(input$stats_base_date),
      unit = "month"
    )
    
    month_end <- lubridate::ceiling_date(
      month_start,
      unit = "month"
    )
    
    start_time <- as.POSIXct(month_start)
    end_time   <- as.POSIXct(month_end)
    
    # Follow the same sensor availability rule used by the Statistics tab
    allowed_labels <- sensor_labels
    
    if (input$stats_station == "mare") {
      allowed_labels <- c(
        sensor_labels,
        airQuality_labels
      )
    }
    
    allowed_sensor_ids <- as.integer(names(allowed_labels))
    
    sensor_summary_list <- list()
    rain_daily_list <- list()
    
    for (table_name in station_tables) {
      
      table_excluded <- excluded_by_table[[table_name]]
      
      if (is.null(table_excluded)) {
        table_excluded <- character(0)
      }
      
      excluded_ids <- as.integer(
        c(
          excluded_global,
          table_excluded
        )
      )
      
      valid_sensor_ids <- setdiff(
        allowed_sensor_ids,
        excluded_ids
      )
      
      if (length(valid_sensor_ids) == 0) {
        next
      }
      
      table_sql <- as.character(
        DBI::dbQuoteIdentifier(
          con,
          DBI::Id(
            schema = schema,
            table = table_name
          )
        )
      )
      
      start_sql <- as.character(
        DBI::dbQuoteString(
          con,
          format(start_time, "%Y-%m-%d %H:%M:%S")
        )
      )
      
      end_sql <- as.character(
        DBI::dbQuoteString(
          con,
          format(end_time, "%Y-%m-%d %H:%M:%S")
        )
      )
      
      sensor_ids_sql <- paste(
        valid_sensor_ids,
        collapse = ", "
      )
      
      # Aggregate statistics and identify the first timestamp
      # associated with the minimum and maximum values
      summary_sql <- paste0(
        "WITH ranked AS (
           
           SELECT
             sensor,
             time,
             value,
             
             ROW_NUMBER() OVER (
               PARTITION BY sensor
               ORDER BY value ASC, time ASC
             ) AS min_rank,
             
             ROW_NUMBER() OVER (
               PARTITION BY sensor
               ORDER BY value DESC, time ASC
             ) AS max_rank,
             
             AVG(value) OVER (
               PARTITION BY sensor
             ) AS avg_value,
             
             COUNT(value) OVER (
               PARTITION BY sensor
             ) AS n_records
           
           FROM ", table_sql, "
           
           WHERE time >= ", start_sql, "
             AND time < ", end_sql, "
             AND sensor IN (", sensor_ids_sql, ")
             AND value IS NOT NULL
         )
         
         SELECT
           sensor,
           
           MAX(
             CASE
               WHEN min_rank = 1 THEN value
             END
           ) AS min_value,
           
           MAX(
             CASE
               WHEN min_rank = 1 THEN time
             END
           ) AS min_time,
           
           MAX(
             CASE
               WHEN max_rank = 1 THEN value
             END
           ) AS max_value,
           
           MAX(
             CASE
               WHEN max_rank = 1 THEN time
             END
           ) AS max_time,
           
           MAX(avg_value) AS avg_value,
           MAX(n_records) AS n_records
         
         FROM ranked
         
         GROUP BY sensor
         ORDER BY sensor"
      )
      
      table_summary <- DBI::dbGetQuery(
        con,
        summary_sql
      )
      
      if (nrow(table_summary) > 0) {
        
        # Follow the same time adjustment used by the dashboard
        table_summary$min_time <- as.POSIXct(
          table_summary$min_time,
          tz = "UTC"
        ) - lubridate::hours(3)
        
        table_summary$max_time <- as.POSIXct(
          table_summary$max_time,
          tz = "UTC"
        ) - lubridate::hours(3)
        
        table_summary$table_name <- table_name
        
        sensor_summary_list[[table_name]] <- table_summary
      }
      
      # Rain requires daily totals instead of raw-reading statistics
      if (35L %in% valid_sensor_ids) {
        
        rain_sql <- paste0(
          "SELECT
             CAST(time AS DATE) AS date,
             SUM(value) AS daily_total,
             COUNT(value) AS n_records
           FROM ", table_sql, "
           WHERE time >= ", start_sql, "
             AND time < ", end_sql, "
             AND sensor = 35
             AND value IS NOT NULL
           GROUP BY CAST(time AS DATE)
           ORDER BY date"
        )
        
        rain_daily <- DBI::dbGetQuery(
          con,
          rain_sql
        )
        
        if (nrow(rain_daily) > 0) {
          
          rain_daily$table_name <- table_name
          
          rain_daily_list[[table_name]] <- rain_daily
        }
      }
    }
    
    # Combine sensor summaries from all real tables
    sensor_summary_raw <- dplyr::bind_rows(
      sensor_summary_list
    )
    
    if (nrow(sensor_summary_raw) == 0) {
      
      sensor_summary_raw <- data.frame(
        sensor = integer(),
        min_value = numeric(),
        min_time = as.POSIXct(
          character(),
          tz = "UTC"
        ),
        max_value = numeric(),
        max_time = as.POSIXct(
          character(),
          tz = "UTC"
        ),
        avg_value = numeric(),
        n_records = numeric()
      )
      
    } else {
      
      sensor_summary_raw <- sensor_summary_raw |>
        dplyr::mutate(
          sensor = as.integer(sensor),
          min_value = as.numeric(min_value),
          min_time = as.POSIXct(
            min_time,
            tz = "UTC"
          ),
          max_value = as.numeric(max_value),
          max_time = as.POSIXct(
            max_time,
            tz = "UTC"
          ),
          avg_value = as.numeric(avg_value),
          n_records = as.numeric(n_records)
        )
      
      # Select the global minimum and its corresponding timestamp
      minimum_rows <- sensor_summary_raw |>
        dplyr::arrange(
          sensor,
          min_value,
          min_time
        ) |>
        dplyr::group_by(sensor) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup() |>
        dplyr::select(
          sensor,
          min_value,
          min_time
        )
      
      # Select the global maximum and its corresponding timestamp
      maximum_rows <- sensor_summary_raw |>
        dplyr::arrange(
          sensor,
          dplyr::desc(max_value),
          max_time
        ) |>
        dplyr::group_by(sensor) |>
        dplyr::slice_head(n = 1) |>
        dplyr::ungroup() |>
        dplyr::select(
          sensor,
          max_value,
          max_time
        )
      
      # Calculate the weighted average and the total record count
      average_rows <- sensor_summary_raw |>
        dplyr::group_by(sensor) |>
        dplyr::summarise(
          avg_value = sum(
            avg_value * n_records,
            na.rm = TRUE
          ) / sum(
            n_records,
            na.rm = TRUE
          ),
          
          n_records = sum(
            n_records,
            na.rm = TRUE
          ),
          
          .groups = "drop"
        )
      
      sensor_summary_raw <- average_rows |>
        dplyr::left_join(
          minimum_rows,
          by = "sensor"
        ) |>
        dplyr::left_join(
          maximum_rows,
          by = "sensor"
        ) |>
        dplyr::select(
          sensor,
          min_value,
          min_time,
          max_value,
          max_time,
          avg_value,
          n_records
        ) |>
        dplyr::arrange(sensor)
    }
    
    # Combine daily rain totals from all real tables
    rain_daily <- dplyr::bind_rows(
      rain_daily_list
    )
    
    if (nrow(rain_daily) == 0) {
      
      rain_daily <- data.frame(
        date = as.Date(character()),
        daily_total = numeric(),
        n_records = numeric()
      )
      
    } else {
      
      rain_daily <- rain_daily |>
        dplyr::mutate(
          date = as.Date(date),
          daily_total = as.numeric(daily_total),
          n_records = as.numeric(n_records)
        ) |>
        dplyr::group_by(date) |>
        dplyr::summarise(
          daily_total = sum(
            daily_total,
            na.rm = TRUE
          ),
          n_records = sum(
            n_records,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        dplyr::arrange(date)
    }
    
    list(
      sensor_summary_raw = sensor_summary_raw,
      rain_daily = rain_daily,
      month_start = month_start,
      month_end = month_end - lubridate::days(1)
    )
  })
  
  # (UPDATED 18.07.2026)
  # Prepare the monthly report for all station sensors -----------------------
  
  stats_month_report_data <- reactive({
    
    monthly_data <- stats_month_station_data()
    
    sensor_summary_raw <- monthly_data$sensor_summary_raw
    rain_daily <- monthly_data$rain_daily
    
    period_label <- paste0(
      format(
        monthly_data$month_start,
        "%d-%m-%Y"
      ),
      " até ",
      format(
        monthly_data$month_end,
        "%d-%m-%Y"
      )
    )
    
    format_record_count <- function(value) {
      
      format(
        round(as.numeric(value)),
        big.mark = ".",
        decimal.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    }
    
    # Format the date and time of an extreme record
    format_record_datetime <- function(value) {
      
      if (
        length(value) == 0 ||
        is.na(value[1])
      ) {
        return("—")
      }
      
      format(
        value[1],
        "%d-%m-%Y %H:%M",
        tz = "UTC"
      )
    }
    
    # Common sensors
    # Rain is removed because it uses daily accumulated values
    common_sensors <- sensor_summary_raw |>
      dplyr::filter(sensor != 35L) |>
      dplyr::mutate(
        sensor_name = vapply(
          as.character(sensor),
          get_sensor_label_by_id,
          character(1)
        )
      ) |>
      dplyr::arrange(
        sensor_name,
        sensor
      )
    
    if (nrow(common_sensors) == 0) {
      
      sensor_summary <- data.frame(
        Sensor = character(),
        Código = character(),
        Mínimo = character(),
        Máximo = character(),
        Média = character(),
        `Número de medições` = character(),
        check.names = FALSE
      )
      
      sensor_details <- list()
      
    } else {
      
      # Compact summary table
      sensor_rows <- lapply(
        seq_len(nrow(common_sensors)),
        function(i) {
          
          sensor_id <- as.character(
            common_sensors$sensor[i]
          )
          
          unit <- get_sensor_unit_by_id(
            sensor_id
          )
          
          data.frame(
            Sensor = common_sensors$sensor_name[i],
            
            Código = sensor_id,
            
            Mínimo = format_stats_value(
              common_sensors$min_value[i],
              unit
            ),
            
            Máximo = format_stats_value(
              common_sensors$max_value[i],
              unit
            ),
            
            Média = format_stats_value(
              common_sensors$avg_value[i],
              unit
            ),
            
            `Número de medições` = format_record_count(
              common_sensors$n_records[i]
            ),
            
            check.names = FALSE
          )
        }
      )
      
      sensor_summary <- dplyr::bind_rows(
        sensor_rows
      )
      
      # Individual detail table for each sensor
      sensor_details <- lapply(
        seq_len(nrow(common_sensors)),
        function(i) {
          
          sensor_id <- as.character(
            common_sensors$sensor[i]
          )
          
          sensor_name <- common_sensors$sensor_name[i]
          
          unit <- get_sensor_unit_by_id(
            sensor_id
          )
          
          list(
            title = paste0(
              sensor_name
            ),
            
            table = data.frame(
              Métrica = c(
                "Mínimo",
                "Máximo",
                "Média",
                "Número de medições"
              ),
              
              Valor = c(
                format_stats_value(
                  common_sensors$min_value[i],
                  unit
                ),
                
                format_stats_value(
                  common_sensors$max_value[i],
                  unit
                ),
                
                format_stats_value(
                  common_sensors$avg_value[i],
                  unit
                ),
                
                format_record_count(
                  common_sensors$n_records[i]
                )
              ),
              
              `Data e hora` = c(
                format_record_datetime(
                  common_sensors$min_time[i]
                ),
                
                format_record_datetime(
                  common_sensors$max_time[i]
                ),
                
                "—",
                "—"
              ),
              
              check.names = FALSE
            )
          )
        }
      )
    }
    
    # Rain statistics
    if (nrow(rain_daily) == 0) {
      
      rain_summary <- data.frame(
        Métrica = character(),
        Valor = character(),
        Data = character(),
        check.names = FALSE
      )
      
      rain_daily_table <- data.frame(
        Data = character(),
        `Acumulado diário` = character(),
        Medições = character(),
        check.names = FALSE
      )
      
    } else {
      
      rain_unit <- get_sensor_unit_by_id("35")
      
      min_idx <- which.min(
        rain_daily$daily_total
      )
      
      max_idx <- which.max(
        rain_daily$daily_total
      )
      
      rain_summary <- data.frame(
        Métrica = c(
          "Chuva acumulada no mês",
          "Menor acumulado diário entre os dias com dados",
          "Maior acumulado diário",
          "Média diária acumulada entre os dias com dados",
          "Dias com dados",
          "Número de medições"
        ),
        Valor = c(
          format_stats_value(
            sum(
              rain_daily$daily_total,
              na.rm = TRUE
            ),
            rain_unit
          ),
          format_stats_value(
            rain_daily$daily_total[min_idx],
            rain_unit
          ),
          format_stats_value(
            rain_daily$daily_total[max_idx],
            rain_unit
          ),
          format_stats_value(
            mean(
              rain_daily$daily_total,
              na.rm = TRUE
            ),
            rain_unit
          ),
          as.character(nrow(rain_daily)),
          format_record_count(
            sum(
              rain_daily$n_records,
              na.rm = TRUE
            )
          )
        ),
        Data = c(
          "-",
          format(
            rain_daily$date[min_idx],
            "%d-%m-%Y"
          ),
          format(
            rain_daily$date[max_idx],
            "%d-%m-%Y"
          ),
          "-",
          "-",
          "-"
        ),
        check.names = FALSE
      )
      
      rain_daily_table <- rain_daily |>
        dplyr::transmute(
          Data = format(
            date,
            "%d-%m-%Y"
          ),
          `Acumulado diário` = paste0(
            round(daily_total, 1),
            " ",
            rain_unit
          ),
          Medições = vapply(
            n_records,
            format_record_count,
            character(1)
          )
        )
    }
    
    list(
      sensor_summary = sensor_summary,
      sensor_details = sensor_details,
      rain_summary = rain_summary,
      rain_daily = rain_daily_table,
      period_label = period_label
    )
  })
  
  # (UPDATED 18.07.2026)
  # Aggregate daily rain data - Statistics tab
  stats_daily_rain_data <- reactive({
    
    df <- stats_sensor_data()
    
    if (nrow(df) == 0) {
      return(data.frame(
        date = as.Date(character()),
        daily_total = numeric()
      ))
    }
    
    df |>
      dplyr::mutate(
        date = stats_date
      ) |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        daily_total = sum(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(date)
  })
  
  # Outputs - Statistics tab
  output$stats_summary_title <- renderUI({
    
    period <- stats_period_range()
    df <- stats_sensor_data()
    sensor_label <- get_stats_sensor_label()
    
    tags$div(
      style = "margin-bottom: 1rem; color: #6c757d;",
      tags$strong("Sensor analisado: "),
      sensor_label,
      tags$br(),
      tags$strong("Período analisado: "),
      paste0(
        format(period$start_date, "%d-%m-%Y"),
        " até ",
        format(period$end_date, "%d-%m-%Y")
      ),
      tags$br(),
      tags$strong("Registros encontrados: "),
      nrow(df)
    )
  })
  
  # (UPDATED 18.07.2026)
  output$stats_min_value <- renderUI({
    
    df <- stats_sensor_data()
    unit <- get_stats_sensor_unit()
    
    if (nrow(df) == 0) {
      return("—")
    }
    
    sensor_label <- get_stats_sensor_label()
    
    if (grepl("Chuva", sensor_label, ignore.case = TRUE)) {
      
      if (input$stats_period == "day") {
        return(format_stats_value(sum(df$value, na.rm = TRUE), unit))
      }
      
      rain_daily <- stats_daily_rain_data()
      
      if (nrow(rain_daily) == 0) {
        return("—")
      }
      
      return(format_stats_value(min(rain_daily$daily_total, na.rm = TRUE), unit))
    }
    
    format_stats_value(min(df$value, na.rm = TRUE), unit)
  })
  
  # (UPDATED 18.07.2026)
  output$stats_min_sub <- renderUI({
    
    sensor_label <- get_stats_sensor_label()
    
    if (grepl("Chuva", sensor_label, ignore.case = TRUE)) {
      
      if (input$stats_period == "day") {
        return("Chuva acumulada no dia")
      }
      
      rain_daily <- stats_daily_rain_data()
      
      if (nrow(rain_daily) == 0) {
        return("Menor acumulado diário")
      }
      
      min_idx <- which.min(rain_daily$daily_total)
      
      return(
        tagList(
          "Menor acumulado diário",
          tags$br(),
          format(rain_daily$date[min_idx], "%d-%m-%Y")
        )
      )
    }
    
    "Valor mínimo"
  })
  
  # (UPDATED 18.07.2026)
  output$stats_max_value <- renderUI({
    
    df <- stats_sensor_data()
    unit <- get_stats_sensor_unit()
    
    if (nrow(df) == 0) {
      return("—")
    }
    
    sensor_label <- get_stats_sensor_label()
    
    if (grepl("Chuva", sensor_label, ignore.case = TRUE)) {
      
      if (input$stats_period == "day") {
        return(format_stats_value(max(df$value, na.rm = TRUE), unit))
      }
      
      rain_daily <- stats_daily_rain_data()
      
      if (nrow(rain_daily) == 0) {
        return("—")
      }
      
      return(format_stats_value(max(rain_daily$daily_total, na.rm = TRUE), unit))
    }
    
    format_stats_value(max(df$value, na.rm = TRUE), unit)
  })
  
  # (UPDATED 18.07.2026)
  output$stats_max_sub <- renderUI({
    
    sensor_label <- get_stats_sensor_label()
    
    if (grepl("Rajada", sensor_label, ignore.case = TRUE)) {
      return("Rajada máxima")
    }
    
    if (grepl("Chuva", sensor_label, ignore.case = TRUE)) {
      
      if (input$stats_period == "day") {
        return("Maior leitura no dia")
      }
      
      rain_daily <- stats_daily_rain_data()
      
      if (nrow(rain_daily) == 0) {
        return("Maior acumulado diário")
      }
      
      max_idx <- which.max(rain_daily$daily_total)
      
      return(
        tagList(
          "Maior acumulado diário",
          tags$br(),
          format(rain_daily$date[max_idx], "%d-%m-%Y")
        )
      )
    }
    
    "Valor máximo"
  })
  
  # (UPDATED 18.07.2026)
  output$stats_avg_value <- renderUI({
    
    df <- stats_sensor_data()
    unit <- get_stats_sensor_unit()
    
    if (nrow(df) == 0) {
      return("—")
    }
    
    sensor_label <- get_stats_sensor_label()
    
    if (grepl("Chuva", sensor_label, ignore.case = TRUE)) {
      
      if (input$stats_period == "day") {
        return(format_stats_value(mean(df$value, na.rm = TRUE), unit))
      }
      
      rain_daily <- stats_daily_rain_data()
      
      if (nrow(rain_daily) == 0) {
        return("—")
      }
      
      return(format_stats_value(mean(rain_daily$daily_total, na.rm = TRUE), unit))
    }
    
    format_stats_value(mean(df$value, na.rm = TRUE), unit)
  })
  
  # (UPDATED 18.07.2026)
  output$stats_avg_sub <- renderUI({
    
    sensor_label <- get_stats_sensor_label()
    
    if (grepl("Chuva", sensor_label, ignore.case = TRUE)) {
      
      if (input$stats_period == "day") {
        return("Média das leituras")
      }
      
      return("Média diária acumulada")
    }
    
    if (grepl("Vento|Rajada", sensor_label, ignore.case = TRUE)) {
      return("Velocidade média")
    }
    
    "Média"
  })
  
  output$stats_period_value <- renderUI({
    
    period <- stats_period_range()
    
    period$label
  })
  
  output$stats_period_sub <- renderUI({
    
    period <- stats_period_range()
    
    paste0(
      format(period$start_date, "%d-%m-%Y"),
      " até ",
      format(period$end_date, "%d-%m-%Y")
    )
  })
  
  output$stats_empty_message <- renderUI({
    
    df <- stats_sensor_data()
    
    if (nrow(df) > 0) {
      return(NULL)
    }
    
    tags$div(
      style = "margin-top: 1rem; color: #6c757d;",
      icon("circle-info", class = "me-2"),
      "Sem dados disponíveis para este sensor no período selecionado."
    )
  })
  
  
  # (UPDATED 23.06.26) Input event-driven reactive logic for DATE calendar menu ------------
  observeEvent(input$station, {
    
    req(input$station)
    
    # Get real table names from selected station
    station_tables <- get_station_tables(input$station)
    
    # Get available dates from selected station/table(s)
    available_dates <- c()
    
    for (table_name in station_tables) {
      
      dates_df <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT DISTINCT DATE(time) as d
         FROM ", schema, ".", table_name, "
         ORDER BY d"
        )
      )
      
      available_dates <- c(available_dates, as.Date(dates_df$d))
    }
    
    # Protection logic for undated station
    available_dates <- sort(unique(available_dates))
    
    if (length(available_dates) == 0) {
      
      updateDateRangeInput(
        session,
        "selected_date",
        start = NULL,
        end = NULL,
        min = NULL,
        max = NULL
      )
      
      updateSelectInput(
        session,
        "sensor",
        choices = character(0),
        selected = character(0)
      )
      
      return()
    }
    
    # Get latest available date
    latest_date <- max(available_dates, na.rm = TRUE) ## Nao esquecer na.rm = TRUE → remove NA data before any calculation!
    
    # Dynamic input calendar update
    # Default behavior: start and end on the latest available date -> keeps the initial plot limited to the latest available day
    updateDateRangeInput(
      session,
      "selected_date",
      start = latest_date,
      end   = latest_date,
      min   = min(available_dates),
      max   = latest_date
    )
    
    # Input Reactive conditional logic for SENSOR dropdown menu ----------------
    
    # Detect available sensors in selected station/table(s)
    sensor_ids <- character(0)
    
    for (table_name in station_tables) {
      
      sensors_df <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT DISTINCT sensor
         FROM ", schema, ".", table_name, "
         ORDER BY sensor"
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
    # print(names(available_categories))
    
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
    ) ## for each id, if id is in sensor names dictionary, use the defined name, otherwise, show "Sensor" and id number
    
    # Put all dropdown label choices together in one vector
    all_choices <- c(category_choices, standalone_choices)
    
    # Update and populate sensor dropdown menu with categories and missing sensor label choices
    updateSelectInput(
      session,
      "sensor",
      choices = all_choices,
      selected = all_choices[1] ## firts choice selected by default
    )
    
  }, ignoreInit = TRUE) ## OBS. NAO ESQUECER: ignoreInit = TRUE → Run code ONLY when the user changes the input
  
  # (UPDATED 18.07.26) Limit selected date range to avoid querying too much data ----------------
  
  # Maximum number of days allowed in the main chart date range
  max_selected_days <- 14
  
  observeEvent(input$selected_date, {
    
    req(input$selected_date)
    req(!is.na(input$selected_date[1]), !is.na(input$selected_date[2]))
    
    start_date <- as.Date(input$selected_date[1])
    end_date   <- as.Date(input$selected_date[2])
    
    selected_days <- as.integer(end_date - start_date) + 1
    
    if (selected_days > max_selected_days) {
      
      new_end_date <- start_date + lubridate::days(max_selected_days - 1)
      
      updateDateRangeInput(
        session,
        "selected_date",
        start = start_date,
        end   = new_end_date
      )
      
      showNotification(
        paste0(
          "O período máximo permitido para visualização é de ",
          max_selected_days,
          " dias."
        ),
        type = "warning",
        duration = 5
      )
    }
    
  }, ignoreInit = TRUE)
  

  # PREPARING DATA FOR PLOTING -----------------------------------------------

  # (UPDATED 23.06.26) Data pipeline to plotting MAIN charts -----------------------------------------
  
  # Reactive logic to fetch station data by sensor
  sensor_data <- reactive({
    
    req(input$station, input$selected_date)
    req(!is.na(input$selected_date[1]), !is.na(input$selected_date[2]))
    
    # Get real table names from selected station
    station_tables <- get_station_tables(input$station)
    
    # Convert selected date range as Date objects
    start_date <- as.Date(input$selected_date[1])
    end_date   <- as.Date(input$selected_date[2])
    
    # Define date range for main chart plots
    # Add 1 day to end_date to include the whole final day.
    start_time <- as.POSIXct(start_date)
    end_time   <- as.POSIXct(end_date) + lubridate::days(1) ## Takes everything btw 00:00 to 00:00 of the selected day
    
    # Fetch data from selected station/table(s)
    df_list <- list()
    
    for (table_name in station_tables) {
      
      df_table <- dplyr::tbl(
        con,
        DBI::Id(schema = schema, table = table_name)
      ) |>
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
    
    df$time <- df$time - lubridate::hours(3) ## Convert UTC timezone to Brasilia time-zone by substracting 3 hours
    ## OBS. Se nao funcionar metodo manual, tentar: df$time <- lubridate::with_tz(df$time, "America/Sao_Paulo")
    
    df
  })

  # (UPDATED 23.06.26) Data pipeline to ploting KPI cards -------------------------------------------
  
  # Function to get latest sensor data from a specific table
  get_latest_sensor_data <- function(table_name, sensor_id) {
    
    df <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT sensor, time, value
       FROM ", schema, ".", table_name, "
       WHERE sensor = ", sensor_id, "
       ORDER BY time DESC
       LIMIT 1"
      )
    )
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    # Convert UTC to Brasília time
    df$time <- df$time - lubridate::hours(3)
    
    df[1, ]
  }
  
  # Function to get all data from the latest available day for one or more sensors
  get_kpi_day_data <- function(table_name, sensor_ids) {
    
    sensor_ids <- as.integer(sensor_ids)
    
    # Find latest timestamp available for these sensors
    latest_df <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT MAX(time) AS max_time
       FROM ", schema, ".", table_name, "
       WHERE sensor IN (", paste(sensor_ids, collapse = ", "), ")"
      )
    )
    
    if (nrow(latest_df) == 0 || is.na(latest_df$max_time[1])) {
      return(data.frame(
        sensor = integer(),
        time = as.POSIXct(character()),
        value = numeric()
      ))
    }
    
    latest_day <- as.Date(latest_df$max_time[1])
    
    start_time <- as.POSIXct(latest_day)
    end_time   <- start_time + lubridate::days(1)
    
    # Fetch all records from the latest available day
    df <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT sensor, time, value
       FROM ", schema, ".", table_name, "
       WHERE sensor IN (", paste(sensor_ids, collapse = ", "), ")
       AND time >= '", start_time, "'
       AND time < '", end_time, "'
       ORDER BY sensor, time"
      )
    )
    
    if (nrow(df) == 0) {
      return(df)
    }
    
    # Convert UTC to Brasília time
    df$time <- df$time - lubridate::hours(3)
    
    df
  }
  
  # Helper to get the latest value from a KPI dataframe
  get_latest_value <- function(df) {
    
    if (nrow(df) == 0) {
      return(NA_real_)
    }
    
    df <- df[order(df$time, decreasing = TRUE), ]
    
    df$value[1]
  }
  
  # Helper to get the latest timestamp from a KPI dataframe
  get_latest_time <- function(df) {
    
    if (nrow(df) == 0) {
      return("Sem dados")
    }
    
    format(max(df$time, na.rm = TRUE), "%d-%m-%Y %H:%M")
  }
  
  # Helper to format KPI values
  format_kpi_value <- function(value, unit, digits = 1) {
    
    if (is.na(value)) {
      return("Sem dados")
    }
    
    value <- round(value, digits)
    
    paste0(value, " ", unit)
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
  
  # (UPDATED 23.06.23) Render selected station heading above KPI cards
  output$selected_station_heading <- renderUI({
    
    req(input$station)
    
    tags$div(
      class = "station-heading",
      tags$span(
        class = "station-heading-icon",
        icon("location-dot")
      ),
      tags$h3(station_nm())
    )
  })

  # (UPDATED 23.06.26) Render KPI charts ----------------------------------

  # Temperature KPI Card
  output$temp_value <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      df <- get_kpi_day_data("tb_estacao_3", 8)
    } else {
      df <- get_kpi_day_data(input$station, 8)
    }
    
    value <- get_latest_value(df)
    
    tags$div(
      class = "kpi-value",
      format_kpi_value(value, "°C")
    )
  })
  
  # Rain KPI Card
  output$rain_value <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      df <- get_kpi_day_data("tb_estacao_3", 35)
    } else {
      df <- get_kpi_day_data(input$station, 35)
    }
    
    value <- get_latest_value(df)
    
    tags$div(
      class = "kpi-value",
      format_kpi_value(value, "mm")
    )
  })
  
  # Third KPI title: Pressure for Merajuba, Air Quality for Mare
  output$third_kpi_title <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      tags$span(
        icon("smog", class = "me-2"),
        "Qualidade do Ar"
      )
    } else {
      tags$span(
        icon("gauge-high", class = "me-2"),
        "Pressão"
      )
    }
  })
  
  # Third KPI value: Pressure for Merajuba, Air Quality for Mare
  output$third_kpi_value <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      
      pm25_data <- get_latest_sensor_data("tb_estacao_4", 71)
      co_data   <- get_latest_sensor_data("tb_estacao_4", 75)
      
      pm25_value <- if (is.null(pm25_data)) NA_real_ else pm25_data$value
      co_value   <- if (is.null(co_data)) NA_real_ else co_data$value
      
      pm25_time <- if (is.null(pm25_data)) {
        "Sem dados"
      } else {
        format(pm25_data$time, "%d-%m-%Y %H:%M")
      }
      
      co_time <- if (is.null(co_data)) {
        "Sem dados"
      } else {
        format(co_data$time, "%d-%m-%Y %H:%M")
      }
      
      tags$div(
        class = "kpi-value",
        style = "font-size: 1.25rem; line-height: 1.15;",
        
        tags$div(paste0("PM2.5: ", round(pm25_value, 1), " µg/m³")),
        tags$div(
          style = "font-size: 0.8rem; font-weight: 400; color: #6c757d; margin-bottom: 0.35rem;",
          paste0("Atualizado: ", pm25_time)
        ),
        
        tags$div(paste0("CO: ", round(co_value, 1), " ppm")),
        tags$div(
          style = "font-size: 0.8rem; font-weight: 400; color: #6c757d;",
          paste0("Atualizado: ", co_time)
        )
      )
      
    } else {
      
      df <- get_kpi_day_data(input$station, 23)
      value <- get_latest_value(df)
      
      tags$div(
        class = "kpi-value",
        format_kpi_value(value, "hPa")
      )
    }
  })
  
  # Wind KPI Card
  output$wind_value <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      df <- get_kpi_day_data("tb_estacao_3", c(36, 347))
    } else {
      df <- get_kpi_day_data(input$station, c(36, 347))
    }
    
    value <- get_latest_value(df)
    
    tags$div(
      class = "kpi-value",
      format_kpi_value(value, "km/h")
    )
  })

  # (UPDATED 23.06.26) Formatting KPI CARDS subtitles -------------------------
  
  # Temperature
  output$temp_sub <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      df <- get_kpi_day_data("tb_estacao_3", 8)
    } else {
      df <- get_kpi_day_data(input$station, 8)
    }
    
    if (nrow(df) == 0) {
      return(
        tagList(
          "Sem dados disponíveis",
          tags$br(),
          tags$br(),
          station_nm()
        )
      )
    }
    
    min_value <- min(df$value, na.rm = TRUE)
    max_value <- max(df$value, na.rm = TRUE)
    
    tagList(
      paste0("Mín.: ", format_kpi_value(min_value, "°C")),
      tags$br(),
      paste0("Máx.: ", format_kpi_value(max_value, "°C")),
      tags$br(),
      paste0("Atualizado: ", get_latest_time(df)),
      tags$br(),
      tags$br()
    )
  })
  
  # Rain
  output$rain_sub <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      df <- get_kpi_day_data("tb_estacao_3", 35)
    } else {
      df <- get_kpi_day_data(input$station, 35)
    }
    
    if (nrow(df) == 0) {
      return(
        tagList(
          "Sem dados disponíveis",
          tags$br(),
          tags$br(),
          station_nm()
        )
      )
    }
    
    rain_total <- sum(df$value, na.rm = TRUE)
    
    tagList(
      paste0("Chuva acumulada: ", format_kpi_value(rain_total, "mm")),
      tags$br(),
      paste0("Atualizado: ", get_latest_time(df)),
      tags$br(),
      tags$br()
    )
  })
  
  # Third KPI subtitle: Pressure for Merajuba, Air Quality for Mare
  output$third_kpi_sub <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      
      tagList()
      
    } else {
      
      df <- get_kpi_day_data(input$station, 23)
      
      if (nrow(df) == 0) {
        return(
          tagList(
            "Sem dados disponíveis",
            tags$br(),
            tags$br(),
            station_nm()
          )
        )
      }
      
      min_value <- min(df$value, na.rm = TRUE)
      max_value <- max(df$value, na.rm = TRUE)
      
      tagList(
        paste0("Mín.: ", format_kpi_value(min_value, "hPa")),
        tags$br(),
        paste0("Máx.: ", format_kpi_value(max_value, "hPa")),
        tags$br(),
        paste0("Atualizado: ", get_latest_time(df)),
        tags$br(),
        tags$br()
      )
    }
  })
  
  # Wind
  output$wind_sub <- renderUI({
    
    req(input$station)
    
    if (input$station == "mare") {
      wind_df <- get_kpi_day_data("tb_estacao_3", c(36, 347))
      gust_df <- get_kpi_day_data("tb_estacao_3", c(37, 348))
    } else {
      wind_df <- get_kpi_day_data(input$station, c(36, 347))
      gust_df <- get_kpi_day_data(input$station, c(37, 348))
    }
    
    if (nrow(wind_df) == 0) {
      return(
        tagList(
          "Sem dados disponíveis",
          tags$br(),
          tags$br(),
          station_nm()
        )
      )
    }
    
    gust_max <- if (nrow(gust_df) == 0) {
      NA_real_
    } else {
      max(gust_df$value, na.rm = TRUE)
    }
    
    tagList(
      paste0("Rajada máx.: ", format_kpi_value(gust_max, "km/h")),
      tags$br(),
      paste0("Atualizado: ", get_latest_time(wind_df)),
      tags$br(),
      tags$br()
    )
  })
  
  # (UPDATED 23.06.26) Render MAIN charts -----------------------------------
  
  output$sensor_plot <- renderPlotly({
    
    # Input reactive conditional logic for outputs
    req(input$sensor)
    
    # Get selected plot data prepared for both chart and CSV download
    plot_data <- selected_sensor_data()
    
    df <- plot_data$df
    plot_title <- plot_data$plot_title
    unit_label <- plot_data$unit_label
    
    # Show friendly empty plot when selected sensor has no data in selected period
    if (nrow(df) == 0) {
      
      empty_title <- paste0(station_nm(), "<br>", plot_title)
      
      return(
        plotly::plot_ly() |>
          plotly::layout(
            title = list(
              text = empty_title,
              font = list(size = 11),
              x = 0.5,
              xanchor = "center"
            ),
            xaxis = list(
              title = "",
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
            ),
            yaxis = list(
              title = "",
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
            ),
            annotations = list(
              list(
                text = "Sem dados disponíveis<br>para este indicador<br>no período selecionado.",
                x = 0.5,
                y = 0.55,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                align = "center",
                font = list(
                  size = 12,
                  color = "#6c757d"
                )
              )
            ),
            margin = list(l = 8, r = 8, t = 80, b = 35)
          )
      )
    }
    
    # Get active sensor metadata from reactive expression
    meta <- sensor_meta()
    
    # Get selected sensor
    selected <- input$sensor
    
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
        paste("Sensor", sid_char) ## use "Sensor" as label
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
          text = ~paste0(
            "Hora: ", format(time, "%H:%M"), "h",
            "<br>Valor: ", value, " ", unit_label
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
        title = list(
          text = paste0(station_nm(), "<br>", plot_title),
          font = list(size = 13),
          x = 0.5,
          xanchor = "center"
        ),
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
          y = -0.25 ## place legend below and to the left of plotting area
        ),
        
        margin = list(l = 70, r = 20, t = 80, b = 80) ## b=80 to add more space; t=80 to fit two-line title
      )
  })
  
  # (CREATED 23.06.26) DONLOAD DATA PIPELINE ---------------------------------
  
  # Prepare selected data for plot and CSV download
  selected_sensor_data <- reactive({
    
    req(input$sensor)
    
    df_all <- sensor_data() |>
      dplyr::distinct(sensor, time, .keep_all = TRUE)
    
    #req(nrow(df_all) > 0) ##UPDATED
    ## Removing this line allows the code to continue even when `df_all` is empty. 
    ##This way,`renderPlotly()` can reach our `if (nrow(df) == 0)` check and display a user-friendly message instead of appearing blank.
    
    meta <- sensor_meta()
    selected <- input$sensor
    
    if (startsWith(selected, "cat_")) {
      
      category_name <- sub("cat_", "", selected)
      
      sensor_ids <- meta$categories[[category_name]]$ids
      unit_label <- meta$categories[[category_name]]$unit
      
      df <- df_all |>
        dplyr::filter(sensor %in% as.integer(sensor_ids))
      
      plot_title <- category_name
      
    } else {
      
      df <- df_all |>
        dplyr::filter(sensor == as.integer(selected))
      
      plot_title <- if (selected %in% names(meta$labels)) {
        meta$labels[selected]
      } else {
        paste("Sensor", selected)
      }
      
      unit_label <- ""
    }
    
    list(
      df = df,
      plot_title = plot_title,
      unit_label = unit_label
    )
  }) 
  
  # Download btn handler -----------------------------------------
  
  # Download CSV with the same data shown in the main plot
  output$download_sensor_csv <- downloadHandler(
    
    filename = function() {
      
      start_date <- as.Date(input$selected_date[1])
      end_date   <- as.Date(input$selected_date[2])
      
      paste0(
        "dados_",
        input$station,
        "_",
        input$sensor,
        "_",
        start_date,
        "_",
        end_date,
        ".csv"
      )
    },
    
    # (UPDATED 18.07.26)
    content = function(file) {
      
      plot_data <- selected_sensor_data()
      meta <- sensor_meta()
      
      df_download <- plot_data$df |>
        dplyr::mutate(
          estacao = station_nm(),
          indicador = plot_data$plot_title,
          variavel_climatica = dplyr::case_when(
            as.character(sensor) %in% names(meta$labels) ~
              unname(meta$labels[as.character(sensor)]),
            TRUE ~ paste("Sensor", sensor)
          )
        ) |>
        dplyr::select(
          estacao,
          indicador,
          variavel_climatica,
          time,
          value
        )
      
      write.csv(
        df_download,
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
  
  # Download monthly statistics report as PDF -------------------------------
  
  output$download_stats_monthly_report <- downloadHandler(
    
    filename = function() {
      
      req(input$stats_station)
      req(input$stats_base_date)
      
      month_ref <- lubridate::floor_date(
        as.Date(input$stats_base_date),
        unit = "month"
      )
      
      safe_station <- gsub(
        "[^A-Za-z0-9_-]+",
        "_",
        input$stats_station
      )
      
      paste0(
        "relatorio_mensal_",
        safe_station,
        "_",
        format(month_ref, "%Y_%m"),
        ".pdf"
      )
    },
    
    contentType = "application/pdf",
    
    content = function(file) {
      
      req(input$stats_station)
      req(input$stats_base_date)
      
      # Show a discreet message while the report is being generated
      report_notification_id <- showNotification(
        ui = "Preparando o relatório mensal para download...",
        type = "message",
        duration = NULL,
        closeButton = FALSE,
        session = session
      )
      
      # Remove the notification when the download finishes or fails
      on.exit(
        removeNotification(
          id = report_notification_id,
          session = session
        ),
        add = TRUE
      )
      
      message(
        "Iniciando geração do relatório mensal da estação..."
      )
      
      report_data <- stats_month_report_data()
      
      template_path <- file.path(
        "reports",
        "monthly_stats_report.qmd"
      )
      
      if (!file.exists(template_path)) {
        stop(
          paste0(
            "Template Quarto não encontrado: ",
            template_path
          )
        )
      }
      
      # Create a temporary directory for this report
      tmp_dir <- tempfile(
        "monthly_station_report_"
      )
      
      dir.create(
        tmp_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )
      
      # Remove temporary files after finishing the download
      on.exit(
        unlink(
          tmp_dir,
          recursive = TRUE,
          force = TRUE
        ),
        add = TRUE
      )
      
      # Copy the Quarto template
      tmp_qmd <- file.path(
        tmp_dir,
        "monthly_stats_report.qmd"
      )
      
      template_copied <- file.copy(
        from = template_path,
        to = tmp_qmd,
        overwrite = TRUE
      )
      
      if (!isTRUE(template_copied)) {
        stop(
          "Não foi possível copiar o template Quarto."
        )
      }
      
      station_name <- if (
        input$stats_station %in% names(station_labels)
      ) {
        unname(
          station_labels[input$stats_station]
        )
      } else {
        input$stats_station
      }
      
      # Prepare all report information as an R object
      report_payload <- list(
        station_name = station_name,
        
        period_label = report_data$period_label,
        
        generated_at = format(
          Sys.time(),
          "%d-%m-%Y %H:%M"
        ),
        
        sensor_summary = report_data$sensor_summary,
        
        sensor_details = report_data$sensor_details,
        
        rain_summary = report_data$rain_summary
      )
      
      report_data_file <- file.path(
        tmp_dir,
        "report_data.rds"
      )
      
      saveRDS(
        object = report_payload,
        file = report_data_file
      )
      
      if (!file.exists(report_data_file)) {
        stop(
          "Não foi possível criar o arquivo temporário de dados."
        )
      }
      
      output_pdf_name <- "relatorio_mensal_estacao.pdf"
      
      generated_pdf <- file.path(
        tmp_dir,
        output_pdf_name
      )
      
      message(
        "Diretório temporário: ",
        normalizePath(
          tmp_dir,
          winslash = "/",
          mustWork = TRUE
        )
      )
      
      message(
        "Renderizando o documento Quarto..."
      )
      
      tryCatch(
        {
          quarto::quarto_render(
            input = normalizePath(
              tmp_qmd,
              winslash = "/",
              mustWork = TRUE
            ),
            output_format = "pdf",
            output_file = output_pdf_name,
            execute_params = list(
              data_file = normalizePath(
                report_data_file,
                winslash = "/",
                mustWork = TRUE
              )
            ),
            execute_dir = normalizePath(
              tmp_dir,
              winslash = "/",
              mustWork = TRUE
            ),
            quiet = FALSE
          )
        },
        error = function(e) {
          
          message(
            "ERRO AO GERAR O RELATÓRIO PDF:"
          )
          
          message(
            conditionMessage(e)
          )
          
          stop(e)
        }
      )
      
      if (!file.exists(generated_pdf)) {
        stop(
          paste0(
            "O Quarto terminou, mas o PDF não foi encontrado em: ",
            generated_pdf
          )
        )
      }
      
      pdf_copied <- file.copy(
        from = generated_pdf,
        to = file,
        overwrite = TRUE
      )
      
      if (!isTRUE(pdf_copied)) {
        stop(
          "Não foi possível copiar o PDF para o download."
        )
      }
      
      message(
        "Relatório mensal da estação gerado com sucesso."
      )
    }
  )
}
