# Load requeired libraries
library(shiny)
library(bslib)
library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)
library(plotly)

# UI
ui <- page_sidebar(
  title = "Painel Estações – v0.2",
  
  sidebar = sidebar(
    # Card for menu ctrls
    card(
      card_header("Selecione uma opção"),
      
      # Dropdown menus
      selectInput(
        inputId = "station",
        label   = "Selecione uma estação",
        choices = NULL
      ),
      
      dateInput(
        inputId = "selected_date",
        label   = "Selecione uma data",
        value   = NULL  ## I'll probably need here Sys.Date() val when the app goes to production
      ),
      
      selectInput(
        inputId = "sensor",
        label   = "Selecione o indicador meteorológico",
        choices = NULL
      )
    )
  ),
  
  card(
    card_header("Dados das últimas 24 horas em relação à data selecionada"),
    plotlyOutput("sensor_plot", height = "500px") # Placeholder card for the rendered plot
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Sensor label dictionary (to be shown in the dropdownmenu) - Is there is a better implementation of this?
  sensor_labels <- c(
    "8"   = "Temperatura (°C)",
    "11"  = "Umidade (%)",
    "18"  = "Luminosidade (lux)",
    "19"  = "UV (uv)",
    "22"  = "Ponto de Orvalho (°C)",
    "23"  = "Pressão (hPa)", 
    "27"  = "Sensação térmica (°C)",
    "28"  = "Delta T (°C)", 
    "34"  = "Nivel de líquidos (mca)", 
    "35"  = "Chuva (mm)",
    "36"  = "Vento (km/h)",
    "37"  = "Rajada (km/h)",
    "347" = "Vento (km/h)",
    "348" = "Rajada (km/h)"
  ) ## OBS. Alguns sensores (48, 49) não estão na lista que o Rapha passou. Perguntar
  
  # Sensor categories dictionary - to costumize plots (color, chart type)
  sensor_config <- list(
    
    # Temperature related
    "8"  = list(type = "scatter", mode = "lines", color = "#e63946"), ## T°
    "27" = list(type = "scatter", mode = "lines", color = "#f4a261"), ## Sensação térmica
    
    # Delta T
    "28" = list(type = "scatter", mode = "lines", color = "#e5e619"), ## Delta T
    "22" = list(type = "scatter", mode = "lines", color = "#228b22"), ## Ponto de Orvalo (WTF is that??)
    
    # Pressure
    "23" = list(type = "scatter", mode = "lines", color = "#800080"), ## Pressão (atmosferica??)
    
    # Humidity
    "11" = list(type = "scatter", mode = "lines", color = "#457b9d"), ## Umidade
    
    # Rain
    "35" = list(type = "bar",     mode = NULL,     color = "#2a9d8f"), ## Chuva (acumulada?)
    
    # Liquid level
    "34" = list(type = "bar",     mode = NULL,     color = "#00009c"), ## Nivel de liquidos
    
    # Wind (speed I think)
    "36"  = list(type = "scatter", mode = "lines", color = "#264653"), ## Vento 
    "347" = list(type = "scatter", mode = "lines", color = "#264653"), ## Vento - pq 2 cod ?
    
    # Wind gust
    "37"  = list(type = "scatter", mode = "lines", color = "#19b2e6"), ## Rajadas de vento
    "348" = list(type = "scatter", mode = "lines", color = "#19b2e6"), ## Rajadas de vento - de novo pq 2 cod ?
    
    # Luminosity
    "18" = list(type = "scatter", mode = "lines", color = "#ffbe0b"), ## Chuva
    
    # UV (Index?)
    "19" = list(type = "scatter", mode = "lines", color = "#ff006e"), ## Uv
    
    # Missinmg codes
    "default" = list(type = "scatter", mode = "lines", color = "#333333") # see and plot those "unknown" sensors codes 
  )
  
  # Categories definition (for dropdown and plots rendering)
  categories <- list(
    Temperatura = list(ids=c("8", "27"), unit="°C"),
    "Delta T" = list(ids=c("28", "22"), unit="°C"),
    Umidade = list(ids=c("11"), unit="%"),
    Pressão = list(ids=c("23"), unit="hPa"),
    Chuva = list(ids=c("35"), unit="mm"),
    "Nivel de Liquidos" = list(ids=c("34"), unit="mca"),
    Vento = list(ids=c("36", "347", "37", "348"), unit="Km/h"),
    Luminosidade = list(ids=c("18"), unit="lux"),
    UV = list(ids=c("19"), unit="uv")
  )
  
  # DB Connection (via DuckDB)
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    file.path("data", "estacoes.duckdb")
  )
  
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con, shutdown = TRUE) ## Close DB conn
  })
  
  # Populate stations dropdown
  station_names <- DBI::dbListTables(con) ## List ALL stations (tables) - Era o que o Rapha queria? Perguntar
  
  #  Update sensor selectInput
  updateSelectInput(
    session,
    "station",
    choices  = station_names, ## adds listed stations to the dropdown  
    selected = station_names[1] ## choose the fisrt table as default one
  )
  
  # Update sensor category dropdown when station changes
  observeEvent(input$station, {
    
    # Get available dates
    dates_df <- DBI::dbGetQuery(
      con,
      paste0("SELECT DISTINCT DATE(time) as d FROM ", input$station, " ORDER BY d") ## DB query to get available dates and show them in the calendar input
    )
    
    # Convert to date object
    available_dates <- as.Date(dates_df$d) 
    
    # Set default to latest date
    latest_date <- max(available_dates, na.rm = TRUE) ## Não esquecer na.rm = TRUE -> remove NA data before any calc!
    
    # Dynamic input calendar update
    updateDateInput(
      session,
      "selected_date",
      value = latest_date,
      min   = min(available_dates),
      max   = latest_date
    )
    
    # Input conditional logic
    req(input$station)
    
    # Looking for available sensors in each station - Foi isso que o rapha pediu?
    sensors_df <- DBI::dbGetQuery(
      con,
      paste0("SELECT DISTINCT sensor FROM ", input$station, " ORDER BY sensor") ## DB query to get available sensors per station
    )
    
    # Convert sensor numeric values as char
    sensor_ids <- as.character(sensors_df$sensor) ## OBS. Estava dando errado porque nos labels armazenei esses dados como string
    
    # Remove unwanted sensors (from the station connectivity)
    sensor_ids <- sensor_ids[!sensor_ids %in% c("1", "25", "26")] ## Wifi, Bluetooth and Mobile data
    
    # Create list with all sensors that belong to a particular category to populate teh dropdown menu
    categorized_ids <- unique(unlist(categories)) ## OBS> Pensar numa forma de simplificr ainda mais
    
    # Create list of all sensors that not belogns to in any category (missing sensors)
    standalone_ids <- sensor_ids[!sensor_ids %in% categorized_ids] ## tem sensor codes que não estão na lista que o rapha passou!
    
    # Category choices for dropdown
    category_choices <- setNames(
      paste0("cat_", names(categories)),
      names(categories)
    )
    
    # Standalone choices (missing sensors) for dropdown
    standalone_choices <- setNames(
      standalone_ids,
      sapply(standalone_ids, function(id) {
        if (id %in% names(sensor_labels)) {
          sensor_labels[id]
        } else {
          paste("Sensor", id)
        }
      })
    )
    
    # GEt all dropdown choices together in one vector
    all_choices <- c(category_choices, standalone_choices)
    
    # Dinamyc sensor (climate var) input update
    updateSelectInput(
      session,
      "sensor",
      choices  = all_choices,
      selected = all_choices[1] ## firts choice selected by default
    )
    
  }, ignoreInit = TRUE) ## OBS. observeEvent - apenas executa o bloco quando selecioanr outra estação - Usar para evitar rodar quando iniciar o app
  
  
  # Fetch last available 24h data (for ALL sensors) - preparing data for plots
  sensor_data <- reactive({
    
    # Conditional input logic
    req(input$station, input$selected_date)
    
    # Get all available dates 
    dates_df <- DBI::dbGetQuery(
      con,
      paste0("SELECT DISTINCT DATE(time) as d FROM ", input$station)
    )
    
    # Convert available dates as date
    available_dates <- as.Date(dates_df$d)
    
    # Covert selected dates as date
    selected_date <- as.Date(input$selected_date) 
    
    # If selected date not available chage to nearest available date
    if (!selected_date %in% available_dates) {
      selected_date <- max(available_dates[available_dates <= selected_date]) ## OBS. Revisar essa lógica. Acho que em algum momento vai retornar um vetor vazio
    }
    
    # Find last measurement data of that selected day - OBS. Revisar, não tenho ctza se fara sentido em produção
    last_time <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT MAX(time) AS rt FROM ", input$station,
        " WHERE DATE(time) = DATE('", selected_date, "')"
      )
    )$rt[1]
    
    # If no data that day return empty
    if (is.null(last_time) || is.na(last_time)) {
      return(dplyr::tibble(
        sensor = integer(),
        time   = as.POSIXct(character()),
        value  = numeric()
      ))
    }
    
    # Calculate the start of the 24-hour window
    start_time <- last_time - lubridate::days(1) ## exactly 24 hours - OBS. Rever com Rapha se procede 
    
    # "Lazy" DB reference 
    dplyr::tbl(con, input$station) |>
      dplyr::filter(
        time >= start_time,
        time <= last_time
      ) |>
      dplyr::select(sensor, time, value) |>
      dplyr::collect() ## actual DB query 
  })
  
  
  # Render plot in the UI
  output$sensor_plot <- renderPlotly({
    
    # Conditional input Logic 
    req(input$sensor)
    
    # Conditional logic to avoid empty plot when no data
    df_all <- sensor_data()
    req(nrow(df_all) > 0)
    
    # Get selected snensor input
    selected <- input$sensor
    
    # If CATEGORY selected 
    if (startsWith(selected, "cat_")) {
      
      # Get category name
      category_name <- sub("cat_", "", selected) ## remove category prefix
      
      # GEt sensors and units from the category
      sensor_ids <- categories[[category_name]]$ids ## take cat names
      unit_label <- categories[[category_name]]$unit ## take cat units
      
      # Only picks sensors from the selected category within the 24-hour window
      df <- df_all |>
        dplyr::filter(sensor %in% as.integer(sensor_ids)) ## OBS. Esqueci de novo converter num para integer!
      
      # Define plot title as cantegory name
      plot_title <- category_name
      
      # ELSE block (when missing sensors are selected)
    } else {
      
      # Standalone sensorselected
      df <- df_all |>
        dplyr::filter(sensor == as.integer(selected))
      
      # Define title dynamically (using sensor labels dictionary)
      plot_title <- if (selected %in% names(sensor_labels)) {
        sensor_labels[selected]
      } else {
        paste("Sensor", selected) ## for missing sensors
      }
      unit_label <- "" ## no units for missing values (for now)
    }
    
    # Placeholder plot
    p <- plotly::plot_ly()
    
    # Loop for sensor plotting
    for (sid in unique(df$sensor)) {
      
      sid_char <- as.character(sid) ## to match string format of dictionary labels
      config <- sensor_config[[sid_char]] ## get plot config from dictionary (sensor_config) - chart type, color, etc.
      if (is.null(config)) config <- sensor_config[["default"]] ## default plot config for missing/new sensors
      
      # Conditional logic for sensor naming
      sensor_name <- if (sid_char %in% names(sensor_labels)) {
        sensor_labels[sid_char] ## use label from dictionary (sensor_labels)
      } else {
        paste("Sensor", sid_char) ## use "Sensor" 
      }
      
      # Plot the chart 
      p <- p |>
        ## OBS. add_trace() method -> permite plot different chart types on the same graph
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
          hoverinfo = "text"
        )
    }
    
    # Add plot bands when UV selected 
    # Apply UV shading if standalone UV sensor selected OR UV category selected - OBS. REVER, acho que não preciso
    if (selected == "19" || selected == "cat_UV") {
      
      p <- p |>
        layout(
          shapes = list(
            list(type="rect",xref="paper",x0=0,x1=1,y0=0,y1=3,
                 fillcolor="green",opacity=0.1,line=list(width=0)),
            list(type="rect",xref="paper",x0=0,x1=1,y0=3,y1=6,
                 fillcolor="yellow",opacity=0.1,line=list(width=0)),
            list(type="rect",xref="paper",x0=0,x1=1,y0=6,y1=8,
                 fillcolor="orange",opacity=0.1,line=list(width=0)),
            list(type="rect",xref="paper",x0=0,x1=1,y0=8,y1=20,
                 fillcolor="red",opacity=0.1,line=list(width=0))
          )
        )
    }
    
    # Final graph layout
    p |>
      layout(
        title = paste("Station:", input$station, "|", plot_title),
        xaxis = list(
          title = "Hora",
          tickformat = "%H:%M\n%b %d"
        ),
        yaxis = list(
          title = "", ## Remove title for now
          automargin = TRUE
        ),
        margin = list(l = 70, r = 20, t = 60, b = 60)
      )
    
  })
  
}

shinyApp(ui, server)