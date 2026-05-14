# Libraries
library(shiny)
library(bslib) ## dashboard UI
library(DBI) ## R Database Interface
# library(duckdb) ## Embedded database - optimized for data analysis (like SQLite but better)
library(RPostgres)
library(dplyr) ## df manipulation
library(lubridate) ## time manipulation
library(plotly) ## interactive charts
library(leaflet) ## interactive maps
library(fontawesome) ## i tooltip icon


# Fix rendering logo issue
addResourcePath("static", "www") ## will need this on production?

# ----------------------------------------------------------
# UI
# ----------------------------------------------------------

ui <- bslib::page_sidebar(
  title = tags$div(
    style = "display:flex; align-items:center; gap:0.75rem;",
    tags$a(
      href = "https://climaesaude.icict.fiocruz.br/",
      target = "_blank",
      tags$img(
        src = "static/logo_ocs.png",
        style = "height:54px;"
      )
    ),
    tags$span(
      "Painel de Estações Meteorológicas",
      style = "font-weight:600; font-size:1.1rem;"
    ),
    bslib::tooltip(
      tags$span(icon("info-circle")),
      "Este painel interativo foi desenvolvido para a visualização e o monitoramento de dados climáticos provenientes de estações meteorológicas localizadas no Brasil.",
      placement = "top"
    )
  ),

  # ui theme
  theme = bslib::bs_theme(
    version = 5, ## boostrap v5
    bootswatch = "flatly", ## boostrap theme
    base_font = bslib::font_google("Inter"), ## Google font

    # Custom header colors
    "navbar-bg" = "#bbd0c9",
    "navbar-fg" = "black"
  ),

  # CSS for ui style
  ## tags$head to create <head> HTML tag
  ## tags$style to create <style> HTML tag -> uses HTML CSS class (.card, .kpi-grid, etc.)
  ## @media rules for responsive design - DON't FORGET to always add tem!
  tags$head(
    tags$style(HTML("
      /* Custom CSS for station input */
      #station + .selectize-control .selectize-input {
      font-size: 0.85rem;
      display: flex; /* flexivel layout */ 
      align-items: center; 
      justify-content: space-between; 
      }

    #station + .selectize-control .selectize-input .item {
      flex: 1; /* text fills all input box - fix blank space at the end */
      }

      /* CSS for station dropdown style */
      #station + .selectize-control .selectize-dropdown {
        font-size: 0.85rem;
      }
      
      /* CSS for station input arrow style */
      #station + .selectize-control .selectize-input::after {
      right: 6px !important;
      margin: 0 !important;
      top: 12px;
      transform: none;
      }
      
      /* CSS for cards */
      .card {
        border-radius: 16px;
        box-shadow: 0 8px 24px rgba(0,0,0,.06);
        border: 1px solid rgba(0,0,0,.06);
      }
      
      .sidebar-card .card-body {
      padding: 0.75rem;
      }

      /* Sidebar spacing */
      .bslib-sidebar-layout .sidebar {
        padding-top: 0.75rem;
      }
      
      .card-sub {
        font-size: 0.85rem;
        color: rgba(0, 0, 0, 0.64);
        margin-bottom: 0.5rem;
      }

      .card-sub small {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        cursor: pointer;
      }

      .card-sub i {
        transition: transform 0.3s ease, color 0.3s ease;
      }

      .card-sub small:hover i {
        transform: scale(1.2);
        color: #0d6efd;
       }

      .card-sub a {
        color: inherit;
        text-decoration: none;
      }

      .card-sub a:hover {
         text-decoration: underline;
      }
      
      /* Allow station dropdown to open outside the card */
      .sidebar-card {
        overflow: visible !important;
      }

      .sidebar-card .card-body {
        overflow: visible !important;
      }

      /* Keep station dropdown above the sidebar card */
      #station + .selectize-control {
        z-index: 1000;
      }

      #station + .selectize-control .selectize-dropdown {
        z-index: 9999 !important;
      }

      /* KPI grid */
      .kpi-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 0.75rem;
      }
      @media (max-width: 992px) {
        .kpi-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      }
      @media (max-width: 576px) {
        .kpi-grid { grid-template-columns: 1fr; }
      }

      /* KPI title row: title + info icon */
      .kpi-title {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.5rem;
      }
      .kpi-value {
        font-size: 1.6rem;
        font-weight: 700;
        line-height: 1.1;
        margin-top: 0.25rem;
      }
      .kpi-sub {
        font-size: 0.85rem;
        color: rgba(0,0,0,.55);
        margin-bottom: 0.5rem;
      }

      /* Placeholder area for mini charts */
      .kpi-chart {
        height: 90px;
        border-radius: 12px;
        background: rgba(0,0,0,.03);
        border: 1px dashed rgba(0,0,0,.15);
        display: flex;
        align-items: center;
        justify-content: center;
        color: rgba(0,0,0,.45);
        font-size: 0.9rem;
      }

      /* Animations for icons */
      .kpi-title {
      display: inline-flex;
      align-items: center;
      gap: 0.5rem;
    }

    .kpi-title i {
      transition: transform 0.3s ease, color 0.3s ease;
    }

    .kpi-title:hover i {
      transform: scale(1.2);
      color: #0d6efd;
    }
    "
    ))
  ),

  # SIDEBAR --------------------
  sidebar = bslib::sidebar(
    
    # Station dropdown input selector
    bslib::card(
      bslib::card_header("Estação/Local"),
      # Stations (locations) selector dropdown
      selectInput(
        inputId = "station",
        label = NULL,
        choices = NULL ## keep empty bc this will populate dinamycaly
      )
      
    ),
    
    # Info section ----------------
    bslib::card(
      full_screen = TRUE,
      # Card header
      bslib::card_header("Sobre a Estação"),
      # Card body
      bslib::card_body(
        tags$small(textOutput("station_info")), ## Render station info text dynamically
        tags$div(
          style = "display:flex; flex-direction:column; gap:.5rem;",
          leafletOutput("station_map", height = 200) ## Render station map dynamically
        )
      )
    ),

    # Contact section ----------------
    bslib::card(
      bslib::card_header("Contato"),
      tags$div(
        style = "display:flex; flex-direction:column; gap:.5rem;",
        tags$div(
          class = "card-sub",
          tags$small(
            icon("envelope"),
            tags$a(
              href = "mailto:obs.climaesaude@fiocruz.br",
              "obs.climaesaude@fiocruz.br"
            )
          )
        )
      )
    )
  ),

  # MAIN --------------------
  # Layout grid
  bslib::layout_column_wrap(
    width = 1, ## each KPI card item takes 1 grid unit
    heights_equal = "row", ## more card height

    # KPI DIV (for 4 cards) ---------------
    tags$div(
      class = "kpi-grid", ## CSS class defined previously

      # Card 1 - Temperature
      # Main card
      bslib::card(
        # HEader card
        bslib::card_header(
          tags$div(
            class = "kpi-title",
            tags$span(
              icon("thermometer-half", class = "me-2"),
              "Temperatura"
            ),
            bslib::tooltip(
              tags$span(icon("info-circle")),
              "Dados mais recentes conforme disponibilidade no banco de dados.",
              placement = "top"
            )
          )
        ),

        # Body card
        bslib::card_body(
          uiOutput("temp_value"),
          tags$div(class = "kpi-sub", uiOutput("temp_sub"))
        )
      ),

      # Card 2 - Rain
      # Main card
      bslib::card(
        # Header card
        bslib::card_header(
          tags$div(
            class = "kpi-title",
            tags$span(
              icon("cloud-rain"),
              "Chuva"
            ),
            bslib::tooltip(
              tags$span(icon("info-circle")),
              "Dados mais recentes conforme disponibilidade no banco de dados.",
              placement = "top"
            )
          )
        ),

        # Body card
        bslib::card_body(
          uiOutput("rain_value"),
          tags$div(class = "kpi-sub", uiOutput("rain_sub"))
        )
      ),

      # Card 3 - Pressure
      # Main card
      bslib::card(
        # Header card
        bslib::card_header(
          tags$div(
            class = "kpi-title",
            tags$span(
              icon("gauge-high", class = "me-2"),
              "Pressão"
            ),
            bslib::tooltip(
              tags$span(icon("info-circle")),
              "Dados mais recentes conforme disponibilidade no banco de dados.",
              placement = "top"
            )
          )
        ),

        # Body card
        bslib::card_body(
          uiOutput("pressure_value"),
          tags$div(class = "kpi-sub", uiOutput("pressure_sub"))
        )
      ),

      # Card 4 - Wind
      # Main card
      bslib::card(
        # Header card
        bslib::card_header(
          tags$div(
            class = "kpi-title",
            tags$span(
              icon("wind", class = "me-2"),
              "Vento"
            ),
            bslib::tooltip(
              tags$span(icon("info-circle")),
              "Dados mais recentes conforme disponibilidade no banco de dados.",
              placement = "top"
            )
          )
        ),

        bslib::card_body(
          uiOutput("wind_value"),
          tags$div(class = "kpi-sub", uiOutput("wind_sub"))
        )
      )
    ),

    # Main plot card ----------------
    bslib::card(
      full_screen = TRUE, ## adds expand button in the top-right corner
      bslib::card_header(
        tags$div(
          class = "kpi-title",
          tags$span(
            icon("chart-line", class = "me-2"),
            "Explorar Indicadores Climáticos"
          ),
          bslib::tooltip(
            tags$span(icon("info-circle")),
            "Selecione uma Estação/local e, em seguida, escolha um indicador para visualizar, no gráfico, as condições climáticas da região.",
            placement = "top"
          )
        )
      ),

      # Small control bar
      bslib::layout_columns(
        col_widths = c(4, 4, 4), ## 3 equal columns (12 col layout) - use remaining col for download button
        # Calendar selector dropdown 
        dateInput(
          inputId = "selected_date",
          label = "Data", 
          value = NULL,
          format = "dd-mm-yyyy",
          language = "pt"
        ),
        # Sensor (climate variables) selector dropdown
        selectInput(
          inputId = "sensor",
          label = "Indicador",
          choices = NULL
        )
      ),

      # Add spacing
      br(),

      # Render plot
      plotlyOutput("sensor_plot", height = "600px")
    )
  )
)
