# Libraries
library(shiny) ## app UI
library(bslib) ## dashboard UI
library(DBI) ## R Database Interface
# library(duckdb) ## Embedded database (used in dev project only)
library(RPostgres) ## DB interface 
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

ui <- bslib::page_navbar(
  fillable = FALSE,
  inverse = TRUE, ## revert white color on mobile menu icon
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
    
    bslib::tooltip(
      tags$span(icon("info-circle")),
      "Este painel interativo foi desenvolvido para a visualização e o monitoramento de dados climáticos provenientes de estações meteorológicas localizadas no Brasil.",
      placement = "top"
    )
  ),
  
  # ui theme
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter"),
    
    # Custom header colors
    "navbar-bg" = "#bbd0c9",
    "navbar-fg" = "black"
  ),
  
  # CSS for ui style
  header = tags$head(
    tags$style(HTML("
    
    /* Selected station heading */
    .station-heading {
      display: flex;
      align-items: center;
      gap: 0.6rem;
      margin: 0.25rem 0 1rem 0;
      color: #6c757d;
    }

    .station-heading h3 {
      margin: 0;
      font-size: 1.15rem;
      font-weight: 600;
    }

    .station-heading-icon {
      font-size: 1rem;
      color: #6c757d;
    }

    /* Navbar size adjustments - desktop */
    .navbar {
      background-image: linear-gradient(
          rgba(255, 255, 255, 0.78),
          rgba(255, 255, 255, 0.78)
        ),
        url('static/hero-bg3.jpg') !important;

      background-size: cover !important;
      background-position: center !important;
      background-repeat: no-repeat !important;

      backdrop-filter: blur(12px) saturate(140%);
      -webkit-backdrop-filter: blur(12px) saturate(140%);

      border-bottom: 1px solid rgba(255, 255, 255, 0.45);
      box-shadow: 0 4px 18px rgba(0, 0, 0, 0.06);

      padding-top: 0 !important;
      padding-bottom: 0 !important;
      min-height: 52px !important;
    }
    
    /* Navbar text and icon colors */
      .navbar,
      .navbar-brand,
      .navbar-brand span,
      .navbar-brand i,
      .navbar-nav .nav-link {
        color: #212529 !important;
      }

    .navbar > .container-fluid {
      min-height: 68px !important;
      padding-top: 0 !important;
      padding-bottom: 0 !important;
      align-items: center !important;
    }

    .navbar-brand {
      font-size: 0.85rem !important;
      padding-top: 0 !important;
      padding-bottom: 0 !important;
      margin-right: 1rem;
      display: flex;
      align-items: center;
    }

    .navbar-brand img {
      height: 55px !important;
    }

    .navbar-nav .nav-link {
      font-size: 0.95rem !important;
      font-weight: 600;
      padding-top: 0.15rem !important;
      padding-bottom: 0.15rem !important;
    }

    .navbar-nav .nav-link.active {
      padding-bottom: 0.2rem !important;
    }

    /* Navbar adjustments - mobile */
    @media (max-width: 576px) {
      .navbar {
        min-height: 40px !important;
      }

      .navbar > .container-fluid {
        min-height: 40px !important;
        padding-left: 0.5rem !important;
        padding-right: 0.5rem !important;
      }

      .navbar-brand {
        font-size: 0.7rem !important;
        margin-right: 0.4rem !important;
        gap: 0.35rem !important;
      }

      .navbar-brand img {
        height: 28px !important;
      }

      .navbar-nav {
        flex-direction: row !important;
        gap: 0.35rem;
      }

      .navbar-nav .nav-link {
        font-size: 0.68rem !important;
        padding-left: 0.25rem !important;
        padding-right: 0.25rem !important;
        padding-top: 0.1rem !important;
        padding-bottom: 0.1rem !important;
      }
    }

    /* Fix page_navbar/tab layout compression */
    .bslib-page-navbar,
    .bslib-page-navbar > .tab-content,
    .bslib-page-navbar > .tab-content > .tab-pane,
    .tab-content,
    .tab-pane {
      height: auto !important;
      min-height: auto !important;
      overflow: visible !important;
    }

    /* Fix sidebar layout inside navbar tabs */
    .bslib-sidebar-layout {
      height: auto !important;
      min-height: auto !important;
    }

    .bslib-sidebar-layout > .main {
      height: auto !important;
      min-height: auto !important;
      overflow: visible !important;
    }

    /* Cards */
    .card {
      height: auto !important;
      min-height: auto !important;
      border-radius: 16px;
      box-shadow: 0 8px 24px rgba(0,0,0,.06);
      border: 1px solid rgba(0,0,0,.06);
    }

    .card-body {
      height: auto !important;
      min-height: auto !important;
    }

    /* Mobile-specific layout fix */
    @media (max-width: 1130px) {
      #sensor_plot {
        width: 100% !important;
        min-height: 420px !important;
      }

      .plotly,
      .plot-container,
      .svg-container {
        width: 100% !important;
      }
    }

    /* Smaller Plotly title on mobile */
    @media (max-width: 576px) {
      #sensor_plot .gtitle {
        font-size: 13px !important;
      }
    }
    
    /* Sidebar background image */
    .bslib-sidebar-layout > .sidebar {
      background-image: linear-gradient(
          rgba(255, 255, 255, 0.78),
          rgba(255, 255, 255, 0.78)
        ),
        url('static/hero-bg3.jpg') !important;

      background-size: cover !important;
      background-position: center !important;
      background-repeat: no-repeat !important;

      backdrop-filter: blur(12px) saturate(140%);
      -webkit-backdrop-filter: blur(12px) saturate(140%);

      border-right: 1px solid rgba(255, 255, 255, 0.45);
      box-shadow: 4px 0 18px rgba(0, 0, 0, 0.06);

      padding-top: 0.75rem;
    }

    /* CSS for station input */
    #station + .selectize-control .selectize-input {
      font-size: 0.85rem;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }

    #station + .selectize-control .selectize-input .item {
      flex: 1;
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

    .sidebar-card .card-body {
      padding: 0.75rem;
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

    /* Contact card */
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

    /* KPI grid */
    .kpi-grid {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 0.75rem;
    }

    @media (max-width: 992px) {
      .kpi-grid {
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
    }

    @media (max-width: 576px) {
      .kpi-grid {
        grid-template-columns: 1fr;
      }
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

    /* Animations for KPI icons */
    .kpi-title i {
      transition: transform 0.3s ease, color 0.3s ease;
    }

    .kpi-title:hover i {
      transform: scale(1.2);
      color: #0d6efd;
    }
    
    /* Statistics tab */
    .stats-filter-row {
      margin-bottom: 1rem;
    }

    .stats-grid {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 0.75rem;
    }

    @media (max-width: 992px) {
      .stats-grid {
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
    }

    @media (max-width: 576px) {
      .stats-grid {
        grid-template-columns: 1fr;
      }
    }

    .stats-card-icon {
      font-size: 1.25rem;
      color: #0d6efd;
      margin-bottom: 0.4rem;
    }

    .stats-value {
      font-size: 1.55rem;
      font-weight: 700;
      line-height: 1.15;
    }

    .stats-sub {
      font-size: 0.85rem;
      color: rgba(0,0,0,.55);
      margin-top: 0.25rem;
    }
    
    /* Tablet layout for plot controls */
    @media (min-width: 577px) and (max-width: 992px) {
      .plot-control-bar {
        grid-template-columns: 1fr 1fr !important;
      }

      .plot-control-bar .shiny-date-range-input {
        min-width: 260px;
      }

      .plot-control-bar .form-group {
        width: 100%;
      }

      .plot-control-bar .btn {
        width: fit-content;
        align-self: end;
      }
    }
    
  "))
  ),
  
  # TAB 1: MAIN PANEL --------------------
  bslib::nav_panel(
    title = "Painel de Estações Meteorológicas",
    
    bslib::layout_sidebar(
      fillable = FALSE,
      
      # SIDEBAR --------------------
      sidebar = bslib::sidebar(
        open = list(
          desktop = "open",
          mobile = "always"
        ),
        
        # Station dropdown input selector
        bslib::card(
          class = "sidebar-card",
          bslib::card_header("Estação/Local"),
          
          # Stations (locations) selector dropdown
          selectInput(
            inputId = "station",
            label = NULL,
            choices = NULL ## keep empty bc this will populate dynamically
          )
        ),
        
        # Info section ----------------
        bslib::card(
          class = "sidebar-card",
          #full_screen = TRUE,#
          
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
          class = "sidebar-card",
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
      uiOutput("selected_station_heading"),
      bslib::layout_column_wrap(
        width = 1,
        heights_equal = "row",
        
        # KPI DIV (for 4 cards) ---------------
        tags$div(
          class = "kpi-grid",
          
          # Card 1 - Temperature
          bslib::card(
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
            
            bslib::card_body(
              uiOutput("temp_value"),
              tags$div(class = "kpi-sub", uiOutput("temp_sub"))
            )
          ),
          
          # Card 2 - Rain
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "kpi-title",
                tags$span(
                  icon("cloud-rain", class = "me-2"),
                  "Chuva"
                ),
                bslib::tooltip(
                  tags$span(icon("info-circle")),
                  "Dados mais recentes conforme disponibilidade no banco de dados.",
                  placement = "top"
                )
              )
            ),
            
            bslib::card_body(
              uiOutput("rain_value"),
              tags$div(class = "kpi-sub", uiOutput("rain_sub"))
            )
          ),
          
          ## UPDATED BLOCK BEGINS
          # Card 3 - Pressure or Air Quality
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "kpi-title",
                uiOutput("third_kpi_title"),
                bslib::tooltip(
                  tags$span(icon("info-circle")),
                  "Dados mais recentes conforme disponibilidade no banco de dados.",
                  placement = "top"
                )
              )
            ),
            
            bslib::card_body(
              uiOutput("third_kpi_value"),
              tags$div(class = "kpi-sub", uiOutput("third_kpi_sub"))
            )
          ), ## UPDATED BLOCK ENDS
          
          # Card 4 - Wind
          bslib::card(
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
          full_screen = TRUE,
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
          
          ## UPDATED BLOCK BEGINS
          # Small control bar
          bslib::layout_columns(
            class = "plot-control-bar",
            col_widths = c(4, 4, 4),
            
            # Date range selector
            dateRangeInput(
              inputId = "selected_date",
              label = "Período",
              start = NULL,
              end = NULL,
              format = "dd-mm-yyyy",
              language = "pt",
              separator = " até "
            ),
            
            # Sensor (climate variables) selector dropdown
            selectInput(
              inputId = "sensor",
              label = "Indicador",
              choices = NULL
            ),
            
            # Download button
            tags$div(
              style = "margin-top: 1.9rem;",
              downloadButton(
                outputId = "download_sensor_csv",
                label = "Baixar CSV"
              )
            )
          ), ## UPDATED BLOCK ENDS
          
          # Add spacing
          br(),
          
          # Render plot
          plotlyOutput("sensor_plot", height = "280px") ## Adjust main chart card height
        )
      )
    )
  ),
  
  # TAB 2: STATISTICS --------------------
  bslib::nav_panel(
    title = "Estatísticas",
    
    bslib::layout_column_wrap(
      width = 1,
      
      bslib::card(
        bslib::card_header(
          tags$div(
            class = "kpi-title",
            tags$span(
              icon("chart-simple", class = "me-2"),
              "Resumo Estatístico"
            ),
            bslib::tooltip(
              tags$span(icon("info-circle")),
              "Selecione uma estação, um sensor e um período para visualizar os principais destaques estatísticos.",
              placement = "top"
            )
          )
        ),
        
        bslib::card_body(
          
          tags$div(
            class = "stats-filter-row",
            
            bslib::layout_columns(
              col_widths = c(3, 3, 3, 3),
              
              selectInput(
                inputId = "stats_station",
                label = "Estação/Local",
                choices = NULL
              ),
              
              selectInput(
                inputId = "stats_sensor",
                label = "Sensor",
                choices = NULL
              ),
              
              dateInput(
                inputId = "stats_base_date",
                label = "Data base",
                value = NULL,
                format = "dd-mm-yyyy",
                language = "pt"
              ),
              
              radioButtons(
                inputId = "stats_period",
                label = "Período",
                choices = c(
                  "Dia" = "day",
                  "Semana" = "week",
                  "Mês" = "month"
                ),
                selected = "day",
                inline = TRUE
              )
            )
          ),
          
          tags$hr(),
          
          uiOutput("stats_summary_title"),
          
          tags$div(
            class = "stats-grid",
            
            bslib::card(
              bslib::card_body(
                tags$div(class = "stats-card-icon", icon("arrow-down")),
                tags$div(class = "stats-value", uiOutput("stats_min_value")),
                tags$div(class = "stats-sub", uiOutput("stats_min_sub"))
              )
            ),
            
            bslib::card(
              bslib::card_body(
                tags$div(class = "stats-card-icon", icon("arrow-up")),
                tags$div(class = "stats-value", uiOutput("stats_max_value")),
                tags$div(class = "stats-sub", uiOutput("stats_max_sub"))
              )
            ),
            
            bslib::card(
              bslib::card_body(
                tags$div(class = "stats-card-icon", icon("calculator")),
                tags$div(class = "stats-value", uiOutput("stats_avg_value")),
                tags$div(class = "stats-sub", uiOutput("stats_avg_sub"))
              )
            ),
            
            bslib::card(
              bslib::card_body(
                tags$div(class = "stats-card-icon", icon("calendar-days")),
                tags$div(class = "stats-value", uiOutput("stats_period_value")),
                tags$div(class = "stats-sub", uiOutput("stats_period_sub"))
              )
            )
          ),
          
          tags$br(),
          
          uiOutput("stats_empty_message")
        )
      )
    )
  ),
  # TAB 3: EDUCATIONAL --------------------
  #bslib::nav_panel(
  #title = "Educativo",
  #tags$div()
  #)
)