library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(tibble)

# ──────────────────────────────────────────────────────────────────────────────
# Theme & CSS
# ──────────────────────────────────────────────────────────────────────────────
spotify_theme <- bs_theme(
  version   = 5,
  bg        = "#121212",
  fg        = "#FFFFFF",
  primary   = "#1DB954",
  secondary = "#535353",
  base_font = font_google("Inter")
)

custom_css <- HTML("\
  /* Top taskbar */\n  #topbar { position: fixed; top: 0; left: 0; right: 0; height: 60px; display: flex; align-items: center; padding: 0 240px; background: linear-gradient(90deg,#006400,#1DB954); z-index:1000;}\n  .nav-icon, .nav-btn { background:transparent; border:none; color:#e0e0e0; margin-right:16px;}\n  .nav-icon { font-size:1.5rem; }\n  .nav-btn { font-weight:700; font-size:1rem; }\n  .nav-icon.active, .nav-btn.active, .nav-icon:hover, .nav-btn:hover { color:#fff; border-bottom:3px solid #fff; }\n  /* Left sidebar */\n  #sidebar-left { position: fixed; top:60px; left:0; bottom:0; width:240px; background:#000; padding:20px; overflow-y:auto; }\n  #sidebar-left h4 { color:#1DB954; margin:0 0 8px; font-weight:800;}\n  #sidebar-left select, #sidebar-left input { background:#181818; color:#fff; border:1px solid #333; margin-bottom:16px; width:100%; }\n  /* Main content */\n  #main { margin-top:80px; margin-left:260px; margin-right:360px; padding:20px; }\n  /* Right sidebar */\n  #sidebar-right { position:fixed; top:60px; right:0; bottom:0; width:300px; background:#000; padding:24px; overflow-y:auto; }\n  #sidebar-right .cover-img { width:100%; border-radius:8px; margin-bottom:16px; }\n  #sidebar-right h4 { color:#FFFFFF; margin:16px 0 8px; font-weight:800; }\n  #sidebar-right a { color:#1DB954; text-decoration:none; font-weight:600; }\n")

custom_js <- HTML("Shiny.addCustomMessageHandler('highlightTab', function(btnId) { $('.nav-icon, .nav-btn').removeClass('active'); $('#' + btnId).addClass('active'); });")

# ──────────────────────────────────────────────────────────────────────────────
# UI
# ──────────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = spotify_theme,
  tags$head(
    tags$style(custom_css),
    tags$script(custom_js)
  ),
  
  # Top taskbar
  tags$div(
    id = "topbar",
    actionButton("btn_home",    NULL,           icon = icon("home"),        class = "nav-icon"),
    actionButton("btn_play",    NULL,           icon = icon("play-circle"), class = "nav-icon"),
    actionButton("nav_summary", "Summary",     class = "nav-btn"),
    actionButton("nav_sailor",  "Sailor Shift",class = "nav-btn"),
    actionButton("nav_oceanus","Oceanus Folk",class = "nav-btn active"),
    actionButton("nav_other",  "Other Artists",class = "nav-btn")
  ),
  
  # Left sidebar controls
  tags$div(
    id = "sidebar-left",
    h4("Oceanus Folk"),
    selectInput("direction", "Outward Influences", choices = c("Outward Influences","Inward Influences")),
    h4("Plot 1"),
    selectInput("plot1", "Network - Outward", choices = c("Network - Outward","Network - Inward")),
    h4("Select by ID"),
    selectInput("select_id", "Oceanus Folk", choices = c("Oceanus Folk","Artist A","Artist B")),
    h4("Plot 2"),
    selectInput("plot2", "Plotly - Outward", choices = c("Plotly - Outward","Plotly - Inward")),
    h4("Genre"),
    selectInput(
      "genre", "Genre",
      choices = c("Americana","Indie Folk","Indie Rock","Blues Rock","Celtic Folk"),
      selected = "Americana", multiple = TRUE, selectize = FALSE, size = 6
    )
  ),
  
  # Main content: two plots + description
  tags$div(
    id = "main",
    fluidRow(
      column(6, plotOutput("network_plot",  height = "400px")),
      column(6, plotlyOutput("influence_plot", height = "400px"))
    ),
    fluidRow(
      column(12,
             tags$h4("General Overview"),
             htmlOutput("general_overview")
      )
    )
  ),
  
  # Right sidebar summary + cover + bio
  tags$div(
    id = "sidebar-right",
    h3("Summary", style = "color:#FFFFFF; margin-top:0; margin-bottom:12px;"),
    selectInput(
      "summary", NULL,
      choices = c("Oceanus Folk","Sailor Shift","Other Artists"),
      selected = "Oceanus Folk", width = "100%"
    ),
    tags$img(
      src   = "OceanusFolk.png",
      alt   = "Cover image",
      class = "cover-img"
    ),
    tags$h4("About the Genre"),
    tags$a("Oceanus Folk", href = "#"),
    htmlOutput("genre_description")
  )
)

# ──────────────────────────────────────────────────────────────────────────────
# Server (stub)
# ──────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  output$network_plot <- renderPlot({ plot(1:10, 1:10) })
  output$influence_plot <- renderPlotly({ plot_ly(x=1:10,y=1:10,type='scatter',mode='lines+markers') })
  output$general_overview <- renderUI({ HTML('<p>Oceanus Folk’s stylistic reach...</p>') })
  output$genre_description <- renderUI({ HTML('<p>Oceanus Folk’s story is one of fits and starts...</p>') })
}

shinyApp(ui, server)
