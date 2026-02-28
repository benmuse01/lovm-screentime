# Vox Machina Screen Time Dashboard
# Save this as app.R and run with: shiny::runApp()
#
# CHARACTER IMAGES SETUP:
# Place character PNG images in a folder called "www" in the same directory as this app.R file
# Name them: vex.png, vax.png, percy.png, keyleth.png, pike.png, grog.png, scanlan.png
# The app will automatically use them in the circular visualizations

library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(DT)
library(patchwork)
library(ggimage)  # For character images
library(htmlwidgets)  # For onRender function
library(zoo)  # For rolling mean in trend lines

# ============================================================================
# DATA LOADING
# ============================================================================

# Update this path to match your data directory
data_dir <- "vox_machina_complete"

# Load data
show_episodes <- read.csv(file.path(data_dir, "SHOW_all_episodes_info.csv"))
show_screen_time <- read.csv(file.path(data_dir, "SHOW_all_screen_time_by_episode.csv"))
show_character_combined <- read.csv(file.path(data_dir, "SHOW_character_totals_combined.csv"))

# Define character groups
main_cast <- c("Vex'ahlia", "Vax'ildan", "Percival", "Keyleth", "Pike", "Grog", "Scanlan")
villains <- c("Anna Ripley", "Delilah Briarwood", "Sylas Briarwood", 
              "Thordak", "Raishan", "Umbrasyl")

# Color palettes
main_cast_colors <- c(
  "Vex'ahlia" = "#2E7D32", "Vax'ildan" = "#1565C0", "Percival" = "#37474F",
  "Keyleth" = "#4CAF50", "Pike" = "#FFB300", "Grog" = "#D32F2F", "Scanlan" = "#7B1FA2"
)

villain_colors <- c(
  "Anna Ripley" = "#C62828", "Delilah Briarwood" = "#4A148C", "Sylas Briarwood" = "#311B92",
  "Thordak" = "#E65100", "Raishan" = "#1B5E20", "Umbrasyl" = "#424242"
)

# Character image mapping (relative to www folder)
# Filenames must match exactly what is on disk (case-sensitive on some systems)
character_images <- c(
  # Main cast
  "Vex'ahlia" = "Vex.png",
  "Vax'ildan" = "Vax.png",
  "Percival"  = "Percy.png",
  "Keyleth"   = "Keyleth.png",
  "Pike"      = "Pike.png",
  "Grog"      = "Grog.png",
  "Scanlan"   = "Scanlan.png",
  # Villains — PNG or WEBP, matched to actual filenames on disk
  "Anna Ripley"       = "Ripley.png",
  "Delilah Briarwood" = "Delilah.png",
  "Sylas Briarwood"   = "Sylas.png",
  "Thordak"           = "Thordak.png",
  "Raishan"           = "Raishan.png",
  "Umbrasyl"          = "Umbraysl.png"
)

# Per-character image zoom (size) overrides — default is 0.58
# Increase to zoom in, decrease to zoom out
character_image_size <- c(
  "Anna Ripley"       = 0.75,   # zoom in
  "Thordak"           = 0.75,   # zoom in
  "Raishan"           = 0.75    # zoom in
)

# Per-character vertical offset overrides — default is 0 (centred)
# Negative values shift the image DOWN within the circle
character_image_yoffset <- c(
  "Sylas Briarwood"   = -0.12   # shift down
)

# Calculate total runtime
total_runtime_hours <- sum(show_episodes$total_duration_min) / 60

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Function to create individual circular progress for one character with image
create_character_circle <- function(char_name, hours, total_hours, color) {
  
  percentage <- (hours / total_hours) * 100
  
  # Create data for background ring (full circle)
  angles_bg <- seq(0, 2 * pi, length.out = 100)
  
  # Outer circle
  circle_outer_bg <- data.frame(
    x = cos(angles_bg) * 1.0,
    y = sin(angles_bg) * 1.0,
    group = "outer_bg"
  )
  
  # Inner circle
  circle_inner_bg <- data.frame(
    x = cos(rev(angles_bg)) * 0.75,
    y = sin(rev(angles_bg)) * 0.75,
    group = "inner_bg"
  )
  
  # Combine for hollow ring background
  ring_bg <- rbind(circle_outer_bg, circle_inner_bg)
  
  # Create data for progress ring arc
  progress_angle <- (percentage / 100) * 2 * pi
  progress_angles <- seq(0, progress_angle, length.out = 100)
  
  # Progress outer arc
  progress_outer <- data.frame(
    x = cos(progress_angles) * 1.0,
    y = sin(progress_angles) * 1.0,
    group = "progress_outer"
  )
  
  # Progress inner arc (reversed for proper polygon)
  progress_inner <- data.frame(
    x = cos(rev(progress_angles)) * 0.75,
    y = sin(rev(progress_angles)) * 0.75,
    group = "progress_inner"
  )
  
  # Combine for hollow ring progress
  ring_progress <- rbind(progress_outer, progress_inner)
  
  # Check if image exists AND can actually be decoded by magick.
  # file.exists() alone is not enough: shinyapps.io's ImageMagick often lacks
  # WebP support, so trying to read a .webp file throws an error that crashes
  # the whole renderPlot.  tryCatch() lets us fall back to text gracefully.
  image_path      <- character_images[char_name]
  image_full_path <- file.path("www", image_path)
  use_image <- tryCatch({
    file.exists(image_full_path) &&
      !is.null(magick::image_read(image_full_path))
  }, error = function(e) FALSE)
  
  # Create circular mask data
  circle_mask <- data.frame(
    x = cos(seq(0, 2*pi, length.out = 100)) * 0.65,
    y = sin(seq(0, 2*pi, length.out = 100)) * 0.65
  )
  
  # Create the plot
  p <- ggplot() +
    coord_equal(xlim = c(-1.3, 1.3), ylim = c(-1.8, 1.3)) +
    theme_void() +
    theme(plot.margin = margin(5, 5, 20, 5))
  
  # Resolve per-character size and y-offset (fall back to defaults if not set)
  img_size    <- if (char_name %in% names(character_image_size))
                   character_image_size[char_name] else 0.58
  img_yoffset <- if (char_name %in% names(character_image_yoffset))
                   character_image_yoffset[char_name] else 0

  # Add character image if it exists (with proper circular clipping)
  if (use_image) {
    # First add the image
    p <- p +
      geom_image(aes(x = 0, y = img_yoffset, image = image_full_path),
                 size = img_size, asp = 1)
    
    # Add white ring to mask the edges of the square image
    # Create ring by using outer and inner circles
    outer_mask <- data.frame(
      x = c(cos(seq(0, 2*pi, length.out = 100)) * 1.5,
            rev(cos(seq(0, 2*pi, length.out = 100)) * 0.65)),
      y = c(sin(seq(0, 2*pi, length.out = 100)) * 1.5,
            rev(sin(seq(0, 2*pi, length.out = 100)) * 0.65))
    )
    
    p <- p +
      geom_polygon(data = outer_mask, aes(x = x, y = y), 
                   fill = "white", color = NA) +
      # Add circular border around image
      geom_path(data = circle_mask, aes(x = x, y = y), 
                color = color, size = 3, lineend = "round")
  } else {
    # If no image, show text
    p <- p +
      annotate("text", x = 0, y = 0.15, 
               label = sprintf("%.1f hrs", hours), 
               size = 7, fontface = "bold", color = color) +
      annotate("text", x = 0, y = -0.15, 
               label = sprintf("%.1f%%", percentage), 
               size = 5.5, fontface = "bold", color = "grey40")
  }
  
  # Add rings on top
  p <- p +
    # Background ring (grey filled donut)
    geom_polygon(data = ring_bg, aes(x = x, y = y),
                 fill = "grey88", color = "grey75", linewidth = 0.3, alpha = 0.6) +
    # Progress ring (filled with character color)
    geom_polygon(data = ring_progress, aes(x = x, y = y),
                 fill = color, color = color, linewidth = 0.5, alpha = 0.92) +
    # Character name below the circle
    annotate("text", x = 0, y = -1.25, 
             label = char_name, 
             size = 5.5, fontface = "bold") +
    # Add hours and percentage below name
    annotate("text", x = 0, y = -1.5,
             label = sprintf("%.1f hrs  •  %.1f%%", hours, percentage),
             size = 4, color = "grey30")
  
  return(p)
}

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Vox Machina Screen Time Analysis", titleWidth = 320),

  dashboardSidebar(width = 320,
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Main Characters", tabName = "maincast", icon = icon("users")),
      menuItem("Story Timeline", tabName = "timeline", icon = icon("clock")),
      menuItem("Episode Analysis", tabName = "episodes", icon = icon("film")),
      menuItem("Character Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Data Table", tabName = "data", icon = icon("table")),
      menuItem("Methodology", tabName = "methodology", icon = icon("info-circle"))
    ),
    
    hr(),
    
    h4("Filters", style = "padding-left: 15px;"),
    
    selectInput("season_filter", "Season:",
                choices = c("All Seasons" = "all", 
                            "Season 1" = "1", 
                            "Season 2" = "2", 
                            "Season 3" = "3"),
                selected = "all"),
    
    hr(),
    
    div(style = "padding: 15px;",
        h5("Show Statistics", style = "font-weight: bold;"),
        p(paste("Total Episodes:", nrow(show_episodes))),
        p(sprintf("Total Runtime: %.1f hours", total_runtime_hours)),
        p(paste("Total Characters:", nrow(show_character_combined)))
    )
  ),
  
  dashboardBody(
    
    tags$head(

      # Tell mobile browsers to render at the device's actual width
      # (without this, phones zoom out and show the desktop layout shrunken)
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),

      tags$style(HTML("

        /* ── Base styles (all screen sizes) ─────────────────────────── */
        .box-title { font-weight: bold; font-size: 18px; }
        .small-box { border-radius: 5px; }
        .info-box  { border-radius: 5px; }
        .main-header .logo {
          font-size: 16px;
          white-space: nowrap;
          overflow: visible;
        }

        /* ── Tablet  (768 px – 991 px) ───────────────────────────────── */
        @media (max-width: 991px) {
          .box-title          { font-size: 15px; }
          .main-header .logo  { font-size: 14px !important; }
        }

        /* ── Mobile  (≤ 767 px) ──────────────────────────────────────── */
        @media (max-width: 767px) {

          /* Header: shrink long title so it doesn't get clipped */
          .main-header .logo {
            font-size: 12px !important;
            padding: 0 6px  !important;
          }

          /* Sidebar: when the user opens it, fill the screen width
             so it doesn't awkwardly half-cover the content */
          .main-sidebar { width: 100vw !important; }

          /* Content area: sit flush at the left edge because the
             sidebar is hidden by default on mobile */
          .content-wrapper,
          .right-side { margin-left: 0 !important; }

          /* Value boxes: 2-per-row (50 %) instead of 4-per-row (25 %) */
          .col-sm-3 {
            width: 50%  !important;
            float: left !important;
          }

          /* Side-by-side boxes become full-width and stack vertically */
          .col-sm-6,
          .col-sm-8,
          .col-sm-9 {
            width: 100% !important;
            float: none !important;
          }

          /* ggplot2 outputs: reduce height so charts don't fill
             the entire screen and users don't have to scroll forever */
          .shiny-plot-output {
            height:     320px !important;
            min-height: 220px !important;
          }

          /* Plotly charts: responsive container height */
          .js-plotly-plot { height: 280px !important; }

          /* Tighter inner padding inside each box */
          .box-body { padding: 8px !important; }

          /* Sidebar statistics text: smaller so the numbers fit */
          .sidebar .h5,
          .sidebar p  { font-size: 12px !important; }

          /* Data table: scroll sideways rather than breaking the layout */
          .dataTables_wrapper { overflow-x: auto; }
        }

      "))
    ),
    
    tabItems(
      
      # ===== OVERVIEW TAB =====
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_episodes_box", width = 3),
                valueBoxOutput("total_runtime_box", width = 3),
                valueBoxOutput("top_character_box", width = 3),
                valueBoxOutput("top_villain_box", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "Top 10 Characters by Screen Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("overview_top10", height = "400px")
                ),
                
                box(
                  title = "Screen Time Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("overview_distribution", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Screen Time by Season",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("overview_by_season", height = "400px")
                )
              )
      ),
      
      # ===== MAIN CHARACTERS TAB =====
      tabItem(tabName = "maincast",

              # --- HEROES VS VILLAINS DISPARITY ---
              fluidRow(
                box(
                  title = "Heroes vs Villains: Screen Time Disparity",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  p(style = "font-size: 14px; line-height: 1.5; padding: 10px;",
                    "The main cast vastly dominates screen time compared to any individual villain. ",
                    "Even the most prominent villain receives only a fraction of the screen time afforded to the heroes."),
                  plotlyOutput("cast_vs_villains", height = "450px")
                )
              ),

              # --- HERO CIRCLES ---
              fluidRow(
                box(
                  title = "Heroes — Screen Time Circles",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("maincast_circles", height = "600px")
                )
              ),

              # --- VILLAIN CIRCLES ---
              fluidRow(
                box(
                  title = "Villains — Screen Time Circles",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("villain_circles", height = "600px")
                )
              ),

              # --- PERCY PARADOX ---
              fluidRow(
                box(
                  title = "The Percy Paradox: From Protagonist to Supporting Cast",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  p(style = "font-size: 15px; line-height: 1.6; padding: 10px;",
                    strong("A Narrative Shift:"),
                    " Percival de Rolo begins as the emotional center of Season 1, commanding the most screen time ",
                    "as the Briarwood arc delves deep into his tragic past and quest for vengeance. However, as the series ",
                    "progresses, his role evolves from protagonist to ensemble member. By Season 3, Percy has the ",
                    em("least"), " screen time among the main cast—a remarkable reversal that reflects the show's shift ",
                    "toward other character arcs and the broader Chroma Conclave threat."),
                  plotlyOutput("percy_decline", height = "420px")
                )
              ),

              # --- HERO BARS + EPISODES ---
              fluidRow(
                box(
                  title = "Hero Screen Time Comparison",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("maincast_bars", height = "400px")
                ),
                box(
                  title = "Hero Episodes Appeared",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("maincast_episodes", height = "400px")
                )
              ),

              # --- VILLAIN BARS + TIMELINE ---
              fluidRow(
                box(
                  title = "Villain Screen Time Comparison",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("villain_bars", height = "400px")
                ),
                box(
                  title = "Villain Presence Across Episodes",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  p(style = "font-size: 13px; padding: 5px;",
                    "Track when each villain appears throughout the series. The Briarwoods dominate Season 1, ",
                    "while the Chroma Conclave members emerge in later seasons."),
                  plotlyOutput("villain_timeline", height = "350px")
                )
              )
      ),

      # ===== STORY TIMELINE TAB =====
      tabItem(tabName = "timeline",
              fluidRow(
                box(
                  title = "The Vox Machina Story: Key Moments Throughout the Series",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p(style = "font-size: 14px; line-height: 1.5; padding: 10px;",
                    "Follow the major story arcs and character moments that shaped the narrative of Vox Machina. 
                    The grey lines show all main cast members' screen time, with colored segments highlighting 
                    key insights: Percy's Season 1 dominance, Pike's reduced presence, and the rising prominence 
                    of Vex, Vax, and Keyleth in later seasons."),
                  plotOutput("story_timeline", height = "700px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Screen Time Growth: The Rising Stars",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  p(style = "font-size: 14px; padding: 10px;",
                    strong("Consistent Growth:"), " Vex'ahlia, Vax'ildan, and Keyleth show remarkable screen time growth 
                    from Season 1 to Season 3. While other characters' focus waxes and wanes with their story arcs, 
                    these three steadily rise to take center stage as the series progresses."),
                  plotlyOutput("growth_trends", height = "450px")
                )
              )
      ),
      
      # ===== EPISODE ANALYSIS TAB =====
      tabItem(tabName = "episodes",
              fluidRow(
                box(
                  title = "Character Presence Heatmap",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("episode_heatmap", height = "500px")
                )
              ),
              
              fluidRow(
                box(
                  title = "Screen Time by Episode",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("episode_timeline", height = "400px")
                )
              )
      ),
      
      # ===== COMPARISON TAB =====
      tabItem(tabName = "comparison",
              fluidRow(
                box(
                  title = "Character Comparison Tool",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  selectInput("compare_char1", "Character 1:",
                              choices = c(main_cast, villains),
                              selected = "Vex'ahlia"),
                  selectInput("compare_char2", "Character 2:",
                              choices = c(main_cast, villains),
                              selected = "Percival")
                ),
                
                box(
                  title = "Comparison Results",
                  status = "info",
                  solidHeader = TRUE,
                  width = 9,
                  plotlyOutput("comparison_plot", height = "600px")
                )
              ),
              
      ),
      
      # ===== DATA TABLE TAB =====
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Character Data Table",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("data_table")
                )
              )
      ),

      # ===== METHODOLOGY TAB =====
      tabItem(tabName = "methodology",
              fluidRow(
                box(
                  title = "Data Collection Methodology",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,

                  h3("How the Data Was Collected"),

                  p("The screen time data for this project was collected by scraping ",
                    strong("Amazon Prime Video's X-Ray feature"), ", which is powered by ",
                    strong("IMDb's database"), ". X-Ray displays real-time character
                    information during playback, and is backed by an
                    unencrypted JSON file that Amazon transmits during page load. By
                    intercepting this network request with Chrome's Developer Tools,
                    the full scenelevel character data (including precise start and
                    end timestamps) can be captured for each episode without any
                    manual logging."),

                  hr(),

                  h4("Collection Process"),

                  tags$ol(
                    tags$li(
                      strong("Navigate to the episode page:"), " Each episode of Legends of
                      Vox Machina was opened on Amazon Prime Video in Chrome."
                    ),
                    tags$li(
                      strong("Open Chrome Developer Tools:"), " Before or during page load,
                      Chrome's Developer Tools were opened (F12) and the ",
                      em("Network"), " tab was activated to record all outgoing and
                      incoming requests."
                    ),
                    tags$li(
                      strong("Intercept the X-Ray JSON request:"), " Amazon's X-Ray feature
                      is backed by an unencrypted JSON file transmitted during page load.
                      This request is identifiable by the ", code("_xray?firmware"), "
                      and the pattern in its URL was located in the Network tab and the full
                      response was saved as a JSON file."
                    ),
                    tags$li(
                      strong("Parse the JSON:"), " The JSON is structured hierarchically,
                      with scenes nested inside page sections and widgets. Each scene
                      entry contains character names, IMDb identifiers, and start/end
                      timestamps marking when each character appears on screen."
                    ),
                    tags$li(
                      strong("Convert to tabular data:"), " The parsed scene data was
                      extracted into CSV format, producing one row per character
                      appearance with start time, end time, character name, and episode
                      metadata."
                    ),
                    tags$li(
                      strong("Aggregate in R:"), " The per-appearance CSV files were
                      combined across all 36 episodes and summarised in R to calculate
                      total and average screen time per character per episode and per
                      season, producing the data files that power this dashboard."
                    )
                  ),

                  hr(),

                  h4("Important Caveats"),

                  tags$ul(
                    tags$li("Screen time figures represent ", em("on-screen presence"),
                            " as detected by X-Ray, not speaking time or narrative importance."),
                    tags$li("Some character times may be ", strong("slightly inflated"),
                            " if a character appears only briefly at the edge of a scene
                            but is still registered by X-Ray for the full scene interval."),
                    tags$li("X-Ray data reflects IMDb's tagging of the episode and may
                            occasionally miss minor background appearances or misattribute
                            very short cameos.")
                  ),

                  hr(),

                  h4("Coverage"),

                  tags$ul(
                    tags$li(strong("Season 1:"), " Episodes 1–12"),
                    tags$li(strong("Season 2:"), " Episodes 1–12"),
                    tags$li(strong("Season 3:"), " Episodes 1–12"),
                    tags$li("Total: 36 episodes across the full run of ",
                            em("Legends of Vox Machina"))
                  ),

                  hr(),

                  h4("Acknowledgements & Further Reading"),

                  p("The data collection approach used in this project was inspired by the
                    excellent tutorial written by Alex at ",
                    tags$a(href = "https://www.curiousgnu.com/movie-character-screen-time",
                           target = "_blank",
                           "curiousgnu.com"),
                    ". That tutorial provides a detailed, step-by-step walkthrough of how to
                    scrape Amazon X-Ray data to measure character screen time. I highly
                    recommended reading if you want to replicate or extend this analysis.")
                )
              )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive data filtering based on season
  filtered_screen_time <- reactive({
    if (input$season_filter == "all") {
      show_screen_time
    } else {
      show_screen_time %>% filter(season == as.numeric(input$season_filter))
    }
  })
  
  filtered_character_totals <- reactive({
    if (input$season_filter == "all") {
      show_character_combined
    } else {
      filtered_screen_time() %>%
        group_by(character_name, actor_name) %>%
        summarise(
          episodes_appeared = n(),
          total_screen_time_hours = sum(total_screen_time_min) / 60,
          .groups = "drop"
        ) %>%
        group_by(character_name) %>%
        summarise(
          actors = paste(unique(actor_name), collapse = " / "),
          num_actors = n_distinct(actor_name),
          episodes_appeared = sum(episodes_appeared),
          total_screen_time_hours = sum(total_screen_time_hours),
          .groups = "drop"
        )
    }
  })
  
  # Value boxes
  output$total_episodes_box <- renderValueBox({
    valueBox(
      nrow(show_episodes),
      "Total Episodes",
      icon = icon("film"),
      color = "blue"
    )
  })
  
  output$total_runtime_box <- renderValueBox({
    valueBox(
      sprintf("%.1f hrs", total_runtime_hours),
      "Total Runtime",
      icon = icon("clock"),
      color = "green"
    )
  })
  
  output$top_character_box <- renderValueBox({
    top_char <- filtered_character_totals() %>%
      filter(character_name %in% main_cast) %>%
      arrange(desc(total_screen_time_hours)) %>%
      slice(1)
    
    valueBox(
      top_char$character_name,
      sprintf("Top Hero (%.1fh)", top_char$total_screen_time_hours),
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$top_villain_box <- renderValueBox({
    top_villain <- filtered_character_totals() %>%
      filter(character_name %in% villains) %>%
      arrange(desc(total_screen_time_hours)) %>%
      slice(1)
    
    if (nrow(top_villain) > 0) {
      valueBox(
        top_villain$character_name,
        sprintf("Top Villain (%.1fh)", top_villain$total_screen_time_hours),
        icon = icon("skull"),
        color = "red"
      )
    } else {
      valueBox(
        "N/A",
        "No Villains",
        icon = icon("skull"),
        color = "red"
      )
    }
  })
  
  # Overview plots
  output$overview_top10 <- renderPlotly({
    top10 <- filtered_character_totals() %>%
      arrange(desc(total_screen_time_hours)) %>%
      head(10)
    
    # Assign colors based on character type
    colors <- sapply(top10$character_name, function(char) {
      if (char %in% names(main_cast_colors)) {
        return(main_cast_colors[char])
      } else if (char %in% names(villain_colors)) {
        return(villain_colors[char])
      } else {
        return("#757575")  # grey for others
      }
    })
    
    plot_ly(top10, 
            x = ~total_screen_time_hours,
            y = ~reorder(character_name, total_screen_time_hours),
            type = "bar",
            orientation = "h",
            marker = list(color = colors),
            text = ~sprintf("%.1f hours", total_screen_time_hours),
            textposition = "outside") %>%
      layout(
        xaxis = list(title = "Hours", range = c(0, 12)),
        yaxis = list(title = ""),
        margin = list(l = 150, r = 80)
      )
  })

  output$overview_distribution <- renderPlotly({
    dist_data <- filtered_character_totals() %>%
      mutate(type = case_when(
        character_name %in% main_cast ~ "Main Cast",
        character_name %in% villains ~ "Villain",
        TRUE ~ "Other"
      ))
    
    # Summarise to one row per type, then explicitly order so colors map correctly
    dist_summary <- dist_data %>%
      group_by(type) %>%
      summarise(total = sum(total_screen_time_hours), .groups = "drop") %>%
      mutate(type = factor(type, levels = c("Main Cast", "Other", "Villain"))) %>%
      arrange(type)

    color_map <- c("Main Cast" = "#1976D2", "Other" = "#757575", "Villain" = "#C62828")

    plot_ly(dist_summary,
            labels = ~type,
            values = ~total,
            type = "pie",
            marker = list(colors = unname(color_map[levels(dist_summary$type)])),
            sort = FALSE) %>%
      layout(title = "Screen Time by Character Type")
  })
  
  output$overview_by_season <- renderPlotly({
    season_data <- show_screen_time %>%
      filter(character_name %in% main_cast) %>%
      group_by(season, character_name) %>%
      summarise(total_hours = sum(total_screen_time_min) / 60, .groups = "drop")
    
    plot_ly(season_data,
            x = ~character_name,
            y = ~total_hours,
            color = ~as.factor(season),
            type = "bar",
            colors = c("#43A047", "#1E88E5", "#E53935")) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Hours"),
        barmode = "group",
        legend = list(title = list(text = "Season"))
      )
  })
  
  # Main cast circular visualization
  output$maincast_circles <- renderPlot({
    main_data <- filtered_character_totals() %>%
      filter(character_name %in% main_cast) %>%
      arrange(desc(total_screen_time_hours))
    
    # Use total runtime for comparison (not max character)
    plots <- map(1:nrow(main_data), function(i) {
      char <- main_data$character_name[i]
      hours <- main_data$total_screen_time_hours[i]
      color <- main_cast_colors[char]
      create_character_circle(char, hours, total_runtime_hours, color)
    })
    
    # Arrange in grid
    wrap_plots(plots, ncol = 4)
  })
  
  # Main cast bars
  output$maincast_bars <- renderPlotly({
    main_data <- filtered_character_totals() %>%
      filter(character_name %in% main_cast) %>%
      arrange(desc(total_screen_time_hours))
    
    colors <- sapply(main_data$character_name, function(x) main_cast_colors[x])
    
    plot_ly(main_data,
            x = ~total_screen_time_hours,
            y = ~reorder(character_name, total_screen_time_hours),
            type = "bar",
            orientation = "h",
            marker = list(color = colors),
            text = ~sprintf("%.1fh", total_screen_time_hours),
            textposition = "outside") %>%
      layout(
        xaxis = list(title = "Hours", range = c(0, 12)),
        yaxis = list(title = ""),
        margin = list(l = 120, r = 60)
      )
  })
  
  output$maincast_episodes <- renderPlotly({
    main_data <- filtered_character_totals() %>%
      filter(character_name %in% main_cast) %>%
      arrange(desc(episodes_appeared))
    
    plot_ly(main_data,
            x = ~episodes_appeared,
            y = ~reorder(character_name, episodes_appeared),
            type = "bar",
            orientation = "h",
            marker = list(color = "#43A047"),
            text = ~episodes_appeared,
            textposition = "outside") %>%
      layout(
        xaxis = list(title = "Episodes"),
        yaxis = list(title = ""),
        margin = list(l = 120)
      )
  })
  
  # Percy's decline across seasons
  output$percy_decline <- renderPlotly({
    # Get all main cast screen time by season
    cast_by_season <- show_screen_time %>%
      filter(character_name %in% main_cast) %>%
      group_by(season, character_name) %>%
      summarise(total_minutes = sum(total_screen_time_min), .groups = "drop") %>%
      arrange(season, desc(total_minutes)) %>%
      group_by(season) %>%
      mutate(rank = row_number()) %>%
      ungroup()
    
    # Highlight Percy
    percy_data <- cast_by_season %>% filter(character_name == "Percival")
    others_data <- cast_by_season %>% filter(character_name != "Percival")
    
    plot_ly() %>%
      # Other characters (grey)
      add_trace(data = others_data,
                x = ~season,
                y = ~total_minutes,
                color = ~character_name,
                colors = c("#BDBDBD", "#E0E0E0", "#9E9E9E", "#757575", "#EEEEEE", "#616161"),
                type = "scatter",
                mode = "lines+markers",
                line = list(width = 2),
                marker = list(size = 8),
                opacity = 0.4,
                name = ~character_name,
                hovertemplate = paste('<b>%{fullData.name}</b><br>',
                                      'Season: %{x}<br>',
                                      'Screen Time: %{y:.0f} min<br>',
                                      '<extra></extra>')) %>%
      # Percy (highlighted in his color)
      add_trace(data = percy_data,
                x = ~season,
                y = ~total_minutes,
                type = "scatter",
                mode = "lines+markers",
                line = list(color = main_cast_colors["Percival"], width = 4),
                marker = list(color = main_cast_colors["Percival"], size = 14,
                              line = list(color = "white", width = 2)),
                name = "Percival",
                hovertemplate = paste('<b>Percival de Rolo</b><br>',
                                      'Season: %{x}<br>',
                                      'Screen Time: %{y:.0f} min<br>',
                                      'Rank: #', percy_data$rank, '<br>',
                                      '<extra></extra>')) %>%
      # Add rank annotations for Percy
      add_annotations(
        data = percy_data,
        x = ~season,
        y = ~total_minutes,
        text = ~paste("#", rank),
        showarrow = TRUE,
        arrowhead = 2,
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = main_cast_colors["Percival"],
        ax = 30,
        ay = -40,
        font = list(size = 14, color = main_cast_colors["Percival"], family = "Arial Black")
      ) %>%
      layout(
        xaxis = list(title = "Season", 
                     tickvals = c(1, 2, 3),
                     ticktext = c("Season 1\n(Briarwood Arc)", 
                                  "Season 2\n(Chroma Conclave)", 
                                  "Season 3\n(Thordak)")),
        yaxis = list(title = "Screen Time (minutes)"),
        showlegend = TRUE,
        legend = list(x = 1.02, y = 1),
        hovermode = "closest"
      )
  })
  
  # ===== STORY TIMELINE =====
  output$story_timeline <- renderPlot({
    # Get screen time data for ALL main cast
    all_cast_data <- show_screen_time %>%
      filter(character_name %in% main_cast) %>%
      mutate(episode_global = case_when(
        season == 1 ~ episode,
        season == 2 ~ episode + 12,
        season == 3 ~ episode + 24
      )) %>%
      group_by(episode_global, character_name) %>%
      summarise(screen_time = mean(total_screen_time_min), .groups = "drop")

    # Apply a rolling mean to smooth out episode-to-episode spikes
    all_cast_smooth <- all_cast_data %>%
      group_by(character_name) %>%
      arrange(episode_global) %>%
      mutate(screen_time = zoo::rollmean(screen_time, k = 3, fill = NA, align = "center")) %>%
      ungroup() %>%
      filter(!is.na(screen_time))

    # Compute a sensible y-axis ceiling (round up to nearest 5)
    y_max <- ceiling(max(all_cast_smooth$screen_time, na.rm = TRUE) / 5) * 5

    # Build the plot using actual screen time (minutes) on y-axis
    p <- ggplot() +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"),
        plot.margin = margin(55, 20, 55, 20),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      ) +
      scale_x_continuous(
        name = "Episode (Global)",
        breaks = c(1, 6, 12.5, 18, 24.5, 30, 36),
        labels = c("Ep 1", "Ep 6", "S1 → S2", "Ep 18", "S2 → S3", "Ep 30", "Ep 36")
      ) +
      scale_y_continuous(
        name = "Screen Time (minutes, 3-ep rolling avg)",
        limits = c(0, y_max),
        breaks = seq(0, y_max, by = 5)
      ) +
      coord_cartesian(xlim = c(1, 36), clip = "off")

    # Season background bands
    p <- p +
      annotate("rect", xmin = 0.5, xmax = 12.5, ymin = 0, ymax = y_max,
               fill = "#43A047", alpha = 0.06) +
      annotate("rect", xmin = 12.5, xmax = 24.5, ymin = 0, ymax = y_max,
               fill = "#1E88E5", alpha = 0.06) +
      annotate("rect", xmin = 24.5, xmax = 36.5, ymin = 0, ymax = y_max,
               fill = "#E53935", alpha = 0.06)

    # Season divider lines
    p <- p +
      geom_vline(xintercept = 12.5, linetype = "dashed", color = "grey50", linewidth = 0.7) +
      geom_vline(xintercept = 24.5, linetype = "dashed", color = "grey50", linewidth = 0.7)

    # Season labels placed ABOVE the plot panel (outside via clip = "off")
    p <- p +
      annotate("text", x = 6.5, y = y_max * 1.12,
               label = "Season 1", size = 5.5, fontface = "bold",
               color = "#43A047", hjust = 0.5) +
      annotate("text", x = 18.5, y = y_max * 1.12,
               label = "Season 2", size = 5.5, fontface = "bold",
               color = "#1E88E5", hjust = 0.5) +
      annotate("text", x = 30.5, y = y_max * 1.12,
               label = "Season 3", size = 5.5, fontface = "bold",
               color = "#E53935", hjust = 0.5)

    # Add all character lines in grey first (base layer)
    for (char in main_cast) {
      char_data <- all_cast_smooth %>% filter(character_name == char)
      if (nrow(char_data) > 0) {
        p <- p + geom_line(data = char_data,
                           aes(x = episode_global, y = screen_time),
                           color = "grey75", linewidth = 1.2, alpha = 0.6)
      }
    }

    # Highlighted character lines
    highlighted <- list(
      list(char = "Percival",   episodes = 1:12,  label = "Percy (S1 Focus)"),
      list(char = "Pike",       episodes = 1:12,  label = "Pike (S1 Reduced)"),
      list(char = "Vex'ahlia",  episodes = 13:36, label = "Vex (S2-3 Growth)"),
      list(char = "Vax'ildan",  episodes = 13:36, label = "Vax (S2-3 Growth)"),
      list(char = "Keyleth",    episodes = 13:36, label = "Keyleth (S2-3 Growth)")
    )

    for (h in highlighted) {
      seg_data <- all_cast_smooth %>%
        filter(character_name == h$char, episode_global %in% h$episodes)
      if (nrow(seg_data) > 0) {
        p <- p + geom_line(data = seg_data,
                           aes(x = episode_global, y = screen_time),
                           color = main_cast_colors[h$char],
                           linewidth = 2.2, alpha = 0.95)
      }
    }

    # Key story event markers (vertical lines only, minimal text)
    timeline_events <- data.frame(
      episode = c(6, 8, 17, 34),
      label    = c("Pike's Journey", "Percy's List\nRevealed",
                   "Vestige Hunt\nSplit", "Percy's\nSacrifice"),
      color    = c("#FFB300", "#37474F", "#1E88E5", "#37474F"),
      vjust    = c(-0.4, -0.4, -0.4, -0.4),
      stringsAsFactors = FALSE
    )

    # Label y positions: first 3 events annotate above data, Percy's Sacrifice (ep 34)
    # goes BELOW the panel since the line chart is filled in at that point.
    # Negative y values work because coord_cartesian(clip = "off") is set.
    label_y      <- c(y_max * 0.88, y_max * 0.72, y_max * 0.88, -y_max * 0.18)
    arrow_tip_y  <- c(y_max * 0.60, y_max * 0.52, y_max * 0.60, -y_max * 0.04)

    for (i in seq_len(nrow(timeline_events))) {
      ev <- timeline_events[i, ]

      # For Percy's Sacrifice the arrow points UP from label (below) toward the line
      arr <- if (i == 4) {
        arrow(length = unit(0.2, "cm"), type = "closed", ends = "first")
      } else {
        arrow(length = unit(0.2, "cm"), type = "closed")
      }

      p <- p +
        geom_vline(xintercept = ev$episode, color = ev$color,
                   linetype = "dotted", linewidth = 1.0, alpha = 0.8) +
        annotate("text", x = ev$episode, y = label_y[i],
                 label = ev$label, color = ev$color,
                 size = 4.2, hjust = 0.5, fontface = "bold", lineheight = 0.9) +
        annotate("segment",
                 x = ev$episode, xend = ev$episode,
                 y = if (i == 4) label_y[i] + y_max * 0.07 else label_y[i] - y_max * 0.07,
                 yend = arrow_tip_y[i],
                 color = ev$color, linewidth = 0.7,
                 arrow = arr)
    }

    # Legend for highlighted characters
    legend_chars <- c("Percival", "Pike", "Vex'ahlia", "Vax'ildan", "Keyleth")
    legend_labels <- c("Percy (S1)", "Pike (S1)", "Vex (S2-3)", "Vax (S2-3)", "Keyleth (S2-3)")
    legend_data <- all_cast_smooth %>%
      filter(character_name %in% legend_chars) %>%
      mutate(legend_label = legend_labels[match(character_name, legend_chars)])

    p <- p +
      scale_color_manual(
        name = "Highlighted",
        values = setNames(main_cast_colors[legend_chars], legend_labels),
        guide = guide_legend(override.aes = list(linewidth = 2))
      )

    return(p)
  }, height = 700)
  
  # Growth trends visualization - showing rising screen time
  output$growth_trends <- renderPlotly({
    # Calculate screen time by season for the rising trio
    growth_data <- show_screen_time %>%
      filter(character_name %in% c("Vex'ahlia", "Vax'ildan", "Keyleth")) %>%
      group_by(season, character_name) %>%
      summarise(
        total_minutes = sum(total_screen_time_min),
        avg_per_episode = mean(total_screen_time_min),
        .groups = "drop"
      )
    
    # Calculate percentage increase from Season 1 to Season 3
    growth_summary <- growth_data %>%
      group_by(character_name) %>%
      arrange(season) %>%
      summarise(
        s1_avg = first(avg_per_episode),
        s3_avg = last(avg_per_episode),
        pct_growth = ((last(avg_per_episode) - first(avg_per_episode)) / first(avg_per_episode)) * 100,
        .groups = "drop"
      )
    
    plot_ly() %>%
      add_trace(
        data = growth_data %>% filter(character_name == "Vex'ahlia"),
        x = ~season,
        y = ~avg_per_episode,
        type = "scatter",
        mode = "lines+markers",
        name = sprintf("Vex'ahlia (+%.0f%%)", growth_summary$pct_growth[growth_summary$character_name == "Vex'ahlia"]),
        line = list(width = 4, color = main_cast_colors["Vex'ahlia"]),
        marker = list(size = 12, color = main_cast_colors["Vex'ahlia"],
                      line = list(color = "white", width = 2)),
        hovertemplate = paste('<b>Vex\'ahlia</b><br>',
                              'Season: %{x}<br>',
                              'Avg: %{y:.1f} min/episode<br>',
                              '<extra></extra>')
      ) %>%
      add_trace(
        data = growth_data %>% filter(character_name == "Vax'ildan"),
        x = ~season,
        y = ~avg_per_episode,
        type = "scatter",
        mode = "lines+markers",
        name = sprintf("Vax'ildan (+%.0f%%)", growth_summary$pct_growth[growth_summary$character_name == "Vax'ildan"]),
        line = list(width = 4, color = main_cast_colors["Vax'ildan"]),
        marker = list(size = 12, color = main_cast_colors["Vax'ildan"],
                      line = list(color = "white", width = 2)),
        hovertemplate = paste('<b>Vax\'ildan</b><br>',
                              'Season: %{x}<br>',
                              'Avg: %{y:.1f} min/episode<br>',
                              '<extra></extra>')
      ) %>%
      add_trace(
        data = growth_data %>% filter(character_name == "Keyleth"),
        x = ~season,
        y = ~avg_per_episode,
        type = "scatter",
        mode = "lines+markers",
        name = sprintf("Keyleth (+%.0f%%)", growth_summary$pct_growth[growth_summary$character_name == "Keyleth"]),
        line = list(width = 4, color = main_cast_colors["Keyleth"]),
        marker = list(size = 12, color = main_cast_colors["Keyleth"],
                      line = list(color = "white", width = 2)),
        hovertemplate = paste('<b>Keyleth</b><br>',
                              'Season: %{x}<br>',
                              'Avg: %{y:.1f} min/episode<br>',
                              '<extra></extra>')
      ) %>%
      layout(
        title = list(
          text = "<b>The Rising Stars:</b> Screen Time Growth Across Seasons",
          font = list(size = 14)
        ),
        xaxis = list(
          title = "Season",
          tickvals = c(1, 2, 3),
          ticktext = c("Season 1", "Season 2", "Season 3")
        ),
        yaxis = list(
          title = "Average Screen Time per Episode (minutes)",
          range = c(15, 20)
        ),
        showlegend = TRUE,
        legend = list(
          x = 0.02,
          y = 0.98,
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "grey",
          borderwidth = 1
        ),
        hovermode = "closest",
        annotations = list(
          list(
            x = 0.5,
            y = -0.15,
            xref = "paper",
            yref = "paper",
            text = "Percentage shows total growth from Season 1 to Season 3",
            showarrow = FALSE,
            font = list(size = 11, color = "grey40")
          )
        )
      )
  })
  
  # Cast vs Villains combined comparison
  output$cast_vs_villains <- renderPlotly({
    cast_data <- filtered_character_totals() %>%
      filter(character_name %in% main_cast) %>%
      mutate(group = "Main Cast")

    villain_data <- filtered_character_totals() %>%
      filter(character_name %in% villains) %>%
      mutate(group = "Villain")

    combined <- bind_rows(cast_data, villain_data) %>%
      arrange(desc(total_screen_time_hours))

    colors_vec <- sapply(combined$character_name, function(char) {
      if (char %in% names(main_cast_colors)) main_cast_colors[char]
      else if (char %in% names(villain_colors)) villain_colors[char]
      else "#757575"
    })

    plot_ly(combined,
            x = ~reorder(character_name, -total_screen_time_hours),
            y = ~total_screen_time_hours,
            type = "bar",
            marker = list(color = colors_vec,
                          line = list(color = "white", width = 1)),
            text = ~sprintf("%.1fh", total_screen_time_hours),
            textposition = "outside",
            hovertemplate = paste('<b>%{x}</b><br>',
                                  'Screen Time: %{y:.1f} hours<br>',
                                  '<extra></extra>')) %>%
      layout(
        xaxis = list(title = "", tickangle = -35),
        yaxis = list(title = "Total Screen Time (hours)"),
        shapes = list(
          list(type = "line",
               x0 = 6.5, x1 = 6.5,
               y0 = 0, y1 = 1,
               yref = "paper",
               line = list(color = "grey50", width = 2, dash = "dot"))
        ),
        annotations = list(
          list(x = 3, y = 1.05, xref = "x", yref = "paper",
               text = "Main Cast", showarrow = FALSE,
               font = list(size = 13, color = "#1565C0", family = "Arial Black")),
          list(x = 9.5, y = 1.05, xref = "x", yref = "paper",
               text = "Villains", showarrow = FALSE,
               font = list(size = 13, color = "#C62828", family = "Arial Black"))
        ),
        margin = list(t = 60, b = 100)
      )
  })

  # Villain circular visualization
  output$villain_circles <- renderPlot({
    villain_data <- filtered_character_totals() %>%
      filter(character_name %in% villains) %>%
      arrange(desc(total_screen_time_hours))
    
    if (nrow(villain_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "No villain data for selected filter", size = 8) +
               theme_void())
    }
    
    # Use total runtime for comparison (not max character)
    plots <- map(1:nrow(villain_data), function(i) {
      char <- villain_data$character_name[i]
      hours <- villain_data$total_screen_time_hours[i]
      color <- villain_colors[char]
      create_character_circle(char, hours, total_runtime_hours, color)
    })

    wrap_plots(plots, ncol = 4)
  })
  
  output$villain_bars <- renderPlotly({
    villain_data <- filtered_character_totals() %>%
      filter(character_name %in% villains) %>%
      arrange(desc(total_screen_time_hours))
    
    if (nrow(villain_data) == 0) return(plotly_empty())
    
    colors <- sapply(villain_data$character_name, function(x) villain_colors[x])
    
    plot_ly(villain_data,
            x = ~total_screen_time_hours,
            y = ~reorder(character_name, total_screen_time_hours),
            type = "bar",
            orientation = "h",
            marker = list(color = colors),
            text = ~sprintf("%.1fh", total_screen_time_hours),
            textposition = "outside") %>%
      layout(
        xaxis = list(title = "Hours"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # Improved villain timeline
  output$villain_timeline <- renderPlotly({
    villain_eps <- show_screen_time %>%
      filter(character_name %in% villains) %>%
      mutate(episode_global = case_when(
        season == 1 ~ episode,
        season == 2 ~ episode + 12,
        season == 3 ~ episode + 24
      )) %>%
      group_by(character_name) %>%
      arrange(episode_global) %>%
      ungroup()

    if (nrow(villain_eps) == 0) return(plotly_empty())

    villain_summary <- villain_eps %>%
      group_by(character_name, episode_global) %>%
      summarise(screen_time = sum(total_screen_time_min), .groups = "drop")

    # Build one trace per villain for consistent coloring
    p <- plot_ly()
    for (vchar in villains) {
      vdata <- villain_summary %>% filter(character_name == vchar)
      if (nrow(vdata) == 0) next
      vcol <- villain_colors[vchar]
      p <- p %>% add_trace(
        data = vdata,
        x = ~episode_global,
        y = ~character_name,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = ~pmin(8 + screen_time / 3, 22),
          color = vcol,
          line = list(color = "white", width = 1.5),
          opacity = 0.85
        ),
        name = vchar,
        hovertemplate = paste0('<b>', vchar, '</b><br>',
                               'Episode: %{x}<br>',
                               'Screen time: %{customdata:.1f} min<extra></extra>'),
        customdata = ~screen_time
      )
    }

    p %>% layout(
      xaxis = list(
        title = "Episode (Global)",
        range = c(0, 37),
        showgrid = TRUE,
        gridcolor = "rgba(200, 200, 200, 0.4)",
        dtick = 4
      ),
      yaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = rev(villains)
      ),
      showlegend = FALSE,
      plot_bgcolor = "rgba(248, 248, 248, 0.6)",
      margin = list(l = 130)
    )
  })
  
  # Episode heatmap
  output$episode_heatmap <- renderPlotly({
    heatmap_data <- filtered_screen_time() %>%
      filter(character_name %in% main_cast) %>%
      mutate(episode_global = case_when(
        season == 1 ~ episode,
        season == 2 ~ episode + 12,
        season == 3 ~ episode + 24
      )) %>%
      group_by(character_name, episode_global) %>%
      summarise(screen_time = sum(total_screen_time_min), .groups = "drop")
    
    plot_ly(heatmap_data,
            x = ~episode_global,
            y = ~character_name,
            z = ~screen_time,
            type = "heatmap",
            colors = colorRamp(c("#FFF3E0", "#E65100"))) %>%
      layout(
        xaxis = list(title = "Episode"),
        yaxis = list(title = "")
      )
  })
  
  output$episode_timeline <- renderPlotly({
    timeline_data <- filtered_screen_time() %>%
      filter(character_name %in% main_cast) %>%
      mutate(episode_global = case_when(
        season == 1 ~ episode,
        season == 2 ~ episode + 12,
        season == 3 ~ episode + 24
      )) %>%
      arrange(character_name, season, episode)

    # Apply 3-episode rolling mean to smooth out spikes
    timeline_smooth <- timeline_data %>%
      group_by(character_name) %>%
      arrange(episode_global) %>%
      mutate(total_screen_time_min = zoo::rollmean(total_screen_time_min, k = 3,
                                                    fill = NA, align = "center")) %>%
      ungroup()

    # Add NA rows to break lines between seasons for each character
    timeline_breaks <- timeline_smooth %>%
      group_by(character_name, season) %>%
      slice_tail(n = 1) %>%
      mutate(
        episode_global = episode_global + 0.5,
        total_screen_time_min = NA
      ) %>%
      ungroup()

    timeline_with_breaks <- bind_rows(timeline_smooth, timeline_breaks) %>%
      arrange(character_name, episode_global)

    y_max <- ceiling(max(timeline_smooth$total_screen_time_min, na.rm = TRUE) / 5) * 5 + 5

    plot_ly(timeline_with_breaks,
            x = ~episode_global,
            y = ~total_screen_time_min,
            color = ~character_name,
            colors = main_cast_colors,
            type = "scatter",
            mode = "lines",
            connectgaps = FALSE,
            hoverinfo = "text",
            text = ~paste(character_name, "<br>Episode:", episode_global,
                          "<br>Screen Time (smoothed):", round(total_screen_time_min, 1), "min")) %>%
      layout(
        xaxis = list(title = "Episode (Global)"),
        yaxis = list(title = "Screen Time (minutes, 3-ep rolling avg)", range = c(0, y_max)),
        hovermode = "closest"
      )
  })
  
  # Comparison
  output$comparison_plot <- renderPlotly({

    # Helper: build smoothed episode data for one character
    build_char_data <- function(char_name) {
      show_screen_time %>%
        filter(character_name == char_name) %>%
        mutate(episode_global = case_when(
          season == 1 ~ episode,
          season == 2 ~ episode + 12,
          season == 3 ~ episode + 24
        )) %>%
        arrange(season, episode) %>%
        group_by(character_name) %>%
        mutate(smoothed = zoo::rollmean(total_screen_time_min, k = 3,
                                        fill = NA, align = "center")) %>%
        ungroup()
    }

    char1_data <- build_char_data(input$compare_char1)
    char2_data <- build_char_data(input$compare_char2)

    # Resolve color per character (hero palette → villain palette → grey)
    all_colors <- c(main_cast_colors, villain_colors)
    get_color <- function(name) {
      if (name %in% names(all_colors)) unname(all_colors[name]) else "#757575"
    }
    c1_color <- get_color(input$compare_char1)
    c2_color <- get_color(input$compare_char2)

    # y-axis ceiling anchored at 0
    y_max <- ceiling(max(c(char1_data$total_screen_time_min,
                           char2_data$total_screen_time_min), na.rm = TRUE) / 5) * 5 + 5

    # Season divider shapes
    season_shapes <- list(
      list(type = "rect", x0 = 0.5, x1 = 12.5, y0 = 0, y1 = 1,
           yref = "paper", fillcolor = "#43A047", opacity = 0.06,
           line = list(width = 0)),
      list(type = "rect", x0 = 12.5, x1 = 24.5, y0 = 0, y1 = 1,
           yref = "paper", fillcolor = "#1E88E5", opacity = 0.06,
           line = list(width = 0)),
      list(type = "rect", x0 = 24.5, x1 = 36.5, y0 = 0, y1 = 1,
           yref = "paper", fillcolor = "#E53935", opacity = 0.06,
           line = list(width = 0)),
      list(type = "line", x0 = 12.5, x1 = 12.5, y0 = 0, y1 = 1,
           yref = "paper", line = list(color = "grey60", width = 1.5, dash = "dash")),
      list(type = "line", x0 = 24.5, x1 = 24.5, y0 = 0, y1 = 1,
           yref = "paper", line = list(color = "grey60", width = 1.5, dash = "dash"))
    )

    season_annotations <- list(
      list(x = 6.5, y = 1.04, xref = "x", yref = "paper",
           text = "<b>Season 1</b>", showarrow = FALSE,
           font = list(size = 12, color = "#43A047"), xanchor = "center"),
      list(x = 18.5, y = 1.04, xref = "x", yref = "paper",
           text = "<b>Season 2</b>", showarrow = FALSE,
           font = list(size = 12, color = "#1E88E5"), xanchor = "center"),
      list(x = 30.5, y = 1.04, xref = "x", yref = "paper",
           text = "<b>Season 3</b>", showarrow = FALSE,
           font = list(size = 12, color = "#E53935"), xanchor = "center")
    )

    plot_ly() %>%
      # Raw (faint) lines
      add_trace(data = char1_data,
                x = ~episode_global, y = ~total_screen_time_min,
                name = paste(input$compare_char1, "(raw)"),
                type = "scatter", mode = "lines",
                line = list(color = c1_color, width = 1, dash = "dot"),
                opacity = 0.3, connectgaps = FALSE,
                hoverinfo = "skip", showlegend = FALSE) %>%
      add_trace(data = char2_data,
                x = ~episode_global, y = ~total_screen_time_min,
                name = paste(input$compare_char2, "(raw)"),
                type = "scatter", mode = "lines",
                line = list(color = c2_color, width = 1, dash = "dot"),
                opacity = 0.3, connectgaps = FALSE,
                hoverinfo = "skip", showlegend = FALSE) %>%
      # Smoothed lines
      add_trace(data = char1_data,
                x = ~episode_global, y = ~smoothed,
                name = input$compare_char1,
                type = "scatter", mode = "lines+markers",
                line = list(color = c1_color, width = 3),
                marker = list(color = c1_color, size = 7,
                              line = list(color = "white", width = 1.5)),
                connectgaps = FALSE,
                hoverinfo = "text",
                text = ~paste0("<b>", input$compare_char1, "</b><br>",
                               "Episode: ", episode_global, "<br>",
                               "Smoothed: ", round(smoothed, 1), " min<br>",
                               "Raw: ", round(total_screen_time_min, 1), " min")) %>%
      add_trace(data = char2_data,
                x = ~episode_global, y = ~smoothed,
                name = input$compare_char2,
                type = "scatter", mode = "lines+markers",
                line = list(color = c2_color, width = 3),
                marker = list(color = c2_color, size = 7,
                              line = list(color = "white", width = 1.5)),
                connectgaps = FALSE,
                hoverinfo = "text",
                text = ~paste0("<b>", input$compare_char2, "</b><br>",
                               "Episode: ", episode_global, "<br>",
                               "Smoothed: ", round(smoothed, 1), " min<br>",
                               "Raw: ", round(total_screen_time_min, 1), " min")) %>%
      layout(
        xaxis = list(
          title = "Episode (Global)",
          range = c(0.5, 36.5),
          tickvals = c(1, 6, 12, 13, 18, 24, 25, 30, 36),
          ticktext = c("Ep 1", "Ep 6", "S1 End", "S2 Start",
                       "Ep 18", "S2 End", "S3 Start", "Ep 30", "Ep 36"),
          showgrid = TRUE,
          gridcolor = "rgba(200,200,200,0.3)"
        ),
        yaxis = list(
          title = "Screen Time (minutes, 3-ep rolling avg)",
          range = c(0, y_max),
          showgrid = TRUE,
          gridcolor = "rgba(200,200,200,0.3)"
        ),
        shapes = season_shapes,
        annotations = season_annotations,
        hovermode = "closest",
        legend = list(
          x = 0.02, y = 0.98,
          bgcolor = "rgba(255,255,255,0.85)",
          bordercolor = "grey80", borderwidth = 1
        ),
        margin = list(t = 50),
        plot_bgcolor = "rgba(252,252,252,1)",
        paper_bgcolor = "rgba(255,255,255,1)"
      )
  })
  
  # Villains scatter plot
  output$villains_scatter <- renderPlotly({
    villains_data <- filtered_character_totals() %>%
      filter(character_name %in% villains)
    
    if (nrow(villains_data) == 0) return(plotly_empty())
    
    colors_vec <- sapply(villains_data$character_name, function(x) villain_colors[x])
    
    plot_ly(villains_data,
            x = ~episodes_appeared,
            y = ~total_screen_time_hours,
            type = "scatter",
            mode = "markers",
            marker = list(size = 15, color = colors_vec,
                          line = list(color = "white", width = 2)),
            text = ~character_name,
            hoverinfo = "text",
            hovertext = ~paste("<b>", character_name, "</b><br>",
                               "Episodes:", episodes_appeared, "<br>",
                               "Total Time:", round(total_screen_time_hours, 1), "hrs")) %>%
      add_text(textposition = "top center",
               textfont = list(size = 11, color = "black", family = "Arial Black"),
               showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "Episodes Appeared", range = c(0, max(villains_data$episodes_appeared) + 2)),
        yaxis = list(title = "Total Screen Time (hours)"),
        hovermode = "closest",
        showlegend = FALSE
      )
  })
  
  # Data table
  output$data_table <- renderDT({
    filtered_character_totals() %>%
      arrange(desc(total_screen_time_hours)) %>%
      mutate(
        total_screen_time_hours = round(total_screen_time_hours, 2),
        pct_of_show = round((total_screen_time_hours / total_runtime_hours) * 100, 1)
      ) %>%
      select(character_name, actors, episodes_appeared, 
             total_screen_time_hours, pct_of_show)
  }, options = list(pageLength = 20))
  
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)