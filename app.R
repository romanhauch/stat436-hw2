install.packages(c("shiny", "tidyverse", "plotly", "bslib"))

library(shiny)
library(tidyverse)
library(plotly)
library(bslib)

# ---- Data Loading & Preparation ----
url <- "https://raw.githubusercontent.com/romanhauch/stat436-hw2/refs/heads/main/skaters.csv"
skaters_raw <- read_csv(url, show_col_types = FALSE)

# Full team name mapping for readability
team_names <- c(
  "ANA" = "Anaheim Ducks", "BOS" = "Boston Bruins", "BUF" = "Buffalo Sabres",
  "CAR" = "Carolina Hurricanes", "CBJ" = "Columbus Blue Jackets",
  "CGY" = "Calgary Flames", "CHI" = "Chicago Blackhawks",
  "COL" = "Colorado Avalanche", "DAL" = "Dallas Stars",
  "DET" = "Detroit Red Wings", "EDM" = "Edmonton Oilers",
  "FLA" = "Florida Panthers", "LAK" = "Los Angeles Kings",
  "MIN" = "Minnesota Wild", "MTL" = "Montreal Canadiens",
  "NJD" = "New Jersey Devils", "NSH" = "Nashville Predators",
  "NYI" = "New York Islanders", "NYR" = "New York Rangers",
  "OTT" = "Ottawa Senators", "PHI" = "Philadelphia Flyers",
  "PIT" = "Pittsburgh Penguins", "SEA" = "Seattle Kraken",
  "SJS" = "San Jose Sharks", "STL" = "St. Louis Blues",
  "TBL" = "Tampa Bay Lightning", "TOR" = "Toronto Maple Leafs",
  "UTA" = "Utah Hockey Club", "VAN" = "Vancouver Canucks",
  "VGK" = "Vegas Golden Knights", "WPG" = "Winnipeg Jets",
  "WSH" = "Washington Capitals"
)

position_labels <- c("C" = "Center", "D" = "Defense", "L" = "Left Wing", "R" = "Right Wing")

# Filter to 'all' situations and compute derived columns
skaters <- skaters_raw |>
  filter(situation == "all", games_played >= 10) |>
  mutate(
    team_full = team_names[team],
    position_full = position_labels[position],
    goals_above_expected = I_F_goals - I_F_xGoals,
    shooting_pct = ifelse(I_F_shotsOnGoal > 0, I_F_goals / I_F_shotsOnGoal * 100, 0),
    xGoals_per_game = I_F_xGoals / games_played,
    goals_per_game = I_F_goals / games_played,
    high_danger_pct = ifelse(
      (I_F_lowDangerShots + I_F_mediumDangerShots + I_F_highDangerShots) > 0,
      I_F_highDangerShots / (I_F_lowDangerShots + I_F_mediumDangerShots + I_F_highDangerShots) * 100,
      0
    ),
    luck_label = case_when(
      goals_above_expected > 2 ~ "Overperforming",
      goals_above_expected < -2 ~ "Underperforming",
      TRUE ~ "As Expected"
    )
  )

# ---- Helper Functions ----

# Build the main scatter plot (Goals vs xGoals)
build_scatter <- function(data, color_var) {
  p <- ggplot(data, aes(
    x = I_F_xGoals, y = I_F_goals,
    text = paste0(
      "<b>", name, "</b> (", team, ")<br>",
      "Position: ", position_full, "<br>",
      "Games: ", games_played, "<br>",
      "Goals: ", I_F_goals, "<br>",
      "xGoals: ", round(I_F_xGoals, 1), "<br>",
      "Difference: ", ifelse(goals_above_expected > 0, "+", ""),
      round(goals_above_expected, 1)
    ),
    color = .data[[color_var]],
    key = name
  )) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_point(alpha = 0.7, size = 2.5) +
    annotate("text", x = max(data$I_F_xGoals) * 0.95, y = max(data$I_F_xGoals) * 0.85,
             label = "Lucky →", color = "gray40", size = 3, fontface = "italic") +
    annotate("text", x = max(data$I_F_xGoals) * 0.75, y = max(data$I_F_xGoals) * 1.0,
             label = "← Unlucky", color = "gray40", size = 3, fontface = "italic") +
    labs(
      x = "Expected Goals (xGoals)",
      y = "Actual Goals"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  if (color_var == "position_full") {
    p <- p + scale_color_brewer(palette = "Set2", name = "Position")
  } else if (color_var == "luck_label") {
    p <- p + scale_color_manual(
      values = c("Overperforming" = "#e63946", "As Expected" = "gray60", "Underperforming" = "#457b9d"),
      name = "Status"
    )
  } else {
    p <- p + scale_color_viridis_c(name = color_var)
  }
  
  ggplotly(p, tooltip = "text", source = "scatter") |>
    layout(dragmode = "select") |>
    config(displayModeBar = FALSE)
}

# Build the shot danger breakdown bar chart for selected players
build_danger_chart <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  danger_df <- data |>
    select(name, team, I_F_lowDangerShots, I_F_mediumDangerShots, I_F_highDangerShots) |>
    pivot_longer(
      cols = starts_with("I_F_") & ends_with("Shots"),
      names_to = "danger_level",
      values_to = "shots"
    ) |>
    mutate(
      danger_level = case_when(
        str_detect(danger_level, "low") ~ "Low Danger",
        str_detect(danger_level, "medium") ~ "Medium Danger",
        str_detect(danger_level, "high") ~ "High Danger"
      ),
      danger_level = factor(danger_level, levels = c("Low Danger", "Medium Danger", "High Danger")),
      player_label = paste0(name, " (", team, ")")
    )
  
  ggplot(danger_df, aes(x = reorder(player_label, shots, sum), y = shots, fill = danger_level)) +
    geom_col(position = "fill") +
    scale_fill_manual(
      values = c("Low Danger" = "#a8dadc", "Medium Danger" = "#f4a261", "High Danger" = "#e63946"),
      name = "Shot Danger"
    ) +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    labs(x = NULL, y = "Proportion of Shots") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

# Build the goals vs xGoals comparison for selected players
build_goal_comparison <- function(data) {
  if (nrow(data) == 0) return(NULL)
  
  comp_df <- data |>
    select(name, team, I_F_goals, I_F_xGoals) |>
    pivot_longer(cols = c(I_F_goals, I_F_xGoals), names_to = "type", values_to = "value") |>
    mutate(
      type = ifelse(type == "I_F_goals", "Actual Goals", "Expected Goals"),
      player_label = paste0(name, " (", team, ")")
    )
  
  ggplot(comp_df, aes(x = reorder(player_label, value, max), y = value, fill = type)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("Actual Goals" = "#e63946", "Expected Goals" = "#457b9d"), name = NULL) +
    coord_flip() +
    labs(x = NULL, y = "Goals") +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

# ---- UI ----
ui <- page_sidebar(
  title = "NHL Shooting Quality Analyzer",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 280,
    h5("Filters"),
    p("Explore which NHL skaters are outperforming or underperforming
       their expected goal totals based on shot quality.",
      style = "font-size: 0.85em; color: #666;"),
    hr(),
    
    selectInput("team", "Team",
                choices = c("All Teams" = "ALL", sort(setNames(names(team_names), team_names))),
                selected = "ALL"
    ),
    
    checkboxGroupInput("position", "Position",
                       choices = c("Center" = "C", "Defense" = "D", "Left Wing" = "L", "Right Wing" = "R"),
                       selected = c("C", "D", "L", "R")
    ),
    
    sliderInput("min_games", "Minimum Games Played",
                min = 10, max = 82, value = 20, step = 5
    ),
    
    radioButtons("color_by", "Color Points By",
                 choices = c("Position" = "position_full", "Luck Status" = "luck_label"),
                 selected = "position_full"
    ),
    
    hr(),
    p(tags$b("How to use:"), "Use the filters above to narrow the scatter plot.
      Then drag to select players on the scatter plot to see their shot danger
      breakdown and goals comparison below.",
      style = "font-size: 0.82em; color: #888;"),
    hr(),
    p("Data: ", tags$a("MoneyPuck.com", href = "https://moneypuck.com"), " | 2025-26 Season",
      style = "font-size: 0.78em; color: #999;")
  ),
  
  layout_columns(
    col_widths = 12,
    card(
      card_header(
        "Goals vs. Expected Goals",
        class = "bg-primary text-white"
      ),
      card_body(
        p("Each point is a skater. Points above the dashed line scored more goals
           than expected — points below scored fewer. Drag to select players for details.",
          style = "font-size: 0.85em; color: #666; margin-bottom: 0;"),
        plotlyOutput("scatter_plot", height = "480px")
      )
    )
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Shot Danger Breakdown"),
      card_body(
        p("Proportion of each selected player's shots by danger level.
           High danger shots (close to the net, rebounds) have the best chance of scoring.",
          style = "font-size: 0.83em; color: #666;"),
        plotOutput("danger_chart", height = "350px")
      )
    ),
    card(
      card_header("Actual vs. Expected Goals"),
      card_body(
        p("Compare each selected player's real goal total to what the xGoals model predicted.
           Large gaps suggest luck or elite finishing ability.",
          style = "font-size: 0.83em; color: #666;"),
        plotOutput("goal_comparison", height = "350px")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Reactive: filtered data based on sidebar inputs
  filtered_data <- reactive({
    df <- skaters |>
      filter(
        games_played >= input$min_games,
        position %in% input$position
      )
    if (input$team != "ALL") {
      df <- df |> filter(team == input$team)
    }
    df
  })
  
  # Reactive: players selected via plotly brush/lasso
  selected_players <- reactive({
    event <- event_data("plotly_selected", source = "scatter")
    if (is.null(event) || length(event$key) == 0) {
      return(filtered_data() |> slice_max(abs(goals_above_expected), n = 8))
    }
    filtered_data() |> filter(name %in% event$key)
  })
  
  # Main scatter plot
  output$scatter_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    build_scatter(filtered_data(), input$color_by)
  })
  
  # Shot danger breakdown
  output$danger_chart <- renderPlot({
    req(nrow(selected_players()) > 0)
    build_danger_chart(selected_players())
  })
  
  # Goals vs xGoals comparison
  output$goal_comparison <- renderPlot({
    req(nrow(selected_players()) > 0)
    build_goal_comparison(selected_players())
  })
}

shinyApp(ui, server)