# ==========================================
# 0. CONFIGURATION INITIALE
# ==========================================

if (!require("plotly", character.only = TRUE)) install.packages("plotly")
if (!require("shinyWidgets", character.only = TRUE)) install.packages("shinyWidgets")

library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(FactoMineR)
library(plotly)
library(shinyWidgets)

packages_requis <- c("shiny", "bslib", "tidyverse", "DT", "FactoMineR", "plotly", "shinyWidgets")
for (pkg in packages_requis) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ==========================================
# 1. PRÉPARATION ET AGRÉGATION DES DONNÉES
# ==========================================
data_matchs <- read.csv("data_premier_league.csv", stringsAsFactors = FALSE)

home_stats <- data_matchs %>%
  group_by(season, team = home_team) %>%
  summarise(
    matches_home        = n(),
    goals_scored_home   = sum(home_goals, na.rm = TRUE),
    goals_conceded_home = sum(away_goals, na.rm = TRUE),
    wins_home   = sum(result == "H", na.rm = TRUE),
    draws_home  = sum(result == "D", na.rm = TRUE),
    losses_home = sum(result == "A", na.rm = TRUE),
    .groups = "drop"
  )

away_stats <- data_matchs %>%
  group_by(season, team = away_team) %>%
  summarise(
    matches_away        = n(),
    goals_scored_away   = sum(away_goals, na.rm = TRUE),
    goals_conceded_away = sum(home_goals, na.rm = TRUE),
    wins_away   = sum(result == "A", na.rm = TRUE),
    draws_away  = sum(result == "D", na.rm = TRUE),
    losses_away = sum(result == "H", na.rm = TRUE),
    .groups = "drop"
  )

team_stats <- full_join(home_stats, away_stats, by = c("season", "team")) %>%
  mutate(
    matches         = matches_home + matches_away,
    goals_scored    = goals_scored_home + goals_scored_away,
    goals_conceded  = goals_conceded_home + goals_conceded_away,
    wins            = wins_home + wins_away,
    draws           = draws_home + draws_away,
    losses          = losses_home + losses_away,
    points          = wins * 3 + draws,
    goal_difference = goals_scored - goals_conceded
  ) %>%
  select(season, team, matches, goals_scored, goals_conceded,
         wins, draws, losses, points, goal_difference)

team_overall <- team_stats %>%
  group_by(team) %>%
  summarise(
    saisons_jouees  = n(),
    goals_scored    = sum(goals_scored),
    goals_conceded  = sum(goals_conceded),
    wins            = sum(wins),
    draws           = sum(draws),
    losses          = sum(losses),
    points          = sum(points),
    goal_difference = sum(goal_difference)
  ) %>%
  as.data.frame()

rownames(team_overall) <- team_overall$team

var_labels <- c(
  goals_scored    = "Buts Marqués",
  goals_conceded  = "Buts Encaissés",
  wins            = "Victoires",
  points          = "Points",
  goal_difference = "Différence de buts",
  draws           = "Matchs Nuls",
  losses          = "Défaites"
)

all_seasons <- sort(unique(team_stats$season))
liste_equipes_uniques <- sort(unique(team_stats$team))

# ==========================================
# 2. THÈME BSLIB
# ==========================================
pl_theme <- bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = "#38003c",
  secondary    = "#00ff85",
  success      = "#04f5ff",
  danger       = "#e90052",
  warning      = "#f5a623",
  bg           = "#f4f4f8",
  fg           = "#1a1a2e",
  base_font    = font_google("Inter"),
  heading_font = font_google("Barlow Condensed", wght = "700"),
  code_font    = font_google("Fira Code")
) %>%
  bs_add_rules("
    .navbar { background: linear-gradient(135deg, #38003c 0%, #1a0020 100%) !important; border-bottom: 3px solid #00ff85; }
    .navbar-brand { color: #ffffff !important; font-family: 'Barlow Condensed', sans-serif; font-size: 1.5rem; letter-spacing: 2px; text-transform: uppercase; }
    .navbar-nav .nav-link { color: #e0d6f5 !important; background: transparent !important; }
    .navbar-nav .nav-link.active, .navbar-nav .nav-item.active > .nav-link { color: #ffffff !important; background: transparent !important; border-bottom: 3px solid #00ff85 !important; font-weight: 700; }
    .navbar-nav .nav-link:hover { color: #00ff85 !important; background: transparent !important; }
    .bslib-sidebar-layout > .sidebar { background: #38003c !important; color: #fff; border-right: 2px solid #00ff85; }
    .bslib-sidebar-layout > .sidebar .form-label, .bslib-sidebar-layout > .sidebar label, .bslib-sidebar-layout > .sidebar h4, .bslib-sidebar-layout > .sidebar p { color: #e0d6f5 !important; }
    .bslib-sidebar-layout > .sidebar .form-select, .bslib-sidebar-layout > .sidebar .form-control { background: #50006b; color: #fff; border-color: #7b2d8b; }
    .bslib-sidebar-layout > .sidebar .form-select option { background: #38003c; }
    .sidebar-toggle { color: #00ff85 !important; }
    .irs-bar { background: #00ff85 !important; border-top: none !important; border-bottom: none !important; }
    .irs-line { background: rgba(255, 255, 255, 0.2) !important; border: none !important; }
    .irs-handle { background: #ffffff !important; border: 3px solid #00ff85 !important; }
    .irs-min, .irs-max, .irs-single, .irs-from, .irs-to, .irs-grid-text { color: #e0d6f5 !important; background: transparent !important; }
    .card { border: none; border-radius: 12px; box-shadow: 0 2px 16px rgba(56,0,60,0.10); transition: box-shadow 0.2s; }
    .card:hover { box-shadow: 0 6px 28px rgba(56,0,60,0.18); }
    .card-header { font-family: 'Barlow Condensed', sans-serif; font-size: 1.05rem; letter-spacing: 1.5px; text-transform: uppercase; }
    .bslib-value-box .value-box-showcase { font-size: 2.2rem !important; }
    .bslib-value-box { border-radius: 12px !important; }
    .insight-box { background: #faf8ff; border-left: 4px solid #e90052; padding: 12px 16px; margin-top: 10px; border-radius: 6px; font-size: 13.5px; color: #1a1a2e; line-height: 1.6; }
    .insight-box.warning { border-left-color: #f5a623; background: #fffdf5; }
    .insight-box.success { border-left-color: #00b894; background: #f3fdf9; }
    .insight-box.info    { border-left-color: #04f5ff; background: #f0fdff; }
    .nav-pills .nav-link.active { background: #38003c !important; color: #00ff85 !important; font-weight: 700; }
    .nav-pills .nav-link { color: #38003c; font-family: 'Barlow Condensed', sans-serif; letter-spacing: 1px; text-transform: uppercase; font-size: 0.95rem; }
    body { background: #f4f4f8; }
    hr { border-color: #7b2d8b44; }
    .bslib-full-screen-enter::after { content: 'Agrandir'; }
    .bslib-full-screen-exit::after  { content: 'Réduire'; }
  ")

# ==========================================
# 3. INTERFACE UTILISATEUR (UI)
# ==========================================
ui <- page_navbar(
  title = span(
    tags$img(src = "https://r2.thesportsdb.com/images/media/league/logo/4c377s1535214890.png", height = "70px", style = "margin-right:10px; vertical-align:middle;"),
    "Premier League Data"
  ),
  theme    = pl_theme, bg = "#38003c", inverse = TRUE, fillable = FALSE,
  
  # --- ONGLET 1 : PRESENTATION ---
  nav_panel(
    title = tagList(icon("circle-info"), " Présentation"), value = "intro",
    layout_columns(
      col_widths = 12,
      card(
        card_header("⚽ Bienvenue sur l'Explorateur de la Premier League", class = "bg-primary text-white"),
        card_body(
          h4("À propos de la base de données"),
          p("Cette base de données regroupe des informations détaillées sur 4 560 matchs du championnat anglais de football, la Premier League, couvrant une période de 12 saisons, de 2006 à 2018."),
          p("Pour chaque rencontre, plusieurs variables sont enregistrées afin de permettre une analyse complète des performances sportives. On y retrouve notamment les équipes opposées, le score final du match, ainsi que l’issue de la rencontre pour chacune des équipes (victoire, défaite ou match nul)."),
          p("Les résultats sont également distingués selon le lieu du match, en précisant la performance de l’équipe à domicile et celle de l’équipe à l’extérieur. Cette structuration permet d’étudier l’influence du facteur domicile, les dynamiques de performance des équipes, ainsi que les tendances globales du championnat sur la période étudiée."),
          
          h4("🧭 Organisation de l'application", style = "margin-top: 15px;"),
          p("L’application associée à cette base de données est organisée en plusieurs onglets permettant d’explorer les données de manière interactive :"),
          tags$ul(
            tags$li(HTML("<b>📊 Stats : Analyse statistique</b> <br> Cet onglet propose une analyse descriptive univariée des variables (répartition, fréquences, indicateurs statistiques) ainsi qu’une analyse bivariée permettant d’étudier les relations entre différentes variables.")),
            tags$li(HTML("<b>📈 Évolution : Comparaison entre clubs</b> <br> Il permet de comparer l’évolution des performances entre les différents clubs (modalités), sur la période étudiée, afin de mettre en évidence les tendances et les différences de niveau.")),
            tags$li(HTML("<b>🏟️ Dom / Ext : Domination domicile / extérieur</b> <br> Cet onglet met en lumière l’impact du lieu du match en analysant la domination des équipes à domicile par rapport aux performances à l’extérieur.")),
            tags$li(HTML("<b>🎯 ACP : Analyse en Composantes Principales</b> <br> Une analyse multivariée est proposée via une ACP afin de synthétiser l’information et visualiser les relations entre les variables et les équipes.")),
            tags$li(HTML("<b>📋 Tableau : Aperçu et export des données transformées</b> <br> Cet onglet présente une version agrégée de la base de données, avec la somme des résultats des variables par équipe et par saison. Il offre également la possibilité de télécharger ces données pour une utilisation externe."))
          )
        )
      )
    )
  ),
  
  # --- ONGLET 2 : STATS ---
  nav_panel(
    title = tagList(icon("chart-bar"), " Stats"), value = "stats",
    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = "#38003c",
        h4("🎛️ Filtres", style = "color:white;"), hr(),
        sliderTextInput("season_choice", "Plage de saisons :", choices = all_seasons, selected = c(all_seasons[1], all_seasons[length(all_seasons)])),
        selectInput("var_stat", "Variable principale :", choices = c("Buts Marqués" = "goals_scored", "Buts Encaissés" = "goals_conceded", "Victoires" = "wins", "Points" = "points", "Différence de buts" = "goal_difference", "Matchs Nuls" = "draws", "Défaites" = "losses")),
        selectInput("var_y", "Variable de comparaison (axe Y) :", choices = c("Points" = "points", "Buts Marqués" = "goals_scored", "Buts Encaissés" = "goals_conceded", "Victoires" = "wins", "Différence de buts" = "goal_difference", "Matchs Nuls" = "draws", "Défaites" = "losses"), selected = "points"),
        selectInput("nb_equipes", "Équipes affichées :", choices  = c("Top 5" = 5, "Top 10" = 10, "Top 15" = 15, "Toutes" = 999), selected = 10),
        hr(), p("💡 Astuce : Survolez les graphiques pour afficher les valeurs exactes.", style = "color:#00ff85; font-size:0.85em;")
      ),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(title = "Matchs joués", value = textOutput("tot_match_val"), showcase = icon("futbol"), theme = value_box_theme(bg = "#38003c", fg = "#00ff85")),
        value_box(title = "Buts marqués", value = textOutput("tot_goals_val"), showcase = icon("bullseye"), theme = value_box_theme(bg = "#e90052", fg = "#ffffff")),
        value_box(title = "Buts / match", value = textOutput("avg_goals_val"), showcase = icon("chart-line"), theme = value_box_theme(bg = "#1a0020", fg = "#00ff85"))
      ),
      card(full_screen = TRUE, card_header("📊 Top des Équipes", class = "bg-danger text-white"), plotlyOutput("bar_plot", height = "700px"), uiOutput("text_bar")),
      card(full_screen = TRUE, card_header("📦 Distribution par équipe (Boxplot)", class = "bg-danger text-white"), plotlyOutput("box_plot", height = "700px"), uiOutput("text_box")),
      card(full_screen = TRUE, card_header("🔀 Comparaison entre deux variables (Nuage de points)", class = "bg-primary text-white"), plotlyOutput("scatter_plot", height = "700px"), uiOutput("text_scatter"))
    )
  ),
  
  # --- ONGLET 3 : EVOLUTION ---
  nav_panel(
    title = tagList(icon("chart-line"), " Évolution"), value = "evolution",
    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = "#38003c",
        h4("🎛️ Filtres Évolution", style = "color:white;"), hr(),
        sliderTextInput("evo_season_choice", "Plage de saisons :", choices = all_seasons, selected = c(all_seasons[1], all_seasons[length(all_seasons)])),
        selectizeInput("evo_teams", "Sélectionnez les équipes :", choices = liste_equipes_uniques, multiple = TRUE, selected = c("Manchester United", "Chelsea", "Arsenal", "Manchester City")),
        selectInput("evo_var", "Indicateur à suivre :", choices = c("Points" = "points", "Buts Marqués" = "goals_scored", "Buts Encaissés" = "goals_conceded", "Différence de buts" = "goal_difference", "Victoires" = "wins")),
        hr(), p("💡 Astuce : Comparez les trajectoires de plusieurs clubs phares au fil des saisons.", style = "color:#00ff85; font-size:0.85em;")
      ),
      card(full_screen = TRUE, card_header("📈 Évolution temporelle des performances", class = "bg-primary text-white"), plotlyOutput("plot_evolution", height = "700px"), uiOutput("text_evolution"))
    )
  ),
  
  # --- ONGLET 4 : DOMICILE VS EXTERIEUR ---
  nav_panel(
    title = tagList(icon("house-user"), " Dom / Ext"), value = "home_away",
    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = "#38003c",
        h4("🎛️ Filtres Dom/Ext", style = "color:white;"), hr(),
        sliderTextInput("ha_season_choice", "Plage de saisons :", choices = all_seasons, selected = c(all_seasons[1], all_seasons[length(all_seasons)])),
        selectInput("ha_var", "Indicateur à comparer :", choices = c("Points Gagnés" = "points", "Buts Marqués" = "goals")),
        selectInput("ha_nb_equipes", "Équipes affichées :", choices = c("Top 5" = 5, "Top 10" = 10, "Top 20" = 20, "Toutes" = 999), selected = 10),
        hr(), p("💡 Analysez l'avantage du terrain : les équipes performent-elles vraiment mieux à domicile ?", style = "color:#00ff85; font-size:0.85em;")
      ),
      card(full_screen = TRUE, card_header("🏟️ Avantage du terrain (Domicile vs Extérieur)", class = "bg-danger text-white"), plotlyOutput("plot_home_away", height = "700px"), uiOutput("text_home_away"))
    )
  ),
  
  # --- ONGLET 5 : ACP ---
  nav_panel(
    title = tagList(icon("diagram-project"), " ACP"), value = "acp",
    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = "#38003c",
        h4("🎛️ Filtres ACP", style = "color:white;"), hr(),
        sliderInput("acp_axes", "Axes de l'ACP :", min = 1, max = 5, value = c(1, 2), step = 1),
        hr(), p("💡 Survolez les graphiques pour afficher les valeurs exactes.", style = "color:#00ff85; font-size:0.85em;")
      ),
      card(card_header("Analyse en Composantes Principales", class = "bg-warning text-dark"), p("L'ACP est réalisée sur les statistiques globales agrégées par équipe (toutes saisons confondues). Elle permet d'étudier les similarités entre clubs et les corrélations entre indicateurs de performance.")),
      card(full_screen = TRUE, card_header("👥 Graphe des individus (Équipes)", class = "bg-warning text-dark"), plotlyOutput("acp_ind", height = "700px"), uiOutput("text_acp_ind")),
      card(full_screen = TRUE, card_header("🔵 Cercle des corrélations (Variables)", class = "bg-warning text-dark"), plotOutput("acp_var", height = "700px"), uiOutput("text_acp_var")),
      card(full_screen = TRUE, card_header("📈 Variance expliquée par axe (Scree Plot)", class = "bg-warning text-dark"), plotlyOutput("acp_scree", height = "600px"), uiOutput("text_acp_scree"))
    )
  ),
  
  # --- ONGLET 6 : TABLEAU ---
  nav_panel(
    title = tagList(icon("table"), " Tableau"), value = "data",
    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = "#38003c",
        h4("🎛️ Filtres", style = "color:white;"), hr(),
        sliderTextInput("season_choice_tab", "Plage de saisons :", choices = all_seasons, selected = c(all_seasons[1], all_seasons[length(all_seasons)]))
      ),
      card(full_screen = TRUE, style = "min-height: 80vh;", card_header("Données agrégées par saison", class = "bg-success text-white"), dataTableOutput("tableau_donnees"))
    )
  ),
  
  # --- SIGNATURE EN HAUT À DROITE ---
  nav_spacer(),
  nav_item(
    tags$span(
      "Dashboard interactif réalisé par Thomas AYMARD, Mancef HENNICHE, Yusuf ASLAN et Malik BESSABIS -- BUT-SD-2A-VCOD-FI",
      style = "color: #b0a8b9; font-size: 0.85rem; font-style: italic; align-self: center; margin-right: 15px;"
    )
  )
)

# ==========================================
# 4. LOGIQUE SERVEUR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # --- FONCTIONS UTILES ---
  get_seasons_list <- function(season_range) {
    idx1 <- which(all_seasons == season_range[1])
    idx2 <- which(all_seasons == season_range[2])
    all_seasons[idx1:idx2]
  }
  
  saison_txt_react <- reactive({
    s <- input$season_choice
    if (s[1] == s[2]) paste("lors de la saison", s[1])
    else if (s[1] == all_seasons[1] && s[2] == all_seasons[length(all_seasons)]) "toutes saisons confondues"
    else paste("sur la période", s[1], "-", s[2])
  })
  
  top_n_txt_react <- reactive({
    n <- as.integer(input$nb_equipes)
    if(n >= 999) "toutes les équipes du championnat" else paste("le Top", n)
  })
  
  data_saison_raw <- reactive({ team_stats %>% filter(season %in% get_seasons_list(input$season_choice)) })
  
  data_saison_agg <- reactive({
    data_saison_raw() %>% group_by(team) %>%
      summarise(matches = sum(matches), goals_scored = sum(goals_scored), goals_conceded = sum(goals_conceded),
                wins = sum(wins), draws = sum(draws), losses = sum(losses), points = sum(points), goal_difference = sum(goal_difference), .groups = "drop") %>% as.data.frame()
  })
  
  data_filtered <- reactive({
    n  <- as.integer(input$nb_equipes)
    df <- data_saison_agg() %>% arrange(desc(get(input$var_stat)))
    if (n >= 999) df else head(df, n)
  })
  
  label_var   <- reactive({ var_labels[input$var_stat] })
  label_var_y <- reactive({ var_labels[input$var_y] })
  
  # --- STATS GLOBALES (Boxes) ---
  output$tot_match_val <- renderText({ as.character(sum(data_saison_agg()$matches) / 2) })
  output$tot_goals_val <- renderText({
    saisons <- get_seasons_list(input$season_choice)
    as.character(data_matchs %>% filter(season %in% saisons) %>% summarise(t = sum(home_goals + away_goals, na.rm = TRUE)) %>% pull(t))
  })
  output$avg_goals_val <- renderText({
    saisons <- get_seasons_list(input$season_choice)
    matchs <- sum(data_saison_agg()$matches) / 2
    buts <- data_matchs %>% filter(season %in% saisons) %>% summarise(t = sum(home_goals + away_goals, na.rm = TRUE)) %>% pull(t)
    as.character(round(buts / matchs, 2))
  })
  
  # --- ONGLET STATS (Graphiques & Textes) ---
  output$bar_plot <- renderPlotly({
    df <- data_filtered(); var <- input$var_stat; lbl <- label_var()
    p <- ggplot(df, aes(x = reorder(team, .data[[var]]), y = .data[[var]], fill = .data[[var]], text = paste0("<b>", team, "</b><br>", lbl, " : ", round(.data[[var]], 1)))) +
      geom_bar(stat = "identity", show.legend = FALSE) + scale_fill_gradient(low = "#f5b7b1", high = "#c0392b") + coord_flip() + theme_minimal(base_size = 14) + labs(x = NULL, y = lbl, title = paste("Classement —", lbl))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "#38003c", font = list(color = "#ffffff", size = 13))) %>% config(displayModeBar = FALSE)
  })
  
  output$text_bar <- renderUI({
    df <- data_filtered(); if (nrow(df) == 0) return(NULL)
    top <- df %>% arrange(desc(get(input$var_stat))) %>% slice(1)
    bot <- df %>% arrange(get(input$var_stat)) %>% slice(1)
    
    texte <- paste0("🏆 <b>Leçon à retenir :</b> En ciblant <b>", top_n_txt_react(), "</b> ", saison_txt_react(), ", c'est <b>", top$team, "</b> qui s'impose avec ", round(top[[input$var_stat]], 1), " ", tolower(label_var()), ". Tout en bas de cette sélection, on retrouve <b>", bot$team, "</b> (", round(bot[[input$var_stat]], 1), ").")
    div(class = "insight-box", HTML(texte))
  })
  
  output$box_plot <- renderPlotly({
    df <- data_saison_raw(); n <- as.integer(input$nb_equipes)
    top_teams <- df %>% group_by(team) %>% summarise(med = median(get(input$var_stat), na.rm = TRUE)) %>% arrange(desc(med)) %>% { if (n >= 999) . else head(., n) } %>% pull(team)
    df <- df %>% filter(team %in% top_teams)
    var <- input$var_stat; lbl <- label_var()
    p <- df %>% ggplot(aes(x = reorder(team, .data[[var]], median), y = .data[[var]], fill = team, text = paste0("<b>", team, "</b><br>Saison : ", season, "<br>", lbl, " : ", .data[[var]]))) +
      geom_boxplot(show.legend = FALSE, outlier.colour = "#c0392b", outlier.size = 2) + coord_flip() + theme_minimal(base_size = 14) + labs(x = NULL, y = lbl, title = paste("Distribution —", lbl))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "#38003c", font = list(color = "#ffffff", size = 13))) %>% config(displayModeBar = FALSE)
  })
  
  output$text_box <- renderUI({
    df <- data_saison_raw(); n <- as.integer(input$nb_equipes)
    top_teams <- df %>% group_by(team) %>% summarise(med = median(get(input$var_stat), na.rm = TRUE)) %>% arrange(desc(med)) %>% { if (n >= 999) . else head(., n) } %>% pull(team)
    df_filtered <- df %>% filter(team %in% top_teams)
    
    stats <- df_filtered %>% group_by(team) %>% summarise(ecart = IQR(get(input$var_stat), na.rm=TRUE)) %>% arrange(ecart)
    if(nrow(stats) < 2) return(NULL)
    reguliere <- head(stats$team, 1); irreguliere <- tail(stats$team, 1)
    
    texte <- paste0("📦 <b>Parlons régularité :</b> Sur ce critère de ", tolower(label_var()), " (", saison_txt_react(), "), l'équipe de <b>", reguliere, "</b> est la plus constante parmi ", top_n_txt_react(), " (sa boîte est la plus resserrée). À l'opposé, <b>", irreguliere, "</b> est l'équipe la plus imprévisible et irrégulière.")
    div(class = "insight-box info", HTML(texte))
  })
  
  output$scatter_plot <- renderPlotly({
    df <- data_saison_agg() %>% arrange(desc(get(input$var_stat))); n <- as.integer(input$nb_equipes)
    if (n < 999) df <- head(df, n)
    var <- input$var_stat; vy <- input$var_y; lbl <- label_var(); lbl_y <- label_var_y()
    p <- ggplot(df, aes(x = .data[[var]], y = .data[[vy]], text = paste0("<b>", team, "</b><br>", lbl, " : ", round(.data[[var]], 1), "<br>", lbl_y, " : ", round(.data[[vy]], 1)))) +
      geom_point(aes(color = .data[[var]]), size = 4, show.legend = FALSE) + geom_smooth(method = "lm", se = TRUE, color = "#c0392b", fill = "#f5b7b1") +
      scale_color_gradient(low = "#f5cba7", high = "#c0392b") + theme_minimal(base_size = 14) + labs(x = lbl, y = lbl_y, title = paste("Relation entre", lbl, "et", lbl_y))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "#38003c", font = list(color = "#ffffff", size = 13))) %>% config(displayModeBar = FALSE)
  })
  
  output$text_scatter <- renderUI({
    df <- data_saison_agg() %>% arrange(desc(get(input$var_stat))); n <- as.integer(input$nb_equipes); if (n < 999) df <- head(df, n)
    if(nrow(df) < 3) return(NULL)
    
    cor_val <- cor(df[[input$var_stat]], df[[input$var_y]], use = "complete.obs")
    
    if(abs(cor_val) > 0.7) { interpretation <- "Il y a un lien très fort ! Si une équipe domine dans l'un, elle domine quasi-systématiquement dans l'autre."
    } else if (abs(cor_val) > 0.3) { interpretation <- "Il y a un lien modéré. C'est utile, mais ce n'est pas une garantie absolue (voir l'éparpillement des points)."
    } else { interpretation <- "Surprise ! Ces deux statistiques n'ont presque aucun lien pour ce groupe d'équipes." }
    
    texte <- paste0("🔀 <b>La vérité des chiffres :</b> En croisant <b>", label_var(), "</b> et <b>", label_var_y(), "</b> pour ", top_n_txt_react(), " (", saison_txt_react(), "), l'algorithme calcule une corrélation de ", round(cor_val, 2), ". ", interpretation)
    div(class = "insight-box success", HTML(texte))
  })
  
  # --- ONGLET : EVOLUTION ---
  output$plot_evolution <- renderPlotly({
    req(input$evo_teams)
    saisons_evo <- get_seasons_list(input$evo_season_choice) 
    df_evo <- team_stats %>% filter(team %in% input$evo_teams, season %in% saisons_evo)
    
    var_evo <- input$evo_var; lbl_evo <- var_labels[var_evo]
    p <- ggplot(df_evo, aes(x = season, y = .data[[var_evo]], color = team, group = team,
                            text = paste0("<b>", team, "</b><br>Saison: ", season, "<br>", lbl_evo, ": ", .data[[var_evo]]))) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      theme_minimal(base_size = 14) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Saison", y = lbl_evo, title = paste("Trajectoire —", lbl_evo))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "#38003c", font = list(color = "#ffffff", size = 13)), legend = list(orientation = "h", x = 0, y = -0.2)) %>% config(displayModeBar = FALSE)
  })
  
  output$text_evolution <- renderUI({
    req(input$evo_teams)
    saisons_evo <- get_seasons_list(input$evo_season_choice)
    df_evo <- team_stats %>% filter(team %in% input$evo_teams, season %in% saisons_evo)
    if(nrow(df_evo) < 2) return(NULL)
    
    evo_calc <- df_evo %>% arrange(season) %>% group_by(team) %>%
      summarise(first_val = first(get(input$evo_var)), last_val = last(get(input$evo_var))) %>%
      mutate(diff = last_val - first_val) %>% arrange(desc(diff))
    
    top_prog <- head(evo_calc, 1); pire_chute <- tail(evo_calc, 1)
    lbl_min <- tolower(var_labels[input$evo_var])
    
    s_txt <- if(input$evo_season_choice[1] == input$evo_season_choice[2]) paste("en", input$evo_season_choice[1]) else paste("entre", input$evo_season_choice[1], "et", input$evo_season_choice[2])
    
    texte <- paste0("📈 <b>Bilan de forme :</b> Vous observez la trajectoire de <b>", length(input$evo_teams), " équipes</b> concernant leurs <b>", lbl_min, "</b> (", s_txt, "). ")
    texte <- paste0(texte, "Sur la période que vous avez sélectionnée, le club de <b>", top_prog$team, "</b> est celui qui a le plus progressé (", ifelse(top_prog$diff > 0, "+", ""), top_prog$diff, "). ")
    
    if(length(input$evo_teams) > 1 && top_prog$team != pire_chute$team) {
      texte <- paste0(texte, "En revanche, <b>", pire_chute$team, "</b> a la pire dynamique du groupe (", pire_chute$diff, ").")
    }
    div(class = "insight-box info", HTML(texte))
  })
  
  # --- ONGLET : DOMICILE VS EXTERIEUR ---
  output$plot_home_away <- renderPlotly({
    saisons_ha <- get_seasons_list(input$ha_season_choice)
    df_home <- home_stats %>% filter(season %in% saisons_ha) %>% group_by(team) %>% summarise(Dom_buts = sum(goals_scored_home, na.rm=T), Dom_pts = sum(wins_home*3 + draws_home, na.rm=T))
    df_away <- away_stats %>% filter(season %in% saisons_ha) %>% group_by(team) %>% summarise(Ext_buts = sum(goals_scored_away, na.rm=T), Ext_pts = sum(wins_away*3 + draws_away, na.rm=T))
    df_ha <- full_join(df_home, df_away, by = "team")
    
    if(input$ha_var == "goals"){ df_plot <- df_ha %>% select(team, Domicile = Dom_buts, Extérieur = Ext_buts); lbl_ha <- "Buts Marqués"
    } else { df_plot <- df_ha %>% select(team, Domicile = Dom_pts, Extérieur = Ext_pts); lbl_ha <- "Points" }
    
    df_plot <- df_plot %>% mutate(Total = Domicile + Extérieur) %>% arrange(desc(Total))
    n <- as.integer(input$ha_nb_equipes); if(n < 999) df_plot <- head(df_plot, n)
    df_long <- df_plot %>% pivot_longer(cols = c("Domicile", "Extérieur"), names_to = "Lieu", values_to = "Valeur")
    
    p <- ggplot(df_long, aes(x = reorder(team, Total), y = Valeur, fill = Lieu, text = paste0("<b>", team, "</b><br>Lieu : ", Lieu, "<br>", lbl_ha, " : ", Valeur))) +
      geom_bar(stat="identity", position="dodge") + scale_fill_manual(values = c("Domicile" = "#38003c", "Extérieur" = "#00ff85")) + coord_flip() +
      theme_minimal(base_size = 14) + labs(x = NULL, y = lbl_ha, title = paste("Comparatif Domicile / Extérieur —", lbl_ha))
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(font = list(color = "#ffffff", size = 13))) %>% config(displayModeBar = FALSE)
  })
  
  output$text_home_away <- renderUI({
    saisons_ha <- get_seasons_list(input$ha_season_choice)
    
    if(input$ha_var == "goals") {
      df_home <- home_stats %>% filter(season %in% saisons_ha) %>% group_by(team) %>% summarise(Dom = sum(goals_scored_home, na.rm=T))
      df_away <- away_stats %>% filter(season %in% saisons_ha) %>% group_by(team) %>% summarise(Ext = sum(goals_scored_away, na.rm=T))
      lbl_min <- "buts"
    } else {
      df_home <- home_stats %>% filter(season %in% saisons_ha) %>% group_by(team) %>% summarise(Dom = sum(wins_home*3 + draws_home, na.rm=T))
      df_away <- away_stats %>% filter(season %in% saisons_ha) %>% group_by(team) %>% summarise(Ext = sum(wins_away*3 + draws_away, na.rm=T))
      lbl_min <- "points"
    }
    
    df_ha <- full_join(df_home, df_away, by = "team") %>% mutate(Total = Dom + Ext) %>% arrange(desc(Total))
    n <- as.integer(input$ha_nb_equipes); if(n < 999) df_ha <- head(df_ha, n)
    
    n_txt <- if(n >= 999) "toutes les équipes" else paste("le Top", n)
    s_txt <- if(input$ha_season_choice[1] == input$ha_season_choice[2]) paste("la saison", input$ha_season_choice[1]) else "la période sélectionnée"
    
    top_domicile <- df_ha %>% arrange(desc(Dom)) %>% slice(1)
    top_exterieur <- df_ha %>% arrange(desc(Ext)) %>% slice(1)
    
    texte <- paste0("🏟️ <b>Le mythe du douzième homme :</b> En étudiant <b>", n_txt, "</b> (sur ", s_txt, "), le stade de <b>", top_domicile$team, "</b> s'impose comme la plus grande forteresse avec un total record de ", top_domicile$Dom, " ", lbl_min, " acquis à domicile. En revanche, hors de ses bases, c'est l'équipe de <b>", top_exterieur$team, "</b> qui est la plus dangereuse (", top_exterieur$Ext, " ", lbl_min, " à l'extérieur).")
    
    div(class = "insight-box success", HTML(texte))
  })
  
  # --- ONGLET ACP ---
  res_acp <- reactive({
    df_pca <- team_overall[, c("goals_scored", "goals_conceded", "wins", "draws", "losses", "points", "goal_difference")]
    PCA(df_pca, graph = FALSE)
  })
  
  output$acp_ind <- renderPlotly({
    acp <- res_acp(); ax1 <- input$acp_axes[1]; ax2 <- input$acp_axes[2]
    coords <- as.data.frame(acp$ind$coord); coords$team <- rownames(coords)
    pct <- round(acp$eig[, 2], 1)
    coords <- coords %>% left_join(team_overall %>% select(team, points, wins, goals_scored, goals_conceded, goal_difference), by = "team")
    p <- ggplot(coords, aes(x = .data[[paste0("Dim.", ax1)]], y = .data[[paste0("Dim.", ax2)]], text = paste0("<b>", team, "</b><br>Points : ", points, "<br>Victoires : ", wins, "<br>Buts marqués : ", goals_scored, "<br>Diff. buts : ", goal_difference))) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") + geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
      geom_point(color = "#e90052", size = 3) + geom_text(aes(label = team), size = 3, vjust = -0.8, color = "#38003c") +
      theme_minimal(base_size = 14) + labs(x = paste0("Dim.", ax1, " (", pct[ax1], "%)"), y = paste0("Dim.", ax2, " (", pct[ax2], "%)"), title = "Projection des équipes")
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "#38003c", font = list(color = "#ffffff", size = 13))) %>% config(displayModeBar = FALSE)
  })
  
  output$text_acp_ind <- renderUI({
    ax1 <- input$acp_axes[1]; ax2 <- input$acp_axes[2]
    if (ax1 == 1 && ax2 == 2) {
      texte <- "👥 <b>Comment lire cette carte ?</b> La règle est simple : les équipes qui se touchent sur ce graphique ont un ADN footballistique similaire ! Les clubs à droite sont les ogres historiques du championnat. Les clubs à gauche luttent souvent pour le maintien."
    } else {
      texte <- paste0("🔍 Vous regardez l'algorithme sous un autre angle (Axes ", ax1, " et ", ax2, "). On cherche ici des profils de jeu plus atypiques : par exemple, des équipes de milieu de tableau qui sont de véritables spécialistes du match nul ou du jeu très défensif.")
    }
    div(class = "insight-box warning", HTML(texte))
  })
  
  output$acp_var <- renderPlot({ plot.PCA(res_acp(), choix = "var", axes = input$acp_axes, title = "Cercle des corrélations") }, res = 96)
  
  output$text_acp_var <- renderUI({ div(class = "insight-box warning", HTML("🔵 <b>La boussole des statistiques :</b> Les flèches pointant dans la même direction s'entraident (ex: marquer aide à gagner). Les flèches opposées s'annulent (ex: prendre des buts augmente les défaites).")) })
  
  output$acp_scree <- renderPlotly({
    eig <- as.data.frame(res_acp()$eig); eig$dim <- paste0("Dim.", 1:nrow(eig))
    p <- ggplot(eig, aes(x = reorder(dim, -`percentage of variance`), y = `percentage of variance`, text = paste0("<b>", dim, "</b><br>Variance : ", round(`percentage of variance`, 1), "%<br>Cumulé : ", round(`cumulative percentage of variance`, 1), "%"))) +
      geom_bar(stat = "identity", fill = "#f39c12") + geom_line(aes(group = 1), color = "#c0392b", linewidth = 1) + geom_point(color = "#c0392b", size = 3) +
      theme_minimal(base_size = 14) + labs(x = "Composante principale", y = "Fiabilité de la projection (%)", title = "Poids de chaque dimension")
    ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(bgcolor = "#38003c", font = list(color = "#ffffff", size = 13))) %>% config(displayModeBar = FALSE)
  })
  
  output$text_acp_scree <- renderUI({
    eig <- res_acp()$eig; var_cum <- round(eig[2, 3], 1)
    qualite <- if (var_cum >= 70) "Le score est excellent : la Carte des équipes ci-dessus est très fiable." else "C'est un score moyen, certains comportements échappent aux deux premiers axes."
    div(class = "insight-box warning", HTML(paste0("📈 <b>Précision de l'algorithme :</b> Les deux premières dimensions réussissent à résumer ", var_cum, "% de toute l'histoire de ce championnat. ", qualite)))
  })
  
  # --- ONGLET TABLEAU ---
  output$tableau_donnees <- renderDT({
    saisons_tab <- get_seasons_list(input$season_choice_tab)
    datatable(team_stats %>% filter(season %in% saisons_tab),
              extensions = "Buttons",
              options = list(pageLength = 25, lengthMenu = list(c(25, 50, 75, 100, -1), c('25', '50', '75', '100', 'Tout')), scrollX = TRUE, scrollY = "calc(100vh - 300px)", scrollCollapse = TRUE, dom = "lBfrtip", buttons = c("copy", "csv", "excel")),
              rownames = FALSE, caption = "Performances complètes des équipes")
  })
}

# ==========================================
# 5. LANCEMENT
# ==========================================
shinyApp(ui = ui, server = server)