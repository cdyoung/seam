# load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyjs)
# library(ggtern) # do not load. kept here for list of packages required
# library(data.table) # do not load. kept here for list of packages required
library(shinyhelper)
library(DT)

# load additional functions
source("./modules.R")

ui = tagList(
  fluidPage(theme = "flatly.css",
        tags$head(
          tags$style(HTML("hr {border-top: 1px solid #bdbdbd;}"))
        ),
        navbarPage(id = "tabs",
             title = "SEAM",
             tabPanel(title = "Synthetic Matchup",
                  sidebarLayout(
                    sidebarPanel(uiOutput("sidebar")),
                    mainPanel(column(width = 12,
                              fluidRow(uiOutput("synth_title"), align = "center"),
                              plotOutput("synth_master"),
                              fluidRow(helper(tableOutput("synth_stats"),
                                      type = "inline",
                                      fade = TRUE,
                                      title = "Stats",
                                      content = c("Spray chart density averaged over 100 balls in play",
                                                  "",
                                                  "<b>xBABIP</b>: Expected batting average on balls in play",
                                                  "",
                                                  "<b>xBsCON</b>: Expected bases on contact (slg% except the denominator is BIP + HR instead of AB)")),
                                     align = "center")),
                        tabsetPanel(id = "tables",
                              tabPanel("Traditional Batter", plotOutput("traditional")),
                              tabPanel("Synthetic Batter",
                                   fluidRow(
                                     plotOutput("synth_batter"),
                                     div(tableOutput("synth_batter_similarities"), align = "center"))),
                              tabPanel("Synthetic Pitcher",
                                   fluidRow(
                                     plotOutput("synth_pitcher"),
                                     div(tableOutput("synth_pitcher_similarities"), align = "center"))))))),
             tabPanel(title = "What's This?",
                  includeMarkdown("writeup.Rmd")),
             tabPanel(title = "Example Matchups",
                  sidebarLayout(
                    sidebarPanel(uiOutput("sidebar2")),
                    mainPanel(dataTableOutput("leaderboard")))))))

server = function(input, output, session) {
  observe_helpers(withMathJax = TRUE)

  withProgress(message = "Loading...", {
    pitcher_pool = load_pitcher_pool() %>%
      mutate_at(vars(release_speed, pfx_x, pfx_z, release_spin_rate,
               pitch_launch_h_c, pitch_launch_v_c,
               release_pos_x, release_pos_y, release_pos_z), scale)

    incProgress(1/4)

    batter_pool = load_batter_pool() %>%
      mutate_at(vars(launch_speed, launch_angle, pull, cent, oppo), scale)

    switch_hitters = batter_pool %>%
      group_by(batter) %>%
      summarise(n_stand = length(unique(stand))) %>%
      filter(n_stand > 1)

    incProgress(1/4)

    incProgress(1/2)
  })

  output$sidebar = renderUI({
    sidebar(pitcher_pool, batter_pool)
  })

  output$leaderboard = renderDataTable({
    top10_pitchers = data.table::fread("./data/top10_pitchers.csv", data.table = FALSE)
    top10_batters = data.table::fread("./data/top10_batters.csv", data.table = FALSE)

    rows = bind_rows(top10_pitchers, top10_batters)
    rows = rows[!duplicated(rows[c("batter", "pitcher")]),]

    leaderboard = rows %>%
      rename(Name = batter,
             Pitcher = pitcher,
             xBABIP = xbabip,
             xBsCON = xbscon) %>%
      mutate(xBABIP = gsub("^0(.*)", "\\1", format(round(xBABIP, 3), nsmall = 3)),
             xBsCON = gsub("^0(.*)", "\\1", format(round(xBsCON, 3), nsmall = 3)),
             singles = as.integer(floor(singles)),
             doubles = as.integer(floor(doubles)),
             triples = as.integer(floor(triples)),
             home_runs = as.integer(floor(home_runs))) %>%
      rename(`1B` = singles,
             `2B` = doubles,
             `3B` = triples,
             `HR` = home_runs) %>%
      select(-outs) %>%
      arrange(desc(xBsCON))

    datatable(leaderboard,
          rownames = FALSE,
          options = list(
            dom = "ftp",
            scrollX = TRUE,
            pageLength = "15",
            columnDefs = list(list(className = 'dt-center', targets = "_all"))
          ))
  })

  output$sidebar2 = renderUI({
    tagList(h2("Example Matchups"),
        h5("xBABIP: Expected batting average on balls in play"),
        h5("xBsCON: Expected bases on contact (slg% except the denominator is BIP + HR instead of AB)"),
        h5("These matchups consist of:"),
        tags$ul(tags$li("The top 10 batters (by average wRC+) vs. 150 qualified pitchers since 2015"),
            tags$li("The top 10 pitchers (by accumulated fWAR) vs. 150 qualified batters since 2015")),
        tags$i("These stat totals are averaged over 100 balls in play"))
  })

  observeEvent(input$submit, {
    if (input$batter %in% switch_hitters$batter) {
      p_hand = pitcher_pool %>% filter(pitcher == input$pitcher)
      p_hand = unique(p_hand$p_throws)

      if (p_hand == "R") {
        b_hand = "L"
        p_hand = "left handed"
      } else {
        b_hand = "R"
        p_hand = "right handed"
      }

      showNotification(paste0("This batter is a switch hitter. Based on the selected pitcher, we are assuming he is batting ", p_hand, "."))
    } else {
      b_hand = batter_pool %>% filter(batter == input$batter, game_year != 0)
      if (nrow(b_hand) == 0) {
        showNotification("Congratulations! You've selected a batter that doesn't have enough data. Please try again.")
        return()
      }

      b_hand = unique(b_hand$stand)
    }

    if (b_hand == "L") {
      pitches_bip = data.table::fread("./data/hittr_bip_lhb.csv", data.table = FALSE)
    } else {
      pitches_bip = data.table::fread("./data/hittr_bip_rhb.csv", data.table = FALSE)
    }

    run(b_hand, pitches_bip)
  })

  run = function(b_hand, pitches_bip) {
    withProgress(message = "Synthesizing...", value = 0, {
      master = synthetic(input$pitcher, input$batter, b_hand, pitcher_pool, batter_pool, pitches_bip, input$p_ratio, input$b_ratio)
      if (length(master) == 0) {
        showNotification("Congratulations! You've selected a pitcher that doesn't have enough data. Please try again.")
        return()
      }

      incProgress(1/2)

      output$traditional = renderPlot({
        graph_field(master$traditional, master, isolate(paste0(input$batter, " vs. All (no weights)")))
      })

      incProgress(1/8)

      output$synth_batter = renderPlot({
        graph_field(master$synth_batter, master, isolate(paste0("Synthetic ", input$batter, " vs. ", input$pitcher)))
      })

      output$synth_batter_similarities = renderTable({
        master$synth_batter_similarities %>%
          filter(Name != input$batter) %>%
          arrange(desc(Similarity)) %>%
          head(10)
      },
      caption = "Top 10 Similar Batters",
      caption.placement = getOption("xtable.caption.placement", "top"))

      incProgress(1/8)

      output$synth_pitcher = renderPlot({
        graph_field(master$synth_pitcher, master, isolate(paste0(input$batter, " vs. Synthetic ", input$pitcher)))
      })

      output$synth_pitcher_similarities = renderTable({
        master$synth_pitcher_similarities %>%
          filter(Name != input$pitcher) %>%
          arrange(desc(Similarity)) %>%
          head(10)
      },
      caption = "Top 10 Similar Pitchers",
      caption.placement = getOption("xtable.caption.placement", "top"))

      incProgress(1/8)

      output$synth_master = renderPlot({
        graph_field(master$synth_master, master, "")
      })

      output$synth_stats = renderTable({
        master$bins %>%
          mutate(singles = as.character(floor(singles)),
                 doubles = as.character(floor(doubles)),
                 triples = as.character(floor(triples)),
                 home_runs = as.character(floor(home_runs)),
                 xbabip = gsub("^0(.*)", "\\1", format(round(xbabip, 3), nsmall = 3)),
                 xbscon = gsub("^0(.*)", "\\1", format(round(xbscon, 3), nsmall = 3))) %>%
          rename(`1B` = singles,
                 `2B` = doubles,
                 `3B` = triples,
                 `HR` = home_runs,
                 xBABIP = xbabip,
                 xBsCON = xbscon) %>%
          select(-outs)
      },
      caption = "Averaged over 100 balls in play",
      caption.placement = getOption("xtable.caption.placement", "top"))

      output$synth_title = renderUI({
        h2(isolate(paste0(input$batter, " vs. ", input$pitcher)))
      })

      incProgress(1/8)
    })
  }
}

sidebar = function(pitcher_pool, batter_pool) {
  pitchers = pitcher_pool %>% filter(game_year == 0) %>% arrange(pitcher)
  batters = batter_pool %>% filter(game_year == 0) %>% arrange(batter)

  tags = tagList(
    selectizeInput("pitcher", label = "Pitcher", choices = unique(pitchers$pitcher), selected = "Justin Verlander"),
    helper(sliderInput("p_ratio", "Ratio of Stuff to Release", min = .50, max = 1, value = .85),
           type = "inline",
           fade = TRUE,
           title = "Stuff, Release",
           content = c("This slider adjusts the weighting of components during pitcher comparison.",
                       "",
                       "<b>Stuff</b>: velocity, spin rate, movement",
                       "<b>Release</b>: release point, release angle",
                       "",
                       "<i>For example, a value of .66 would make stuff twice as important as release point when comparing pitchers.</i>")),
    tags$i("Suggested pitchers: Gerrit Cole, Jacob deGrom", style = "color: darkgray"),
    hr(),
    selectizeInput("batter", label = "Batter", choices = unique(batters$batter), selected = "Mike Trout"),
    helper(sliderInput("b_ratio", "Ratio of LA/EV to Batted Ball Location", min = 0, max = 1, value = .85),
           type = "inline",
           fade = TRUE,
           title = "LA/EV, Batted Ball Location",
           content = c("This slider adjusts the weighting of components during batter comparison.",
                       "",
                       "<b>LA/EV</b>: launch angle, exit velocity",
                       "<b>Batted Ball Location</b>: pull%, cent%, oppo%",
                       "",
                       "<i>For example, a value of .66 would make LA/EV twice as important as batted ball location when comparing batters</i>")),
    tags$i("Suggested batters: Aaron Judge, Cody Bellinger", style = "color: darkgray"),
    hr(),
    actionButton("submit", "Submit")
    )

  return(tags)
}

shinyApp(ui = ui, server = server)
