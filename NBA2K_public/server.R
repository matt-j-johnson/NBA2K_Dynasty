#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


unrollables <- c("All-Decade 2000s All-Stars", "All-Decade 1960s All-Stars", "All-Decade 1980s All-Stars", 
                 "All-Decade 1990s All-Stars", "All-Decade 2010s All-Stars", "All-Decade 1970s All-Stars",
                 "Team World", "Team USA", "Team Deron", "Team Jason", "Team Joakim", "Team Pau")
player_ratings <- read.csv("player2k ratings.csv") %>% 
  filter(!Team %in% unrollables) %>% 
  select(!X)

# Define server logic
function(input, output, session) {
  
  ###-###-###-###-###-###-###
  # Refreshing the data -----
  ###-###-###-###-###-###-###
  
  # In the full app - this reads from a Google sheet using Google API and updates when it detects edits have been made
   dynasty_data <- read.csv('dynasty_data.csv')
  
  
  ###-###-###-###-###-###-###
  # Home Tab Output ---------
  ###-###-###-###-###-###-###
  output$logo2k <- renderImage({
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$milestones <- render_gt({
    
    milestones <- dynasty_data %>% 
      group_by(person) %>% 
      summarise(gp = n(),
                across(pts:dnk, \(x) sum(x, na.rm = TRUE))) %>% 
      select(1:8, 11, 13, 16, 17) 
    
    colnames(milestones) <- c("Person", "Games Played", "Points", "Rebounds", "Assists", "Steals", "Blocks", "Turnovers",
                              "Threes Made", "Free Throws Made", "Fouls", "Dunks")
    
    milestones <- milestones %>% 
      mutate(across(c(2, 6:7, 9, 12), ~ cut(.x, seq(0,5000,50), right = FALSE), .names = "{.col}_band"),
             across(c(4:5), ~ cut(.x, seq(0,5000,200), right = FALSE), .names = "{.col}_band"),
             across(c(8, 10:11), ~ cut(.x, seq(0,5000,100), right = FALSE), .names = "{.col}_band"),
             Points_band = cut(Points, seq(0,10000, 250), right = FALSE),
             across(13:23, ~ str_remove(gsub(".*,", "", .x), "\\)"))) %>% 
      pivot_longer(cols = 2:12, 
                   names_to = "Stat", values_to = "Total") %>% 
      pivot_longer(cols = 2:12, 
                   names_to = "stat_band", 
                   values_to = "Milestone") %>% 
      filter(Stat == gsub("_band", "", stat_band)) %>% 
      mutate(perc = Total / as.numeric(Milestone),
             Milestone = as.numeric(Milestone)) %>% 
      arrange(desc(perc)) %>% 
      select(Person, Stat, Total, Milestone) %>% 
      head(n = 10) %>% 
      gt(rowname_col = "Person")
  })
  
  # Create Podium HTML 
  # Adapted from online example for Olympic medals -> same var names haven't been changed
  output$DYNpodium <- renderUI({
    
    leaderboard <- dynasty_data %>% 
      group_by(dynasty_number) %>% 
      filter(dynasty_game_number == max(dynasty_game_number),
             dynasty_number != 0,
             context == "Dynasty") %>% 
      distinct(dynasty_number, .keep_all = TRUE) %>% 
      arrange(desc(dynasty_game_number)) %>% 
      select(dynasty_number, dynasty_game_number, team) %>% 
      mutate(display_text = paste(dynasty_game_number, " games", ",", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Games", "Team")
    rownames(top_three) <- NULL
    
    games <- top_three$Game
    teams <- top_three$Team
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  games[2]),
                p(class = "country_label",
                  teams[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  games[1]),
                p(class = "country_label",
                  teams[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  games[3]),
                p(class = "country_label",
                  teams[3])
            )
        ),
    )
  })
  
  ###-###-###-###-###-###-###
  # Averages Tab Output -----
  ###-###-###-###-###-###-###
  
  rval_filtered_averages  <- reactive({
    filtered <- dynasty_data %>% 
      filter(!is.na(pts)) %>% 
      filter(pos %in% input$AVGposition,
             context %in% input$AVGdynasty)
    
    #Filter by player
    if(input$AVGplayer == ""){
      #no filter
    } else {
      filtered <- filtered %>%  filter(player == input$AVGplayer)
    }    
    #Filter by result
    if(input$AVGresult == "ALL"){
      #no filter
    } else {
      filtered <- filtered %>%  filter(result == input$AVGresult)
    }  
    #Filter by teammate
    if(input$AVGteammate == "ALL"){
      #no filter
    } else {
      filtered <- filtered %>%  filter(teammate == input$AVGteammate)
    }
    
    filtered
    
  })
  
  #averages HTML table
  output$averageHTML <- function(){
    
    ##Filtering Data Based on UI Inputs
    table_stats <- rval_filtered_averages() 
    
    ##Creating the Summary Table      
    sum_tab <- aggregate(table_stats[,13:27], list(table_stats$person), mean)
    
    full_games <- dynasty_data
    
    games <- full_games %>%
      group_by(person, result) %>%
      summarise(n = n()) %>% 
      mutate(gp = sum(n)) %>% 
      filter(result == "Win")
    
    
    
    names <- sum_tab$Group.1
    stats <- c("Name", "Games Played", "Points", "Rebounds", "Off Rebounds", "Assists", "Steals", "Blocks", "Turnovers", "Fouls", "Dunks", "FGA", "FG%", "FTA", "FT%", "3PA", "3P%")
    
    ##Editing the Summary Table
    out_tab <- sum_tab %>% 
      mutate(fgperc = (fgm/fga)*100,
             ftperc = (ftm/fta)*100, 
             threepperc = (X3pm/X3pa)*100) %>% 
      full_join(games, join_by(Group.1 == person)) %>% 
      select(gp, pts, reb, or, ast, stl, blk, to, fls, dnk, fga, fgperc, fta, ftperc, X3pa, threepperc) %>% 
      round(digits = 2) %>% 
      mutate(Names = names, .before = gp)
    
    colnames(out_tab) <- stats
    out_tab %>% 
      kable() %>% 
      kable_material_dark()
    
  }
  
  ##Averages table
  output$averages <- render_gt({
    
    ##Filtering Data Based on UI Inputs
    table_stats <- rval_filtered_averages()
    
    filtered_games <- dynasty_data %>% 
      filter(pos %in% input$AVGposition,
             context %in% input$AVGdynasty)
    
    #Filter by player
    if(input$AVGplayer == ""){
      #no filter
    } else {
      filtered_games <- filtered_games %>%  filter(player == input$AVGplayer)
    }    
    #Filter by result
    if(input$AVGresult == "ALL"){
      #no filter
    } else {
      filtered_games <- filtered_games %>%  filter(result == input$AVGresult)
    }  
    #Filter by teammate
    if(input$AVGteammate == "ALL"){
      #no filter
    } else {
      filtered_games <- filtered_games %>%  filter(teammate == input$AVGteammate)
    }
    
    ##Creating the Summary Table      
    sum_tab <- aggregate(table_stats[,13:27], list(table_stats$person), mean)
    
    games <- filtered_games %>%
      group_by(person, result) %>%
      summarise(n = n()) %>% 
      mutate(gp = sum(n)) %>% 
      filter(result == "Win")
    

    
    names <- sum_tab$Group.1
    stats <- c("Name", "Games Played", "Points", "Rebounds", "Off Rebounds", "Assists", "Steals", "Blocks", "Turnovers", "Fouls", "Dunks", "FGA", "FG%", "FTA", "FT%", "3PA", "3P%")
    
    ##Editing the Summary Table
    out_tab <- sum_tab %>% 
      mutate(fgperc = (fgm/fga)*100,
             ftperc = (ftm/fta)*100, 
             threepperc = (X3pm/X3pa)*100) %>% 
      full_join(games, join_by(Group.1 == person)) %>% 
      select(gp, pts, reb, or, ast, stl, blk, to, fls, dnk, fga, fgperc, fta, ftperc, X3pa, threepperc) %>% 
      round(digits = 2) %>% 
      mutate(Names = names, .before = gp)
    
    colnames(out_tab) <- stats
    out_tab %>% 
      gt(rowname_col = "Name")
    
  })
  
  output$metrics <- render_gt({
    full_games <- dynasty_data
    
    wins <- full_games %>%
      group_by(person, result) %>%
      summarise(n = n()) %>% 
      mutate(winperc = round((n / sum(n))*100,2),
             gp = sum(n)) %>% 
      filter(result == "Win")
    
    eff <- full_games %>% 
      group_by(person) %>% 
      mutate(eff = (pts + reb + ast + stl + blk - (fga-fgm) - (fta-ftm) - to),
             trueshooting = (0.5 * pts) / fga + (0.475 * fta)) %>% 
      filter(!is.na(eff),
             !is.na(trueshooting)) %>% 
      summarise(eff = mean(eff),
                trueshooting = mean(trueshooting, na.rm = T)) %>% 
      select(person, eff, trueshooting)
    
    wins %>% 
      ungroup() %>% 
      full_join(eff, by = join_by(person)) %>% 
      select(person, gp, winperc, eff) %>% 
      gt() 
      
    
    
    
  })
  
  ###-###-###-###-###-###-###
  # Records Tab Output ------
  ###-###-###-###-###-###-###
  
  filtered_records_data <- reactive({
    filtered <- dynasty_data %>% 
      filter(!is.na(pts)) %>% 
      filter(person %in% input$RECperson,
             pos %in% input$RECposition,
             context %in% input$RECdynasty)
    #Filter by player
    if(input$RECplayer == ""){
      #no filter
    } else {
      filtered <- filtered %>%  filter(player == input$RECplayer)
    }    
    
    #Filter by result
    if(input$RECresult == "ALL"){
      #no filter
    } else {
      filtered <- filtered %>%  filter(result == input$RECresult)
    }      
    
    filtered
    
  })
  
  #Points Record
  output$PTSpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(pts)) %>% 
      select(pts, person, player, team) %>% 
      mutate(display_text = paste(pts, " Points", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Points", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Points
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$PTSleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(pts)) %>% 
      select(pts, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(pts), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  #Rebound Record
  output$REBpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(reb)) %>% 
      select(reb, person, player, team) %>% 
      mutate(display_text = paste(reb, " Rebounds", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Rebounds", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Rebounds
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$REBleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(reb)) %>% 
      select(reb, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(reb), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  #Assist Record
  output$ASTpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(ast)) %>% 
      select(ast, person, player, team) %>% 
      mutate(display_text = paste(ast, " Assists", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Assists", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Assists
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$ASTleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(ast)) %>% 
      select(ast, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(ast), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  #Steals Record
  output$STLpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(stl)) %>% 
      select(stl, person, player, team) %>% 
      mutate(display_text = paste(stl, " Steals", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Steals", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Steals
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$STLleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(stl)) %>% 
      select(stl, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(stl), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  #Blocks Record
  output$BLKpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(blk)) %>% 
      select(blk, person, player, team) %>% 
      mutate(display_text = paste(blk, " Blocks", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Blocks", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Blocks
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$BLKleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(blk)) %>% 
      select(blk, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(blk), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  #Turnover Record
  output$TOpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(to)) %>% 
      select(to, person, player, team) %>% 
      mutate(display_text = paste(to, " Turnovers", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Turnovers", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Turnovers
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$TOleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(to)) %>% 
      select(to, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(to), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  #Threes Record
  output$THREEpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(X3pm)) %>% 
      select(X3pm, person, player, team) %>% 
      mutate(display_text = paste(X3pm, " Threes Made", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Threes", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Threes
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$THREEleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(X3pm)) %>% 
      select(X3pm, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(X3pm), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  #Dunk Record
  output$DNKpodium <- renderUI({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(dnk)) %>% 
      select(dnk, person, player, team) %>% 
      mutate(display_text = paste(dnk, " Dunks", ",", person, " as ", player, ": ", team))
    
    top_three <- head(leaderboard, 3)$display_text
    top_three <- data.frame(t(data.frame(strsplit(top_three, ","))))
    colnames(top_three) <- c("Dunks", "Player")
    rownames(top_three) <- NULL
    
    points <- top_three$Dunks
    player <- top_three$Player
    
    div(id = "podium",
        div(id = "second_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[2]),
                p(class = "country_label",
                  player[2])
            )
        ),
        div(id = "first_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[1]),
                p(class = "country_label",
                  player[1])
            )
        ),
        div(id = "third_place",
            class = "podium_box",
            div(class = "podium_country",
                p(class = "country_flag",
                  points[3]),
                p(class = "country_label",
                  player[3])
            )
        ),
    )
  })
  output$DNKleaderboard <- renderReactable({
    
    leaderboard <- filtered_records_data() %>% 
      arrange(desc(dnk)) %>% 
      select(dnk, person, player, team) %>% 
      slice(1:20) %>% 
      mutate(rank = rank(desc(dnk), ties.method = "min"))
    
    reactable(data = leaderboard[4:10, ],
              style = list(backgroundColor = "rgba(255, 255, 255, 0.4)"),
              borderless = TRUE,
              outlined = FALSE,
              striped = TRUE,
              pagination = FALSE,
              width = "100%",
              columns = list(
                rank = colDef(
                  html = TRUE,
                  cell = function(value) {
                    value <- paste0("<p class = 'country_rank_row'>",
                                    value, "<sup>th</p>"
                    )
                    value
                  }
                ))
    )
  })
  
  ###-###-###-###-###-###-###
  # Data Entry Tab Output ----
  ###-###-###-###-###-###-###
  
  output$activeDYN <- function(){
    data <- dynasty_data
    players <- input$participants
    n_players <- length(players)
    
    #Get active dynasty IDs by looking if the 'DYNASTY' team won the most recent game
    activeIDs <- data %>% group_by(dynasty_number) %>% 
      filter(context == "Dynasty" | context == "CBC") %>% 
      summarize(
        across(everything(), function(x) {
          if (any(result == "Win")) x[which.max(result == "Loss")] else x
        })) %>% 
      filter(result == "Win") %>% 
      select(dynasty_number) 
    activeIDs <- as.vector(activeIDs$dynasty_number)
    
    #Mutating a vector of participants in each active dynasty
    activeDYNASTY <- data %>% 
      filter(dynasty_number %in% activeIDs) %>% 
      group_by(dynasty_number) %>% 
      distinct(person, .keep_all = T) %>% 
      mutate(gamemode = as.numeric(str_sub(gamemode, start = 1, end =1)),
             people = list(person[1:gamemode])) %>% 
      distinct(dynasty_number, .keep_all = T) %>% 
      ungroup()
    
    if(n_players %in% activeDYNASTY$gamemode){
      activeDYNASTY <- activeDYNASTY %>% 
        filter(gamemode == n_players)
    }
    
    people <- activeDYNASTY$people
    # Match the selected participants with dynasty players to find relevant active dynasties
    for(i in 1:length(people)) {
      if(!any(is.na(match(people[[i]], players)))){
        active <- unlist(as.vector(activeDYNASTY[i,"dynasty_number"]))
      } 
    }
    
    #Return active dynasty information
    if(!exists('active')) {
      print("No active Dynasties")
    } else {
    data %>% 
      filter(dynasty_number == active) %>% 
      filter(dynasty_game_number == max(dynasty_game_number)) %>% 
        arrange(desc(context)) %>% 
      select(game_number, dynasty_number, dynasty_game_number, context, person, team, player, pos) %>% 
       kable() %>% 
       kable_material_dark()}
    
  }
  
  
  # New match up roller
  roll_table <- eventReactive(input$roll, {
    
    rollable_teams <- unique(player_ratings$Team)
    all_teams <- unique(player_ratings$Team)
    rollable_teams <- rollable_teams[!grepl("All-Time.*", rollable_teams)]
    rollable_teams <- rollable_teams[!rollable_teams %in% c("2016 USA Basketball", "2012 USA Basketball", "Team Lebron", "Team Antetokounmpo")]
    
    positions <- c("PG", "SG", "SF", "PF", "C")
    data <- dynasty_data
    players <- input$participants
    n_players <- length(players)
    
    #Get active dynasty IDs by looking if the 'DYNASTY' team won the most recent game
    activeIDs <- data %>% group_by(dynasty_number) %>% 
      filter(context == "Dynasty" | context == "CBC") %>% 
      summarize(
        across(everything(), function(x) {
          if (any(result == "Win")) x[which.max(result == "Loss")] else x
        })) %>% 
      filter(result == "Win") %>% 
      select(dynasty_number) 
    activeIDs <- as.vector(activeIDs$dynasty_number)
    
    #Mutating a vector of participants in each active dynasty
    activeDYNASTY <- data %>% 
      filter(dynasty_number %in% activeIDs) %>% 
      group_by(dynasty_number) %>% 
      distinct(person, .keep_all = T) %>% 
      mutate(gamemode = str_sub(gamemode, start = 1, end =1),
             people = list(person[1:gamemode])) %>% 
      distinct(dynasty_number, .keep_all = T) %>% 
      ungroup()
    #Filter game mode by number of people selected
    if(n_players %in% activeDYNASTY$gamemode){
      activeDYNASTY <- activeDYNASTY %>% 
        filter(gamemode == n_players)
    }
    
    people <- activeDYNASTY$people
    # Match the selected participants with dynasty players to find relevant active dynasties
    for(i in 1:length(people)) {
      if(!any(is.na(match(people[[i]], players)))){
        active <- unlist(as.vector(activeDYNASTY[i,"dynasty_number"]))
      } 
    }
    
    # IF dynasty is active -> fill home team values with active info
    if(exists('active')) {
      home <- data %>% 
        filter(dynasty_number == active,
               result == 'Win') %>% 
        distinct(person) %>% 
        unlist() %>% 
        as.vector()
      
      home_pos <- data %>% 
        filter(dynasty_number == active,
               result == 'Win') %>% 
        distinct(person, pos) %>% 
        select(pos) %>% 
        as.vector()
      
      home_team <- data %>% 
        filter(dynasty_number == active,
               result == 'Win') %>% 
        distinct(team) %>% 
        as.vector()
      
      dynasty_number <- active
      dynasty_game_number <- max(subset(data, dynasty_number == active)$dynasty_game_number) + 1
      
      
    } else { # else - random roll
      home <- sample(players, length(players)/2)
      home_pos <- sample(positions, length(players)/2)
      home_team <- sample(rollable_teams, 1)
      dynasty_number <-  max(data$dynasty_number) + 1
      dynasty_game_number <-  1
    } 
    
    
    away <- players[!players %in% home]
    away_pos <- sample(positions, length(away))
    
    if(exists('active')) {
      away_team <- sample(all_teams, 1)
    } else {
      away_team <- sample(rollable_teams, 1)
    }
    
    
    if(dynasty_game_number > 3) {
      away_team2 <- sample(all_teams, 1)
      away_pos2 <- sample(positions, length(away))
    } else {
      away_team2 <- NA
      away_pos2 <- NA
    }
    
    top <- data.frame(
      game_number = max(data$game_number) + 1,
      dynasty_number = dynasty_number,
      dynasty_game_number = dynasty_game_number, 
      gamemode = paste(length(players), 'man'), 
      difficulty = "All Star", 
      context = NA,
      person = home,
      teammate = NA, 
      team = home_team,
      team2 = NA, 
      player = NA, 
      pos = home_pos,
      pos2 = NA
    )
    
    
    bottom <- data.frame(
      game_number = max(data$game_number) + 1,
      dynasty_number = dynasty_number,
      dynasty_game_number = dynasty_game_number, 
      gamemode = paste(length(players), 'man'), 
      difficulty = "All Star", 
      context = NA,
      person = away,
      teammate = NA, 
      team = away_team,
      team2 = away_team2, 
      player = NA, 
      pos = away_pos,
      pos2 = away_pos2
    )
    
    Rolltable <- rbind(top, bottom) 
    
    Rolltable
  })
  
  output$randomRoll <- render_gt({
    roll <- roll_table()
    
    if(max(roll$dynasty_game_number) > 3) {
      roll_table() %>%
        select(person, team, pos, team2, pos2) %>%
        gt()
    } else {
      roll_table() %>%
        select(person, team, pos) %>%
        gt()
    }
  })
  
  
  ###-###-###-###-###-###-###
  # Data View Tab Output ----
  ###-###-###-###-###-###-###

  output$dataVtable <- DT::renderDataTable({
    dynasty_data <- dynasty_data
    
    unrollables <- c("All-Decade 2000s All-Stars", "All-Decade 1960s All-Stars", "All-Decade 1980s All-Stars", 
                     "All-Decade 1990s All-Stars", "All-Decade 2010s All-Stars", "All-Decade 1970s All-Stars",
                     "Team World", "Team USA", "Team Deron", "Team Jason", "Team Joakim", "Team Pau")
    
    player_ratings <- read.csv("player2k ratings.csv") %>% 
      filter(!Team %in% unrollables) %>% 
      select(!X)
    
    player_ratings <- player_ratings %>% 
      group_by(Team) %>% 
      arrange(desc(Rating)) %>% 
      slice(1:5) %>% 
      summarise(Team_rating = mean(Rating)) %>% 
      right_join(player_ratings, by = join_by("Team"), multiple = "all")
    
    dynasty_data_full <- left_join(dynasty_data, player_ratings, by = join_by(player == Name, team == Team))
    
  })
  
}
