#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(gt)
library(knitr)
library(kableExtra)
library(reactable)
library(sass)

# Used in the private app to read updating data from a Google Sheet
# library(googledrive)
# library(googlesheets4)
# library(gargle)


# Sass file for Podium elements, adapted from online source
css <- sass(sass_file("style.scss"))


background2k <- "https://images2.minutemediacdn.com/image/upload/c_crop,w_1326,h_745,x_0,y_32/c_fill,w_1080,ar_16:9,f_auto,q_auto,g_auto/images%2FvoltaxMediaLibrary%2Fmmsport%2Fdbltap_en_international_web%2F01gchcsybnreya1wp1gp.jpg"
unrollables <- c("All-Decade 2000s All-Stars", "All-Decade 1960s All-Stars", "All-Decade 1980s All-Stars", 
                 "All-Decade 1990s All-Stars", "All-Decade 2010s All-Stars", "All-Decade 1970s All-Stars",
                 "Team World", "Team USA", "Team Deron", "Team Jason", "Team Joakim", "Team Pau")
player_ratings <- read.csv("player2k ratings.csv") %>% 
  filter(!Team %in% unrollables) %>% 
  select(!X)


dashboardPage(
  dashboardHeader(title = "NBA2K23"),
  dashboardSidebar(
# Sidebar -----------------------------------------------------------------
    sidebarMenu(
      menuItem(text = "Home",
               tabName = "home",
               icon = icon("house")),
      menuItem(text = "Player Averages",
               tabName = "averages",
               icon = icon("basketball")),
      menuItem(text = "Records",
               tabName = "records",
               icon = icon("medal")),
      menuItem(text = "Dynasty Roller",
               tabName = "dataEntry",
               icon = icon("database")),
      menuItem(text = "Data Viewer",
               tabName = "dataViewer",
               icon = icon("table")),
      menuItem(text = "Github Link",
               href = "",
               icon = icon("link"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(css)),
    
    tabItems(
      ###-###-###-###-###
      # Home Tab --------
      ###-###-###-###-###
      tabItem(tabName = "home",
              fluidRow(
                img(src = "https://cdn.prgloo.com/media/b69ffe5adaad4994aac76157de10af3a.png?width=1200&height=1800")
              ),
              fluidRow(),
              fluidRow(
                box(title = "Upcoming Milestones", 
                    solidHeader = TRUE,
                    background = "maroon"),
                box(title = "Longest Dynasties",
                    solidHeader = TRUE, 
                    background = "maroon")
              ),
              fluidRow(
                box(gt_output("milestones")),
                uiOutput("DYNpodium")
              )
              ),

      ###-###-###-###-###
      # Averages Tab ----
      ###-###-###-###-###
      tabItem(tabName = "averages",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel("Filters"),
                    selectInput("AVGteammate", "Filter by Teammate:", 
                                choices = c("ALL", "Ben", "Frank", "Guy", "MJ", "Riley", "Nick")),
                    textInput("AVGplayer", "Filter by Player:"),
                    checkboxGroupInput("AVGposition", "Filter by Position:", 
                                       choices = c("PG", "SG", "SF", "PF", "C"),
                                       selected = c("PG", "SG", "SF", "PF", "C")),
                    checkboxGroupInput("AVGdynasty", "Filter by Dynasty:", 
                                       choices = c("Dynasty", "Challenger", "CBC"), 
                                       selected = c("Dynasty", "Challenger", "CBC")),
                    selectInput("AVGresult", "Filter by Result:", 
                                choices = c("ALL", "Win", "Loss"))
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel(title = "Box Score",
                               gt_output("averages")),
                      tabPanel(title = "Metrics",
                               gt_output("metrics")),
                      tabPanel(title = "Other",
                               tableOutput("averageHTML"))
                    )
                  )
                )
              )),
      
      ###-###-###-###-###
      # Records Tab -----
      ###-###-###-###-###
      tabItem(tabName = "records",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel("Filters"),
                    checkboxGroupInput("RECperson", "Filter by Person:", 
                                       choices = c("Ben", "Frank", "Guy", "MJ", "Riley", "Nick"), 
                                       selected = c("Ben", "Frank", "Guy", "MJ", "Riley", "Nick"), inline = TRUE),
                    textInput("RECplayer", "Filter by Player:"),
                    checkboxGroupInput("RECposition", "Filter by Position:", 
                                       choices = c("PG", "SG", "SF", "PF", "C"),
                                       selected = c("PG", "SG", "SF", "PF", "C")),
                    checkboxGroupInput("RECdynasty", "Filter by Dynasty:", 
                                       choices = c("Dynasty", "Challenger", "CBC"), 
                                       selected = c("Dynasty", "Challenger", "CBC")),
                    selectInput("RECresult", "Filter by Result:", 
                                choices = c("ALL", "Win", "Loss")),
                    width = 3
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel(title = "Points Record",
                               fluidPage(uiOutput("PTSpodium"),
                                         reactableOutput("PTSleaderboard"))
                               ),
                      tabPanel(title = "Rebound Record",
                               fluidPage(uiOutput("REBpodium"),
                                         reactableOutput("REBleaderboard"))
                               ),
                      tabPanel(title = "Assist Record",
                               fluidPage(uiOutput("ASTpodium"),
                                         reactableOutput("ASTleaderboard"))
                               ),
                      tabPanel(title = "Steals Record",
                               fluidPage(uiOutput("STLpodium"),
                                         reactableOutput("STLleaderboard"))
                               ),
                      tabPanel(title = "Blocks Record",
                               fluidPage(uiOutput("BLKpodium"),
                                         reactableOutput("BLKleaderboard"))
                               ),
                      tabPanel(title = "Turnover Record",
                               fluidPage(uiOutput("TOpodium"),
                                         reactableOutput("TOleaderboard"))
                               ),
                      tabPanel(title = "3 Point Record",
                               fluidPage(uiOutput("THREEpodium"),
                                         reactableOutput("THREEleaderboard"))
                               ),
                      tabPanel(title = "Dunk Record",
                               fluidPage(uiOutput("DNKpodium"),
                                         reactableOutput("DNKleaderboard"))
                               )
                    ),
                    width = 9
                  )
                )
              )),
      
      ###-###-###-###-###
      # Data Entry Tab ----
      ###-###-###-###-###
      tabItem(tabName = "dataEntry",
              h2("Dynasty Roller"),
              fluidRow(
                box(checkboxGroupInput('participants', 
                                       "Who is playing tonight",
                                       choices = c("Ben", "Frank", "Guy", "MJ", "Riley", "Nick")),
                    width = 4),
                box(tableOutput('activeDYN'),
                    width = 8)
              ),
              fluidRow(
                box(actionBttn(inputId = "roll",
                               label = "Roll",
                               color = "primary"))
              ),
              fluidRow(box(gt_output('randomRoll')))
              ),
      
      ###-###-###-###-###
      # Data View Tab ----
      ###-###-###-###-###
      tabItem(tabName = "dataViewer",
              fluidPage(box(
                title = "Full Data",
                DT::dataTableOutput("dataVtable"),
                width = 12
                )
              ))
      
    )
  )
)
