## ui.R ##
library(shinydashboard)
library(highcharter)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Team",tabName="team",icon=icon("train")),
    menuItem("Players",tabName="ind",icon=icon("child")),
    menuItem("Data",tabName="data_view",icon=icon("database")),
    menuItem("Details",tabName="details",icon=icon("info-circle"))
  )
)

dashboardPage(skin = "red",
  dashboardHeader(title = "March Madness Stats Preview"),
  sidebar,
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Team Comparision",width=12,height=550, status = "danger", solidHeader = TRUE,
                    collapsible = TRUE,
                    column(1),
                    div(class="row",
                        div(style="display:inline-block; padding-right: 20px;",
                            selectizeInput("team", "Choose Team 1:",choices = team_names,width='150px',
                                                         options = list(placeholder = "Type a team name,e.g Texas A&M"))),
                        div(style="display:inline-block; padding-right: 20px;",
                            selectizeInput("team2", "Choose Team 2:",choices = team_names,width='150px',
                                                         options = list(placeholder = "Type a team name,e.g Texas")))),
                    tags$style(type="text/css", '#foo {width: 60px; padding-left:50px;}'),
                    tags$style(type="text/css", '#bar {width: 60px;}'),
                    withSpinner(highchartOutput("team_season")))),
              fluidRow(
                valueBoxOutput("recordBox_team1"),
                column(width = 4, offset = 0, style='padding:0px;'),
                valueBoxOutput("recordBox_team2")),
              fluidRow(
                valueBoxOutput("rpi_team1"),
                column(width = 4, offset = 0, style='padding:0px;'),
                valueBoxOutput("rpi_team2")),
              fluidRow(
                valueBoxOutput("sos_team1"),
                column(width = 4, offset = 0, style='padding:0px;'),
                valueBoxOutput("sos_team2"))),
      tabItem(tabName = "team",
              fluidRow(
                box(title="Team 1 - Overview stats",width=12,height=550,status="danger",solidHeader = TRUE,
                    collapsible = TRUE,
                    column(1),
                    div(class="row",
                        div(style="display:inline-block; padding-right: 20px;",
                            selectizeInput("team_plot", "Choose Plot Type:",choices = c("Team Volatility","Morey Index",
                                                                                        "Four Factors"),width='150px'))),
                    withSpinner(plotOutput("team_plots")))
              ),
              fluidRow(
                box(title="Team 1 - Similar",width=12,height=500,status="danger",solidHeader=TRUE,
                    withSpinner(plotOutput("team_sim")))
              ),
              fluidRow(
                box(title="Team 2 - Similar",width=12,height=500,status="danger",solidHeader=TRUE,
                    withSpinner(plotOutput("team_2_sim")))
              )
              ),
      tabItem(tabName = "ind",
              fluidRow(
                collapsible = TRUE,
                column(1),
                div(class="row",
                    div(style="display:inline-block; padding-right: 20px;",
                        selectizeInput("player_plot", "Choose Plot Type:",choices = c("Player Effectiveness",
                                                                                      "Offensive Density"),width='150px')))
              ),
              fluidRow(
                box(title="Team 1 - Player stats",width=12,height=500,status="danger",solidHeader = TRUE,
                    withSpinner(highchartOutput("player_1_sim")))),
              fluidRow(
                box(title="Team 2 - Player stats",width=12,height=500,status="danger",solidHeader = TRUE,
                    withSpinner(highchartOutput("player_2_sim")))
              )),
    tabItem(tabName = "data_view",
            selectizeInput("data_type","Select Data",choices =
                             c("Team Summary","Team Game","Player Summary"),width='150px'),
            dataTableOutput("view")
    ),
    tabItem(tabName = "details",
            includeMarkdown("info.md")
            )
    ),
  
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))),
    tags$style(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
)