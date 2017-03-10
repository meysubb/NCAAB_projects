library(shiny)

shinyUI(navbarPage("NCAA Division 1: Plots/Stats",
        tabPanel("Inputs",
            sidebarLayout(
                sidebarPanel(
                # Chose Teams
                    selectizeInput("team", "Choose a team:",choices = team_names,options = list(placeholder = "Type a team name,e.g Texas A&M")),
                    selectizeInput("team2", "Choose a team:",choices = team_names,options = list(placeholder = "Type a team name,e.g Texas A&M")),
                    #Team vs. Individual Stats
                    selectInput("stat", "Team or Individual Stats:",choices = c("Team","Individual")),
                    # Select Data Type and Plot Type
                    selectInput("plot_p", "Plot type:",type.plot),
                    uiOutput("plot_t")
                    
                ),
                mainPanel(
                  includeMarkdown("about.Rmd")
                )
              )
            ),
        tabPanel("Table Data",
            dataTableOutput("view")
        ),
        tabPanel("Plots",
            #splitLayout(cellWidths = c("50%","50%"),h2(textOutput("text")),h2(textOutput("text1"))),
            splitLayout(cellWidths = c("50%","50%"),plotOutput("plot1",click="plot_click"),plotOutput("plot2",click="plot_click"))
        ),
        tabPanel("Schedule",
            splitLayout(cellWidths = c("50%","50%"),h2(textOutput("text")),h2(textOutput("text1"))),
            splitLayout(cellWidths = c("50%","50%"),dataTableOutput("team1"),dataTableOutput("team2"))),
        tabPanel("Bracket",
                 tags$img(src='march_madness.jpg'))
))