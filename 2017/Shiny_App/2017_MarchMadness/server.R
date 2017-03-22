library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  updateSelectizeInput(session,'team',
                       choices=team_names,
                       selected="Texas A&M",
                       server=TRUE)
  
  updateSelectizeInput(session,'team2',
                       choices=team_names,
                       selected="Texas",
                       server=TRUE)
  
   
  statInput <- reactive({
    switch(input$stat,
           "Individual" = "ind",
           "Team")})
  
  typeplotInput <- reactive({
    switch(input$plot_p,
           "Variation",
           "Time" = "Time",
           #"Percentile" = "Percentile"
           "Compare" = "Compare")}) 
           
  
  team1 <- reactive({
    input$team
  })
  
  team2 <- reactive({
    input$team2
  })
  
  ## Highchart Data 
  ## Team 1
  hc_tm_dat <- reactive({
    team <- ind.team %>% select(team_name,opp_team_name,ptsdiff,game_date,wins,losses)  %>% filter(team_name == input$team)
    team <- arrange(team, game_date)
    team$wins <- cumsum(team$ptsdiff>0)
    team$losses <- cumsum(team$ptsdiff<0)
    team
  })
  ## Team 2
  hc_tm_dat_2 <- reactive({
    team2 <- ind.team %>% select(team_name,opp_team_name,ptsdiff,game_date,wins,losses)  %>% filter(team_name == input$team2)
    team2 <- arrange(team2, game_date)
    team2$wins <- cumsum(team2$ptsdiff>0)
    team2$losses <- cumsum(team2$ptsdiff<0)
    team2
  })
  ## DataTable Data
  view_dat <- reactive({
    stat_type <- statInput()
    stat <- input$plot_t
    teams <- c(team1(),team2())
    if(stat_type == "ind"){
      l_df <- lookup_df %>% filter(df_loc == "ind.player")
      s_vars <- l_df$stat_type[match(stat,lookup_df$show)]
      sel_vars <- l_df %>% filter(stat_type == s_vars)
      vars_vec <- c("player_name","team_name",as.vector(sel_vars[['df_id']]))
      name_vec <- c("Player Name","Team Name",l_df$show[match(vars_vec[3:length(vars_vec)],l_df$df_id)])
      agg.data <- ind.player %>% filter(team_name %in% teams) %>% select_(.dots = vars_vec)
      agg.data <- agg.data[order(-agg.data[[vars_vec[3]]]),] %>% setNames(nm = name_vec)
    }else if(stat_type == "Team"){
      if(input$plot_p == "Variation"){
        l_df <- lookup_df %>% filter(df_loc == "agg.player")
        s_vars <- l_df$stat_type[match(stat,l_df$show)]
        s_val <- l_df$df_loc[match(stat,l_df$show)]
        sel_vars <- l_df %>% filter(df_loc == s_val) %>% filter(stat_type == s_vars)
        vars_vec <- c("player_name","team_name",as.vector(sel_vars[['df_id']]))
        name_vec <- c("Player Name","Team Name",l_df$show[match(vars_vec[3:length(vars_vec)],l_df$df_id)])
        agg.data <- agg.player %>% filter(team_name %in% teams) %>% select_(.dots = vars_vec)
        agg.data <- agg.data[order(-agg.data[[vars_vec[3]]]),] %>% setNames(nm = name_vec)
      }
      else if(input$plot_p == "Compare"){
        l_df <- lookup_df %>% filter(df_loc == "ind.team")
        s_vars <- l_df$stat_type[match(stat,l_df$show)]
        s_val <- l_df$df_loc[match(stat,l_df$show)]
        sel_vars <- l_df %>% filter(df_loc == s_val) %>% filter(stat_type == s_vars)
        vars_vec <- c("team_name","game_date","opp_team_name",as.vector(sel_vars[['df_id']]))
        name_vec <- c("Team Name","Game Date","Opponent Team Name",l_df$show[match(vars_vec[4:length(vars_vec)],l_df$df_id)])
        agg.data <- ind.team %>% filter(team_name %in% teams) %>% select_(.dots = vars_vec)
        agg.data <- agg.data[order(-agg.data[[vars_vec[4]]]),] %>% setNames(nm = name_vec)
      }
      else if(input$plot_p == "Time"){
        l_df <- lookup_df %>% filter(df_loc == "ind.team")
        s_vars <- l_df$stat_type[match(stat,l_df$show)]
        s_val <- l_df$df_loc[match(stat,l_df$show)]
        sel_vars <- l_df %>% filter(df_loc == s_val) %>% filter(stat_type == s_vars)
        vars_vec <- c("team_name","game_date","opp_team_name",as.vector(sel_vars[['df_id']]))
        name_vec <- c("Team Name","Game Date","Opponent Team Name",l_df$show[match(vars_vec[4:length(vars_vec)],l_df$df_id)])
        agg.data <- ind.team %>% filter(team_name %in% teams) %>% select_(.dots = vars_vec)
        agg.data <- agg.data[order(-agg.data[[vars_vec[4]]]),] %>% setNames(nm = name_vec)
      }
    }
    agg.data
  })
  
  observe({
    if(statInput() == "ind"){
      if(input$plot_p %in% c("Variation","Compare")){
        dat <- lookup_df %>% filter(df_loc == "ind.player")
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = dat$show,
                                               selected = "Pts")})
      }else if(input$plot_p == "Time"){
        dat <- lookup_df %>% filter(df_loc == "ind.player")
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = dat$show,
                                               selected = "Pts")})
      }
    }else if(statInput() == "Team"){
      if(input$plot_p == "Variation"){
        dat <- lookup_df %>% filter(df_loc == "agg.player")
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = dat$show,
                                               selected = "Team Pts")})
      }else if(input$plot_p == "Compare"){
        dat <- lookup_df %>% filter(df_loc == "ind.team")
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = dat$show,
                                               selected = "Team Pts")})
      }
      else if(input$plot_p == "Time"){
        dat <- lookup_df %>% filter(df_loc == "ind.team")
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = dat$show,
                                               selected = "Team Pts")})
      }
    }
    
    })
  ## Datatable Schedule Team 1
  output$team1 <- renderDataTable({
    team_h <- input$team
    team_h_data <- ind.team %>% filter(team_name == team_h)
    team_h_schedule <- team_h_data %>% select(opp_team_name,game_date,neutral_site,home,team_pts,opp_pts)
    team_h_schedule <- team_h_schedule[order(team_h_schedule$game_date),]
    colnames(team_h_schedule)[1] <- "Opponent"
    colnames(team_h_schedule)[2] <- "Date"
    colnames(team_h_schedule)[3] <- "Neutral Site"
    colnames(team_h_schedule)[4] <- "Home Game"
    colnames(team_h_schedule)[5] <- "Points Scored"
    colnames(team_h_schedule)[6] <- "OPP Points Scored"
    team_h_schedule
  })
  
  output$team2 <- renderDataTable({
    team_a <- input$team2
    team_a_data <- ind.team %>% filter(team_name == team_a)
    team_a_schedule <- team_a_data %>% select(opp_team_name,game_date,neutral_site,home,team_pts,opp_pts)
    team_a_schedule <- team_a_schedule[order(team_a_schedule$game_date),]
    colnames(team_a_schedule)[1] <- "Opponent"
    colnames(team_a_schedule)[2] <- "Date"
    colnames(team_a_schedule)[3] <- "Neutral Site"
    colnames(team_a_schedule)[4] <- "Home Game"
    colnames(team_a_schedule)[5] <- "Points Scored"
    colnames(team_a_schedule)[6] <- "OPP Points Scored"
    team_a_schedule
  })  
  
  ## Time Series of Team 1 season
  output$team_season <- renderHighchart({
    team <- hc_tm_dat()
    tit <- paste(team1(), "Season")
    hc <- hchart(team,"column", hcaes(game_date, ptsdiff)) %>% 
      hc_title(text = tit) %>% 
      hc_chart(type = "column") %>% 
      hc_xAxis(title = list(text = "Game Date")) %>% 
      hc_yAxis(title = list(text = "Points Difference")) %>% 
      hc_tooltip(
        headerFormat = as.character(tags$h4("{point.key}", tags$br())),
        pointFormat = tt,
        useHTML = TRUE,
        backgroundColor = "rgba(247,247,247,0.85)",
        borderColor = "rgba(247,247,247,0.85)",
        shadow = FALSE,
        style = list(color = "black", fontSize = "0.8em", fontWeight = "normal"),
        positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: this.chart.plotTop + 0 }; }"),
        shape = "square"
      ) 
    hc
  })
  ## Time Series of Team 2 season
  output$team_season2 <- renderHighchart({
    team2 <- hc_tm_dat_2()
    tit2 <- paste(team2(), "Season")
    hc2 <- hchart(team2,"column", hcaes(game_date, ptsdiff)) %>% 
      hc_title(text = tit2) %>% 
      hc_chart(type = "column") %>% 
      hc_xAxis(title = list(text = "Game Date")) %>% 
      hc_yAxis(title = list(text = "Points Difference")) %>% 
      hc_tooltip(
        headerFormat = as.character(tags$h4("{point.key}", tags$br())),
        pointFormat = tt,
        useHTML = TRUE,
        backgroundColor = "rgba(247,247,247,0.85)",
        borderColor = "rgba(247,247,247,0.85)",
        shadow = FALSE,
        style = list(color = "black", fontSize = "0.8em", fontWeight = "normal"),
        positioner = JS("function () { return { x: this.chart.plotLeft + 15, y: this.chart.plotTop + 0 }; }"),
        shape = "square"
      ) 
    hc2
  })
  ## General Stats (Depends on Team or Player choice)
  output$view <- renderDataTable({
   dat <- view_dat()
   datatable(
     dat,
     rownames = FALSE,
     extensions = 'Buttons',
     options = list(
       dom = 'Bfrtip',
       buttons = c('copy','csv','excel','pdf','print')
     ),
     style = 'bootstrap',
     class = 'table-bordered table-hover')
  },server = FALSE)
  ## Plot1 and Plot2
  output$plot1 <- renderPlotly({
    team <- team1()
    index <- match(input$plot_t,lookup_df$show)
    var_name <- as.character(lookup_df$df_id[index])
    ## If: Player vs Team 
    ## If: Compare, Variation, Time Series
    if(statInput() == "ind"){
      if(typeplotInput() %in%  c("Compare","Variation")){
        ind_var_comp_plot(var_name,team)
      }else if(typeplotInput() == "Time"){
        ind_time_plot(var_name,team)
      }
    }else if(statInput() == "Team"){
      if(typeplotInput() == "Compare"){
        team_comparitive_fun(var_name,team)
      }else if(typeplotInput() == "Variation"){
        team_var_fun(var_name,team)
      }else if(typeplotInput() == "Time"){
        team_time_plot(var_name,team)
      }
    }
  })
  
  output$plot2 <- renderPlotly({
    team <- team2()
    index <- match(input$plot_t,lookup_df$show)
    var_name <- as.character(lookup_df$df_id[index])
    ## If: Player vs Team 
    ## If: Compare, Variation, Time Series
    if(statInput() == "ind"){
      if(typeplotInput() %in%  c("Compare","Variation")){
        ind_var_comp_plot(var_name,team)
      }else if(typeplotInput() == "Time"){
        ind_time_plot(var_name,team)
      }
    }else if(statInput() == "Team"){
      if(typeplotInput() == "Compare"){
        team_comparitive_fun(var_name,team)
      }else if(typeplotInput() == "Variation"){
        team_var_fun(var_name,team)
      }else if(typeplotInput() == "Time"){
        team_time_plot(var_name,team)
      }
    }
  })  
  
})