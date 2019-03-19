library(shiny)

shinyServer(function(input, output,session) {
  
  updateSelectizeInput(session,'team',
                       choices=team_names,
                       selected="Texas A&M",
                       server=TRUE)
  
  updateSelectizeInput(session,'team2',
                       choices=team_names,
                       selected="Yale",
                       server=TRUE)
  
  hc_tm_dat <- reactive({agg.team %>% filter(team_name==input$team)})
  hc_tm1_dat <- reactive({agg.team %>% filter(team_name==input$team2)})
  
  output$team_season <- renderHighchart({
    x <- c("Team Name","Wins", "Losses","OE","DE")
    y <-c ("{point.team_name}","{point.team_wins}", "{point.team_losses}",
           sprintf("{point.%s:.3f}", c("team_ORTG_sum", "team_DRTG_sum")))
    tltip <- tooltip_table(x,y)
    
    ## team selection
    tam <- hc_tm_dat()
    tex <- hc_tm1_dat()
    avg_ORTG <- mean(agg.team$team_ORTG_sum,na.rm=T) 
    avg_DRTG <- mean(agg.team$team_DRTG_sum,na.rm=T)
    
    ### annotate text
    good_o <- paste( 'Better Offense',sprintf('\u2192'))
    bad_o <- paste( sprintf('\u2190'), 'Worse Offense')
    good_d <- paste( sprintf('\u2191'),'Better Defense')
    bad_d <- paste( sprintf('\u2193'),'Worse Defense')
    
    ### Annotation list for good and bad
    ann_list <-  list(
      list(xValue = avg_ORTG+5, yValue = 83, title = list(text = good_o)),
      list(xValue = avg_ORTG-5, yValue = 83, title = list(text = bad_o)),
      list(xValue = 90, yValue = avg_DRTG-5, title = list(text = good_d)),
      list(xValue = 90, yValue = avg_DRTG+5, title = list(text = bad_d))
    )
    
    highchart() %>%
      hc_add_series(data=agg.team,hcaes(x=team_ORTG_sum,y=team_DRTG_sum),type='scatter',color=hex_to_rgba("#707070", alpha = 0.4),marker = list(symbol = "circle")) %>% 
      hc_add_series(data=tam,hcaes(x=team_ORTG_sum,y=team_DRTG_sum),type='scatter',color='blue',marker = list(symbol = "circle",size=2)) %>% 
      hc_add_series(data=tex,hcaes(x=team_ORTG_sum,y=team_DRTG_sum),type='scatter',color='red',marker = list(symbol = "circle",size=2)) %>% 
      hc_legend() %>% 
      hc_add_annotations(ann_list) %>% 
      hc_yAxis(reversed=T,title = list(text = "Defensive Efficiency")) %>%
      hc_xAxis(title=list(text="Offensive Efficiency")) %>% 
      hc_title(text = "Team Effciency (Per 100 Poss)",align = "left") %>% 
      hc_credits(
        enabled = TRUE,
        text = "@msubbaiah1",
        href = "https://twitter.com/msubbaiah1"
      ) %>% hc_add_theme(hc_theme_flat()) %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)
    
  })
  
  sos_team1 <- reactive({sos %>% filter(team_name == input$team)})
  sos_team2 <- reactive({sos %>% filter(team_name == input$team2)})
  
  output$recordBox_team1 <- renderValueBox({
    df <- agg.team %>% filter(team_name==input$team)
    win <- df$team_wins
    loss <- df$team_losses
    valueBox(
      paste0(win,"-",loss), "Overall Record", icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$recordBox_team2 <- renderValueBox({
    df <- agg.team %>% filter(team_name==input$team2)
    win <- df$team_wins
    loss <- df$team_losses
    valueBox(
      paste0(win,"-",loss), "Overall Record", icon = icon("credit-card"),
      color = "red"
    )
  })
  
  output$rpi_team1 <- renderValueBox({
    df <- sos_team1()
    valueBox(
      df$NET, "NET", icon = icon("credit-card"),
      color = "blue"
    )
  })

  output$rpi_team2 <- renderValueBox({
    df <- sos_team2()
    valueBox(
      df$NET, "NET", icon = icon("credit-card"),
      color = "red"
    )
  })
  
  output$sos_team1 <- renderValueBox({
    df <- sos_team1()
    valueBox(
      df$SOS, "SOS", icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$sos_team2 <- renderValueBox({
    df <- sos_team2()
    valueBox(
      df$SOS, "SOS", icon = icon("credit-card"),
      color = "red"
    )
  })
  
  ### Team Plots
  output$team_plots <- renderPlot({
    ### Create an IF statement here for types of plots
    if(input$team_plot=="Team Volatility"){
      tam_tex <- create_roll_df(ind.team,input$team,input$team2)
      create_roll_plot(tam_tex)
    }else if(input$team_plot == "Morey Index"){
      morey_index(agg.team,input$team,input$team2)
    }else if(input$team_plot == "Four Factors"){
      four_df <- four_factors_df(agg.team,input$team,input$team2)
      four_factors_plot(four_df)
    }
  })
  
  
  output$team_sim <- renderPlot({
    top_teams_names1 <- names(tail(sort(clus_mat[,input$team]),5))
    if(input$team_plot=="Team Volatility"){
    tam_sim <- create_roll_df(ind.team,input$team,team_lst = top_teams_names1,similarity = T)
    create_roll_plot(tam_sim,team1=input$team,similarity = T)
    }else if(input$team_plot == "Morey Index"){
      morey_index(agg.team,input$team,team_lst1=top_teams_names1,similarity=T,col="red")
    }else if(input$team_plot == "Four Factors"){
      top_teams_names1 <- names(tail(sort(clus_mat[,input$team]),10))
      four_df <- four_factors_df(agg.team,input$team,team_lst1=top_teams_names1,similarity = T)
      four_factors_plot(four_df,similarity=T)
    }
  })
  
  output$team_2_sim <- renderPlot({
    top_teams_names2 <- names(tail(sort(clus_mat[,input$team2]),5))
    if(input$team_plot=="Team Volatility"){
      tam_sim_2 <- create_roll_df(ind.team,input$team2,team_lst = top_teams_names2,similarity = T)
      create_roll_plot(tam_sim_2,team1=input$team2,similarity = T)
    }else if(input$team_plot == "Morey Index"){
      morey_index(agg.team,input$team2,team_lst1=top_teams_names2,similarity=T,col="blue")
    }else if(input$team_plot == "Four Factors"){
      top_teams_names1 <- names(tail(sort(clus_mat[,input$team2]),10))
      four_df <- four_factors_df(agg.team,input$team2,team_lst1=top_teams_names1,similarity = T)
      four_factors_plot(four_df,similarity=T)
    }
  })
  
  ### Player PLOTS
  output$player_1_sim <- renderHighchart({
    if(input$player_plot=="Player Effectiveness"){
      usg_l <- create_usg_df(agg.player,input$team)
      create_usg_plot(usg_l,input$team,col="red")
    }else{
      play_plot <- player_min_df(agg.player,conf_team_dat,input$team)
      player_min_plot(play_plot,input$team)
    }
    
  })
  
  output$player_2_sim <- renderHighchart({
    if(input$player_plot=="Player Effectiveness"){
      usg_l2 <- create_usg_df(agg.player,input$team2)
      create_usg_plot(usg_l2,input$team2,col="blue")
    }else{
      play_plot <- player_min_df(agg.player,conf_team_dat,input$team2)
      player_min_plot(play_plot,input$team2)
    }
  })
  
  output$view <- renderDataTable({
    if(input$data_type=="Team Summary"){
      dat <- agg.team %>% select(team_name,team_wins,team_losses,
                                 team_o_ppp,team_d_ppp,team_efg_pct,
                                 winpct,team_pyth_win)
    }else if(input$data_type=="Team Game"){
      dat <- ind.team %>% select(team_name,opp_team_name,team_pts,
                                 opp_pts,points_per_poss,d_points_per_poss)
    }else if(input$data_type=="Player Summary"){
      dat <- agg.player %>% select(player_name,team_name,ast_to,
                                   ts_pct,efg_pct,three_par,reb_rate,
                                   ast_rate,blk_rate,usg_rate)
    }
    
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

})