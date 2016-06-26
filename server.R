# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output,session) {
  
  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #	  (it only executes a single time)
  #
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
           "Team")
  })
  
  typeplotInput <- reactive({
    switch(input$plot_p,
           "Variation",
           "Time" = "Time",
           "Comparitive" = "Comparitive",
           "Percentile" = "Percentile")
  }) 
  
  observe({
    if(input$plot_p == "Variation"){
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = ind.team.plot,
                                               selected = "PTS")
        })
      }else if(input$plot_p == "Time"){
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = ind.team.plot,
                                               selected = "PTS")
        })
      }else if(input$plot_p == "Time"){
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = agg.team.plot,
                                               selected = "PTS")
        })
      }else if(input$plot_p == "Percentile"){
        output$plot_t <- renderUI({selectInput("plot_t",label = "Data",
                                               choices = agg.team.plot,
                                               selected = "PTS")
        })
      }
    


  
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  #  1) This function is automatically called to recompute the 
  #     output 
  #  2) The new caption is pushed back to the browser for 
  #     re-display
  # 
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes.
  output$caption <- renderText({
    input$caption
  })

  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed.
  output$view <- renderDataTable({
    stat_type <- statInput()
    if(stat_type == "ind"){
      player_h <- input$team
      player_a <- input$team2
      player_h_data <- subset(agg.player,agg.player$team_name == player_h)
      player_a_data <- subset(agg.player,agg.player$team_name == player_a)
      player_data <- full_join(player_h_data,player_a_data)
      player_data <- player_data[order(-player_data$ptsavg),]
      player_data <- player_data %>% select(player_name,team_name,played,started,ptsavg,astavg,ast_to,rebavg,fgpct,three_fgpct,ftpct)
      cp_names <- colnames(player_data)
      cp_names <- sapply(seq(1:length(cp_names)),function(y){
        index <- match(cp_names[y],var_names$Var)
        if(is.na(index)){
           val <- cp_names[y]
        }else{
          val <- var_names$Names[index]
        }
        return(as.character(val))
        })
      colnames(player_data) <- cp_names
      player_data
    }else if(stat_type == "Team"){
      team_h <- input$team
      team_a <- input$team2
      team_h_data <- subset(agg.team,agg.team$team_name == team_h)
      team_a_data <- subset(agg.team,agg.team$team_name == team_a)
      team_data <- full_join(team_h_data,team_a_data) %>% select(team_name,team_ptsavg,opp_team_ptsavg,team_rebavg,opp_team_rebavg,team_fgpct,opp_team_fgpct,team_three_fgpct,opp_team_three_fgpct,team_ftpct)
      ct_names <- colnames(team_data)
      ct_names <- sapply(seq(1:length(ct_names)),function(y){
        index <- match(ct_names[y],var_names$Var)
        if(is.na(index)){
          val <- ct_names[y]
        }else{
          val <- var_names$Names[index]
        }
        return(as.character(val))
      })
      colnames(team_data) <- ct_names
      team_data
    }
  })
  output$team1 <- renderDataTable({
    team_h <- input$team
    team_h_data <- subset(ind.team,ind.team$thisteam_team_name == team_h)
    team_h_schedule <- team_h_data %>% select(opp_team_name,game_date,neutral_site,home,thisteam_pts,opp_pts)
    team_h_schedule <- team_h_schedule[order(team_h_schedule$game_date),]
    colnames(team_h_schedule)[1] <- "Opponent"
    colnames(team_h_schedule)[2] <- "Date"
    colnames(team_h_schedule)[3] <- "Neutral Site"
    colnames(team_h_schedule)[4] <- "Home Game"
    colnames(team_h_schedule)[5] <- "Points Scored"
    colnames(team_h_schedule)[6] <- "OPP Points Scored"
    team_h_schedule
  })
  output$text <- renderText({
    paste(input$team)
  })
  output$text1 <- renderText({
    paste(input$team2)
  })
  output$team2 <- renderDataTable({
    team_a <- input$team2
    team_a_data <- subset(ind.team,agg.team$team_name == team_a)
    team_a_schedule <- team_a_data %>% select(opp_team_name,game_date,neutral_site,home,thisteam_pts,opp_pts)
    team_a_schedule <- team_a_schedule[order(team_a_schedule$game_date),]
    colnames(team_a_schedule)[1] <- "Opponent"
    colnames(team_a_schedule)[2] <- "Date"
    colnames(team_a_schedule)[3] <- "Neutral Site"
    colnames(team_a_schedule)[4] <- "Home Game"
    colnames(team_a_schedule)[5] <- "Points Scored"
    colnames(team_a_schedule)[6] <- "OPP Points Scored"
    team_a_schedule
  })  
  output$plot1 <- renderPlot({
    stat_type <- statInput()
    index <- match(input$plot_t,var_names$Names)
    gen_var_name <- as.character(var_names$Var[index])
    team_h <- input$team
    if(stat_type == "Team"){
      type <- typeplotInput()
      if(type == "Comparitive"){
        gen_thisteam_var_name <- paste0("team_", gen_var_name)
        gen_opp_var_name <- paste0("opp_team_", gen_var_name)
        tmp_thisteam_stat_df <- subset(agg.team, team_name == team_h, select = c("team_name", gen_thisteam_var_name))
        colnames(tmp_thisteam_stat_df) <- c("team", "vals")
        tmp_opp_stat_df <- subset(agg.team, team_name == team_h, select = c(gen_opp_var_name))
        colnames(tmp_opp_stat_df) <- c("vals")
        tmp_opp_stat_df$team <- c ("Opposing Teams")
        command <- paste0('tmp_avg_stat_df <- data.frame(team=c("National Average", "Tournament Average"), vals=c(mean(agg.team$team_', gen_var_name, ', na.rm=TRUE), mean(tourney_team_agg_stats$team_', gen_var_name, ', na.rm=TRUE)))')
        eval(parse(text=command))
        tmp_stat_df <- rbind(tmp_thisteam_stat_df, tmp_opp_stat_df, tmp_avg_stat_df)
        tmp_stat_df$team <- factor(tmp_stat_df$team, c(team_h, 'Opposing Teams', 'National Average', 'Tournament Average'))
        index <- match(gen_var_name,var_names$Var)
        gen_plot_name <- var_names$Names[index]
        ggplot(data=tmp_stat_df, aes(x=team, y=vals, fill=team)) +
          geom_bar(stat="identity") +
          #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
          xlab("") +
          ylab(gen_plot_name) + 
          scale_fill_manual(values=c("#9E373E", "#319C86", "#999999", "#D1B993")) +
          guides(fill=FALSE) +
          theme_bw()
      }else if(type == "Variation"){
        gen_this_team_var_name <- paste0("thisteam_", gen_var_name)
        gen_opp_var_name <- paste0("opp_", gen_var_name)
        tmp_thisteam_stat_df <- subset(ind.team, thisteam_team_name == team_h, select = c("thisteam_team_name", gen_this_team_var_name))
        colnames(tmp_thisteam_stat_df) <- c("team", "vals")
        tmp_opp_stat_df <- subset(ind.team, thisteam_team_name == team_h, select = c(gen_opp_var_name))
        colnames(tmp_opp_stat_df) <- c("vals")
        tmp_opp_stat_df$team <- c ("Opposing Teams")
        tmp_stat_df <- rbind(tmp_thisteam_stat_df, tmp_opp_stat_df)
        tmp_stat_df$team <- factor(tmp_stat_df$team, c(team_h, 'Opposing Teams'))
        index <- match(gen_var_name,var_names$Var)
        gen_plot_name <- var_names$Names[index]
        ggplot(data=tmp_stat_df, aes(x=team, y=vals, fill=team)) +
          geom_jitter(aes(color = team), alpha = 0.4) +
          geom_boxplot(alpha = 0.7) +
          #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
          xlab("") +
          ylab(gen_plot_name) +
          guides(fill=FALSE, color=FALSE) +
          theme_bw()
      }else if(type == "Time"){
        gen_this_team_var_name <- paste0("thisteam_", gen_var_name)
        gen_opp_var_name <- paste0("opp_", gen_var_name)
        tmp_thisteam_stat_df <- subset(ind.team, thisteam_team_name == team_h, select = c("thisteam_team_name", "game_date", gen_this_team_var_name))
        colnames(tmp_thisteam_stat_df) <- c("team", "game_date", "vals")
        tmp_opp_stat_df <- subset(ind.team, thisteam_team_name == team_h, select = c("game_date", gen_opp_var_name))
        colnames(tmp_opp_stat_df) <- c("game_date", "vals")
        tmp_opp_stat_df$team <- c ("Opposing Teams")
        tmp_stat_df <- rbind(tmp_thisteam_stat_df, tmp_opp_stat_df)
        tmp_stat_df[is.na(tmp_stat_df)] <- 0
        index <- match(gen_var_name,var_names$Var)
        gen_plot_name <- var_names$Names[index]
        ggplot(data=tmp_stat_df, aes(x=game_date, y=vals, group=team, color=team)) +
          geom_line() +
          #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
          xlab("") +
          ylab(gen_plot_name) + 
          labs(color="") +
          theme_bw() + 
          theme(axis.text.x=element_text(angle = 330, hjust = 0), legend.position="bottom")
      }else if(type == "Percentile"){
        # Create a percentile graph for each stat (This might be a bit confusing since high own-team = good, high opp-team = bad. Ideally, you want high, low, respectively.)
        tmp_stat_df <- subset(tourney_team_agg_stats.percentile, team_name == team_h, select = c("team_name", "variable", "percentile"))
        tmp_stat_df$variable <- factor(tmp_stat_df$variable, c("team_fgpct", "opp_team_fgpct", "team_three_fgpct", "opp_team_three_fgpct", "team_ptsavg", "opp_team_ptsavg", "team_offreb", "opp_team_offreb", "team_rebavg", "opp_team_rebavg", "team_ast", "opp_team_ast", "team_to", "opp_team_to", "team_stl", "opp_team_stl", "team_blk", "opp_team_blk", "team_avg_ptsdiff", "opp_team_avg_ptsdiff"))
        ggplot(data=tmp_stat_df, aes(x=variable, y=percentile, fill=percentile)) +
          geom_bar(stat="identity") +
          theme(legend.position = "none", axis.text.x=element_text(size=10, angle = 90, hjust = 0)) +
          scale_x_discrete(expand = c(0, 0), labels=c("FG Pct", "Opp FG Pct", "3Pt Pct", "Opp 3Pt Pct", "Avg Pts", "Opp Avg Pts", "Offensive Reb", "Opp Off Reb", "Avg Reb", "Opp Avg Reb", "Assists", "Opp Assists", "Turnovers", "Opp Turnovers", "Steals", "Opp Steals", "Blocks", "Opp Blocks", "Avg Point Diff", "Opp Avg Pt Diff")) +
          ylim(0, 1) +
          xlab("") +
          ylab("Percentile (relative to tournament teams)") +
          ggtitle(team_h) + 
          scale_fill_gradient(low = "#4199c4", high = "#d12d10") +
          guides(fill=FALSE)
      } 
    }else if(stat_type == "ind"){
      tmp_stat_df <- subset(ind.player, team_name == team_h, select = c("player_name", gen_var_name))
      colnames(tmp_stat_df)[2] <- "vals"
      tmp_stat_df[is.na(tmp_stat_df)] <- 0
      index <- match(gen_var_name,var_names$Var)
      gen_plot_name <- var_names$Names[index]
      ggplot(data=tmp_stat_df, aes(x=reorder(substring(player_name, 1, 20), -vals), y=vals, fill=player_name)) +
        geom_jitter(aes(color = player_name), alpha = 0.4) + geom_boxplot(alpha = 0.7) +
        #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
        #xlab("Player Name") +
        xlab("") +
        ylab(gen_plot_name) +
        ggtitle(team_h) + 
        #ylab(varnames[varnames$Variable == gen_var_name, "Text"]) +
        guides(fill=FALSE, color=FALSE) + theme_bw() + theme(axis.text.x=element_text(angle = 330, hjust = 0))
    }
  })
  output$plot2 <- renderPlot({
    stat_type <- statInput()
    index <- match(input$plot_t,var_names$Names)
    gen_var_name <- as.character(var_names$Var[index])
    team_a <- input$team2
    if(stat_type == "Team"){
      type <- typeplotInput()
      if(type == "Comparitive"){
        gen_thisteam_var_name <- paste0("team_", gen_var_name)
        gen_opp_var_name <- paste0("opp_team_", gen_var_name)
        tmp_thisteam_stat_df <- subset(agg.team, team_name == team_a, select = c("team_name", gen_thisteam_var_name))
        colnames(tmp_thisteam_stat_df) <- c("team", "vals")
        tmp_opp_stat_df <- subset(agg.team, team_name == team_a, select = c(gen_opp_var_name))
        colnames(tmp_opp_stat_df) <- c("vals")
        tmp_opp_stat_df$team <- c ("Opposing Teams")
        command <- paste0('tmp_avg_stat_df <- data.frame(team=c("National Average", "Tournament Average"), vals=c(mean(agg.team$team_', gen_var_name, ', na.rm=TRUE), mean(tourney_team_agg_stats$team_', gen_var_name, ', na.rm=TRUE)))')
        eval(parse(text=command))
        tmp_stat_df <- rbind(tmp_thisteam_stat_df, tmp_opp_stat_df, tmp_avg_stat_df)
        tmp_stat_df$team <- factor(tmp_stat_df$team, c(team_a, 'Opposing Teams', 'National Average', 'Tournament Average'))
        index <- match(gen_var_name,var_names$Var)
        gen_plot_name <- var_names$Names[index]
        ggplot(data=tmp_stat_df, aes(x=team, y=vals, fill=team)) +
          geom_bar(stat="identity") +
          #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
          xlab("") +
          ylab(gen_plot_name) + 
          scale_fill_manual(values=c("#9E373E", "#319C86", "#999999", "#D1B993")) +
          guides(fill=FALSE) +
          theme_bw() 
      }else if(type == "Variation"){
        gen_this_team_var_name <- paste0("thisteam_", gen_var_name)
        gen_opp_var_name <- paste0("opp_", gen_var_name)
        tmp_thisteam_stat_df <- subset(ind.team, thisteam_team_name == team_a, select = c("thisteam_team_name", gen_this_team_var_name))
        colnames(tmp_thisteam_stat_df) <- c("team", "vals")
        tmp_opp_stat_df <- subset(ind.team, thisteam_team_name == team_a, select = c(gen_opp_var_name))
        colnames(tmp_opp_stat_df) <- c("vals")
        tmp_opp_stat_df$team <- c ("Opposing Teams")
        tmp_stat_df <- rbind(tmp_thisteam_stat_df, tmp_opp_stat_df)
        tmp_stat_df$team <- factor(tmp_stat_df$team, c(team_a, 'Opposing Teams'))
        index <- match(gen_var_name,var_names$Var)
        gen_plot_name <- var_names$Names[index]
        ggplot(data=tmp_stat_df, aes(x=team, y=vals, fill=team)) +
          geom_jitter(aes(color = team), alpha = 0.4) +
          geom_boxplot(alpha = 0.7) +
          #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
          xlab("") +
          ylab(gen_plot_name) +
          #ylab(varnames[varnames$Variable == gen_var_name, "Text"]) +
          guides(fill=FALSE, color=FALSE) +
          theme_bw()
      }else if(type == "Time"){
        gen_this_team_var_name <- paste0("thisteam_", gen_var_name)
        gen_opp_var_name <- paste0("opp_", gen_var_name)
        tmp_thisteam_stat_df <- subset(ind.team, thisteam_team_name == team_a, select = c("thisteam_team_name", "game_date", gen_this_team_var_name))
        colnames(tmp_thisteam_stat_df) <- c("team", "game_date", "vals")
        tmp_opp_stat_df <- subset(ind.team, thisteam_team_name == team_a, select = c("game_date", gen_opp_var_name))
        colnames(tmp_opp_stat_df) <- c("game_date", "vals")
        tmp_opp_stat_df$team <- c ("Opposing Teams")
        tmp_stat_df <- rbind(tmp_thisteam_stat_df, tmp_opp_stat_df)
        tmp_stat_df[is.na(tmp_stat_df)] <- 0
        index <- match(gen_var_name,var_names$Var)
        gen_plot_name <- var_names$Names[index]
        ggplot(data=tmp_stat_df, aes(x=game_date, y=vals, group=team, color=team)) +
          geom_line() +
          #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
          xlab("") +
          ylab(gen_plot_name) + 
          #ylab(varnames[varnames$Variable == gen_var_name, "Text"]) +
          labs(color="") +
          theme_bw() +
          theme(axis.text.x=element_text(angle = 330, hjust = 0), legend.position="bottom")
      }else if(type == "Percentile"){
        # Create a percentile graph for each stat (This might be a bit confusing since high own-team = good, high opp-team = bad. Ideally, you want high, low, respectively.)
        tmp_stat_df <- subset(tourney_team_agg_stats.percentile, team_name == team_a, select = c("team_name", "variable", "percentile"))
        tmp_stat_df$variable <- factor(tmp_stat_df$variable, c("team_fgpct", "opp_team_fgpct", "team_three_fgpct", "opp_team_three_fgpct", "team_ptsavg", "opp_team_ptsavg", "team_offreb", "opp_team_offreb", "team_rebavg", "opp_team_rebavg", "team_ast", "opp_team_ast", "team_to", "opp_team_to", "team_stl", "opp_team_stl", "team_blk", "opp_team_blk", "team_avg_ptsdiff", "opp_team_avg_ptsdiff"))
        ggplot(data=tmp_stat_df, aes(x=variable, y=percentile, fill=percentile)) +
          geom_bar(stat="identity") +
          theme(legend.position = "none", axis.text.x=element_text(size=10, angle = 90, hjust = 0)) +
          scale_x_discrete(expand = c(0, 0), labels=c("FG Pct", "Opp FG Pct", "3Pt Pct", "Opp 3Pt Pct", "Avg Pts", "Opp Avg Pts", "Offensive Reb", "Opp Off Reb", "Avg Reb", "Opp Avg Reb", "Assists", "Opp Assists", "Turnovers", "Opp Turnovers", "Steals", "Opp Steals", "Blocks", "Opp Blocks", "Avg Point Diff", "Opp Avg Pt Diff")) +
          ylim(0, 1) +
          xlab("") +
          ylab("Percentile (relative to tournament teams)") +
          ggtitle(team_a) + 
          scale_fill_gradient(low = "#4199c4", high = "#d12d10") +
          guides(fill=FALSE)
      }
    }else if(stat_type == "ind"){
      tmp_stat_df <- subset(ind.player, team_name == team_a, select = c("player_name", gen_var_name))
      colnames(tmp_stat_df)[2] <- "vals"
      tmp_stat_df[is.na(tmp_stat_df)] <- 0
      index <- match(gen_var_name,var_names$Var)
      gen_plot_name <- var_names$Names[index]
      ggplot(data=tmp_stat_df, aes(x=reorder(substring(player_name, 1, 20), -vals), y=vals, fill=player_name)) +
        geom_jitter(aes(color = player_name), alpha = 0.4) + geom_boxplot(alpha = 0.7) +
        #scale_y_continuous(limits=c(min_avg,max_avg), breaks=c(min_avg,((range_avg/8*1)+min_avg),((range_avg/8*2)+min_avg),((range_avg/8*3)+min_avg),((range_avg/8*4)+min_avg),((range_avg/8*5)+min_avg),((range_avg/8*6)+min_avg),((range_avg/8*7)+min_avg),max_avg), oob=rescale_none) +
        #xlab("Player Name") +
        ylab(gen_plot_name) +
        ggtitle(team_a) + 
        #ylab(varnames[varnames$Variable == gen_var_name, "Text"]) +
        guides(fill=FALSE, color=FALSE) + theme_bw() + theme(axis.text.x=element_text(angle = 330, hjust = 0))
    }
  })    
  
    
  })
})