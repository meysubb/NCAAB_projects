team_var_fun <- function(var,t_name){
  gen_var_name <- var
  l_df <- lookup_df %>% filter(df_loc == "agg.player")
  plot_name <- l_df$show[match(gen_var_name,l_df$df_id)]
  sel_vars <- c("player_name","team_name","minutes",gen_var_name)
  team <- agg.player %>% filter(team_name == t_name) %>% select_(.dots = sel_vars) %>% filter(minutes>10)
  colnames(team)[4] <- "vals"
  team <- arrange(team,vals,minutes)
  
  p <- ggplot(data = team, aes(x=reorder(substring(player_name, 1, 20), -vals),y=vals,fill=minutes,label=vals)) + 
    geom_bar(stat = "identity") +
    guides(fill=FALSE, color=FALSE) + 
    geom_text(aes(label=minutes),vjust=5,colour="black") +
    theme_bw() + 
    theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
    xlab("") +
    ylab(plot_name) +
    ggtitle(t_name)
  
  g <- plotly_build(p)
  g$x$layout$showlegend <- FALSE
  ggplotly(g)
}

team_comparitive_fun <- function(var,t_name){
  ## IF advanced stat, plot without opponent stat. 
  l_df <- lookup_df %>% filter(df_loc == "ind.team")
  index <- match(var,l_df$df_id)
  s_vars <- l_df$stat_type[index]
  if(s_vars == "advanced"){
    gen_var_name <- var
    plot_name <- l_df$show[match(gen_var_name,l_df$df_id)]
    ## pace stats
    if(gen_var_name %in% c("d_points_per_poss","ORTG_sum","points_per_poss","DRTG_sum")){
      if(gen_var_name %in% c("d_points_per_poss","DRTG_sum")){
        vec_match <- c("d_points_per_poss","DRTG_sum")
        ind <- match(gen_var_name,vec_match)
        if(ind == 1){
          opp_var_name <- "points_per_poss"
        }
        else{
          opp_var_name <- gsub("D","O",gen_var_name)
        }
        
      }
      else if(gen_var_name %in% c("points_per_poss","ORTG_sum")){
        opp_var_name <- paste0("d_",tolower(gen_var_name))
      }
      sel_vars <- c("team_name",var,"game_date")
      opp_sel_vars <- c("opp_team_name",opp_var_name,"game_date")
      team_c <- ind.team %>% filter(team_name == t_name) %>% select_(.dots = sel_vars) 
      opp_team <- ind.team %>% filter(team_name == t_name) %>% select_(.dots = opp_sel_vars) %>% rename(team_name = opp_team_name)
      colnames(team_c)[2] <- "value"
      colnames(opp_team)[2] <- "value"
      team_c$variable <- "Team"
      opp_team$variable <- "Opponents"
      plot <- rbind(team_c,opp_team)
    }
    else{
      sel_vars <- c("team_name",var,"game_date")
      team_c <- ind.team %>% filter(team_name == t_name) %>% select_(.dots = sel_vars) 
      colnames(team_c)[2] <- "value"
      team_c$variable <- "Team"
      plot <- team_c
    }
  }
 else{
    gen_var_name <- var
    opp_var_name <- gsub("team","opp",var)
    plot_name <- l_df$show[match(gen_var_name,l_df$df_id)]
    sel_vars <- c("team_name",var,"game_date")
    opp_sel_vars <- c("opp_team_name",opp_var_name,"game_date")
    team_c <- ind.team %>% filter(team_name == t_name) %>% select_(.dots = sel_vars) 
    opp_team <- ind.team %>% filter(team_name == t_name) %>% select_(.dots = opp_sel_vars) %>% rename(team_name = opp_team_name)
    colnames(team_c)[2] <- "value"
    colnames(opp_team)[2] <- "value"
    team_c$variable <- "Team"
    opp_team$variable <- "Opponents"
    plot <- rbind(team_c,opp_team)
  }
    
  
  p <- ggplot(data=plot, aes(x=variable, y=value, fill=variable)) +
    geom_jitter(aes(color = variable), alpha = 0.4) +
    geom_boxplot(alpha = 0.7) + 
    guides(fill=FALSE, color=FALSE) + 
    theme_bw() + 
    xlab("") +
    ylab(plot_name) +
    ggtitle(t_name)
  
  g <- plotly_build(p)
  g$x$layout$showlegend <- FALSE
  len <- length(g$x$data)/2
  for(i in 1:len){
    p_name <- g$x$data[[i]]$name
    dat <- plot %>% filter(variable == p_name)
    g$x$data[[i]]$text <- paste("Team Name:", dat$team_name, "<br>",
                                "Game Date:", dat$game_date, "<br>",
                                "Value:", dat$value)
  }
  ggplotly(g)
}

ind_var_comp_plot <- function(var,t_name){
  gen_var_name <- var
  l_df <- lookup_df %>% filter(df_loc == "ind.player")
  plot_name <- l_df$show[match(gen_var_name,l_df$df_id)]
  sel_vars <- c("player_name","game_date",gen_var_name)
  ind_t <- ind.player %>% filter(team_name == t_name) %>% select_(.dots = sel_vars)
  colnames(ind_t)[3] <- "vals"
  ind_t[is.na(ind_t)] <- 0
  ind_t <- arrange(ind_t,player_name,game_date)
  
  p <- ggplot(data=ind_t, aes(x=reorder(substring(player_name, 1, 20), -vals), y=vals, fill=player_name,label=game_date)) +
    geom_jitter(aes(color = player_name), height = 0, width=0.4,alpha=0.7) + 
    geom_boxplot(alpha = 0.7) + 
    guides(fill=FALSE, color=FALSE) + 
    theme_bw() + 
    theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
    xlab("") +
    ylab(plot_name) +
    ggtitle(t_name)
  
  ## Clean up the Plotly Build
  ind_t <- arrange(ind_t,player_name,game_date)
  g <- plotly_build(p)
  g$x$layout$showlegend <- FALSE
  
  len <- length(g$x$data)/2
  for(i in 1:len){
    p_name <- g$x$data[[i]]$name
    dat <- ind_t %>% filter(player_name == p_name)
    g$x$data[[i]]$text <- paste("Player Name:", dat$player_name, "<br>",
                                "Game Date:", dat$game_date, "<br>",
                                "Value:", dat$vals)
  }
  ggplotly(g)
}

team_time_plot <- function(var,t_name){
  gen_var_name <- var
  l_df <- lookup_df %>% filter(df_loc == "ind.team")
  s_vars <- l_df$stat_type[match(var,l_df$df_id)]
  #if(s_vars == "advanced"){
  #  if(gen_var_name %in% c("points_per_poss","ORTG_sum")){
  #    sel_vars <- c("team_name","game_date","opp_team_name",gen_var_name)
  #  }
  #}
  #else{
    #l_df <- lookup_df %>% filter(df_loc == "ind.team")
    plot_name <- l_df$show[match(gen_var_name,l_df$df_id)]
    sel_vars <- c("team_name","game_date","opp_team_name",gen_var_name)
    team_time_plot <- ind.team %>% filter(team_name == t_name) %>% select_(.dots = sel_vars)
    team_time_plot[is.na(team_time_plot)] <- 0
    colnames(team_time_plot)[4] <- "value"
    team_time_plot <- arrange(team_time_plot,game_date)
  #}
  
  
  
  p <- ggplot(data=team_time_plot, aes(x=game_date, y=value)) +
    geom_point(aes(color=opp_team_name)) + 
    geom_line(linetype = "longdash") + 
    theme_bw() + 
    theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
    xlab("") +
    ylab(plot_name) +
    ggtitle(t_name)
  
  g <- plotly_build(p)
  g$x$layout$showlegend <- FALSE
  for(i in 1:length(g$x$data)){
    g$x$data[[i]]$text <- paste("Team Name:", team_time_plot$team_name, "<br>",
                                "Opp Team Name:", team_time_plot$opp_team_name, "<br>",
                                "Game Date:", team_time_plot$game_date, "<br>",
                                "Value:", team_time_plot$value)
  }
  ggplotly(g)
}

ind_time_plot <- function(var,t_name){
  gen_var_name <- var
  l_df <- lookup_df %>% filter(df_loc == "ind.player")
  plot_name <- l_df$show[match(gen_var_name,l_df$df_id)]
  sel_vars <- c("player_name","team_name",gen_var_name,"game_date")
  plays <- ind.player %>% filter(team_name == t_name) %>% filter(minutes>2) %>% select_(.dots = sel_vars)
  plays[is.na(plays)] <- 0
  colnames(plays)[3] <- "value"
  plays <- arrange(plays,game_date,-value)
  #plays$player_name <- as.factor(plays$player_name)
  
  
  p <- ggplot(data=plays, aes(x=game_date, y=value,fill=player_name)) +
    #geom_point(aes(color=player_name)) + 
    #geom_line(linetype = "longdash") + 
    geom_bar(stat = "identity") + 
    facet_wrap( ~ player_name,ncol=3) + 
    guides(fill=FALSE, color=FALSE) + 
    theme_bw() + 
    theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
    xlab("") +
    ylab(plot_name) +
    ggtitle(t_name)
  
  g <- plotly_build(p)
  g$x$layout$showlegend <- FALSE
  len <- length(g$x$data)
  for(i in 1:len){
    p_name <- g$x$data[[i]]$name
    dat <- plays %>% filter(player_name == p_name)
    g$x$data[[i]]$text <- paste("Player Name:", dat$player_name, "<br>",
                                "Game Date:", dat$game_date, "<br>",
                                "Value:", dat$value)
  }
  ggplotly(g)
}