## Try a Team Variation Plot
gen_var_name <- "usg_rate"
sel_vars <- c("player_name","team_name","minutes",gen_var_name)
tamu <- agg.player %>% filter(team_name == "Texas A&M") %>% select_(.dots = sel_vars) %>% filter(minutes>10)
colnames(tamu)[4] <- "vals"
tamu <- arrange(tamu,vals,minutes)


p <- ggplot(data = tamu, aes(x=reorder(substring(player_name, 1, 20), -vals),y=vals,fill=minutes)) + 
  geom_bar(stat = "identity") +
  guides(fill=FALSE, color=FALSE) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
  xlab("") +
  ylab(gen_var_name) +
  ggtitle("Texas A&M")

g <- plotly_build(p)
g$x$layout$showlegend <- FALSE
ggplotly(g)


## Try a Team Comparitive Plot 
## Need Opp Stat for this
# ind.team column names 
# make check for advanced stats 
gen_var_name <- "team_three_fgm"
opp_var_name <- gsub("team","opp",gen_var_name)
sel_vars <- c("team_name",gen_var_name,"game_date")
opp_sel_vars <- c("opp_team_name",opp_var_name,"game_date")
tamu <- ind.team %>% filter(team_name == "Texas A&M") %>% select_(.dots = sel_vars) 
opp_team <- ind.team %>% filter(team_name == "Texas A&M") %>% select_(.dots = opp_sel_vars) %>% rename(team_name = opp_team_name)
colnames(tamu)[2] <- "value"
colnames(opp_team)[2] <- "value"
tamu$variable <- "Team"
opp_team$variable <- "Opponents"

plot <- rbind(tamu,opp_team)


p <- ggplot(data=plot, aes(x=variable, y=value, fill=variable)) +
  geom_jitter(aes(color = variable), alpha = 0.4) +
  geom_boxplot(alpha = 0.7) + 
  guides(fill=FALSE, color=FALSE) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
  xlab("") +
  ylab(gen_var_name) +
  ggtitle("TAMU")

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

## Try a Ind Variation Plot
## Try a Ind Comparitive Plot
gen_var_name <- "pts"
sel_vars <- c("player_name","game_date",gen_var_name)
tamu <- ind.player %>% filter(team_name == "Texas A&M") %>% select_(.dots = sel_vars)
colnames(tamu)[3] <- "vals"
tamu[is.na(tamu)] <- 0
tamu <- arrange(tamu,player_name,game_date)

p <- ggplot(data=tamu, aes(x=reorder(substring(player_name, 1, 20), -vals), y=vals, fill=player_name,label=game_date)) +
  geom_jitter(aes(color = player_name), height = 0, width=0.4) + 
  geom_boxplot(alpha = 0.7) + 
  guides(fill=FALSE, color=FALSE) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
  xlab("") +
  ylab(gen_var_name) +
  ggtitle("Ok State")

## Clean up the Plotly Build
tamu <- arrange(tamu,player_name,game_date)
g <- plotly_build(p)
g$x$layout$showlegend <- FALSE

len <- length(g$x$data)/2
for(i in 1:len){
  p_name <- g$x$data[[i]]$name
  dat <- tamu %>% filter(player_name == p_name)
  g$x$data[[i]]$text <- paste("Player Name:", dat$player_name, "<br>",
                              "Game Date:", dat$game_date, "<br>",
                              "Value:", dat$vals)
}
ggplotly(g)


## Try a Team time series plot
#library(reshape2)
#okstate_plot <- melt(okstate,id.vars = colnames(okstate)[1:3])
gen_var_name <- "ptsdiff"
sel_vars <- c("team_name","game_date","opp_team_name",gen_var_name)
okstate <- ind.team %>% filter(team_name == "Oklahoma St.") %>% select_(.dots = sel_vars)
okstate[is.na(okstate)] <- 0
colnames(okstate)[4] <- "value"
okstate <- arrange(okstate,game_date)

p <- ggplot(data=okstate, aes(x=game_date, y=value)) +
  geom_point(aes(color=opp_team_name)) + 
  geom_line() + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
  xlab("") +
  ylab(gen_var_name) +
  ggtitle("Ok State")

g <- plotly_build(p)
g$x$layout$showlegend <- FALSE
for(i in 1:length(g$x$data)){
  g$x$data[[i]]$text <- paste("Team Name:", okstate$team_name, "<br>",
                              "Opp Team Name:", okstate$opp_team_name, "<br>",
                              "Game Date:", okstate$game_date, "<br>",
                              "Value:", okstate$value)
}
ggplotly(g)

## Try a Ind time series plot
gen_var_name <- "fga"
sel_vars <- c("player_name","team_name",gen_var_name,"game_date")
plays <- ind.player %>% filter(team_name == "Oklahoma St.") %>% filter(minutes>2) %>% select_(.dots = sel_vars)
plays[is.na(plays)] <- 0
plays <- arrange(plays,game_date,-fga)
#plays$player_name <- as.factor(plays$player_name)
colnames(plays)[3] <- "value"

p <- ggplot(data=plays, aes(x=game_date, y=value,fill=player_name)) +
  geom_point(aes(color=player_name)) + 
  facet_wrap( ~ player_name,scales="free_y",ncol=3) + 
  guides(fill=FALSE, color=FALSE) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle = 330, hjust = 0)) + 
  xlab("") +
  ylab(gen_var_name) +
  ggtitle("TAMU")

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
