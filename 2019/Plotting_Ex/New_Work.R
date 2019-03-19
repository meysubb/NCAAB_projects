### Aggerate Team plot
### Compare offensive vs. Defensive Efficiency 

library(highcharter)

## Tooltip
x <- c("Team Name","Wins", "Losses","OE","DE")
y <-c ("{point.team_name}","{point.team_wins}", "{point.team_losses}",
       sprintf("{point.%s:.3f}", c("ORTG_sum", "DRTG_sum")))
tltip <- tooltip_table(x,y)

## team selection
tam <- agg.team %>% filter(team_name=="Texas A&M")
tex <- agg.team %>% filter(team_name=="Texas")
avg_ORTG <- mean(agg.team$ORTG_sum,na.rm=T) 
avg_DRTG <- mean(agg.team$DRTG_sum,na.rm=T)

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
  hc_add_series(data=agg.team,hcaes(x=ORTG_sum,y=DRTG_sum),type='scatter',color=hex_to_rgba("#707070", alpha = 0.4),marker = list(symbol = "circle")) %>% 
  hc_add_series(data=tam,hcaes(x=ORTG_sum,y=DRTG_sum),type='scatter',color='blue',marker = list(symbol = "circle",size=2)) %>% 
  hc_add_series(data=tex,hcaes(x=ORTG_sum,y=DRTG_sum),type='scatter',color='red',marker = list(symbol = "circle",size=2)) %>% 
  hc_legend() %>% 
  hc_add_annotations( ann_list) %>% 
  hc_yAxis(reversed=T,title = list(text = "Defensive Efficiency")) %>%
  hc_xAxis(title=list(text="Offensive Efficiency")) %>% 
  hc_title(text = "Team Effciency (Per 100 Poss)",align = "left") %>% 
  hc_credits(
    enabled = TRUE,
    text = "@msubbaiah1",
    href = "https://twitter.com/msubbaiah1"
  ) %>% hc_add_theme(hc_theme_flat()) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)


#### Highcharts for teams schedule
library(zoo)
library(ggthemes)
team <- ind.team %>% 
  select(team_name,opp_team_name,ORTG,DRTG,game_date,wins,losses,ptsdiff)  %>% 
  filter(team_name %in% c('Texas','Texas A&M')) %>% 
  group_by(team_name) %>% 
  arrange(game_date) %>% 
  mutate(game_num = row_number(),
         NRTG = ORTG - DRTG,
         streakW = cumsum(ifelse(ptsdiff>0,1,0)),
         streakL =cumsum(ifelse(ptsdiff<0,1,0)),
         roll_NetRTG = rollapply(NRTG,3,FUN=mean,align='right',fill=NA)) %>% ungroup() %>% 
  mutate(fill2 = ifelse(roll_NetRTG>0,"Good","Bad"))

### Base plot of rolling netratings


team_interest <- ggplot(team,aes(x=game_num,y=roll_NetRTG)) + 
  geom_bar(aes(fill=fill2),stat='identity',alpha=0.75) + 
  geom_hline(yintercept =0,linetype='dashed') + 
  scale_fill_manual(values=c("Good" = "navyblue", "Bad" = "firebrick3")) + 
  facet_wrap(~team_name) + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(x="Game",y='',
       title="Net Rating in 3-Game Rolling Intervals",
       subtitle = 'NetRTG (Per 100 possessions)',
       caption="@msubbaiah1") + 
  theme(strip.text.x = element_text(size = 12, colour = "maroon",face="bold"),
        axis.title.x = element_text(size=16),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(color="#999999")) + 
  guides(fill=FALSE)


### Kmeans cluster to identify similar teams
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
        set.seed(seed)
        wss[i] <- sum(kmeans(na.omit(data), centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

kmeans_agg_team <- agg.team %>% select(ORTG_sum,DRTG_sum,winpct,pyth_win,possessions)
## 6 clusters is good
wssplot(kmeans_agg_team)

kmeans_agg_team <-  scale(na.omit(kmeans_agg_team))

kmeans_cluster <- kmeans(kmeans_agg_team,6)
k_agg <- agg.team %>% drop_na(ORTG_sum,DRTG_sum,winpct,pyth_win,possessions) %>% mutate(
  clus = as.factor(kmeans_cluster$cluster)
)

### finding the most similar team in your cluster
library(lsa)
resid <- kmeans_agg_team 
resid <- cbind(resid,k_agg$team_name)
resid_t <- t(resid)
colnames(resid_t) <- resid_t[6,]
resid_t <- resid_t[-6,]
resid_t <- apply(resid_t, 2, as.numeric)
cos_vals <- cosine(resid_t) - diag(346)

top_teams_names1 <- names(tail(sort(cos_vals[,"Texas A&M"]),5))
top_teams_names2 <- names(tail(sort(cos_vals[,"Texas"]),5))

team1_sim <- ind.team %>% 
  select(team_name,opp_team_name,ORTG,DRTG,game_date,wins,losses,ptsdiff) %>%
  filter(team_name %in% top_teams_names1) %>% 
  group_by(team_name) %>% 
  arrange(game_date) %>% 
  mutate(game_num = row_number(),
         NRTG = ORTG - DRTG,
         streakW = cumsum(ifelse(ptsdiff>0,1,0)),
         streakL =cumsum(ifelse(ptsdiff<0,1,0)),
         roll_NetRTG = rollapply(NRTG,3,FUN=mean,align='right',fill=NA)) %>% ungroup() %>% 
  mutate(fill2 = ifelse(roll_NetRTG>0,"Good","Bad"))


team1_sim_plot <- ggplot(team1_sim,aes(x=game_num,y=roll_NetRTG)) + 
  geom_bar(aes(fill=fill2),stat='identity',alpha=0.75) + 
  geom_hline(yintercept =0,linetype='dashed') + 
  scale_fill_manual(values=c("Good" = "navyblue", "Bad" = "firebrick3")) + 
  facet_wrap(~team_name,ncol=1) + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(x="Game",y='',
       title="5 similar teams to TAMU",
       subtitle = 'NetRTG (Per 100 possessions)',
       caption="@msubbaiah1") + 
  theme(strip.text.x = element_text(size = 12, colour = "maroon",face="bold"),
        axis.title.x = element_text(size=16),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(color="#999999")) + 
  guides(fill=FALSE)


team2_sim <- ind.team %>% 
  select(team_name,opp_team_name,ORTG,DRTG,game_date,wins,losses,ptsdiff)  %>% 
  filter(team_name %in% top_teams_names2) %>% 
  group_by(team_name) %>% 
  arrange(game_date) %>% 
  mutate(game_num = row_number(),
         NRTG = ORTG - DRTG,
         streakW = cumsum(ifelse(ptsdiff>0,1,0)),
         streakL =cumsum(ifelse(ptsdiff<0,1,0)),
         roll_NetRTG = rollapply(NRTG,3,FUN=mean,align='right',fill=NA)) %>% ungroup() %>% 
  mutate(fill2 = ifelse(roll_NetRTG>0,"Good","Bad"))

team2_sim_plot <- ggplot(team2_sim,aes(x=game_num,y=roll_NetRTG)) + 
  geom_bar(aes(fill=fill2),stat='identity',alpha=0.75) + 
  geom_hline(yintercept =0,linetype='dashed') + 
  scale_fill_manual(values=c("Good" = "navyblue", "Bad" = "firebrick3")) + 
  facet_wrap(~team_name,ncol=1) + 
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) + 
  labs(x="Game",y='',
       title="5 similar teams to Texas",
       subtitle = 'NetRTG (Per 100 possessions)',
       caption="@msubbaiah1") + 
  theme(strip.text.x = element_text(size = 12, colour = "maroon",face="bold"),
        axis.title.x = element_text(size=16),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(color="#999999")) + 
  guides(fill=FALSE)

library(cowplot)

### note make sure that y-ranges match on both plots
plot_grid(team2_sim_plot,team1_sim_plot, ncol = 2, align = 'v')
ggplotly(team_interest)
