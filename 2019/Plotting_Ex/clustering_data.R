agg.team <- read_csv("final_data/agg_team.csv")

kmeans_agg_team <- agg.team %>% 
  mutate(team_tov_pct = team_to/(team_fga + 0.44 * team_fta + team_to),
         team_ft_rate = team_ft/team_fga,
         team_oreb_pct = team_offreb/opp_team_defreb,
         team_poss_g = team_possessions/(team_wins + team_losses)) %>% 
  select(team_ORTG_sum,team_DRTG_sum,team_tov_pct,
         team_ft_rate,team_oreb_pct,team_poss_g,team_name)

kmeans_agg_team$team_oreb_pct[!is.finite(kmeans_agg_team$team_oreb_pct)] <- median(kmeans_agg_team$team_oreb_pct,na.rm=T)

### finding the most similar team in your cluster
library(lsa)
t_names <- as.character(kmeans_agg_team$team_name)
resid <- kmeans_agg_team %>% select(-team_name)

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

resid <- resid %>% mutate_all(funs(scale_this))

resid <- cbind(resid,t_names)

resid_t <- t(resid)
colnames(resid_t) <- resid_t[7,]
resid_t <- resid_t[-7,]
resid_t <- apply(resid_t, 2, as.numeric)

cos_vals <- cosine(resid_t) - diag(346)

names(tail(sort(cos_vals[,"North Carolina"]),5))
## save as RDS, and don't compress
saveRDS(cos_vals,"cosine_team_matrix_2018.RDS",compress=F)



if(FALSE){
  ### Kmeans cluster to identify similar teams
  wssplot <- function(data, nc=15, seed=1234){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(na.omit(data), centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  }
  ## 6 clusters is good
  wssplot(kmeans_agg_team)
  kmeans_agg_team <-  scale(na.omit(kmeans_agg_team))
  kmeans_cluster <- kmeans(kmeans_agg_team,6)
  k_agg <- agg.team %>% drop_na(team_ORTG_sum,team_DRTG_sum,winpct,team_pyth_win,team_possessions) %>% mutate(
    clus = as.factor(kmeans_cluster$cluster))
}
