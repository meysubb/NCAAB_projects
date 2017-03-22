### Create Seperate DataFrames for Agg.Player Agg.Team 
### Variation/Comparitive
colnames(agg.team)[c(82:83,85)] <- paste0("team_",colnames(agg.team)[c(82:83,85)])

cname_lookup_df <- as.data.frame(c(colnames(agg.team),colnames(agg.player)))
colnames(cname_lookup_df)[1] <- "df_id"
cname_lookup_df$df_loc <- c(rep("agg.team",length(colnames(agg.team))),rep("agg.player",length(colnames(agg.player))))
cname_lookup_df$show <- gsub("_"," ",cname_lookup_df$df_id)
cname_lookup_df$show <- gsub("pct"," %",cname_lookup_df$show)
cname_lookup_df$show <- str_to_title(cname_lookup_df$show)
cname_lookup_df$show[13] <- "Team PPG"
cname_lookup_df$show[17] <- "Team Reb/G"
cname_lookup_df$show[36] <- "OPP Team PPG"
cname_lookup_df$show[40] <- "OPP Team Reb/G"
cname_lookup_df$show[48] <- "Team Ast/TO"
cname_lookup_df$show[77] <- "Points/Poss - Off"
cname_lookup_df$show[78] <- "Points/Poss - Def"
cname_lookup_df$show[79] <- "Off-Rating (100 poss)"
cname_lookup_df$show[80] <- "Def-Rating (100 poss)"
cname_lookup_df$show[81] <- "Points/Poss - Diff"
cname_lookup_df$show[84] <- "Pyth Win %"
cname_lookup_df$show[85] <- "Team Three Att Rate"
cname_lookup_df$show[106] <- "PPG"
cname_lookup_df$show[110] <- "Reb/G"
cname_lookup_df$show[118] <- "Ast/G"
cname_lookup_df$show[119] <- "Ast/TO"
cname_lookup_df$show[123] <- "Three Att Rate"
cname_lookup_df$show <- gsub("reb"," Reb",cname_lookup_df$show)
cname_lookup_df$show <- gsub("Three","3 PT",cname_lookup_df$show)
cname_lookup_df$show <- gsub("dbl"," Dbl",cname_lookup_df$show)
cname_lookup_df <- cname_lookup_df[-c(1:2,23:25,46:47,49,75,86:88,120),]
cname_lookup_df <- cname_lookup_df[-grep("Opp",cname_lookup_df$show),]
cname_lookup_df <- cname_lookup_df[-grep("OPP",cname_lookup_df$show),]
cname_lookup_df <- cname_lookup_df[-(48:54),]
rownames(cname_lookup_df) <- seq(1:nrow(cname_lookup_df))

cname_lookup_df$stat_type <- "stat"
## Shooting stats (1:9,44:45,47)
# Agg.team
cname_lookup_df$stat_type[c(1:9,44:45,47)] <- "shooting"
## Pace stats (38:43)
cname_lookup_df$stat_type[c(38:43)] <- "advanced"
## Position based stats
cname_lookup_df$stat_type[c(32:37)] <- "position"
## Record stats
cname_lookup_df$stat_type[c(22:31,46)] <- "record"
## Basic stats 
cname_lookup_df$stat_type[c(10:21)] <- "basic"

# Agg.player
cname_lookup_df$stat_type[c(48:56,72:73)] <- "shooting"
cname_lookup_df$stat_type[c(68:69,74:78)] <- "advanced"
cname_lookup_df$stat_type[c(58,62,70)] <- "game"
cname_lookup_df$stat_type[c(59:61,63:67,71)] <- "basic"



### Create Seperate DataFrames for Ind.Player Ind.Team 
### Time Series Selection 
ind_cname_lookup_df <- as.data.frame(c(colnames(ind.team),colnames(ind.player)))
colnames(ind_cname_lookup_df)[1] <- "df_id"
ind_cname_lookup_df$df_loc <- c(rep("ind.team",length(colnames(ind.team))),rep("ind.player",length(colnames(ind.player))))
ind_cname_lookup_df$show <- gsub("_"," ",ind_cname_lookup_df$df_id)
ind_cname_lookup_df$show <- gsub("pct"," %",ind_cname_lookup_df$show)
ind_cname_lookup_df$show <- str_to_title(ind_cname_lookup_df$show)
ind_cname_lookup_df$show <- gsub("Three","3 PT",ind_cname_lookup_df$show)
ind_cname_lookup_df$show <- gsub("reb"," Reb",ind_cname_lookup_df$show)
ind_cname_lookup_df <- ind_cname_lookup_df[-c(1:8,27:44,45:50,69:92,100:107,123:124),]
rownames(ind_cname_lookup_df) <- seq(1:nrow(ind_cname_lookup_df))
ind_cname_lookup_df$show[39] <- "Points/Poss - Off"
ind_cname_lookup_df$show[40] <- "Points/Poss - Def"
ind_cname_lookup_df$show[42:43] <- toupper(ind_cname_lookup_df$show[42:43])
ind_cname_lookup_df <- ind_cname_lookup_df[-grep("Opp",ind_cname_lookup_df$show),]

ind_cname_lookup_df$stat_type <- "stat"
ind_cname_lookup_df$stat_type[c(1:6,16:18,26:31)] <- "shooting"
ind_cname_lookup_df$stat_type[c(7:15,32:40)] <- "basic"
ind_cname_lookup_df$stat_type[19:25] <- "advanced"


lookup_df <- full_join(cname_lookup_df,ind_cname_lookup_df)
saveRDS(lookup_df,file="lookup.RDS")
