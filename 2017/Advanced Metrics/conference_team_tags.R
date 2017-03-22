setwd("conferences")
csv_files <- list.files(pattern="*.csv")
list_csv <- lapply(csv_files, read.csv)
library(plyr)
df <- ldply(list_csv, data.frame)
colnames(df) <- df[1,]

df <- df[!duplicated(df), ]
df <- df[-1,-1]

conf_team_dat <- df %>% select(School,Conf) %>% rename(team_name = School)
rownames(conf_team_dat) <- seq(1:nrow(conf_team_dat))
## Fix some team names 
## State -> St. 

conf_team_dat$team_name <- gsub("State","St.",conf_team_dat$team_name)
conf_team_dat$team_name <- gsub("-"," ",conf_team_dat$team_name)
conf_team_dat$team_name[1] <- "FGCU"
conf_team_dat$team_name[295] <- "Lamar University"
conf_team_dat$team_name[4] <- "USC Upstate"
conf_team_dat$team_name[10] <- "VCU"
conf_team_dat$team_name[23] <- "SMU"
conf_team_dat$team_name[26] <- "UCF"
conf_team_dat$team_name[28] <- "UConn"
conf_team_dat$team_name[33] <- "South Fla."
conf_team_dat$team_name[53] <- "UMBC"
conf_team_dat$team_name[54] <- "UMass Lowell"
conf_team_dat$team_name[64] <- "TCU"
conf_team_dat$team_name[90] <- "Winthrop"
conf_team_dat$team_name[91] <- "UNC Asheville"
conf_team_dat$team_name[93] <- "Gardner-Webb"
conf_team_dat$team_name[137] <- "UNCW"
conf_team_dat$team_name[188] <- "N.C. Central"
conf_team_dat$team_name[200] <- "N.C. A&T"
conf_team_dat$team_name[281] <- "UNCG"
conf_team_dat$team_name[301] <- "Central Ark."
conf_team_dat$team_name[302] <- "McNeese"
conf_team_dat$team_name[305] <- "Omaha"


match_probs <- is.na(match(conf_team_dat$team_name,agg.team$team_name))


conf_team_dat[match_probs,]$team_name <- c(
  "Eastern Wash.","Northern Colo.","Northern Ariz.",
  "Charleston So.","CSUN","Middle Tenn.",
  "UTEP","UAB","Western Ky.",
  "UTSA","Fla. Atlantic","Southern Miss.",
  "FIU","Col. of Charleston","Northern Ky.",
  "Ill.-Chicago","Penn","Bowling Green",
  "Western Mich.","Eastern Mich.","Northern Ill.",
  "Central Mich.","UMES","Bethune-Cookman",
  "Wichita St.","Southern Ill.","UNI",
  "Loyola Chicago","UNLV","Mt. St. Mary's",
  "LIU Brooklyn","Central Conn. St.","St. Francis Brooklyn",
  "Eastern Ky.","UT Martin","	Southeast Mo. St.",
  "Eastern Ill.","SIUE","California",
  "Boston U.","Loyola Maryland","Army West Point",
  "Ole Miss","LSU","ETSU",
  "The Citadel","Western Caro.","VMI",
  "A&M-Corpus Chris","SFA","Southeastern La.",
  "Fort Wayne","Western Ill.","UT Arlington",
  "Ga. Southern","Lafayette","Coastal Caro.",
  "South Ala.","Little Rock","La.-Monroe",
  "Alcorn","Southern U.","Mississippi Val.",
  "Ark.-Pine Bluff","CSU Bakersfield","UMKC",
  "Seattle U","UTRGV","St. Mary's (CA)",
  "BYU")

conf_team_dat$team_name[239] <- "Southeast Mo. St."
saveRDS(conf_team_dat,"conf_team.RDS")
