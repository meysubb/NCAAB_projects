library(tabulizer)
### https://extra.ncaa.org/solutions/rpi/SitePages/Home.aspx <- to get pdf
location <- "select_sunday.pdf"
out <- extract_tables(location,encoding="UTF-8")
### 
raw_mat_12 <- out[[12]]
raw_mat_12 <- raw_mat_12[-c(1:2),] 
rpi <- gsub( " .*$", "", raw_mat_12[,3])
rank <- gsub(".* ", "", raw_mat_12[,3])

non_conf_rpi <- gsub( " .*$", "", raw_mat_12[,5])
non_conf_rec <- gsub(".* ", "", raw_mat_12[,5])
clean_mat_12 <- cbind(raw_mat_12[,1:2],rpi,rank,raw_mat_12[,4],non_conf_rpi,non_conf_rec,raw_mat_12[,6:15])
colnames(clean_mat_12) <- NULL

out[[12]] <- clean_mat_12

final <- do.call(rbind, out)
final_df <- as.data.frame(final)
colnames(final_df) <- c("team_name","rpi","avg_opp_rpi","avg_opp_rank","record",
                        "non_conf_rpi","non_conf_record","conf_record","road_record",
                        "SOS","non_conf_SOS","opp_SOS","non_conf_opp_SOS","Q1","Q2","Q3",
                        "Q4")

sos_clean <- final_df %>% mutate(
  record_wins = gsub("-.*","",record),
  record_losses = gsub(".*-","",record),
  non_conf_record_wins = gsub("-.*","",non_conf_record),
  non_conf_record_losses = gsub(".*-","",non_conf_record),
  conf_record_wins = gsub("-.*","",conf_record),
  conf_record_losses = gsub(".*-","",conf_record),
  road_record_wins = gsub("-.*","",road_record),
  road_record_losses = gsub(".*-","",road_record),
  Q1_wins = gsub("-.*","",Q1),
  Q1_losses = gsub(".*-","",Q1),
  Q2_wins = gsub("-.*","",Q2),
  Q2_losses = gsub(".*-","",Q2),
  Q3_wins = gsub("-.*","",Q3),
  Q3_losses = gsub(".*-","",Q3),
  Q4_wins = gsub("-.*","",Q4),
  Q4_losses = gsub(".*-","",Q4)
)



write.csv(sos_clean,file="final_data_new/SOS_dat.csv",row.names = F)


sos <- read_csv("SOS_dat.csv")
 