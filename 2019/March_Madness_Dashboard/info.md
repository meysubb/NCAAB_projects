---
title: "March Madness Details"
output: markdown
---

## March Madness - Welcome

The main goal of this app is to find a way to aggregate statistics for all tourney teams. In the past, I always struggled to fill brackets. I only watched 2-3 teams pretty often and had only seen maybe 20 teams or more play through the season that were tourney bound. This meant tons and tons of open tabs on google chrome. 

Two years ago I deubted a Shiny App, that let you browse all team and individual statistics for teams (including basic and advanced). This year, I decided I'd go with the more of an advanced only approach. So here is what you have in this app: 

### Front Page 

Interactive plot highlighting ORTG and DRTG (adjusted per 100 possessions). Note this is not however adjusted for SOS and Conference. You'll also see record, SOS and RPI. 

### Team Page

You have three options of plots to look at here. 

1) Team Volatility - Rolling NetRTG (ORTG - DRTG), on a 3-game basis, looks to show whether a team was consistent or was streaky. Texas A&M we know was definitely streaky, Virginia was definitely consistent.    
2) Morey Index (MI) - You probbaly guessd it, how often is a team shooting the 3 or getting to the basket. The difference however is teams can do this while running at either a slow or fast pace. Trae Young and OU quite quite fast, Texas Tech was much slower. But both had very similary MI values.   
3) Four Factor's - Dean Oliver's traditional four factors, with respect to your conference.  

In addition to these three plots, a similarity metric was calculted to compare teams to other D1 teams. The similarity metric focused on ORTG,DRTG,Possesions per game and the four factors. I'll write up a little something on this and discuss it, used a cosine similarity for those curious. 


### Player Page 

Keeping it simple, two plots here. 

1) USG vs TS%, looking at the player impact.  
2) % Mins vs % Shots (Size shows % #3PTers taken) - studying a team's offensive density/depth, who the main scorere/shooters are. Note if you are looking for a big, they are most likely going to be smaller bubbles than guards. Just the nature of the plot.   

You can view the raw data as well. It's quite large, so I've sampled down to a few columns. Feel free to reach out if you would like all the data (@msubbaiah1)!
