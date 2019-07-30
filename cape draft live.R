library(readxl)
library(dplyr)
library(ggplot2)

cape_draft_hitters <- data.frame(read_excel("D:/cape_draft.xlsx", sheet = "Draft Eligible Hitters", col_names = TRUE))
cape_draft_pitchers <- data.frame(read_excel("D:/cape_draft.xlsx", sheet = "Draft Eligible Pitchers", col_names = TRUE))

"PITCHERS"
### FILTERED FOR INNINGS PITCHED PITCHERS ###
cape_draft_pitchers <- cape_draft_pitchers %>%
  filter(IP > 10) %>%
  mutate(utility_unit_proxy = round((((IP/2)+(K.9)-(1.2*WHIP)-(ERC))^4)))

###ARRANGE NEW COLUMNS###
cape_draft_pitchers_2=(
  cape_draft_pitchers %>%
    arrange(desc(utility_unit_proxy)) %>%
    mutate(pct_change_dollars = round((Dollar.Value/lag(Dollar.Value)-1),2)) %>%
    mutate(pct_change_utility = round((utility_unit_proxy/lag(utility_unit_proxy)-1))) %>%
    mutate(cost_score = ((utility_unit_proxy)/Dollar.Value)) %>%
    select(Player, P, Dollar.Value, utility_unit_proxy, cost_score))

###CHANGE ROW NAMES TO NUMBERS AND FIX NUMBER FORMATTING###
cape_draft_pitchers_2$cost_score = round(cape_draft_pitchers_2$cost_score,3)


"HITTERS"
cape_draft_hitters <- cape_draft_hitters %>%
  filter(PA > 25) %>%
  mutate(utility_unit_proxy = (round(((PA/3)+(RC)+(OPS*8)+(SECA*5)+(ISOP*5))^3)))

cape_draft_hitters_2 <- cape_draft_hitters %>%
  mutate(pct_change_dollars = round((Dollar.Value/lag(Dollar.Value)-1),2)) %>%
  mutate(pct_change_utility = round((utility_unit_proxy/lag(utility_unit_proxy)-1))) %>%
  mutate(cost_score = ((utility_unit_proxy)/Dollar.Value)) %>%
  select(Player, P, Dollar.Value, utility_unit_proxy, cost_score) %>%
  arrange(desc(utility_unit_proxy))


###CHANGE ROW NAMES TO NUMBERS AND FIX NUMBER FORMATTING###
cape_draft_hitters_2$cost_score = round(cape_draft_hitters_2$cost_score,3)
hitter_ranks = as.numeric(row.names(cape_draft_hitters_2))

"DEFINE MARGINAL UTILITY SCHEDULE"
marginal_utility_schedule_hitters <- data.frame("C" = c(1, 1, 1, 1.5, 2),
                                                "SS" = c(1, 1, 1, 2, 3),
                                                "3B" = c(1.25, 1.5, 2.5, 3, 3),
                                                "2B" = c(1.25, 1.5, 2.5, 3, 3),
                                                "1B" = c(1, 1.25, 2.5, 3, 3),
                                                "IF" = c(1, 1.2, 2, 2, 2.5),
                                                "OF" = c(1, 1.2, 1.75, 2, 2.5),
                                                "CF" = c(1, 1, 1.5, 1.75, 2)
)
marginal_utility_schedule_pitchers <- data.frame("P" = c(1,1,1, 1, 2, 2, 2.5, 2.5, 2.5))

combined_cape_draft <- rbind(cape_draft_hitters_2,cape_draft_pitchers_2)
player_list <- as.list(combined_cape_draft$Player)

row.names(combined_cape_draft) <- player_list

catcher_count = 0
shortstop_count = 0
second_basemen_count = 0
third_basemen_count = 0
first_basemen_count = 0
infielder_count = 0
outfielder_count = 0
centerfielder_count = 0
pitcher_count = 0
marginal_utility_score = list()

test <- function(player){
  if (combined_cape_draft[player,"P"] == "C") {
    if(catcher_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"C"])
    } else if (catcher_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"C"])
    } else if (catcher_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"C"])
    } else if (catcher_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"C"])
    } else if (catcher_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[5,"C"])
    }
  } else if (combined_cape_draft[player,"P"] == "SS") {
    if(shortstop_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"SS"])
    } else if (shortstop_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"SS"])
    } else if (shortstop_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"SS"])
    } else if (shortstop_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"SS"])
    } else if (shortstop_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[5,"SS"])
    }
  } else if (combined_cape_draft[player,"P"] == "1B") {
    if(first_basemen_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"X1B"])
    } else if (first_basemen_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"X1B"])
    } else if (first_basemen_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"X1B"])
    } else if (first_basemen_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"X1B"])
    } else if (first_basemen_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[5,"X1B"])
    }
  } else if (combined_cape_draft[player,"P"] == "3B") {
    if(third_basemen_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"X3B"])
    } else if (third_basemen_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"X3B"])
    } else if (third_basemen_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"X3B"])
    } else if (third_basemen_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"X3B"])
    } else if (third_basemen_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]*marginal_utility_schedule_hitters[5,"X3B"])
    }
  } else if (combined_cape_draft[player,"P"] == "2B") {
    if(second_basemen_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"X2B"])
    } else if (second_basemen_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"X2B"])
    } else if (second_basemen_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"X2B"])
    } else if (second_basemen_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"X2B"])
    } else if (second_basemen_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[5,"X2B"])
    }
  } else if (combined_cape_draft[player,"P"] == "IF") {
    if(infielder_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"IF"])
    } else if (infielder_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"IF"])
    } else if (infielder_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"IF"])
    } else if (infielder_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"IF"])
    } else if (infielder_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[5,"IF"])
    }
  } else if (combined_cape_draft[player,"P"] == "OF") {
    if(outfielder_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"OF"])
    } else if (outfielder_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"OF"])
    } else if (outfielder_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"OF"])
    } else if (outfielder_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"OF"])
    } else if (outfielder_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[5,"OF"])
    }
  } else if (combined_cape_draft[player,"P"] == "CF") {
    if(centerfielder_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[1,"CF"])
    } else if (centerfielder_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[2,"CF"])
    } else if (centerfielder_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[3,"CF"])
    } else if (centerfielder_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[4,"CF"])
    } else if (centerfielder_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_hitters[5,"CF"])
    }
  } else if (combined_cape_draft[player,"P"] == "P") {
    if(pitcher_count == 0){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[1,"P"])
    } else if (pitcher_count == 1){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[2,"P"])
    } else if (pitcher_count == 2){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[3,"P"])
    } else if (pitcher_count == 3){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[4,"P"])
    } else if (pitcher_count == 4){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[5,"P"])
    } else if (pitcher_count == 5){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[6,"P"])
    } else if (pitcher_count == 6){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[7,"P"])
    } else if (pitcher_count == 7){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[8,"P"])
    } else if (pitcher_count == 8){
      marginal_utility_score <<- (combined_cape_draft[player, "utility_unit_proxy"]/marginal_utility_schedule_pitchers[9,"P"])
    }
  }
}

marginal_utility_score_list<-lapply(player_list,test)
marginal_utility_score_list<-unlist(marginal_utility_score_list)
print(length(player_list))
print(length(marginal_utility_score_list))
print(nrow(combined_cape_draft))
combined_cape_draft$marginal_utility_score <- marginal_utility_score_list

combined_cape_draft<-combined_cape_draft %>%
  arrange(desc(marginal_utility_score)) %>%
  mutate(pct_change_dollars = round((Dollar.Value/lag(Dollar.Value)-1),2)) %>%
  mutate(pct_change_utility = round((utility_unit_proxy/lag(utility_unit_proxy)-1),2))


head(combined_cape_draft)
combined_cape_draft[1:25,]
