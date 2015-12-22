library(dplyr)
library(stringr)
library(readxl)
library(sqldf)

setwd("E:/IcebergPro/game_table")

# Чтение файла с событиями и координатами

filelist <- as.data.frame(dir())
names(filelist) <- "name"
events <- read_excel(as.character(filelist$name[1]), 1, col_names = F)
coord <- read.csv(as.character(filelist$name[2]), header = F, sep = ",")
coord.best <- read.csv(as.character(filelist$name[3]), header = F, sep = ",")
names(coord) <- c("Team", "Jersey", "Period", "Time", "X", "Y")
names(coord.best) <- c("Team", "Jersey", "Period", "Time", "X", "Y")
names(events) <- c("Event_team", "Jersey", "Period", "VideoTime", "Event", "xcoord", "ycoord")
filelist <- filter(filelist, name != "coord.csv" & name != "coordBest.csv")
events$xcoord <- as.numeric(events$xcoord)
events$ycoord <- as.numeric(events$ycoord)
events$Period <- as.numeric(events$Period)
match <- read_excel("../temp_table/Match.xlsx", 1)
team.roster <- read_excel("../temp_table/TeamRoster.xlsx", 1)
load("../temp_table/temp.RData")

# Определяем строки только с игровым временем и вычисляем время игры

i <- 1
while(i <= nrow(events)){
  ifelse(events$Event[i] == "Face-off", events$flag[i] <- "1", 
         ifelse(events$Event[i] == "Whistle" | events$Event[i] == "End of period", 
                events$flag[i] <- -1, events$flag[i] <- 0))
  i <- i + 1
}
events$flag2 <- cumsum(events$flag)
events$flag <- events$flag2
events$flag2 <- NULL
events$Hour <- as.numeric(str_extract(events$VideoTime, "\\d+"))
events$Min <- as.numeric(str_extract(str_replace(events$VideoTime, "\\d+", ""), "\\d+"))
events$Sec <- as.numeric(str_extract(str_replace(str_replace(events$VideoTime, "\\d+", ""),"\\d+", ""), "\\d+"))
events$MSec <- as.numeric(str_extract(str_replace(str_replace(str_replace(events$VideoTime, "\\d+", ""), "\\d+", ""),"\\d+", ""), "\\d+"))
events$Seconds <- as.numeric(round(events$MSec/100 + events$Sec + events$Min*60 + events$Hour*3600, 1))

# Определяем длительность момента

events$Duration[nrow(events)] <- 0
i <- 1
while(i <= nrow(events)){
  ifelse(events$Event[i] != "End of period" &  events$Event[i] != "Whistle" & str_sub(events$Event[i], 1, 7) != "Penalty",events$Duration[i] <- round(events$MSec[i+1]/100 + events$Sec[i+1] + events$Min[i+1]*60 + events$Hour[i+1]*3600, 1) - round(events$MSec[i]/100 + events$Sec[i] + events$Min[i]*60 + events$Hour[i]*3600, 1), events$Duration[i] <- 0) 
  i <- i + 1
}
i <- 1
while(i <= nrow(events)){
  ifelse(events$Duration[i] < 0, 
         ifelse(events$Duration[i] >= -5, events$Duration[i] <- abs(events$Duration[i]), 
                events$Duration[i] <- events$Duration[i] + events$Seconds[i-1]), 
         events$Duration[i] <- events$Duration[i])
  i <- i + 1
}
events$Min <- events$Sec <- events$MSec <- events$VideoTime <- events$Hour <- NULL
events[events$Period == 1,]$Seconds <- events[events$Period == 1,]$Seconds - events[events$Period == 1,]$Seconds[1]
events[events$Period == 2,]$Seconds <- events[events$Period == 2,]$Seconds - events[events$Period == 2,]$Seconds[1]
events[events$Period == 3,]$Seconds <- events[events$Period == 3,]$Seconds - min(events[events$Period == 3,]$Seconds)
events$Seconds3[1] <- events$Seconds[1]
i <- 2
while(i <= nrow(events)){
  ifelse(events$Event[i] != "Face off" & events$Event[i-1] != "End of period", 
         events$Seconds3[i] <- events$Seconds3[i-1] + events$Duration[i-1], events$Seconds3[i] <- events$Seconds[i])
  i <- i + 1
}
events$Seconds <- events$Seconds3
events$Seconds3 <- NULL

# Определяем дату игры

events$GameDate <- as.Date(str_extract(filelist$name[1], "\\d{4}-\\d{2}-\\d{2}"), "%Y-%m-%d") 

# Определяем игровые зоны

i <- 1
while(i <= nrow(events)){
  ifelse(events$xcoord[i] <= -8.83, events$Homezone[i] <- "Def", ifelse(events$xcoord[i] >= 8.83, 
                                                                            events$Homezone[i] <- "Off", 
                                                                            events$Homezone[i] <- "Neu"))
  i <- i + 1
}

# Определяем команды хозяев и гостей

del <- as.data.frame(str_locate_all(filelist$name[1], "-"))
events$Hometeam <- str_sub(filelist$name[1], start = del$start[3]+1, 
                             end = del$start[4]-1)
events$Awayteam <- str_sub(filelist$name[1], start = del$start[4]+1, 
                             end = del$start[5]-1)

# Определяем счет и делаем с накопительным итогом

i <- 1
while(i <= nrow(events)){
  ifelse(events$Event[i] == "Goal" & events$Event_team[i] == events$Hometeam[i], 
         events$Homescore[i] <- 1, events$Homescore[i] <- 0)
  ifelse(events$Event[i] == "Goal" & events$Event_team[i] == events$Awayteam[i], 
         events$Awayscore[i] <- 1, events$Awayscore[i] <- 0)
  i <- i + 1
}
events$Homescore <- cumsum(events$Homescore)
events$Awayscore <- cumsum(events$Awayscore)

# Определяем зоны опасности

events$DZ[1] <- 1
i <- 2
while(i <= nrow(events)){
  ifelse(events$ycoord[i] > 7 | events$ycoord[i] < -7, 
         events$DZ[i] <- 1, ifelse((events$ycoord[i] > 2.5 | events$ycoord[i] < -2.5) & (abs(events$xcoord[i]) > 5.8 & abs(events$xcoord[i]) < 14.57), 
                                     events$DZ[i] <- 1, ifelse(ifelse(events$xcoord[i] == 0, 0, tan(events$ycoord[i]/abs(events$xcoord[i]))) >= 0.75, 
                                                                 events$DZ[i] <- 1, ifelse(events$ycoord[i] <= 2.5 & events$ycoord[i] >= -2.5 & 
                                                                                               abs(events$xcoord[i]) <= 28.5 & abs(events$xcoord[i]) >= 23.5, 
                                                                                             events$DZ[i] <- 3, events$DZ[i] <- 2))))
  i <- i + 1
}

# Отмечаем опасный момент

events$Danger[1] <- "-"
i <- 2
while(i <= nrow(events)){
  ifelse(events$DZ[i] == 3 & str_sub(events$Event[i], 1, 4) == "Shot", 
         events$Danger[i] <- "Danger", ifelse(events$DZ[i] == 2 & 
                                                  str_sub(events$Event[i], 1, 4) == "Shot"& events$Event[i] != "Shot Attempt - Blocked", 
                                                events$Danger[i] <- "Danger", ifelse(str_sub(events$Event[i], 1, 4) == "Shot"& events$Event[i] != "Shot Attempt - Blocked" & events$Event[i-1] == "Pass" 
                                                                                       & events$ycoord[i]*events$ycoord[i-1] < 0,events$Danger[i] <- "Danger", 
                                                                                       ifelse(str_sub(events$Event[i-1], 1, 4) == "Shot" & str_sub(events$Event[i], 1, 4) == "Shot" & events$Event[i+1] != "Shot Attempt - Blocked", 
                                                                                              events$Danger[i] <- "Danger", events$Danger[i] <- "-"))))
  i <- i + 1
}

# Вычисляем состояние игры

i <- 1
while(i <= nrow(events)){
  if(events$Awayscore[i] - events$Homescore[i] >= 3) events$score.diff.cat[i] <- "away lead by 3+" else 
    if(events$Awayscore[i] - events$Homescore[i] >= 2) events$score.diff.cat[i] <- "away lead by 2" else 
      if(events$Awayscore[i] - events$Homescore[i] >= 1) events$score.diff.cat[i] <- "away lead by 1" else 
        if(events$Awayscore[i] == events$Homescore[i]) events$score.diff.cat[i] <- "tied" else
          if(events$Homescore[i] - events$Awayscore[i] >= 3) events$score.diff.cat[i] <- "home lead by 3+" else 
            if(events$Homescore[i] - events$Awayscore[i] >= 2) events$score.diff.cat[i] <- "home lead by 2" else
              events$score.diff.cat[i] <- "home lead by 1"
            i <- i + 1
}

# Определяем голкиперов команд из событий

events$HomeG[1] <- 0
events$AwayG[1] <- 0
i <- 2
while(i <= nrow(events)){
  ifelse(str_sub(events$Event[i], 1, 4) == "Save" & events$Event_team[i] == events$Hometeam[i], 
         events$HomeG[i] <- as.character(events$Jersey[i]), ifelse(events$HomeG[i-1] != 0, events$HomeG[i] <- events$HomeG[i-1],events$HomeG[i] <- 0))
  ifelse(str_sub(events$Event[i], 1, 4) == "Save" & events$Event_team[i] == events$Awayteam[i], 
         events$AwayG[i] <- as.character(events$Jersey[i]), ifelse(events$AwayG[i-1] != 0, events$AwayG[i] <- events$AwayG[i-1],events$AwayG[i] <- 0))
  i <- i + 1
}
events[events$HomeG == 0,]$HomeG <- events$HomeG[nrow(events[events$HomeG == 0,])+1]
events[events$AwayG == 0,]$AwayG <- events$AwayG[nrow(events[events$AwayG == 0,])+1]

# Определяем удачный ли был пас

i <- 1
while(i <= nrow(events)){
  ifelse(events$Event[i] == "Pass",
         {
           tt <- events[(i+1):(i+20),];
           tt <- tt[tt$Event_team != "-",];
           tt <- tt[order(tt$Period, tt$Seconds),];
           ifelse(tt$Event_team[1] == events$Event_team[i], events$GoodPass[i] <- 1,
                  events$GoodPass[i] <- 0)
         },
         events$GoodPass[i] <- "-")
  i <- i + 1
}

# Определяем типа паса

events$TypePass[1] <- "-"
i <- 2
while(i <= nrow(events)){
  ifelse(events$GoodPass[i] == "1" & abs((events$xcoord[i] - events$xcoord[i+1])/(events$ycoord[i] - events$ycoord[i+1])) <= 0.675,
         events$TypePass[i] <- "Cross", ifelse(events$GoodPass[i] == 1 & ((events$xcoord[i] < events$xcoord[i+1] & events$Event_team[i] == events$Hometeam) | (events$xcoord[i] > events$xcoord[i+1] & events$Event_team[i] == events$Awayteam)), events$TypePass[i] <- "Forward",
                                                 ifelse(events$GoodPass[i] == 1 & ((events$xcoord[i] > events$xcoord[i+1] & events$Event_team[i] == events$Hometeam) | (events$xcoord[i] < events$xcoord[i+1] & events$Event_team[i] == events$Awayteam)), events$TypePass[i] <- "Backward",
                                                        events$TypePass[i] <- "-")))
  ifelse(events$TypePass[i] == "Cross" & abs(events$ycoord[i] - events$ycoord[i+1]) >= 3 & events$ycoord[i]*events$ycoord[i+1] < 0 & ((events$Event_team[i] == events$Hometeam[i] & events$Homezone[i] == "Off") | (events$Event_team[i] == events$Awayteam[i] & events$Homezone[i] == "Def")), events$TypePass[i] <- str_c(events$TypePass[i], "Royal route", sep = "-"),
         ifelse(events$TypePass[i] == "Forward" & ((events$Event_team[i] == events$Hometeam[i] & abs(events$xcoord[i+1] - 8.83) <= 2) | (events$Event_team[i] == events$Awayteam & abs(-8.83 - events$xcoord[i+1]) <= 2)),
                events$TypePass[i] <- str_c(events$TypePass[i], "Under line", sep = "-"),
                events$TypePass[i] <- events$TypePass[i]))
  i <- i + 1
}
i <- 2
while(i <=nrow(events)){
  ifelse(events$GoodPass[i] == "0" & abs((events$xcoord[i] - events$xcoord[i+1])/(events$ycoord[i] - events$ycoord[i+1])) <= 0.675, 
         events$TypePass[i] <- "Cross", ifelse(events$GoodPass[i] == 0 & ((events$xcoord[i] < events$xcoord[i+1] & events$Event_team[i] == events$Hometeam) | (events$xcoord[i] > events$xcoord[i+1] & events$Event_team[i] == events$Awayteam)), 
                                               events$TypePass[i] <- "Forward",
                                               ifelse(events$GoodPass[i] == 0 & ((events$xcoord[i] > events$xcoord[i+1] & events$Event_team[i] == events$Hometeam) | (events$xcoord[i] < events$xcoord[i+1] & events$Event_team[i] == events$Awayteam)), events$TypePass[i] <- "Backward", events$TypePass[i] <- events$TypePass[i])))
  i <- i + 1
}
i <- 1
while(i <= nrow(events)){
  int <- events[events$Seconds3 >= events$Seconds3[i] & events$Seconds3 <= events$Seconds3[i] + 2,]
  tm <- sqldf("SELECT Event_team, Jersey FROM int GROUP BY Event_team")
  int <- str_count(int$Event, "Shot")
  int <- sum(int)
  ifelse(events$GoodPass[i] == 1 & int != 0 & nrow(tm) == 1,
         events$TypePass[i] <- str_c(events$TypePass[i], "Under shot", sep = "-"),events$TypePass[i] <- events$TypePass[i])
  i <- i + 1
}

events$Event_team2[1] <- as.character(events$Event_team[1])
i <- 2
while(i <= nrow(events)){
  ifelse(events$Event[i] == "Off the boards", events$Event_team2[i] <- as.character(events$Event_team2[i-1]),
         events$Event_team2[i] <- as.character(events$Event_team[i]))
  i <- i + 1
}
events$ComPass[1] <- 1
i <- 2
while(i <= nrow(events)){
  ifelse(events$Event_team2[i] == events$Event_team2[i-1], events$ComPass[i] <- events$ComPass[i-1],
         events$ComPass[i] <- events$ComPass[i-1] + 1)
  i <- i + 1
}
i <- 1
while(i <= nrow(events)){
  ifelse(events$Event_team2[i] == "-", events$ComPass[i] <- 0,
         events$ComPass[i] <- events$ComPass[i])
  i <- i + 1
}
events$Event_team2 <- NULL
i <- 1
while(i <= nrow(events)){
  events$ID[i] <- i
  i <- i + 1
}

i <- 1
while(i <= max(events$ComPass)){
  tt <- events[events$ComPass == i & (events$Event == "Pass" | substr(events$Event, 1, 4) == "Shot" | events$Event == "Goal"),]
  j <- 1
  while(j <= nrow(tt)){
    tt$seq3[j] <- str_c(substr(tt$Event[j],1,4), substr(tt$Event[j+1],1,4), substr(tt$Event[j+2],1,4), sep = "-")
    tt$seq2[j] <- str_c(substr(tt$Event[j],1,4), substr(tt$Event[j+1],1,4), sep = "-")
    ifelse(tt$seq2[j] == "Pass-Shot", tt$TypePass[j] <- str_c(tt$TypePass[j], "PotentialAssist1", sep = "-"),
           ifelse(tt$seq3[j] == "Pass-Pass-Shot", tt$TypePass[j] <- str_c(tt$TypePass[j], "PotentialAssist2", sep = "-"), tt$TypePass[j] <- tt$TypePass[j]))
    j <- j + 1
  }
  tt <- tt[,c("ID","TypePass")]
  events <- merge(events, tt, by = "ID", all = T)
  events$TypePass.y <- str_replace_na(events$TypePass.y, "-")
  j <- 1
  while(j <= nrow(events)){
    ifelse(events$TypePass.y[j] != "-", events$TypePass.x[j] <- events$TypePass.y[j],
           events$TypePass.x[j] <- events$TypePass.x[j])
    j <- j + 1
  }
  events$TypePass.y <- NULL
  events$TypePass <- events$TypePass.x
  events$TypePass.x <- NULL
  #names(events)[22] <- "TypePass"
  i <- i + 1
}
events$TypePass <- str_replace(events$TypePass, "Under shot-PotentialAssist1", "Under shot")
events$TypePass <- str_replace(events$TypePass, "Under shot-PotentialAssist2", "PotentialAssist2")

# Определяем дистанцию до ворот соперника

i <- 1
while(i <= nrow(events)){
  ifelse(events$Event_team[i] == events$Hometeam[i], 
         events$DtA[i] <- sqrt((28.5 - events$xcoord[i])^2 + (events$ycoord[i])^2), 
         ifelse(events$Event_team[i] == events$Awayteam[i], 
                events$DtA[i] <- sqrt((-28.5 - events$xcoord[i])^2 + (events$ycoord[i])^2), events$DtA[i] <- 0))
  i <- i + 1
}

# Проставляем OwnerID

events$OwnerID[1] <- 1
i <- 2
while(i <= nrow(events)){
  if(events$Event_team[i] == events$Event_team[i-1] & events$Jersey[i] == events$Jersey[i-1])
    events$OwnerID[i] <- events$OwnerID[i-1] else events$OwnerID[i] <- events$OwnerID[i-1] + 1
    i <- i + 1
}
i <- 1
while(i <= nrow(events)){
  ifelse(events$Event_team[i] == "-", events$OwnerID[i] <- 0, events$OwnerID[i] <- events$OwnerID[i])
  i <- i + 1
}

# выводим типы паса в отдельные столбцы

events$Pass.cross <- str_extract(events$TypePass, "Cross")
events$Pass.cross <- str_replace_na(events$Pass.cross, "-")
events$Pass.forward <- str_extract(events$TypePass, "Forward")
events$Pass.forward <- str_replace_na(events$Pass.forward, "-")
events$Pass.backward <- str_extract(events$TypePass, "Backward")
events$Pass.backward <- str_replace_na(events$Pass.backward, "-")
events$Pass.royal <- str_extract(events$TypePass, "Royal route")
events$Pass.royal <- str_replace_na(events$Pass.royal, "-")
events$Pass.assist1 <- str_extract(events$TypePass, "PotentialAssist1")
events$Pass.assist1 <- str_replace_na(events$Pass.assist1, "-")
events$Pass.assist2 <- str_extract(events$TypePass, "PotentialAssist2")
events$Pass.assist2 <- str_replace_na(events$Pass.assist2, "-")
events$Pass.under.shot <- str_extract(events$TypePass, "Under shot")
events$Pass.under.shot <- str_replace_na(events$Pass.under.shot, "-")
events$Pass.under.line <- str_extract(events$TypePass, "Under line")
events$Pass.under.line <- str_replace_na(events$Pass.under.line, "-")

# Сборка файла по матчу

match$GameDate <- events$GameDate[1]
match$HomeTeam <- events$Hometeam[1]
match$AwayTeam <- events$Awayteam[1]
match$HomeScore <- max(events$Homescore)
match$AwayScore <- max(events$Awayscore)
match$FirstScore <- str_c(max(events[events$Period == 1,]$Homescore), max(events[events$Period == 1,]$Awayscore), sep = "-")
match$SecondScore <- str_c((max(events[events$Period == 2,]$Homescore)-max(events[events$Period == 1,]$Homescore)), (max(events[events$Period == 2,]$Awayscore) - max(events[events$Period == 1,]$Awayscore)), sep = "-")
match$ThirdScore <- str_c((max(events[events$Period == 3,]$Homescore)-max(events[events$Period == 2,]$Homescore)), (max(events[events$Period == 3,]$Awayscore) - max(events[events$Period == 2,]$Awayscore)), sep = "-")
match$Season <- str_extract(filelist$name, "КХЛ-\\d+-\\d+")
match$Stadium <- filter(team.roster, Team == match$HomeTeam)$Arena

# Собираем файл с координатами шайбы

events$N <- events$Duration*10
puck <- events[0,c("Period","Seconds","xcoord","ycoord")]
pk <- events[,c("Period","Seconds","xcoord","ycoord", "N")]
i <- 1
while(i <= nrow(pk) - 1){
  ifelse(pk$N[i] == 0 | round(pk$N[i], 0) == 1, {sq <- pk[i,-5]; puck <- rbind(puck, sq)}, 
         {sq <- seq(pk$Seconds[i], pk$Seconds[i+1] - 0.1, by = 0.1);
         sq <- data.frame(sq);
         names(sq) <- "Seconds";
         sq$xcoord[1] <- pk$xcoord[i];
         j <- 2;
         while(j <= nrow(sq)){sq$xcoord[j] <- sq$xcoord[j-1] + (pk$xcoord[i+1] - pk$xcoord[i])/pk$N[i]
         j <- j + 1};
         sq$ycoord[1] <- pk$ycoord[i];
         j <- 2;
         while(j <= nrow(sq)){sq$ycoord[j] <- sq$ycoord[j-1] + (pk$ycoord[i+1] - pk$ycoord[i])/pk$N[i]
         j <- j + 1};
         sq$Period <- pk$Period[i];
         sq <- sq[,c("Period", "Seconds", "xcoord", "ycoord")];
         puck <- rbind(puck, sq)})
  i <- i + 1
}
puck <- distinct(puck, Period, Seconds)
names(puck)[2] <- "Time"


# Отмечаем входы в зону и их тип

## Начинаем с формирования пятерок

coord.best[coord.best$Period == 1,]$Time <- coord.best[coord.best$Period == 1,]$Time - min(coord.best[coord.best$Period == 1,]$Time)
coord.best[coord.best$Period == 2,]$Time <- coord.best[coord.best$Period == 2,]$Time - min(coord.best[coord.best$Period == 2,]$Time)
coord.best[coord.best$Period == 3,]$Time <- coord.best[coord.best$Period == 3,]$Time - min(coord.best[coord.best$Period == 3,]$Time)
coord.best$Jersey <- ifelse(str_count(coord.best$Jersey, "\\d") == 2, coord.best$Jersey, str_c("0", coord.best$Jersey, sep = ""))
c.dist <- distinct(coord.best,Period, Time)
c.dist <- c.dist[order(c.dist$Period, c.dist$Time),]
c.dist <- c.dist[, c("Period", "Time")]
c.dist$Homepl[1] <- ""
c.dist$Awaypl[1] <- ""
i <- 1
while(i <= nrow(c.dist)){
  h <- filter(coord.best, Period == c.dist$Period[i] & Time == c.dist$Time[i])
  c.dist$Homepl[i] <- str_c(str_replace_na(h[h$Team == events$Hometeam[1],]$Jersey[1], ""), str_replace_na(h[h$Team == events$Hometeam[1],]$Jersey[2], ""), str_replace_na(h[h$Team == events$Hometeam[1],]$Jersey[3], ""), str_replace_na(h[h$Team == events$Hometeam[1],]$Jersey[4], ""), str_replace_na(h[h$Team == events$Hometeam[1],]$Jersey[5], ""), str_replace_na(h[h$Team == events$Hometeam[1],]$Jersey[6], ""), sep = "-")
  c.dist$Awaypl[i] <- str_c(str_replace_na(h[h$Team == events$Awayteam[1],]$Jersey[1], ""), str_replace_na(h[h$Team == events$Awayteam[1],]$Jersey[2], ""), str_replace_na(h[h$Team == events$Awayteam[1],]$Jersey[3], ""), str_replace_na(h[h$Team == events$Awayteam[1],]$Jersey[4], ""), str_replace_na(h[h$Team == events$Awayteam[1],]$Jersey[5], ""), str_replace_na(h[h$Team == events$Awayteam[1],]$Jersey[6], ""), sep = "-")
  i <- i + 1
}
events$D <- str_c(events$Period, events$Seconds, sep = "-")
c.dist$D <- str_c(c.dist$Period, c.dist$Time, sep = "-")
c.dist[c.dist$Period == 1,]$Time <- c.dist[c.dist$Period == 1,]$Time - min(c.dist[c.dist$Period == 1,]$Time)
c.dist[c.dist$Period == 2,]$Time <- c.dist[c.dist$Period == 2,]$Time - min(c.dist[c.dist$Period == 2,]$Time)
c.dist[c.dist$Period == 3,]$Time <- c.dist[c.dist$Period == 3,]$Time - min(c.dist[c.dist$Period == 3,]$Time)
events <- left_join(events, c.dist, by = "D")

## Зона помечается как нахождение в зоне атаки или обороны до выхода из этой зоны
## или до свистка в этой зоне
events$EZ[1] <- 0
i <- 2
while(i <= nrow(events)){
  ifelse(events$Homezone[i] == "Neu", events$EZ[i] <- 0, 
         ifelse(events$Homezone[i-1] != events$Homezone[i], 
                events$EZ[i] <- max(events$EZ[1:i-1]) + 1, ifelse(events$Event[i] != "Whistle",events$EZ[i] <- events$EZ[i-1], events$EZ[i] <- 0)))
  i <- i + 1
}

## Отмечаем только те входы в зону когда игрок атаки был не далее 5 метров от шайбы
## в течении всего входа

puck$Time <- round(puck$Time, 1)
coord.best$Time <- round(coord.best$Time, 1)
coord.best[coord.best$Period == 1,]$Time <- coord.best[coord.best$Period == 1,]$Time - min(coord.best[coord.best$Period == 1,]$Time)
coord.best[coord.best$Period == 2,]$Time <- coord.best[coord.best$Period == 2,]$Time - min(coord.best[coord.best$Period == 2,]$Time)
coord.best[coord.best$Period == 3,]$Time <- coord.best[coord.best$Period == 3,]$Time - min(coord.best[coord.best$Period == 3,]$Time)
coord.best <- left_join(coord.best, puck, by = c("Period", "Time"))
coord.best$DistanceToPuck <- ((coord.best$X - coord.best$xcoord)^2 + (coord.best$Y - coord.best$ycoord)^2)^0.5
events$Period.y <- events$D <- events$N <- NULL
names(events)[4] <- "Period"
i <- 1
while(i <= max(events$EZ)){
  per <- events[events$EZ == i,]$Period[1]
  mi <- min(events[events$EZ == i,]$Seconds)
  ma <- max(events[events$EZ == i,]$Seconds)
  zn <- events[events$EZ == i,]$Homezone[1]
  rw <- nrow(filter(coord.best, Period == per & Time >= mi & Time < ma & Team == ifelse(zn == "Off", events[events$EZ == i,]$Hometeam[1], events[events$EZ == i,]$Awayteam[1]) & 
         DistanceToPuck <= 5))
  ifelse(rw != 0, events[events$EZ == i,]$EZ <- i, events[events$EZ == i,]$EZ <- 0)
  i <- i + 1
}

## Отмечаем только те входы в зону при которых игрок атакующей команды сделал
## хоть один эвент кроме Reception
# i <- 1
# while(i <= max(events$EZ)){
#   cur <- filter(events, EZ == i)
#   ifelse(cur$Homezone[1] == "Off", var <- nrow(filter(cur, Event_team == Hometeam & Event != "Reception")), 
#          var <- nrow(filter(cur, Event_team == Awayteam & Event != "Reception")))
#   if(var == 0) events[events$EZ == i,]$EZ <- 0 else  events[events$EZ == i,]$EZ <-  events[events$EZ == i,]$EZ
#   i <- i + 1
# }
## Определяем тип входа в зону
### Определяем вход в зону с владением
# events$TypeEZ[1] <- "-"
# i <- 2
# while(i <= nrow(events)){
#   ifelse(events$EZ[i-1] == 0 & events$EZ[i] != 0,ifelse(events$EZ[i] != 0 & ((events$Event_team[i] == events$Hometeam[i] & events$Homezone[i] == "Off") | (events$Event_team[i] == events$Awayteam[i] & events$Homezone[i] == "Def")),
#                                                         ifelse((events$Event_team[i-1] == events$Event_team[i] & events$Event[i-1] == "Skating with the puck") | (events$Event[i-1] == "Skating with the puck" & events$Event_team[i-1] == events$Event_team[i] & events$Jersey[i-1] == events$Jersey[i]) | 
#                                                                  (events$Event[i-1] == "Reception" & abs(8.83 - abs(events$xcoord[i-1])) <= 2 & events$Event_team[i-1] == events$Event_team[i] & events$Jersey[i-1] == events$Jersey[i]),
#                                                                events$TypeEZ[i] <- "Carry in",
#                                                                events$TypeEZ[i] <- "Dump in"), 
#                                                         events$TypeEZ[i] <- "-"), events$TypeEZ[i] <- "-")
#   i <- i + 1
# }
events$TypeEZ[1] <- "-"
i <- 2
while(i <= nrow(events)){
  ifelse(events$EZ[i-1] == 0 & events$EZ[i] != 0, ifelse((events$Event[i-1] == "Skating with the puck" & ((events$Event_team[i-1] == events$Hometeam[i-1] & events$Homezone[i] == "Off")|(events$Event_team[i-1] == events$Awayteam[i-1] & events$Homezone[i] == "Def"))) | 
                                                           (((events$Event_team[i-1] == events$Hometeam[i-1] & events$Homezone[i] == "Off")|(events$Event_team[i-1] == events$Awayteam[i-1] & events$Homezone[i] == "Def")) & 
                                                              events$Event[i-1] == "Reception" & abs(8.83 - abs(events$xcoord[i-1])) <= 2 & events$Event_team[i-1] == events$Event_team[i] & events$Jersey[i-1] == events$Jersey[i]), 
                                                         events$TypeEZ[i] <- "Carry in", ifelse((events$Event_team[i-1] == events$Hometeam[i-1] & events$Homezone[i] == "Off")|(events$Event_team[i-1] == events$Awayteam[i-1] & events$Homezone[i] == "Def"), events$TypeEZ[i] <- "Dump in", events$TypeEZ[i] <- "-")), 
         events$TypeEZ[i] <- "-")
  i <- i + 1
}
# В ДАЛЬНЕЙШЕМ НЕОБХОДИМО ПОДКОРРЕКТИРОВАТЬ ВХОДЫ В ЗОНУ DUMP IN

i <- 1
while(i <= nrow(events)){
  ifelse(events$TypeEZ[i] != "Dump in", i <- i + 1, 
         {
           tmp <- filter(events, Seconds >= events$Seconds[i] & Seconds < events$Seconds[i] + 3)
           ifelse(tmp$Homezone[1] == "Off", 
                  {fst <- tmp$Homepl[1]; lst <- tmp$Homepl[nrow(tmp)]}, 
                  {fst <- tmp$Awaypl[1]; lst <- tmp$Awaypl[nrow(tmp)]});
           k <- 0;
           j <- 1;
           while(j <= str_count(fst, "\\d+")){
             dig <- str_extract(fst, "\\d+")
             ifelse(str_match(lst, dig) == T, k <- k + 1, k <- k)
             fst <- str_replace(fst, dig, "")
             j <- j + 1
           };
           ifelse(k >= 3, events$TypeEZ[i] <- "-", events$TypeEZ[i] <- "Dump in")
           i <- i + 1
         })
}

## Отмечаем локацию входа в зону

events$LocationEZ[1] <- "-"
i <- 2
while(i <= nrow(events)){
  ifelse(events$EZ[i] != 0,
         ifelse(events$Event_team[i-1] == events$Hometeam,
                ifelse(events$ycoord[i] >= 7.5, events$LocationEZ[i] <- "Far Left", 
                       ifelse(events$ycoord[i] <= -7.5, events$LocationEZ[i] <- "Far Right", 
                              ifelse(events$ycoord[i] <= 0, events$LocationEZ[i] <- "Centr Right", events$LocationEZ[i] <- "Centr Left"))),
                ifelse(events$ycoord[i] >= 7.5, events$LocationEZ[i] <- "Far Right", 
                       ifelse(events$ycoord[i] <= -7.5, events$LocationEZ[i] <- "Far Left", 
                              ifelse(events$ycoord[i] <= 0, events$LocationEZ[i] <- "Centr Left", events$LocationEZ[i] <- "Centr Right")))),
         events$LocationEZ[i] <- "-")
  i <- i + 1
}

# Корректируем отметки входа в зону

i <- 2
while(i <= nrow(events)){
  ifelse(events$LocationEZ[i-1] != "-" & events$Homezone[i] == events$Homezone[i-1], 
         events$LocationEZ[i] <- events$LocationEZ[i-1], events$LocationEZ[i] <- events$LocationEZ[i])
  i <- i + 1
}
i <- 1
while(i <= nrow(events) - 1){
  ifelse(events$EZ[i+1] != 0, {events$EZ2[i] <- 1 ; events$TypeEZ2[i] <-  events$TypeEZ[i+1] ; events$LocationEZ2[i] <-  events$LocationEZ[i+1]}, {events$EZ2[i] <- 0; events$TypeEZ2[i] <-  events$TypeEZ[i] ; events$LocationEZ2[i] <-  events$LocationEZ[i]})
  i <- i + 1
}
i <- 1
while(i <= nrow(events)){
  ifelse(events$EZ[i] != 0 & events$EZ2[i] == 0, 
         events$EZ2[i] <- 1, events$EZ2[i] <- events$EZ2[i])
  i <- i + 1
}
events$EZ <- events$EZ2
events$TypeEZ <- events$TypeEZ2
events$LocationEZ <- events$LocationEZ2
events$EZ2 <- events$TypeEZ2 <- events$LocationEZ2 <- NULL
events$ID <- NULL
i <- 2
while(i <= nrow(events)){
  ifelse(events$TypeEZ2[i-1] != "-" & events$Homezone[i] == events$Homezone[i-1], 
         events$TypeEZ2[i] <- events$TypeEZ[i-1], events$TypeEZ2[i] <- events$TypeEZ2[i])
  i <- i + 1
}

# Определяем количество игроков на площадке

events$HomeSkate[1] <- 5
events$AwaySkate[1] <- 5
penH <- 5000
penA <- 5000
events$GameState[1] <- "5v5"
i <- 2
while(i <= nrow(events)){
  ifelse(substr(events$Event[i], 1, 3) == "Pen" & events$Event_team[i] == events$Hometeam[i], {ifelse(events$HomeSkate[i-1] >= 4, events$HomeSkate[i] <- events$HomeSkate[i-1] - 1, events$HomeSkate[i] <- events$HomeSkate[i-1]); penH <- events$Seconds[i] + as.numeric(str_extract(events$Event[i], "\\d+"))*60}, 
         ifelse(events$Seconds[i] > penH, {events$HomeSkate[i] <- (events$HomeSkate[i-1] + 1) ; penH <- 5000}, 
                ifelse(events$Event[i-1] == "Goal" & events$Event_team[i-1] == events$Awayteam[i-1] & penH != 5000, {events$HomeSkate[i] <- (events$HomeSkate[i-1] + 1) ; penH <- 5000}, 
                       events$HomeSkate[i] <- events$HomeSkate[i-1])))
  ifelse(substr(events$Event[i], 1, 3) == "Pen" & events$Event_team[i] == events$Awayteam[i], {ifelse(events$AwaySkate[i-1] >= 4, events$AwaySkate[i] <- events$AwaySkate[i-1] - 1, events$AwaySkate[i] <- events$AwaySkate[i-1]); penA <- events$Seconds[i] + as.numeric(str_extract(events$Event[i], "\\d+"))*60}, 
         ifelse(events$Seconds[i] > penA, {events$AwaySkate[i] <- (events$AwaySkate[i-1] + 1) ; penA <- 5000}, 
                ifelse(events$Event[i-1] == "Goal" & events$Event_team[i-1] == events$Hometeam[i-1] & penA != 5000, {events$AwaySkate[i] <- (events$AwaySkate[i-1] + 1) ; penA <- 5000}, 
                       events$AwaySkate[i] <- events$AwaySkate[i-1])))
  ifelse(events$HomeSkate[i] == events$AwaySkate[i], events$GameState[i] <- "5v5", 
         ifelse(events$HomeSkate[i] > events$AwaySkate[i], events$GameState[i] <- "PP", 
                events$GameState[i] <- "SH"))
  i <- i + 1
}
delH <- data_frame()

# Заменяем значения на ID

match$HomeTeam <- filter(team.roster, Team == match$HomeTeam)$TeamID
match$AwayTeam <- filter(team.roster, Team == match$AwayTeam)$TeamID
events[events$Event_team == events$Hometeam,]$Event_team <- filter(team.roster, Team == events$Hometeam[1])$TeamID
events[events$Event_team == events$Awayteam,]$Event_team <- filter(team.roster, Team == events$Awayteam[1])$TeamID
events$Hometeam <- filter(team.roster, Team == events$Hometeam[1])$TeamID
events$Awayteam <- filter(team.roster, Team == events$Awayteam[1])$TeamID
events$flag <- events$N <- NULL

# Вводим следующие и предыдущие события

events$Event_team.next <- ""
events$Jersey.next <- ""
events$Event_team.prev <- ""
events$Jersey.prev <- ""
events$Event.next <- ""
events$Event.prev <- ""
i <- 2
while(i <= nrow(events)-1){
  events$Event_team.next[i] <- events$Event_team[i+1]
  events$Jersey.next[i] <- events$Jersey[i+1]
  events$Event_team.prev[i] <- events$Event_team[i-1]
  events$Jersey.prev[i] <- events$Jersey[i-1]
  events$Event.next[i] <- events$Event[i+1]
  events$Event.prev[i] <- events$Event[i-1]
  i <- i + 1
}
events$Event_team.next[nrow(events)] <- "-"
events$Event_team.next[1] <- events$Event_team.next[2]
events$Jersey.next[nrow(events)] <- "-"
events$Jersey.next[1] <- events$Jersey.next[2]
events$Event_team.prev[1] <- "-"
events$Event_team.prev[nrow(events)] <- events$Event_team.prev[nrow(events)-1]
events$Jersey.prev[1] <- "-"
events$Jersey.prev[nrow(events)] <- events$Jersey.prev[nrow(events)-1]
events$Event.next[nrow(events)] <- "-"
events$Event.next[1] <- events$Event[2]
events$Event.prev[1] <- "-"
events$Event.prev[nrow(events)] <- events$Event.prev[nrow(events)-1]

plUniq <- read.table("../player_update/plRosterUnic.csv", sep = ";", header = T)

# Составляем таблицу статистик

grd <- distinct(events, Event_team, Jersey)
grd$Jersey <- ifelse(str_count(grd$Jersey, "\\d") == 1, str_c("0", grd$Jersey, ssep = ""), grd$Jersey)
events$Jersey <- ifelse(str_count(events$Jersey, "\\d") == 1, str_c("0", events$Jersey, ssep = ""), events$Jersey)
grdG <- filter(grd, (Event_team == Hometeam & Jersey == HomeG) | (Event_team == Awayteam & Jersey == AwayG))
grd <- filter(grd, (Event_team == Hometeam & Jersey != HomeG) | (Event_team == Awayteam & Jersey != AwayG))
grd <- grd[,c("Event_team", "Jersey")]
grdG <- grdG[,c("Event_team", "Jersey")]
names(grd)[1] <- names(grdG)[1] <- "Team"
grd <- arrange(grd, Team, Jersey)
grdG <- arrange(grdG, Team, Jersey)

# # temp
# events_tmp <- events
# events <- filter(events_tmp, is.na(str_match(Homepl, "55")) == F & is.na(str_match(Homepl, "22")) == F & is.na(str_match(Homepl, "\\d+")) == F)
# events <- filter(events_tmp, is.na(str_match(Homepl, "\\d+")) == F)
# grd <- 
# events <- events_tmp

i <- 1
while(i <= nrow(grd)){
  grd$Goals[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event")], Event == "Goal" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$ShotsAll[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event")], substr(Event, 1, 4) == "Shot" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$ShotsNotBlocked[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event")], (substr(Event, 1, 4) == "Shot" & Event != "Shot Attempt - Blocked" & Event != "Shot Attempt - Wide left" & Event != "Shot Attempt - Wide right" & Event != "Shot Attempt - Over the net") & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$ShotOntarget[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event")], (substr(Event, 1, 4) == "Shot" & Event != "Shot Attempt - Blocked") & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$ShotAccuracy[i] <- str_c(round(as.numeric(grd$ShotOntarget[i])/as.numeric(grd$ShotsAll[i]), 4)*100, "%", sep = "")
  grd$ScoringChance[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Danger")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & Danger == "Danger"))
  grd$Realization[i] <- str_c(round(as.numeric(grd$Goals[i])/as.numeric(grd$ShotOntarget[i]), 4)*100, "%", sep = "")
  grd$Pass[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event")], Event == "Pass" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassOZ[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event", "Hometeam", "Awayteam", "Homezone")], Event == "Pass" & ((Event_team == Hometeam & Homezone == "Off")|(Event_team == Awayteam & Homezone == "Def")) & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassNZ[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event", "Homezone")], Event == "Pass" & Homezone == "Neu" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassDZ[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event", "Hometeam", "Awayteam", "Homezone")], Event == "Pass" & ((Event_team == Hometeam & Homezone == "Def")|(Event_team == Awayteam & Homezone == "Off")) & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassGood[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event", "GoodPass")], GoodPass == 1 & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassGoodOZ[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event", "Hometeam", "Awayteam", "Homezone", "GoodPass")], GoodPass == 1 & ((Event_team == Hometeam & Homezone == "Off")|(Event_team == Awayteam & Homezone == "Def")) & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassGoodNZ[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event", "GoodPass", "Homezone")], GoodPass == 1 & Homezone == "Neu" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassGoodDZ[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Event", "Hometeam", "Awayteam", "Homezone", "GoodPass")], GoodPass == 1 & ((Event_team == Hometeam & Homezone == "Def")|(Event_team == Awayteam & Homezone == "Off")) & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PassPer[i] <- str_c(round((as.numeric(grd$PassGood[i])/as.numeric(grd$Pass[i]))*100,2), "%", sep = "")
  grd$PassPerOZ[i] <- str_c(round((as.numeric(grd$PassGoodOZ[i])/as.numeric(grd$PassOZ[i]))*100,2), "%", sep = "")
  grd$PassPerNZ[i] <- str_c(round((as.numeric(grd$PassGoodNZ[i])/as.numeric(grd$PassNZ[i]))*100,2), "%", sep = "")
  grd$PassPerDZ[i] <- str_c(round((as.numeric(grd$PassGoodDZ[i])/as.numeric(grd$PassDZ[i]))*100,2), "%", sep = "")
  grd$Cross[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.cross")], Pass.cross != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$Forward[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.forward")], Pass.forward != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$Backward[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.backward")], Pass.backward != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$Royal[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.royal")], Pass.royal != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PA1[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.assist1")], Pass.assist1 != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$PA2[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.assist2")], Pass.assist2 != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$US[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.under.shot")], Pass.under.shot != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$UL[i] <- group_size(filter(events[,c("Event_team", "Jersey", "Pass.under.line")], Pass.under.line != "-" & grd$Team[i] == Event_team & grd$Jersey[i] == Jersey))
  grd$Possession[i] <- sum(filter(events[,c("Event_team", "Event_team.next", "Jersey", "Jersey.next", "Event", "Duration")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (Event == "Skating with the puck" | (Event_team == Event_team.next & Jersey == Jersey.next)))$Duration)
  grd$PossessionOff[i] <- sum(filter(events[,c("Event_team", "Event_team.next", "Jersey", "Jersey.next", "Event", "Duration", "Homezone", "Hometeam", "Awayteam")], ((Hometeam == grd$Team[i] & Homezone == "Off")|(Awayteam == grd$Team[i] & Homezone == "Def")), Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (Event == "Skating with the puck" | (Event_team == Event_team.next & Jersey == Jersey.next)))$Duration)
  grd$PossessionDef[i] <- sum(filter(events[,c("Event_team", "Event_team.next", "Jersey", "Jersey.next", "Event", "Duration", "Homezone", "Hometeam", "Awayteam")], ((Hometeam == grd$Team[i] & Homezone == "Def")|(Awayteam == grd$Team[i] & Homezone == "Off")), Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (Event == "Skating with the puck" | (Event_team == Event_team.next & Jersey == Jersey.next)))$Duration)
  
  grd$Min[i] <- floor(sum(filter(events[,c("Event_team", "Event_team.next", "Jersey", "Jersey.next", "Event", "Duration")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (Event == "Skating with the puck" | (Event_team == Event_team.next & Jersey == Jersey.next)))$Duration)/60)
  grd$Pos[i] <- str_c(grd$Min[i], round(as.numeric(grd$Possession[i]) - as.numeric(grd$Min[i])*60, 0), sep = ":")
  grd$Possession.num[i] <- as.numeric(nrow(distinct(filter(events[, c("Event_team", "Event_team.next", "Jersey", "Jersey.next", "Event", "OwnerID")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (Event == "Skating with the puck" | (Event_team == Event_team.next & Jersey == Jersey.next))), OwnerID)))
  grd$AVGPuckPossession[i] <- round(as.numeric(grd$Possession[i])/as.numeric(grd$Possession.num[i]), 2)
  grd$Losses.num[i] <- as.numeric(group_size(filter(events[,c("Event_team", "Jersey", "GoodPass")], grd$Team[i] == Event_team & grd$Jersey[i] == Jersey & GoodPass == "0")))
  grd$LossesPer[i] <- str_c((round(as.numeric(grd$Losses.num[i])/as.numeric(grd$Possession.num[i]), 4))*100, "%", sep = "")
  grd$Interception[i] <- group_size(filter(events[,c("Event_team", "Event_team.prev", "Jersey", "Event.prev")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & Event.prev == "Pass" & Event_team != Event_team.prev))
  grd$Speed.avg[i] <- mean(filter(temp[, c("Team", "Jersey", "AVG_Speed")], Team == grd$Team[i] & Jersey  == grd$Jersey[i])$AVG_Speed)
  grd$Acceleration.avg[i] <- mean(filter(temp[, c("Team", "Jersey", "AVG_Acceleration")], Team == grd$Team[i] & Jersey == grd$Jersey[i])$AVG_Acceleration)
  grd$Dynamism.avg[i] <- sum(filter(temp[, c("Team", "Jersey", "AVG_Speed", "Distance")], Team == grd$Team[i] & Jersey == grd$Jersey[i] & AVG_Speed >= 5.5)$Distance)/sum(filter(temp[, c("Team", "Jersey", "AVG_Speed", "Distance")], Team == grd$Team[i] & Jersey == grd$Jersey[i])$Distance)
  grd$Distance.km[i] <- sum(filter(temp[,c("Team", "Jersey", "Distance")], Team == grd$Team[i] & Jersey == grd$Jersey[i])$Distance)/1000
  grd$TOI[i] <- sum(filter(temp[,c("Team", "Jersey", "Duration")], Team == grd$Team[i] & Jersey == grd$Jersey[i])$Duration)
  grd$EZ.all[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Jersey", "EZ", "TypeEZ")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (EZ == 1 & TypeEZ != "-"))))
  grd$EZ.dump[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Jersey", "EZ", "TypeEZ")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (EZ == 1 & TypeEZ == "Dump in"))))
  grd$EZ.carry[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Jersey", "EZ", "TypeEZ")], Event_team == grd$Team[i] & Jersey == grd$Jersey[i] & (EZ == 1 & TypeEZ == "Carry in"))))
  grd$SF[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Event", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Jersey != grd$Jersey[i] & Event_team == grd$Team[i] & substr(Event, 1, 4) == "Shot")))
  grd$SA[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Event", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Event_team != grd$Team[i] & substr(Event, 1, 4) == "Shot")))
  grd$ShootsOntargerF[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Event", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Jersey != grd$Jersey[i] & Event_team == grd$Team[i] & substr(Event, 1, 4) == "Shot" & Event != "Shot Attempt - Blocked" & Event != "Shot Attempt - Over the net" & Event != "Shot Attempt - Wide left" & Event != "Shot Attempt - Wide right")))
  grd$ShootsOnTargetA[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Event", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Event_team != grd$Team[i] & substr(Event, 1, 4) == "Shot" & Event != "Shot Attempt - Blocked" & Event != "Shot Attempt - Over the net" & Event != "Shot Attempt - Wide left" & Event != "Shot Attempt - Wide right")))
  grd$GF[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Event", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Jersey != grd$Jersey[i] & Event_team == grd$Team[i] & substr(Event, 1, 4) == "Goal")))
  grd$GA[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Event", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Event_team != grd$Team[i] & substr(Event, 1, 4) == "Goal")))
  grd$SCF[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Danger", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Jersey != grd$Jersey[i] & Event_team == grd$Team[i] & Danger == "Danger")))
  grd$SCA[i] <- as.numeric(group_size(filter(events[, c("Event_team", "Danger", "Jersey", "Homepl", "Awaypl", "Hometeam", "Awayteam")], ((is.na(str_match(Awaypl, grd$Jersey[i])) == F & grd$Team[i] == Awayteam)|(is.na(str_match(Homepl, grd$Jersey[i])) == F & grd$Team[i] == Hometeam)) & Event_team != grd$Team[i] & Danger == "Danger")))
  
  
  i <- i + 1
}
i <- 1
while(i <= nrow(grd)){
  grd$PossessionPer[i] <- str_c(round(as.numeric(grd$Possession[i])/sum(as.numeric(filter(grd, Team == grd$Team[i])$Possession)), 4)*100, "%", sep = "")
  i <- i + 1
}
grd$Possession <- grd$Pos
grd$Min <- grd$Pos <- NULL

# выгрузка данных

write.table(events, "../result_table/events.csv", sep = ";", row.names = F)
write.table(puck, "../result_table/puck.csv", sep = ";", row.names = F)
write.table(match, "../result_table/match.csv", sep = ";", row.names = F)
write.table(grd, "../result_table/gridwith.csv", sep = ";", row.names = F)








