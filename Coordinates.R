library(dplyr)
library(stringr)
library(sqldf)
library(readxl)

# Устанавливаем текущую директорию

setwd("E:/Iceberg/game_table")

# Формируем список файлов

filelist <- as.data.frame(dir())
names(filelist) <- "name"
coord <- read.csv(as.character(filelist$name[2]), header = F, sep = ",")
names(coord) <- c("Team", "Jersey", "Period", "Time", "X", "Y")
temp <- read.table("../temp_table/cap.txt", header = T, sep = ";")
temp.soc <- read.table("../temp_table/cap.soc.txt", header = T, sep = ";")
team.roster <- read_excel("../temp_table/TeamRoster.xlsx", 1)


# Формируем столбец с чистым временем

tm <- distinct(coord, Period, Time)
tm <- tm[, c(3,4)]
tm <- tm[order(tm$Period, tm$Time),]
i <- 1
while(i <= nrow(tm)){
  ifelse(tm$Period[i] == tm$Period[i+1], tm$duration[i] <- tm$Time[i+1] - tm$Time[i], tm$duration[i] <- 0)
  ifelse(tm$duration[i] > 0.5, tm$duration[i] <- 0.1, tm$duration[i] <- tm$duration[i])
  i <- i + 1
}
tm$Gametime <- tm$Time
i <- 2
while(i <= nrow(tm)){
  ifelse(tm$Period[i] == tm$Period[i-1], tm$Gametime[i] <- tm$Gametime[i-1] + tm$duration[i-1], tm$Gametime[i] <- 0)
  i <- i + 1
}
tm[tm$Period == 1, ]$Gametime <- tm[tm$Period == 1, ]$Gametime - min(tm[tm$Period == 1, ]$Gametime)
tm[tm$Period == 2, ]$Gametime <- tm[tm$Period == 2, ]$Gametime - min(tm[tm$Period == 2, ]$Gametime)
tm[tm$Period == 3, ]$Gametime <- tm[tm$Period == 3, ]$Gametime - min(tm[tm$Period == 3, ]$Gametime)
coord <- left_join(coord, tm, by = c("Period", "Time"))
coord$duration <- NULL
coord.best <- coord[,c(1,2,3,7,5,6)]
coord$Gametime <- NULL
coord <- left_join(coord, team.roster, by = "Team")
coord <- coord[, c("TeamID", "Jersey", "Period", "Time", "X", "Y")]
names(coord)[1] <- "Team"

# Преобразовываем время

coord[coord$Period == 1, ]$Time <- coord[coord$Period == 1, ]$Time - min(coord[coord$Period == 1, ]$Time)
coord[coord$Period == 2, ]$Time <- coord[coord$Period == 2, ]$Time - min(coord[coord$Period == 2, ]$Time)
coord[coord$Period == 3, ]$Time <- coord[coord$Period == 3, ]$Time - min(coord[coord$Period == 3, ]$Time)

# Определяем команды гостей и хозяев

del <- as.data.frame(str_locate_all(filelist$name[1], "-"))
Hometeam <- str_sub(filelist$name[1], start = del$start[3]+1, 
                           end = del$start[4]-1)
Awayteam <- str_sub(filelist$name[1], start = del$start[4]+1, 
                           end = del$start[5]-1)
Hometeam <- filter(team.roster, Team == Hometeam)$TeamID
Awayteam <- filter(team.roster, Team == Awayteam)$TeamID

# Запускаем сортировку

uniq <- distinct(coord, Team, Jersey)
uniq$Period <- uniq$Time <- uniq$X <- uniq$Y <- NULL
uniq <- uniq[order(uniq$Team, uniq$Jersey),]
i <- 1
while(i <= nrow(uniq)){
  uniq$I[i] <- i
  i <- i + 1
}

# Запускаем обработку

i <- 1
while(i <= max(uniq$I)){
  test <- filter(coord, Team == uniq$Team[i] & Jersey == uniq$Jersey[i])
  test$Player <- str_c(test$Team, test$Jersey, sep = "_") 
  test$Team <- test$Jersey <- NULL
  test <- test[order(test$Player, test$Period, test$Time),]
  
  # Схлопываем данные до 0.3 секунды
  
  test$Change[1] <- 1 # группа игровой смены
  test$flag[1] <- 0   # флан начала смены
  test$Gr[1] <- 0     # группа для группировки данных   
  
  j <- 2
  while(j <= nrow(test)){
    ifelse(round((test$Time[j+1]-test$Time[j]),1) <= 2, test$Change[j+1] <- test$Change[j], test$Change[j+1] <- test$Change[j] + 1)
    ifelse(test$Change[j+1] != test$Change[j], test$flag[j+1] <- 1, test$flag[j+1] <- 0)
    test$Gr[j] <- (j - 1)%/%3
    j <- j + 1
  }
  test$flag <- cumsum(test$flag)
  j <- 1
  while(j <= nrow(test)){
    test$GRP[j] <- test$Gr[j] + test$flag[j]
    j <- j + 1
  }

  # Схлопываем по знаку ускорения
  
  test$X <- round(test$X, 1)
  test$Y <- round(test$Y, 1)
  
  test <- test[order(test$Period, test$Time),]
  test$Duration[1] <- 0.1
  test$Distance[1] <- 0
  test$L <- 0 # флаг конца смены игрока
  
  j <- 2
  while(j <= nrow(test)){
    ifelse(test$Period[j] == test$Period[j+1], test$Duration[j] <- test$Time[j+1] - test$Time[j], test$Duration[j] <- 0)
    ifelse(test$Period[j] == test$Period[j+1], test$Distance[j] <- round(((test$X[j] - test$X[j+1])^2 + (test$Y[j] - test$Y[j+1])^2)^0.5, 4), test$Distance[j] <- 0)
    ifelse(test$Duration[j] > 0.5, test$L[j] <- 1, test$L[j] <- 0)
    ifelse(test$Duration[j] > 0.5, test$Distance[j] <- 0, test$Distance[j] <- test$Distance[j])
    ifelse(test$Duration[j] > 0.5, test$Duration[j] <- 0.1, test$Duration[j] <- test$Duration[j])
    j <- j +1
  }
    test <- test[order(test$Period, test$Time),]
    j <- 1
    while(j <= nrow(test)){
      ifelse(test$Duration[j] == 0, test$Speed[j] <- 0, test$Speed[j] <- round(test$Distance[j]/test$Duration[j], 4))
      j <- j +1
    }
  
  # Вычисляем данные по сменам
  
  pl <- test
  pl$Time_2[1] <- 0
  j <- 2
  while(j <= nrow(pl)){
    ifelse(pl$L[j-1] == 0,pl$Time_2[j] <- pl$Time_2[j-1] + 0.1,pl$Time_2[j] <- 0)
    j <- j + 1
  }
  pl$TS <- floor(pl$Time_2/5)+1
  pl$Team <- str_replace(pl$Player, "_\\d+", "")
  pl$Jersey <- as.numeric(str_extract(pl$Player, "\\d+"))
  pl3 <- sqldf("SELECT Period, Change, MIN(Time) AS ShiftStart, MAX(Time) AS ShiftEnd FROM pl GROUP BY Period, Change")
  pl2<- sqldf("SELECT Team, Jersey, TS, Change, SUM(Distance) AS SUM_Dist FROM pl GROUP BY TS, Change")
  pl2 <- pl2[order(pl2$Change, pl2$TS),]
  pl2$Duration <- pl2$TS*5
  pl2$Distance[1] <- pl2$SUM_Dist[1]
  j <- 2
  while(j <= nrow(pl2)){
    ifelse(pl2$Change[j] == pl2$Change[j-1], pl2$Distance[j] <- pl2$SUM_Dist[j] + pl2$Distance[j-1], 
           pl2$Distance[j] <- pl2$SUM_Dist[j])
    j <- j + 1
  }
  pl2$Speed <- pl2$Distance/pl2$Duration
  pl2 <- sqldf("SELECT Team, Jersey, TS, AVG(Speed) AS Speed FROM pl2 GROUP BY TS")
  temp.soc <- rbind(temp.soc, pl2)
  
  # Подготовка к группировке
  test$flag <- test$Gr <- NULL
  test <- sqldf("SELECT Player, Period, SUM(Distance) AS Distance, AVG(Speed) AS Speed, 
                SUM(Duration) AS Duration, MIN(Time) AS first_time, MAX(Time) AS last_time, 
                AVG(X) AS AVG_X, AVG(Y) AS AVG_Y FROM test GROUP BY GRP")
  test <- test[order(test$Period, test$first_time),]
  
  
  test$Acceleration[1] <- 0
  j <- 2
  while(j <= nrow(test)){
    if(j == 1) test$Speed[j-1] <- 0 
    test$Acceleration[j] <- (test$Speed[j] - test$Speed[j-1])/test$Duration[j]
    ifelse(test$L[j-1] == 1, test$Acceleration[j] <- 0, test$Acceleration[j] <- test$Acceleration[j])
    j <- j +1
  }
  test <- test[test$Distance != 0,]
  
  
  
  test <- test[order(test$Period, test$first_time),]
  test$Flag[1] <- 1
  j <- 2
  while(j <= nrow(test)){
    ifelse(is.na(test$Acceleration[j]) == TRUE, test$Acceleration[j] <- 0, test$Acceleration[j] <- test$Acceleration[j])
    if(test$Acceleration[j] * test$Acceleration[j-1] <= 0) test$Flag[j] <- test$Flag[j-1] + 1 else test$Flag[j] <- test$Flag[j-1]
    j <- j + 1
  }
  test$L <- NULL
  team.curent <- as.character(uniq$Team[i])
  j <- 1
  while(j <= nrow(test)-1){
    ifelse(team.curent == Hometeam,ifelse(test$AVG_X[j+1] <= test$AVG_X[j], test$Trend[j] <- "-", test$Trend[j] <- "+"), 
           ifelse(test$AVG_X[j+1] <= test$AVG_X[j], test$Trend[j] <- "+", test$Trend[j] <- "-"))
    j <- j + 1
  }
  ifelse(team.curent == Hometeam, test$Trend[nrow(test)] <- "-", test$Trend[nrow(test)] <- "+") 
  test_2 <- sqldf("SELECT Period, Flag, Trend, SUM(Distance) AS SUM_Distance FROM test 
                  GROUP BY Period, Flag, Trend")
  testOff <- test_2[test_2$Trend == "+",]
  testDef <- test_2[test_2$Trend == "-",]
  
  temp_2 <- sqldf("SELECT Player, Period, SUM(Distance) AS Distance, AVG(Speed) AS AVG_Speed, 
                  AVG(Acceleration) AS AVG_Acceleration, MIN(first_time) AS FIRST_Time, 
                  MAX(last_time) AS LAST_Time, SUM(Duration) AS Duration, AVG(AVG_X) AS AVG_X, 
                  AVG(AVG_Y) AS AVG_Y, Trend, flag FROM test GROUP BY Period, flag")
  temp_2 <- merge(temp_2, testOff, by = c("Period", "Flag", "Trend"), all.x = TRUE)
  names(temp_2)[13] <- "SUM_Dist_Off"
  temp_2 <- merge(temp_2, testDef, by = c("Period", "Flag", "Trend"), all.x = TRUE)
  names(temp_2)[14] <- "SUM_Dist_Def"
  temp_2$SUM_Dist_Off <- as.numeric(str_replace_na(temp_2$SUM_Dist_Off, 0))
  temp_2$SUM_Dist_Def <- as.numeric(str_replace_na(temp_2$SUM_Dist_Def, 0))
  
  
  j <- 2
  while(j <= nrow(temp_2)){
    temp_2$Distance[j] <- round(as.numeric(temp_2$Distance[j]), 4)
    temp_2$AVG_Speed[j] <- round(as.numeric(temp_2$AVG_Speed[j]), 4)
    temp_2$AVG_Acceleration[j] <- round(as.numeric(temp_2$AVG_Acceleration[j]), 4)
    j <- j + 1
  }
  temp_2 <- temp_2[-1,]
  temp_2$Distance <- as.numeric(temp_2$Distance)
  temp_2$AVG_Speed <- as.numeric(temp_2$AVG_Speed)
  temp_2$AVG_Acceleration <- as.numeric(temp_2$AVG_Acceleration)
  temp_2$FIRST_Time <- as.numeric(temp_2$FIRST_Time)
  temp_2$LAST_Time <- as.numeric(temp_2$LAST_Time)
  temp_2$Duration <- as.numeric(temp_2$Duration)
  temp_2$Trend <-NULL
  names(pl3)[1] <- "Per"
  temp_2$ShiftStart <- 0
  temp_2$ShiftEnd <- 0
  temp_2$Shift <- 0
  j <- 1
  while(j <= nrow(temp_2)){
    m <- 1
    while(m <= nrow(pl3)){
      ifelse((pl3$Per[m] == temp_2$Period[j] & pl3$ShiftEnd[m] >= temp_2$LAST_Time[j] & pl3$ShiftStart[m] <= temp_2$FIRST_Time[j]) == T | 
               (pl3$Per[m] == temp_2$Period[j] & pl3$ShiftStart[m] > temp_2$FIRST_Time[j] & pl3$ShiftStart[m] < temp_2$LAST_Time[m] & pl3$ShiftEnd[m] > temp_2$LAST_Time[j]) == T | 
               (pl3$Per[m] == temp_2$Period[j] & pl3$ShiftStart[m] < temp_2$FIRST_Time[j] & pl3$ShiftEnd[m] > temp_2$FIRST_Time[j] & temp_2$LAST_Time[j] > pl3$ShiftEnd[m]) == T, a <- 1, a <- 0) 
      ifelse(a == 1, temp_2$ShiftStart[j] <- pl3$ShiftStart[m], m <- m)
      ifelse(a == 1, temp_2$ShiftEnd[j] <- pl3$ShiftEnd[m], m <- m)
      ifelse(a == 1, temp_2$Shift[j] <- pl3$Change[m], m <- m)
      ifelse(a == 1, m <- m + 1000, m <- m + 1)
      }
    j <- j + 1
  }
  temp <- rbind(temp, temp_2)
  
  

  
  i <- i + 1
}
temp$Flag <- NULL
i <- 1
while(i <= nrow(temp)){
  temp$SecondsIntoShift[i] <- min(temp$LAST_Time[i], temp$ShiftEnd[i]) - max(temp$FIRST_Time[i], temp$ShiftStart[i])
  i <- i + 1
}

# Определяем группы для формирования графика 6, модуля 1

temp <- temp[order(temp$Period, temp$FIRST_Time),]
i <- 1
while(i <= nrow(temp)){
  j <- 30
  while(j <= 1170){
    if(temp$FIRST_Time[i] >= (j - 30) & temp$FIRST_Time[i] < (j + 30)) {temp$Q[i] <- j; j <- 2000} else 
      j <- j + 60
  }
  
  i <- i + 1
}
temp$Team <- str_replace(temp$Player, "_\\d+", "")
temp$Jersey <- str_extract(temp$Player, "\\d+")
temp$Player <- NULL

# Сохраняем данные для обработки

write.table(temp.soc, "../result_table/soc.csv", row.names = F, sep = ";")
write.table(temp, "../result_table/speed.csv", row.names = F, sep = ";")
write.table(coord.best, "coordBest.csv", row.names = F, col.names = F, sep = ",")
save(temp, file = "../temp_table/temp.RData")









