# Name crosspondence between team name
teamNameMatch <- "ATL	Atlanta Hawks
BRK	Brooklyn Nets
BOS	Boston Celtics
CHI	Chicago Bulls
CLE	Cleveland Cavaliers
DAL	Dallas Mavericks
DEN	Denver Nuggets
DET	Detroit Pistons
GSW	Golden State Warriors
HOU	Houston Rockets
IND	Indiana Pacers
LAC	Los Angeles Clippers
LAL	Los Angeles Lakers
MEM	Memphis Grizzlies
MIA	Miami Heat
MIL	Milwaukee Bucks
MIN	Minnesota Timberwolves
NYK	New York Knicks
OKC	Oklahoma City Thunder
ORL	Orlando Magic
PHI	Philadelphia 76ers
PHO	Phoenix Suns
POR	Portland Trail Blazers
SAC	Sacramento Kings
SAS	San Antonio Spurs
TOR	Toronto Raptors
UTA	Utah Jazz
WAS	Washington Wizards
CHA	Charlotte Hornets
NOP	New Orleans Pelicans
CHO	Charlotte Hornets"

# Put the crosspongdence into a data.frame 
teamShort <- sapply(strsplit(teamNameMatch, '\n'), substring, 1, 3)
teamLong <- toupper(sapply(strsplit(teamNameMatch, '\n'), substring, 5))
teamDic <- cbind(teamShort, teamLong)
colnames(teamDic) <-  c('acm', 'long')

# xxMA is goldsheet data
xxMA <- read.csv('Data/NBA 2017.csv', stringsAsFactors = FALSE)
xxMA <- xxMA[, 2:ncol(xxMA)]
xxMA$Date_of_game <- as.Date(xxMA$Date_of_game)

# Dummy for 1309 
teamNames <- unique(sort(unique(cbind(xxMA$Team1, xxMA$Team2))))
xpos <- list()
for(i in 1:length(teamNames)){
  xpos[[i]] <- ifelse(xxMA$Team1 == teamNames[i], 1, 0)
}
posmat <- matrix(unlist(unlist(xpos)), ncol = length(teamNames), byrow = FALSE)

xneg <- list()
for(i in 1:length(teamNames)){
  xneg[[i]] <- ifelse(xxMA$Team2 == teamNames[i], -1, 0)
}
negmat <- matrix(unlist(unlist(xneg)), ncol = length(teamNames), byrow = FALSE)

mm <- posmat + negmat

colnames(mm) <- teamNames

xxMA1309 <- cbind(xxMA, mm)
write.csv(xxMA1309, 'Data/variable1309.csv')

# Every game stats in 16-17 season
gameStats <- read.csv('Data/games16-17.csv', stringsAsFactors = FALSE)

gameStats$X.1 <- NULL
names(gameStats) <- gsub('X', 'HoV', names(gameStats),fix = TRUE)
names(gameStats) <- gsub('Team', 'Team1', names(gameStats),fix = TRUE)
names(gameStats) <- gsub('Oppo', 'Team2', names(gameStats),fix = TRUE)
names(gameStats) <- gsub('Tm1', 'Team1short', names(gameStats),fix = TRUE)
names(gameStats) <- gsub('Opp.1', 'Score2', names(gameStats),fix = TRUE)
names(gameStats) <- gsub('Tm', 'Score1', names(gameStats),fix = TRUE)
names(gameStats) <- gsub('Opp', 'Team2short', names(gameStats),fix = TRUE)

gameStats <- gameStats[, c(4, 42, 6, 5, 8:41)]

gameStats$HoV[gameStats$HoV != '@'] <- 1
gameStats$HoV[gameStats$HoV == '@'] <- -1

gameStats$Team1long <- NA
gameStats$Team2long <- NA
for(i in 1:nrow(teamDic)) {
  gameStats[gameStats$Team1short == teamDic[i, 1], 'Team1long'] <- teamDic[i, 2]
  gameStats[gameStats$Team2short == teamDic[i, 1], 'Team2long'] <- teamDic[i, 2]
}
gameStats$Date <- as.Date(gameStats$Date)

# xx is cleaned game data
xx <- data.frame()
for (i in 1:nrow(xxMA)){
  dt <- xxMA$Date_of_game[i]
  t1 <- xxMA$Team1[i]
  t2 <- xxMA$Team2[i]
  n <- gameStats[gameStats$Date == dt & gameStats$Team1long == t1 & gameStats$Team2long == t2, ]
  xx <- rbind(xx, n)
}
xx$HoV <- as.numeric(xx$HoV) 
nrow(xx)

write.csv(xx, 'Data/gameStats16-17.csv')

############ Construct Data for regression, using game stats ###################
# make a MA  for per team 
xxMA <- data.frame()
numGameUsed <- 3
weightMode <- 'MA' # 'MA', 'GeoMA', 'AriMA'

if (weightMode == 'MA') {
  weight <- rep(1/numGameUsed, numGameUsed)
} else if (weightMode == 'GeoMA') {
  weight <- `^`(0.6,c(1:numGameUsed)) / sum(`^`(0.6,c(1:numGameUsed)))
} else {
  weight <- c(1:numGameUsed) / sum(1:numGameUsed)
}

for (i in 1:nrow(xx)){
  dt <- xx$Date[i]
  hv <- xx$HoV[i]
  t1 <- xx$Team1long[i]
  t2 <- xx$Team2long[i]
  s1 <- xx$Score1[i]
  s2 <- xx$Score2[i]
  
  t1in1 <- xx[ xx$Date < dt & xx$Team2long == t1, c(1, 23:38)]
  t1in2 <- xx[ xx$Date < dt & xx$Team1long == t1, c(1, 7:22)]
  names(t1in1) <- c(1:17)
  names(t1in2) <- c(1:17)
  t1total <- rbind(t1in1, t1in2)
  
  t2in1 <- xx[ xx$Date < dt & xx$Team2long == t2, c(1, 23:38)]
  t2in2 <- xx[ xx$Date < dt & xx$Team1long == t2, c(1, 7:22)]
  names(t2in1) <- c(1:17)
  names(t2in2) <- c(1:17)
  t2total <- rbind(t2in1, t2in2)

  
  if (nrow(t1total) >= numGameUsed & nrow(t2total) >= numGameUsed) {
    t1total <- t1total[order(t1total$"1", decreasing = TRUE), ][1:numGameUsed, ]
    t2total <- t2total[order(t1total$"1", decreasing = TRUE), ][1:numGameUsed, ]
    t1ma <- t1total[1, 2:17] * weight[1]
    t2ma <- t2total[1, 2:17] * weight[1]
    for (ng in 2:numGameUsed) {
      t1ma <- t1ma + t1total[ng, 2:17] * weight[ng]
      t2ma <- t2ma + t2total[ng, 2:17] * weight[ng]
    }
    gameMA <- cbind(dt, t1, t2, s1, s2 ,hv ,t1ma , t2ma)
    
    xxMA <- rbind(xxMA, gameMA)
  }
  print(i)
}

names(xxMA) <- c('dt', 't1', 't2', 's1', 's2', 'hv', names(xx)[7:38])
head(xxMA)
dim(xxMA)




# write.csv(xx, 'NBA116-17full.csv')
# write.csv(xxMA, 'NBA16-17ma3.csv')


########################### DUMMY construction #################################
xxMA$t1 <- as.character(xxMA$t1)
xxMA$t2 <- as.character(xxMA$t2)

teamNames <- unique(sort(unique(cbind(xxMA$t1, xxMA$t2))))
xpos <- list()
for(i in 1:length(teamNames)){
  xpos[[i]] <- ifelse(xxMA$t1 == teamNames[i], 1, 0)
}
posmat <- matrix(unlist(unlist(xpos)), ncol = length(teamNames), byrow = FALSE)

xneg <- list()
for(i in 1:length(teamNames)){
  xneg[[i]] <- ifelse(xxMA$t2 == teamNames[i], -1, 0)
}
negmat <- matrix(unlist(unlist(xneg)), ncol = length(teamNames), byrow = FALSE)

mm <- posmat + negmat

colnames(mm) <- teamNames
############################ Player Data #####################################


pinfo <- read.csv('Data/PlayerStatsPerGame.csv', stringsAsFactors = FALSE)

pinfo <- pinfo[, c(32, 4, 6, 7, 11:31)]

# Filtering out players stats who did not played a specific game
notplaystring <- c("Did Not Dress", "Not With Team", "Inactive", 
                   "Player Suspended", "Did Not Play")
pinfonew <- data.frame()
pinfonull <- data.frame()
for (i in 1:nrow(pinfo)) {
  if (!(pinfo[i, 6] %in% notplaystring)) {
    pinfonew <- rbind(pinfonew, pinfo[i, ])
  } else {
    pinfonull <- rbind(pinfonull, pinfo[i, ])
  }
  print(i)
}
head(pinfonew)
dim(pinfonew)

head(pinfonull)
dim(pinfonull)



write.csv(pinfonew, 'pinfonew.csv')

########################### INJURY DATA ##############################
injury <- read.csv('Data/injurylist.csv', stringsAsFactors = FALSE)
injury <- injury[, c(2, 3, 4, 5) ]
head(injury)

injury$GoneDate <- as.Date(injury$GoneDate)
injury$BackDate <- as.Date(injury$BackDate)

injury$Team <- toupper(injury$Team)
unique(sort(xxMA$t1))
unique(sort(injury$Team))

xxMA$t1short <- sapply(strsplit(xxMA$t1, ' '), tail, 1)
xxMA$t2short <- sapply(strsplit(xxMA$t2, ' '), tail, 1)

sort(unique(xxMA$t1short))
sort(unique(xxMA$t2short))
sort(unique(injury$Team))
# They match

###################### Working with injury ####################################
########################### EDA for injury ###################################

# # Find out the num of player got injuried and could not play the game for both
# # teams
# x1$t1miss <- 0
# x1$t2miss <- 0
# for(i in 1:nrow(x1)){
#   Team1MissPlayer <- injury[injury$Team == x1[i, 't1short'] &
#                             injury$GoneDate < x1[i, 'dt'] &
#                             injury$BackDate > x1[i, 'dt'], ]
#   Team2MissPlayer <- injury[injury$Team == x1[i, 't2short'] &
#                             injury$GoneDate < x1[i, 'dt'] &
#                             injury$BackDate > x1[i, 'dt'], ]
# 
#   x1[i, 't1miss'] <- nrow(Team1MissPlayer)
#   x1[i, 't2miss'] <- nrow(Team2MissPlayer)
# }
# 
# head(x1)
# 
# x1$playerdiff <- x1$t2miss -x1$t1miss
# 
# # Plot the score difference vs the difference in the number of missing player 
# # for both team
# plot(jitter(x1$s1 - x1$s2) ~ jitter(x1$playerdiff))
# 
# # We need to put team ability difference here
# # Does not seem to have a meaning just by the number


# change pinfonew to pin
pin <- pinfonew

# Round the player on court time to the min
pin$MP <- as.numeric(sapply(strsplit(pin$MP, ':'), head, 1)) + 1



# Put the long team name in the pin
for (i in 1: nrow(teamDic)) {
  pin[pin$Tm == teamDic[i, 1], 'Tmlong'] <- teamDic[i, 2]
}

# Fill na with 0 for pin, as the na caused by '', 
# which need to be 0 to be processed
pin[is.na(pin)] <- 0
injuryold <- injury

# Drop na rows for injury data set
injury <- injury[complete.cases(injury), ]

# Used for model4 explanation
write.csv(injury, 'Data/injuryExample.csv')
write.csv(pin, 'Data/pinfoExample.csv')
write.csv(xxMA, 'Data/xxMAExample.csv')

# 
t1pmp <- list()
t2pmp <- list()
# 
t1tp <- list()
t2tp <- list()
# 
t1aa <- list()
t2aa <- list()
# 
t1tpsq <- list()
t2tpsq <- list()
for (i in 1:nrow(xxMA)) {
  print(i)
  dt <- xxMA[i, 'dt']
  t1short <- xxMA[i, 't1short']
  t2short <- xxMA[i, 't2short']
  # Find out missing players in both team in a game
  Team1miss <- injury[injury$Team == t1short & injury$GoneDate <= dt & 
                        injury$BackDate > dt, ]
  Team2miss <- injury[injury$Team == t2short & injury$GoneDate <= dt & 
                        injury$BackDate > dt, ]
  
  # Find The score penalty for team 1 due to missing player
  t1totalmissDif <- 0
  # Avrage Minte Played
  t1totalmissMP <- 0
  # Better than average
  t1totalmissAa <- 0
  # Avrage Minute Player sqared
  t1totalmissMPsq<- 0
  if (nrow(Team1miss) > 0) {
    # Find the score penalty for team 1 missing that specific player 
    for (p in 1:nrow(Team1miss)) {
      spstats <- pin[pin$player == Team1miss[p, 'Player'] & pin$Date < dt, ]
      if (nrow(spstats) > numGameUsed) {
        for (i in 6:25) {
          spstats[, i] <- as.numeric(spstats[, i])
        }
        spstemp <- spstats[order(spstats$Date, decreasing = TRUE),
                           ][1:numGameUsed, c(6:25, 5,26, 2)]
        spstemp$ppm <- spstemp$PTS / spstemp$MP
        spstemp[is.na(spstemp)] <- 0
        # we use numGameUsed# games to take avg to calculate the player 
        # efficency aganst avg player
        for(m in 1:numGameUsed){
          teamscore <- xxMA[xxMA$t1 == spstemp$Tmlong[m] & 
                              xxMA$dt == spstemp$Date[m],]
          if (nrow(teamscore) == 0) {
            teamscore <- xxMA[xxMA$t2 == spstemp$Tmlong[m] & 
                                xxMA$dt == spstemp$Date[m],]
            ts <- teamscore$s2
          } else {
            ts <- teamscore$s1
          }
          if (length(ts) > 0){
            spstemp[m, 'avgEff'] <- (ts - spstemp[m, "PTS"]) / (
              240 - spstemp[m, "MP"])
            spstemp[m, 'aa'] <- ifelse((spstemp[m, 'ppm'] > 
                                          spstemp[m, 'avgEff']) ,1 , 0)
          } else {
            spstemp[m, 'avgEff'] <- 0 
            spstemp[m, 'aa'] <- 0
          }
          spstemp[is.na(spstemp)] <- 0
        }
        avgMP <- mean(spstemp$MP)
        avgppm <- mean(spstemp$ppm)
        avgavgEff <- mean(spstemp$avgEff)
        avgaa <- mean(spstemp$aa)
        if (avgavgEff < avgppm) {
          playerMissDif <- avgMP * (avgppm - avgavgEff)
        } else {
          playerMissDif <- 0
        }
        t1totalmissMP <- t1totalmissMP + avgMP
        t1totalmissDif <- t1totalmissDif + playerMissDif
        t2totalmissAa <- t2totalmissAa + avgaa
        t1totalmissMPsq <- t1totalmissMPsq + (avgMP ^2)
      }
    }
  }
  
  # Find The score penalise for team 2 due to missing player
  t2totalmissDif <- 0
  t2totalmissMP <- 0
  t2totalmissAa <- 0
  t2totalmissMPsq<- 0
  if (nrow(Team2miss) > 0) {
    for (p in 1:nrow(Team2miss)) {
      spstats <- pin[pin$player == Team2miss[p, 'Player'] & pin$Date < dt, ]
      if (nrow(spstats) > numGameUsed) {
        for (i in 6:25) {
          spstats[, i] <- as.numeric(spstats[, i])
        }
        spstemp <- spstats[order(spstats$Date, decreasing = TRUE), ][
          1:numGameUsed, c(6:25, 5,26, 2)]
        spstemp$ppm <- spstemp$PTS / spstemp$MP
        spstemp[is.na(spstemp)] <- 0

        for(m in 1:numGameUsed){
          teamscore <- xxMA[xxMA$t1 == spstemp$Tmlong[m] & 
                              xxMA$dt == spstemp$Date[m],]
          if (nrow(teamscore) == 0) {
            teamscore <- xxMA[xxMA$t2 == spstemp$Tmlong[m] & 
                                xxMA$dt == spstemp$Date[m],]
            ts <- teamscore$s2
          } else {
            ts <- teamscore$s1
          }
          if (length(ts) > 0){
            # My complicated score system, not usefull
            spstemp[m, 'avgEff'] <- (ts - spstemp[m, "PTS"]) / 
              (240 - spstemp[m, "MP"])
            # This player better than avg player?
            spstemp[m, 'aa'] <- ifelse((spstemp[m, 'ppm'] > 
                                          spstemp[m, 'avgEff']) ,1 , 0)
          } else {
            spstemp[m, 'avgEff'] <- 0 
            spstemp[m, 'aa'] <- 0
          }
          
          spstemp[is.na(spstemp)] <- 0
        }
        avgMP <- mean(spstemp$MP)
        avgppm <- mean(spstemp$ppm)
        avgavgEff <- mean(spstemp$avgEff)
        avgaa <- mean(spstemp$aa)
        if (avgavgEff < avgppm) {
          playerMissDif <- avgMP * (avgppm - avgavgEff)
        } else {
          playerMissDif <- 0
        }
        t2totalmissMP <- t2totalmissMP + avgMP
        t2totalmissDif <- t2totalmissDif + playerMissDif
        t2totalmissAa <- t2totalmissAa + avgaa
        t2totalmissMPsq <- t2totalmissMPsq + (avgMP ^2)
      }
    }
  }
  t1pmp <- append(t1pmp, t1totalmissDif)
  t2pmp <- append(t2pmp, t2totalmissDif)
  t1tp <- append(t1tp, t1totalmissMP)
  t2tp <- append(t2tp, t2totalmissMP)
  t1aa <- append(t1aa, t1totalmissAa)
  t2aa <- append(t2aa, t2totalmissAa)
  t1tpsq <- append(t1tpsq, t1totalmissMPsq)
  t2tpsq <- append(t2tpsq, t2totalmissMPsq)  
}
xxMA$t1pmp <- as.numeric(unlist(t1pmp))
xxMA$t2pmp <- as.numeric(unlist(t2pmp))
xxMA$t1tp <- as.numeric(unlist(t1tp))
xxMA$t2tp <- as.numeric(unlist(t2tp))
xxMA$t1aa <- as.numeric(unlist(t1aa))
xxMA$t2aa <- as.numeric(unlist(t2aa))

sdif <- xxMA$s1 - xxMA$s2
pmpdif <- xxMA$t2pmp - xxMA$t1pmp
ptdif <- xxMA$t2tp - xxMA$t1tp 
aadif <- xxMA$t2aa - xxMA$t1aa
ptdifsq <- as.numeric(unlist(t1tpsq)) - as.numeric(unlist(t2tpsq))
statMM <- matrix(unlist(unlist((xxMA[, 7:22] - xxMA[, 23:38]))), ncol = 16, byrow = FALSE)
colnames(statMM) <- c('FGdif', 'FGAdif', 'FGPdif', '3Pdif', '3PAdif', '3PPdif', 
                'FTdif', 'FTAdif', 'FTPdif', 'ORBdif', 'RBdif', 'ASTdif', 
                'STLdif', 'BLKdif', 'TOVdif', 'PFdif')

########################### DUMMY construction for 1260 ########################
xxMA$t1 <- as.character(xxMA$t1)
xxMA$t2 <- as.character(xxMA$t2)

teamNames <- unique(sort(unique(cbind(xxMA$t1, xxMA$t2))))
xpos <- list()
for(i in 1:length(teamNames)){
  xpos[[i]] <- ifelse(xxMA$t1 == teamNames[i], 1, 0)
}
posmat <- matrix(unlist(unlist(xpos)), ncol = length(teamNames), byrow = FALSE)

xneg <- list()
for(i in 1:length(teamNames)){
  xneg[[i]] <- ifelse(xxMA$t2 == teamNames[i], -1, 0)
}
negmat <- matrix(unlist(unlist(xneg)), ncol = length(teamNames), byrow = FALSE)

mm <- posmat + negmat

colnames(mm) <- teamNames

head(mm)

#############################
xxMA1260 <- cbind(xxMA, mm)

write.csv(xxMA1260, 'Data/allVariable.csv')




model2<- lm(sdif ~ -1 + xxMA$hv + mm[, 1:29])
summary(model2)

model3 <- lm(sdif ~ -1 + xxMA$hv +statMM + mm[, 1:29])
summary(model3)

# pmp for player efficency (complicated)
model4 <- lm(sdif ~ -1 + xxMA$hv  + mm[, 1:29] + pmpdif)
summary(model4)

# pt for player time
model5 <- lm(sdif ~ -1 + xxMA$hv  + mm[, 1:29] + ptdif)
summary(model5)
summary(ptdif)
xxMA[which(ptdif > 140), ]

head(injury)
injury[injury$Team == 'HEAT' & injury$GoneDate < as.Date('2016-12-07') &
         injury$BackDate > as.Date('2016-12-07'),]

# aa for above avg
model6 <- lm(sdif ~ -1 + xxMA$hv  + mm[, 1:29] + aadif)
summary(model6)





# if player is 42 min in mean, this player is important 
# Value of player is calculted by the av num min playerd
# simple may be corret, and both attack and defense
#  min played may not have that many zero
# mine rn has too many 0 to be helpful
# look into pmpdif for extreme value, also for the min played as well


# Just some example se the difference between a star player got injuried, vs, a 
# avg player got injuried
# Star Stephen Curry


starName <- "Kawhi Leonard"
starInjury <-  injury[injury$Player == starName , ]
starInjury[9, ]
starInfo <- pin[pin$player == starName , ]
starTeam <- injury[injury$Player == starName , ][1, 1]
starGame <- xxMA[xxMA$t1short == starTeam | xxMA$t2short == starTeam, ]
starGame <- starGame[order(starGame$dt, decreasing = FALSE),]
tail(starGame[, 1:6], 10)
# Matters a lot, SPURS performance are worse after kawhi gone

avgName <- 'Zaza Pachulia'
avgInjury <-  injury[injury$Player == avgName , ]
avgInjury[3, ]
avgInfo <- pin[pin$player == avgName , ]
avgTeam <- injury[injury$Player == avgName , ][1, 1]
avgGame <- xxMA[xxMA$t1short == avgTeam | xxMA$t2short == avgTeam, ]
avgGame <- avgGame[order(avgGame$dt, decreasing = FALSE),]
tail(avgGame[, 1:6], 10)
# Does not matter, WARRIEORS performance still good?



############### TRY PREDICTION ###############
xxMAdt <- cbind(xxMA, mm)
xxMAdt <- xxMAdt[order(xxMAdt$dt, decreasing = FALSE), ]
rgames <- xxMAdt[xxMAdt$dt < as.Date('2017-04-15'), ]
rnum <- nrow(rgames)
pgames <- xxMAdt[xxMAdt$dt >= as.Date('2017-04-15'), ]
pnum <- nrow(pgames)
preddif <- list()
for (i in 1:pnum) {
  xxMAdttp <- xxMAdt[1:(rnum - 1 + i),]
  gameStats <- xxMAdttp[, 23:38] - xxMAdttp[, 7:22]
  teamDummy <- as.matrix(xxMAdttp[, 47:76])[, 1:29]
  pmpdif <- xxMAdttp[, 42] - xxMAdttp[, 41]
  ptdif <- xxMAdttp[, 44] - xxMAdttp[, 43]
  aadif <- xxMAdttp[, 46] - xxMAdttp[, 45]
  sd <- xxMAdttp[, 4] - xxMAdttp[, 5]
  hv <- xxMAdttp[, 6]
  
  xxMAdtng <- xxMAdt[(rnum + i),]
  nggameStats <- xxMAdtng[, 23:38] - xxMAdtng[, 7:22]
  ngteamDummy <- as.matrix(xxMAdtng[, 47:76])[, 1:29]
  ngpmpdif <- xxMAdtng[, 42] - xxMAdtng[, 41]
  ngptdif <- xxMAdtng[, 44] - xxMAdtng[, 43]
  ngaadif <- xxMAdtng[, 46] - xxMAdtng[, 45]
  ngsd <- xxMAdtng[, 4] - xxMAdtng[, 5]
  nghv <- xxMAdtng[, 6]
  
  cbind(hv, teamDummy, ptdif)
  # Train a model after every game played, use the up to the latest last game
  m1 <- lm(sd ~ -1 + cbind(hv, teamDummy, ptdif))
  # m1 <- lm(sd ~ -1 + hv + teamDummy + ptdif)
  summary(m1)
  # predict(m1, c(nghv, ngteamDummy, ngptdif))
  psd <- m1$coefficients %*% c(nghv, ngteamDummy, ngptdif)
  preddif <- append(preddif, psd)
}
preddif <- unlist(preddif)

realdif <- pgames$s1 -pgames$s2

plot(realdif ~ preddif)
# look carefully for the 2 data point below

