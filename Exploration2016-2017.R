x <- read.csv("Data/NBA 2016-2017(1289).csv", stringsAsFactors = FALSE)
a <- read.csv("Data/3point2016-2017.csv", stringsAsFactors = FALSE)
b <- read.csv("Data/Rebound2016-2017.csv", stringsAsFactors = FALSE)
d <- read.csv("Data/FreeThrow2016-2017.csv", stringsAsFactors = FALSE)
e <- read.csv("Data/Assist2016-2017.csv", stringsAsFactors = FALSE)
f <- read.csv("Data/Block2016-2017.csv", stringsAsFactors = FALSE)
g <- read.csv("Data/Turnover2016-2017.csv", stringsAsFactors = FALSE)

y <- read.csv("Data/cleaning_games2016-2017.csv", stringsAsFactors = FALSE)

################################################################################
# 只导入这两个数据
x <- read.csv("Data/NBA 2016-2017.csv", stringsAsFactors = FALSE)
m <- read.csv("Data/NBA 2016-2017(2618).csv", stringsAsFactors = FALSE)


######################### Data Analysis ########################################

# ##### EDA for "home-field" advantage
# plot(density(x$scorediff[x$Location == "H"], adjust = 2), 
#      main="Density of scorediff", lwd = 3, col = 'blue')
# lines(density(x$scorediff[x$Location == 'V'], adjust = 2), lwd = 3, col = 'red')
# # lines(density(x$scorediff[x$Location == 'N']),lwd = 2, col = 'yellow')
# legend("topright", col = c("blue", "red", "blue", "red"), lty = c(1, 1, 2, 2),
#        lwd = c(3,3,1,1), legend = c("Home", "Visit", "Home scorediff mean", 
#                                     "Visit scorediff mean"), cex = 0.4)
# abline(v = 0)
# m <- tapply(x$scorediff, x$Location, mean)
# abline(v = c(m[1], m[3]), col = c('blue', 'red'), lwd = 1, lty = 2)
# We got 1306 Home matches and 1306 Visit matches, the plot shows difference 
# but since the data is duplicated internally, maybe we should research 
# these types individually to get more information.

# boxplot(x$scorediff ~ x$Location)
# tapply(m$scorediff, m$Location, mean)
tapply(m$scorediff, m$Team1, mean)

# We can see that there is small difference 3.07 between home-games and 
# visit-games. 


################################################################################
# 图
len_team <- length(unique(m$Team1))
hscorediff <- rep(NA, len_team)
vscorediff <- rep(NA, len_team)
for (i in 1:len_team){
  thisteam <- unique(m$Team1)[i]
  hscorediff[i] <- mean(m$scorediff[m$Team1 == unique(m$Team1)[i] &
                                      m$Location == "H"])
  vscorediff[i] <- mean(m$scorediff[m$Team1 == unique(m$Team1)[i] &
                                      m$Location == "V"])
}
plot(hscorediff-vscorediff)
plot(density(hscorediff - vscorediff, adj = 2), 
main="Differences of scorediff between home and visit")
abline(v = 0, col = 'red')

################################################################################

# boxplot(hscorediff, vscorediff, names = c("hscorediff", "vscorediff"))
# mean(hscorediff)    # 2.84
# mean(vscorediff)    # -3.30
# # We want to try to visualize the different performances as "H" or "V". 
# 
# for (i in 1:len_team){
#   thisteam <- unique(z$Team1)[i]
#   hscorediff[i] <- mean(z$scorediff[z$Team1 == unique(z$Team1)[i] &
#                                       z$Location == "H"])
#   vscorediff[i] <- mean(z$scorediff[z$Team1 == unique(z$Team1)[i] &
#                                       z$Location == "V"])
# }
# 
# 
# 
# 
# 
# 
# unique(x$Team1)
# thisteam <- "WASHINGTON WIZARDS"
# tapply(x$scorediff[x$Team1 == thisteam], x$Location[x$Team1 == thisteam], mean)
# # Here, we tried several teams separately, and we can discover that generally
# # speaking, teams at home tend to win higher scores. It seems that "home-field
# # advantage" does exist.
# 
# a <- x$scorediff[x$Team1 == thisteam & x$Location == "H"]
# b <- x$scorediff[x$Team1 == thisteam & x$Location == "V"]
# t.test(a, b)
# 
# 
# ans <- rep(NA, len_team)
# for (i in 1:len_team){
#   cat("Studying", unique(x$Team1)[i], "\n")
#   a <- x$scorediff[x$Team1 == unique(x$Team1)[i] & x$Location == "H"]
#   b <- x$scorediff[x$Team1 == unique(x$Team1)[i] & x$Location == "V"]
#   d <- t.test(a, b, var.equal = TRUE)
#   ans[i] <- d$p.value
#   cat("p-value", d$p.value, "\n")
#   cat("\n")
# }
# ans
# # It seems that some teams have "strong" home-field advantage, while some not.
# 
# y <- y[y$Date >= "2016-10-25" & y$Date <= "2017-05-07", ]

par(mfrow = c(2,3))

##### EDA for 3 points shoots
plot(jitter(x$scorediff) ~ jitter(y$TPdiff))
ThreePoint <- lm(x$scorediff ~ y$TPdiff)
summary(ThreePoint)
abline(-0.23788, 1.45652, col = 2, lwd = 3)
abline(0, 1)

##### EDA for Rebounds
b1 <- plot(jitter(x$scorediff) ~ jitter(y$TRBdiff))
Rebounds <- lm(x$scorediff ~ y$TRBdiff)
summary(Rebounds)
b1 <- abline(0.80243, 0.76137, col = 2, lwd = 3)
abline(0, 1)


##### EDA for Free Throws
plot(jitter(x$scorediff) ~ jitter(y$FTdiff))
FreeThrow <- lm(x$scorediff ~ y$FTdiff)
summary(FreeThrow)
abline(0.17894, 0.33953, col = 2, lwd = 3)
abline(0, 1)


##### EDA for Assists
plot(jitter(x$scorediff) ~ jitter(y$ASTdiff))
Assists <- lm(x$scorediff ~ y$ASTdiff)
summary(Assists)
abline(0.27725, 1.25857, col = 2, lwd = 3)
abline(0, 1)


##### EDA for Blocks
plot(jitter(x$scorediff) ~ jitter(y$BLKdiff))
Blocks <- lm(x$scorediff ~ y$BLKdiff)
summary(Blocks)
abline(0.6661, 1.1485, col = 2, lwd = 3)
abline(0, 1)


##### EDA for Turnovers
plot(jitter(x$scorediff) ~ jitter(y$TOVdiff))
Turnovers <- lm(x$scorediff ~ y$TOVdiff)
summary(Turnovers)
abline(0.44562, -0.57627, col = 2, lwd = 3)
abline(0, -1)


par()


################################################################################
# 
# 
# ?matrixplot
# 
# 
# 
# 
# 
# 
# # Dummy for teams 
# 
# # Function for dummy
# 
# teamNames <- sort(unique(append(x$Team1, x$Team2)))[1:29]
#   
# xpos <- list()
# for(i in 1:length(teamNames)){
#   xpos[[i]] <- ifelse(x$Team1 == teamNames[i], 1, 0)
# }
# posmat <- matrix(unlist(xpos), ncol = length(teamNames), byrow = FALSE)
# 
# xneg <- list()
# for(i in 1:length(teamNames)){
#   xneg[[i]] <- ifelse(x$Team2 == teamNames[i], -1, 0)
# }
# negmat <- matrix(unlist(xneg), ncol = length(teamNames), byrow = FALSE)
# mm <- posmat + negmat
# mm
# 
# 
# 
# 
# # mm needs to have names: use colnames(mm) <- ….  so the column names 
# # describe the team.
# x$HorV <- 0
# x$HorV[x$Location == 'H'] <- 1
# x$HorV[x$Location == 'V'] <- -1
#   
# colnames(mm) <- teamNames
# 
# 
# ThreePoint <- a[ ,2]
# Rebounds <- b[ ,2]
# FreeThrow <- d[ ,2]
# Assist <- e[ ,2]
# Block <- f[ ,2]
# Turnover <- g[ ,2]
# 
# 
# 
# x <- cbind(x, ThreePoint, FreeThrow, Rebounds, Assist, Block, Turnover)
# head(x)
# 
# ##### Building the model
# ##### variables: home-field advantage, 30 teams, 3 Points, Free Throws, 
# ##### Rebounds, Assist, Blocks, Turnover.
# Pointspread1 <- lm(scorediff ~ x$HorV + mm + ThreePoint + FreeThrow + Rebounds 
#                    + Assist + Block + Turnover - 1, data = x)
# summary(Pointspread1)
# 
# 
# ##### Only numerical variables
# Pointspread2 <- lm(scorediff ~ ThreePoint + FreeThrow + Rebounds 
#                    + Assist + Block + Turnover - 1, data = x)
# summary(Pointspread2)
# 
# 
# ##### Our original model with only home-field advantage and 30 teams. 
# Pointspread_org <- lm(x$scorediff ~ -1 + mm + x$HorV)
# summary(Pointspread_org)
# anova(Pointspread_org)
# 
# 
# Pointspread <- lm(x$scorediff ~ -1 + mm)
# summary(Pointspread)
# anova(Pointspread)
# 
# 
# ################################################################################
# ##### Predictive model (with only 1289 games)
# 
# x1 <- read.csv("NBA 2016-2017(1289).csv", stringsAsFactors = FALSE)
# a1 <- read.csv("3point2016-2017(1289).csv", stringsAsFactors = FALSE)
# b1 <- read.csv("Rebound2016-2017(1289).csv", stringsAsFactors = FALSE)
# d1 <- read.csv("FreeThrow2016-2017(1289).csv", stringsAsFactors = FALSE)
# e1 <- read.csv("Assist2016-2017(1289).csv", stringsAsFactors = FALSE)
# f1 <- read.csv("Block2016-2017(1289).csv", stringsAsFactors = FALSE)
# g1 <- read.csv("Turnover2016-2017(1289).csv", stringsAsFactors = FALSE)
# 
# 
# 
# 
# 
# teamNames <- sort(unique(append(x1$Team1, x1$Team2)))[1:29]
# 
# xpos <- list()
# for(i in 1:length(teamNames)){
#   xpos[[i]] <- ifelse(x1$Team1 == teamNames[i], 1, 0)
# }
# posmat <- matrix(unlist(xpos), ncol = length(teamNames), byrow = FALSE)
# 
# xneg <- list()
# for(i in 1:length(teamNames)){
#   xneg[[i]] <- ifelse(x1$Team2 == teamNames[i], -1, 0)
# }
# negmat <- matrix(unlist(xneg), ncol = length(teamNames), byrow = FALSE)
# mm <- posmat + negmat
# mm
# dim(mm)
# 
# 
# 
# # mm needs to have names: use colnames(mm) <- ….  so the column names 
# # describe the team.
# x1$HorV <- 0
# x1$HorV[x1$Location == 'H'] <- 1
# x1$HorV[x1$Location == 'V'] <- -1
# 
# colnames(mm) <- teamNames
# 
# 
# ThreePoint <- a1[ ,2]
# Rebounds <- b1[ ,2]
# FreeThrow <- d1[ ,2]
# Assist <- e1[ ,2]
# Block <- f1[ ,2]
# Turnover <- g1[ ,2]
# 
# 
# x1 <- cbind(x1, ThreePoint, FreeThrow, Rebounds, Assist, Block, Turnover)
# head(x1)
# 
# ##### Building the model
# ##### variables: home-field advantage, 30 teams, 3 Points, Free Throws, 
# ##### Rebounds, Assist, Blocks, Turnover.
# Pointspread1 <- lm(scorediff ~ x1$HorV + mm + ThreePoint + FreeThrow + Rebounds 
#                    + Assist + Block + Turnover - 1, data = x1)
# summary(Pointspread1)
# anova(Pointspread1)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ################################################################################
# 
# 
# Pointspread <- lm(scorediff ~ x$HorV + mm + ThreePoint + FreeThrow + Rebounds 
#                   + Steals + Asist + Block + FieldGoal + Turnover - 1, data = x)
# summary(Pointspread)
# library(car)
# vif(Pointspread)
# Pointspread1 <- lm(scorediff ~ x$HorV + mm + ThreePoint + FreeThrow + Rebounds 
#                    + Asist + Block + Turnover - 1, data = x1)
# summary(Pointspread1)
# x2 <- x1[, -c(1:7,9)]
# library("corrplot")
# corrplot.mixed(x2, lower.col = "black", number.cex = .7, family = "STSong")
# 
# cor(x2)
# 
# sqrt(vif(Pointspread)) > 2
# summary(Pointspread1)
# anova(Pointspread)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot(x$scorediff ~ predict(Pointspread_org))
# abline(0, 1)
# abline(lm(x$scorediff ~ predict(Pointspread_org)), col = "red")
# 
# hist(Pointspread_org$residuals)
# 
# 
# 
# 
# plot(-x$Pointspread, predict(Pointspread_org))
# abline(0, 1)
# 
# 
# 
# plot(Pointspread)
# 
# 
# 
# 
# 
# 
# # NOTE:
# 
# ##### We discover that the statistic of home-field advantage has decreased from
# ##### 3.05 to 1.89. Our assumption is that the home-field advantage has been 
# ##### explained by the performance of players, for example, 3 Points, 
# ##### Free Throws, Rebounds and Steals.
# 
# 
# 
# 
# 
# 
# # lm(x$scorediff ~ mm)
# # c <- lm(scorediff ~ mm, data = x)
# # summary(c)
# # n <- c$coefficients
# # names(n) <- c("Home Advantage", teamNames)
# 
# 
# 
# # lm(xx$scorediff ~ cbind(hassign, sudodummy))
# 
# 
# 
# 
# 
# 
# 
