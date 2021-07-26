x <- read.csv("games16-17.csv", stringsAsFactors = FALSE)
y <- read.csv("NBA 2016-2017.csv", stringsAsFactors = FALSE)
x[ ,6] <- gsub("CHO", "CHA", x[ ,6])
x$Date <- format(as.Date(x$Date, "%Y/%m/%d"), "%Y-%m-%d")


unique(x$Tm1)
unique(x$Opp)
name1 <- sort(unique(append(y$Team1, y$Team2)))
name2 <- sort(unique(x$Opp))
data.frame(name1, name2)
# By visually checking they match!
for( i in 1:length(name2)) {
  x[x$Tm1 == name2[i], "Tm1"] <- name1[i]
  x[x$Opp == name2[i], "Opp"] <- name1[i]
}


xx <- data.frame()
for (i in 1:1309){
  n <- x[x$Date == y$Date_of_game[i] & x$Tm1 == y$Team1[i] & 
           x$Opp == y$Team2[i], ]
  xx <- rbind(xx, n)
}
nrow(xx)


write.csv(xx, "cleaning_games2016-2017.csv")


a <- xx[ , "Team3P"] - xx[ ,"Oppo3P"]
write.csv(a, "3point2016-2017.csv")

b <- xx[ ,"TeamTRB"] - xx[ ,"OppoTRB"]
write.csv(b, "Rebound2016-2017.csv")

d <- xx[ , "TeamFT"] - xx[ ,"OppoFT"]
write.csv(d, "FreeThrow2016-2017.csv")

e <- xx[ ,"TeamAST"] - xx[ ,"OppoAST"]
write.csv(e, "Assist2016-2017.csv")

f <- xx[ ,"TeamBLK"] - xx[ ,"OppoBLK"]
write.csv(f, "Block2016-2017.csv")

g <- xx[ ,"TeamTOV"] - xx[ ,"OppoTOV"]
write.csv(g, "Turnover2016-2017.csv")

################################################################################


xx1 <- xx[xx$Date >= "2016-10-25" & xx$Date <= "2017-05-07", ]
nrow(xx1)

a <- xx1[ , "Team3P"] - xx1[ ,"Oppo3P"]
write.csv(a, "3point2016-2017(1289).csv")

b <- xx1[ ,"TeamTRB"] - xx1[ ,"OppoTRB"]
write.csv(b, "Rebound2016-2017(1289).csv")

d <- xx1[ , "TeamFT"] - xx1[ ,"OppoFT"]
write.csv(d, "FreeThrow2016-2017(1289).csv")

e <- xx1[ ,"TeamAST"] - xx1[ ,"OppoAST"]
write.csv(e, "Assist2016-2017(1289).csv")

f <- xx1[ ,"TeamBLK"] - xx1[ ,"OppoBLK"]
write.csv(f, "Block2016-2017(1289).csv")

g <- xx1[ ,"TeamTOV"] - xx1[ ,"OppoTOV"]
write.csv(g, "Turnover2016-2017(1289).csv")








