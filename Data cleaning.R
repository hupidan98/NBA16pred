x1 <- read.csv("2013ATL.csv", stringsAsFactors = FALSE)
x2 <- read.csv("2013BOS.csv", stringsAsFactors = FALSE)
x3 <- read.csv("2013BRK.csv", stringsAsFactors = FALSE)
x4 <- read.csv("2013CHA.csv", stringsAsFactors = FALSE)
x5 <- read.csv("2013CHI.csv", stringsAsFactors = FALSE)
x6 <- read.csv("2013CLE.csv", stringsAsFactors = FALSE)
x7 <- read.csv("2013DAL.csv", stringsAsFactors = FALSE)
x8 <- read.csv("2013DEN.csv", stringsAsFactors = FALSE)
x9 <- read.csv("2013DET.csv", stringsAsFactors = FALSE)
x10 <- read.csv("2013GSW.csv", stringsAsFactors = FALSE)
x11 <- read.csv("2013HOU.csv", stringsAsFactors = FALSE)
x12 <- read.csv("2013IND.csv", stringsAsFactors = FALSE)
x13 <- read.csv("2013LAC.csv", stringsAsFactors = FALSE)
x14 <- read.csv("2013LAL.csv", stringsAsFactors = FALSE)
x15 <- read.csv("2013MEM.csv", stringsAsFactors = FALSE)
x16 <- read.csv("2013MIA.csv", stringsAsFactors = FALSE)
x17 <- read.csv("2013MIL.csv", stringsAsFactors = FALSE)
x18 <- read.csv("2013MIN.csv", stringsAsFactors = FALSE)
x19 <- read.csv("2013NOP.csv", stringsAsFactors = FALSE)
x20 <- read.csv("2013NYK.csv", stringsAsFactors = FALSE)
x21 <- read.csv("2013OKC.csv", stringsAsFactors = FALSE)
x22 <- read.csv("2013ORL.csv", stringsAsFactors = FALSE)
x23 <- read.csv("2013PHI.csv", stringsAsFactors = FALSE)
x24 <- read.csv("2013PHO.csv", stringsAsFactors = FALSE)
x25 <- read.csv("2013POR.csv", stringsAsFactors = FALSE)
x26 <- read.csv("2013SAC.csv", stringsAsFactors = FALSE)
x27 <- read.csv("2013SAS.csv", stringsAsFactors = FALSE)
x28 <- read.csv("2013TOR.csv", stringsAsFactors = FALSE)
x29 <- read.csv("2013UTA.csv", stringsAsFactors = FALSE)
x30 <- read.csv("2013WAS.csv", stringsAsFactors = FALSE)


i <- 1
m <- read.csv(dir()[i], stringsAsFactors = FALSE)
n <- substring(dir()[i], 5, 7)
m$Tm1 <- rep(n, nrow(m))
y2012 <- m

for (i in 2:30){
  m2 <- read.csv(dir()[i], stringsAsFactors = FALSE)
  n <- substring(dir()[i], 5, 7)
  m2$Tm1 <- rep(n, nrow(m2))
  y2012 <- rbind(y2012, m2)
}


i <- 31
m <- read.csv(dir()[i], stringsAsFactors = FALSE)
n <- substring(dir()[i], 5, 7)
m$Tm1 <- rep(n, nrow(m))
y2013 <- m

for (i in 32:60){
  m2 <- read.csv(dir()[i], stringsAsFactors = FALSE)
  n <- substring(dir()[i], 5, 7)
  m2$Tm1 <- rep(n, nrow(m2))
  y2013 <- rbind(y2013, m2)
}



i <- 61
m <- read.csv(dir()[i], stringsAsFactors = FALSE)
n <- substring(dir()[i], 5, 7)
m$Tm1 <- rep(n, nrow(m))
y2014 <- m

for (i in 62:90){
  m2 <- read.csv(dir()[i], stringsAsFactors = FALSE)
  n <- substring(dir()[i], 5, 7)
  m2$Tm1 <- rep(n, nrow(m2))
  y2014 <- rbind(y2014, m2)
}


i <- 91
m <- read.csv(dir()[i], stringsAsFactors = FALSE)
n <- substring(dir()[i], 5, 7)
m$Tm1 <- rep(n, nrow(m))
y2015 <- m

for (i in 92:120){
  m2 <- read.csv(dir()[i], stringsAsFactors = FALSE)
  n <- substring(dir()[i], 5, 7)
  m2$Tm1 <- rep(n, nrow(m2))
  y2015 <- rbind(y2015, m2)
}


i <- 121
m <- read.csv(dir()[i], stringsAsFactors = FALSE)
n <- substring(dir()[i], 5, 7)
m$Tm1 <- rep(n, nrow(m))
y2016 <- m

for (i in 122:150){
  m2 <- read.csv(dir()[i], stringsAsFactors = FALSE)
  n <- substring(dir()[i], 5, 7)
  m2$Tm1 <- rep(n, nrow(m2))
  y2016 <- rbind(y2016, m2)
}


# write.csv(y2012, "../games12-13.csv")
# write.csv(y2013, "../games13-14.csv")
# write.csv(y2014, "../games14-15.csv")
# write.csv(y2015, "../games15-16.csv")
write.csv(y2016, "../games16-17.csv")







