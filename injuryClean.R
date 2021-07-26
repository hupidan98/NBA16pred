# injury data
injury <- read.csv('Data/injuries_2010-2020.csv', stringsAsFactors = FALSE)
head(injury)
# Last column is not useful
# injury <- injury[, -5]
injury$Date <- as.Date(injury$Date)
head(injury)

injury1617 <- injury[injury$Date < as.Date('2017-07-01')
                     & injury$Date > as.Date('2016-08-01'),]
write.csv(injury1617, 'Data/injury1617.csv')

injuryback <- injury[!injury$Acquired == "", ]


# What doI need
# Team, Player, GoneDate, BackDate

# injuryback <- injury[injury$Acquired != '' ,]
injuryback <- injury[injury$Acquired != '' 
                     & injury$Date < as.Date('2017-07-01')
                     & injury$Date > as.Date('2016-08-01'),]



ib <- injuryback
injuryback <- injuryback[, c(2, 3, 1)]
injuryback$gone <- NA


ig <- injury[injury$Relinquished != '' 
                     & injury$Date < as.Date('2017-07-01')
                     & injury$Date > as.Date('2016-08-01'),]

ib$Name <- ib$Acquired
ib$Dir <- 1
ib <- ib[,c(1, 2, 6, 7, 5)]
ig$Name <- ig$Relinquished
ig$Dir <- -1
ig <- ig[,c(1, 2, 6, 7, 5)]
iall <- rbind(ib, ig)
iall <- iall[order(iall$Date), ]

c('Team', 'Player', 'GoneDate', 'BackDate')

iall[iall$Name == 'Klay Thompson', ]
# The model looks right


for( i in 1:nrow(injuryback)){
  DateGone <- sort(injury[injury$Relinquished == injuryback[i, 2] &
                            injury$Date < injuryback[i, 3], 'Date'],
                   decreasing = TRUE)[1]
  injuryback[i, 'gone'] <- DateGone
}
# Using only injury data from 2016 -2017, use less data
# Row as date, 270 days as season is 9 months, 270 rows
# Col as player, so matrixs is # play
# 1 is player can play, 0 if can't
# E.G. 0910 a got injurd, he is 1, after he is 0
# 400ish cols in 16-17
# know what player team played for, subset of cols that only play for one team

# Another mat, same dim, store stat of that player before game date
#  EXPLORE FROM HERE!!!!!


# expand.grid (look it up in R, create the mat described above)!!!!!!!!!
# 

injuryback$gone <-  as.Date(injuryback$gone, origin = "1970-01-01")
injuryback <- injuryback[, c(1, 2, 4, 3)]
names(injuryback) <- c('Team', 'Player', 'GoneDate', 'BackDate')
head(injuryback)

write.csv(injuryback, 'Data/injurylist.csv')


