#' ---
#' title: "NBA 16-17 Goldsheet Analysis"
#' date: ""
#' author: ""
#' output: html_document
#' # Or use output: pdf_document
#' # Or don't use either, in which case you will be prompted to choose
#' ---
#'
#' # NOTE
#' 
#' To "Compile Report" you may need additional packages.  Depending on your
#' version of R and R Studio, this sometimes can be a challenge (due to
#' update schedules). Although RStudio
#' isn't required, I strongly recommend using it.
#' 
#' You can (and perhaps should) do this course producing documents and
#' even the final report as HTML.
#' 
#' Although it is not required, PDF is pretty nice!  If you want to try
#' this, you'll be on your own because Boyu and I need to focus on other
#' more important things.  However, you would need
#' \LaTeX~ (MacTeX on the Mac and MikTeX on Windows).  For Windows, make
#' sure you get the ___complete distribution___ of MikTeX -- the basic version
#' will not work!
#' 

# fileurl <- 'https://www.goldsheet.com/historic/nbalog16.html'
# x <- scan(fileurl, what = "", sep = "\n")

figure <- "Data/nbalog16.html"
x <- scan(figure, what = "", sep = "\n")


######################### Data Cleaning ########################################

x <- gsub('&nbsp;', '', x, fixed = TRUE)      
x <- gsub('</span>', ',', x, fixed = TRUE)
x <- gsub("<[^<>]*>", "", x)
y <- strsplit(x, ',')

table(sapply(y, length))
#
# Scores
#
z <- y[sapply(y, length) == 7]
zz <- matrix(unlist(z), ncol = 7, byrow = TRUE)
scores <- strsplit(zz[, 5], "-")
table(sapply(scores, length))
scores <- matrix(as.numeric(unlist(scores)), ncol = 2, byrow = TRUE)

#
# Team 1
#
team1 <- y[sapply(y, length) == 1]
team1 <- gsub("FINAL 2016-17 NBA HOOP LOGS","", team1, fixed = TRUE)
index_collection <- grep("^[A-Z]", team1) 
team_1 <- list()
number <- list()
j <- 1
for(i in c(index_collection)){
  team_1[j] <- team1[i]
  number[j] <- team1[i+1]
  number[j] <- strsplit(number[[j]], " ")[[1]][2] 
  number[j] <- as.numeric(substring(number[j], 1, regexpr("-", number[j]) - 1))+
    as.numeric(substring(number[j], regexpr("-", number[j]) + 1))
  j <- j + 1
}

team_1 <- as.vector(unlist(rep(team_1, times = number))) 
#
# Point spread
#
sp <- gsub("'", ".5", zz[, 4])
sp <- as.numeric(gsub("P", 0, sp))



x <- data.frame(Date_of_game = zz[,1],
                Team1 = team_1,
                Team2 = zz[,2],
                Pointspread = sp,
                Score1 = scores[,1],
                Score2 = scores[,2],
                scorediff = scores[,1] - scores[,2],
                Location = zz[,6],
                stringsAsFactors = FALSE)

table(x$Location)
x[x$Location == "N", ]
x$Location <- substring(x$Location, 1, 1)
# We only take care of "H" or "V", so we do not need the numbers afterwards. 

unique(x$Team1)  # 30 teams
unique(x$Team2)  # 31 teams
x[x$Team2 == "LA Laker", 3]
x[x$Team2 == "LA Laker", 3] <- gsub("LA Laker", "LA Lakers", 
                                   x[x$Team2 == "LA Laker", 3])


# Here, we adjust the names of Team2.
name1 <- sort(unique(x$Team1))
name2 <- sort(unique(x$Team2))
data.frame(name1, name2)
# By visually checking they match!
for( i in 1:length(name2)) {
  x[x$Team2 == name2[i], 'Team2'] <- name1[i]
}
identical(sort(unique(x$Team1)), sort(unique(x$Team2))) # TRUE!


# We changed the format of the date.
year1 <- '2016'
year2 <- '2017'
datemat <- as.data.frame(matrix(unlist(strsplit(x$Date_of_game, '/')), 
                  ncol = 2, byrow = TRUE))
listdate <- rep(NA, nrow(x))

listdate[as.numeric(datemat[, 1]) >= 10] <- format(as.Date(paste(year1, 
        x$Date_of_game[as.numeric(datemat[, 1]) >= 10], sep = '/'), "%Y/%m/%d"))
listdate[as.numeric(datemat[, 1]) < 10] <- format(as.Date(paste(year2, 
        x$Date_of_game[as.numeric(datemat[, 1]) < 10], sep = '/'), "%Y/%m/%d"))

x$Date_of_game <- listdate




# Here, we extract a new data set of independent games(1309).
teams <- rep(NA, 30)
x_new <- data.frame()
unique(x$Team1)
for (i in 1:30){
  teams[i] <- unique(x$Team1)[i]
  mat <- x[x$Team1 == unique(x$Team1)[i] & !(x$Team2 %in% teams), ]
  x_new <- rbind(x_new, mat)
}
nrow(x_new)
row.names(x_new) <- c(1:1309)


t1 <- sort(unique(x_new$Team1))
t2 <- sort(unique(x_new$Team2))
length(t2)
xx <- data.frame()
for (i in c(1:29)){
  m <- x_new[x_new$Team1 == t1[i], ]
  for (j in c(1:length(t2))) {
    n <- m[m$Team2 == t2[j], ]
    xx <- rbind(xx, n)
  }
  t2 <- t2[2:length(t2)]
}


nrow(xx)
row.names(xx) <- c(1:1309)

write.csv(xx, "Data/NBA 2017.csv")










