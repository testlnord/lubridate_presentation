library(lubridate)
library(dplyr)

set.seed(4)

# active users
min.date <- ymd('2010-1-1')
max.seconds <- as.numeric(ddays(as.numeric(ymd('2016-12-31')-min.date)))
user.activities <-  do.call(rbind, 
        mapply(function(user.id, activities.count){
                  activity.dates <- round_date(min.date + duration(rbeta(activities.count, 4, 1)*max.seconds), "day")
                  data.frame(user.id = rep(user.id, activities.count), activity.dates = activity.dates )
              }, 1:1000, as.integer(runif(1000, 1,100)), SIMPLIFY = FALSE)
        ) 
write.csv(user.activities %>% arrange(activity.dates), "data/user_activities.csv", row.names = FALSE)

# bad formats
min.date <- ymd('2012-1-1')
max.date <- ymd('2015-1-1')
seconds.diff <- duration(interval(min.date, max.date))
dates.count <- 100
bad.dates <- c(
  format(min.date+duration(runif(dates.count, 1, seconds.diff)), "%Y-%m-%d"),
  format(min.date+duration(runif(dates.count, 1, seconds.diff)), "%Y-%m-%d %H:%M"),
  format(min.date+duration(runif(dates.count, 1, seconds.diff)) )) %>% sort()
write.csv(bad.dates, "data/bad_formats.csv", row.names=FALSE)


# seconds
trials.count <- 100
seconds.a <- data.frame(condition = rep("A",trials.count), time = runif(trials.count,10,2000))
seconds.b <- data.frame(condition = rep("B",trials.count), time = runif(trials.count,1000,5000))
randomly <- function(x) sample(xtfrm(x))
seconds.all <- rbind(seconds.a, seconds.b) %>% arrange(randomly(condition))
seconds.all$time <- format(ymd("12-2-2")+duration(seconds.all$time), "%H:%M:%S")
write.csv(seconds.all, "data/seconds.csv", row.names=FALSE)