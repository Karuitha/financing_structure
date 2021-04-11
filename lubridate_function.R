## Working with dates and times in R 
# Lubridate 
library(lubridate)

## Generate series of dates ----
starting_week <- ymd("2010-01-01")
ending_week <- ymd("2020-12-31")

dates <- seq(starting_week, ending_week, by = "2 weeks")
dates
date1week <- seq(starting_week, ending_week, by = "1 week")
date1week
dates_weeks <- seq(starting_week, ending_week, by = "weeks")
dates_weeks
dates1month <- seq(starting_week, ending_week, by = "1 month")
dates1month
dates_days <- seq(starting_week, ending_week, by = "days")
dates_days

sample_quarters <- seq(starting_week, ending_week, 
                       by = "quarters")
sample_quarters
paste("We have included", length(sample_quarters), 
      "quarters in our dataset")

sample_years <- seq(starting_week, ending_week, 
                       by = "years")
sample_years
leap_year("2020-01-01")


## base R sys.date()
Sys.Date()
leo <- today()
day(leo)
month(leo)
year(leo)

# Day of the week ----
wday(leo, label = TRUE)

## System,time in base R ----
Sys.time() #lubridate equivalent now()
now()
hour(now())
minute(now())
second(now())

