## Time Series Final ##

#monthly data of new one family home sales
homes <- read.csv("monthly-sales-of-new-onefamily-h.csv")

#the last missing value was a result of some junk at the end and can
#be filtered out
homes <- homes[-nrow(homes),]

#time series begins the first month of 1973
homesales <- ts(homes[,2],freq=12,start=c(1973,1))

#the last missing value was a result of some junk at the end and can
#be filtered out
homes <- homes[-length(homesales)]

#some summary statistics
summary(homesales)
min(time(homesales))
max(time(homesales))