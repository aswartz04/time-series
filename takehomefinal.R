## Time Series Final ##

#monthly data of new one family home sales
homes <- read.csv("monthly-sales-of-new-onefamily-h.csv")

#the last missing value was a result of some junk at the end and can
#be filtered out
homes <- homes[-nrow(homes),]

#time series begins the first month of 1973
homesales <- ts(homes[,2],freq=12,start=c(1973,1))

#some summary statistics
summary(homesales);min(time(homesales));max(time(homesales))

#output
plot.ts(homesales,ylab="Sales",main="New one-family homes sold in US")

##SARIMA model
homesales.fit <- arima(homesales,order=c(1,0,0),seasonal=list(order=c(4,1,1),
			     period=12))

#diagnostics
sarima(homesales,1,0,0,4,1,1,12)

##forecasting next year
homesales.predicted <- predict(homesales.fit,n.ahead=12)
U <- homesales.predicted$pred + homesales.predicted$se
L <- homesales.predicted$pred - homesales.predicted$se

#output
plot.ts(homesales,xlim=c(1990,1997),ylab="Sales", 
        main="One-year forecast of home sales")
lines(homesales.predicted$pred,type="p",col="blue")
lines(homesales.predicted$pred,type="l",col="blue")
lines(U,lty="dashed",col="red")
lines(L,lty="dashed",col="red")

##observing frequencies
homesales.pgram <- spec.pgram(homesales,taper=0,log="no")
spec.desc <- sort(homesales.pgram$spec,decreasing=T)
homesales.pgram$freq[homesales.pgram$spec==spec.desc[1]]
homesales.pgram$freq[homesales.pgram$spec==spec.desc[2]]
homesales.pgram$freq[homesales.pgram$spec==spec.desc[5]]

U <- qchisq(0.025,2)
L <- qchisq(0.975,2)
#8-year cycle
c(2*spec.desc[1]/L,2*spec.desc[1]/U)
#1-year cycle
c(2*spec.desc[2]/L,2*spec.desc[2]/U)
#6-month cycle
c(2*spec.desc[5]/L,2*spec.desc[5]/U)
#proportion of frequencies falling below lower bound of 6-month cycle
sum(2*spec.desc[5]/L > spec.desc)/length(spec.desc)
#output
par(mfrow=c(2,2))
spec.pgram(homesales,taper=0,log="no",
	     main="a. Periodogram with marked frequencies")
abline(v=0.125,lty="dashed");abline(v=1,lty="dashed");abline(v=2,lty="dashed")
spec.pgram(homesales,taper=0,log="no",
	     main="b. Lower bound for 8-year cycle")
abline(h=2*spec.desc[1]/L,col="blue")
spec.pgram(homesales,taper=0,log="no",
	     main="c. Lower bound for 1-year cycle")
abline(h=2*spec.desc[2]/L,col="blue")
spec.pgram(homesales,taper=0,log="no",
	     main="d. Lower bound for 6-month cycle")
abline(h=2*spec.desc[5]/L,col="blue")




