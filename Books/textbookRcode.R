library(astsa) # SEE THE FOOTNOTE
plot(jj, type="o", ylab="Quarterly Earnings per Share")

plot(speech)

library(TTR)
djia = getYahooData("^DJI", start = 20060420, end=20190912, freq = "daily")
library(xts)
djiar = diff(log(djia$Close))[-1]
plot(djiar, main = "DJIA Returns",type = "n")
lines(djiar)

par(mfrow = c(2,1)) #set up graphics
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")

par(mfrow=c(2,1))
ts.plot(fmri1[,2:5], col=1:4, ylab="BOLD", main="Cortex")
ts.plot(fmri1[,6:9], col=1:4, ylab="BOLD", main="Thalamus & Cerebellum")

par(mfrow=c(2,1))
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion")


#white noise and its 3mma
w = rnorm(500,0,1)
v = filter(w,sides=2,filter=rep(1/3,3))
par(mfrow=c(2,1))
plot.ts(w,main="white noise")
plot.ts(v,ylim=c(-3,3),main="moving average")

#autoregression
w = rnorm(550,0,1) #50 extra to avoid startup problems
x = filter(w, filter = c(1,-.9),method = "recursive")[-(1:50)] #remove first 50
plot.ts(x, main ="autoregression")

#random walk with/without drift
set.seed(154)
w = rnorm(200); x = cumsum(w)
wd = w+.2; xd = cumsum(wd)
plot.ts(xd, ylim = c(-5,55),main="random walk",ylab='')
lines(x,col=4); abline(h=0,col=4,lty=2);
abline(a=0,b=0.2,lty=2)

#signal in noise
cs = 2*cos(2*pi*1:500/50 + .6*pi); w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))


