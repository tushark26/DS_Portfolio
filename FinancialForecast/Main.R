library("tseries")
library("forecast")
library("imputeTS")


getwd()
setwd("/01 Models - External Variables/")


#Check accuracy of models  
calculate.MAPE <- function(fcst,actuals,periods){
  actualVal<- colSums(matrix(tail(actuals, n = periods), nrow=3))
  predictedVal <- colSums(matrix(fcst$mean, nrow=3))
  result = abs(actualVal-predictedVal)/actualVal
  View(result)
  return(result)
}

#Invoke function
calculate.MAPE (forecast(sdar1a,h=6),rev,6L)



#Segment 6
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==6, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
    
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,9))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    sdar1a <- Arima(revt,order=c(0,0,1),seasonal=c(0,1,0),include.mean=T)
    sdar1a
    plot(forecast(sdar1a,6))
    lines(rev)
    View(forecast(sdar1a,12))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    
    
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=6))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=6))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=6))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1b,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)


#Segment 9 
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==9, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
    
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,9))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    sdar1a <- Arima(revt,order=c(0,0,1),seasonal=c(0,1,0),include.mean=T)
    sdar1a
    plot(forecast(sdar1a,6))
    lines(rev)
    View(forecast(sdar1a,6))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    
    
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=6))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=6))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=6))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1a,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)

    
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")    
    lines(rev)
    View(forecast(sdar1a,h=12))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    
        
#Segment 11 
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==11, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
    
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,12))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=6))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=6))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1a,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)
    
    
    sdar1a <- Arima(revt,order=c(0,0,0),seasonal=c(0,1,0),include.mean=T)
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")    
    lines(rev)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    
        
#Segment 16 
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==16, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
    
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,9))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=12))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=9))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1b,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)
        
    
#Segment 22 
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==22, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,9))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=9))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=12))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1b,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)
    

#Segment 38 
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==38, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,9))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=9))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=12))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1a,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)
    
# Seg 38 - gseas ARIMA(0,1,1)x(0,1,1)m without constant - 89% accuracy
    

#Segment 39
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==39, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,9))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=12))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=9))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1b,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)
    
    
#Segment 40 
    all_products<-read.csv("ModelingDatasewithVariables v1.1.csv",header=T)
    temp <- all_products[all_products$SEGMENT==40, ]
    rev <- ts(temp[,c("NET_SALES")],start=c(2013,1),frequency=12)
  
    #Imputation Replace zeros with NA
    rev[rev == "0"] <- NA
    #Spline interpolation
    rev <- na.interpolation(rev, option ="spline")
    rev <- na.ma(rev, k=6)
    
    plot(rev, type="l",main="Revenue",ylab="Dollars",xlab="Month")
    
    revt <- window(rev, start=2013, end=2016.41667)
    plot(revt, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    
    revf <- window(rev, start=2016.5)
    plot(revf, type = "l" ,main="Revenue",ylab="Dollars",xlab="Month")
    

    #ACF
    acf(revt)
    #PACF
    pacf(revt)
    # Augmented Dickey-Fuller Test ADF
    adf.test(revt, alternative = "stationary")
    # Kwiatkowski-Phillips-Schmidt-Shin Test KPSS 
    kpss.test(revt)
    
    #Differencing
    acf(diff(revt,1))
    pacf(diff(revt,1))
    
    #Double differencing 
    acf(diff(diff(revt,1)))
    pacf(diff(diff(revt,1)))
    
    
    #Best model no approximations 
    abest <- auto.arima(revt,approximation = FALSE)
    abest
    View(forecast(abest,9))
    plot(forecast(abest,9))
    lines(rev)
    calculate.MAPE(forecast(abest,h=6),rev,6L)
    
    
    # 4.13. Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant
    srt <- Arima(revt,order=c(0,1,0),seasonal=c(0,1,0))
    # Chart
    plot(forecast(srt,h=6),main="Seasonal Random Trend ARIMA(0,1,0)x(0,1,0)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(srt)
    View(forecast(srt,h=9))
    calculate.MAPE(forecast(srt,h=6),rev,6L)
    
    
    # 4.14. General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant
    gseas <- Arima(revt,order=c(0,1,1),seasonal=c(0,1,1))
    # Chart
    plot(forecast(gseas,h=6),main="General Seasonal Model ARIMA(0,1,1)x(0,1,1)m without constant",ylab="Level",xlab="Day")
    lines(rev)
    # Coefficients
    summary(gseas)
    View(forecast(gseas,h=9))
    calculate.MAPE(forecast(gseas,h=6),rev,6L)
    
    
    # 4.15. General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant
    gsar1a <- Arima(revt,order=c(1,0,1),seasonal=c(0,1,1),include.mean=T)
    gsar1b <- Arima(diff(revt,lag=5),order=c(1,0,1),seasonal=c(0,0,1),include.mean=T)
    # Charts
    plot(forecast(gsar1b,h=6),main="General First Order Autoregressive Seasonal Model ARIMA(1,0,1)x(0,1,1)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(gsar1b) 
    View(forecast(gsar1a,h=6))
    calculate.MAPE(forecast(gsar1a,h=6),rev,6L)
    calculate.MAPE(forecast(gsar1b,h=6),rev,6L)
    
    
    # 4.16. Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant
    sdar1a <- Arima(revt,order=c(1,0,0),seasonal=c(0,1,0),include.mean=T)
    sdar1b <- Arima(diff(revt,lag=5), order=c(1,0,0),seasonal=c(0,0,0),include.mean=T)
    # Charts
    plot(forecast(sdar1a,h=6),main="Seasonally Differentiated First Order Autoregressive ARIMA(1,0,0)x(0,1,0)m with constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(sdar1b)
    View(forecast(sdar1a,h=9))
    View(forecast(sdar1b,h=12))
    calculate.MAPE(forecast(sdar1a,h=6),rev,6L)
    calculate.MAPE(forecast(sdar1b,h=6),rev,6L)
    
    
    # 4.17. Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant
    ahwa <- Arima(revt,order=c(0,1,6),seasonal=c(0,1,0))
    # Chart
    plot(forecast(ahwa,h=6),main="Holt-Winters Additive Seasonality ARIMA (0,1,6)X(0,1,0)5 without constant",ylab="Level",xlab="Day")
    lines(rev)
    # ARIMA Coefficients
    summary(ahwa)
    View(forecast(ahwa,h=9))
    calculate.MAPE(forecast(ahwa,h=6),rev,6L)
    
    
