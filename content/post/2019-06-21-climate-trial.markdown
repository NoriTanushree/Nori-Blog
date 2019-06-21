---
title: Climate Trial
author: ''
date: '2019-06-21'
slug: climate-trial
categories: []
tags: []
keywords:
  - tech
---








##  Time Series Analysis

***
Apart from exploring the patterns of change in climate data we also wanted to predict and benchmark temperatures using several types of time series forecast models. 

Why time series forecasting?

Because climate data is heavily time-dependent and has naturally temporal ordering, we can use previously observed values to predict future values

We will try and predict temperatures in NYC

Data Import




```r
ggplot(GlobalTemp_NYC[GlobalTemp_NYC$dt >= as.Date("1950-01-01"),] , aes(dt, AverageTemperature)) + geom_line() + xlab("") + ylab("Daily Temperature") + ggtitle("Temperature variation through the years in NYC")
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-3-1.png" width="672" />

There isn't a strong visible trend component, but there does seem to be a seasonal pattern.
<br>

### **Can we discern a trend in the data?**


```r
# Group NYC data by year
GlobalTemp_NYC_Year = GlobalTemp_NYC %>% group_by(Year) %>% summarise(AverageTemperatureYearly = mean(AverageTemperature,na.rm=T))

# Add moving average column to the data frame
GlobalTemp_NYC_Year = GlobalTemp_NYC_Year %>% mutate(roll_ma = rollmean(AverageTemperatureYearly, k=5, fill=NA))
```

<br>


```r
ggplot(GlobalTemp_NYC_Year[GlobalTemp_NYC_Year$Year >= 1900, ], aes(Year, AverageTemperatureYearly),size=2) + geom_line(position=position_jitter(1,3), pch=21, fill="#FF0000AA") + geom_line(aes(y=rollmean(roll_ma, 10, na.pad=TRUE), color="red")) + xlab("Years") + ylab("Yearly averages") + scale_x_continuous(breaks = pretty(GlobalTemp_NYC_Year[GlobalTemp_NYC_Year$Year >= 1900, ]$Year,n=10)) + ggtitle("Yearly averages of temperatures in NYC ")
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-5-1.png" width="672" />


The rolling means of temperature over time do seem to be sloping upwards.



### **More evidence of an existing trend and non-stationarity component ! (albeit not statistically proven yet)**


```r
# Converts the dataset into a time series object
# Split data into test and train 
GlobalTemp_NYC_sub = dplyr::filter(GlobalTemp_NYC, Year >= 1900 & Year <= 2010)
Test = dplyr::filter(GlobalTemp_NYC, Year > 2010)

# Convert columns to ts() type
GlobalTemp_NYC_sub$AverageTemperatureTS = ts(GlobalTemp_NYC_sub[,c('AverageTemperature')])
Test$AverageTemperatureTS = ts(Test[,c('AverageTemperature')])

# Calculate rolling mean with a 12 period window
GlobalTemp_NYC_sub$cnt_ma = ma(GlobalTemp_NYC_sub$AverageTemperatureTS, order=12) 

# Calculate rolling standard deviation
GlobalTemp_NYC_sub$cnt_std =  rollapply(GlobalTemp_NYC_sub$cnt_ma, width = 60, FUN = sd, na.pad = TRUE)
```

*Plots of rolling means and standard deviations over time*


```r
# Moving average plot
ggplot() +
  geom_line(data = GlobalTemp_NYC_sub, aes(x = dt, y = cnt_ma,   colour = "Yearly Moving Average"))  + ylab('Temperature') + xlab('Date')

# Rolling SD plot
ggplot() +
  geom_line(data = GlobalTemp_NYC_sub, aes(x = dt, y = cnt_std,   colour = "Rolling Standard Deviation"))  + ylab('Temperature') + xlab('Date')
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-7-1.png" width="50%" /><img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-7-2.png" width="50%" />

The data definitely does not look stationary. This is supported by both the rolling mean and standard deviation plots. The standard deviation has been considered over a 5 year rolling period window, the average of which does not look constant over time.

We do have to verify the above trends statistically though.

Let's analyse seasonality in the data. We expect climate data to be seasonal in nature


```r
ggplot(GlobalTemp_NYC[GlobalTemp_NYC$Year>=1900,],aes(x=dt,y=AverageTemperature,colour=reorder(Month.String,-AverageTemperature,mean)))+
  geom_point()+
  geom_smooth(method="loess")+
  labs(title="Average Temperatures by Month in New York",
       x="Year",
       y="Average Temperature",
       colour="Month")
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Through the years we see that temperatures do stay around similar ranges within a month. We'll come back to seasonality in a bit.

#### **Time series decompostion:**
Time series data is decomposed into:

1. Seasonal
2. Trend
3. Cycle
4. Residual or error

It's a useful abstraction and involves thinking of time series as a combination of the above components


```r
# Indicating a seasonality of 12
count_ma = ts(GlobalTemp_NYC_sub$AverageTemperatureTS, frequency=12)
# decompsing data,
decomp = stl(count_ma, s.window="periodic")
# removing seasonality for further modeling
deseasonal_cnt <- seasadj(decomp)
#plot of components
plot(decomp)
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-9-1.png" width="672" />

We notice that based on the gray bars in each plot, the impact of trend on over-all variation in the data isn't very high. Seasonality on the other hand explains the variation in the data to a much greater extent.


A closer look at seasonality as decomposed above:


```r
seasonplot(count_ma,year.labels=TRUE,col=rainbow(6), main="Seasonal plot colored by year")
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-10-1.png" width="672" />

Yep, looks pretty seasonal


### **Statistical proof of stationarity through the ADF test:**

*Theory :* ADF procedure tests whether the change in Y can be explained by lagged value and a linear trend. If contribution of the lagged value to the change in Y is non-significant and there is a presence of a trend component, the series is non-stationary and null hypothesis will not be rejected.


```r
adf.test(deseasonal_cnt, alternative = "stationary")
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  deseasonal_cnt
## Dickey-Fuller = -10.319, Lag order = 10, p-value = 0.01
## alternative hypothesis: stationary
```

* The results indicate that the time series is in-fact, stationary which weakly translates to - mean and variance stay the same over time (Or, that the marginal distribution of the process does not change over time)

* This is counter-intuitive to the expectation that average temperature have risen over time (Does not disprove global warming though)


<br>

#### A look at the ACF and PACF plots of the raw time series data:

```r
Acf(count_ma, main='')
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
Pacf(count_ma, main='')
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-12-2.png" width="672" />


* Acf plot gives us values of auto-correlation of any series with its lagged values
* Pacf plot gives us the partial correlation coefficients between the series and lags of itself

<br>

### **Time to fit our Models!**

***

My baseline model will be a seasonal exponential smoothing model. 

Models to be used for selection and bench-marking:

1. Naive seasonal exponential smoothing model
2. ARIMA model
3. SARIMA model with drift

**Fitting models to the training data**

```r
#1 
naive_fit = Arima(count_ma,order=c(0, 1, 1),
            seasonal=list(order=c(0, 1, 1), period=12))

#2
fit = auto.arima(deseasonal_cnt, seasonal=FALSE)


#3
# Force the model to acknowlede the seasonality pattern with the Drift parameter set to 1
fit_seasonal = auto.arima(count_ma,D=1)
```


### **Evaluate results!**

<br>


## Residuals by model type {.tabset .tabset-fade .tabset-pills}

### Residuals for Baseline Model


```r
tsdisplay(residuals(naive_fit), lag.max=45, main='Naive baseline model')
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-14-1.png" width="672" />

### Residuals for ARIMA


```r
tsdisplay(residuals(fit), lag.max=45, main='ARIMA Model')
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-15-1.png" width="672" />

### Residuals for SARIMA with Drift


```r
tsdisplay(residuals(fit_seasonal), lag.max=45, main='SARIMA Model')
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-16-1.png" width="672" />

*Some observations*

* The only auto-correlations seem to be in the seasonal lags for the SARIMA model (12,24,36)

* ACF results from the baseline model do not seem to be dampening with time

<br>

**Compare MAEs from all the models**

<br>

* MAE is mean(|et|)
* We pick MAE since it is easy to interpret and all the models use the same data (so no scaling issues)
* We forecast on our test data set with observations from 2011-2013


```r
Naive_MAE = accuracy(naive_fit, h=Test)[3]
Arima_MAE = accuracy(fit, h=Test)[3]
Sarima_MAE = accuracy(fit_seasonal, h=Test)[3]

eval_frame = data.frame(Naive_MAE,Arima_MAE,Sarima_MAE)
knitr::kable(eval_frame, caption = ' MAE across models' ) %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>Table 1:  MAE across models</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Naive_MAE </th>
   <th style="text-align:right;"> Arima_MAE </th>
   <th style="text-align:right;"> Sarima_MAE </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1.269324 </td>
   <td style="text-align:right;"> 1.247306 </td>
   <td style="text-align:right;"> 1.432573 </td>
  </tr>
</tbody>
</table>


* We see that MAE seems lowest for the baseline model (the irony!). But let's look at the forecasts too.

<br>


**Forecast using all the three models**


```r
Naive.Mean= as.data.frame(forecast(naive_fit, h=33)$mean)
Arima.Mean= as.data.frame(forecast(fit, h=33)$mean)
Sarima.Mean= as.data.frame(forecast(fit_seasonal, h=33)$mean)
```

Forecast from various models

```r
Test$dt = as.Date(Test$dt)
cols=colnames(Test)
Test = cbind(Test,Naive.Mean,Arima.Mean,Sarima.Mean)
names(Test) = append(cols, c("naive","arima","sarima"))

ggplot() +
  geom_line(data = Test, aes(x = dt, y = AverageTemperatureTS,color="Test Data"),size=1)+ ylab('Temperature') + geom_line(data=Test, aes(x=dt,y=naive, color="Naive Baseline") ,size=1,linetype="dashed") + geom_line(data=Test, aes(x=dt,y=arima,color="ARIMA"),linetype="solid",size =1) + geom_line(data=Test, aes(x=dt,y=sarima,color="SARIMA"),size=1,linetype="dotted") +scale_color_manual(name = "Colors", 
                     values = c("Test Data"="orange","Naive Baseline" = "red", "ARIMA" = "darkgreen", "SARIMA"= "blue"))
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-19-1.png" width="672" />

What we see here is the forecasts by each model against the Test data over 2011-2013 in Orange. The baseline doesn't seem to be much worse than the model that does take seasonality and differencing into account.

<style>
div.blue { background-color:#e6f0ff; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">
Something kind of interesting can be highlighted here:

1. Why does it seem like our Naive forecasts and ARIMA do just as well (Naive does better infact) than our seasonal forecast? Especially because Naive and ARIMA "do not account for seasonality"?
    + The Naive method and the ARIMA are *perfectly good* for one-step ahead forecasts because the evident smoothness of seasonality means that knowing information from exactly one year ago gives little or no information about the next likely value than knowing just the last couple of observations.
 
2. If we wished to forecast over a longer time frame - the next 4-5 years say, we would have to take seasonality into account. Since we only have a fixed 12 seasonal effects we can go ahead by estimating the seasonal effects. In that case, the SARIMA could out-perform

3. But it is good to note that time series forecasts are not reliable over extended time periods and the models need to be re-run/refreshed routinely

</div>

<br>

**An extrapolation of the seasonal model's fit**


```r
plot(forecast(fit_seasonal, 12), include=36)
```

<img src="/post/2019-06-21-climate-trial_files/figure-html/unnamed-chunk-20-1.png" width="672" />

#### Next steps?

Use forecast package to create Fourier terms to use as covariates. This can lead to a more precise modeling of seasonal information.


