# import library
library(tseries)
library(astsa)
library(zoo)
library(lubridate)
library(ggplot2)
library(forecast)
library(TSA)

# import data
# the original data is csv, so I used `read.csv` there
# but the dataset that I uploaded is txt file since it requires that
data <- read.csv("/Users/peiyiwang/Desktop/Stats 4A03/Project/Aquifer_Petrignano.csv")

# Data Processing
data$Date <- dmy(data$Date)

data$Depth_to_Groundwater_P24 <- na.locf(data$Depth_to_Groundwater_P24)
data$Depth_to_Groundwater_P25 <- na.locf(data$Depth_to_Groundwater_P25)

data$Depth_to_Groundwater <- rowMeans(
  data[, c("Depth_to_Groundwater_P24", "Depth_to_Groundwater_P25")]
  )
data <- data[, c("Date", "Depth_to_Groundwater")]

colSums(is.na(data))

data$Time <- as.Date(format(data$Date, "%Y-%m-01"))
data_month <- aggregate(Depth_to_Groundwater ~ Time, data = data, FUN = mean)

ggplot(data_month, aes(x = Time, y = Depth_to_Groundwater)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Monthly Average Depth to Groundwater",
    x = "Year",
    y = "Avg Depth to Groundwater per Month (m)"
  ) +
  theme_minimal()

# specify a tentative model and estimate parameters

# check stationary
adf.test(data_month$Depth_to_Groundwater) # not stationary

fdiff <- diff(data_month$Depth_to_Groundwater)
sfdiff <- diff(fdiff, lag = 12)
adf.test(sfdiff) # stationary

sfdiff_time <- data_month$Time[-c(1:13)]
sfdiff_df <- data.frame(Time = sfdiff_time, Value = sfdiff)

ggplot(sfdiff_df, aes(x = Time, y = Value)) +
  geom_line() +
  geom_point(size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Stationary Time Series (First ane Seasonal Difference)",
    x = "Time",
    y = "Differenced Value"
  ) +
  theme_minimal()

acf(sfdiff, lag.max = 50, main = "Autocorrelation Function (ACF)") 
pacf(sfdiff, lag.max = 50, main = "Partial Autocorrelation Function (PACF)")

# Model Diagnostic
mod <- arima(data_month$Depth_to_Groundwater, order=c(1,1,0), 
             seasonal = list(order = c(0,1,0), period = 12))
mod

# plot(residuals(mod), type='o') 

tsdiag(mod, gof=50)

# check for stationary
hist(rstandard(mod), xlab='Standardized Residuals') 
qqnorm(residuals(mod))
qqline(residuals(mod))

mod_overfit <- arima(data_month$Depth_to_Groundwater, order = c(2,1,0), 
                     seasonal = list(order = c(0,1,0), period = 12))
mod_overfit


# forecasting
pred <- predict(mod, n.ahead=24) 
pred

plot(pred$pred, type = "o", pch = 16,
     ylab = "Forecasted Groundwater Depth", xlab = "Time",
     main = " Forecast of Groundwater Depth",
     ylim = range(c(pred$pred + 2*pred$se, pred$pred - 2*pred$se)))
lines(pred$pred + 2 * pred$se, lty = 2)
lines(pred$pred - 2 * pred$se, lty = 2)




