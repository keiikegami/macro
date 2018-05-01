# (2)
log_diff <- ts(SA[["log"]])
ma <- ts(SA[["MA"]])

# ma
AIC_ma <- 1:24
for (i in 1:24){
  result <- arima(ma, order = c(i,0,0))
  AIC_ma[i] <- result$aic
}

ma_optimal_p <- which.min(AIC_ma)

# log
AIC_log <- 1:24
for (i in 1:24){
  result <- arima(log_diff, order = c(i,0,0))
  AIC_log[i] <- result$aic
}

log_optimal_p <- which.min(AIC_log)

# maの方が圧倒的にAICが小さい。のでmaの方がいいんじゃね？


# (3)

## (a)
gap <- ts(official_gap[["gap"]])
ts.plot(gap)

## (b)
install.packages("mFilter")
library(mFilter)
real_gdp <- read_csv("~/Desktop/2018summer/macro/hw1/empirics/real_gdp.csv")

rgdp <- ts(real_gdp[["rgdp"]])
hp_result <- hpfilter(rgdp)
hp_trend <- hp_result$trend
hp_cycle <- hp_result$cycle
ts.plot(hp_cycle)

## (c) unit root test
install.packages("tseries")
library(tseries)
# ADF test
result_adf <- adf.test(rgdp)
# DF test
result_df <- adf.test(rgdp, k = 0)
### 棄却されないので、非定常であることがわかる。

## (c)BN decomposition
first_diff_rgdp <- diff(rgdp)
# 差分系列のモデルとして最適なものを見つける。
AIC_diff <- 1:5
for (i in 1:5){
  result <- arima(first_diff_rgdp, order = c(i,0,0))
  AIC_diff[i] <- result$aic
}

diff_optimal_p <- which.min(AIC_diff)

## これで選ばれるのが2なので、AR(2)における公式を使って出せる。
## むしろAR(2)の時の問題をMorleyでちゃんととくのが大事
rgdp_ts <- ts(real_gdp[["rgdp"]], c(1947,1),, 4)

real_gdp[["X1"]][1]
real_gdp[["X1"]][length(real_gdp[["X1"]])]















