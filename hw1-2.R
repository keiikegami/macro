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



# (4)

## (a)
gap <- ts(official_gap[["gap"]], c(1949,1),,4)
ts.plot(gap)

## (b)
install.packages("mFilter")
library(mFilter)
real_gdp <- read_csv("~/Desktop/2018summer/macro/hw1/empirics/real_gdp.csv")

rgdp <- ts(real_gdp[["rgdp"]], c(1947),,4)
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
result <- arima(first_diff_rgdp, order = c(2,0,0))
beta1 <- result$coef[1]
beta2 <- result$coef[2]
BN_trend <- 1:length(rgdp)-2
for (i in 3:length(rgdp)){
  BN_trend[i-2] <- rgdp[i] + ((beta1+beta2)*first_diff_rgdp[i] + beta2*first_diff_rgdp[i-1])/(1-beta1-beta2)
}
BN_cycle <- ts(rgdp[3:length(rgdp)] - BN_trend, c(1947,7),,4)
ts.plot(BN_cycle)


## (d) トレンド
rgdp_ts <- ts(real_gdp[["rgdp"]], c(1947,1),, 4)

real_gdp[["X1"]][1]
real_gdp[["X1"]][length(real_gdp[["X1"]])]

ind <- (1973 - 1947) * 4 + 2
# トレンド変化前
before <- rgdp_ts[1:ind]
# トレンド変化後
after <- rgdp_ts[ind:length(rgdp_ts)]
# トレンド項の作成
before_trend <- time(before)
after_trend <- time(after)

# トレンド回帰
before_result <- lm(before ~ before_trend)
before_gap <- residuals((before_result))
after_result <- lm(after ~ after_trend)
after_gap <- residuals((after_result))

kinked_gap <- ts(c(data.frame(before_gap)$before_gap, data.frame(after_gap)$after_gap),c(1947,1),,4)

ts.plot(kinked_gap)












