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