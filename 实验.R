sy$DLNRFP <- c(NA, diff(log(sy$RFP))*100)
sy$DLNIFP <- c(NA, diff(log(sy$IFP))*100)
sy$DLNHFP <- c(NA, diff(log(sy$HFP))*100)
sy$DLNJFP <- c(NA, diff(log(sy$JFP))*100)
sy$TIME <- as.Date(sy$time)
#讲数据集按照时间排序
sy <- sy[order(sy$TIME),]
library(ggplot)
#使用线性插值法将时间序列DLNRFP、DLNISP、DLNHFP、DLNJSP补齐
sy$DLNRFP[is.na(sy$DLNRFP)] <- approx(x = sy$TIME[!is.na(sy$DLNRFP)], y = sy$DLNRFP[!is.na(sy$DLNRFP)], xout = sy$TIME[is.na(sy$DLNRFP)])$y
sy$DLNIFP[is.na(sy$DLNIFP)] <- approx(x = sy$TIME[!is.na(sy$DLNIFP)], y = sy$DLNISP[!is.na(sy$DLNIFP)], xout = sy$TIME[is.na(sy$DLNIFP)])$y
sy$DLNHFP[is.na(sy$DLNHFP)] <- approx(x = sy$TIME[!is.na(sy$DLNHFP)], y = sy$DLNHFP[!is.na(sy$DLNHFP)], xout = sy$TIME[is.na(sy$DLNHFP)])$y
sy$DLNJFP[is.na(sy$DLNJFP)] <- approx(x = sy$TIME[!is.na(sy$DLNJFP)], y = sy$DLNJSP[!is.na(sy$DLNJFP)], xout = sy$TIME[is.na(sy$DLNJFP)])$y
sy$DLNJFP <- na.locf(sy$DLNJFP, na.rm = FALSE)  # 前向填充开头的 NA
sy$DLNJFP <- na.locf(sy$DLNJFP, fromLast = TRUE)  # 后向填充结尾的 NA
sy$DLNHFP <- na.locf(sy$DLNHFP, na.rm = FALSE)  
sy$DLNHFP <- na.locf(sy$DLNHFP, fromLast = TRUE)  
sy$DLNIFP <- na.locf(sy$DLNIFP, na.rm = FALSE)  
sy$DLNIFP <- na.locf(sy$DLNIFP, fromLast = TRUE) 
sy$DLNRFP <- na.locf(sy$DLNRFP, na.rm = FALSE)  
sy$DLNRFP <- na.locf(sy$DLNRFP, fromLast = TRUE)  
#用ggplot2画出以上新生成变量的的时间序列图，分成四张图  分别是DLNRFP、DLNISP、DLNHFP、DLNJSP
ggplot(sy, aes(x=TIME, y=DLNRFP)) + geom_line() + ggtitle("DLNRFP") + theme_minimal()
ggplot(sy, aes(x=TIME, y=DLNIFP)) + geom_line() + ggtitle("DLNIFP") + theme_minimal()
ggplot(sy, aes(x=TIME, y=DLNHFP)) + geom_line() + ggtitle("DLNHFP") + theme_minimal()
ggplot(sy, aes(x=TIME, y=DLNJFP)) + geom_line() + ggtitle("DLNJFP") + theme_minimal()
library(urca)
# 进行 ADF 检验
adf_DLNJFP<- ur.df(sy$DLNJFP, type = "none")
summary(adf_DLNJSP)
adf_DLNHFP<- ur.df(sy$DLNHFP, type = "none")
summary(adf_DLNHFP)
adf_DLNIFP<- ur.df(sy$DLNIFP, type = "none")
summary(adf_DLNISP)
adf_DLNRFP<- ur.df(sy$DLNRFP, type = "none")
summary(adf_DLNRFP)
# 进行 ARCH_LM 检验 
install.packages("lmtest")  # 如果尚未安装
library(lmtest)
library(FinTS)
ArchTest(sy$DLNRFP, lags = 1)
ArchTest(sy$DLNIFP, lags = 1)
ArchTest(sy$DLNHFP, lags = 1)
ArchTest(sy$DLNJFP, lags = 1)
# 对每个时间序列拟合ARMA模型，结果都为（0，0）
library(forecast)
DLNRFParma <- auto.arima(sy$DLNRFP)
DLNRFParma
DLNIFParma <- auto.arima(sy$DLNISP)
DLNIFParma
DLNHFParma <- auto.arima(sy$DLNHFP)
DLNHFParma
DLNJFParma <- auto.arima(sy$DLNJSP)
DLNJFParma
#拟合GARCH模型
library(rugarch)
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), distribution.model = "std")
DLNRFGarch <- ugarchfit(spec = spec, data = sy$DLNRFP)
DLNRFGarch
DLNIFGarch <- ugarchfit(spec = spec, data = sy$DLNIFP)
DLNIFGarch
DLNHFGarch <- ugarchfit(spec = spec, data = sy$DLNHFP)
DLNHFGarch
DLNJFGarch <- ugarchfit(spec = spec, data = sy$DLNJFP)
DLNJFGarch
#对DLNRFGarch，DLNISGarch，DLNHFGarch，DLNJSGarch模型进行残差检验
DLNRFGarchres <- residuals(DLNRFGarch)
DLNIFGarchres <- residuals(DLNIFGarch)
DLNHFGarchres <- residuals(DLNHFGarch)
DLNJFGarchres <- residuals(DLNJFGarch)
#残差序列的自相关检验
Box.test(DLNRFGarchres, lag = 12, type = "Ljung-Box")
Box.test(DLNIFGarchres, lag = 12, type = "Ljung-Box")
Box.test(DLNHFGarchres, lag = 12, type = "Ljung-Box")
Box.test(DLNJFGarchres, lag = 12, type = "Ljung-Box")
#残差序列的ARCH效应检验
ArchTest(DLNRFGarchres, lags = 1)
ArchTest(DLNISGarchres, lags = 1)
ArchTest(DLNHFGarchres, lags = 1)
ArchTest(DLNJSGarchres, lags = 1)
#标准化残差
DLNRFGarchstdres <- DLNRFGarchres/sqrt(DLNRFGarch@fit$sigma)
DLNIFGarchstdres <- DLNIFGarchres/sqrt(DLNIFGarch@fit$sigma)
DLNHFGarchstdres <- DLNHFGarchres/sqrt(DLNHFGarch@fit$sigma)
DLNJFGarchstdres <- DLNJSGarchres/sqrt(DLNJFGarch@fit$sigma)
#残差序列的自相关检验
Box.test(DLNRFGarchstdres, lag = 12, type = "Ljung-Box")
Box.test(DLNIFGarchstdres, lag = 12, type = "Ljung-Box")
Box.test(DLNHFGarchstdres, lag = 12, type = "Ljung-Box")
Box.test(DLNJFGarchstdres, lag = 12, type = "Ljung-Box")
# 两两配对组成矩阵
RIcopula_data <- cbind(DLNRFGarchstdres, DLNIFGarchstdres)
RHcopula_data <- cbind(DLNRFGarchstdres, DLNHFGarchstdres)
HIcopula_data <- cbind(DLNHFGarchstdres, DLNIFGarchstdres)
RJcopula_data <- cbind(DLNRFGarchstdres, DLNJFGarchstdres)
HJcopula_data <- cbind(DLNHFGarchstdres, DLNJFGarchstdres)
IJcopula_data <- cbind(DLNIFGarchstdres, DLNJFGarchstdres)
#概率函数运算
library(copula)
IJcopula_data_pobs <- pobs(IJcopula_data)
HJcopula_data_pobs <- pobs(HJcopula_data)
RJcopula_data_pobs <- pobs(RJcopula_data)
HIcopula_data_pobs <- pobs(HIcopula_data)
RHcopula_data_pobs <- pobs(RHcopula_data)
RIcopula_data_pobs <- pobs(RIcopula_data)
#转化为矩阵
IJcopula_data_pobs <- as.matrix(IJcopula_data_pobs)
HJcopula_data_pobs <- as.matrix(HJcopula_data_pobs)
RJcopula_data_pobs <- as.matrix(RJcopula_data_pobs)
HIcopula_data_pobs <- as.matrix(HIcopula_data_pobs)
RHcopula_data_pobs <- as.matrix(RHcopula_data_pobs)
RIcopula_data_pobs <- as.matrix(RIcopula_data_pobs)
#用ggplot2画出RIcopula_data_pobs、HJcopula_data_pobs、RJcopula_data_pobs、HIcopula_data_pobs、RHcopula_data_pobs、IJcopula_data_pobs的散点图
ggplot(data = as.data.frame(RIcopula_data_pobs), aes(x = RIcopula_data_pobs[, 1], y = RIcopula_data_pobs[, 2])) + geom_point() + ggtitle("RIcopula_data_pobs") + theme_minimal()
ggplot(data = as.data.frame(HJcopula_data_pobs), aes(x = HJcopula_data_pobs[, 1], y = HJcopula_data_pobs[, 2])) + geom_point() + ggtitle("HJcopula_data_pobs") + theme_minimal()
ggplot(data = as.data.frame(RJcopula_data_pobs), aes(x = RJcopula_data_pobs[, 1], y = RJcopula_data_pobs[, 2])) + geom_point() + ggtitle("RJcopula_data_pobs") + theme_minimal()
ggplot(data = as.data.frame(HIcopula_data_pobs), aes(x = HIcopula_data_pobs[, 1], y = HIcopula_data_pobs[, 2])) + geom_point() + ggtitle("HIcopula_data_pobs") + theme_minimal()
ggplot(data = as.data.frame(RHcopula_data_pobs), aes(x = RHcopula_data_pobs[, 1], y = RHcopula_data_pobs[, 2])) + geom_point() + ggtitle("RHcopula_data_pobs") + theme_minimal()
ggplot(data = as.data.frame(IJcopula_data_pobs), aes(x = IJcopula_data_pobs[, 1], y = IJcopula_data_pobs[, 2])) + geom_point() + ggtitle("IJcopula_data_pobs") + theme_minimal()
#拟合模型
#BiCopEst函数用于拟合二元联合分布的参数，其中family表示拟合模型，method表示拟合方法，mle表示极大似然估计
#拟合模型,1=高斯，尾部效应不明显，2=学生t，尾部效应明显，3=Clayton右尾，4=Frank对称，7=Joe，8=BB1
library(VineCopula)
IJfit_t1.gauss <- BiCopEst(IJcopula_data_pobs [, 1], IJcopula_data_pobs [, 2], family = 1, method = "mle")  # family = 1 -> Gaussian
plot(IJfit_t1.gauss)
IJfit_t1.student <- BiCopEst(IJcopula_data_pobs [, 1], IJcopula_data_pobs [, 2], family = 2, method = "mle")  # family = 2 -> Student
plot(IJfit_t1.student)
IJfit_t1.clayton <- BiCopEst(IJcopula_data_pobs [, 1], IJcopula_data_pobs [, 2], family = 3, method = "mle")  # family = 3 -> Clayton
plot(IJfit_t1.clayton)
IJfit_t1.frank <- BiCopEst(IJcopula_data_pobs [, 1], IJcopula_data_pobs [, 2], family = 4, method = "mle")  # family = 4 -> Frank
plot(IJfit_t1.frank)
IJfit_t1.plackett <- BiCopEst(IJcopula_data_pobs [, 1], IJcopula_data_pobs [, 2], family = 6, method = "mle")  # family = 6 -> Plackett
plot(IJfit_t1.plackett)
IJfit_t1.joe <- BiCopEst(IJcopula_data_pobs [, 1], IJcopula_data_pobs [, 2], family = 7, method = "mle")  # family = 7 -> Joe
plot(IJfit_t1.joe)
IJfit_t1.bb1 <- BiCopEst(IJcopula_data_pobs [, 1], IJcopula_data_pobs [, 2], family = 8, method = "mle")  # family = 8 -> BB1 
plot(IJfit_t1.bb1)

HJfit_t1.gauss <- BiCopEst(HJcopula_data_pobs [, 1], HJcopula_data_pobs [, 2], family = 1, method = "mle")  # family = 1 -> Gaussian
plot(HJfit_t1.gauss)
HJfit_t1.student <- BiCopEst(HJcopula_data_pobs [, 1], HJcopula_data_pobs [, 2], family = 2, method = "mle")  # family = 2 -> Student
plot(HJfit_t1.student)
HJfit_t1.clayton <- BiCopEst(HJcopula_data_pobs [, 1], HJcopula_data_pobs [, 2], family = 3, method = "mle")  # family = 3 -> Clayton
plot(HJfit_t1.clayton)
HJfit_t1.frank <- BiCopEst(HJcopula_data_pobs [, 1], HJcopula_data_pobs [, 2], family = 4, method = "mle")  # family = 4 -> Frank
plot(HJfit_t1.frank)
HJfit_t1.plackett <- BiCopEst(HJcopula_data_pobs [, 1], HJcopula_data_pobs [, 2], family = 6, method = "mle")  # family = 6 -> Plackett
plot(HJfit_t1.plackett)
HJfit_t1.joe <- BiCopEst(HJcopula_data_pobs [, 1], HJcopula_data_pobs [, 2], family = 7, method = "mle")  # family = 7 -> Joe
plot(HJfit_t1.joe)
HJfit_t1.bb1 <- BiCopEst(HJcopula_data_pobs [, 1], HJcopula_data_pobs [, 2], family = 8, method = "mle")  # family = 8 -> BB1
plot(HJfit_t1.bb1)

RJfit_t1.gauss <- BiCopEst(RJcopula_data_pobs [, 1], RJcopula_data_pobs [, 2], family = 1, method = "mle")  # family = 1 -> Gaussian
plot(RJfit_t1.gauss)
RJfit_t1.student <- BiCopEst(RJcopula_data_pobs [, 1], RJcopula_data_pobs [, 2], family = 2, method = "mle")  # family = 2 -> Student
plot(RJfit_t1.student)
RJfit_t1.clayton <- BiCopEst(RJcopula_data_pobs [, 1], RJcopula_data_pobs [, 2], family = 3, method = "mle")  # family = 3 -> Clayton
plot(RJfit_t1.clayton)
RJfit_t1.frank <- BiCopEst(RJcopula_data_pobs [, 1], RJcopula_data_pobs [, 2], family = 4, method = "mle")  # family = 4 -> Frank
plot(RJfit_t1.frank)
RJfit_t1.plackett <- BiCopEst(RJcopula_data_pobs [, 1], RJcopula_data_pobs [, 2], family = 6, method = "mle")  # family = 6 -> Plackett
plot(RJfit_t1.plackett)
RJfit_t1.joe <- BiCopEst(RJcopula_data_pobs [, 1], RJcopula_data_pobs [, 2], family = 7, method = "mle")  # family = 7 -> Joe
plot(RJfit_t1.joe)
RJfit_t1.bb1 <- BiCopEst(RJcopula_data_pobs [, 1], RJcopula_data_pobs [, 2], family = 8, method = "mle")  # family = 8 -> BB1
plot(RJfit_t1.bb1)

HIfit_t1.gauss <- BiCopEst(HIcopula_data_pobs [, 1], HIcopula_data_pobs [, 2], family = 1, method = "mle")  # family = 1 -> Gaussian
plot(HIfit_t1.gauss)
HIfit_t1.student <- BiCopEst(HIcopula_data_pobs [, 1], HIcopula_data_pobs [, 2], family = 2, method = "mle")  # family = 2 -> Student
plot(HIfit_t1.student)
HIfit_t1.clayton <- BiCopEst(HIcopula_data_pobs [, 1], HIcopula_data_pobs [, 2], family = 3, method = "mle")  # family = 3 -> Clayton
plot(HIfit_t1.clayton)
HIfit_t1.frank <- BiCopEst(HIcopula_data_pobs [, 1], HIcopula_data_pobs [, 2], family = 4, method = "mle")  # family = 4 -> Frank
plot(HIfit_t1.frank)
HIfit_t1.plackett <- BiCopEst(HIcopula_data_pobs [, 1], HIcopula_data_pobs [, 2], family = 6, method = "mle")  # family = 6 -> Plackett
plot(HIfit_t1.plackett)
HIfit_t1.joe <- BiCopEst(HIcopula_data_pobs [, 1], HIcopula_data_pobs [, 2], family = 7, method = "mle")  # family = 7 -> Joe
plot(HIfit_t1.joe)
HIfit_t1.bb1 <- BiCopEst(HIcopula_data_pobs [, 1], HIcopula_data_pobs [, 2], family = 8, method = "mle")  # family = 8 -> BB1
plot(HIfit_t1.bb1)

RHfit_t1.gauss <- BiCopEst(RHcopula_data_pobs [, 1], RHcopula_data_pobs [, 2], family = 1, method = "mle")  # family = 1 -> Gaussian
plot(RHfit_t1.gauss)
RHfit_t1.student <- BiCopEst(RHcopula_data_pobs [, 1], RHcopula_data_pobs [, 2], family = 2, method = "mle")  # family = 2 -> Student
plot(RHfit_t1.student)
RHfit_t1.clayton <- BiCopEst(RHcopula_data_pobs [, 1], RHcopula_data_pobs [, 2], family = 3, method = "mle")  # family = 3 -> Clayton
plot(RHfit_t1.clayton)
RHfit_t1.frank <- BiCopEst(RHcopula_data_pobs [, 1], RHcopula_data_pobs [, 2], family = 4, method = "mle")  # family = 4 -> Frank
plot(RHfit_t1.frank)
RHfit_t1.plackett <- BiCopEst(RHcopula_data_pobs [, 1], RHcopula_data_pobs [, 2], family = 6, method = "mle")  # family = 6 -> Plackett
plot(RHfit_t1.plackett)
RHfit_t1.joe <- BiCopEst(RHcopula_data_pobs [, 1], RHcopula_data_pobs [, 2], family = 7, method = "mle")  # family = 7 -> Joe
plot(RHfit_t1.joe)
RHfit_t1.bb1 <- BiCopEst(RHcopula_data_pobs [, 1], RHcopula_data_pobs [, 2], family = 8, method = "mle")  # family = 8 -> BB1
plot(RHfit_t1.bb1)

RIfit_t1.gauss <- BiCopEst(RIcopula_data_pobs [, 1], RIcopula_data_pobs [, 2], family = 1, method = "mle")  # family = 1 -> Gaussian
plot(RIfit_t1.gauss)
RIfit_t1.student <- BiCopEst(RIcopula_data_pobs [, 1], RIcopula_data_pobs [, 2], family = 2, method = "mle")  # family = 2 -> Student
plot(RIfit_t1.student)
RIfit_t1.clayton <- BiCopEst(RIcopula_data_pobs [, 1], RIcopula_data_pobs [, 2], family = 3, method = "mle")  # family = 3 -> Clayton
plot(RIfit_t1.clayton)
RIfit_t1.frank <- BiCopEst(RIcopula_data_pobs [, 1], RIcopula_data_pobs [, 2], family = 4, method = "mle")  # family = 4 -> Frank
plot(RIfit_t1.frank)
RIfit_t1.plackett <- BiCopEst(RIcopula_data_pobs [, 1], RIcopula_data_pobs [, 2], family = 6, method = "mle")  # family = 6 -> Plackett
plot(RIfit_t1.plackett)
RIfit_t1.joe <- BiCopEst(RIcopula_data_pobs [, 1], RIcopula_data_pobs [, 2], family = 7, method = "mle")  # family = 7 -> Joe
plot(RIfit_t1.joe)
RIfit_t1.bb1 <- BiCopEst(RIcopula_data_pobs [, 1], RIcopula_data_pobs [, 2], family = 8, method = "mle")  # family = 8 -> BB1
plot(RIfit_t1.bb1)
show(RIfit_t1.student)
library(VineCopula)
# 生成 2488 个样本
set.seed(123)  # 设置随机种子以便结果可复现,生成 2488 个样本
n_samples <- 2488
RIsimulated_data <- BiCopSim(n_samples, family = 2, 
                           par = RIfit_t1.student$par, 
                           par2 = RIfit_t1.student$par2)
RHsimulated_data <- BiCopSim(n_samples, family = 2, 
                             par = RHfit_t1.student$par, 
                             par2 = RHfit_t1.student$par2)
HIsimulated_data <- BiCopSim(n_samples, family = 2,
                             par = HIfit_t1.student$par, 
                             par2 = HIfit_t1.student$par2)
RJsimulated_data <- BiCopSim(n_samples, family = 2,
                             par = RJfit_t1.student$par, 
                             par2 = RJfit_t1.student$par2)
HJsimulated_data <- BiCopSim(n_samples, family = 2,
                             par = HJfit_t1.student$par, 
                             par2 = HJfit_t1.student$par2)
IJsimulated_data <- BiCopSim(n_samples, family = 2,
                             par = IJfit_t1.student$par, 
                             par2 = IJfit_t1.student$par2)
# 查看生成的样本
head(RIsimulated_data)
VaR_time_series <- rep(NA, length(sy$TIME)) 

window_size <- 70  # 设置滑动窗口的大小，例如250天
for (i in window_size:length(sy$TIME)){
# 在每个时间点，选择过去N天的数据
RIwindow_data <- RIsimulated_data[(i - window_size + 1):i, 1]
# 计算VaR，5%分位数
VaR_time_series[i] <- quantile(RIwindow_data, probs = 0.05)
}
RItime_series_data <- data.frame(time = sy$TIME, asset1 = RIsimulated_data[, 1], VaR = VaR_time_series)# 合并VaR到时间序列数据
for (i in window_size:length(sy$TIME)){
  # 在每个时间点，选择过去N天的数据
  RHwindow_data <- RHsimulated_data[(i - window_size + 1):i, 1]
  # 计算VaR，5%分位数
  VaR_time_series[i] <- quantile(RHwindow_data, probs = 0.05)
}
RHtime_series_data <- data.frame(time = sy$TIME, asset1 = RHsimulated_data[, 1], VaR = VaR_time_series)
for (i in window_size:length(sy$TIME)){
  # 在每个时间点，选择过去N天的数据
  HIwindow_data <- HIsimulated_data[(i - window_size + 1):i, 1]
  # 计算VaR，5%分位数
  VaR_time_series[i] <- quantile(HIwindow_data, probs = 0.05)
}
HItime_series_data <- data.frame(time = sy$TIME, asset1 = HIsimulated_data[, 1], VaR = VaR_time_series)
for (i in window_size:length(sy$TIME)){
  # 在每个时间点，选择过去N天的数据
  RJwindow_data <- RJsimulated_data[(i - window_size + 1):i, 1]
  # 计算VaR，5%分位数
  VaR_time_series[i] <- quantile(RJwindow_data, probs = 0.05)
}
RJtime_series_data <- data.frame(time = sy$TIME, asset1 = RJsimulated_data[, 1], VaR = VaR_time_series)
for (i in window_size:length(sy$TIME)){
  # 在每个时间点，选择过去N天的数据
  HJwindow_data <- HJsimulated_data[(i - window_size + 1):i, 1]
  # 计算VaR，5%分位数
  VaR_time_series[i] <- quantile(HJwindow_data, probs = 0.05)
}
HJtime_series_data <- data.frame(time = sy$TIME, asset1 = HJsimulated_data[, 1], VaR = VaR_time_series)
for (i in window_size:length(sy$TIME)){
  # 在每个时间点，选择过去N天的数据
  IJwindow_data <- IJsimulated_data[(i - window_size + 1):i, 1]
  # 计算VaR，5%分位数
  VaR_time_series[i] <- quantile(IJwindow_data, probs = 0.05)
}
IJtime_series_data <- data.frame(time = sy$TIME, asset1 = IJsimulated_data[, 1], VaR = VaR_time_series)
# 绘制IJtime_series_data,HJtime_series_data,RJtime_series_data,HItime_series_data,RHtime_series_data,RItime_series_data的VaR时序图,用不同颜色标出
library(ggplot2)

# 将所有时间序列数据合并到一个长格式数据框中
all_time_series_data <- data.frame(
  time = RItime_series_data$time,  # 假设所有数据的时间列相同
  VaR_IJ = IJtime_series_data$VaR,
  VaR_HJ = HJtime_series_data$VaR,
  VaR_RJ = RJtime_series_data$VaR,
  VaR_HI = HItime_series_data$VaR,
  VaR_RH = RHtime_series_data$VaR,
  VaR_RI = RItime_series_data$VaR
)

# 转换为长格式
long_time_series <- reshape2::melt(all_time_series_data, id.vars = "time",
                                   variable.name = "Asset", value.name = "VaR")

# 绘制图表
ggplot(long_time_series, aes(x = time, y = VaR, color = Asset)) +
  geom_line(size = 0.3) +  # 设置线宽
  geom_hline(yintercept = 0.15, color = "red", linetype = "dashed", size = 0.5) +  # 添加水平红色虚线
  geom_vline(xintercept = as.Date(c("2019-11-30", "2022-10-31")), color = "black", linetype = "longdash", size = 0.3) +  # 添加黑色垂直虚线
  geom_vline(xintercept = as.Date("2015-12-30"), color = "black", linetype = "dashed", size = 0.3) +  # 添加黑色垂直虚线
  labs(title = "VaR Time Series for Multiple Assets (2014-2024)",  # 标题
       x = "Date",                                               # X轴标签
       y = "VaR Value") +                                        # Y轴标签
  scale_color_manual(values = c("VaR_IJ" = "red",                # 自定义颜色
                                "VaR_HJ" = "blue",
                                "VaR_RJ" = "green",
                                "VaR_HI" = "purple",
                                "VaR_RH" = "orange",
                                "VaR_RI" = "black")) +
  scale_x_date(date_breaks = "1 year",                           # X轴日期间隔
               date_labels = "%Y") +                             # 显示年份格式
  theme_minimal() +                                              # 简洁主题
  theme(legend.title = element_blank(),                          # 去掉图例标题
        legend.position = "bottom")                              # 将图例放在底部


library(dplyr)

# 过滤数据
long_time_series_HJ <- long_time_series %>%
  filter(Asset == "VaR_HJ")

# 绘图
ggplot(long_time_series_HJ, aes(x = time, y = VaR, color = Asset)) +
  geom_line(size = 0.3) +  # 设置线宽
  labs(title = "VaR Time Series for VaR_HJ (2014-2024)",  # 标题
       x = "Date",                                           # X轴标签
       y = "VaR Value") +                                    # Y轴标签
  scale_color_manual(values = c("VaR_HJ" = "blue")) +  # 只为 VaR_HJ 指定颜色
  scale_x_date(date_breaks = "1 year",                  # X轴日期间隔
               date_labels = "%Y") +                    # 显示年份格式
  theme_minimal() +                                     # 简洁主题
  theme(legend.title = element_blank(),                 # 去掉图例标题
        legend.position = "bottom")
# 过滤数据
long_time_series_IJ <- long_time_series %>%
  filter(Asset == "VaR_IJ")
# 绘图
ggplot(long_time_series_IJ, aes(x = time, y = VaR, color = Asset)) +
  geom_line(size = 0.3) +  # 设置线宽
  labs(title = "VaR Time Series for VaR_IJ (2014-2024)",  # 标题
       x = "Date",                                           # X轴标签
       y = "VaR Value") +                                    # Y轴标签
  scale_color_manual(values = c("VaR_IJ" = "red")) +  # 只为 VaR_IJ 指定颜色
  scale_x_date(date_breaks = "1 year",                  # X轴日期间隔
               date_labels = "%Y") +                    # 显示年份格式
  theme_minimal() +                                     # 简洁主题
  theme(legend.title = element_blank(),                 # 去掉图例标题
        legend.position = "bottom")
# 过滤数据
long_time_series_RJ <- long_time_series %>%
  filter(Asset == "VaR_RJ")
# 绘图
ggplot(long_time_series_RJ, aes(x = time, y = VaR, color = Asset)) +
  geom_line(size = 0.3) +  # 设置线宽
  labs(title = "VaR Time Series for VaR_RJ (2014-2024)",  # 标题
       x = "Date",                                           # X轴标签
       y = "VaR Value") +                                    # Y轴标签
  scale_color_manual(values = c("VaR_RJ" = "green")) +  # 只为 VaR_RJ 指定颜色
  scale_x_date(date_breaks = "1 year",                  # X轴日期间隔
               date_labels = "%Y") +                    # 显示年份格式
  theme_minimal() +                                     # 简洁主题
  theme(legend.title = element_blank(),                 # 去掉图例标题
        legend.position = "bottom")
# 过滤数据
long_time_series_HI <- long_time_series %>%
  filter(Asset == "VaR_HI")
# 绘图
ggplot(long_time_series_HI, aes(x = time, y = VaR, color = Asset)) +
  geom_line(size = 0.3) +  # 设置线宽
  labs(title = "VaR Time Series for VaR_HI (2014-2024)",  # 标题
       x = "Date",                                           # X轴标签
       y = "VaR Value") +                                    # Y轴标签
  scale_color_manual(values = c("VaR_HI" = "purple")) +  # 只为 VaR_HI 指定颜色
  scale_x_date(date_breaks = "1 year",                  # X轴日期间隔
               date_labels = "%Y") +                    # 显示年份格式
  theme_minimal() +                                     # 简洁主题
  theme(legend.title = element_blank(),                 # 去掉图例标题
        legend.position = "bottom")
# 过滤数据
long_time_series_RH <- long_time_series %>%
  filter(Asset == "VaR_RH")
# 绘图
ggplot(long_time_series_RH, aes(x = time, y = VaR, color = Asset)) +
  geom_line(size = 0.3) +  # 设置线宽
  labs(title = "VaR Time Series for VaR_RH (2014-2024)",  # 标题
       x = "Date",                                           # X轴标签
       y = "VaR Value") +                                    # Y轴标签
  scale_color_manual(values = c("VaR_RH" = "orange")) +  # 只为 VaR_RH 指定颜色
  scale_x_date(date_breaks = "1 year",                  # X轴日期间隔
               date_labels = "%Y") +                    # 显示年份格式
  theme_minimal() +                                     # 简洁主题
  theme(legend.title = element_blank(),                 # 去掉图例标题
        legend.position = "bottom")
# 过滤数据
long_time_series_RI <- long_time_series %>%
  filter(Asset == "VaR_RI")
# 绘图
ggplot(long_time_series_RI, aes(x = time, y = VaR, color = Asset)) +
  geom_line(size = 0.3) +  # 设置线宽
  labs(title = "VaR Time Series for VaR_RI (2014-2024)",  # 标题
       x = "Date",                                           # X轴标签
       y = "VaR Value") +                                    # Y轴标签
  scale_color_manual(values = c("VaR_RI" = "black")) +  # 只为 VaR_RI 指定颜色
  scale_x_date(date_breaks = "1 year",                  # X轴日期间隔
               date_labels = "%Y") +                    # 显示年份格式
  theme_minimal() +                                     # 简洁主题
  theme(legend.title = element_blank(),                 # 去掉图例标题
        legend.position = "bottom")













