rm(list=ls())


load("datasets.RData")

# 确保nlmodel数据集已加载
if("nlmodel" %in% ls()) {
  nlmodel <- get("nlmodel")
}

# b 定义对数似然函数
logLikelihood <- function(params, y, x) {
  theta1 <- params[1]
  theta2 <- params[2]
  sigma <- params[3]
  mu <- theta1 * x / (theta2 + x)
  n <- length(y)
  -0.5 * n * log(2 * pi * sigma^2) - sum((y - mu)^2) / (2 * sigma^2)
}


# c 定义负对数似然函数
mylike <- function(params) {
  y <- nlmodel$y 
  x <- nlmodel$x  
  -logLikelihood(params, y, x)
}


# d
# 设置初始参数
initial_params <- c(mean(nlmodel$y) / mean(nlmodel$x), mean(nlmodel$x), sd(nlmodel$y))

# 使用nlm函数
opt_result <- nlm(mylike, initial_params)


theta1 <- opt_result$estimate[1]
theta2 <- opt_result$estimate[2]
sigma <- opt_result$estimate[3]

cat("Optimized Parameters:\n")
cat("theta1 =", theta1, "\n")
cat("theta2 =", theta2, "\n")
cat("sigma =", sigma, "\n")


# 按 x 排序数据
sorted_data <- nlmodel[order(nlmodel$x), ]

# 计算拟合值 mu
mu <- theta1 * sorted_data$x / (theta2 + sorted_data$x)

# 绘制实际数据与拟合曲线
plot(sorted_data$x, sorted_data$y, main="Scatter Plot with Model Fit", xlab="X", ylab="Y", col="black", pch=19)
lines(sorted_data$x, mu, col="red", lwd=2)  # 绘制红色的拟合曲线


dev.copy(png, "Scatter Plot with Model Fit.png")
dev.off() 

# e

# nlm函数没有hessian矩阵，用optim
optim_result <- optim(par = initial_params, fn = mylike, hessian = TRUE)

# 查看优化结果
cat("Optimized Parameters:\n")
cat("theta1 =", optim_result$par[1], "\n")
cat("theta2 =", optim_result$par[2], "\n")
cat("sigma =", optim_result$par[3], "\n")

# 计算标准误差
hessian_matrix <- optim_result$hessian
standard_errors <- sqrt(diag(solve(hessian_matrix)))

cat("Standard Errors:\n")
cat("Standard Error of theta1:", standard_errors[1], "\n")
cat("Standard Error of theta2:", standard_errors[2], "\n")
cat("Standard Error of sigma:", standard_errors[3], "\n")


conf_interval_theta1 <- c(optim_result$par[1] - 1.96 * standard_errors[1], optim_result$par[1] + 1.96 * standard_errors[1])
conf_interval_theta2 <- c(optim_result$par[2] - 1.96 * standard_errors[2], optim_result$par[2] + 1.96 * standard_errors[2])
conf_interval_sigma <- c(optim_result$par[3] - 1.96 * standard_errors[3], optim_result$par[3] + 1.96 * standard_errors[3])

cat("95% Confidence Interval for theta1: [", conf_interval_theta1[1], ", ", conf_interval_theta1[2], "]\n")
cat("95% Confidence Interval for theta2: [", conf_interval_theta2[1], ", ", conf_interval_theta2[2], "]\n")
cat("95% Confidence Interval for sigma: [", conf_interval_sigma[1], ", ", conf_interval_sigma[2], "]\n")



# f

# 获取优化后的theta2和标准误差
theta2_hat <- optim_result$par[2]  # 估计的theta2
se_theta2 <- standard_errors[2]     # theta2的标准误差

# 计算z统计量
z_stat <- (theta2_hat - 0.08) / se_theta2

# 计算p值
p_value <- 2 * (1 - pnorm(abs(z_stat)))  # 双尾检验

# 输出z统计量和p值
cat("z-statistic:", z_stat, "\n")
cat("p-value:", p_value, "\n")

# 检验结果
if (p_value < 0.05) {
  cat("Reject the null hypothesis: theta2 is significantly different from 0.08\n")
} else {
  cat("Fail to reject the null hypothesis: no significant difference from 0.08\n")
}


#g

# 获取优化后的参数
theta1 <- opt_result$estimate[1]
theta2 <- opt_result$estimate[2]
sigma <- opt_result$estimate[3]

# 按 x 排序数据
sorted_data <- nlmodel[order(nlmodel$x), ]

# 计算拟合值 mu
mu <- theta1 * sorted_data$x / (theta2 + sorted_data$x)

# 残差计算
residuals <- sorted_data$y - mu
sigma_est <- sqrt(sum(residuals^2) / length(sorted_data$y))  # 估计的sigma

# 计算预测标准误差
SE_pred <- sigma_est * sqrt(1 + 1 / length(sorted_data$y) + (sorted_data$x - mean(sorted_data$x))^2 / sum((sorted_data$x - mean(sorted_data$x))^2))

# 计算95%预测区间
lower_bound <- mu - 1.96 * SE_pred
upper_bound <- mu + 1.96 * SE_pred

# 绘制数据点
plot(sorted_data$x, sorted_data$y, main="Scatter Plot with Model Fit and 95% Prediction Interval", xlab="X", ylab="Y", col="black", pch=19)

# 绘制拟合曲线
lines(sorted_data$x, mu, col="red", lwd=2)

# 绘制95%预测区间
polygon(c(sorted_data$x, rev(sorted_data$x)), c(lower_bound, rev(upper_bound)), col=rgb(1, 0, 0, 0.2), border=NA)


dev.copy(png, "scatter_plot_with_model_fit_and_95_prediction_interval.png")
dev.off() 







