# 设置工作目录并加载数据
setwd("T:/506")
load("datasets.RData")

# 确保nlmodel数据集已加载
if("nlmodel" %in% ls()) {
  nlmodel <- get("nlmodel")
}

#b 定义对数似然函数
logLikelihood <- function(params, y, x) {
  theta1 <- params[1]
  theta2 <- params[2]
  sigma <- params[3]
  mu <- theta1 * x / (theta2 + x)
  # 计算正态分布的对数似然
  n <- length(y)
  -0.5 * n * log(2 * pi * sigma^2) - sum((y - mu)^2) / (2 * sigma^2)
}

#c 定义负对数似然函数
mylike <- function(params) {
  y <- nlmodel$y
  x <- nlmodel$x
  -logLikelihood(params, y, x)
}

# 使用optim进行参数优化
initial_params <- c(0.5, 0.3, 1)  # 设置初始参数
optim_results <- optim(par = initial_params, fn = mylike, method = "BFGS", hessian = TRUE)
# 打印优化结果
print(optim_results$par)
print(optim_results$value)

#d 绘图显示数据和模型拟合
plot(nlmodel$x, nlmodel$y, main = "Scatter Plot with Model Fit", xlab = "x", ylab = "y", pch = 19, col = "black")
curve((optim_results$par[1] * x) / (optim_results$par[2] + x), from=min(nlmodel$x), to=max(nlmodel$x), add = TRUE, col = "red", lwd = 2)

print(optim_results$par)

initial_params <- c(0.5, 0.3, 1)  # 设置初始参数
optim_results <- optim(par = initial_params, fn = mylike, method = "BFGS", hessian = TRUE)

# 检查优化是否成功收敛
if (optim_results$convergence == 0) {
  print("Optimization converged successfully!")
  print("Optimal parameters:")
  print(optim_results$par)
  print("Minimum value of the function:")
  print(optim_results$value)
  
  # 绘图显示数据和模型拟合
  plot(nlmodel$x, nlmodel$y, main = "Scatter Plot with Model Fit", xlab = "x", ylab = "y", pch = 19, col = "black")
  curve((optim_results$par[1] * x) / (optim_results$par[2] + x), from=min(nlmodel$x), to=max(nlmodel$x), add = TRUE, col = "red", lwd = 2)
} else {
  print("Optimization failed to converge.")
  print("Convergence message:")
  print(optim_results$message)
}

#e # 确保已经完成了参数优化



