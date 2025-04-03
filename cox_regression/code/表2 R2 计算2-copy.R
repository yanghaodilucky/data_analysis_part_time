


library(rms)
library(Hmisc)
library(survival)

rm(list=ls())



data_4 <- read.csv("data/predictrisk4y.csv")
data_20 <- read.csv("data/predictrisk20y.csv")

data_4$ranked_PI_with_race <- rank(data_4$LP_With_Race)
data_4$ranked_PI_without_race <- rank(data_4$LP_Without_Race)

data_4$rankit_with_race <- qnorm((data_4$ranked_PI_with_race - 0.5) / nrow(data_4))
data_4$rankit_without_race <- qnorm((data_4$ranked_PI_without_race - 0.5) / nrow(data_4))


kappa <- sqrt(8 / pi)

data_4$scaled_rankit_with_race <- data_4$rankit_with_race / kappa
data_4$scaled_rankit_without_race <- data_4$rankit_without_race / kappa


cox_model_with_race_4 <- coxph(Surv(CombindT_m, 终点事件) ~ scaled_rankit_with_race, data = data_4)
cox_model_without_race_4 <- coxph(Surv(CombindT_m, 终点事件) ~ scaled_rankit_without_race, data = data_4)

D_with_race_4 <- summary(cox_model_with_race_4)$coefficients[1, 1]  # 回归系数D (含种族模型)
D_without_race_4 <- summary(cox_model_without_race_4)$coefficients[1, 1]  # 回归系数D (不含种族模型)


sigma2 <- pi^2 / 6

R2D_with_race_4 <- D_with_race_4^2 / (D_with_race_4^2 + sigma2)
R2D_without_race_4 <- D_without_race_4^2 / (D_without_race_4^2 + sigma2)

cat("4年 R²D (with race):", round(R2D_with_race_4 * 100, 2), "%\n")
cat("4年 R²D (without race):", round(R2D_without_race_4 * 100, 2), "%\n")


data_20$ranked_PI_with_race <- rank(data_20$LP_With_Race)
data_20$ranked_PI_without_race <- rank(data_20$LP_Without_Race)

data_20$rankit_with_race <- qnorm((data_20$ranked_PI_with_race - 0.5) / nrow(data_20))
data_20$rankit_without_race <- qnorm((data_20$ranked_PI_without_race - 0.5) / nrow(data_20))


kappa <- sqrt(8 / pi)

data_20$scaled_rankit_with_race <- data_20$rankit_with_race / kappa
data_20$scaled_rankit_without_race <- data_20$rankit_without_race / kappa


cox_model_with_race_20 <- coxph(Surv(CombindT_m, 终点事件) ~ scaled_rankit_with_race, data = data_20)
cox_model_without_race_20 <- coxph(Surv(CombindT_m, 终点事件) ~ scaled_rankit_without_race, data = data_20)

D_with_race_20 <- summary(cox_model_with_race_20)$coefficients[1, 1]  # 回归系数D (含种族模型)
D_without_race_20 <- summary(cox_model_without_race_20)$coefficients[1, 1]  # 回归系数D (不含种族模型)


sigma2 <- pi^2 / 6

R2D_with_race_20 <- D_with_race_20^2 / (D_with_race_20^2 + sigma2)
R2D_without_race_20 <- D_without_race_20^2 / (D_without_race_20^2 + sigma2)

cat("20年 R²D (with race):", round(R2D_with_race_20 * 100, 2), "%\n")
cat("20年 R²D (without race):", round(R2D_without_race_20 * 100, 2), "%\n")

