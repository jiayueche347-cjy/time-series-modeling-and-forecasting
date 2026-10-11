
#1
#1.1-1.2
dat <- read.csv("C:/yqw/DTU/26spring/02417 Time Series Analysis, Spring 2026/Assignment1/DST_BIL54.csv")
str(dat)

# See the help
?strftime
dat$time <- as.POSIXct(paste0(dat$time,"-01"), format ="%Y-%m-%d", tz="UTC")
dat$time
class(dat$time)

## Year to month for each of them
dat$year <- 1900 + as.POSIXlt(dat$time)$year + as.POSIXlt(dat$time)$mon / 12

## Make the output variable a floating point (i.e.\ decimal number)
dat$total <- as.numeric(dat$total) / 1E6

## Divide intro train and test set
teststart <- as.POSIXct("2024-01-01", tz="UTC")
dattrain <- dat[dat$time < teststart, ]
dattest <- dat[dat$time >= teststart, ]

plot(dattrain$year, dattrain$total,
     type="l", lwd=2,
     xlab="Time (year + month/12)",
     ylab="Total vehicles (millions)",
     main="Training data")
grid()


2
2.1
x3 <- dattrain$year[1:3]
y3 <- dattrain$total[1:3]

# build design matrix X (first column 1s, second column x)
X3 <- cbind(1, x3)

# print with max 3 decimals
y3_print <- round(y3, 3)
X3_print <- round(X3, 3)

y3_print
X3_print

3
3.1-3.6
## ========== 3.1 Fit OLS model (estimate theta1, theta2) ==========
ols_fit <- lm(total ~ year, data = dattrain)   # total in millions, year = x
summary(ols_fit)

## Explain calculation (for report): OLS minimizes sum of squared residuals
## theta_hat = (X'X)^{-1} X' y
X <- model.matrix(ols_fit)   # design matrix for training set
y <- dattrain$total
#手算 theta_hat <- solve(t(X) %*% X) %*% t(X) %*% y   # (theta1_hat, theta2_hat)
#theta_hat


## ========== 3.2 Parameter estimates + standard errors + plot mean line ==========
#只采纳下面这一步就行，R中自带的算theta
coef(ols_fit)                 # theta1_hat, theta2_hat
summary(ols_fit)$coefficients # includes standard errors, t-values, p-values

# Plot observations as points and estimated mean as a line
plot(dattrain$year, dattrain$total,
     pch = 16, cex = 0.6,
     xlab = "Time (year + month/12)",
     ylab = "Total vehicles (millions)",
     main = "OLS fit on training set")
abline(ols_fit, lwd = 2)
grid()

## ========== 3.3 Forecast for test set + prediction intervals table ==========
# 95% prediction intervals for 2024
pred_test <- predict(ols_fit, newdata = dattest,
                     interval = "prediction", level = 0.95)

# Combine into a table
forecast_table <- data.frame(
  time = dattest$time,
  x = dattest$year,
  y_obs = dattest$total,
  y_pred = pred_test[, "fit"],
  PI_low = pred_test[, "lwr"],
  PI_high = pred_test[, "upr"]
)

# Print table (you can also write to CSV)
print(forecast_table)
# write.csv(forecast_table, "OLS_forecast_table.csv", row.names = FALSE)


## ========== 3.4 Improved Plot ==========
plot(dattrain$year, dattrain$total,
     pch = 16, cex = 0.7,
     col = "darkgrey",   # 训练数据灰色
     xlab = "Time (year + month/12)",
     ylab = "Total vehicles (millions)",
     main = "OLS: training fit + 2024 forecast with PI",
     xlim = range(dat$year),
     ylim = range(c(dattrain$total, forecast_table$PI_low, forecast_table$PI_high))
)

grid(col = "lightgrey")

# ---- Fitted mean (training) ----
lines(dattrain$year, fitted(ols_fit),
      col = "blue", lwd = 2.5)

# ---- Forecast (test) ----
points(dattest$year, forecast_table$y_pred,
       pch = 16, col = "red", cex = 0.8)

lines(dattest$year, forecast_table$y_pred,
      col = "red", lwd = 2.5)

# ---- Prediction intervals ----
lines(dattest$year, forecast_table$PI_low,
      col = "darkred", lty = 2, lwd = 1.8)

lines(dattest$year, forecast_table$PI_high,
      col = "darkred", lty = 2, lwd = 1.8)

legend("topleft",
       legend = c("Training data",
                  "Fitted mean (train)",
                  "Forecast (test)",
                  "95% prediction interval"),
       col = c("darkgrey", "blue", "red", "darkred"),
       pch = c(16, NA, 16, NA),
       lty = c(NA, 1, 1, 2),
       lwd = c(NA, 2.5, 2.5, 1.8),
       bty = "n")

## ========== 3.5 Comment on forecast quality (compute simple metrics) ==========
# RMSE on test set
rmse_test <- sqrt(mean((forecast_table$y_obs - forecast_table$y_pred)^2, na.rm = TRUE))
rmse_test

# Coverage: how many observed points fall inside the 95% prediction interval?
coverage <- mean(forecast_table$y_obs >= forecast_table$PI_low &
                   forecast_table$y_obs <= forecast_table$PI_high, na.rm = TRUE)
coverage


res <- resid(ols_fit)
fitted_vals <- fitted(ols_fit)

# Residuals over time (structure / mean stability)
plot(dattrain$year, res, type="l",
     xlab="Time (year + month/12)", ylab="Residual",
     main="Residuals over time")
abline(h=0, lty=2); grid()

# ACF (independence / white noise check)
acf(res, main="ACF of residuals")

# Optional: heteroskedasticity / nonlinearity check
plot(fitted_vals, res, pch=16, cex=0.6,
     xlab="Fitted values", ylab="Residual",
     main="Residuals vs fitted")
abline(h=0, lty=2); grid()

4
4.1-4.6

############################################################
## Q4: WLS - local linear trend model
############################################################

lambda <- 0.9

# Ensure training data are sorted by time (important!)
dattrain <- dattrain[order(dattrain$year), ]
dattest  <- dattest[order(dattest$year), ]

n <- nrow(dattrain)

############################################################
## 4.2 Lambda-weights (most recent gets weight 1)
## w_t = lambda^(N - t)  (older -> smaller)
############################################################
w <- lambda^rev(0:(n-1))  # oldest: lambda^(n-1), newest: lambda^0 = 1

# Plot weights vs time
plot(dattrain$year, w, type="b", pch=16,
     xlab="Time (year + month/12)", ylab="Weight",
     main=paste0("Lambda-weights over time (lambda = ", lambda, ")"))
grid()

# Which time point has the highest weight?
which_max <- which.max(w)
dattrain$year[which_max]   # time of max weight (should be the latest time)

############################################################
## 4.3 Sum of weights, compare to OLS
############################################################
sum_w_wls <- sum(w)
sum_w_ols <- n  # OLS: equal weights 1 for each observation
sum_w_wls
sum_w_ols

############################################################
## 4.4 Fit WLS (weighted least squares)
############################################################
wls_fit <- lm(total ~ year, data = dattrain, weights = w)
summary(wls_fit)

coef(wls_fit)  # theta1_hat, theta2_hat (WLS)


############################################################
## 4.5 Forecast next 12 months (test set) + compare OLS vs WLS
############################################################
pred_ols <- predict(ols_fit, newdata = dattest, interval="prediction", level=0.95)
pred_wls <- predict(wls_fit, newdata = dattest, interval="prediction", level=0.95)

# Build comparison table
compare_table <- data.frame(
  time  = dattest$time,
  year  = dattest$year,
  y_obs = dattest$total,
  
  ols_fit = pred_ols[, "fit"],
  ols_lwr = pred_ols[, "lwr"],
  ols_upr = pred_ols[, "upr"],
  
  wls_fit = pred_wls[, "fit"],
  wls_lwr = pred_wls[, "lwr"],
  wls_upr = pred_wls[, "upr"]
)
print(compare_table)

# Plot training + both forecasts (clean colors)
plot(dattrain$year, dattrain$total,
     pch=16, cex=0.7, col="darkgrey",
     xlab="Time (year + month/12)", ylab="Total vehicles (millions)",
     main=paste0("OLS vs WLS forecasts (lambda = ", lambda, ")"),
     xlim=range(dat$year),
     ylim=range(c(dattrain$total, compare_table$ols_lwr, compare_table$ols_upr,
                  compare_table$wls_lwr, compare_table$wls_upr))
)
grid(col="lightgrey")

# Fitted mean line over training
lines(dattrain$year, fitted(ols_fit), col="blue", lwd=2.2)   # OLS fitted (train)
lines(dattrain$year, fitted(wls_fit), col="purple", lwd=2.2) # WLS fitted (train)

# Forecast lines over test
lines(dattest$year, compare_table$ols_fit, col="blue", lwd=2.2)
lines(dattest$year, compare_table$wls_fit, col="purple", lwd=2.2)

# Prediction intervals (optional: keep light)
lines(dattest$year, compare_table$ols_lwr, col="blue", lty=2)
lines(dattest$year, compare_table$ols_upr, col="blue", lty=2)

lines(dattest$year, compare_table$wls_lwr, col="purple", lty=2)
lines(dattest$year, compare_table$wls_upr, col="purple", lty=2)

legend("topleft",
       legend=c("Training data", "OLS (mean)", "WLS (mean)", "OLS 95% PI", "WLS 95% PI"),
       col=c("darkgrey","blue","purple","blue","purple"),
       pch=c(16, NA, NA, NA, NA),
       lty=c(NA, 1, 1, 2, 2),
       lwd=c(NA, 2.2, 2.2, 1, 1),
       bty="n")

## 4.6 Zoomed comparison (magnified differences)
############################################################

lambda_values <- c(0.99, 0.8, 0.7, 0.6)
cols <- c("black","red","darkgreen","blue")
ltys <- c(1,2,3,4)   # 不同线型
lwds <- c(3,2,2,2)   # λ=0.99 更粗

# ---- 计算所有预测 ----
pred_matrix <- matrix(NA, nrow=nrow(dattest), ncol=length(lambda_values))

for (i in 1:length(lambda_values)) {
  lam <- lambda_values[i]
  n <- nrow(dattrain)
  w <- lam^rev(0:(n-1))
  fit <- lm(total ~ year, data=dattrain, weights=w)
  pred_matrix[,i] <- predict(fit, newdata=dattest)
}

# ---- 放大 y 轴范围（只围绕预测）----
y_min <- min(pred_matrix) - 0.002
y_max <- max(pred_matrix) + 0.002

# ---- 开始画图 ----
plot(dattest$year, pred_matrix[,1],
     type="n",
     ylim=c(y_min, y_max),
     xlab="Time (2024 only)",
     ylab="Total vehicles (millions)",
     main="Zoomed WLS forecasts for different lambda values")

grid(col="lightgrey")

# ---- 画预测线 ----
for (i in 1:length(lambda_values)) {
  lines(dattest$year, pred_matrix[,i],
        col=cols[i],
        lty=ltys[i],
        lwd=lwds[i])
}

# ---- 加真实观测点 ----
points(dattest$year, dattest$total,
       pch=16, col="black", cex=1.2)

# ---- 图例 ----
legend("topleft",
       legend=c("λ=0.99","λ=0.8","λ=0.7","λ=0.6","Observed 2024"),
       col=c(cols,"black"),
       lty=c(ltys,NA),
       lwd=c(lwds,NA),
       pch=c(NA,NA,NA,NA,16),
       bty="n")


## ---- 5.2 RLS for t = 1..3 (no forgetting, lambda = 1) ----

# 1) Use training set only (as assignment says: up to 2023-Dec)
# If your file has more columns, keep only what's needed
# Here we assume dat has a column named "total"
# If it is named differently, replace dat$total accordingly.
y_all <- dat$total

# 2) Construct time variable x exactly as in 1.1:
# 2018-Jan -> 2018, 2018-Feb -> 2018 + 1/12, ...
N <- length(y_all)
x_all <- 2018 + (0:(N-1))/12

# 3) Take first 3 time points
tmax <- 3
y <- y_all[1:tmax]
x <- x_all[1:tmax]

# 4) Initialize R0 and theta0 (given in assignment)
R <- matrix(c(0.1, 0,
              0,   0.1), nrow = 2, byrow = TRUE)
theta <- matrix(c(0, 0), nrow = 2)

# Store results
theta_hist <- matrix(NA, nrow = tmax, ncol = 2)
colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")

# 5) RLS recursion
for (t in 1:tmax) {
  phi <- matrix(c(1, x[t]), nrow = 2)         # phi_t = [1; x_t]
  denom <- as.numeric(1 + t(phi) %*% R %*% phi)
  K <- (R %*% phi) / denom                     # gain
  
  yhat <- as.numeric(t(phi) %*% theta)         # one-step prediction
  e <- as.numeric(y[t] - yhat)                 # prediction error
  
  theta <- theta + K * e                       # update theta
  R <- R - (R %*% phi %*% t(phi) %*% R) / denom # update R
  
  theta_hist[t, ] <- as.numeric(theta)
}

# 6) Print results for hand-in
theta_hist


## ---- 5.3: Compare RLS (lambda=1) vs OLS on training set ----

# 1) Ensure time is Date
dat$time <- as.Date(dat$time)   # works if it's like "2018-01-01"

# 2) Check sizes and missing values
cat("Rows in dat:", nrow(dat), "\n")
cat("Rows in train:", nrow(dattrain), "\n")
cat("NA in train$total:", sum(is.na(dattrain$total)), "\n")
cat("NA in train$year:",  sum(is.na(dattrain$year)), "\n")

# 3) Keep only complete cases for OLS
train_cc <- dattrain[complete.cases(dattrain[, c("total", "year")]), ]

cat("Rows in train_cc:", nrow(train_cc), "\n")

# 4) OLS
ols_fit <- lm(total ~ year, data = train_cc)
theta_ols <- coef(ols_fit)
theta_ols

# 5) RLS function (lambda = 1)
rls_run <- function(y, x, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  for (t in seq_along(y)) {
    phi <- matrix(c(1, x[t]), nrow = 2)
    denom <- as.numeric(1 + t(phi) %*% R %*% phi)
    K <- (R %*% phi) / denom
    e <- as.numeric(y[t] - t(phi) %*% theta)
    theta <- theta + K * e
    R <- R - (R %*% phi %*% t(phi) %*% R) / denom
  }
  as.numeric(theta)
}

# 6) Run RLS on training set
y <- train_cc$total
x <- train_cc$year

theta_rls_base <- rls_run(y, x, R0 = diag(0.1, 2), theta0 = c(0, 0))

# 7) Compare
out <- rbind(
  OLS = c(theta_ols[1], theta_ols[2]),
  RLS_base = theta_rls_base
)
colnames(out) <- c("theta1_intercept", "theta2_slope")
out

diff <- theta_rls_base - theta_ols
diff


## ---- 5.4 RLS with forgetting (lambda = 0.7 and 0.99) ----

# RLS with forgetting function
rls_forgetting <- function(y, x, lambda, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  
  theta_hist <- matrix(NA, nrow = length(y), ncol = 2)
  colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")
  
  for (t in seq_along(y)) {
    phi <- matrix(c(1, x[t]), nrow = 2)
    
    # NOTE: forgetting factor modification
    denom <- as.numeric(lambda + t(phi) %*% R %*% phi)
    K <- (R %*% phi) / denom
    
    e <- as.numeric(y[t] - t(phi) %*% theta)
    theta <- theta + K * e
    
    R <- (R - (R %*% phi %*% t(phi) %*% R) / denom) / lambda
    
    theta_hist[t, ] <- as.numeric(theta)
  }
  
  return(theta_hist)
}

# Run for lambda = 0.7 and 0.99
theta_07  <- rls_forgetting(y, x, lambda = 0.7)
theta_099 <- rls_forgetting(y, x, lambda = 0.99)

# Remove burn-in period (recommended)
burn <- 5
idx <- burn:length(y)

# -----------------------------
# Plot theta1 (intercept)
# -----------------------------
plot(train_cc$time[idx], theta_07[idx, 1],
     type = "l",
     col = "red",
     lwd = 2,
     lty = 1,
     xlab = "Time",
     ylab = expression(hat(theta)[1,t]),
     main = expression("RLS with forgetting: " * hat(theta)[1,t]))

lines(train_cc$time[idx], theta_099[idx, 1],
      col = "blue",
      lwd = 2,
      lty = 2)

legend("topleft",
       legend = c("lambda = 0.7", "lambda = 0.99"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1, 2),
       bty = "n")

# -----------------------------
# Plot theta2 (slope)
# -----------------------------
plot(train_cc$time[idx], theta_07[idx, 2],
     type = "l",
     col = "red",
     lwd = 2,
     lty = 1,
     xlab = "Time",
     ylab = expression(hat(theta)[2,t]),
     main = expression("RLS with forgetting: " * hat(theta)[2,t]))

lines(train_cc$time[idx], theta_099[idx, 2],
      col = "blue",
      lwd = 2,
      lty = 2)

legend("topleft",
       legend = c("lambda = 0.7", "lambda = 0.99"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1, 2),
       bty = "n")

# Print final parameter values
cat("Final theta (lambda=0.7): ", theta_07[length(y), ], "\n")
cat("Final theta (lambda=0.99):", theta_099[length(y), ], "\n")


## ---- 5.5 One-step ahead residuals for RLS with forgetting ----

# Function: run RLS-with-forgetting and return:
# - theta history (theta_t after processing y_t)
# - one-step predictions yhat_t|t-1
# - one-step residuals ehat_t|t-1 = yhat_t|t-1 - y_t
rls_one_step <- function(y, x, lambda, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  
  n <- length(y)
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  
  theta_hist <- matrix(NA, nrow = n, ncol = 2)
  colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")
  
  yhat <- rep(NA, n)   # yhat[t] = yhat_{t|t-1}
  ehat <- rep(NA, n)   # ehat[t] = yhat_{t|t-1} - y[t]
  
  for (t in 1:n) {
    phi_t <- matrix(c(1, x[t]), nrow = 2)
    
    # one-step prediction for y_t given info up to t-1:
    # use current theta (which is theta_{t-1} at loop start)
    yhat[t] <- as.numeric(t(phi_t) %*% theta)
    ehat[t] <- yhat[t] - y[t]
    
    # RLS with forgetting update using observation (y_t, phi_t)
    denom <- as.numeric(lambda + t(phi_t) %*% R %*% phi_t)
    K <- (R %*% phi_t) / denom
    innovation <- as.numeric(y[t] - t(phi_t) %*% theta)  # (y_t - prediction)
    
    theta <- theta + K * innovation
    R <- (R - (R %*% phi_t %*% t(phi_t) %*% R) / denom) / lambda
    
    theta_hist[t, ] <- as.numeric(theta)
  }
  
  list(theta_hist = theta_hist, yhat = yhat, ehat = ehat)
}

# Run for lambda = 0.7 and 0.99
res_07  <- rls_one_step(y, x, lambda = 0.7)
res_099 <- rls_one_step(y, x, lambda = 0.99)

# Plot residuals for t = 5..N (burn-in removal as assignment suggests)
burn <- 5
idx <- burn:length(y)

par(mar = c(5, 6, 4, 2))

plot(train_cc$time[idx], res_07$ehat[idx], type = "l",
     col = "red",
     lwd = 2,
     lty = 1,
     xlab = "Time",
     ylab = expression(hat(epsilon)[t*","*t-1]),
     main = "One-step ahead residuals (prediction - observation)")

lines(train_cc$time[idx], res_099$ehat[idx],
      col = "blue",
      lwd = 2,
      lty = 2)

abline(h = 0, lty = 3)

legend("topleft",
       legend = c("lambda = 0.7", "lambda = 0.99"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1,2),
       bty = "n")

# summarize residual variability
cat("Residual SD (lambda=0.7):  ", sd(res_07$ehat[idx], na.rm = TRUE), "\n")
cat("Residual SD (lambda=0.99): ", sd(res_099$ehat[idx], na.rm = TRUE), "\n")
cat("Residual mean (lambda=0.7):  ", mean(res_07$ehat[idx], na.rm = TRUE), "\n")
cat("Residual mean (lambda=0.99): ", mean(res_099$ehat[idx], na.rm = TRUE), "\n")


## ---- 5.6 Optimize lambda for horizons k = 1..12 ----

# --- Safety checks ---
stopifnot(exists("train_cc"), exists("y"), exists("x"))
N <- length(y)

# 1) RLS with forgetting: return theta history (theta_t after seeing y_t)
rls_theta_hist <- function(y, x, lambda, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  n <- length(y)
  
  theta_hist <- matrix(NA, nrow = n, ncol = 2)
  colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")
  
  for (t in 1:n) {
    phi <- matrix(c(1, x[t]), nrow = 2)
    
    denom <- as.numeric(lambda + t(phi) %*% R %*% phi)
    K <- (R %*% phi) / denom
    
    innovation <- as.numeric(y[t] - t(phi) %*% theta)
    theta <- theta + K * innovation
    
    R <- (R - (R %*% phi %*% t(phi) %*% R) / denom) / lambda
    
    theta_hist[t, ] <- as.numeric(theta)
  }
  theta_hist
}

# 2) Compute RMSE_k for k = 1..K for a given lambda
rmse_by_horizon <- function(y, x, lambda, K = 12, burn = 5) {
  n <- length(y)
  th <- rls_theta_hist(y, x, lambda = lambda)
  
  rmse <- rep(NA, K)
  
  for (k in 1:K) {
    # valid t: need t (info time) >= burn, and t+k <= n
    t_idx <- burn:(n - k)
    
    # predictions: yhat_{t+k|t} = [1, x_{t+k}] %*% theta_t
    yhat <- th[t_idx, 1] + th[t_idx, 2] * x[t_idx + k]
    eps  <- yhat - y[t_idx + k]
    
    rmse[k] <- sqrt(mean(eps^2, na.rm = TRUE))
  }
  rmse
}

# 3) Lambda grid and horizons
lambda_grid <- seq(0.50, 0.99, by = 0.01)
Kmax <- 12
burn <- 5

# 4) RMSE matrix: rows = lambdas, cols = horizons k
RMSE <- matrix(NA, nrow = length(lambda_grid), ncol = Kmax)
colnames(RMSE) <- paste0("k=", 1:Kmax)

for (i in seq_along(lambda_grid)) {
  lam <- lambda_grid[i]
  RMSE[i, ] <- rmse_by_horizon(y, x, lambda = lam, K = Kmax, burn = burn)
}

# 5) Plot RMSE vs lambda (all horizons in one plot)
matplot(lambda_grid, RMSE, type = "l", lty = 1, lwd = 2,
        xlab = expression(lambda),
        ylab = "RMSE",
        main = "RMSE vs lambda for k-step ahead predictions (k = 1..12)")
legend("topright", legend = colnames(RMSE), lty = 1, lwd = 2, cex = 0.8, bty = "n")

# 6) Find best lambda for each horizon
best_idx <- apply(RMSE, 2, which.min)
best_lambda <- lambda_grid[best_idx]
best_rmse <- RMSE[cbind(best_idx, 1:Kmax)]

best_table <- data.frame(
  horizon_k = 1:Kmax,
  best_lambda = best_lambda,
  best_RMSE = best_rmse
)
best_table



#1
#1.1-1.2
dat <- read.csv("C:/yqw/DTU/26spring/02417 Time Series Analysis, Spring 2026/Assignment1/DST_BIL54.csv")
str(dat)

# See the help
?strftime
dat$time <- as.POSIXct(paste0(dat$time,"-01"), format ="%Y-%m-%d", tz="UTC")
dat$time
class(dat$time)

## Year to month for each of them
dat$year <- 1900 + as.POSIXlt(dat$time)$year + as.POSIXlt(dat$time)$mon / 12

## Make the output variable a floating point (i.e.\ decimal number)
dat$total <- as.numeric(dat$total) / 1E6

## Divide intro train and test set
teststart <- as.POSIXct("2024-01-01", tz="UTC")
dattrain <- dat[dat$time < teststart, ]
dattest <- dat[dat$time >= teststart, ]

plot(dattrain$year, dattrain$total,
     type="l", lwd=2,
     xlab="Time (year + month/12)",
     ylab="Total vehicles (millions)",
     main="Training data")
grid()


2
2.1
x3 <- dattrain$year[1:3]
y3 <- dattrain$total[1:3]

# build design matrix X (first column 1s, second column x)
X3 <- cbind(1, x3)

# print with max 3 decimals
y3_print <- round(y3, 3)
X3_print <- round(X3, 3)

y3_print
X3_print

3
3.1-3.6
## ========== 3.1 Fit OLS model (estimate theta1, theta2) ==========
ols_fit <- lm(total ~ year, data = dattrain)   # total in millions, year = x
summary(ols_fit)

## Explain calculation (for report): OLS minimizes sum of squared residuals
## theta_hat = (X'X)^{-1} X' y
X <- model.matrix(ols_fit)   # design matrix for training set
y <- dattrain$total
#手算 theta_hat <- solve(t(X) %*% X) %*% t(X) %*% y   # (theta1_hat, theta2_hat)
#theta_hat


## ========== 3.2 Parameter estimates + standard errors + plot mean line ==========
#只采纳下面这一步就行，R中自带的算theta
coef(ols_fit)                 # theta1_hat, theta2_hat
summary(ols_fit)$coefficients # includes standard errors, t-values, p-values

# Plot observations as points and estimated mean as a line
plot(dattrain$year, dattrain$total,
     pch = 16, cex = 0.6,
     xlab = "Time (year + month/12)",
     ylab = "Total vehicles (millions)",
     main = "OLS fit on training set")
abline(ols_fit, lwd = 2)
grid()

## ========== 3.3 Forecast for test set + prediction intervals table ==========
# 95% prediction intervals for 2024
pred_test <- predict(ols_fit, newdata = dattest,
                     interval = "prediction", level = 0.95)

# Combine into a table
forecast_table <- data.frame(
  time = dattest$time,
  x = dattest$year,
  y_obs = dattest$total,
  y_pred = pred_test[, "fit"],
  PI_low = pred_test[, "lwr"],
  PI_high = pred_test[, "upr"]
)

# Print table (you can also write to CSV)
print(forecast_table)
# write.csv(forecast_table, "OLS_forecast_table.csv", row.names = FALSE)


## ========== 3.4 Improved Plot ==========
plot(dattrain$year, dattrain$total,
     pch = 16, cex = 0.7,
     col = "darkgrey",   # 训练数据灰色
     xlab = "Time (year + month/12)",
     ylab = "Total vehicles (millions)",
     main = "OLS: training fit + 2024 forecast with PI",
     xlim = range(dat$year),
     ylim = range(c(dattrain$total, forecast_table$PI_low, forecast_table$PI_high))
)

grid(col = "lightgrey")

# ---- Fitted mean (training) ----
lines(dattrain$year, fitted(ols_fit),
      col = "blue", lwd = 2.5)

# ---- Forecast (test) ----
points(dattest$year, forecast_table$y_pred,
       pch = 16, col = "red", cex = 0.8)

lines(dattest$year, forecast_table$y_pred,
      col = "red", lwd = 2.5)

# ---- Prediction intervals ----
lines(dattest$year, forecast_table$PI_low,
      col = "darkred", lty = 2, lwd = 1.8)

lines(dattest$year, forecast_table$PI_high,
      col = "darkred", lty = 2, lwd = 1.8)

legend("topleft",
       legend = c("Training data",
                  "Fitted mean (train)",
                  "Forecast (test)",
                  "95% prediction interval"),
       col = c("darkgrey", "blue", "red", "darkred"),
       pch = c(16, NA, 16, NA),
       lty = c(NA, 1, 1, 2),
       lwd = c(NA, 2.5, 2.5, 1.8),
       bty = "n")

## ========== 3.5 Comment on forecast quality (compute simple metrics) ==========
# RMSE on test set
rmse_test <- sqrt(mean((forecast_table$y_obs - forecast_table$y_pred)^2, na.rm = TRUE))
rmse_test

# Coverage: how many observed points fall inside the 95% prediction interval?
coverage <- mean(forecast_table$y_obs >= forecast_table$PI_low &
                   forecast_table$y_obs <= forecast_table$PI_high, na.rm = TRUE)
coverage


res <- resid(ols_fit)
fitted_vals <- fitted(ols_fit)

# Residuals over time (structure / mean stability)
plot(dattrain$year, res, type="l",
     xlab="Time (year + month/12)", ylab="Residual",
     main="Residuals over time")
abline(h=0, lty=2); grid()

# ACF (independence / white noise check)
acf(res, main="ACF of residuals")

# Optional: heteroskedasticity / nonlinearity check
plot(fitted_vals, res, pch=16, cex=0.6,
     xlab="Fitted values", ylab="Residual",
     main="Residuals vs fitted")
abline(h=0, lty=2); grid()

4
4.1-4.6

############################################################
## Q4: WLS - local linear trend model
############################################################

lambda <- 0.9

# Ensure training data are sorted by time (important!)
dattrain <- dattrain[order(dattrain$year), ]
dattest  <- dattest[order(dattest$year), ]

n <- nrow(dattrain)

############################################################
## 4.2 Lambda-weights (most recent gets weight 1)
## w_t = lambda^(N - t)  (older -> smaller)
############################################################
w <- lambda^rev(0:(n-1))  # oldest: lambda^(n-1), newest: lambda^0 = 1

# Plot weights vs time
plot(dattrain$year, w, type="b", pch=16,
     xlab="Time (year + month/12)", ylab="Weight",
     main=paste0("Lambda-weights over time (lambda = ", lambda, ")"))
grid()

# Which time point has the highest weight?
which_max <- which.max(w)
dattrain$year[which_max]   # time of max weight (should be the latest time)

############################################################
## 4.3 Sum of weights, compare to OLS
############################################################
sum_w_wls <- sum(w)
sum_w_ols <- n  # OLS: equal weights 1 for each observation
sum_w_wls
sum_w_ols

############################################################
## 4.4 Fit WLS (weighted least squares)
############################################################
wls_fit <- lm(total ~ year, data = dattrain, weights = w)
summary(wls_fit)

coef(wls_fit)  # theta1_hat, theta2_hat (WLS)


############################################################
## 4.5 Forecast next 12 months (test set) + compare OLS vs WLS
############################################################
pred_ols <- predict(ols_fit, newdata = dattest, interval="prediction", level=0.95)
pred_wls <- predict(wls_fit, newdata = dattest, interval="prediction", level=0.95)

# Build comparison table
compare_table <- data.frame(
  time  = dattest$time,
  year  = dattest$year,
  y_obs = dattest$total,
  
  ols_fit = pred_ols[, "fit"],
  ols_lwr = pred_ols[, "lwr"],
  ols_upr = pred_ols[, "upr"],
  
  wls_fit = pred_wls[, "fit"],
  wls_lwr = pred_wls[, "lwr"],
  wls_upr = pred_wls[, "upr"]
)
print(compare_table)

# Plot training + both forecasts (clean colors)
plot(dattrain$year, dattrain$total,
     pch=16, cex=0.7, col="darkgrey",
     xlab="Time (year + month/12)", ylab="Total vehicles (millions)",
     main=paste0("OLS vs WLS forecasts (lambda = ", lambda, ")"),
     xlim=range(dat$year),
     ylim=range(c(dattrain$total, compare_table$ols_lwr, compare_table$ols_upr,
                  compare_table$wls_lwr, compare_table$wls_upr))
)
grid(col="lightgrey")

# Fitted mean line over training
lines(dattrain$year, fitted(ols_fit), col="blue", lwd=2.2)   # OLS fitted (train)
lines(dattrain$year, fitted(wls_fit), col="purple", lwd=2.2) # WLS fitted (train)

# Forecast lines over test
lines(dattest$year, compare_table$ols_fit, col="blue", lwd=2.2)
lines(dattest$year, compare_table$wls_fit, col="purple", lwd=2.2)

# Prediction intervals (optional: keep light)
lines(dattest$year, compare_table$ols_lwr, col="blue", lty=2)
lines(dattest$year, compare_table$ols_upr, col="blue", lty=2)

lines(dattest$year, compare_table$wls_lwr, col="purple", lty=2)
lines(dattest$year, compare_table$wls_upr, col="purple", lty=2)

legend("topleft",
       legend=c("Training data", "OLS (mean)", "WLS (mean)", "OLS 95% PI", "WLS 95% PI"),
       col=c("darkgrey","blue","purple","blue","purple"),
       pch=c(16, NA, NA, NA, NA),
       lty=c(NA, 1, 1, 2, 2),
       lwd=c(NA, 2.2, 2.2, 1, 1),
       bty="n")

## 4.6 Zoomed comparison (magnified differences)
############################################################

lambda_values <- c(0.99, 0.8, 0.7, 0.6)
cols <- c("black","red","darkgreen","blue")
ltys <- c(1,2,3,4)   # 不同线型
lwds <- c(3,2,2,2)   # λ=0.99 更粗

# ---- 计算所有预测 ----
pred_matrix <- matrix(NA, nrow=nrow(dattest), ncol=length(lambda_values))

for (i in 1:length(lambda_values)) {
  lam <- lambda_values[i]
  n <- nrow(dattrain)
  w <- lam^rev(0:(n-1))
  fit <- lm(total ~ year, data=dattrain, weights=w)
  pred_matrix[,i] <- predict(fit, newdata=dattest)
}

# ---- 放大 y 轴范围（只围绕预测）----
y_min <- min(pred_matrix) - 0.002
y_max <- max(pred_matrix) + 0.002

# ---- 开始画图 ----
plot(dattest$year, pred_matrix[,1],
     type="n",
     ylim=c(y_min, y_max),
     xlab="Time (2024 only)",
     ylab="Total vehicles (millions)",
     main="Zoomed WLS forecasts for different lambda values")

grid(col="lightgrey")

# ---- 画预测线 ----
for (i in 1:length(lambda_values)) {
  lines(dattest$year, pred_matrix[,i],
        col=cols[i],
        lty=ltys[i],
        lwd=lwds[i])
}

# ---- 加真实观测点 ----
points(dattest$year, dattest$total,
       pch=16, col="black", cex=1.2)

# ---- 图例 ----
legend("topleft",
       legend=c("λ=0.99","λ=0.8","λ=0.7","λ=0.6","Observed 2024"),
       col=c(cols,"black"),
       lty=c(ltys,NA),
       lwd=c(lwds,NA),
       pch=c(NA,NA,NA,NA,16),
       bty="n")


## ---- 5.2 RLS for t = 1..3 (no forgetting, lambda = 1) ----

# 1) Use training set only (as assignment says: up to 2023-Dec)
# If your file has more columns, keep only what's needed
# Here we assume dat has a column named "total"
# If it is named differently, replace dat$total accordingly.
y_all <- dat$total

# 2) Construct time variable x exactly as in 1.1:
# 2018-Jan -> 2018, 2018-Feb -> 2018 + 1/12, ...
N <- length(y_all)
x_all <- 2018 + (0:(N-1))/12

# 3) Take first 3 time points
tmax <- 3
y <- y_all[1:tmax]
x <- x_all[1:tmax]

# 4) Initialize R0 and theta0 (given in assignment)
R <- matrix(c(0.1, 0,
              0,   0.1), nrow = 2, byrow = TRUE)
theta <- matrix(c(0, 0), nrow = 2)

# Store results
theta_hist <- matrix(NA, nrow = tmax, ncol = 2)
colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")

# 5) RLS recursion
for (t in 1:tmax) {
  phi <- matrix(c(1, x[t]), nrow = 2)         # phi_t = [1; x_t]
  denom <- as.numeric(1 + t(phi) %*% R %*% phi)
  K <- (R %*% phi) / denom                     # gain
  
  yhat <- as.numeric(t(phi) %*% theta)         # one-step prediction
  e <- as.numeric(y[t] - yhat)                 # prediction error
  
  theta <- theta + K * e                       # update theta
  R <- R - (R %*% phi %*% t(phi) %*% R) / denom # update R
  
  theta_hist[t, ] <- as.numeric(theta)
}

# 6) Print results for hand-in
theta_hist


## ---- 5.3: Compare RLS (lambda=1) vs OLS on training set ----

# 1) Ensure time is Date
dat$time <- as.Date(dat$time)   # works if it's like "2018-01-01"

# 2) Check sizes and missing values
cat("Rows in dat:", nrow(dat), "\n")
cat("Rows in train:", nrow(dattrain), "\n")
cat("NA in train$total:", sum(is.na(dattrain$total)), "\n")
cat("NA in train$year:",  sum(is.na(dattrain$year)), "\n")

# 3) Keep only complete cases for OLS
train_cc <- dattrain[complete.cases(dattrain[, c("total", "year")]), ]

cat("Rows in train_cc:", nrow(train_cc), "\n")

# 4) OLS
ols_fit <- lm(total ~ year, data = train_cc)
theta_ols <- coef(ols_fit)
theta_ols

# 5) RLS function (lambda = 1)
rls_run <- function(y, x, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  for (t in seq_along(y)) {
    phi <- matrix(c(1, x[t]), nrow = 2)
    denom <- as.numeric(1 + t(phi) %*% R %*% phi)
    K <- (R %*% phi) / denom
    e <- as.numeric(y[t] - t(phi) %*% theta)
    theta <- theta + K * e
    R <- R - (R %*% phi %*% t(phi) %*% R) / denom
  }
  as.numeric(theta)
}

# 6) Run RLS on training set
y <- train_cc$total
x <- train_cc$year

theta_rls_base <- rls_run(y, x, R0 = diag(0.1, 2), theta0 = c(0, 0))

# 7) Compare
out <- rbind(
  OLS = c(theta_ols[1], theta_ols[2]),
  RLS_base = theta_rls_base
)
colnames(out) <- c("theta1_intercept", "theta2_slope")
out

diff <- theta_rls_base - theta_ols
diff


## ---- 5.4 RLS with forgetting (lambda = 0.7 and 0.99) ----

# RLS with forgetting function
rls_forgetting <- function(y, x, lambda, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  
  theta_hist <- matrix(NA, nrow = length(y), ncol = 2)
  colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")
  
  for (t in seq_along(y)) {
    phi <- matrix(c(1, x[t]), nrow = 2)
    
    # NOTE: forgetting factor modification
    denom <- as.numeric(lambda + t(phi) %*% R %*% phi)
    K <- (R %*% phi) / denom
    
    e <- as.numeric(y[t] - t(phi) %*% theta)
    theta <- theta + K * e
    
    R <- (R - (R %*% phi %*% t(phi) %*% R) / denom) / lambda
    
    theta_hist[t, ] <- as.numeric(theta)
  }
  
  return(theta_hist)
}

# Run for lambda = 0.7 and 0.99
theta_07  <- rls_forgetting(y, x, lambda = 0.7)
theta_099 <- rls_forgetting(y, x, lambda = 0.99)

# Remove burn-in period (recommended)
burn <- 5
idx <- burn:length(y)

# -----------------------------
# Plot theta1 (intercept)
# -----------------------------
plot(train_cc$time[idx], theta_07[idx, 1],
     type = "l",
     col = "red",
     lwd = 2,
     lty = 1,
     xlab = "Time",
     ylab = expression(hat(theta)[1,t]),
     main = expression("RLS with forgetting: " * hat(theta)[1,t]))

lines(train_cc$time[idx], theta_099[idx, 1],
      col = "blue",
      lwd = 2,
      lty = 2)

legend("topleft",
       legend = c("lambda = 0.7", "lambda = 0.99"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1, 2),
       bty = "n")

# -----------------------------
# Plot theta2 (slope)
# -----------------------------
plot(train_cc$time[idx], theta_07[idx, 2],
     type = "l",
     col = "red",
     lwd = 2,
     lty = 1,
     xlab = "Time",
     ylab = expression(hat(theta)[2,t]),
     main = expression("RLS with forgetting: " * hat(theta)[2,t]))

lines(train_cc$time[idx], theta_099[idx, 2],
      col = "blue",
      lwd = 2,
      lty = 2)

legend("topleft",
       legend = c("lambda = 0.7", "lambda = 0.99"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1, 2),
       bty = "n")

# Print final parameter values
cat("Final theta (lambda=0.7): ", theta_07[length(y), ], "\n")
cat("Final theta (lambda=0.99):", theta_099[length(y), ], "\n")


## ---- 5.5 One-step ahead residuals for RLS with forgetting ----

# Function: run RLS-with-forgetting and return:
# - theta history (theta_t after processing y_t)
# - one-step predictions yhat_t|t-1
# - one-step residuals ehat_t|t-1 = yhat_t|t-1 - y_t
rls_one_step <- function(y, x, lambda, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  
  n <- length(y)
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  
  theta_hist <- matrix(NA, nrow = n, ncol = 2)
  colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")
  
  yhat <- rep(NA, n)   # yhat[t] = yhat_{t|t-1}
  ehat <- rep(NA, n)   # ehat[t] = yhat_{t|t-1} - y[t]
  
  for (t in 1:n) {
    phi_t <- matrix(c(1, x[t]), nrow = 2)
    
    # one-step prediction for y_t given info up to t-1:
    # use current theta (which is theta_{t-1} at loop start)
    yhat[t] <- as.numeric(t(phi_t) %*% theta)
    ehat[t] <- yhat[t] - y[t]
    
    # RLS with forgetting update using observation (y_t, phi_t)
    denom <- as.numeric(lambda + t(phi_t) %*% R %*% phi_t)
    K <- (R %*% phi_t) / denom
    innovation <- as.numeric(y[t] - t(phi_t) %*% theta)  # (y_t - prediction)
    
    theta <- theta + K * innovation
    R <- (R - (R %*% phi_t %*% t(phi_t) %*% R) / denom) / lambda
    
    theta_hist[t, ] <- as.numeric(theta)
  }
  
  list(theta_hist = theta_hist, yhat = yhat, ehat = ehat)
}

# Run for lambda = 0.7 and 0.99
res_07  <- rls_one_step(y, x, lambda = 0.7)
res_099 <- rls_one_step(y, x, lambda = 0.99)

# Plot residuals for t = 5..N (burn-in removal as assignment suggests)
burn <- 5
idx <- burn:length(y)

par(mar = c(5, 6, 4, 2))

plot(train_cc$time[idx], res_07$ehat[idx], type = "l",
     col = "red",
     lwd = 2,
     lty = 1,
     xlab = "Time",
     ylab = expression(hat(epsilon)[t*","*t-1]),
     main = "One-step ahead residuals (prediction - observation)")

lines(train_cc$time[idx], res_099$ehat[idx],
      col = "blue",
      lwd = 2,
      lty = 2)

abline(h = 0, lty = 3)

legend("topleft",
       legend = c("lambda = 0.7", "lambda = 0.99"),
       col = c("red", "blue"),
       lwd = 2,
       lty = c(1,2),
       bty = "n")

# summarize residual variability
cat("Residual SD (lambda=0.7):  ", sd(res_07$ehat[idx], na.rm = TRUE), "\n")
cat("Residual SD (lambda=0.99): ", sd(res_099$ehat[idx], na.rm = TRUE), "\n")
cat("Residual mean (lambda=0.7):  ", mean(res_07$ehat[idx], na.rm = TRUE), "\n")
cat("Residual mean (lambda=0.99): ", mean(res_099$ehat[idx], na.rm = TRUE), "\n")


## ---- 5.6 Optimize lambda for horizons k = 1..12 ----

# --- Safety checks ---
stopifnot(exists("train_cc"), exists("y"), exists("x"))
N <- length(y)

# 1) RLS with forgetting: return theta history (theta_t after seeing y_t)
rls_theta_hist <- function(y, x, lambda, R0 = diag(0.1, 2), theta0 = c(0, 0)) {
  R <- R0
  theta <- matrix(theta0, nrow = 2)
  n <- length(y)
  
  theta_hist <- matrix(NA, nrow = n, ncol = 2)
  colnames(theta_hist) <- c("theta1_intercept", "theta2_slope")
  
  for (t in 1:n) {
    phi <- matrix(c(1, x[t]), nrow = 2)
    
    denom <- as.numeric(lambda + t(phi) %*% R %*% phi)
    K <- (R %*% phi) / denom
    
    innovation <- as.numeric(y[t] - t(phi) %*% theta)
    theta <- theta + K * innovation
    
    R <- (R - (R %*% phi %*% t(phi) %*% R) / denom) / lambda
    
    theta_hist[t, ] <- as.numeric(theta)
  }
  theta_hist
}

# 2) Compute RMSE_k for k = 1..K for a given lambda
rmse_by_horizon <- function(y, x, lambda, K = 12, burn = 5) {
  n <- length(y)
  th <- rls_theta_hist(y, x, lambda = lambda)
  
  rmse <- rep(NA, K)
  
  for (k in 1:K) {
    # valid t: need t (info time) >= burn, and t+k <= n
    t_idx <- burn:(n - k)
    
    # predictions: yhat_{t+k|t} = [1, x_{t+k}] %*% theta_t
    yhat <- th[t_idx, 1] + th[t_idx, 2] * x[t_idx + k]
    eps  <- yhat - y[t_idx + k]
    
    rmse[k] <- sqrt(mean(eps^2, na.rm = TRUE))
  }
  rmse
}

# 3) Lambda grid and horizons
lambda_grid <- seq(0.50, 0.99, by = 0.01)
Kmax <- 12
burn <- 5

# 4) RMSE matrix: rows = lambdas, cols = horizons k
RMSE <- matrix(NA, nrow = length(lambda_grid), ncol = Kmax)
colnames(RMSE) <- paste0("k=", 1:Kmax)

for (i in seq_along(lambda_grid)) {
  lam <- lambda_grid[i]
  RMSE[i, ] <- rmse_by_horizon(y, x, lambda = lam, K = Kmax, burn = burn)
}

# 5) Plot RMSE vs lambda (all horizons in one plot)
matplot(lambda_grid, RMSE, type = "l", lty = 1, lwd = 2,
        xlab = expression(lambda),
        ylab = "RMSE",
        main = "RMSE vs lambda for k-step ahead predictions (k = 1..12)")
legend("topright", legend = colnames(RMSE), lty = 1, lwd = 2, cex = 0.8, bty = "n")

# 6) Find best lambda for each horizon
best_idx <- apply(RMSE, 2, which.min)
best_lambda <- lambda_grid[best_idx]
best_rmse <- RMSE[cbind(best_idx, 1:Kmax)]

best_table <- data.frame(
  horizon_k = 1:Kmax,
  best_lambda = best_lambda,
  best_RMSE = best_rmse
)
best_table

## ---- 5.7 Predictions of the test set using RLS + compare to OLS/WLS ----

# 1) Make sure time types are consistent (safe)
# (Your dattrain/dattest time are POSIXct; this makes plotting easier)
if (!inherits(dattrain$time, "Date")) dattrain$time <- as.Date(dattrain$time)
if (!inherits(dattest$time,  "Date")) dattest$time  <- as.Date(dattest$time)

# 2) Choose lambda for RLS (based on your 5.6 conclusion)
lambda_rls <- 0.55   # change if you want, e.g., 0.50~0.60

# 3) Run RLS on TRAINING set only, get theta_N
y_tr <- train_cc$total
x_tr <- train_cc$year

theta_hist_rls <- rls_forgetting(y_tr, x_tr, lambda = lambda_rls)
thetaN_rls <- theta_hist_rls[nrow(theta_hist_rls), ]  # (theta1, theta2)

cat("RLS final theta (lambda=", lambda_rls, "): ",
    thetaN_rls[1], thetaN_rls[2], "\n")

# 4) Forecast test set (2024)
x_te <- dattest$year
y_te <- dattest$total

yhat_rls <- as.numeric(thetaN_rls[1] + thetaN_rls[2] * x_te)

# 5) Ensure OLS and WLS fits exist (from Q3 and Q4); refit if not
if (!exists("ols_fit")) {
  ols_fit <- lm(total ~ year, data = train_cc)
}
if (!exists("wls_fit")) {
  # If wls_fit not in env, rebuild with Q4 lambda (default 0.9)
  lambda_wls <- 0.9
  n_tr <- nrow(dattrain)
  w_tr <- lambda_wls^rev(0:(n_tr-1))
  wls_fit <- lm(total ~ year, data = dattrain, weights = w_tr)
}

yhat_ols <- as.numeric(predict(ols_fit, newdata = dattest))
yhat_wls <- as.numeric(predict(wls_fit, newdata = dattest))

# 6) RMSE on test set
rmse <- function(pred, obs) sqrt(mean((pred - obs)^2, na.rm = TRUE))

rmse_ols <- rmse(yhat_ols, y_te)
rmse_wls <- rmse(yhat_wls, y_te)
rmse_rls <- rmse(yhat_rls, y_te)

rmse_table_57 <- data.frame(
  Model = c("OLS", "WLS (lambda=0.9)", paste0("RLS (lambda=", lambda_rls, ")")),
  RMSE  = c(rmse_ols, rmse_wls, rmse_rls)
)
print(rmse_table_57)

# 7) Build a neat forecast table for hand-in
forecast_57 <- data.frame(
  time = dattest$time,
  year = dattest$year,
  y_obs = y_te,
  yhat_OLS = yhat_ols,
  yhat_WLS = yhat_wls,
  yhat_RLS = yhat_rls
)
print(forecast_57)

# 8) Plot: training + test obs + forecasts --
plot(dattrain$time, dattrain$total, type = "l",
     col = "gray60", lwd = 2,
     xlab = "Time", ylab = "Total vehicles (millions)",
     main = "Forecasts on test set (2024): OLS vs WLS vs RLS",
     xlim = range(dat$time),
     ylim = range(c(dattrain$total, y_te, yhat_ols, yhat_wls, yhat_rls), na.rm = TRUE))

grid(col = "lightgrey")

# Test observations as points
points(dattest$time, y_te, pch = 16, cex = 0.9)

# Forecast lines
lines(dattest$time, yhat_ols, col = "blue", lwd = 2, lty = 2)
lines(dattest$time, yhat_wls, col = "darkgreen", lwd = 2, lty = 3)
lines(dattest$time, yhat_rls, col = "red", lwd = 2, lty = 1)

legend("topleft",
       legend = c("Train (truth)", "Test (truth)",
                  "OLS forecast", "WLS forecast (λ=0.9)",
                  paste0("RLS forecast (λ=", lambda_rls, ")")),
       col = c("gray60", "black", "blue", "darkgreen", "red"),
       pch = c(NA, 16, NA, NA, NA),
       lty = c(1, NA, 2, 3, 1),
       lwd = c(2, NA, 2, 2, 2),
       bty = "n")
