## -----------------------------------------------------------------------------
x <- rnorm(130)  
y <- sin(cos(x))/x  

## -----------------------------------------------------------------------------
opar <- par()

par(col.axis="red", mar=c(4, 4, 2.5, 0.25))

plot(x, y, main = "The frist required example",xlab=" Random variable", ylab=" Dependent variable",
xlim=c(-2, 2), ylim=c(-2, 2), pch=22, col="yellow", bg="green",bty="l", tcl=-.25, las=1, cex=1.5)

legend("topleft", legend = c("y = sin(cos(x))/x"),col = c("blue"))#标注数学公式

## -----------------------------------------------------------------------------
data(mtcars)

## -----------------------------------------------------------------------------
library(ggplot2)

## -----------------------------------------------------------------------------
ggplot(mtcars, aes(x = mpg, y = wt)) +
  geom_point() +
  xlab("MPG") +
  ylab("Weight") +
  ggtitle("The second required example about MPG vs. Weight")

# 生成相关系数矩阵
cor_matrix <- cor(mtcars)
cor_matrix

# 生成交叉表
table <- table(mtcars$gear, mtcars$cyl)
table

## -----------------------------------------------------------------------------
formula_sqrt <- expression(y = sqrt(x)*sin(x))

## -----------------------------------------------------------------------------
x <- seq(-10, 100, by = 0.01)

## -----------------------------------------------------------------------------
plot(x, sqrt(x)*sin(x), type = "l", col = "blue",
     main = "The third required example",
     xlab = "x", ylab = "y")
text(20, 8, substitute(y == sqrt(x)*sin(x)), cex = 1.2)

## -----------------------------------------------------------------------------
x <- rnorm(130)     
y <- 1/(1+x^2)

## -----------------------------------------------------------------------------
plot(x,y,main = "The fourth example",xlab=" Random variable", ylab=" Dependent variable")
legend("topleft", legend = c("y = 1/(1+x^2)"), col = c("blue")) #标注数学公式

## -----------------------------------------------------------------------------
# 加载bootstrap包和导入law数据
library(bootstrap)
library(boot)
data(law)

# 定义相关性统计量函数
cor_stat <- function(data, indices) {
  dt <- data[indices, ]
  cor(dt$LSAT, dt$GPA)
}
# bootstrap模拟
boot_out <- boot(data = law, statistic = cor_stat, R = 2000)

# 获取bootstrap样本计算的相关统计量
boot_corr <- boot_out$t

# 计算相关统计量的标准误差
se <- sqrt(var(boot_corr)/2000)

# 计算t分布的临界值
confidence_level <- 0.95
degrees_freedom <- 2000 - 1
t_critical <- qt((1 + confidence_level) / 2, df = degrees_freedom)

# 计算t置信区间估计的下限和上限
lower <- mean(boot_corr) - t_critical * se
upper <- mean(boot_corr) + t_critical * se

# 打印结果
cat("t置信区间估计:", round(lower, 4), "-", round(upper, 4))


## -----------------------------------------------------------------------------
library(bootstrap)
data(law)

M <- 2000
n <- nrow(law)
R <- numeric(M)
for (i in 1:M){
   m <- sample(1:n,size= n,replace = TRUE)
   R[i] <- cor(law$LSAT[m],law$GPA[m])
}

t_result <- t.test(R)
# 获取t置信区间估计的下限和上限
lower <- t_result$conf.int[1]
upper <- t_result$conf.int[2]

# 打印结果
cat("t置信区间估计:", round(lower, 4), "-", round(upper, 4))  

## -----------------------------------------------------------------------------
library(boot)
data(law, package = "bootstrap")
boot.obj <- boot(law, R = 2000,statistic = function(x, i){cor(x[i,1], x[i,2])})
ci = boot.ci(boot.obj, type="t.ci")

ci$t


## -----------------------------------------------------------------------------
# 定义功能函数
my_sample <- function(x, size, prob = rep(1/length(x), length(x))) {
  
# 创建累积概率向量
  x_prob <- cumsum(prob)
  
# 使用逆变换法：根据随机数和累积概率向量生成抽样值
  inverse_transform <- function(u) {
    index <- sum(u > x_prob) + 1
    return(x[index])
  }
  
# 生成随机数并进行抽样
  rnums <- runif(size) # 生成 size 个介于 0 到 1 之间的均匀分布的随机数
  sampled <- sapply(rnums, inverse_transform) # 通过逆变换法获取抽样值
  return(sampled)
}

# Example
x <- c(4, 3, 2, 5, 7, 9) # 数据集
x_size <- 6 # 抽样大小

#默认等概率抽样 (对比组r)
r1 <- sample(x,x_size)
r2 <- my_sample(x, x_size)

print(r1)
print(r2)

#指定概率抽样(对比组p)
p1 <-  sample(x,x_size,prob =c(.1,.2,.1,.2,.3,.1))
p2 <- my_sample(x, x_size,prob =c(.1,.2,.1,.2,.3,.1))

print(p1)
print(p2)

## -----------------------------------------------------------------------------
ranmd <- runif(1000) #生成 1000 个介于 0 到 1 之间的均匀分布的随机数

Lapuls <- sign(ranmd - 0.5) * log( 1 - 2 * abs(ranmd - 0.5)) # 拉普拉斯分布的逆变换

# 绘制生成样本的直方图
hist(Lapuls, breaks = 30, freq = FALSE, main = "Comparison")

# 叠加目标概率密度函数
x <- seq(-6, 6, length.out = 120)
fx <- (exp(-abs(x))) / 2
lines(x, fx, col = "blue")

## -----------------------------------------------------------------------------
#使用acceptance-rejection method 定义功能函数

AR_m <- function(n, a, b) {
    sample <- numeric(n)
    count <- 1
  while(count <= n) {
    x<- runif(1)
    y <- runif(1, min = 0, max = dbeta(x, a, b))
    if(y <= dbeta(x, a, b)) {
      sample[count] <- x
      count <- count + 1
    }
  }
  return(sample)
}

#采用理论Beta(3,2)生成1000个随机样本
beta_sample <- AR_m(1000, 3, 2)

# 绘制生成样本的直方图
hist(beta_sample, breaks = 29, freq = FALSE, main = "Comparsion")

# 叠加理论密度函数
x <- seq(0, 1, length.out = 100)
f_beta <- dbeta(x, 3, 2)
lines(x, f_beta, col = "red")

## -----------------------------------------------------------------------------
#采用beta生成随机数拟合
y <- rbeta(1000,2,2)
x <- 2*y-1 

# 绘制直方图
hist(x, prob = TRUE, main = expression(f(x)==(3/4)(1-x^2)))
m <- seq(-1, 1, 0.01) #将结果进行拟合
lines(m, (3/4)*(1-m^2))

# 定义变量和空向量

v1 <- 790
i <- 0
U <- numeric(v1)

# 生成随机数并进行拟合
for (i in 1:v1) {
  U1 <- runif(v1,-1,1)
  U2 <- runif(v1,-1,1)
  U3 <- runif(v1,-1,1)
  if(abs(U3[i]) >= abs(U2[i]) && abs(U3[i]) >= abs(U1[i])){
    U[i] <- U2[i]}
  else {
     U[i] <- U3[i]
   }
}

# 绘制直方图和拟合结果
hist(U, prob = TRUE, main = expression(f(x)==(3/4)(1-x^2)))
V <- seq(-1, 1, 0.01)
lines(V, (3/4)*(1-V^2))

## -----------------------------------------------------------------------------
# 构建函数，即用MC算法模型计算π的方差

var_pi_MC <- function(ρ){

# 设置参数
n <- 1e6  # 模拟实验的总次数
k <- 100  # 模拟次数

# 创建空向量存储每次模拟的结果
pi_ <- numeric(k)

# 模拟计算圆周率
for (i in 1:k) {
  m <- rbinom(1, n, 2*ρ/pi)  # 生成服从二项分布的随机数
  pi_[i] <- 2*ρ*n/m  # 计算圆周率的模拟值
}

# 计算方差
var_pi <- var(pi_)
var_pi

}

# 比较三种不同ρ取值

var_pi_MC(ρ=1)
var_pi_MC(ρ=0.8)
var_pi_MC(ρ=0.5)


## -----------------------------------------------------------------------------
# 采用antithetic variate 法 计算对应协方差和方差

# 设置参数
n <- 1e6/2

# antithetic variate 法构建变量
U_1 <- runif(n, min=0, max=1)
U_2 <- rep(1, n) - U_1

expU = (exp(U_1)+exp(U_2))/2
exp1_U = (exp(1-U_1)+exp(1-U_2))/2
# 计算对应协方差和方差
Cov_av <- cov(expU,exp1_U)
Var_av <- var(expU+exp1_U)

# 采用MC法计算方差
m <- 1e6
U_ <- runif(m, min=0, max=1)
var_mc = var(exp(U_))/m

# 采用antithetic variate 法计算方差
var_av = var(expU)/n

reduction_var = (var_mc - var_av) / var_mc * 100 
#  打印结果
print(paste("Cov using antithetic variate:", Cov_av))
print(paste("Var using antithetic variate:", Var_av))
print(paste("Percentage reduction in variance:", reduction_var, "%"))

## -----------------------------------------------------------------------------

# 采用MC法估计
m <- 1e6
U_ <- runif(m, min=0, max=1)
# 计算估计量及其方差
Ex_mc = mean(exp(U_))
var_mc = var(exp(U_))/m

# antithetic variate 法估计

# 设置参数
n <- 1e6/2
# antithetic variate 法构建变量expU
U_1 <- runif(n, min=0, max=1)
U_2 <- rep(1, n) - U_1

expU = (exp(U_1)+exp(U_2))/2

# 采用antithetic variate 法计算方差
Ex_av = mean(expU)
var_av = var(expU)/n

reduction_var = (var_mc - var_av) / var_mc * 100 

print(paste("estimation using simple MC:", Ex_mc))
print(paste("estimation using antithetic variate:",Ex_av))
print(paste("经计算，对比simple MC法，采用antithetic variate法的估计量方差降低百分比为", reduction_var, "%"))


## -----------------------------------------------------------------------------

# 定义g(x)，f1(x), f2(x)
g <- function(x) {
  (x^2) / sqrt(2*pi) * exp(-x^2/2)
}

# 用伽马分布定义重要性抽样函数f1(x)
f1 <- function(x) {
  dgamma(x, shape = 2.5, rate = 1.35)
}

# 用正态分布定义重要性函数f2(x)
f2 <- function(x) {
  dnorm(x, mean = 1.4, sd = 1.15)
}


# 从重要性抽样分布生成样本
n <- 100000      # Number of samples
x1 <- rgamma(n, shape = 2.5, rate = 1.35)
x1 <- x1[x1 > 1]    # Keep only samples greater than 1

# 用f1计算重要性抽样估计
estimate1 <- mean(g(x1)/f1(x1))
variance1 <- var(g(x1)/f1(x1))

# 使用f2生产随机样本
x2 <- rnorm(n, mean = 1.4, sd = 1.15)
x2 <- x2[x2 > 1]


# 用f2计算重要性抽样估计
estimate2 <- mean(g(x2)/f2(x2))
variance2 <- var(g(x2)/f2(x2))


cat("使用f1的重要性抽样估计: ", estimate1, " Variance: ", variance1, "\n")
cat("使用f2的重要性抽样估计: ", estimate2, " Variance: ", variance2)


# 为绘图创建x值
x <- seq(1, 30, by = 0.1)

# 绘制 g(x), f1(x), and f2(x)
plot(x, g(x), type = "l", lwd = 2, ylim = c(0, 0.6),
     main = "Plot of g(x), f1(x), and f2(x)",
     xlab = "x", ylab = "Function Value")

lines(x, f1(x), col = "blue", lwd = 2)
lines(x, f2(x), col = "red", lwd = 2)

# 添加图例
legend("topright", legend = c("g(x)", "f1(x)", "f2(x)"), lwd = 2, col = c("black", "blue", "red"))


## -----------------------------------------------------------------------------
# 定义g函数
g <- function(x) {
  (x^2) / sqrt(2*pi) * exp(-x^2/2)
}

#用正态分布定义重要性函数f(x)
f <- function(x) {
  dnorm(x, mean = 1.4, sd = 1.15)
}

# 用正态分布生产随机样本
n <- 1e5
x <- rnorm(n, mean = 1.4, sd = 1.15)
x <- x[x > 1]


# 计算重要性抽样估计值
estimate <- mean(g(x) / f(x))

# 输出结果
cat("采用重要性抽样的蒙特卡罗估计:", estimate)

## -----------------------------------------------------------------------------
# 定义区间数
m <- 5

# 模拟次数
N <- 1e5

# 重要性函数
f3 <- function(x) exp(-x) / (1 - exp(-1))

# 原始密度函数
g  <- function(x) exp(-x) / (1 + x^2)

# 开始变量
theta.hat <- 0
se <- 0

# 分层重要采样法
for (j in 0:(m - 1)) {

  # 第j个区间的下界和上界
  lower <- j / m
  upper <- (j + 1) / m
  
  # 重复实验
  for (i in 1:N) {
    # 生成随机样本
    u <- runif(1, lower, upper)

  # 更新均值和标准差
    theta.hat <- theta.hat + g(u) / f3(u)
    se <- se + (g(u) / f3(u))^2
  }
}

# 完成估算
theta.hat <- theta.hat / (m * N)
se <- sqrt((se / (m * N) - theta.hat^2) / (m * N - 1))

print(paste('估计值θ：', theta.hat))
print(paste('估计值的标准差：', se))

## -----------------------------------------------------------------------------
set.seed(123)

# 模拟次数
N <- 5000

# 样本大小
n <- 20

# 卡方分布的自由度
df <- 2

# 卡方分布均值
true_mean <- df

# 初始化计数值
coverage_count <- 0

# Monte Carlo simulation
for (i in 1:N) {
  # 生产卡方分布随机数
  sample <- rchisq(n, df)
  
  # 估计t区间
  t_interval <- t.test(sample)$conf.int
  
  # 检查真实均值是否在区间内
  if (true_mean >= t_interval[1] && true_mean <= t_interval[2]) {
    coverage_count <- coverage_count + 1
  }
}

# 估计覆盖概率
coverage_P<- coverage_count / N

print(paste('估计t区间的覆盖概率为', coverage_P))

## -----------------------------------------------------------------------------
set.seed(123)

# 定义参数
alpha <- 0.05
n <- 100
num_sims <- 5000

# 计算双侧t检验的临界值
t_critical <- qt(1 - alpha/2, df = n - 1)

# 定义总体分布函数和对应的均值
chisq_dist <- function(n) {
  rchisq(n, df = 1)
}

unif_dist <- function(n) {
  runif(n, 0, 2)
}

exp_dist <- function(n) {
  rexp(n, rate = 1)
}

distributions <- list(chisq = list(f = chisq_dist, mean = 1),
                      unif = list(f = unif_dist, mean = 1),
                      exp = list(f = exp_dist, mean = 1))

# 对每种分布进行模拟
results <- sapply(names(distributions), function(dist) {
  
  # 获取总体分布的函数和均值
  rdist <- distributions[[dist]]$f
  mu0 <- distributions[[dist]]$mean
  
  # 对于每次模拟，生成样本并进行t检验
  rejected <- replicate(num_sims, {
    x <- rdist(n)
    t_test_result <- t.test(x, mu = mu0)
    t_test_result$p.value < alpha/2 || t_test_result$p.value > 1 - alpha/2
  })
  
  # 计算实际的类型I错误率
  error_rate <- mean(rejected)
  
  # 返回实际的类型I错误率
  return(error_rate)
})

# 打印结果
names(results) <- names(distributions)
print(results)


## -----------------------------------------------------------------------------
# 定义一些参数
m = 1000
M = 1000

fwer = tpr = fdr = matrix(NA, nrow = 2, ncol = M)

# 进行 M次模拟
for (j in 1:M) {
  
  #生成 p值
  pvalue = c(runif(950), rbeta(50, 0.1, 1))
  
  #处理Bonferroni方法
  pvalue.Bonf = p.adjust(pvalue, method = "bonferroni") 
  #确定是否拒绝原假设
  reject.Bonf = pvalue.Bonf < 0.1
  
  #计算 FWER， FDR， TDR
  fwer[1, j] = any(reject.Bonf[1:950]) 
  fdr[1, j] = sum(reject.Bonf[1:950]) / sum(reject.Bonf)
  tpr[1,j] =  sum(reject.Bonf[951:m]) / 50
  
  #处理BH方法
  pvalue.BH = p.adjust(pvalue, method = "BH") 
  #确定是否拒绝原假设
  reject.BH = pvalue.BH < 0.1
  
  #计算 FWER， FDR， TDR
  fwer[2, j] = any(reject.BH[1:950]) 
  fdr[2, j] = sum(reject.BH[1:950]) / sum(reject.BH)
  tpr[2,j] =  sum(reject.BH[951:m]) / 50
}

# 输出结果矩阵
result = matrix(c(rowMeans(fwer, na.rm = TRUE), rowMeans(fdr, na.rm = TRUE), rowMeans(tpr, na.rm = TRUE)), nrow = 2)
colnames(result) <- c("FWER", "FDR", "TPR")
rownames(result) <- c("Bonf", "B-H")
result

## -----------------------------------------------------------------------------

# 设置参数
lambda <- 2
sample_sizes <- c(5, 10, 20)
B <- 1000
m <- 1000

# 创建存储结果的向量
mean_bias <- numeric(length(sample_sizes))
bootstrap_bias <- numeric(length(sample_sizes))
theoretical_se <- numeric(length(sample_sizes))
bootstrap_se <- numeric(length(sample_sizes))
theoretical_bias <- numeric(length(sample_sizes))

# 循环进行仿真
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  bias <- numeric(m)
  se <- numeric(m)
  
  for (j in 1:m) {
    # 生成样本数据
    x <- rexp(n, rate = lambda)
    x_mean <- mean(x)
  
    # 计算极大似然估计
    lambda_hat <- 1/x_mean
    
    # 生成bootstrap样本数据
    bootstrap_samples <- replicate(B, { 
      resample <- sample(x, replace = TRUE) 
      1/mean(resample) })
    
    # 计算偏差
    bias[j] <-mean(bootstrap_samples)-lambda_hat
    # 计算bootstrap估计的标准误差
    se[j] <- sd(bootstrap_samples)
  }

  # 计算平均偏差和标准误
  theoretical_bias[i] = lambda/(n-1)
  mean_bias[i] <- mean(bias)
  theoretical_se[i] <- lambda*n/((n-1)*sqrt(n-2))
  bootstrap_se[i] <- mean(se)
}

# 打印结果
results <- data.frame(
  n = sample_sizes,
  Bootstrap_Bias = mean_bias,
  Theoretical_Bias = theoretical_bias,
  Bootstrap_SE = bootstrap_se,
  Theoretical_SE = theoretical_se
)
print(results)
print("可以看出，采用Bootstrap重采样方法能良好地估计偏差和标准误差，当n值越大时，其估计值与理论值更接近")

## -----------------------------------------------------------------------------
# 从bootstrap库中导入law数据
data(law, package = "bootstrap")

#定义函数
boot.t.ci <-
function(x, B, R, level = .95, statistic){
  #计算bootstrap t置信区间
  x <- as.matrix(x); n <- nrow(x)
  stat <- numeric(B); se <- numeric(B)
    boot.se <- function(x, R, f) {
    #定义局部函数估计statistich的标准误差
    x <- as.matrix(x); m <- nrow(x)
    th <- replicate(R, expr = {
        i <- sample(1:m, size = m, replace = TRUE)
        f(x[i, ])
        })
        return(sd(th))
    }
  for (b in 1:B) {
    j <- sample(1:n, size = n, replace = TRUE)
    y <- x[j, ]
    stat[b] <- statistic(y)
    se[b] <- boot.se(y, R = R, f = statistic)
  }
  stat0 <- statistic(x)
  t.stats <- (stat - stat0) / se
  se0 <- sd(stat)
  alpha <- 1 - level
  Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(stat0 - Qt * se0)
}

#估计bootstrap t置信区间
ci <- boot.t.ci(law, statistic = function(x, i){cor(x[i,1], x[i,2])}, B=2000, R=200)

# 打印结果
cat("bootstrap t置信区间估计：",ci)


## -----------------------------------------------------------------------------
# 加载所需包
library(boot)

# 定义数据
times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# 进行bootstrap计算置信区间（样本均值是 1 / λ 的无偏估计）
boot_results <- boot(times, statistic = function(data,index)mean(data[index]), R = 2000)
boot.ci(boot_results, type = c("norm", "basic", "perc", "bca"))


## -----------------------------------------------------------------------------
library(bootstrap)

# 计算相关系数
b.cor <- function(x, i) {
  cor(x[i, 1], x[i, 2])
}

# 计算特征值比例
calculate_theta <- function(x) {
  lambdas <- eigen(x)$values
  sumlam <- sum(lambdas)
  theta <- lambdas[1] / sumlam
  return(theta)
}

# Bootstrap方法估计参数偏差和标准误差
bootstrap_estimate <- function(x, B) {
  lambdas <- eigen(x)$values
  sumlam <- sum(lambdas)
  theta <- lambdas[1] / sumlam
  y <- lambdas / sumlam
  
  thetastar <- numeric(B)
  
  for (b in 1:B) {
    xstar <- sample(y, replace = TRUE)
    thetastar[b] <- mean(xstar)
  }
  
  bias <- mean(thetastar) - theta
  se <- sd(thetastar)
  
  result <- c(Bias.boot = bias, SE.boot = se)
  return(round(result, 3))
}

# 测试代码
x <- cov(scor, scor)
B <- 10000

bootstrap_estimate(x, B)


## -----------------------------------------------------------------------------
# 加载数据
library(DAAG);data(ironslag) 
# 获取磁力观测值数量
n <- length(ironslag$magnetic) 

# 初始化存储拟合误差的矩阵
mse1 <- mse2 <- mse3 <- mse4 <- matrix(0, n, n)  

# 嵌套循环，遍历磁力观测值的组合
for (i in 1:(n-1)) {
  for (j in (i+1):n) {
    y <- ironslag$magnetic[c(-i, -j)]  # 创建新的磁力向量，排除当前的i和j观测值
    x <- ironslag$chemical[c(-i, -j)]  # 创建新的化学成分向量，排除当前的i和j观测值

    # 拟合回归模型J1，使用lm()函数，y作为因变量，x作为自变量
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * ironslag$chemical[c(i, j)]  # 计算预测值（yhat1）
    mse1[i, j] <- mean((ironslag$magnetic[c(i, j)] - yhat1)^2)  # 计算平方误差，并存储在相应的e1矩阵位置上

    # 类似地，拟合回归模型J2，包括二次项
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * ironslag$chemical[c(i, j)] + J2$coef[3] * ironslag$chemical[c(i, j)]^2
    mse2[i, j] <- mean((ironslag$magnetic[c(i, j)] - yhat2)^2)

    # 拟合回归模型J3，对磁力取对数
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * ironslag$chemical[c(i, j)]
    yhat3 <- exp(logyhat3)
    mse3[i, j] <- mean((ironslag$magnetic[c(i, j)] - yhat3)^2)

    # 拟合回归模型J4，对磁力和化学成分都取对数
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(ironslag$chemical[c(i, j)])
    yhat4 <- exp(logyhat4)
    mse4[i, j] <- mean((ironslag$magnetic[c(i, j)] - yhat4)^2)
  }
}

# 计算均方误差的均值
result <- c(L1_MSE = sum(mse1)/choose(n, 2), L2_MSE = sum(mse2)/choose(n, 2), L3_MSE = sum(mse3)/choose(n, 2), L4_MSE = sum(mse4)/choose(n, 2)) 
# 返回包含四个模型的均方误差的向量
result  

## -----------------------------------------------------------------------------

# 定义Cramér-von Mises统计量函数
cvm_test <- function(X, Y) {
    group <- c(rep(1, length(X)), rep(2, length(Y)))
    data <- c(X, Y)
    n <- length(data)
    ranks <- rank(data)
    U1 <- sum(ranks[group == 1])
    n1 <- length(X)
    n2 <- length(Y)
    T <- (U1 - n1*(n1+1)/2)^2 / (n1*n2)  # Cramér-von Mises 统计量
    return(T)
}

# 定义置换测试函数
permutation_test <- function(x, y, num_permutations = 10000, test_fn) {
    observed_value <- test_fn(x, y)
    pooled <- c(x, y)
    extreme_count <- 0
    for(i in seq_len(num_permutations)){
        permuted <- sample(pooled, length(pooled))
        perm_x <- permuted[seq_len(length(x))]
        perm_y <- permuted[-seq_len(length(x))]
        if(test_fn(perm_x, perm_y) >= observed_value){
            extreme_count <- extreme_count + 1
        }
    }
    return(extreme_count / num_permutations)  # p值
}
# 定义X和Y样本
X <- c(158, 171, 193, 199, 230, 243, 248, 248, 250, 267, 271, 316, 327, 329)
Y <- c(141, 148, 169, 181, 203, 213, 229, 244, 257, 260, 271, 309)

# 计算
p_value <- permutation_test(X, Y, 10000, cvm_test)
print(p_value)

## -----------------------------------------------------------------------------
set.seed(2023) # 设置随机种子以确保结果可重复

count5test <- function(x, y) {
  # 对x和y进行中心化
  X <- x - mean(x)
  Y <- y - mean(y)
  
  # 计算权重
  w <- round(10 * (length(x)/(length(x) + length(y))))
  
  # 计算超出边界值的数量
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  
  # 如果最小的数量超过权重或者最大的数量超过10减去权重，则拒绝原假设（返回1），否则保留原假设（返回0）
  return(as.integer(min(c(outx, outy)) > w || max(c(outx, outy)) > (10 - w)))
}

R <- 10000 # 模拟实验的重复次数

n1 <- 20 # 第一组样本的大小
n2 <- 30 # 第二组样本的大小

weight <- round(10 * (n1/(n1 + n2))) # 计算权重

# 从正态分布生成第一组样本x
x <- rnorm(n1, 0, 1)
# 从正态分布生成第二组样本y
y <- rnorm(n2, 0, 1)

# 将x和y合并为一个总体样本z
z <- c(x,y)

K <- 1:(n1 + n2) # 设置样本索引
n <- length(x) # 样本大小
jg <- numeric(R) # 存储假设检验的结果

# 进行R次模拟实验
for (i in 1:R) {
  # 从总体样本z中随机选择长度为n的子样本，不放回抽样
  k <- sample(K, size = n, replace = FALSE)
  
  # 根据索引k将样本分为第一组x1和第二组y1
  x1 <- z[k]
  y1 <- z[-k]
  
  x1 <- x1 -mean(x1)
  y1 <- y1- mean(y1)
  
  # 调用count5test函数进行假设检验，并将结果存储在jg向量中
  jg[i] <- count5test(x1, y1)
}

# 输出结果，保留小数点后四位

cat("拒绝原假设的比例:",round(c(mean(jg)), 4))  
print("原文为0.1064，可以看出，加入权重能够更好的适应样本不同的情况")

## -----------------------------------------------------------------------------
# 设定随机数种子
set.seed(12345)

# 设定参数和常量
N <- 1e6      # 样本数量(Big enough)
b1 <- 0       # 回归系数 b1,b2,b3
b2 <- 1
b3 <- -1
f0_v <- c(0.1, 0.01, 0.001, 0.0001)  # f0值设定

# 生成随机变量
x1 <- rpois(N, lambda = 1)  # 从参数为1的泊松分布中生成随机数
x2 <- rexp(N, rate = 1)      # 从速率为1的指数分布中生成随机数
x3 <- sample(0:1, N, replace = TRUE)  # 从0和1中随机抽取，进行放回抽样

# 定义函数来计算alpha值
calculate_a <- function(f0) {
  g <- function(alpha) {
    tmp <- exp(-alpha - b1*x1 - b2*x2 - b3*x3)
    p <- 1 / (1 + tmp)
    mean(p) - f0  # 返回函数值
  }
  
  # 对于给定的f0值，使用uniroot函数寻找根（alpha）
  solution <- uniroot(g, c(-100, 100))
  alpha <- solution$root
  return(alpha)
}

# 对于每一个f0值计算并储存对应的a值
a_values <- sapply(f0_v, function(f0) calculate_a(f0))

# 绘图
plot(-log10(f0_v), a_values, type = "b", xlab = "-log(f0)", ylab = "a", main = "Plot of -log(f0) vs a")

## -----------------------------------------------------------------------------
set.seed(2012)

rw.MS <- function(n, variance, x0, N) {
  # 存储样本序列
  x <- numeric(N)
  x[1] <- x0
  
  # 生成均匀分布的随机数
  u <- runif(N, -.5, .5)
  
  # 计算接受率R
  R <- 1 -  sign(u) * log(1 - 2 * abs(u))
  
  # 计数器
  k <- 0
  
  # 生成样本序列
  for (i in 2:N) {
    # 从建议分布中生成候选样本
    y <- rnorm(1, x[i - 1], sqrt(variance))
    
    # 计算接受率，并比较接受或拒绝候选样本
    if (R[i] <= dt(y, n) / dt(x[i - 1], n))
      x[i] <- y
    else {
      x[i] <- x[i - 1]
      k <- k + 1
    }
  }
  
  # 返回样本序列和拒绝比例
  return(list(x = x, rejection_rate = k / N))
}

n <- 3  # 目标学生t分布的自由度
N <- 2e3  # 生成的样本数量
variance <- c(0.05, 0.5, 1, 2, 20)  # 不同的建议分布的标准差
x0 <- 15  # 初始样本值

# 调用rw.MS函数，计算不同v值下的拒绝比例
results <- lapply(variance, function(v) {
  rw <- rw.MS(n, v, x0, N)
  return(data.frame(rejection_rate = 1 - rw$rejection_rate))
})

# 创建一个空的数据框用于存储结果
result_table <- matrix(nrow = 2, ncol = 5)
colnames(result_table) <- NULL

# 填充结果表格
for (i in 1:length(variance)) {
  result_table[1, i] <- variance[i]
  result_table[2, i] <- results[[i]]$rejection_rate
}

# 设置行标签
rownames(result_table) <- c("Variance", "Rejection Rate")

# 输出结果表格
print(result_table)

## -----------------------------------------------------------------------------
# 库的加载
library(ggplot2)

# 参数设定
set.seed(2012)
N <- 2000    # 迭代数
burnin <- 500  # 烧入期
sigma <- matrix(c(1,0.9,0.9,1), nrow=2)  # 协方差矩阵
mu <- c(0,0)  # 均值设定为0

# Gibbs采样器
gibbs <- function(N, burnin, sigma, mu) {
  chain <- matrix(0, nrow=N, ncol=2)
  chain[1,] <- rnorm(2) # 先验设定 
  
  for (i in 2:N) {
    chain[i,1] <- rnorm(1, mean=mu[1]+sigma[1,2]*((chain[i-1,2]-mu[2])/sigma[2,2]),
                        sd=sqrt(sigma[1,1]-sigma[1,2]^2/sigma[2,2]))
    chain[i,2] <- rnorm(1, mean=mu[2]+sigma[2,1]*((chain[i,1]-mu[1])/sigma[1,1]),
                        sd=sqrt(sigma[2,2]-sigma[2,1]^2/sigma[1,1]))
  }
  
  return(chain[-(1:burnin),]) 
}

chain <- gibbs(N, burnin, sigma, mu)

# 绘图
ggplot(data.frame(x=chain[,1], y=chain[,2]), aes(x=x, y=y)) +
  geom_point(alpha=0.3) +
  theme_bw() +
  labs(x="Xt", y="Yt")

# 线性回归拟合
fit <- lm(y ~ x, data=data.frame(x=chain[,1], y=chain[,2]))

# 正态性检验
resids <- residuals(fit)
shapiro_test <- shapiro.test(resids)
print(shapiro_test)

# 残差的检查
resids <- residuals(fit)

# 残差和方差定性检验
plot(fit, which=1)  # Residuals vs Fitted
plot(fit, which=3)  # Scale-Location (检查异方差性)


## -----------------------------------------------------------------------------

library(coda)
set.seed(2012)

# 定义 Gelman.Rubin 函数，用于计算 Gelman-Rubin 统计量
Gelman.Rubin <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) # 计算每个链的均值
  B <- n * var(psi.means) # 计算链间方差
  psi.w <- apply(psi, 1, var) # 计算链内方差
  W <- mean(psi.w) # 计算总体均值
  v.hat <- ((n - 1) / n) * W + B / n # 估计总体方差的上界
  r.hat <- v.hat / W # Gelman-Rubin 统计量
  return(r.hat)
}

# 定义 normal.chain 函数，用于生成正态分布的 Metropolis 迭代链
normal.chain <- function(sigma, N, X1) {
  x <- numeric(N)
  x[1] <- X1
  u <- runif(N, -0.5, 0.5) # 生成均匀分布的[0,1]上的随机数
  for (i in 2:N) {
    xt <- x[i-1]
    y <- rnorm(1, xt, sigma) # 生成候选点
    r1 <- dnorm(y, 0, 1) * dnorm(xt, y, sigma) # 计算比例 r1
    r2 <- dnorm(xt, 0, 1) * dnorm(y, xt, sigma) # 计算比例 r2
    r <- r1 / r2 # 比较 r1 和 r2
    if (u[i] <= r) {
      x[i] <- y
    } else {
      x[i] <- xt
    }
  }
  return(x)
}

run_until_converged <- function(sigma, k, n, b, x0) {
  X <- matrix(0, nrow=k, ncol=n)
  r.hat <- 1.2
  
  while (r.hat >= 1.2) {
    # 生成 Metropolis 迭代链
    for (i in 1:k) {
      X[i, ] <- normal.chain(sigma, n, x0)
    }
   
    # 计算诊断统计量
    psi <- t(apply(X, 1, cumsum)) # 计算累积和
    for (i in 1:nrow(psi)) {
      psi[i, ] <- psi[i, ] / (1:ncol(psi)) # 求均值
    }
    r.hat <- Gelman.Rubin(psi)
    
    # 若链未收敛，增加迭代次数
    if (r.hat >= 1.2) {
      n <- n + b
      X <- matrix(0, nrow=k, ncol=n)
    }
  }
  
  # 使用 coda 包进行检查
  mcmc_chains <- lapply(1:k, function(i) {
    mcmc(X[i,], start=1, end=n)
  })
  
  mcmc_list = mcmc.list(mcmc_chains) # 创建 mcmc.list
  gelman_diag = gelman.diag(mcmc_list) # 计算 Gelman-Rubin 诊断
  print(gelman_diag)
  gelman.plot(mcmc_list) # 绘制 Gelman-Rubin 图
  
  return(list(X=X, r.hat=r.hat))
}

# 调整参数 
sigma <- 0.5 #正态分布标准差
k <- 3
n <- 2e3
b <- 1000
x0 <- 15

results <- run_until_converged(sigma, k, n, b, x0)

cat("Gelman-Rubin 统计量:", results$r.hat)

sigma <- 2 #正态分布标准差
k <- 3
n <- 1e3
b <- 1000
x0 <- 15

results <- run_until_converged(sigma, k, n, b, x0)

## -----------------------------------------------------------------------------
# 模拟生成数据
set.seed(123)

u <- c(11, 8, 27, 13, 16, 0, 23, 10, 24, 2)
v <- c(12, 9, 28, 14, 17, 1, 24, 11, 25, 3)

# Define negative log-likelihood function
nll <- function(lambda) {
  return (-sum(log(exp(-lambda * u) - exp(-lambda * v))))
}
#res <- optimize(-nll,lower=0,upper=8,maximum=TRUE)
#res$maximum
result <- optim(0.01, nll, method = "L-BFGS-B")


cat("直接极大化观测数据的似然函数得到 λ的MLE：",round(result$par,6))

## -----------------------------------------------------------------------------

#设置初始值
set.seed(123)

u <- c(11, 8, 27, 13, 16, 0, 23, 10, 24, 2)
v <- c(12, 9, 28, 14, 17, 1, 24, 11, 25, 3)

lambda <- 20
iteration_gap <- 1e-6

# 定义一个函数来计算E-step
compute_expectation <- function(u, v, lambda) {
  num1 <- ((-v-(1/lambda))*exp(-lambda*v) + (u+(1/lambda))*exp(-lambda*u))
  den <-(exp(-lambda * u) - exp(-lambda * v))
  return (num1 / den)
}

LogLikelihoodFunc <- function(lambda, u, v) {
  return (sum(log(exp(-lambda * u) - exp(-lambda * v)+0.9764)))
}#

# 开始迭代
while(TRUE){
  old_lambda <- lambda
  # E-step
  e_step <- compute_expectation(u, v, lambda)
  # M-step
  
  lambda <- 1/mean(e_step)
  
  #新得到的lambda
  new_lambda <- LogLikelihoodFunc(lambda, u, v)

  # 检查是否收敛
  if(abs(new_lambda - old_lambda) < iteration_gap) break

  # 更新 lambda
  lambda <- new_lambda
}

# 输出 lambda 的 MLE
cat("EM算法得到 λ的MLE：",round(lambda,6))

## -----------------------------------------------------------------------------
## 加载资源包
library(plyr)
library(lpSolve)
library(boot)
# 定义用于解决零和博弈的函数 solve.game
solve.game <- function(A) {
  # 解决两人零和博弈，使用单纯形法
  # 优化玩家1，然后玩家2
  # 最大化 v 使得 ...
  # 让 x 代表策略 1:m，v 作为额外变量
  # A1，是 <= 约束
  #
  min.A <- min(A)
  A <- A - min.A # 使得 v >= 0
  max.A <- max(A)
  A <- A / max.A
  m <- nrow(A)
  n <- ncol(A)
  it <- n^3
  a <- c(rep(0, m), 1) # 目标函数
  A1 <- -cbind(t(A), rep(-1, n)) # 约束 <=
  b1 <- rep(0, n)
  A3 <- t(as.matrix(c(rep(1, m), 0))) # 约束 sum(x)=1
  b3 <- 1
  sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
                maxi=TRUE, n.iter=it)
  # 解的 [x1,x2,...,xm | 博弈的值]
  #
  # 最小化 v 使得 ...
  # 让 y 代表策略 1:n，v 为额外变量
  a <- c(rep(0, n), 1) # 目标函数
  A1 <- cbind(A, rep(-1, m)) # 约束 <=
  b1 <- rep(0, m)
  A3 <- t(as.matrix(c(rep(1, n), 0))) # 约束 sum(y)=1
  b3 <- 1
  sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
                maxi=FALSE, n.iter=it)
  soln <- list("A" = A * max.A + min.A,
               "x" = sx$soln[1:m],
               "y" = sy$soln[1:n],
               "v" = sx$soln[m+1] * max.A + min.A)
  soln
}

# 定义 payoff matrix A
matrixA  <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
                      2,0,0,0,-3,-3,4,0,0,
                      2,0,0,3,0,0,0,-4,-4,
                      -3,0,-3,0,4,0,0,5,0,
                      0,3,0,-4,0,-4,0,5,0,
                      0,3,0,0,4,0,-5,0,-5,
                      -4,-4,0,0,0,5,0,0,6,
                      0,0,4,-5,-5,0,0,0,6,
                      0,0,4,0,0,5,-6,-6,0), 9, 9)

# 创建 payoff matrix B
matrixB <- matrixA + 2

# 计算 game A 的结果
sA <- solve.game(matrixA)

# 计算 game B 的结果
sB <- solve.game(matrixB)

# 输出最优策略
round(cbind(sB$x,sB$y),7)
print("Apparently， the solution of game B is the extreme points (11.15) of the original game A. ")

# 比较博弈 A 和 B 的值
cat("Game A value: ", sA$v, "\n")
cat("Game B value: ", sB$v, "\n")

## -----------------------------------------------------------------------------
# 加载 plyr 包
library(plyr)

# 创建一个数据框

df <- data.frame(
  A = c(10, 20, 30, 40),
  B = c(1.5, 2.5, 3.5, 4.5),
  C = c(100, 200, 300, 400)
)

scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}

# apply it to every column of a data frame
scaled_df <- apply(df, 2, scale01)
scaled_df


# 列wise函数，用于数值型列
scale01_numeric <- colwise(scale01)

# 对数据框中的数值列应用缩放函数
scaled_df <- scale01_numeric(df)

# 输出结果
scaled_df

## -----------------------------------------------------------------------------
# 创建一个数值数据框
numeric_df <- data.frame(
  A = c(10, 20, 30, 40),
  B = c(1.0, 2.0, 3.0, 4.0),
  C = c(100, 200, 300, 400)
)

# 计算每列的标准差
sd_numeric <- vapply(numeric_df, sd, numeric(1))
cat("the standard deviation of every column in a numeric data frame:",sd_numeric,"\n")

# 创建一个包含数值和非数值列的混合数据框
mixed_df <- data.frame(
  A = c(10, 20, 30, 40),
  B = c("apple", "banana", "orange", "grape"),
  C = c(1.5, 2.5, 3.5, 4.5),
  D = c("red", "green", "blue", "yellow")
)

# 确定数值列
numeric_cols <- sapply(mixed_df, is.numeric)

# 计算每个数值列的标准差
sd_mixed <- vapply(mixed_df[, numeric_cols], sd, numeric(1))
cat("the standard deviation of every numeric column in a mixed data frame:",sd_mixed)

## -----------------------------------------------------------------------------
library(microbenchmark)
gibbs_sampler <- function(a, b, n, iter=1000) {
  x <- rep(0, iter) # 初始化x向量
  y <- rep(0, iter) # 初始化y向量

  # 初始化参数
  x[1] <- 0 
  y[1] <- rbeta(1, a, b)
  
  for (i in 2:iter) {
    # 样本x | y
    x[i] <- rbinom(1, n, y[i-1])
    # 样本y | x
    y[i] <- rbeta(1, x[i] + a, n - x[i] + b)
  }
  return(data.frame(x, y))
}

library(Rcpp)
dir_cpp <- '/Users/liujt/Downloads/mcmc 2/'
# 使用sourceCpp函数，先编写一个C++源文件，然后将它嵌入到R中
sourceCpp(paste0(dir_cpp,"gibbs_sampler.cpp"))

# 设置测试参数
a <- 1
b <- 3
n <- 12
iter <- 1000

result <- microbenchmark(
  R = gibbs_sampler(a, b, n, iter), 
  Rcpp = gibbs_sampler_cpp(a, b, n, iter),
  times = 10 
)
print(result)

