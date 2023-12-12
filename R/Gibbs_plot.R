#' @useDynLib SA23229024
#' @title Gibbs Plot Function
#' @name Gibbs_plot
#' @description This function performs Gibbs sampling and creates plots for the sampling chain.
#'
#' @param N An integer for the number of iterations.
#' @param burnin An integer for the number of burn-in iterations.
#' @param sigma A covariance matrix.
#' @param mu A numeric vector for the mean values.
#' @param set_seed An optional integer for the RNG seed.
#' @importFrom Rcpp sourceCpp
#' @importFrom ggplot2 ggplot aes geom_point theme_bw labs
#' @importFrom stats lm residuals shapiro.test
#' @return A list containing a linear model object and a Shapiro-Wilk test object.
#' @examples
#' \dontrun{
#' results <- Gibbs_plot(N = 2000, burnin = 500, sigma = matrix(c(1,0.9,0.9,1),
#'                       nrow=2), mu = c(0,0))
#' # View summary information for linear models
#' summary(results$fit)
#'
#' # View the results of the Shapiro-Wilk normality test
#' results$shapiro
#' }
#' @export
Gibbs_plot <- function(N, burnin, sigma, mu, set_seed = 2012) {

  # Same function body as previously provided
  if (!is.null(set_seed)) {
    set.seed(set_seed)
  }
  cpp_path <- system.file("src", "Gibbs_sampling.cpp", package = "SA23229024")

  # 确保文件存在
  if (!file.exists(cpp_path)) {
    stop("Cannot find Gibbs_sampling.cpp in the src directory of the package")
  }
  # Source the C++ function
  sourceCpp(cpp_path)

  # 进行Gibbs采样
  chain <- Gibbs_sampling_cpp(N, burnin, sigma, mu)

  chain_df <- data.frame(x=chain[,1], y=chain[,2])
  # 绘图
  p <- ggplot(chain_df, aes(x=chain_df$x, y=chain_df$y)) +
    geom_point(alpha=0.3) +
    theme_bw() +
    labs(x="Xt", y="Yt")
  print(p)

  # 线性回归拟合
  fit <- lm(chain_df$y ~ chain_df$x, data=chain_df)

  # 正态性检验
  resids <- residuals(fit)
  shapiro_test <- shapiro.test(resids)

  # 残差的检查
  resids <- residuals(fit)

  # 残差和方差定性检验
  plot(fit, which=1)  # Residuals vs Fitted
  plot(fit, which=3)  # Scale-Location (检查异方差性)

  # 返回线性模型拟合结果和Shapiro-Wilk测试结果
  return(list(fit = fit, shapiro = shapiro_test))
}
