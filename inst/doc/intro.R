## ----eval=FALSE---------------------------------------------------------------
#  simulate_p_values <- function(m = 1000, M = 1000, pvalue_distribution_null = runif,
#                                dist_params_null = list(0,1),
#                                pvalue_distribution_alt = rbeta,
#                                dist_params_alt = list(0.1,1),
#                                alpha = 0.1) {
#  
#    # Initialize matrices to store error rates and power
#    tdr <- fwer <- tpr <- fdr <- f1 <- matrix(NA, nrow = 2, ncol = M)
#  
#    for (j in 1:M) {
#  
#      # Generate p-values: (1-alpha)*m from null and alpha*m from alternative
#      pvalue <- c(do.call(pvalue_distribution_null, c(list(m - m / 20), dist_params_null)),
#                  do.call(pvalue_distribution_alt, c(list(m / 20), dist_params_alt)))
#  
#      # Bonferroni method
#      pvalue.Bonf <- p.adjust(pvalue, method = "bonferroni") # Adjust p-values using Bonferroni correction
#      reject.Bonf <- pvalue.Bonf < alpha # Determine which hypotheses to reject
#      # Calculate true discovery rate (TDR), family-wise error rate (FWER), false discovery rate (FDR), true positive rate (TPR), and F1 score for Bonferroni method
#      tdr[1,j] <-  sum(reject.Bonf[(m - m / 20 + 1):m]) / max(sum(reject.Bonf), 1)
#      fwer[1, j] <- any(reject.Bonf[1:(m - m / 20)])
#      fdr[1, j] <- sum(reject.Bonf[1:(m - m / 20)]) / max(sum(reject.Bonf), 1)
#      tpr[1,j] <- sum(reject.Bonf[(m - m / 20 + 1):m]) / (m / 20)
#      f1[1,j] <- 2*(tdr[1,j]*tpr[1,j])/(tdr[1,j]+tpr[1,j])
#  
#      # BH (Benjamini-Hochberg) method
#      pvalue.BH <- p.adjust(pvalue, method = "BH") # Adjust p-values using Benjamini-Hochberg method
#      reject.BH <- pvalue.BH < alpha # Determine which hypotheses to reject
#      # Calculate TDR, FWER, FDR, TPR, and F1 score for BH method
#      tdr[2,j] <- sum(reject.BH[(m - m / 20 + 1):m]) / max(sum(reject.BH), 1)
#      fwer[2, j] <- any(reject.BH[1:(m - m / 20)])
#      fdr[2, j] <- sum(reject.BH[1:(m - m / 20)]) / max(sum(reject.BH), 1)
#      tpr[2,j] <- sum(reject.BH[(m - m / 20 + 1):m]) / (m / 20)
#      f1[2,j] <- 2*(tdr[2,j]*tpr[2,j])/(tdr[2,j]+tpr[2,j])
#    }
#  
#    # Return the mean of each metric across all simulations for both methods
#    result <- matrix(c(rowMeans(tdr, na.rm = TRUE), rowMeans(tpr, na.rm = TRUE), rowMeans(fdr, na.rm = TRUE),
#                     rowMeans(fwer, na.rm = TRUE), rowMeans(f1, na.rm = TRUE)),nrow = 2)
#    colnames(result) <- c("TDR", "TPR", "FDR", "FWER", "F1_score")
#    rownames(result) <- c("Bonf", "BH")
#  
#    return(result)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(2023) # for reproducibilit
#  simulate_p_values(m = 1000, M = 1000, pvalue_distribution_null = runif,
#                          dist_params_null = list(0, 1),
#                          pvalue_distribution_alt = rbeta,
#                          dist_params_alt = list(0.1, 1),
#                          alpha = 0.1)

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(2023) # for reproducibilit
#  simulate_p_values(m = 1000, M = 1000, pvalue_distribution_null = runif,
#                          dist_params_null = list(0,1),
#                          pvalue_distribution_alt = rt,
#                          dist_params_alt = list(10),
#                          alpha = 0.05)

## ----eval=FALSE---------------------------------------------------------------
#  Gibbs_plot <- function(N, burnin, sigma, mu, set_seed = 2012) {
#  
#    # Same function body as previously provided
#    if (!is.null(set_seed)) {
#      set.seed(set_seed)
#    }
#    cpp_path <- system.file("src", "Gibbs_sampling.cpp", package = "SA23229024")
#  
#    # 确保文件存在
#    if (!file.exists(cpp_path)) {
#      stop("Cannot find Gibbs_sampling.cpp in the src directory of the package")
#    }
#    # Source the C++ function
#    sourceCpp(cpp_path)
#  
#    # 进行Gibbs采样
#    chain <- Gibbs_sampling_cpp(N, burnin, sigma, mu)
#  
#    chain_df <- data.frame(x=chain[,1], y=chain[,2])
#    # 绘图
#    p <- ggplot(chain_df, aes(x=chain_df$x, y=chain_df$y)) +
#      geom_point(alpha=0.3) +
#      theme_bw() +
#      labs(x="Xt", y="Yt")
#    print(p)
#  
#    # 线性回归拟合
#    fit <- lm(chain_df$y ~ chain_df$x, data=chain_df)
#  
#    # 正态性检验
#    resids <- residuals(fit)
#    shapiro_test <- shapiro.test(resids)
#  
#    # 残差的检查
#    resids <- residuals(fit)
#  
#    # 残差和方差定性检验
#    plot(fit, which=1)  # Residuals vs Fitted
#    plot(fit, which=3)  # Scale-Location (检查异方差性)
#  
#    # 返回线性模型拟合结果和Shapiro-Wilk测试结果
#    return(list(fit = fit, shapiro = shapiro_test))
#  }
#  

## ----eval=FALSE---------------------------------------------------------------
#  # Set random number seeds to ensure reproducible results
#  set.seed(2023)
#  results <- Gibbs_plot(N = 2000, burnin = 500, sigma = matrix(c(1,0.9,0.9,1),
#                                                               nrow=2), mu = c(0,0))
#  # View summary information for linear models
#  summary(results$fit)
#  
#   # View the results of the Shapiro-Wilk normality test
#  results$shapiro
#  

## ----eval=FALSE---------------------------------------------------------------
#  # Set random number seeds to ensure reproducible results
#  set.seed(2023)
#  results <- Gibbs_plot(N = 5000, burnin = 1000, sigma = matrix(c(1, 0.7, 0.7, 1),
#                                                                 nrow=2), mu = c(-1, 1))
#  # View summary information for linear models
#  summary(results$fit)
#  
#   # View the results of the Shapiro-Wilk normality test
#  results$shapiro

