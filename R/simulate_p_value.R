#' @title Simulate P-Values and Compare Correction Methods
#' @name simulate_p_values
#' @description This function simulates p-values under the null and alternative hypotheses,
#' then applies Bonferroni and Benjamini-Hochberg (BH) correction methods to
#' assess True discovery rate(TDR), true positive rate (TPR),false discovery rate (FDR),
#' family-wise error rate (FWER), and F1_score. It repeats the simulation 'M' times and
#' returns the average metrics for each correction method.
#' @param m Integer; total number of hypotheses (default 1000).
#' @param M Integer; number of simulations to perform (default 1000).
#' @param pvalue_distribution_null Function; a function to generate p-values
#'        under the null hypothesis (default is `runif`).You can use some function,
#'        such as rbeta  rf rt rexp rnorm.
#' @param dist_params_null List; list of parameters to pass to the null
#'        distribution function (default is `list(0,1)`).
#' @param pvalue_distribution_alt Function; a function to generate p-values
#'        under the alternative hypothesis (default is `rbeta`).You can use some function
#'         different from the null hypothesis，such as runif  rf rt rexp rnorm.
#' @param dist_params_alt List; list of parameters to pass to the alternative
#'        distribution function (default is `list(0.1,1)`).
#' @param alpha Numeric; significance level used to decide if a p-value indicates
#'        a statistically significant finding (default 0.1).
#'
#' @importFrom stats p.adjust rbeta runif rf rt rexp rnorm
#'
#' @return A matrix with average FWER, FDR, and TPR for Bonferroni and BH
#'         correction methods across all simulations.
#' @details This function simulates p-values under the null and alternative hypotheses.
#' @examples
#' \dontrun{
#' set.seed(123) # for reproducibility
#' result <- simulate_p_values(m = 1000, M = 1000, pvalue_distribution_null = runif,
#'                            dist_params_null = list(min = 0, max = 1),
#'                            pvalue_distribution_alt = rbeta,
#'                            dist_params_alt = list(shape1 = 0.1, shape2 = 1),
#'                            alpha = 0.1)
#' result
#' }
#' @export
simulate_p_values <- function(m = 1000, M = 1000, pvalue_distribution_null = runif,
                              dist_params_null = list(0,1),
                              pvalue_distribution_alt = rbeta,
                              dist_params_alt = list(0.1,1),
                              alpha = 0.1) {

  # Initialize matrices to store error rates and power
  tdr <- fwer <- tpr <- fdr <- f1 <- matrix(NA, nrow = 2, ncol = M)

  for (j in 1:M) {

    # Generate p-values: (1-alpha)*m from null and alpha*m from alternative
    pvalue <- c(do.call(pvalue_distribution_null, c(list(m - m / 20), dist_params_null)),
                do.call(pvalue_distribution_alt, c(list(m / 20), dist_params_alt)))

    # Bonferroni method
    pvalue.Bonf <- p.adjust(pvalue, method = "bonferroni") # Adjust p-values using Bonferroni correction
    reject.Bonf <- pvalue.Bonf < alpha # Determine which hypotheses to reject
    # Calculate true discovery rate (TDR), family-wise error rate (FWER), false discovery rate (FDR), true positive rate (TPR), and F1 score for Bonferroni method
    tdr[1,j] <-  sum(reject.Bonf[(m - m / 20 + 1):m]) / max(sum(reject.Bonf), 1)
    fwer[1, j] <- any(reject.Bonf[1:(m - m / 20)])
    fdr[1, j] <- sum(reject.Bonf[1:(m - m / 20)]) / max(sum(reject.Bonf), 1)
    tpr[1,j] <- sum(reject.Bonf[(m - m / 20 + 1):m]) / (m / 20)
    f1[1,j] <- 2*(tdr[1,j]*tpr[1,j])/(tdr[1,j]+tpr[1,j])

    # BH (Benjamini-Hochberg) method
    pvalue.BH <- p.adjust(pvalue, method = "BH") # Adjust p-values using Benjamini-Hochberg method
    reject.BH <- pvalue.BH < alpha # Determine which hypotheses to reject
    # Calculate TDR, FWER, FDR, TPR, and F1 score for BH method
    tdr[2,j] <- sum(reject.BH[(m - m / 20 + 1):m]) / max(sum(reject.BH), 1)
    fwer[2, j] <- any(reject.BH[1:(m - m / 20)])
    fdr[2, j] <- sum(reject.BH[1:(m - m / 20)]) / max(sum(reject.BH), 1)
    tpr[2,j] <- sum(reject.BH[(m - m / 20 + 1):m]) / (m / 20)
    f1[2,j] <- 2*(tdr[2,j]*tpr[2,j])/(tdr[2,j]+tpr[2,j])
  }

  # Return the mean of each metric across all simulations for both methods
  result <- matrix(c(rowMeans(tdr, na.rm = TRUE), rowMeans(tpr, na.rm = TRUE), rowMeans(fdr, na.rm = TRUE),
                     rowMeans(fwer, na.rm = TRUE), rowMeans(f1, na.rm = TRUE)),nrow = 2)
  colnames(result) <- c("TDR", "TPR", "FDR", "FWER", "F1_score")
  rownames(result) <- c("Bonf", "BH")

  return(result)
}
