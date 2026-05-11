#' Estimate Fisher's alpha from species richness and total abundance
#'
#' This function estimates Fisher's alpha diversity index from the observed
#' species richness (S) and total abundance (N) by numerically solving the
#' Fisher log-series relationship.
#'
#' The function uses numerical optimization to solve the implicit equation
#' linking richness, abundance, and the log-series parameter, and then
#' computes Fisher's alpha.
#'
#' @param S Integer. Observed species richness in the sample.
#' @param N Integer. Total abundance (total number of individuals) in the sample.
#'
#' @return Numeric. Estimated Fisher's alpha diversity index.
#'
#' @details
#' Fisher's alpha is derived from the log-series distribution and satisfies:
#' \deqn{S = \alpha \log(1 + N / \alpha)}
#'
#' This function estimates the intermediate parameter x by minimizing:
#' \deqn{r = (1 - x) n / x * (-\log(1 - x))}
#'
#' and then computes:
#' \deqn{\alpha = n (1 - x) / x}
#'
#' @references
#' Fisher, R.A., Corbet, A.S., Williams, C.B. (1943).
#' The relation between the number of species and the number of individuals
#' in a random sample of an animal population.
#' Journal of Animal Ecology, 12(1), 42–58.
#'
#' @examples
#' fisher_alpha(S = 10, N = 100)
#'
#' @export
fisher_alpha <- function(S,N){
  if (length(S) != 1 || length(N) != 1 ||
      is.na(S) || is.na(N) ||
      S <= 0 || N <= 0 ||
      S > N ||
      S %% 1 != 0 || N %% 1 != 0) {
    stop("S and N must be positive integers with S <= N.")
  }
  f.x <- function( r , n , x )( abs( ( 1 - x )* n / x * ( -log( 1 - x ) ) - r ) )
  x <- optimize( f = f.x , interval = c(1e-10, 1 - 1e-10) , n = N , r = S , tol = 0.0000001)[[1]]
  alpha = N * ( 1 - x ) / x
  return(alpha)
}
