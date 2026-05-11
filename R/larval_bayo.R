#' Duration and survival of larval stage of \emph{Anopheles gambiae} s.s.
#'
#' Computes the duration and survival rate of the aquatic larval stage of
#' \emph{Anopheles gambiae} sensu stricto as a function of temperature,
#' based on empirical data from Bayoh & Lindsay (2003).
#'
#' @param T Numeric. Water temperature in degrees Celsius.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{d}{Duration of larval development (in days), estimated from a
#'     mechanistic equation (Bayoh & Lindsay, 2003).}
#'   \item{e}{Survival rate of larvae (\%), i.e. the proportion of eggs that will emerge as an adult, estimated from a 4th-order
#'     polynomial equation fitted to data from Table 1 of Bayoh & Lindsay
#'     (2003).}
#' }
#'
#' @details
#' The duration \code{d} is computed using the mechanistic equation proposed
#' by Bayoh & Lindsay (2003):
#' \deqn{d = \frac{1}{-0.05 + 0.005T - 2.14 \times 10^{-16} e^T - 281357.656\, e^{-T}}}
#'
#' The survival \code{s} is estimated from a 4th-order polynomial fitted with
#' GraphPad Prism to the data in Table 1 of Bayoh & Lindsay (2003):
#' \deqn{s = -17.47 + 2.337T - 0.1088T^2 + 0.002213T^3 - 1.733 \times 10^{-5} T^4}
#'
#' @references
#' Bayoh, M.N., Lindsay, S.W., 2003. Effect of temperature on the development
#' of the aquatic stages of \emph{Anopheles gambiae} sensu stricto (Diptera:
#' Culicidae). \emph{Bulletin of Entomological Research}, 93, 375–381.
#' \doi{10.1079/BER2003259}
#'
#' @examples
#' # Duration and survival at 25°C
#' dlarv.gambiae(25)
#'
#' # Over a temperature gradient
#' temps <- seq(15, 35, by = 1)
#' results <- lapply(temps, dlarv.gambiae)
#' d_vals <- sapply(results, `[[`, "d")
#' s_vals <- sapply(results, `[[`, "s")
#'
#' @export

dlarv.gambiae <- function(T){
  d <- 1/((-0.05)+0.005*T+(-2.14e-16)*exp(T)+(-281357.656*exp(-T))) # Bayoh et al 2003
  if (d < 0){
    d = 0
  }
  s <- -17.47 + 2.337*T -0.1088*T^2 + 0.002213*T^3 -1.733e-005*T^4 	# 4th order polynomial equation fitted with Graphpad on data from Bayoh et al. 2003 (Table 1)
  if (s < 0){
    s = 0
  }
  return(list(d = d,s = s))
}
