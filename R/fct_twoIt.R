#' twoIt
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#'
#'
#'
#'




twoItStanGlm <- function(fit, var_name, HA_diff_l = NULL, HA_diff_u = NULL,
                         HP_diff_l = NULL, HP_diff_u = NULL, Sim = 100000) {
  position <- which(var_name == fit$coefficients %>% names())
  if (length(position) == 0) stop("Var name, not in the model")

  n_iter_mod <- nrow(as.data.frame(fit))
  diff <- as.array(fit)[, , position]
  loc <- prior_summary(fit)$prior$location[position - 1]
  scale <- prior_summary(fit)$prior$adjusted_scale[position - 1]
  if (is.null(prior_summary(fit)$prior$adjusted_scale)) {
    scale <- prior_summary(fit)$prior$scale[position - 1]
  }
  diffprior <- rnorm(Sim, loc, scale)


  respdi <- data.frame(names = c(
    paste0("HA : Pr(", HA_diff_l, " < diff < ", HA_diff_u, ")"),
    paste0("HP : Pr(", HP_diff_l, " < diff < ", HP_diff_u, ")"),
    paste0("HA | D : Pr(", HA_diff_l, " < diff < ", HA_diff_u, " | D)"),
    paste0("HP | D : Pr(", HP_diff_l, " < diff < ", HP_diff_u, " | D)")
  ))



  respdi$values <- c(
    sum(diffprior > (HA_diff_l) & diffprior < (HA_diff_u)) / Sim,
    sum(diffprior > (HP_diff_l) & diffprior < (HP_diff_u)) / Sim,
    sum(diff > (HA_diff_l) & diff < (HA_diff_u)) / n_iter_mod,
    sum(diff > (HP_diff_l) & diff < (HA_diff_u)) / n_iter_mod
  )



  return(respdi)
}
