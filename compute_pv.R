#bayesian bp value function from matt
library(brms)

compute_bpv <- function(fit,
                        y = NULL,
                        stat = c("mean","sd","zero_prop"),
                        ndraws = 400,
                        re_formula = NULL,
                        stat_fun = NULL,
                        resp = NULL) {
  stat <- match.arg(stat)
  if (is.null(y)) {
    y_raw <- brms::get_y(fit, resp = resp)
    y <- if (is.matrix(y_raw)) as.numeric(y_raw[, 1]) else as.numeric(y_raw)
  }
  if (is.null(stat_fun)) {
    stat_fun <- switch(stat,
                       mean      = function(z) mean(z, na.rm = TRUE),
                       sd        = function(z) sd(z,   na.rm = TRUE),
                       zero_prop = function(z) mean(z == 0, na.rm = TRUE)
    )
  }
  T_obs <- stat_fun(y)
  yrep <- brms::posterior_predict(fit, ndraws = ndraws, re_formula = re_formula, resp = resp)
  if (length(dim(yrep)) == 3) yrep <- yrep[, , 1, drop = FALSE]
  if (!is.null(ndraws) && nrow(yrep) > ndraws) {
    yrep <- yrep[seq_len(ndraws), , drop = FALSE]
  }
  T_rep <- apply(yrep, 1, stat_fun)
  p_upper <- mean(T_rep >= T_obs)
  p_lower <- mean(T_rep <= T_obs)
  p_bayes <- 2 * min(p_upper, p_lower)
  p_bayes <- max(min(p_bayes, 1), 0)
  list(
    p_value = p_bayes,
    T_obs   = T_obs,
    T_rep   = T_rep,
    p_upper = p_upper,
    p_lower = p_lower
  )
}
