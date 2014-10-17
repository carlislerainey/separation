#' @title Obtain posterior simulations using Gelman et al.'s suggested Cauchy prior.
#' 
#' @description
#' \code{sim_post_gelman()} uses a Metropolis algorithm to obtain posterior 
#' simulations using Gelman et al.'s suggested default prior.
#' 
#' 
#' @param formula A logistic regression model. Importantly, the explanatory variables should
#' be rescaled so that continuous variables have mean zero and standard deviation one-half
#' and binary variable are centered at zero. This can be done automatically with the \code{rescale()} function 
#' in the \code{arm} package. See Gelman (2008) and Gelman et al (2008).
#' @param data A data frame.
#' @param n_sims The number of simulations after the burn-in period.
#' @param n_burnin The number of burn-in iterations for the sample.
#' @param n_chains The number of MCMC chains being run.
#' @param n_thin The thinning interval used in the simulation. The number of MCMC 
#'                iterations must be divisible by this value.
#' @param n_cores The number of MCMC cores. Defaults to the number of chains.
#' @param tune The tuning parameter for the Metropolis sampling. Can be either a 
#' positive scalar or a (k+1)-vector, where k is the number of variables in the
#' model. Presently passed to \code{MCMCmetrop1R}.
#' @param scale_int The scale paramater for the Cauchy prior on the intercept. As
#' suggested by Gelman et al. (2008), this defaults to 10.
#' @param scale_int The scale paramater for the Cauchy prior on the coefficients. As
#' suggested by Gelman et al. (2008), this defaults to 2.5.
#' 
#' @references Gelman, Andrew. 2008. "Scaling Regression Inputs by Dividing by
#'   Two Standard Deviations." Statistics in Medicine 27(15):2865–2873.
#' @references Gelman, Andrew, Aleks Jakulin, Maria Grazia Pittau, and Yu-Sung
#'   Su. 2008. "A Weakly Informative Prior Distribution for Logistic and Other
#'   Regression Models." The Annals of Applied Statistics 2(4):1360–1383.
#'   
#' @export

sim_post_gelman <- function(formula, data, 
                            n_sims = 1000, n_burnin = n_sims/2,
                            n_thin = 1, n_chains = 3, n_cores = n_chains,
                            tune = 1, scale_int = 10, scale_coef = 2.5) {
  mf <- model.frame(formula, data) 
  y <- model.response(mf)
  X <- model.matrix(formula, data)
  mean_X <- apply(X, 2, mean)
  sd_X <- apply(X, 2, sd)
  std_X <- apply(X, 2, arm::rescale)
  X <- Matrix::Matrix(std_X)
  n_burnin <- floor(n_burnin)
  cat("\nComputing proposal distribution...\n")
  mle <- arm::bayesglm(formula, data = data, family = binomial)
  V <- vcov(mle)
  # set up sampler
  l_seed <- runif(6, 100000, 999999)
  init_seed <- runif(n_chains, 100000, 999999)
  run_mcmc <- function(x) {
    set.seed(init_seed[x])
    init <- MASS::mvrnorm(1, mu = coef(mle), Sigma = 10*V)
    mcmc <- MCMCpack::MCMCmetrop1R(fun = log_post_gelman, 
                                   theta.init = init, V = V,
                                   scale_coef = scale_coef, scale_int = scale_int, 
                                   X = X, y = y, 
                                   thin = n_thin, burnin = n_burnin, mcmc = n_sims,
                                   tune = tune, verbose = 0,
                                   seed = list(l_seed, x))
    return(mcmc)
  }
  # do the sampling
  cat(paste("\nRunning ", n_chains, " chains in parallel of ", n_sims + n_burnin, " iterations each--this may take a while...", sep = ""))
  mcmc_chains <- parallel::mclapply(1:n_chains, run_mcmc, mc.cores = n_cores)
  cat(paste("\nFinished running chains!\n", sep = ""))
  # clean up chains
  mcmc_chains <- coda::as.mcmc.list(mcmc_chains)
  mcmc <- NULL
  for (i in 1:n_chains) {  
    mcmc <- rbind(mcmc, mcmc_chains[[i]])
  }
  colnames(mcmc) <- colnames(X)
  # convergence diagnostics
  if (n_chains == 1) {
    R_hat <- NA
    cat("\nYou only ran one chain, so I'm not using the R-hat to check convergence.\n")
  } else {
    R_hat <- coda::gelman.diag(mcmc_chains)
    cat(paste("\nChecking convergence...\n", sep = ""))
    if (R_hat[[2]] <= 1.02) {
      cat(paste("\nThe multivariate R-hat statistic of ", round(R_hat[[2]], 2), 
                " suggests that the chains have converged.\n\n", sep = ""))
    }
    if (R_hat[[2]] > 1.02) {
      cat(paste("\n######## WARNING: #########\n\nThe multivariate R-hat statistic of ", round(R_hat[[2]], 2), 
                " suggests that the chains have NOT converged.\n\n", sep = ""))
    }
  }
  data <- list(X = matrix(X),
               y = y)
  fn_args <- list(formula = formula, 
                  data = data,
                  n_sims = n_sims,
                  n_burnin = n_burnin, 
                  n_thin = n_thin,
                  tune = tune, 
                  n_chains = n_chains, 
                  n_cores = n_cores)
  res <- list(mcmc_chains = mcmc_chains,
              mcmc = mcmc,
              R_hat = R_hat,
              mle = mle,
              data = data,
              prior = "Gelman",
              fn_args = fn_args)
  return(res)
}
