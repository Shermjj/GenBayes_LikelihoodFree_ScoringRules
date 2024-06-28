library(BSL)
require(MASS)
require(elliplot)
library(reticulate)

summStatRobust <- function(x) {
  T <- length(x)
  ssx <- numeric(4)
  octile <- elliplot::ninenum(x)[2:8]
  ssx[1] <- octile[4]
  ssx[2] <- octile[6] - octile[2]
  ssx[3] <- (octile[7] - octile[5] + octile[3] - octile[1]) / ssx[2]
  ssx[4] <- (octile[6] + octile[2] - 2*octile[4]) / ssx[2]
  return(ssx)
}

normScore <- function(x, y) {
  n <- length(x)
  r0 <- 1 : n
  z1 <- qnorm(rank(x) / (n + 1))
  z2 <- qnorm(rank(y) / (n + 1))
  c <- qnorm(r0 / (n + 1))
  r <- sum(z1 * z2) / sum(c ^ 2)
  norm_score <- 0.5 * log((1 + r) / (1 - r))
  return(norm_score)
}
logTransform <- function(x, bound) {
  x_tilde <- numeric(5)
  for (i in 1 : 5) {
    x_tilde[i] <- log((x[i] - bound[i, 1]) / (bound[i, 2] - x[i]))
  }
  return(x_tilde)
}

backLogTransform <- function(x_tilde, bound) {
  x <- numeric(5)
  for (i in 1 : 5) {
    x[i] <- (bound[i, 1] + bound[i, 2] * exp(x_tilde[i])) / (1 + exp(x_tilde[i]))
  }
  return(x)
}

mvgk_sum <- function (y) 
{
  if (is.vector(y) && !is.matrix(y)) {
    # Convert the vector to a 1xN matrix
    y <- matrix(y, nrow = 1)
  }
  D <- ncol(y)
  N <- nrow(y)
  #ssxRobust <- apply(y, MARGIN = 2, FUN = summStatRobust)
  #ssxRobust <- rowMeans(ssxRobust) #This should give 4 summary stats
  ssxRobust <- summStatRobust(as.vector(y))
  if (N < 6L) {
    return(ssx = c(ssxRobust, rep(0,10)))
  }
  else {
    ssxNormScore <- numeric(choose(D, 2)) # 5C2 = 10
    count <- 1
    for (i in 1:(D - 1)) {
      for (j in (i + 1):D) {
        ssxNormScore[count] <- normScore(y[, i], y[, j])
        count <- count + 1
      }
    }
    return(ssx = c(ssxRobust, ssxNormScore)) #total should be 14 SS
  }
}

mvgk_sim <- function(theta, n_sim) {
#  bounds <- rbind(cbind(0,4),
#                  cbind(0,4),
#                  cbind(0,4),
#                  cbind(0,4),
#                  cbind(-sqrt(3)/3, sqrt(3)/3))
  
#  theta <- backLogTransform(theta_uncon, bounds)
  # Extract parameters from theta
  A <- theta[1]
  B <- theta[2]
  g <- theta[3]
  k <- theta[4]
  rho <- theta[5]
  dim <- 5  # Dimension of the simulation, fixed as 5
  
  # Create a covariance matrix with rho in the off-diagonal (adjacent only)
  create_cov_matrix <- function(dim, rho) {
    cov <- diag(1, dim)
    if (dim > 1 && abs(rho) > 0) {
      cov[row(cov) == col(cov) + 1] <- rho
      cov[row(cov) == col(cov) - 1] <- rho
    }
    return(cov)
  }
  
  # Transform from z to g-and-k distribution values
  z_to_gk <- function(z, A, B, g, k, c) {
    term1 <- ifelse(g == 0, 1, 1 + 0.8 * tanh(g * z / 2))
    term2 <- z * (1 + z^2)^k
    return(A + B * term1 * term2)
  }
  
  # Generate multivariate normal samples
  cov_matrix <- create_cov_matrix(dim, rho)
  z_samples <- mvrnorm(n_sim, mu = rep(0, dim), Sigma = cov_matrix)
  
  if(n_sim == 1){
    z_samples <- t(matrix(z_samples))
  }
  results <- apply(z_samples, c(1,2), function(z_row) z_to_gk(z_row, A, B, g, k, 0.8))
  # Apply the g-and-k transformation to each sample
  
  return(results) #this should be an n x dim matrix (dim of output same as param dim)
}


np <- import("numpy", convert=TRUE)
gk_obs <- np$load("gk_obs_4.npy")
bounds <- rbind(cbind(0,4),
                cbind(0,4),
                cbind(0,4),
                cbind(0,4),
                cbind(-sqrt(3)/3, sqrt(3)/3))

for(n_obs in c(10, 20, 50, 70, 100, 150, 200)){
  set.seed(0)
  n = floor(20000 / n_obs)
  pilot_mod <- newModel(fnSim = mvgk_sim, fnSum = mvgk_sum, 
                    theta0=backLogTransform(rep(0,5), bounds),
                    thetaNames = expression(a,b,g,k,rho),
                    simArgs = list(n_sim = n_obs))
  
  
  pilot_res <- bsl(gk_obs[1:n_obs,], n = n, M = 1000, model = pilot_mod, covRandWalk = diag(5),
                       logitTransformBound = bounds,verbose=1L)
  
  samples <- getTheta(pilot_res)
  cov_mat = cov(samples[500:1000,])
  init_mean = colMeans(samples[500:1000,])
  
  full_mod <- newModel(fnSim = mvgk_sim, fnSum = mvgk_sum, 
                       theta0=backLogTransform(init_mean, bounds),
                    thetaNames = expression(a,b,g,k,rho),
                    simArgs = list(n_sim = n_obs))
  
  full_res <- bsl(gk_obs[1:n_obs,], n = n, M = 3000, model = full_mod, covRandWalk = cov_mat,
                       logitTransformBound = bounds,verbose=1L)
  save_file <- sprintf("bsl_res_%.1d.rds", n_obs)
  saveRDS(full_res, save_file)
  
  #np$save(sprintf("bsl_res_%.1d.npy", n_obs), getTheta(full_res))
}