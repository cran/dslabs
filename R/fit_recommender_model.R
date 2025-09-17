#' Fit a Latent Factor Recommender Model via Alternating Least Squares
#'
#' This function fits a penalized latent factor model for recommendation systems
#' using an alternating least squares (ALS) algorithm. It estimates user effects,
#' item effects, and latent factors simultaneously, with regularization to
#' prevent overfitting. The implementation supports filtering out items with too
#' few ratings.  
#' 
#' @param rating A numeric vector of observed ratings.
#' @param user_id A character vector identifying the user
#'   for each rating. Must be the same length as `rating`.
#' @param item_id A character vector identifying the item
#'   for each rating. Must be the same length as `rating`.
#' @param K Integer. The number of latent factors to estimate.
#' @param lambda_1 Numeric. Regularization parameter for user and item effects.
#' @param lambda_2 Numeric. Regularization parameter for latent factors.
#' @param min_ratings Integer. Minimum number of ratings required for an item to
#'   be included in the estimation of latent factors.
#' @param maxit Integer. Maximum number of iterations.
#' @param reltol Numeric. Relative reltolerance for convergence, based on change in
#'   the objective function.
#' @param damping Numeric between 0 and 1. Damping factor used to blend updates
#'   with the previous iteration for convergence stability.
#' @param verbose Logical. If `TRUE`, prints progress messages during
#'   optimization.
#'   
#' @return A list with the following components:
#'   \item{mu}{Global mean rating.}
#'   \item{a}{Named numeric vector of user-specific effects.}
#'   \item{b}{Named numeric vector of item-specific effects.}
#'   \item{p}{Matrix of user latent factors, with one row per user. The rownames of this matrix match the names of `a`.}
#'   \item{q}{Matrix of item latent factors, with one row per item. The rownames of this matrix match the names of `b`.}
#'   \item{min_ratings}{The threshold value used to filter items: only items with at least this many ratings are included in the estimation of latent factors.}
#'   \item{n_item}{Named integer vector of number of ratings per item.}
#'   \item{n_user}{Named integer vector of number of retained ratings per user.}
#'   \item{fitted}{Fitted values.}
#'
#'
#' @details
#' The model being fit is:
#' \deqn{Y_{ij} = \mu + \alpha_i + \beta_j + \sum_{k=1}^K p_{ik} q_{jk} + \varepsilon_{ij}}
#' where \eqn{\mu} is the global mean, \eqn{\alpha_i} are user effects,
#' \eqn{\beta_j} are item effects, and the \eqn{p_{ik}}, \eqn{q_{jk}} are latent
#' factors for users and items respectively. The estimation minimizes a penalized
#' least squares criterion with separate penalties for user/item effects and
#' latent factors.
#' 
#' Items with less than min_ratings observations are excluded from the estimation 
#' of `p` and `q`.
#'
#' @examples
#' set.seed(2010)
#' ## Simulation settings
#' n_users <- 200       # number of users
#' n_items <- 300       # number of items
#' K_true  <- 4         # true number of latent factors
#' sparsity <- 0.25     # ~5% of user-item pairs are observed
#' 
#' ## True parameters
#' mu <- 3.5
#' a_true <- rnorm(n_users, 0, 0.3)     # user effects
#' b_true <- rnorm(n_items, 0, 0.4)     # item effects
#' p_true <- matrix(rnorm(n_users * K_true, 0, 0.5), n_users, K_true)
#' q_true <- matrix(rnorm(n_items * K_true, 0, 0.5), n_items, K_true)
#'
#' names(a_true) <- 1:n_users
#' names(b_true) <- 1:n_items
#' rownames(p_true) <- 1:n_users
#' rownames(q_true) <- 1:n_items
#' ## Generate observed ratings matrix with sparsity
#' user_id <- rep(as.character(1:n_users), each = n_items)
#' item_id <- rep(as.character(1:n_items), times = n_users)
#' 
#' 
#' ## Which entries are observed?
#' obs <- runif(length(user_id)) < sparsity
#' 
#' ## Ratings with noise
#' rating_full <- mu + a_true[user_id] + b_true[item_id] +
#'   rowSums(p_true[user_id, ] * q_true[item_id, ]) +
#'   rnorm(length(user_id), 0, 0.25)
#' 
#' rating <- rating_full[obs]
#' user_id <- user_id[obs]
#' item_id <- item_id[obs]
#' 
#' ## Call your recommender function
#' fit <- fit_recommender_model(rating, user_id, item_id, K = 4, reltol = 1e-5,
#'                              min_ratings = 5, verbose = TRUE)
#' plot(fit$fitted, rating)
#'
#' @export

fit_recommender_model <- function(rating, user_id, item_id,
                                  K = 8,
                                  lambda_1 = 0.00005,
                                  lambda_2 = 0.0001,
                                  min_ratings = 20,
                                  maxit = 500,
                                  reltol = 1e-8,
                                  damping = 0.75,
                                  verbose = FALSE){
  
  if (!all.equal(length(rating), length(user_id), length(item_id)))
    stop("rating, userId, and item_id must be the same length.")
  
  ## make sure they are character since they are used as names
  user_id <- as.character(user_id) 
  item_id <- as.character(item_id)
  N <- length(rating)
  
  ## index of indexes for each user and item
  indexes_u <- split(1:N, user_id)
  indexes_i <- split(1:N, item_id)
  
  ## Number of ratings per item
  n_item <- sapply(indexes_i, length)
  ## index of items with at least min_ratings
  index_q <- which(n_item >= min_ratings)
  
  ## The n_item associated with each observed rating
  ns <- n_item[item_id]
  
  ## indices of data points whose items have at least min_ratings
  ind <- which(ns >= min_ratings)
  ## If users ends up with less than min_ratings we ask they be excluded
  n_user <- table(as.factor(user_id)[ind])
  if (any(n_user == 0)) {
    stop("After filtering out movies with too few ratings, no ratings remain for the following user(s): ", 
         paste(names(n_user)[n_user == 0], collapse = ", "))
  } 
  if (min(n_user) < min_ratings) {
    warning("After filtering out items with too few ratings, one or more users no longer have more than min_ratings observation. Use the `n_user` component to identify the affected user(s).")
  }
  ## speed-up: index only observations that survive the min_ratings filter (avoid implicit zeros)
  indexes_u_nozeros <- split(ind, user_id[ind])
  indexes_i_q <- indexes_i[index_q]
    
  ## starting values for global mean (mu), user effects (a), movie/item effects (b)
  mu <- mean(rating)
  a <- sapply(indexes_u, function(i) sum(rating[i] - mu)/(length(i) + lambda_1*N))
  resid <- rating - mu - a[user_id]
  b <- sapply(indexes_i, function(i) sum(resid[i])/(length(i) + lambda_1*N))
  
  I <- length(indexes_u)
  J <- length(indexes_i)
  p <- svd(matrix(rnorm(K*I, 0, 0.1), I, K))$u
  rownames(p) <- names(indexes_u)
  q <- matrix(rep(0, K*J), J, K)
  rownames(q) <- names(indexes_i)
  pq <- rep(0, N)
  prev_obj <- 0
  
  for (iter in 1:maxit) {
    ## estimate user and movie effects
    resid <- rating - (mu + b[item_id] + pq)
    a <- sapply(indexes_u, function(i) sum(resid[i])/(length(i) + lambda_1*N))
    resid <- rating - (mu + a[user_id] + pq)
    b <- sapply(indexes_i, function(i) sum(resid[i])/(length(i) + lambda_1*N))
    
    ## Now estimate latent factors p (users) and q (items)
    ## ALS updates one factor (column k) at a time, holding others fixed
    ## treating all the others as constant.
    ## closed-form ridge update using only observed entries (sparse)
    prev_p <- p ## prev_p and prev_q are defined for damping
    prev_q <- q 
    pq <- rowSums(p[user_id, -1, drop = FALSE] * q[item_id, -1, drop = FALSE])
    resid <- rating - (mu + a[user_id] + b[item_id] + pq)
    for (k in 1:K) {
      ## update q 
      q[index_q, k] <- sapply(indexes_i_q, function(i) { 
        x <- p[user_id[i], k]
        sum(x*resid[i])/(sum(x^2) + N*lambda_2)
      })
      q[,k] <- damping*q[,k] + (1 - damping)*prev_q[,k]
      
      ## update p
      p[,k] <- sapply(indexes_u_nozeros, function(i) {
        x <- q[item_id[i], k]
        sum(x*resid[i])/(sum(x^2) + N*lambda_2)
      })
      p[,k] <- damping*p[,k] + (1 - damping)*prev_p[,k]
      
      ## update residual to reflect the newly updated p[,k] and q[,k]
      resid <- resid - p[user_id, k]*q[item_id, k]
    }
    ## Build final latent interaction term pq = row-wise sum of p[user]*q[item]
    pq <- rowSums(p[user_id, ] * q[item_id, ])
    
    resid <- rating - (mu + a[user_id] + b[item_id] + pq)
    
    obj <- mean(resid^2) + 
      lambda_1*(sum(a^2) + sum(b^2)) + 
      lambda_2*(sum(p^2) + sum(q^2))
    
    rel_change <- (prev_obj - obj) / (prev_obj + 1e-8)
    if (verbose) {
      message(sprintf("Iteration %d: Objective = %.6f, Relative = %.6f", 
                      iter, obj, ifelse(iter > 1, rel_change, NA)))
    }
    prev_obj <- obj
    if (abs(rel_change) < reltol) break
  }

  
  ## orthogonalize factors via SVD of p %*% t(q[index_q,]) and rescale by sqrt(s$d)
  QR_p <- qr(p)
  QR_q <- qr(q[index_q,,drop = FALSE])
  s <- svd(qr.R(QR_p) %*% t(qr.R(QR_q)))
  u <- qr.Q(QR_p) %*% s$u
  v <- qr.Q(QR_q) %*% s$v
  
  rownames(u) <- rownames(p)
  rownames(v) <- rownames(q[index_q,,drop = FALSE])
  p <- sweep(u, 2, sqrt(s$d), FUN = "*")
  q[index_q,] <- sweep(v, 2, sqrt(s$d), FUN = "*")
  
  return(list(mu = mu, a = a, b = b, p = p, q = q, 
              min_ratings = min_ratings,
              n_item = sapply(indexes_i, length),
              n_user = n_user,
              fitted = mu + a[user_id] + b[item_id] + pq))
}

