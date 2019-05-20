f <- function(
    delta = .5
  , alpha = 0
  , beta = 0
  , D = 2
  , M = 40000
  , missing_type = 1
  , seed = 4134
  , output = "delta"
  , mu = NA
  , sigma = NA
  , Z = NA
  , X = NA
){
  N <- M + 10000
  set.seed(seed)
  G <- rep(c(1,-1), N/2)
  mu <- list(rep(0,D), rep(delta*c(1,-1),D/2)/(1:D))
  sigma <- diag(rep(1,D))
  for(i in 2:D){
    for(j in 1:(i-1)){
      sigma[i,j] <- sigma[j,i] <- .2/abs(i-j)
    }
  }
  sig_inv <- solve(sigma)
  R_sig_inv <- chol(sig_inv)
  X <- matrix(rnorm(N*D),N,D) %*% chol(sigma) + 
    (G == -1) * matrix(1,N,1) %*% t(mu[[2]])
  
  g <- function(X){
    2 * X %*% sig_inv %*% (mu[[1]] - mu[[2]]) + as.numeric(-t(mu[[1]]) %*% sig_inv %*% mu[[1]] + t(mu[[2]]) %*% sig_inv %*% mu[[2]])
  }
  
  if(output == "delta") return(mean(sign(g(X)) == G))
  
  Z <- if(missing_type %in% c(1, 3)){ 
    X[,1]
  }else if(missing_type %in% c(2, 4)){
    X[,2]
  }
  ZZ <- G
  if(missing_type %in% c(1, 2)) ZZ <- 1
  rlp <- function(a){ 1 / (1 + exp(-alpha - as.numeric(beta * Z * ZZ)))}
  if(output == "alpha") return(mean(rlp(alpha)))
  R <- rbinom(N, 1, rlp(X))
  
  
  out <- list()
  bd <- as.data.frame(cbind(G, X))
  names(bd) <- c("y", paste0("x",1:D))
  
  out$W <- out$X <- bd[1:M, ] 
  out$X[R[1:M] == 1, 2] <- NA
  out$V <- bd[-c(1:M), ]
  out$bc <- g
  out$settings <- data.frame(
      delta = delta
    , alpha = alpha
    , beta = beta
    , missing_type = missing_type
    , D = D
    , M = M
    , seed = seed
    , obs_missing_pct = mean(is.na(out$X[,2]))
    , obs_bayes_risk = 1 - mean(sign(out$bc(as.matrix(out$V[,-1]))) == out$V[,1])
    , stringsAsFactors = FALSE
  )
  return(out)
}


get_data <- function(
    N
  , D
  , signal
  , beta
  , missing_type
  , missing_pct
  , seed
  , delta = NA
  , alpha = NA
){

if(is.na(delta)){
  dd <- function(x){f(delta = x, D = D, output = "delta") - signal}
  delta <- uniroot(dd, c(0, 20))$root
} 

if(is.na(alpha)){
  da <- function(x){ 
    f(
        delta
      , alpha = x
      , beta = beta
      , D = D
      , output = "alpha"
      , missing_type = missing_type
    ) - missing_pct 
  }
  alpha <- uniroot(da, c(-20,20))$root
}

out <- f(delta = delta, alpha = alpha, beta = beta, D = D, M = N, output = "data", seed = seed, missing_type = missing_type)
out$settings$target_signal <- signal
out$settings$target_missing_pct <- missing_pct
return(out)
}

single_sim <- function(
    N
  , D
  , signal
  , beta
  , missing_type
  , missing_pct
  , seed
  , delta
  , alpha
){
  a <- try({
    data <- get_data(
        N
      , D
      , signal
      , beta
      , missing_type
      , missing_pct
      , seed
      , delta
      , alpha
    )

    cc_flag <- !is.na(data$X[,2])
    svm1 <- best.svm(
        x = data$X[cc_flag,-1]
      , y = factor(data$X[cc_flag,1])
      , type = "C-classification"
      , kernel = "linear"
      , cost = 2^(-5:5)
      )
    
    svm2 <- best.svm(
        x = data$W[,-1]
      , y = factor(data$W[,1])
      , type = "C-classification"
      , kernel = "linear"
      , cost = 2^(-5:5)
    )
    
    data$settings$pe_o <- 1 - mean(predict(svm2, data$V[,-1]) == data$V[,1])
    data$settings$pe_c <- 1 - mean(predict(svm1, data$V[,-1]) == data$V[,1])
    data$settings$peao <- data$settings$pe_c - data$settings$pe_o 
    
    data$settings
  }, FALSE)

  if(is.data.frame(a)) return(a)
  else return(NULL)
}

# 
# for(i in 1:nrow(sim_settings)){
#    ss1 <- single_sim(
#       N = sim_settings[i, "M"]
#     , D = sim_settings[i, "D"]
#     , signal = sim_settings[i, "signal"]
#     , beta = sim_settings[i, "beta"]
#     , missing_type = sim_settings[i, "missing_type"]
#     , missing_pct = sim_settings[i, "missing_pct"]
#     , seed = sim_settings[i, "seed"]
#   )
#   
#   if(is.null(ss1)) next
#    
#   sim_settings[i,"peao"] <- ss1$peao
#   sim_settings[i,"pe_o"] <- ss1$pe_o
#   sim_settings[i,"pe_c"] <- ss1$pe_c
# }


