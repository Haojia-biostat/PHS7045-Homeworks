Homework 1
================
Haojia Li
2/6/23

``` r
rar_sim <- function(
    N             = 228, # total number of patients to be allocated in the trial
    interim       = 40, # size of each interim analysis when RAR is used
    trt_effect    = c(0.35, 0.45, 0.55, 0.65), # treatment effect
    alpha         = 0.35, # alpha for all the arms in beta distribution
    beta          = 0.65, # beta for all the arms in beta distribution
    K             = 1000, # number of draws in posterior distribution
    rar           = T, # logical flag of whether to use RAR (T) or equal allocation (F)
    delta         = ifelse(rar, 0.9892, 0.9912) # threshold to determine a successful trial
) {
  # function 1 - allocate participants
  # inputs: 
  #   1. n - total number of patients to be allocated
  #   2. alloc_prob - allocation probability
  # output:
  #   number of participants (nt) allocated to each arm in a vector
  allocation <- function(n, alloc_prob) {
    nt <- round(n*alloc_prob)
    # make sure the sum of allocated numbers equals to the input n
    if(sum(nt, na.rm = T) != n) nt[1] <- n - sum(nt[-1], na.rm = T)
    return(nt)
  }
  
  # function 2 - flip the coin and count the number of successes
  # inputs:
  #   1. nt - number of participants allocated to each arm in a vector
  #   2. pt - treatment effect for each arm
  # output:
  #   number of successes (sum_yt) in each arm
  rbinom_success <- function(nt, pt) {
    mapply(rbinom, nt, 1, pt, SIMPLIFY = F) |> sapply(sum)
  }
  
  # function 3 - draw 1000 times from the posterior distribution
  # inputs:
  #   1. cum_nt - cumulative number of participants allocated to each arm in a vector
  #   2. cum_sum_yt - cumulative number of successes in each arm
  # output:
  #   posterior probability in matrix with 1000 rows and 4 columns (post_mat)
  posterior_prob <- function(cum_nt, cum_sum_yt) {
    mapply(rbeta, K, alpha + cum_sum_yt, beta + cum_nt - cum_sum_yt)
  }
  
  # function 4 - calculate V0, V1, V2, and V3 and re-normalized to be the adaptive allocation prob
  # inputs:
  #   post_mat - posterior matrix
  #   nt - number of participants allocated to each arm in a vector
  # output:
  #   new allocation probability (alloc_prob), summing up to 1
  adapt_allocpr <- function(post_mat, nt) {
    vt <- sapply(2:4, \(i) mean(apply(post_mat, 1, which.max) == i))
    v0 <- min(c(sum(vt * (nt[-1]+1) / (nt[1]+1)), max(vt)))
    return(c(v0,vt)/sum(c(v0,vt)))
  }
  
  # function 5 - calculate the cumulative probability of picking successively better arms
  # input:
  #   post_mat - posterior matrix
  # output:
  #   probability that each treatment arm is better than control in a vector (pt_lt_p0)
  cum_prob_best <- function(post_mat) {
    best_arm <- apply(post_mat, 1, which.max)
    return(sapply(list(c(2,3,4), c(3,4), 4), \(x) mean(best_arm %in% x)))
  }
  
  # define result components
  res <- vector(mode = "list", length = 5)
  names(res) <- c(
    "Design",
    "Num. of patients allocated (nt)", "Num. of successes (sum_yt)", 
    "Cum. prob. of best", "Conclusion"
  )
  res$`Num. of patients allocated (nt)` <- res$`Num. of successes (sum_yt)` <- matrix(ncol = 4, nrow = 0)
  
  alloc_prob <- rep(0.25,4) # initial allocation probability regardless of design
  while(sum(res$`Num. of patients allocated (nt)`) < N) {
    if(rar) n <- min(interim, N - sum(res$`Num. of patients allocated (nt)`)) 
    else n <- N
    # 1. simulate nt and save it to `Num. of patients allocated (nt)`
    nt <- allocation(n, alloc_prob)
    res$`Num. of patients allocated (nt)` <- rbind(res$`Num. of patients allocated (nt)`, nt)
    # 2. simulate sum_yt and save it to `Num. of successes (sum_yt)`
    sum_yt <- rbinom_success(nt, trt_effect)
    res$`Num. of successes (sum_yt)` <- rbind(res$`Num. of successes (sum_yt)`, sum_yt)
    # 3. simulate posterior probability matrix based on cum_nt and cum_sum_yt
    post_mat <- posterior_prob(colSums(res$`Num. of patients allocated (nt)`), colSums(res$`Num. of successes (sum_yt)`))
    
    # 4. if the allocation for the whole trial has not been completed, calculate the adaptive allocation probability
    if(sum(res$`Num. of patients allocated (nt)`) < N) alloc_prob <- adapt_allocpr(post_mat, nt)
    # 5. else, calculate the probability that each treatment arm is better than control
    else res$`Cum. prob. of best` <- cum_prob_best(post_mat)
  }
  
  colnames(res$`Num. of patients allocated (nt)`) <- colnames(res$`Num. of successes (sum_yt)`) <- 
    c("Ctrl", "Arm1", "Arm2", "Arm3")
  rownames(res$`Num. of patients allocated (nt)`) <- rownames(res$`Num. of successes (sum_yt)`) <- 
    1:nrow(res$`Num. of patients allocated (nt)`)
  names(res$`Cum. prob. of best`) <- c("Third best", "Second best", "Best")
  if(rar) {
    res$Design <- "Response-adaptive randomization (RAR)"
    res$`Num. of patients allocated (nt)` <- addmargins(res$`Num. of patients allocated (nt)`)
    res$`Num. of successes (sum_yt)` <- addmargins(res$`Num. of successes (sum_yt)`)
  } else {
    res$Design <- "Equal allocation"
    res$`Num. of patients allocated (nt)` <- addmargins(res$`Num. of patients allocated (nt)`, 2)
    res$`Num. of successes (sum_yt)` <- addmargins(res$`Num. of successes (sum_yt)`, 2)
  }
  res$Conclusion <- paste0(
    "Comparing to the threshold of ", delta, ", the trial is a ", 
    ifelse(max(res$`Cum. prob. of best`) > delta, "success.", "failure.")
  )
  return(res)
}
```
