Homework 2
================
Haojia Li
2/28/23

# Background

For this assignment, you’ll be quested with speeding up some code using
what you have learned about vectorization and Rcpp.

## Part 1: Vectorizing code

The following functions can be written to be more efficient without
using parallel computing:

1.  This function generates a `n x k` dataset with all its entries
    distributed Poisson with mean `lambda`.

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n*k, lambda), ncol = k)
}

# Benchmarking
bench::mark(
  fun1(),
  fun1alt(), relative = TRUE, check = FALSE
)
```

    # A tibble: 2 × 6
      expression   min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun1()      17.5   23.1       1        62.4     2.46
    2 fun1alt()    1      1        21.9       1       1   

2.  Like before, speed up the following functions (it is OK to use
    StackOverflow)

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  t(apply(mat, 1, cumsum))
}

# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
bench::mark(
  fun1(dat),
  fun1alt(dat), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression     min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>   <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun1(dat)     4.84   5.84      1         196.     7.28
    2 fun1alt(dat)  1      1         6.13        1      1   

``` r
# Test for the second
bench::mark(
  fun2(dat),
  fun2alt(dat), relative = TRUE
)
```

    # A tibble: 2 × 6
      expression     min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>   <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun2(dat)     4.55   3.45      1         1         NaN
    2 fun2alt(dat)  1      1         3.33      5.29      Inf

3.  Find the column max (hint: Check out the function `max.col()`).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  max_row_indices <- max.col(t(x))
  max_col <- NULL
  for(j in 1:ncol(x)) {
    max_col[j] <- x[max_row_indices[j], j]
  }
  return(max_col)
}

fun2alt1 <- function(x) {
  matrixStats::colMaxs(x)
}

# Benchmarking
bench::mark(
  fun2(x),
  fun2alt(x),
  fun2alt1(x), relative = TRUE
)
```

    # A tibble: 3 × 6
      expression    min median `itr/sec` mem_alloc `gc/sec`
      <bch:expr>  <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    1 fun2(x)     21.9   22.8       1         1        6.18
    2 fun2alt(x)   7.56   8.88      2.58      2.87     5.36
    3 fun2alt1(x)  1      1        22.2       1.41     1   

## Part 2: Rcpp code

As we saw in the Rcpp week, vectorization may not be the best solution.
For this part, you must write a function using Rcpp that implements the
propensity score matching algorithm. You can use [Week 5’s
lab](https://github.com/UofUEpiBio/PHS7045-advanced-programming/issues/8#issuecomment-1424974938)
as a starting point for the problem. Your C++ file should look something
like the following:

``` rcpp
#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List psmatch(
    NumericVector pscores,
    LogicalVector is_treated
) {
  
  // check for lengths
  if (pscores.size() != is_treated.size())
    stop("Error: the length of two inputs are not identical");
  
  // check whether both treated and untreated individuals are included
  int n = static_cast<int>(pscores.size());
  if (sum(is_treated) == n || sum(is_treated) == 0)
    stop("Error: there should be at least one individual in both group");

  // setup the problem creating the output
  IntegerVector indices(n);
  NumericVector values(n);
  values.fill(std::numeric_limits< double >::max());
  
  // Implement matching
  for (int i = 0; i < n; ++i) {
    
    // Instead of allocating new memory, we can point by reference
    // (saves operations)
    double & cur_best = values[i]; 
    auto   & cur_i    = indices[i];
    
    for (int j = 0; j < i; ++j) {
      
      // skip to the next if the two pscore compared belong to the same group
      if (is_treated[i] == is_treated[j])
        continue;
      
      // If it is lower, then update
      double d = std::abs(pscores[i] - pscores[j]);
      if (d < cur_best) {
        cur_best = d;
        cur_i    = j;
      }
      if (d < values[j]) {
        values[j] = d;
        indices[j] = i;
      }
      
    }
    
  }
  for (int i = 0; i < n; ++i) 
    values[i] = pscores[indices[i]];
  
  // Returning
  return List::create(
    _["match_id"] = indices + 1, // We add one to match R's indices
    _["match_pscore"] = values
  );
  
}
```
