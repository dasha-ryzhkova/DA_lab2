library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)
library(boot)
library(MASS)
install.packages('MASS')


#cor.test(~ adr + lead_time, data = data, method = c("pearson"))


adr_data <- data %>%  select(c(adr, booking_changes, total_of_special_requests, 
                                   required_car_parking_spaces, stays_in_nights, lead_time))
adr_data <- adr_data[with(adr_data, order(-adr)), ]

adr_data <- mutate(adr_data, total_requests = booking_changes + total_of_special_requests
                   + required_car_parking_spaces)

ggcorr(adr_data,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       palette = "PuOr")


ggcorr(adr_data, 
       label = TRUE,
       method = c("pairwise", "spearman"))



################
## Pearson
################
# booking_changes
adr_data <- data %>%  select(c(adr, booking_changes))
cor.test(~ adr + booking_changes, data = data, method = c("pearson"))

n <- 100
B <- 2000

Sigma <- cor(adr_data, method = "pearson", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))

# total_of_special_requests

adr_data <- data %>%  select(c(adr, total_of_special_requests))
cor.test(~ adr + total_of_special_requests, data = data, method = c("pearson"))

n <- 100
B <- 2000

Sigma <- cor(adr_data, method = "pearson", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))


# required_car_parking_spaces

adr_data <- data %>%  select(c(adr, required_car_parking_spaces))
cor.test(~ adr + required_car_parking_spaces, data = data, method = c("pearson"))

n <- 100
B <- 2000

Sigma <- cor(adr_data, method = "pearson", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))


# stays_in_nights
adr_data <- data %>%  select(c(adr, stays_in_nights))
cor.test(~ adr + stays_in_nights, data = data, method = c("pearson"))

n <- 100
B <- 2000

Sigma <- cor(adr_data, method = "pearson", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))


# lead_time
adr_data <- data %>%  select(c(adr, lead_time))
cor.test(~ adr + lead_time, data = data, method = c("pearson"))

n <- 100
B <- 2000
Sigma <- cor(adr_data, method = "pearson", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))


################
## Spearman
################
# booking_changes
adr_data <- data %>%  select(c(adr, booking_changes))
cor.test(~ adr + booking_changes, data = data, method = c("spearman"))

n <- 100
B <- 2000

Sigma <- cor(adr_data, method = "spearman", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))

# total_of_special_requests, 

adr_data <- data %>%  select(c(adr, total_of_special_requests))
cor.test(~ adr + total_of_special_requests, data = data, method = c("spearman"))

n <- 100
B <- 2000

Sigma <- cor(adr_data, method = "spearman", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))


# required_car_parking_spaces

adr_data <- data %>%  select(c(adr, required_car_parking_spaces))
cor.test(~ adr + required_car_parking_spaces, data = data, method = c("spearman"))

n <- 100
B <- 2000

Sigma <- cor(adr_data, method = "spearman", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))


# stays_in_nights, lead_time
adr_data <- data %>%  select(c(adr, stays_in_nights))
cor.test(~ adr + stays_in_nights, data = data, method = c("spearman"))

n <- 100
B <- 2000
Sigma <- cor(adr_data, method = "spearman", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))


# lead_time
adr_data <- data %>%  select(c(adr, lead_time))
cor.test(~ adr + lead_time, data = data, method = c("spearman"))

n <- 100
B <- 2000
Sigma <- cor(adr_data, method = "spearman", use = "complete.obs")

boot_cor_with_sd <- function(X, indices, estimate_var = TRUE){
  cor_bar <- cor(X[indices, ])[1, 2]
  if (estimate_var){
    boot_out <- boot(X[indices, ], statistic = boot_cor_with_sd, R = 200, estimate_var = FALSE)
    return(c(cor_bar, var(boot_out$t[, 1])))
  }
  else {
    return(cor_bar)
  }
}

set.seed(100)
X <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
boot_result_cor <- boot(X, statistic = boot_cor_with_sd, R = B)
boot.ci(boot.out = boot_result_cor, index = c(1, 2))

