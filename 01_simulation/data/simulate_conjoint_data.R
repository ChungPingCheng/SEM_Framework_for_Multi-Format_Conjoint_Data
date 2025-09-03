library(MASS,purrr)

# Simulate data for an untegrated SEM framework in conjoint analysis
# Includes: rating, binary, pairwise, ranking, categorical choice and mixed format data

set.seed(20250726)

# Basic setup
N <- 500  # number of participants
p <- 4    # number of products
k <- 3    # number of attributes per product + 1

# Define product attribute matrix (effect-coded attributes for 4 products)
T <- matrix(c(
  1, -1, -1,  # Product 1: chocolate, simple
  1, 1, -1,  # Product 2: vanilla, simple
  1, -1, 1,  # Product 3: chocolate, deluxe
  1, 1, 1   # Product 4: vanilla, deluxe
), nrow = p, byrow = TRUE)

colnames(T) <- c("F0","F1", "F2")

# Latent part worth: MVN(mu, sigma)
mu <- c(3, 0.5, 0.5)
corr <- matrix(c(1, 0.3, 0.3, 0.3, 1,0.3,0.3,0.3,1), nrow = 3)
variances <- c(1, 1, 2.25)
std_dev_mat <- diag(sqrt(variances))
sigma <- std_dev_mat %*% corr %*% std_dev_mat


eta <- MASS::mvrnorm(N, mu = mu, Sigma = sigma)
errsigma <- 1

# Generate latent preference y_star for all participants and products
y_stru2 <- matrix(NA, nrow = N, ncol = p)
y_stru3 <- matrix(NA, nrow = N, ncol = p)
y_star2 <- matrix(NA, nrow = N, ncol = p)
y_star3 <- matrix(NA, nrow = N, ncol = p)
for (i in 1:N) {
  for (j in 1:p) {
    y_stru2[i, j] <- sum(T[j,2:3 ] * eta[i,2:3 ]) 
    y_stru3[i, j] <- sum(T[j, ] * eta[i,]) 
    e <- rnorm(1, mean = 0, sd = sigma)
    y_star2[i, j] <- y_stru2[i, j] + e
    y_star3[i, j] <- y_stru3[i, j] + e
  }
}

# 1. Rating data: direct observation of y*
rating_data <- y_star3
rating_data <- as.data.frame(rating_data)
names(rating_data) <- c('RT1','RT2','RT3','RT4')

# 2. Binary choice: y = 1 if y* > 0, else 0
binary_data <- ifelse(y_star3 > 3.2, 1, 0)
binary_data <- as.data.frame(binary_data)
names(binary_data) <- c('B1','B2','B3','B4')


# 3. Pairwise comparisons: matrix of comparisons (1 = prefer j1, 0 = prefer j2)
pairwise_data <- matrix(NA, nrow = N, ncol = choose(p, 2))
colnames(pairwise_data) <- combn(p, 2, FUN = function(x) paste0("P", x[1], "_", x[2]))
pair_index <- combn(p, 2)
for (i in 1:N) {
  for (c in 1:ncol(pair_index)) {
    j1 <- pair_index[1, c]
    j2 <- pair_index[2, c]
    pairwise_data[i, c] <- ifelse(y_star2[i, j1] - y_star2[i, j2]+rnorm(1,0,1)>0, 1, 0)
  }
}

# 4. Ranking data: rank products from highest to lowest y*
ranking_data <- t(apply(y_star2, 1, function(x) rank(-x, ties.method = "first")))
ranking_data <- as.data.frame(ranking_data)
names(ranking_data) <- c('RK1','RK2','RK3','RK4')

# 5. Categorical choices from 4 choice sets of 3 randomly selected products
# Simulate 4 choice sets per participant
choice_sets <- combn(1:4, 3, simplify = FALSE)
categorical_choices <- matrix(NA, nrow = N, ncol = 4)
for (i in 1:N) {
  for (s in 1:4) {
    options <- choice_sets[[s]]
    chosen <- which.max(y_star2[i, options]+rnorm(3,0,1))
    categorical_choices[i, s] <- options[chosen]
  }
}
categorical_choices <- as.data.frame(categorical_choices)
names(categorical_choices) <- c('C1','C2','C3','C4')

ranking_to_pairwise <- function(ranks) {
  p <- ncol(ranks)
  pair_index <- combn(p, 2)
  pairwise_matrix <- matrix(NA, nrow = nrow(ranks), ncol = ncol(pair_index))
  colnames(pairwise_matrix) <- apply(pair_index, 2, function(x) paste0("P", x[1], "_", x[2]))

  for (i in 1:nrow(ranks)) {
    for (c in 1:ncol(pair_index)) {
      j1 <- pair_index[1, c]
      j2 <- pair_index[2, c]
      rank1 <- ranks[i, j1]
      rank2 <- ranks[i, j2]
      pairwise_matrix[i, c] <- ifelse(rank1 < rank2, 1, 0)  # lower rank = preferred
    }
  }
  return(pairwise_matrix)
}
ranking_pairwise <- as.data.frame(ranking_to_pairwise(ranking_data))
names(ranking_pairwise) <- paste0('RK',names(ranking_pairwise))

# Convert categorical choices into inferred pairwise (incomplete) comparisons
categorical_to_pairwise <- function(choices, sets) {
  N <- nrow(choices)
  nsets <- length(sets)
  p <- max(unlist(sets))
  pair_index <- combn(p, 2)
  pair_names <- apply(pair_index, 2, function(x) paste0("P", x[1], "_", x[2]))

  pairwise_list <- vector("list", N)
  for (i in 1:N) {
    inferred <- rep(NA, length(pair_names))
    names(inferred) <- pair_names
    for (s in 1:nsets) {
      opts <- sets[[s]]
      choice <- choices[i, s]
      for (o in opts) {
        if (o != choice) {
          pair_name <- if (choice < o) paste0("P", choice, "_", o) else paste0("P", o, "_", choice)
          inferred[pair_name] <- if (choice < o) 1 else 0
        }
      }
    }
    pairwise_list[[i]] <- inferred
  }
  pairwise_matrix <- do.call(rbind, pairwise_list)
  return(pairwise_matrix)
}

# Create the choice_sets (as in simulation)
categorical_pairwise <- as.data.frame(categorical_to_pairwise(categorical_choices, choice_sets))
names(categorical_pairwise) <- paste0('C',names(categorical_pairwise))
categorical_pairwise[is.na(categorical_pairwise)] <- 999

write.table(rating_data,'rating_data.txt',row.names=F,col.names=F)
write.table(binary_data,'binary_data.txt',row.names=F,col.names=F)
write.table(pairwise_data,'pairwise_data.txt',row.names=F,col.names=F)
write.table(cbind(ranking_data,ranking_pairwise),'ranking_data.txt',row.names=F,col.names=F)
write.table(cbind(categorical_choices,categorical_pairwise),'choices_data.txt',row.names=F,col.names=F)
write.table(cbind(binary_data,pairwise_data),'binary_pairwise_data.txt',row.names=F,col.names=F)


