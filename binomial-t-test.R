source('sim_geno_trait_k3.R')

p <- 0.01 # probability
m <- 10000
n <- 100

# overkill way to get a genetic trait, though we don't use the simulated genotypes
data <- sim_geno_trait_k3( n_ind = n )
Y <- data$trait
mean_y <- mean(Y) #mean of y
sigma_y <- mean( (Y-mean_y)^2 ) #calculate the sigma^2 for y

### TODO: regularize denominator!!!

# construct null Binomial data, corresponding t-tests
t <- rep(0, m)
for (i in 1:m){
  x<-rbinom(n = n, size = 2, prob = p) #generate the x randomly
  numerator_t <- mean(x * (Y-mean_y)) # numerator of t_i
  denominator_t <- mean( x*(2-x) )
  denominator_t <- (1 + n * denominator_t) / (2 + n)
  denominator_t <- sqrt(2 * denominator_t) #denominator of t_i
  t[i] <- numerator_t / denominator_t * sqrt(n / sigma_y)
}

#t<-sort(t)
hist(t)

qqnorm(t, pch = 1, frame = FALSE) #, ylim = c(-500,500))
qqline(t, col = "steelblue", lwd = 2)
