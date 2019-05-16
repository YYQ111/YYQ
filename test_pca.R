#######
n<-1000 #total number of locus will be tested


#do the loop to generate 1000 x_i

p<-0.5 # probability
x<-rbinom(n=1000,size=2,prob=p) #generate the x randomly
e_x<-sum(x)/2 #expectation of x
X[i]<-e_x


mean_y<-mean(Y) #mean of y
sigma_y<-sum((Y-mean_y)^2) #calculate the sigma^2 for y
sigma_c<-(1/n) * sqrt( 1/(2*p*(1-p)) -1 ) #estimation of sigma_c

A_i<-rnorm(1000, 0, 1)
D_i<-rnorm(1000, 1, 1/sqrt(2)*sigma_c) #normal approxiamtion of square root of C_i

ratio<-A_i/D_i

ratio<-sort(ratio) #sort the ratio to generate the plot


hist(ratio) #, xlim = c(-500,500)) #histgram loos not so good

qqnorm(ratio, pch = 1, frame = FALSE) #, ylim = c(-500,500))
qqline(ratio, col = "steelblue", lwd = 2)

x <- sort(ratio)
plot(x, dcauchy(x, location = 0, scale = 1), type="l",  lwd=2,
     main="", xlab="x", ylab="Probability Density") #, yaxt='n')
lines(density( ratio / sqrt(mean(ratio^2) )), lty =2, col = 'red')


source('distr_norm_ratio_rgls.R')

# number of replicates
n <- 1000
# variance of second variable



x1 <- rnorm( n )
# denominators are Normal with mean 1 and specified variance
y1 <- rnorm( n, mean = 1, sd = sqrt(v) )
# the final ratio statistics we want
z1 <- x1 / y1
# histogram verifies correctness
hist(z1, freq = FALSE, breaks = 100)
z_range <- max( abs( z1 ) )
# when the mean of y is much larger than its variance, we expect z to be Normal with mean zero and stddev 1/n
xq <- (-499:500)/999 * z_range
yq <- dnorm(xq, mean = 0, sd = 1 )
lines(xq, yq, col = 'red', lty = 2)

yq2 <- d_norm_ratio_rgls(xq, v = v)
lines(xq, yq2, col = 'blue', lty = 2)
lines(xq,ratio,col="yellow", lty=2)
# cumulative tests
p_norm_ratio_rgls(Inf, v = v)
# [1] 1 # as desired!

# works with vector inputs!
p_norm_ratio_rgls(c(0, Inf), v = v)
# [1] 0.5 1.0 # also perfect!

# p-p plots of the data!
# expected cumulative probabilities
p_exp <- p_norm_ratio_rgls(z1, v = v)
#intergation converges to infinity becasue of the low value of v


t<-rep(0,1:10000)
for (i in 1:10000){
x<-rbinom(n=1000,size=2,prob=p) #generate the x randomly
numerator_t<-(1/n)*sum(x*(y-mean_y)) # numerator of t_i
denominator_t<-sqrt((2/n)*sum(x*(2-x))) #denominator of t_i
t[i]<-numerator_t/denominator_t
}

t<-sort(t)
hist(t)


