library(simtrait)
data<-sim_geno_trait_k3()
Y<-data$trait#generate trait data

N<-rep(0,1000)
for (j in 1:1000){
    #######
    n<-1000 #total number of locus will be tested
    X<-rep(0,1000) 

    #do the loop to generate 1000 x_i
    for (i in 1:1000){
        p<-0.5 # probability
        x<-rbinom(n=2,size=1,prob=p) #generate the x randomly
        e_x<-sum(x)/2 #expectation of x
        X[i]<-e_x
    } 

    mean_y<-mean(Y) 
    sigma_y<-sum((Y-mean_y)^2) #calculate the sigma^2

    V<-(1/2*n)*sum(X*(2-X)) #estimation of V
    sigma_c<-(1/n) * sqrt( 1/(2*p*(1-p)) -1 ) #estimation of sigma_c

    A_i<-rnorm(1, 0, 1)
    D_i<-rnorm(1, 1, 1/sqrt(2)*sigma_c) #normal approxiamtion of square root of C_i

    ratio<-A_i/D_i

    N[j]<-ratio
}


hist(N) #, xlim = c(-500,500)) #histgram loos not so good

qqnorm(N, pch = 1, frame = FALSE) #, ylim = c(-500,500))
qqline(N, col = "steelblue", lwd = 2)

x <- sort(N)
plot(x, dcauchy(x, location = 0, scale = 1), type="l",  lwd=2,
     main="", xlab="x", ylab="Probability Density") #, yaxt='n')
lines(density( N / sqrt(mean(N^2) )), lty =2, col = 'red')
