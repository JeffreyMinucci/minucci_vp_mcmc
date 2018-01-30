## Code for example 6.1 - create generalized metropolis hastings algorithm, sample from beta(2.7,6.2) distribution

a <- 2.7; b <- 6.2; c <- 2.669 #initial values

Nsim <- 10000

X <- rep(runif(1),Nsim)  #initialize the chain

for(i in 2:Nsim){
  Y <- runif(1)  #generate new step in chain (proposal distribution is uniform [0,1])
  rho <- dbeta(Y,a,b)/dbeta(X[i-1],a,b) #ratio of liklihoods
  X[i] <- X[i-1] + (Y-X[i-1])*(runif(1) < rho) #accept the move with probablity rho
}

#visualize markov chain X[t]
plot((4500:4800), X[4500:4800], type = 'l')

#visualize stationary distribution (should be ~ beta(2.7,6.2))
par(mfrow=c(1,2))
hist(X,prob=T, breaks = 50)
lines(density(X))

actual <- rbeta(Nsim,a,b)
hist(actual,prob=T,breaks = 50)
lines(density(actual))
par(mfrow=c(1,1))

mean(X)
var(X)

mean(actual)
var(actual)

#acceptance rate = ~.45
length(unique(X))/length(X)


####### example 6.5 - random walk Metropolis-Hastings with a mixture mode

#Mixing model = 1/4 N(u1,1) + 3/4 N(u2,1), where u1 = 0 and u2 = 2.5
da <- rbind(rnorm(10^2),2.5+rnorm(3*10^2)) #sample from two normal distributions with means 0 and 2.5 (with second part being 3 times more weighted)
like <- function(mu){     #- log likelihood function for this mixture model 
  sum(log((.25*dnorm(da-mu[1])+ .75*dnorm(da-mu[2]))))
}

con_x <- con_y <- seq(-2,5,by=.01)
trials <- expand.grid(con_x,con_y)
z <- matrix(1,nrow=length(con_x),ncol=(length(con_y)))
for(i in 1:length(con_x)){
  for(j in 1:length(con_y)){
    z[i,j] <- like(c(con_x[i],con_y[j]))
  }
}

contour(con_x,con_y,z,nlevels=100)


# random walk monte carlo
scale <- .1  #can modify this. with scale = 1, it is common to only explore one mode (peak). At .1, acceptance is close to 45%
the<-matrix(runif(2,-2,5),ncol=2)
curlike <- hval <- like(the)
Niter <- 10^6
for(iter in 1:Niter){
  prop <- the[iter,]+rnorm(2)*scale
  if((max(-prop)>2) || (max(prop)>5) || (log(runif(1)) > like(prop) - curlike)){
    prop <- the[iter,]
  }
  curlike <- like(prop)
  hval <- c(hval,curlike)
  the <- rbind(the,prop)
}

lines(the[,1],the[,2],type='l', ylim=c(-2,5),xlim=c(-2,5),col="blue",lwd=2)

#acceptance rate
length(unique(hval))/length(hval)

#plot stationary distribution
library(hexbin)
h <- hexbin(the,xbins=30,ylim=c(-2,5))
plot(h)
the_plot <- data.frame(x = the[,1],y=the[,2])
h2 <- hexbinplot(y~x,data=the_plot,xbins=30,ylim=c(-2,5),xlim=c(-2,5))
plot(h2)
