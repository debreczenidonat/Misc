datalen <- 2000
data <- rbinom(datalen,1,0.3)
#success= 0

###############prior, we think its 0.2
x<-seq(from=0,to=1,length.out=1000)

success<-15
fail<-100

prior_d_vis <- dbeta(x,success,fail)
#plot(x,prior_d_vis)

prior <- pbeta((x),success,fail) - pbeta((x-0.001),success,fail)
#plot(x,prior)

############likelihood
x_<-seq(from=0,to=2000,by=1)
likelihood_vis <- dbinom(x_,2000,0.2)
#plot(x_,likelihood_vis)

x_<-seq(from=0,to=2000,by=1)
likelihood_vis_p <- pbinom(x_,2000,0.2)-pbinom(x_-1,2000,0.2)
#plot(x_,likelihood_vis_p)


likelihood <- pbinom(sum(data),length(data),x)-pbinom(sum(data),length(data),(x+0.001))

#plot(x,likelihood)

#############posterior

posterior<- prior*likelihood
post_sum <- sum(posterior,na.rm = TRUE)

posterior <- posterior/post_sum
plot(x,posterior)


par(mfrow=c(3,1))
plot(x,prior)
plot(x,likelihood)
plot(x,posterior)
