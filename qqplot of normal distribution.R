n=70
x=rnorm(n,0,1) # random data set from normal distrinution


x1=qnorm(((1:n)-0.5)/n,0,1) #generates 70 theoritcal quantiles

qqplot(x1,x,xlab="Theoritical quantile",ylab="sampple quantile")
qqline(x)



