library(Deriv)
sample_points=c(5.637941,4.942002,4.861254 ,3.469588 ,5.009333 ,7.702125 ,5.473228,
                3.613141 ,3.444167, 4.509174, 5.171716, 3.680117 ,2.365371, -4.959420,
                5.030187, 4.815630, 4.564628, 4.224900, 4.426912, 4.471680
)


n=length(sample_points)

log_likelihood<-function(x,theta){
  -1*sum(log(1+(x-theta)^2))
}

x0=median(sample_points)


deriv_likelihood=Deriv(log_likelihood,"theta")
second_deriv_likelihood=Deriv(deriv_likelihood,"theta")


fisher_info<-function(x,theta){
  -mean(second_deriv_likelihood(x,theta))
}


err=1
itr=1
while(itr<1000&err>0.00000001){
  x1=x0+(deriv_likelihood(sample_points,x0)/fisher_info(sample_points,x0))
  err=abs(x1-x0)
  itr=itr+1
  x0=x1
}
theta_hat=x1
print(theta_hat)

print(paste("The value of the maximum liklihood estimate is: ",theta_hat))

thired_deriv_likelihood=Deriv(second_deriv_likelihood,"theta")
print(thired_deriv_likelihood(sample_points,theta_hat)<0)
