machineA=c(25,30,36,38,31)
machineB=c(31,39,38,42,35)
machineC=c(24,30,28,25,28)
corresponding_means=c(mean(machineA),mean(machineB),mean(machineC))
grand_mean=mean(corresponding_means)

design= data.frame(machineA,machineB,machineC)

print(design)

#speed_data=data.frame(machine=c(rep(c("A","B","C"),each=length(machineA))),speed=c(machineA,machineB,machineC))
#speed_data
speed_data=c(machineA,machineB,machineC)

#qqplot
theoritical_quantile=qnorm(ppoints(length(speed_data),a=3/8)) # ppoints=((1:n)-a)/(n+(1-a)-a) if n>10 a=0.5 else a=3/8.
qqplot(theoritical_quantile,speed_data) #plots thoritical quantile against sample quantile.
qqline(speed_data,col="red") #The qqline function calculates the slope and intercept based on the first and third quartiles of both the theoretical and data quantiles, and then draws the line.


##sum of squares

sum_of_sq_error=0
for(i in 1:ncol(design)){
  sum_of_sq_error=sum_of_sq_error+sum((design[,i]-mean(design[,i]))^2)
}
sum_of_sq_error

sum_of_sq_treatment=0
for(i in 1:ncol(design)){
  sum_of_sq_treatment=sum_of_sq_treatment+length(design[,i])*sum((mean(design[,i])-grand_mean)^2)
}
sum_of_sq_treatment

total_sum_of_sq=sum_of_sq_error+sum_of_sq_treatment

#degree of freedom

df_treatment=ncol(design)-1
df_error=ncol(design)*(length(design[,1])-1)
df_total=ncol(design)*length(design[,1])-1

#mean sum of squares
mean_sum_of_sq_error=sum_of_sq_error/df_error
mean_sum_of_sq_treatment=sum_of_sq_treatment/df_treatment

# f-statstic
f_statstic=round(mean_sum_of_sq_treatment/mean_sum_of_sq_error,3)
f_statstic

#critical f
critical_f=qf(0.05,df_treatment,df_error,lower.tail =FALSE)
critical_f

#p value
p_value = round(pf(f_statstic,df_treatment,df_error,lower.tail = FALSE),3)
p_value

#anova table

source_of_var=c("machine","error","total")
sum_of_sqs=c(sum_of_sq_treatment,sum_of_sq_error,total_sum_of_sq)
df=c(df_treatment,df_error,df_total)
mean_sum_of_sqs=c(round(mean_sum_of_sq_treatment,2),round(mean_sum_of_sq_error,2),"")
f=c(round(f_statstic,3),"","")
p_value=c(p_value,"","")

anova_table=data.frame(source_of_var,sum_of_sqs,df,mean_sum_of_sqs,f,p_value)
print(anova_table)

print(paste("Since f_stastic ",f_statstic,"> critcal_f ", {round(critical_f,2)},", there is sufficient evidence to reject the null hypothesis at 0.05 level of significance"))
print("There is a significant differnce between the average speed of machines")

      