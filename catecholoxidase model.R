#install.packages(c("readxl","deSolve"))
library("readxl")
library(deSolve)

# # # ## # # #
# Import Data
# # # ## # # #
my_data <- read_excel("experiment2_template (1).xlsx", sheet = "Data Entry",range="A2:Z9")
maxconc=max(my_data$Mean__1)
averages=my_data$Mean__1*(40/maxconc) #
std_dev=my_data$`Std Dev__1`*(40/maxconc) #
t=my_data$TIME

# # # ## # # #
# Parameters
# # # ## # # #
r = .0035 # 
f=8 #
S_mL=5 #
E_mL=1 #
ce = E_mL*f # 
se = S_mL*f # 
tmax = 180 # 

# # # ## # # #
# Equations
# # # ## # # #
yini=c(S=se,P=0,E=ce) #
derivs = function(t,y,parms){
  with(as.list(y),{
    dS=-r*E*S # 
    dP=r*E*S # 
    dE=0 #
    list(c(dS,dP,dE))
  })
}

# # # ## # # #
# Calculate
# # # ## # # #
times = seq(from=0,to=tmax,by=15)
out = ode(y=yini,t=times,func=derivs,parms=c(r))
head(out) #

# # # ## # # #
# 
# # # ## # # #
par(bg = 'gray96')
plot(out[,"time"],out[,"P"]*1,type="l",lwd='3',col="bisque4",xlab="label (units)",ylab="label (units)",sub="Figure description",xlim=c(0,tmax),ylim=c(0,60))
lines(out[,"time"],out[,"S"]*1,type="l",lwd='3',col="rosybrown1")
lines(t,averages,type='p')
arrows(t, (averages+std_dev),t, (averages-std_dev), length=0.05, angle=90, code=3)
legend("right",legend=c("1","2"),col=c("bisque4","rosybrown1"),pch=c(1,1))

