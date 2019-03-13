## load packages we need to run the functions
library("readxl")
library(deSolve)
library(nloptr)
library(reshape2)
library(minpack.lm)
## set working directory 
# session -> set working directory-> choose directory
# click on FOLDER the file is in - you wont see the file
# # # ## # # #
# Import Data
# # # ## # # #
## Paste this from your existing code

# # # ## # # #
# Parameters
# # # ## # # #
r = .001 # 
i = 0.00001 # 
f=10 #
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
    dP=r*E*S-i*E*P# 
    dE=-i*E*P #
    list(c(dS,dP,dE))
  })
}

# # # ## # # #
# Calculate
# # # ## # # #
times = seq(from=0,to=tmax,by=15)
out = ode(y=yini,t=times,func=derivs,parms=c(r,i))
head(out) #

# # # ## # # #
# Plot inittial guess
# # # ## # # #
plot(out[,"time"],out[,"P"]*1,type="l",lwd='3',col="bisque4",xlab="label (units)",ylab="label (units)",sub="Figure description",xlim=c(0,tmax),ylim=c(0,60))
lines(t,averages,type='p')
arrows(t, (averages+std_dev),t, (averages-std_dev), length=0.05, angle=90, code=3)

# # # ## # # #
# parameter fitting using levenberg marquart algorithm
# # # ## # # #
ssq=function(p){
  # parameters from the parameter estimation routine
  f=p[3]
  r=p[1]
  i=p[2]
  print(p)
  # inital concentration
  cinit=c(S=as.numeric(5*f),P=0,E=as.numeric(1*f))
  # time points for which conc is reported
  # include the points where data is available
  tvs=c(seq(0,180,30),my_data$TIME)
  tvs=sort(unique(t))
  
  # solve ODE for a given set of parameters
  out=ode(y=cinit,t=tvs,func=derivs,parms=list(r,i))
  
  # Filter data that contains time points where data is available
  outdf=data.frame(out)
  outdf=outdf[outdf$time %in% my_data$TIME,]
  times = c(outdf$time,outdf$time,outdf$time)
  ys = c(outdf$P,outdf$P,outdf$P)
  
  # Evaluate predicted vs experimental residual
  preddf=data.frame(times,ys)
  
  expdf=melt(c(my_data$'Rep 1__1',my_data$'Rep 2__1',my_data$'Rep 3__1')*(40/maxconc),id.var="time",variable.name="species",value.name="conc")#(my_data$Rep 1__1,my_data$Rep 2__1,my_data$Rep 3__1,*(40/maxconc),id.var="time",variable.name="species",value.name="conc")
  ssqres=(preddf$ys-expdf$conc)
  print(sum(ssqres)^2)
  
  # return predicted vs experimental residual
  return(ssqres)
}

# initial guess for parameters
p=c(.001,.0001,10)
# initial ssq
initial_res=ssq(p)
sum(initial_res)^2

# fit model - return sum of squares and parameters at each iteration
fitval=nls.lm(par=p,lower=c(0.00001,0.000001,1),upper=c(0.1,.01,20),fn=ssq)
# improvement in fit:
(sum(initial_res)^2 - 41.4)/sum(initial_res)^2

# # # ## # # #
# Plot fitted model
# # # ## # # #
r = fitval$par[1]
i = fitval$par[2]
f = fitval$par[3]
cinit=c(S=as.numeric(5*f),P=0,E=as.numeric(1*f))
legend("topleft",legend=c("Initial Guess","Fitted Model"),col=c("bisque4","bisque2"),pch=c(1,1))
out=ode(y=cinit,times=t,func=derivs,parms=c(r,i))
lines(out[,"time"],out[,"P"]*1,type="l",lwd='3',col="bisque2")
outdf=data.frame(out)
names(outdf)=c("time","ca_pred","cb_pred","cc_pred")
