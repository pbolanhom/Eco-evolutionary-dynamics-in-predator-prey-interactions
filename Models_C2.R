#Packages
library(deSolve)
#Model 1. No Search Image
m3.sys.b = function(t,y,parms){
  with(as.list(c(y,parms)),
       {
         dg=(a+b*z)*(1-g)^2-g*(Sj*(1-g)+((mu0*(1-mu0)+z)/(1+z-mu0)))-N*(1-g)*g*(lj-(k0+k1*z))+P*z*g*(2*f*(g-1)+(1-g))/(1+h*z*N*(g*f+(1-g)*(1-f)))
         dN=N*((a+b*z)*(1-g)-Sj*g-N*(g*lj+(k0+k1*z)*(1-g))-P*z*(g*f+(1-g)*(1-f))/(1+h*z*N*(g*f+(1-g)*(1-f))))
         dP=P*(e*N*z*(g*f+(1-g)*(1-f))/(1+h*z*N*(g*f+(1-g)*(1-f)))-d)
         dz=V*exp(-s/(z-s))*(b*(1-g)-N*k1*(1-g)-P*(g*f+(1-g)*(1-f))/(1+h*z*N*(g*f+(1-g)*(1-f))))
         dr=b*(1-g)*(1-N*(g*lj+(k0+k1*z)*(1-g))/((a+b*z)*(1-g)-Sj*g))
         dK=N*(b*(1-g)*(g*lj+(k0+k1*z)*(1-g))/((a+b*z)*(1-g)-Sj*g)-k1*(1-g))
         return(list(c(g=dg,N=dN,P=dP,z=dz,dr=dr,dr=dK)))
       })
}
#Parameters
#parameters=c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0,e=1,h=1.5,d=0.5,s=0.0001,V=0.05)
parameters=c(a=2.2,b=0.8,Sj=0,k0=0.5,k1=0.25,lj=0,mu0=0.5,f=0.5,e=1,h=1,d=0.5,s=0.0001,V=0.05) 

#Time
Time=seq(from=1,to=10000,by=0.1)
#Initial Conditions
n0.b = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
#Solving the Systems
m3.sys.b.out=ode(y=n0.b,times=Time,func=m3.sys.b,parms=parameters)
#Time Series
par(mfrow=c(1,1))
#populations
matplot(x=m3.sys.b.out[,1],y=m3.sys.b.out[,c(3:4)],type = "l",lwd=1,lty=c(1,2),col="black",
        xlab="Time",ylab="Population Density",frame=T, main="Model 3 b)")
legend("topright", legend = c("Prey","Predator"),lty=c(1,2), lwd=1, col="black",cex=0.7)

#trait
matplot(x=m3.sys.b.out[,1],y=m3.sys.b.out[,5],type = "l",lwd=1,lty=1,
        xlab="Time",ylab="Trait Value",frame=T,main="Model 3 b)")



#proportion of juveniles
matplot(x=m3.sys.b.out[,1],y=m3.sys.b.out[,2],type = "l",lwd=1,lty=1,
        xlab="Time",ylab="Proportion of Juveniles",frame=T,main="Model 3 b)")


#rK dynamics
matplot(x=m3.sys.b.out[,1],y=log(1+m3.sys.b.out[,c(6:7)]),type = "l",lwd=1,lty=1,col=c("black","azure4"),
        xlab="Time",ylab="Selection Strength",frame=T,main="Model 3 b)")
legend("topright", legend = c("r","K"), 
       lty = 1, lwd=1, col=c("black","azure4"),cex=0.7)


################################################Bifurcations#################################################################

#Predator Foraging Effort

#bifurcation for f in FULL MODEL
ff = seq(from = 0, to=1, by=0.1)
Nminmax.f = matrix(NA, ncol=2, nrow=length(ff))#prey minimum and maximum
Pminmax.f = matrix(NA, ncol=2, nrow=length(ff))#predator minimux ans maximum
zminmax.f = matrix(NA, ncol=2, nrow=length(ff))#trait minum and maximum
gminmax.f = matrix(NA, ncol=2, nrow=length(ff))
rminmax.f = matrix(NA, ncol=2, nrow=length(ff))
Kminmax.f = matrix(NA, ncol=2, nrow=length(ff))
for(i in 1:length(ff)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=ff[i],e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.f[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.f[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.f[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.f[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.f[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.f[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.f$mean=rowMeans(rminmax.f, na.rm=TRUE)
Kminmax.f$mean=rowMeans(Kminmax.f, na.rm=TRUE) 


##########################################################################################################################################
#Bifurcation of h
#for f=0

hh = seq(from = 0, to=2, by=0.1)
Nminmax.hf0 = matrix(NA, ncol=2, nrow=length(hh))#prey minimum and maximum
Pminmax.hf0 = matrix(NA, ncol=2, nrow=length(hh))#predator minimux ans maximum
zminmax.hf0 = matrix(NA, ncol=2, nrow=length(hh))#trait minum and maximum
gminmax.hf0 = matrix(NA, ncol=2, nrow=length(hh))
rminmax.hf0 = matrix(NA, ncol=2, nrow=length(hh))
Kminmax.hf0 = matrix(NA, ncol=2, nrow=length(hh))
for(i in 1:length(hh)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0,e=1,h=hh[i],d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.hf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.hf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.hf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.hf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.hf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.hf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.hf0$mean=rowMeans(rminmax.hf0, na.rm=TRUE)
Kminmax.hf0$mean=rowMeans(Kminmax.hf0, na.rm=TRUE)

######################################################################################
#for f=0.5

hh = seq(from = 0, to=2, by=0.1)
Nminmax.hf05 = matrix(NA, ncol=2, nrow=length(hh))#prey minimum and maximum
Pminmax.hf05 = matrix(NA, ncol=2, nrow=length(hh))#predator minimux ans maximum
zminmax.hf05 = matrix(NA, ncol=2, nrow=length(hh))#trait minum and maximum
gminmax.hf05 = matrix(NA, ncol=2, nrow=length(hh))
rminmax.hf05 = matrix(NA, ncol=2, nrow=length(hh))
Kminmax.hf05 = matrix(NA, ncol=2, nrow=length(hh))
for(i in 1:length(hh)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0.5,e=1,h=hh[i],d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.hf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.hf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.hf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.hf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.hf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.hf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.hf05$mean=rowMeans(rminmax.hf05, na.rm=TRUE)
Kminmax.hf05$mean=rowMeans(Kminmax.hf05, na.rm=TRUE)

######################################################################################
#for f=1

hh = seq(from = 0, to=2, by=0.1)
Nminmax.hf1 = matrix(NA, ncol=2, nrow=length(hh))#prey minimum and maximum
Pminmax.hf1 = matrix(NA, ncol=2, nrow=length(hh))#predator minimux ans maximum
zminmax.hf1 = matrix(NA, ncol=2, nrow=length(hh))#trait minum and maximum
gminmax.hf1 = matrix(NA, ncol=2, nrow=length(hh))
rminmax.hf1 = matrix(NA, ncol=2, nrow=length(hh))
Kminmax.hf1 = matrix(NA, ncol=2, nrow=length(hh))
for(i in 1:length(hh)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=1,e=1,h=hh[i],d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.hf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.hf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.hf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.hf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.hf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.hf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.hf1$mean=rowMeans(rminmax.hf1, na.rm=TRUE)
Kminmax.hf1$mean=rowMeans(Kminmax.hf1, na.rm=TRUE)
##########################################################################################
#for f=0.2

hh = seq(from = 0, to=2, by=0.1)
Nminmax.hf02 = matrix(NA, ncol=2, nrow=length(hh))#prey minimum and maximum
Pminmax.hf02 = matrix(NA, ncol=2, nrow=length(hh))#predator minimux ans maximum
zminmax.hf02 = matrix(NA, ncol=2, nrow=length(hh))#trait minum and maximum
gminmax.hf02 = matrix(NA, ncol=2, nrow=length(hh))
rminmax.hf02 = matrix(NA, ncol=2, nrow=length(hh))
Kminmax.hf02 = matrix(NA, ncol=2, nrow=length(hh))
for(i in 1:length(hh)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0.2,e=1,h=hh[i],d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.hf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.hf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.hf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.hf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.hf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.hf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.hf02$mean=rowMeans(rminmax.hf02, na.rm=TRUE)
Kminmax.hf02$mean=rowMeans(Kminmax.hf02, na.rm=TRUE)
##########################################################################################
#for f=0.8

hh = seq(from = 0, to=2, by=0.1)
Nminmax.hf08 = matrix(NA, ncol=2, nrow=length(hh))#prey minimum and maximum
Pminmax.hf08 = matrix(NA, ncol=2, nrow=length(hh))#predator minimux ans maximum
zminmax.hf08 = matrix(NA, ncol=2, nrow=length(hh))#trait minum and maximum
gminmax.hf08 = matrix(NA, ncol=2, nrow=length(hh))
rminmax.hf08 = matrix(NA, ncol=2, nrow=length(hh))
Kminmax.hf08 = matrix(NA, ncol=2, nrow=length(hh))
for(i in 1:length(hh)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0.8,e=1,h=hh[i],d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.hf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.hf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.hf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.hf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.hf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.hf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.hf08$mean=rowMeans(rminmax.hf08, na.rm=TRUE)
Kminmax.hf08$mean=rowMeans(Kminmax.hf08, na.rm=TRUE)




#################################################################################################################################################




#######################################################################
#bifurcation of f for lj=0

ff = seq(from = 0, to=1, by=0.1)
Nminmax.fl0 = matrix(NA, ncol=2, nrow=length(ff))#prey minimum and maximum
Pminmax.fl0 = matrix(NA, ncol=2, nrow=length(ff))#predator minimux ans maximum
zminmax.fl0 = matrix(NA, ncol=2, nrow=length(ff))#trait minum and maximum
gminmax.fl0 = matrix(NA, ncol=2, nrow=length(ff))
rminmax.fl0 = matrix(NA, ncol=2, nrow=length(ff))
Kminmax.fl0 = matrix(NA, ncol=2, nrow=length(ff))
for(i in 1:length(ff)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=0,mu0=0.5,f=ff[i],e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.fl0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.fl0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.fl0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.fl0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.fl0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.fl0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.fl0$mean=rowMeans(rminmax.fl0, na.rm=TRUE)
Kminmax.fl0$mean=rowMeans(Kminmax.fl0, na.rm=TRUE)

#######################################################################
#bifurcation of f for Sj=0

ff = seq(from = 0, to=1, by=0.1)
Nminmax.fS0 = matrix(NA, ncol=2, nrow=length(ff))#prey minimum and maximum
Pminmax.fS0 = matrix(NA, ncol=2, nrow=length(ff))#predator minimux ans maximum
zminmax.fS0 = matrix(NA, ncol=2, nrow=length(ff))#trait minum and maximum
gminmax.fS0 = matrix(NA, ncol=2, nrow=length(ff))
rminmax.fS0 = matrix(NA, ncol=2, nrow=length(ff))
Kminmax.fS0 = matrix(NA, ncol=2, nrow=length(ff))
for(i in 1:length(ff)){
  parmsi = c(a=2.2,b=0.8,Sj=0,k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=ff[i],e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.fS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.fS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.fS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.fS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.fS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.fS0[i,] = range(outi.log[(nrow(outi.log)-2000):nrow(outi.log),7])
}
#rK mean values
rminmax.fS0$mean=rowMeans(rminmax.fS0, na.rm=TRUE)
Kminmax.fS0$mean=rowMeans(Kminmax.fS0, na.rm=TRUE)

#######################################################################
#bifurcation of f for lj=0 and S0=0

ff = seq(from = 0, to=1, by=0.1)
Nminmax.flS0 = matrix(NA, ncol=2, nrow=length(ff))#prey minimum and maximum
Pminmax.flS0 = matrix(NA, ncol=2, nrow=length(ff))#predator minimux ans maximum
zminmax.flS0 = matrix(NA, ncol=2, nrow=length(ff))#trait minum and maximum
gminmax.flS0 = matrix(NA, ncol=2, nrow=length(ff))
rminmax.flS0 = matrix(NA, ncol=2, nrow=length(ff))
Kminmax.flS0 = matrix(NA, ncol=2, nrow=length(ff))
for(i in 1:length(ff)){
  parmsi = c(a=2.2,b=0.8,Sj=0,k0=0.5,k1=0.25,lj=0,mu0=0.5,f=ff[i],e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.flS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.flS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.flS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.flS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.flS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.flS0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.flS0$mean=rowMeans(rminmax.flS0, na.rm=TRUE)
Kminmax.flS0$mean=rowMeans(Kminmax.flS0, na.rm=TRUE)


########################################################################################################################################
#Bifurcation of Sj
#for f=0

SJ = seq(from = 0, to=1, by=0.1)
Nminmax.SJf0 = matrix(NA, ncol=2, nrow=length(SJ))#prey minimum and maximum
Pminmax.SJf0 = matrix(NA, ncol=2, nrow=length(SJ))#predator minimux ans maximum
zminmax.SJf0 = matrix(NA, ncol=2, nrow=length(SJ))#trait minum and maximum
gminmax.SJf0 = matrix(NA, ncol=2, nrow=length(SJ))
rminmax.SJf0 = matrix(NA, ncol=2, nrow=length(SJ))
Kminmax.SJf0 = matrix(NA, ncol=2, nrow=length(SJ))
for(i in 1:length(SJ)){
  parmsi = c(a=2.2,b=0.8,Sj=SJ[i],k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.SJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.SJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.SJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.SJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.SJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.SJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.SJf0$mean=rowMeans(rminmax.SJf0, na.rm=TRUE)
Kminmax.SJf0$mean=rowMeans(Kminmax.SJf0, na.rm=TRUE)

##########################################################################
#for f=0.2

SJ = seq(from = 0, to=1, by=0.1)
Nminmax.SJf02 = matrix(NA, ncol=2, nrow=length(SJ))#prey minimum and maximum
Pminmax.SJf02 = matrix(NA, ncol=2, nrow=length(SJ))#predator minimux ans maximum
zminmax.SJf02 = matrix(NA, ncol=2, nrow=length(SJ))#trait minum and maximum
gminmax.SJf02 = matrix(NA, ncol=2, nrow=length(SJ))
rminmax.SJf02 = matrix(NA, ncol=2, nrow=length(SJ))
Kminmax.SJf02 = matrix(NA, ncol=2, nrow=length(SJ))
for(i in 1:length(SJ)){
  parmsi = c(a=2.2,b=0.8,Sj=SJ[i],k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0.2,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.SJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.SJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.SJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.SJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.SJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.SJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.SJf02$mean=rowMeans(rminmax.SJf02, na.rm=TRUE)
Kminmax.SJf02$mean=rowMeans(Kminmax.SJf02, na.rm=TRUE)
##########################################################################
#for f=0.5

SJ = seq(from = 0, to=1, by=0.1)
Nminmax.SJf05 = matrix(NA, ncol=2, nrow=length(SJ))#prey minimum and maximum
Pminmax.SJf05 = matrix(NA, ncol=2, nrow=length(SJ))#predator minimux ans maximum
zminmax.SJf05 = matrix(NA, ncol=2, nrow=length(SJ))#trait minum and maximum
gminmax.SJf05 = matrix(NA, ncol=2, nrow=length(SJ))
rminmax.SJf05 = matrix(NA, ncol=2, nrow=length(SJ))
Kminmax.SJf05 = matrix(NA, ncol=2, nrow=length(SJ))
for(i in 1:length(SJ)){
  parmsi = c(a=2.2,b=0.8,Sj=SJ[i],k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0.5,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.SJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.SJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.SJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.SJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.SJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.SJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.SJf05$mean=rowMeans(rminmax.SJf05, na.rm=TRUE)
Kminmax.SJf05$mean=rowMeans(Kminmax.SJf05, na.rm=TRUE)


##########################################################################
#for f=0.8

SJ = seq(from = 0, to=1, by=0.1)
Nminmax.SJf08 = matrix(NA, ncol=2, nrow=length(SJ))#prey minimum and maximum
Pminmax.SJf08 = matrix(NA, ncol=2, nrow=length(SJ))#predator minimux ans maximum
zminmax.SJf08 = matrix(NA, ncol=2, nrow=length(SJ))#trait minum and maximum
gminmax.SJf08 = matrix(NA, ncol=2, nrow=length(SJ))
rminmax.SJf08 = matrix(NA, ncol=2, nrow=length(SJ))
Kminmax.SJf08 = matrix(NA, ncol=2, nrow=length(SJ))
for(i in 1:length(SJ)){
  parmsi = c(a=2.2,b=0.8,Sj=SJ[i],k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=0.8,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.SJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.SJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.SJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.SJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.SJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.SJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.SJf08$mean=rowMeans(rminmax.SJf08, na.rm=TRUE)
Kminmax.SJf08$mean=rowMeans(Kminmax.SJf08, na.rm=TRUE)

##########################################################################
#for f=1

SJ = seq(from = 0, to=1, by=0.1)
Nminmax.SJf1 = matrix(NA, ncol=2, nrow=length(SJ))#prey minimum and maximum
Pminmax.SJf1 = matrix(NA, ncol=2, nrow=length(SJ))#predator minimux ans maximum
zminmax.SJf1 = matrix(NA, ncol=2, nrow=length(SJ))#trait minum and maximum
gminmax.SJf1 = matrix(NA, ncol=2, nrow=length(SJ))
rminmax.SJf1 = matrix(NA, ncol=2, nrow=length(SJ))
Kminmax.SJf1 = matrix(NA, ncol=2, nrow=length(SJ))
for(i in 1:length(SJ)){
  parmsi = c(a=2.2,b=0.8,Sj=SJ[i],k0=0.5,k1=0.25,lj=0.5,mu0=0.5,f=1,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.SJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.SJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.SJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.SJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.SJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.SJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.SJf1$mean=rowMeans(rminmax.SJf1, na.rm=TRUE)
Kminmax.SJf1$mean=rowMeans(Kminmax.SJf1, na.rm=TRUE)


##############################################################################################################################################
#Bifurcation for lj
#for f=0

lJ = seq(from = 0, to=1.5, by=0.1)
Nminmax.lJf0 = matrix(NA, ncol=2, nrow=length(lJ))#prey minimum and maximum
Pminmax.lJf0 = matrix(NA, ncol=2, nrow=length(lJ))#predator minimux ans maximum
zminmax.lJf0 = matrix(NA, ncol=2, nrow=length(lJ))#trait minum and maximum
gminmax.lJf0 = matrix(NA, ncol=2, nrow=length(lJ))
rminmax.lJf0 = matrix(NA, ncol=2, nrow=length(lJ))
Kminmax.lJf0 = matrix(NA, ncol=2, nrow=length(lJ))
for(i in 1:length(lJ)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=lJ[i],mu0=0.5,f=0,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.lJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.lJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.lJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.lJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.lJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.lJf0[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.lJf0$mean=rowMeans(rminmax.lJf0, na.rm=TRUE)
Kminmax.lJf0$mean=rowMeans(Kminmax.lJf0, na.rm=TRUE)

##########################################################################
#for f=0.2

lJ = seq(from = 0, to=1.5, by=0.1)
Nminmax.lJf02 = matrix(NA, ncol=2, nrow=length(lJ))#prey minimum and maximum
Pminmax.lJf02 = matrix(NA, ncol=2, nrow=length(lJ))#predator minimux ans maximum
zminmax.lJf02 = matrix(NA, ncol=2, nrow=length(lJ))#trait minum and maximum
gminmax.lJf02 = matrix(NA, ncol=2, nrow=length(lJ))
rminmax.lJf02 = matrix(NA, ncol=2, nrow=length(lJ))
Kminmax.lJf02 = matrix(NA, ncol=2, nrow=length(lJ))
for(i in 1:length(lJ)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=lJ[i],mu0=0.5,f=0.2,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.lJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.lJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.lJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.lJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.lJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.lJf02[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.lJf02$mean=rowMeans(rminmax.lJf02, na.rm=TRUE)
Kminmax.lJf02$mean=rowMeans(Kminmax.lJf02, na.rm=TRUE)

##########################################################################
#for f=0.5

lJ = seq(from = 0, to=1.5, by=0.1)
Nminmax.lJf05 = matrix(NA, ncol=2, nrow=length(lJ))#prey minimum and maximum
Pminmax.lJf05 = matrix(NA, ncol=2, nrow=length(lJ))#predator minimux ans maximum
zminmax.lJf05 = matrix(NA, ncol=2, nrow=length(lJ))#trait minum and maximum
gminmax.lJf05 = matrix(NA, ncol=2, nrow=length(lJ))
rminmax.lJf05 = matrix(NA, ncol=2, nrow=length(lJ))
Kminmax.lJf05 = matrix(NA, ncol=2, nrow=length(lJ))
for(i in 1:length(lJ)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=lJ[i],mu0=0.5,f=0.5,e=1,h=1,d=0.5,s=0.0001,V=0.05)  
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.lJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.lJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.lJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.lJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.lJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.lJf05[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.lJf05$mean=rowMeans(rminmax.lJf05, na.rm=TRUE)
Kminmax.lJf05$mean=rowMeans(Kminmax.lJf05, na.rm=TRUE)


##########################################################################
#for f=0.8

lJ = seq(from = 0, to=1.5, by=0.1)
Nminmax.lJf08 = matrix(NA, ncol=2, nrow=length(lJ))#prey minimum and maximum
Pminmax.lJf08 = matrix(NA, ncol=2, nrow=length(lJ))#predator minimux ans maximum
zminmax.lJf08 = matrix(NA, ncol=2, nrow=length(lJ))#trait minum and maximum
gminmax.lJf08 = matrix(NA, ncol=2, nrow=length(lJ))
rminmax.lJf08 = matrix(NA, ncol=2, nrow=length(lJ))
Kminmax.lJf08 = matrix(NA, ncol=2, nrow=length(lJ))
for(i in 1:length(lJ)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=lJ[i],mu0=0.5,f=0.8,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.lJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.lJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.lJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.lJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.lJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.lJf08[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.lJf08$mean=rowMeans(rminmax.lJf08, na.rm=TRUE)
Kminmax.lJf08$mean=rowMeans(Kminmax.lJf08, na.rm=TRUE)


##########################################################################
#for f=1

lJ = seq(from = 0, to=1.5, by=0.1)
Nminmax.lJf1 = matrix(NA, ncol=2, nrow=length(lJ))#prey minimum and maximum
Pminmax.lJf1 = matrix(NA, ncol=2, nrow=length(lJ))#predator minimux ans maximum
zminmax.lJf1 = matrix(NA, ncol=2, nrow=length(lJ))#trait minum and maximum
gminmax.lJf1 = matrix(NA, ncol=2, nrow=length(lJ))
rminmax.lJf1 = matrix(NA, ncol=2, nrow=length(lJ))
Kminmax.lJf1 = matrix(NA, ncol=2, nrow=length(lJ))
for(i in 1:length(lJ)){
  parmsi = c(a=2.2,b=0.8,Sj=0.5,k0=0.5,k1=0.25,lj=lJ[i],mu0=0.5,f=1,e=1,h=1,d=0.5,s=0.0001,V=0.05) 
  n = c(g=0,N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 80000, by=0.1), func = m3.sys.b, parms = parmsi)
  outi.log = log(outi+1)
  gminmax.lJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Nminmax.lJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  Pminmax.lJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
  zminmax.lJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  rminmax.lJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Kminmax.lJf1[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),7])
}
#rK mean values
rminmax.lJf1$mean=rowMeans(rminmax.lJf1, na.rm=TRUE)
Kminmax.lJf1$mean=rowMeans(Kminmax.lJf1, na.rm=TRUE)



