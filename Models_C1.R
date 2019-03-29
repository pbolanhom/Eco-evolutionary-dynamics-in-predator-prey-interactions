#Packages:
library(deSolve)

#The Model
system = function(t,y,parms){
  with(as.list(c(y,parms)),
       {
         dN=N*(a+b*z-N*(k0+k1*z)-P*z/(1+h*z*N))            #Prey Population
         dP=P*(e*z*N/(1+h*z*N)-d)                          #Predator Population
         dz=V*exp(-s/(z-s))*(b-N*k1-P/(1+h*z*N))           #Trait Dynamics
         dr=b*(1-N*(k0+k1*z)/(a+b*z))                      #r-selection term
         dK=N*(b*(k0+k1*z)/(a+b*z)-k1)                     #K-selection term
         return(list(c(N=dN,P=dP,z=dz,dr=dr,dK=dK)))
       })
}

#Parameters
#parameters=c(a=2.5,b=0.8,k0=1,k1=0,h=1,e=1,d=0.5,V=0.05,s=0.0001) #Pattern - Limit Cycles - Abrams & Matsuda (1997)
parameters=c(a=0.64,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5,V=0.05,s=0.0001)
#Initial Conditions
n0 = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
#Time Steps
Time=seq(from=1,to=10000,by=0.1)
#Solving the system
system.out=ode(y=n0,times=Time,func=system,parms=parameters)

#############################Time Series Analyses##########################################
#Population Dynamics
matplot(x=system.out[,1],y=system.out[,c(2:3)],type = "l",lwd=1,lty=c(1,2),col="black",
        xlab="Time",ylab="Population Density",frame=T)
legend("topright", legend = c("Prey","Predator"), 
       lty=c(1,2), lwd=1, col="black",cex=0.7)

#Trait Dynamics
matplot(x=system.out[,1],y=system.out[,4],type = "l",lwd=1,lty=1,
        xlab="Time",ylab="Trait Value",frame=T)

#r and K selection Strength Dynamics
matplot(x=system.out[,1],y=log(1+system.out[,c(5:6)]),type = "l",lwd=1,lty=1,col=c("black","azure4"),
        xlab="Time",ylab="Selection Strength",frame=T)
legend("topright", legend = c("r","K"), 
       lty = 1, lwd=1, col=c("black","azure4"),cex=0.7)

##############################################Isoclines and Selection Strength#############################################################
# When Ne=r'/l', the function g' will be 0. Therefore, selection will be guided solely by predation pressure. 
# Since Ne=a/k0, r'=b, and l'=k1, the condition for predator extinction can be met when a=b*k0/k1
#The function to generate the data

isoclines=function(Nrange,parms)
{
  with(as.list(c(Nrange,parms)),
       {
         Pe=(e*b/(e-d*h))*(1-(Nrange*k1/b))
         #for predator extinction
         a=b*k0/k1
         Ne=a/k0
         ze=d/((e-d*h)*Nrange)
         r=a+b*ze
         l=k0+k1*ze
         rsel=b*(1-Nrange*l/r)
         Ksel=Nrange*((b*l/r)-k1)
         #for coexistence
         zE=d*k0/((e-d*h)*A)
         rE=A+b*zE
         lE=k0+k1*zE
         NE=b*rE/(2*b*lE-k1*rE)
         rselE=b*(1-Nrange*lE/rE)
         KselE=Nrange*((b*lE/rE)-k1)
         #For any predominant selection
         N=A/k0
         simul=data.frame(Nrange,Ne,Pe,ze,rsel,Ksel,NE,zE,rselE,KselE,N)
         return(simul)
       })
}
############################################################################################

# BIFURCATION FOR A RANGE OF a
#the behaviour of r and K selection for a range of a
ArK = seq(from = 0, to=2, by=0.01)
Arminmax = matrix(NA, ncol=2, nrow=length(ArK))
AKminmax = matrix(NA, ncol=2, nrow=length(ArK))
Nminmax = matrix(NA, ncol=2, nrow=length(ArK))#prey minimum and maximum
Pminmax = matrix(NA, ncol=2, nrow=length(ArK))#predator minimux ans maximum
zminmax = matrix(NA, ncol=2, nrow=length(ArK))#trait minum and maximum
for(i in 1:length(ArK)){
  parmsi = c(a=ArK[i],b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5,V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 15000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  Arminmax[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),5])
  AKminmax[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),6])
  Nminmax[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),2])
  Pminmax[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),3])
  zminmax[i,] = range(outi.log[(nrow(outi.log)-5000):nrow(outi.log),4])
}

#Calculating the mean between minimum and maximum values of r and K terms
Arminmax$mean=rowMeans(Arminmax, na.rm=TRUE)
AKminmax$mean=rowMeans(AKminmax, na.rm=TRUE) 
#Finding the a values that allows for stabilities
b=0.8
k0=0.5
k1=0.25
a1=b*k0/k1
a2=0.64108 

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################

#Bifurcation for a range of e
#The effects of conversion enficiency, e.
#on r and K selection

#Predominant r-selection
er = seq(from = 0, to=2.5, by=0.01)
erminmax.r = matrix(NA, ncol=2, nrow=length(er))
eKminmax.r = matrix(NA, ncol=2, nrow=length(er))
eNminmax.r = matrix(NA, ncol=2, nrow=length(er))#prey minimum and maximum
ePminmax.r = matrix(NA, ncol=2, nrow=length(er))#predator minimux ans maximum
ezminmax.r = matrix(NA, ncol=2, nrow=length(er))#trait minum and maximum
for(i in 1:length(er)){
  parmsi = c(a=0.34108,b=0.8,k0=0.5,k1=0.25,h=1,e=er[i],d=0.5,V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  eNminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  ePminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  ezminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  erminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  eKminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
erminmax.r$mean=rowMeans(erminmax.r, na.rm=TRUE)
eKminmax.r$mean=rowMeans(eKminmax.r, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.34108
b=0.8
k0=0.5
k1=0.25
h=1
d=0.5
alphaer=((b*k0/a)-k1)*(b*k0/(a*(b*k0-a*k1)))
alphaer
e2r=d*(alphaer+h)
e1=d*h
e2r

###################################################################
#Predominant K-selection
eK = seq(from = 0, to=2.5, by=0.01)
erminmax.K = matrix(NA, ncol=2, nrow=length(eK))
eKminmax.K = matrix(NA, ncol=2, nrow=length(eK))
eNminmax.K = matrix(NA, ncol=2, nrow=length(eK))#prey minimum and maximum
ePminmax.K = matrix(NA, ncol=2, nrow=length(eK))#predator minimux ans maximum
ezminmax.K = matrix(NA, ncol=2, nrow=length(eK))#trait minum and maximum
for(i in 1:length(eK)){
  parmsi = c(a=0.94108,b=0.8,k0=0.5,k1=0.25,h=1,e=eK[i],d=0.5,V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  eNminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  ePminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  ezminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  erminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  eKminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
erminmax.K$mean=rowMeans(erminmax.K, na.rm=TRUE)
eKminmax.K$mean=rowMeans(eKminmax.K, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.94108
b=0.8
k0=0.5
k1=0.25
h=1
d=0.5
alphaeK=((b*k0/a)-k1)*(b*k0/(a*(b*k0-a*k1)))
alphaeK
e2K=d*(alphaeK+h)
e1=d*h
e2K
###################################################################
#Non Predominant selection
erK = seq(from = 0, to=2.5, by=0.01)
erminmax = matrix(NA, ncol=2, nrow=length(erK))
eKminmax = matrix(NA, ncol=2, nrow=length(erK))
eNminmax = matrix(NA, ncol=2, nrow=length(erK))#prey minimum and maximum
ePminmax = matrix(NA, ncol=2, nrow=length(erK))#predator minimux ans maximum
ezminmax = matrix(NA, ncol=2, nrow=length(erK))#trait minum and maximum
for(i in 1:length(erK)){
  parmsi = c(a=0.64108,b=0.8,k0=0.5,k1=0.25,h=1,e=erK[i],d=0.5,V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  eNminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  ePminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  ezminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  erminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  eKminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
erminmax$mean=rowMeans(erminmax, na.rm=TRUE)
eKminmax$mean=rowMeans(eKminmax, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.64108
b=0.8
k0=0.5
k1=0.25
h=1
d=0.5
alphae=((b*k0/a)-k1)*(b*k0/(a*(b*k0-a*k1)))
alphae
e2=d*(alphae+h)
e1=d*h
e2
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#The effects of predator parameters

#The effects of handling time, h.
#on r and K selection

#Predominant r-selection
hr = seq(from = 0, to=2, by=0.01)
hrminmax.r = matrix(NA, ncol=2, nrow=length(hr))
hKminmax.r = matrix(NA, ncol=2, nrow=length(hr))
hNminmax.r = matrix(NA, ncol=2, nrow=length(hr))#prey minimum and maximum
hPminmax.r = matrix(NA, ncol=2, nrow=length(hr))#predator minimux ans maximum
hzminmax.r = matrix(NA, ncol=2, nrow=length(hr))#trait minum and maximum
for(i in 1:length(hr)){
  parmsi = c(a=0.34108,b=0.8,k0=0.5,k1=0.25,h=hr[i],e=1,d=0.5,V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  hNminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  hPminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  hzminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  hrminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  hKminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
hrminmax.r$mean=rowMeans(hrminmax.r, na.rm=TRUE)
hKminmax.r$mean=rowMeans(hKminmax.r, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.34108
b=0.8
k0=0.5
k1=0.25
e=1
d=0.5
alphahr=(b*k0/a-k1)*(b*k0/(a*(b*k0-a*k1)))
h2r=e/d-alphahr
h1=e/d
h2r

###################################################################
#Predominant K-selection
hK = seq(from = 0, to=2, by=0.01)
hrminmax.K = matrix(NA, ncol=2, nrow=length(hK))
hKminmax.K = matrix(NA, ncol=2, nrow=length(hK))
hNminmax.K = matrix(NA, ncol=2, nrow=length(hK))#prey minimum and maximum
hPminmax.K = matrix(NA, ncol=2, nrow=length(hK))#predator minimux ans maximum
hzminmax.K = matrix(NA, ncol=2, nrow=length(hK))#trait minum and maximum
for(i in 1:length(hK)){
  parmsi = c(a=0.94108,b=0.8,k0=0.5,k1=0.25,h=hK[i],e=1,d=0.5,V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  hNminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  hPminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  hzminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  hrminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  hKminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
hrminmax.K$mean=rowMeans(hrminmax.K, na.rm=TRUE)
hKminmax.K$mean=rowMeans(hKminmax.K, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.94108
b=0.8
k0=0.5
k1=0.25
e=1
d=0.5
alphahK=(b*k0/a-k1)*(b*k0/(a*(b*k0-a*k1)))
h2K=e/d-alphahK
h1=e/d
h2K

###################################################################
#Non Predominant selection
hrK = seq(from = 0, to=2, by=0.01)
hrminmax = matrix(NA, ncol=2, nrow=length(hrK))
hKminmax = matrix(NA, ncol=2, nrow=length(hrK))
hNminmax = matrix(NA, ncol=2, nrow=length(hrK))#prey minimum and maximum
hPminmax = matrix(NA, ncol=2, nrow=length(hrK))#predator minimux ans maximum
hzminmax = matrix(NA, ncol=2, nrow=length(hrK))#trait minum and maximum
for(i in 1:length(hrK)){
  parmsi = c(a=0.64108,b=0.8,k0=0.5,k1=0.25,h=hrK[i],e=1,d=0.5,V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  hNminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  hPminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  hzminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  hrminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  hKminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
hrminmax$mean=rowMeans(hrminmax, na.rm=TRUE)
hKminmax$mean=rowMeans(hKminmax, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.64108
b=0.8
k0=0.5
k1=0.25
e=1
d=0.5
alphah=(b*k0/a-k1)*(b*k0/(a*(b*k0-a*k1)))
h2=e/d-alphah
h1=e/d
h2

###############################################################################################################################
###############################################################################################################################
###############################################################################################################################
#The effects of conversion enficiency, d.
#on r and K selection

#Predominant r-selection
dr = seq(from = 0, to=1, by=0.01)
drminmax.r = matrix(NA, ncol=2, nrow=length(dr))
dKminmax.r = matrix(NA, ncol=2, nrow=length(dr))
dNminmax.r = matrix(NA, ncol=2, nrow=length(dr))#prey minimum and maximum
dPminmax.r = matrix(NA, ncol=2, nrow=length(dr))#predator minimux ans maximum
dzminmax.r = matrix(NA, ncol=2, nrow=length(dr))#trait minum and maximum
for(i in 1:length(dr)){
  parmsi = c(a=0.34108,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=dr[i],V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  dNminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  dPminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  dzminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  drminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  dKminmax.r[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
drminmax.r$mean=rowMeans(drminmax.r, na.rm=TRUE)
dKminmax.r$mean=rowMeans(dKminmax.r, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.34108
b=0.8
k0=0.5
k1=0.25
h=1
e=1
alphadr=((b*k0/a)-k1)*(b*k0/(a*(b*k0-a*k1)))
alphadr
d2r=e/(alphadr+h)
d1=h/e
d2r

###################################################################
#Predominant K-selection
dK = seq(from = 0, to=1, by=0.01)
drminmax.K = matrix(NA, ncol=2, nrow=length(dK))
dKminmax.K = matrix(NA, ncol=2, nrow=length(dK))
dNminmax.K = matrix(NA, ncol=2, nrow=length(dK))#prey minimum and maximum
dPminmax.K = matrix(NA, ncol=2, nrow=length(dK))#predator minimux ans maximum
dzminmax.K = matrix(NA, ncol=2, nrow=length(dK))#trait minum and maximum
for(i in 1:length(dK)){
  parmsi = c(a=0.94108,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=dK[i],V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  dNminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  dPminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  dzminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  drminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  dKminmax.K[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
drminmax.K$mean=rowMeans(drminmax.K, na.rm=TRUE)
dKminmax.K$mean=rowMeans(dKminmax.K, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.94108
b=0.8
k0=0.5
k1=0.25
h=1
e=1
alphadK=((b*k0/a)-k1)*(b*k0/(a*(b*k0-a*k1)))
alphadK
d2K=e/(alphadK+h)
d1=h/e
d2K


###################################################################
#Non Predominant selection
drK = seq(from = 0, to=1, by=0.01)
drminmax = matrix(NA, ncol=2, nrow=length(drK))
dKminmax = matrix(NA, ncol=2, nrow=length(drK))
dNminmax = matrix(NA, ncol=2, nrow=length(drK))#prey minimum and maximum
dPminmax = matrix(NA, ncol=2, nrow=length(drK))#predator minimux ans maximum
dzminmax = matrix(NA, ncol=2, nrow=length(drK))#trait minum and maximum
for(i in 1:length(drK)){
  parmsi = c(a=0.64108,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=drK[i],V=0.05,s=0.0001)
  n = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
  outi = ode(y=n, times = seq(from = 1, to = 5000, by=0.1), func = system, parms = parmsi)
  outi.log = log(outi+1)
  dNminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),2])
  dPminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),3])
  dzminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),4])
  drminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),5])
  dKminmax[i,] = range(outi.log[(nrow(outi.log)-1000):nrow(outi.log),6])
}

#Calculating the mean between minimum and maximum values
drminmax$mean=rowMeans(drminmax, na.rm=TRUE)
dKminmax$mean=rowMeans(dKminmax, na.rm=TRUE) 
#Finding the h values that allows for stabilities
a=0.64108
b=0.8
k0=0.5
k1=0.25
h=1
e=1
alphad=((b*k0/a)-k1)*(b*k0/(a*(b*k0-a*k1)))
alphad
d2=e/(alphad+h)
d1=h/e
d2


