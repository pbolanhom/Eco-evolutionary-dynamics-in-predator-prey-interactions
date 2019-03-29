
library(deSolve)
#Chapter I

#FIG 1
#Bifurcation of a
#Prey
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_a_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ArK, y=Nminmax[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(a)), ylab=expression("Prey Density"~log(italic(N+1))),frame=T,col="black")
points(x=ArK, y=Nminmax[,2],type="l", lwd=3)
abline(v=a2, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=a1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.38, col="darkgrey",lwd=3,lty=2) #cycles
abline(v=0.294, col="darkgrey",lwd=3,lty=2) #runaway
text(0.15,3,paste("AI"),cex=3)
text(0.84,3,paste("AII"),cex=3)
text(1.48,3,paste("AIII"),cex=3)
text(1.8,3,paste("AIV"),cex=3)
dev.off()

#predator
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_a_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ArK, y=Pminmax[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(a)), ylab=expression("Predator Density"~log(italic(P+1))),frame=T,col="black")
points(x=ArK, y=Pminmax[,2],type="l", lwd=3)
abline(v=a2, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=a1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.38, col="darkgrey",lwd=3,lty=2) #cycles
abline(v=0.294, col="darkgrey",lwd=3,lty=2) #runaway
text(0.15,3,paste("AI"),cex=3)
text(0.84,3,paste("AII"),cex=3)
text(1.48,3,paste("AIII"),cex=3)
text(1.8,3,paste("AIV"),cex=3)
dev.off()
#trait
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_a_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ArK, y=zminmax[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(a)), ylab=expression("Trait Value"~log(italic(zeta+1))),frame=T,col="black")
points(x=ArK, y=zminmax[,2],type="l", lwd=3)
abline(v=a2, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=a1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.38, col="darkgrey",lwd=3,lty=2) #cycles
abline(v=0.294, col="darkgrey",lwd=3,lty=2) #runaway
text(0.15,3,paste("AI"),cex=3)
text(0.84,3,paste("AII"),cex=3)
text(1.48,3,paste("AIII"),cex=3)
text(1.8,3,paste("AIV"),cex=3)
dev.off()
#rK
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_a_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ArK, y=Arminmax$mean, type="l", lwd=3,lty=c(1),ylim=range(0:9),
     xlab=expression(italic(a)), ylab="Selection Strength",frame=T,col="black")
points(x=ArK, y=AKminmax$mean, type="l", lwd=3,lty=c(2),ylim=range(AKminmax))
abline(v=a2, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=a1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.38, col="darkgrey",lwd=3,lty=2) #cycles
abline(v=0.294, col="darkgrey",lwd=3,lty=2) #runaway
text(0.15,9,paste("AI"),cex=3)
text(0.84,9,paste("AII"),cex=3)
text(1.48,9,paste("AIII"),cex=3)
text(1.8,9,paste("AIV"),cex=3)
dev.off()


########################################################################################
#FIG 2
#Bifurcation of e
########################################################################################
#for conversion efficiency
#prey
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_e_r_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=er, y=eNminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab=expression("Prey Density"~log(italic(N+1))),frame=T,col="black",lty=2)
points(x=er, y=eNminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=e2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.89, col="darkgrey",lwd=3,lty=2)
text(0.65,4,paste("AI"),cex=3)
text(1.75,4,paste("AII"),cex=3)
text(0.25,4,paste("AIV"),cex=3)
dev.off()
#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_e_K_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=eK, y=eNminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab="",frame=T,col="black",lty=2)
points(x=eK, y=eNminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=e2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.56, col="grey",lwd=3,lty=2)
abline(v=2.1, col="grey",lwd=3,lty=2)
text(0.53,4,paste("AI"),cex=3)
text(1.25,4,paste("AII"),cex=3)
text(2.35,4,paste("AIII"),cex=3)
text(0.25,4,paste("AIV"),cex=3)
dev.off()

#for Predator
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_e_r_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=er, y=ePminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab=expression("Predator Density"~log(italic(P+1))),frame=T,col="black",lty=2)
points(x=er, y=ePminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=e2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.89, col="darkgrey",lwd=3,lty=2)
text(0.65,4,paste("AI"),cex=3)
text(1.75,4,paste("AII"),cex=3)
text(0.25,4,paste("AIV"),cex=3)
dev.off()

#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_e_K_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=er, y=ePminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab="",frame=T,col="black",lty=2)
points(x=eK, y=ePminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=e2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.56, col="grey",lwd=3,lty=2)
abline(v=2.1, col="grey",lwd=3,lty=2)
text(0.53,4,paste("AI"),cex=3)
text(1.25,4,paste("AII"),cex=3)
text(2.35,4,paste("AIII"),cex=3)
text(0.25,4,paste("AIV"),cex=3)
dev.off()

#for trait
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_e_r_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=er, y=ezminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab=expression(italic(e)), ylab=expression("Trait Value"~log(italic(zeta+1))),frame=T,col="black",lty=2)
points(x=er, y=ezminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=e2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.89, col="darkgrey",lwd=3,lty=2)
text(0.65,4,paste("AI"),cex=3)
text(1.75,4,paste("AII"),cex=3)
text(0.25,4,paste("AIV"),cex=3)
dev.off()
#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_e_K_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=er, y=ezminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab=expression(italic(e)), ylab="",frame=T,col="black",lty=2)
points(x=eK, y=ezminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=e2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.56, col="grey",lwd=3,lty=2)
abline(v=2.1, col="grey",lwd=3,lty=2)
text(0.53,4,paste("AI"),cex=3)
text(1.25,4,paste("AII"),cex=3)
text(2.35,4,paste("AIII"),cex=3)
text(0.25,4,paste("AIV"),cex=3)
dev.off()


########################################################################################
#FIG 3
#Bifurcation of h
########################################################################################
#for handling time
#prey
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_r_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hr, y=hNminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab=expression("Prey Density"~log(italic(N+1))),frame=T,col="black",lty=2)
points(x=hr, y=hNminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=h2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.16, col="grey",lwd=3,lty=2)
text(1.51,4,paste("AI"),cex=3)
text(0.51,4,paste("AII"),cex=3)
text(1.92,4,paste("AIV"),cex=3)
dev.off()

#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_K_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hr, y=hNminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab="",frame=T,col="black",lty=2)
points(x=hr, y=hNminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=h2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.84, col="grey",lwd=3,lty=2)
text(1.9,4,paste("AI"),cex=3)
text(1,4,paste("AII"),cex=3)
#text(1.92,4,paste("AIV"),cex=3)
dev.off()

#for Predator
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_r_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hr, y=hPminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab=expression("Predator Density"~log(italic(P+1))),frame=T,col="black",lty=2)
points(x=hr, y=hPminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=h2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.16, col="grey",lwd=3,lty=2)
text(1.51,4,paste("AI"),cex=3)
text(0.51,4,paste("AII"),cex=3)
text(1.92,4,paste("AIV"),cex=3)
dev.off()
#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_K_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hr, y=hPminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab="",frame=T,col="black",lty=2)
points(x=hr, y=hPminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=h2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.84, col="grey",lwd=3,lty=2)
text(1.9,4,paste("AI"),cex=3)
text(1,4,paste("AII"),cex=3)
#text(2.02,4,paste("AIV"),cex=3)
dev.off()

#for trait
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_r_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hr, y=hzminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab=expression(italic(h)), ylab=expression("Trait Value"~log(italic(z+1))),frame=T,col="black",lty=2)
points(x=hr, y=hzminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=h2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.16, col="grey",lwd=3,lty=2)
text(1.51,4,paste("AI"),cex=3)
text(0.51,4,paste("AII"),cex=3)
text(1.96,4,paste("AIV"),cex=3)
dev.off()
#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_K_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hr, y=hzminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab=expression(italic(h)), ylab="",frame=T,col="black",lty=2)
points(x=hr, y=hzminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=h2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.84, col="grey",lwd=3,lty=2)
text(1.9,4,paste("AI"),cex=3)
text(1,4,paste("AII"),cex=3)
#text(2.02,4,paste("AIV"),cex=3)
dev.off()

########################################################################################
#FIG 4
#Bifurcation of d
########################################################################################
#for death rate
#prey
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_d_r_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=dr, y=dNminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab=expression("Prey Density"~log(italic(N+1))),frame=T,col="black",lty=2)
points(x=dr, y=dNminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=d2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.08, col="grey",lwd=3,lty=2)
abline(v=0.56, col="grey",lwd=3,lty=2)
text(0.8,4,paste("AI"),cex=3)
text(0.3,4,paste("AII"),cex=3)
text(0.025,4,paste("AIII"),cex=3)
text(0.96,4,paste("AIV"),cex=3)
dev.off()
#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_d_K_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=dK, y=dNminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab="",frame=T,col="black",lty=2)
points(x=dr, y=dNminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=d2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.33, col="grey",lwd=3,lty=2)
abline(v=0.91, col="grey",lwd=3,lty=2)
text(0.95,4,paste("AI"),cex=3)
text(0.6,4,paste("AII"),cex=3)
text(0.17,4,paste("AIII"),cex=3)
#text(0.96,4,paste("AIV"),cex=3)
dev.off()


#for Predator
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_d_r_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=dr, y=dPminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab=expression("Predator Density"~log(italic(P+1))),frame=T,col="black",lty=2)
points(x=dr, y=dPminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=d2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.08, col="grey",lwd=3,lty=2)
abline(v=0.56, col="grey",lwd=3,lty=2)
text(0.8,4,paste("AI"),cex=3)
text(0.3,4,paste("AII"),cex=3)
text(0.025,4,paste("AIII"),cex=3)
text(0.96,4,paste("AIV"),cex=3)
dev.off()
#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_d_K_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=dK, y=dPminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab="", ylab="",frame=T,col="black",lty=2)
points(x=dr, y=dPminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=d2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.33, col="grey",lwd=3,lty=2)
abline(v=0.91, col="grey",lwd=3,lty=2)
text(0.95,4,paste("AI"),cex=3)
text(0.6,4,paste("AII"),cex=3)
text(0.17,4,paste("AIII"),cex=3)
#text(0.96,4,paste("AIV"),cex=3)
dev.off()

#for trait
#r
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_d_r_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=dr, y=dzminmax.r[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab=expression(italic(d)), ylab=expression("Trait Value"~log(italic(zeta+1))),frame=T,col="black",lty=2)
points(x=dr, y=dzminmax.r[,2],type="l", lwd=3,col="black",lty=1)
abline(v=d2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.08, col="grey",lwd=3,lty=2)
abline(v=0.56, col="grey",lwd=3,lty=2)
text(0.8,4,paste("AI"),cex=3)
text(0.3,4,paste("AII"),cex=3)
text(0.025,4,paste("AIII"),cex=3)
text(0.96,4,paste("AIV"),cex=3)
dev.off()
#K
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_d_K_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=dK, y=dzminmax.K[,1],  type="l", lwd=3,ylim=range(0:4),
     xlab=expression(italic(d)), ylab="",frame=T,col="black",lty=2)
points(x=dr, y=dzminmax.K[,2],type="l", lwd=3,col="black",lty=1)
abline(v=d2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.33, col="grey",lwd=3,lty=2)
abline(v=0.91, col="grey",lwd=3,lty=2)
text(0.95,4,paste("AI"),cex=3)
text(0.6,4,paste("AII"),cex=3)
text(0.17,4,paste("AIII"),cex=3)
#text(0.96,4,paste("AIV"),cex=3)
dev.off()


#supplementary information

#FIGURE 1 SM
parms=c(A=0.64108,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5)
Nrange=seq(0,4,0.1)
data1=as.data.frame(isoclines(Nrange,parms))
#Predator Extinction
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//iso_Pextinc.jpeg")
par(mar = c(5,6,0,5),cex.axis=1.75,cex.lab=2.75)
with(data1, plot(Nrange, Pe, type="l", col="black", lty=2,lwd=3,
                 ylab="Predator Equilibrium",xlab="Prey Equilibrium",
                 ylim=c(0,2)))
abline(v=with(data1,Ne),col="black",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,rsel),col="azure4",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,Ksel),col="azure4",lty=2,lwd=3)
axis(side = 4)
#mtext(side=4,line=2,'Selection Strength')
dev.off()
#Coexistence in a non-predominant selection environment
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//iso_coex.jpeg")
par(mar = c(5,6,0,5),cex.axis=1.75,cex.lab=2.75)
with(data1, plot(Nrange, Pe, type="l", col="black", lty=2,lwd=3,
                 ylab="",xlab="Prey Equilibrium",
                 ylim=c(0,2)))
abline(v=with(data1,NE),col="black",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,rselE),col="azure4",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,KselE),col="azure4",lty=2,lwd=3)
axis(side = 4)
mtext(side=4,line=3,'Selection',cex=2.75)
dev.off()

#FIG 2 SM
library(scatterplot3d)

#RUNAWAY EVOLUTION
parameters=c(a=0.25,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5,V=0.05,s=0.0001)
#Initial Conditions
n0 = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
#Time Steps
Time=seq(from=1,to=1000,by=0.01)
#Solving the system
system.out=ode(y=n0,times=Time,func=system,parms=parameters)
#Population Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NP_run.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,c(2:3)],type = "l",lwd=2,lty=c(1,2),col="black",
        xlab="Time",ylab="Population Density",frame=T,xlim=range(300:1000))
dev.off()
#Trait Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//z_run.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,4],type = "l",lwd=2,lty=1,
        xlab="Time",ylab="Trait Value",frame=T,xlim=range(300:1000))
dev.off()
#pHASE SPACE 3d
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NPz_run_ps.jpeg")
par(mar = c(4,4,0,4),cex.axis=1.75,cex.lab=2.75)
scatterplot3d(x=system.out[,2], y=system.out[,3], z=system.out[,4],type="p",pch=".",
              xlab="Prey",ylab="Predator",zlab="Trait", box=F,mar = c(4,4,0,4))
dev.off()

#FIGURE 3
#Stable
parameters=c(a=0.64,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5,V=0.05,s=0.0001)
#Initial Conditions
n0 = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
#Time Steps
Time=seq(from=1,to=700,by=0.01)
#Solving the system
system.out=ode(y=n0,times=Time,func=system,parms=parameters)
#Population Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NP_stable.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,c(2:3)],type = "l",lwd=2,lty=c(1,2),col="black",
        xlab="Time",ylab="Population Density",frame=T)
dev.off()
#Trait Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//z_stable.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,4],type = "l",lwd=2,lty=1,
        xlab="Time",ylab="Trait Value",frame=T)
dev.off()
#pHASE SPACE 3d
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NPz_stable_ps.jpeg")
par(mar = c(4,4,0,4),cex.axis=1.75,cex.lab=2.75)
scatterplot3d(x=system.out[,2], y=system.out[,3], z=system.out[,4],type="p",pch=".",
              xlab="Prey",ylab="Predator",zlab="Trait", box=F,mar = c(4,4,0,4))
dev.off()
#FIGURE 4
#Cycles
parameters=c(a=1.4,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5,V=0.05,s=0.0001)
#Initial Conditions
n0 = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
#Time Steps
Time=seq(from=1,to=7000,by=0.01)
#Solving the system
system.out=ode(y=n0,times=Time,func=system,parms=parameters)
#Population Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NP_cy.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,c(2:3)],type = "l",lwd=2,lty=c(1,2),col="black",
        xlab="Time",ylab="Population Density",frame=T,xlim=range(6000:7000))
dev.off()
#Trait Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//z_cy.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,4],type = "l",lwd=2,lty=1,
        xlab="Time",ylab="Trait Value",frame=T,xlim=range(6000:7000))
dev.off()
#pHASE SPACE 3d
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NPz_cy_ps.jpeg")
par(mar = c(4,4,0,4),cex.axis=1.75,cex.lab=2.75)
scatterplot3d(x=system.out[,2], y=system.out[,3], z=system.out[,4],type="p",pch=".",
              xlab="Prey",ylab="Predator",zlab="Trait", box=F,mar = c(4,4,0,4))
dev.off()

#FIGURE 5
#Predator extinction
parameters=c(a=1.6,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5,V=0.05,s=0.0001)
#Initial Conditions
n0 = c(N=0.5,P=0.5,z=1,dr=0,dK=0)
#Time Steps
Time=seq(from=1,to=100,by=0.01)
#Solving the system
system.out=ode(y=n0,times=Time,func=system,parms=parameters)
#Population Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NP_extin.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,c(2:3)],type = "l",lwd=2,lty=c(1,2),col="black",
        xlab="Time",ylab="Population Density",frame=T)
dev.off()
#Trait Dynamics
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//z_extin.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
matplot(x=system.out[,1],y=system.out[,4],type = "l",lwd=2,lty=1,
        xlab="Time",ylab="Trait Value",frame=T)
dev.off()
#pHASE SPACE 3d
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//NPz_extin_ps.jpeg")
par(mar = c(4,4,0,4),cex.axis=1.75,cex.lab=2.75)
scatterplot3d(x=system.out[,2], y=system.out[,3], z=system.out[,4],type="p",pch=".",
              xlab="Prey",ylab="Predator",zlab="Trait", box=F,mar = c(4,4,0,4))
dev.off()


#FIGURE 6
#Predominant r-selection
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//iso_rsel.jpeg")
parms=c(A=0.34108,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5)
data1=as.data.frame(isoclines(Nrange,parms))
par(mar = c(5,6,0,5),cex.axis=1.75,cex.lab=2.75)
with(data1, plot(Nrange, Pe, type="l", col="black", lty=2,lwd=3,
                 ylab="Predator Equilibrium",xlab="Prey Equilibrium",
                 ylim=c(0,2)))
abline(v=with(data1,N),col="black",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,rselE),col="azure4",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,KselE),col="azure4",lty=2,lwd=3)
axis(side = 4)
#axis(side = 3,line=NA)
#mtext(side=4,line=2,'Selection Strength')
dev.off()
#Predominant K-selection
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//iso_Ksel.jpeg")
parms=c(A=0.94108,b=0.8,k0=0.5,k1=0.25,h=1,e=1,d=0.5)
data1=as.data.frame(isoclines(Nrange,parms))
par(mar = c(5,6,0,5),cex.axis=1.75,cex.lab=2.75)
with(data1, plot(Nrange, Pe, type="l", col="black", lty=2,lwd=3,
                 ylab="",xlab="Prey Equilibrium",
                 ylim=c(0,2)))
abline(v=with(data1,N),col="black",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,rselE),col="azure4",lty=1,lwd=3)
lines(with(data1,Nrange),with(data1,KselE),col="azure4",lty=2,lwd=3)
axis(side = 4)
mtext(side=4,line=3,'Selection',cex=2.75)
dev.off()


#FIGURE 7
#rK selection
#FOR a=0.34
#e
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//bife_r_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=erK, y=erminmax.r$mean, type="l", lwd=3,lty=c(1),ylim=range(0:9),
     xlab="", ylab="Selection",frame=T,col="black")
points(x=erK, y=eKminmax.r$mean, type="l", lwd=3,lty=c(2),ylim=range(eKminmax.r))
abline(v=e2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.89, col="darkgrey",lwd=3,lty=2)
text(0.65,9,paste("AI"),cex=3)
text(1.75,9,paste("AII"),cex=3)
text(0.25,9,paste("AIV"),cex=3)
dev.off()

#h
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//bifh_r_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hrK, y=hrminmax.r$mean, type="l", lwd=3,lty=c(1),ylim=range(0:9),
     xlab="", ylab="",frame=T,col="black")
points(x=hrK, y=hKminmax.r$mean, type="l", lwd=3,lty=c(2),ylim=range(hKminmax.r))
abline(v=h2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.16, col="grey",lwd=3,lty=2)
text(1.51,9,paste("AI"),cex=3)
text(0.51,9,paste("AII"),cex=3)
text(1.9,9,paste("AIV"),cex=3)
dev.off()

#d
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//bifd_r_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=drK, y=drminmax.r$mean, type="l", lwd=3,lty=c(1),ylim=range(0:9),
     xlab="", ylab="",frame=T,col="black")
points(x=drK, y=dKminmax.r$mean, type="l", lwd=3,lty=c(2),ylim=range(dKminmax.r))
abline(v=d2r, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.08, col="grey",lwd=3,lty=2)
abline(v=0.56, col="grey",lwd=3,lty=2)
text(0.8,9,paste("AI"),cex=3)
text(0.3,9,paste("AII"),cex=3)
text(0.025,9,paste("AIII"),cex=3)
text(0.96,9,paste("AIV"),cex=3)
dev.off()


#FOR a=0.94
#e
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//bife_K_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=erK, y=erminmax.K$mean, type="l", lwd=3,lty=c(1),ylim=range(0:9),
     xlab=expression(italic(e)), ylab="Selection",frame=T,col="black")
points(x=erK, y=eKminmax.K$mean, type="l", lwd=3,lty=c(2),ylim=range(eKminmax.K))
abline(v=e2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=e1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.56, col="grey",lwd=3,lty=2)
abline(v=2.1, col="grey",lwd=3,lty=2)
text(0.53,9,paste("AI"),cex=3)
text(1.25,9,paste("AII"),cex=3)
text(2.35,9,paste("AIII"),cex=3)
text(0.12,9,paste("AIV"),cex=3)
dev.off()

#h
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//bifh_K_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hrK, y=hrminmax.K$mean, type="l", lwd=3,lty=c(1),ylim=range(0:9),
     xlab=expression(italic(h)), ylab="",frame=T,col="black")
points(x=hrK, y=hKminmax.K$mean, type="l", lwd=3,lty=c(2),ylim=range(hKminmax.K))
abline(v=h2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=h1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=1.84, col="grey",lwd=3,lty=2)
text(1.9,9,paste("AI"),cex=3)
text(1,9,paste("AII"),cex=3)
#text(1.93,7,paste("AIV"),cex=3)
dev.off()

#d
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//bifd_K_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=drK, y=drminmax.K$mean, type="l", lwd=3,lty=c(1),ylim=range(0:9),
     xlab=expression(italic(d)), ylab="",frame=T,col="black")
points(x=drK, y=dKminmax.K$mean, type="l", lwd=3,lty=c(2),ylim=range(dKminmax.K))
abline(v=d2K, col="darkgrey",lwd=3,lty=1) #r=K
abline(v=d1, col="darkgrey",lwd=3,lty=2) #r=-K
abline(v=0.33, col="grey",lwd=3,lty=2)
abline(v=0.91, col="grey",lwd=3,lty=2)
text(0.95,9,paste("AI"),cex=3)
text(0.6,9,paste("AII"),cex=3)
text(0.17,9,paste("AIII"),cex=3)
#text(1.01,9,paste("AIV"),cex=3)
dev.off()