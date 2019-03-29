
library(deSolve)
#Chapter II.
#Figure I
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_rK.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=rminmax.f$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(f)), ylab="Selection",frame=T,col="black")
points(x=ff, y=Kminmax.f$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.f))
abline(v=0.29, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.525, col="darkgrey",lwd=3,lty=2) #r=K
text(0.15,10,paste("AI"),cex=3) #Unstable r
text(0.4,10,paste("AII"),cex=3) #stable r
text(0.8,10,paste("AIII"),cex=3) #stable K
dev.off()

#Figures II
#N
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_N.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Nminmax.f[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab=expression("Prey Density"~log(italic(N+1))),frame=T,col="black")
points(x=ff, y=Nminmax.f[,2],type="l", lwd=3)
abline(v=0.29, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.525, col="darkgrey",lwd=3,lty=2) #r=K
text(0.15,3,paste("AI"), cex=3) #Unstable r
text(0.4,3,paste("AII"), cex=3) #stable r
text(0.8,3,paste("AIII"), cex=3) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_P.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Pminmax.f[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab=expression("Predator Density"~log(italic(P+1))),frame=T,col="black")
points(x=ff, y=Pminmax.f[,2],type="l", lwd=3)
abline(v=0.29, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.525, col="darkgrey",lwd=3,lty=2) #r=K
text(0.15,3,paste("AI"), cex=3) #Unstable r
text(0.4,3,paste("AII"), cex=3) #stable r
text(0.8,3,paste("AIII"), cex=3) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_z.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=zminmax.f[,1],  type="l", lwd=3,lty=2,ylim=range(0:5),
     xlab=expression(italic(f)), ylab=expression("Trait Value"~log(italic(zeta+1))),frame=T,col="black")
points(x=ff, y=zminmax.f[,2],type="l", lwd=3)
abline(v=0.29, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.525, col="darkgrey",lwd=3,lty=2) #r=K
text(0.15,5,paste("AI"), cex=3) #Unstable r
text(0.4,5,paste("AII"), cex=3) #stable r
text(0.8,5,paste("AIII"), cex=3) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_g.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=gminmax.f[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(f)), ylab=expression("Prop. of Juveniles "~log(italic(gamma+1))),frame=T,col="black")
points(x=ff, y=gminmax.f[,2],type="l", lwd=3)
abline(v=0.29, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.525, col="darkgrey",lwd=3,lty=2) #r=K
text(0.15,3,paste("AI"), cex=3) #Unstable r
text(0.4,3,paste("AII"), cex=3) #stable r
text(0.8,3,paste("AIII"), cex=3) #stable K
dev.off()

######################################################################################################################################
#FIGURE III

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_rK_l0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=rminmax.fl0$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(f)), ylab="Selection",frame=T,col="black")
points(x=ff, y=Kminmax.fl0$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.fl0))
abline(v=0.15, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.02, col="darkgrey",lwd=3,lty=2) #r=K
text(0,4,paste("AI"),cex=3) #runaway
text(0.08,10,paste("AII"),cex=3) #stable r
text(0.6,10,paste("AIII"),cex=3) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_rK_S0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=rminmax.fS0$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(f)), ylab="",frame=T,col="black")
points(x=ff, y=Kminmax.fS0$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.fS0))
abline(v=0.08, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.05, col="darkgrey",lwd=3,lty=2) #r=K
text(0,4,paste("AI"),cex=3) #runaway
text(0.065,10,paste("AII"),cex=3) #stable r
text(0.6,10,paste("AIII"),cex=3) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_rK_Sl0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=rminmax.flS0$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(f)), ylab="",frame=T,col="black")
points(x=ff, y=Kminmax.flS0$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.flS0))
dev.off()

############################################################################################################################
#FIGURES IV

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_rK_f0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=rminmax.hf0$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(h)), ylab="Selection",frame=T,col="black")
points(x=hh, y=Kminmax.hf0$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.hf0))
#abline(v=1.32, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1) #r=K
text(1,10,paste("AI"),cex=3) #Unstable r
#text(0.84,9,paste("AII")) #stable r
#text(1.48,9,paste("AIII")) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_rK_f02.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=rminmax.hf02$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(h)), ylab="Selection",frame=T,col="black")
points(x=hh, y=Kminmax.hf02$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.hf02))
abline(v=0.53, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=0.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.5,10,paste("AI"),cex=3) #runaway
text(0.63,10,paste("AII"),cex=3) #stable r
text(0.25,10,paste("AIII"),cex=3) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_rK_f05.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=rminmax.hf05$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(h)), ylab="Selection",frame=T,col="black")
points(x=hh, y=Kminmax.hf05$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.hf05))
abline(v=0.97, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.43, col="darkgrey",lwd=3,lty=2)
abline(v=2, col="darkgrey",lwd=3,lty=1)#r=K
text(1.7,10,paste("AI"),cex=3) #runaway
text(1.2,10,paste("AII"),cex=3) #stable r
text(0.5,10,paste("AIII"),cex=3) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_rK_f08.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=rminmax.hf08$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(h)), ylab="Selection",frame=T,col="black")
points(x=hh, y=Kminmax.hf08$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.hf08))
abline(v=1.18, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=1.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.8,10,paste("AI"),cex=3) #Runaway
text(1.45,10,paste("AII"),cex=3) #stable r
text(0.5,10,paste("AIII"),cex=3) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_rK_f1.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=rminmax.hf1$mean, type="l", lwd=3,lty=c(1),ylim=range(0:10),
     xlab=expression(italic(h)), ylab="Selection",frame=T,col="black")
points(x=hh, y=Kminmax.hf1$mean, type="l", lwd=3,lty=c(2),ylim=range(Kminmax.hf1))
abline(v=1.27, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.84, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.9,10,paste("AI"),cex=3) #runaway
text(1.6,10,paste("AII"),cex=3) #stable r
text(0.5,10,paste("AIII"),cex=3) #stable K
dev.off()

#######################################################################################################################
#FIGURE AI
#N
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_N_l0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Nminmax.fl0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab=expression("Prey Density"~log(italic(N+1))),frame=T,col="black")
points(x=ff, y=Nminmax.fl0[,2],type="l", lwd=3)
abline(v=0.15, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.02, col="darkgrey",lwd=3,lty=2) #r=K
text(0,2,paste("AI"),cex=3) #runaway
text(0.08,3,paste("AII"),cex=3) #stable r
text(0.6,3,paste("AIII"),cex=3) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_P_l0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Pminmax.fl0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab=expression("Predator Density"~log(italic(P+1))),frame=T,col="black")
points(x=ff, y=Pminmax.fl0[,2],type="l", lwd=3)
abline(v=0.15, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.02, col="darkgrey",lwd=3,lty=2) #r=K
text(0,2,paste("AI"),cex=3) #runaway
text(0.08,3,paste("AII"),cex=3) #stable r
text(0.6,3,paste("AIII"),cex=3) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_z_l0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=zminmax.fl0[,1],  type="l", lwd=3,lty=2,ylim=range(0:6),
     xlab="", ylab=expression("Trait Value"~log(italic(zeta+1))),frame=T,col="black")
points(x=ff, y=zminmax.fl0[,2],type="l", lwd=3)
abline(v=0.15, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.02, col="darkgrey",lwd=3,lty=2) #r=K
text(0,5,paste("AI"),cex=3) #runaway
text(0.08,6,paste("AII"),cex=3) #stable r
text(0.6,6,paste("AIII"),cex=3) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_g_l0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=gminmax.fl0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(f)), ylab=expression("Prop. Juveniles"~log(italic(gamma+1))),frame=T,col="black")
points(x=ff, y=gminmax.fl0[,2],type="l", lwd=3)
abline(v=0.15, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.02, col="darkgrey",lwd=3,lty=2) #r=K
text(0,2,paste("AI"),cex=3) #runaway
text(0.08,3,paste("AII"),cex=3) #stable r
text(0.6,3,paste("AIII"),cex=3) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_N_S0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Nminmax.fS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=ff, y=Nminmax.fS0[,2],type="l", lwd=3)
abline(v=0.08, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.05, col="darkgrey",lwd=3,lty=2) #r=K
text(0,2,paste("AI"),cex=3) #runaway
text(0.065,3,paste("AII"),cex=3) #stable r
text(0.6,3,paste("AIII"),cex=3) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_P_S0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Pminmax.fS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=ff, y=Pminmax.fS0[,2],type="l", lwd=3)
abline(v=0.08, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.05, col="darkgrey",lwd=3,lty=2) #r=K
text(0,2,paste("AI"),cex=3) #runaway
text(0.065,3,paste("AII"),cex=3) #stable r
text(0.6,3,paste("AIII"),cex=3) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_z_S0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=zminmax.fS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:6),
     xlab="", ylab="",frame=T,col="black")
points(x=ff, y=zminmax.fS0[,2],type="l", lwd=3)
abline(v=0.08, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.05, col="darkgrey",lwd=3,lty=2) #r=K
text(0,5,paste("AI"),cex=3) #runaway
text(0.065,6,paste("AII"),cex=3) #stable r
text(0.6,6,paste("AIII"),cex=3) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_g_S0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=gminmax.fS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(f)), ylab="",frame=T,col="black")
points(x=ff, y=gminmax.fS0[,2],type="l", lwd=3)
abline(v=0.08, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=0.05, col="darkgrey",lwd=3,lty=2) #r=K
text(0,2,paste("AI"),cex=3) #runaway
text(0.065,3,paste("AII"),cex=3) #stable r
text(0.6,3,paste("AIII"),cex=3) #stable K
dev.off()

jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_N_Sl0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Nminmax.flS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=ff, y=Nminmax.flS0[,2],type="l", lwd=3)
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_P_Sl0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=Pminmax.flS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=ff, y=Pminmax.flS0[,2],type="l", lwd=3)
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_z_Sl0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=ff, y=zminmax.flS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:6),
     xlab="", ylab="",frame=T,col="black")
points(x=ff, y=zminmax.flS0[,2],type="l", lwd=3)
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_f_g_Sl0.jpeg")
par(mar = c(4,6,2,0),cex.axis=2,cex.lab=3)
plot(x=ff, y=gminmax.flS0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(f)), ylab="",frame=T,col="black")
points(x=ff, y=gminmax.flS0[,2],type="l", lwd=3)
dev.off()


######################################################################################################################
#FIGURE AII
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_N_f0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Nminmax.hf0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab=expression("Prey Density"~log(italic(N+1))),frame=T,col="black")
points(x=hh, y=Nminmax.hf0[,2],type="l", lwd=3)
#abline(v=1.32, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1) #r=K
text(1,3,paste("AI"),cex=3) #Unstable r
#text(0.84,9,paste("AII")) #stable r
#text(1.48,9,paste("AIII")) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_P_f0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Pminmax.hf0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab=expression("Predator Density"~log(italic(P+1))),frame=T,col="black")
points(x=hh, y=Pminmax.hf0[,2],type="l", lwd=3)
#abline(v=1.32, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1) #r=K
text(1,3,paste("AI"),cex=3) #Unstable r
#text(0.84,9,paste("AII")) #stable r
#text(1.48,9,paste("AIII")) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_z_f0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=zminmax.hf0[,1],  type="l", lwd=3,lty=2,ylim=range(0:8),
     xlab="", ylab=expression("Trait Value"~log(italic(zeta+1))),frame=T,col="black")
points(x=hh, y=zminmax.hf0[,2],type="l", lwd=3)
#abline(v=1.32, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1) #r=K
text(1,8,paste("AI"),cex=3) #Unstable r
#text(0.84,9,paste("AII")) #stable r
#text(1.48,9,paste("AIII")) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_g_f0.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=gminmax.hf0[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(h)), ylab=expression("Prop. of Juveniles"~log(italic(gamma+1))),frame=T,col="black")
points(x=hh, y=gminmax.hf0[,2],type="l", lwd=3)
#abline(v=1.32, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1) #r=K
text(1,3,paste("AI"),cex=3) #Unstable r
#text(0.84,9,paste("AII")) #stable r
#text(1.48,9,paste("AIII")) #stable K
dev.off()

#N
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_N_f02.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Nminmax.hf02[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Nminmax.hf02[,2],type="l", lwd=3)
abline(v=0.53, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=0.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.5,3,paste("AI"),cex=3) #runaway
text(0.63,3,paste("AII"),cex=3) #stable r
text(0.25,3,paste("AIII"),cex=3) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_P_f02.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Pminmax.hf02[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Pminmax.hf02[,2],type="l", lwd=3)
abline(v=0.53, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=0.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.5,3,paste("AI"),cex=3) #runaway
text(0.63,3,paste("AII"),cex=3) #stable r
text(0.25,3,paste("AIII"),cex=3) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_z_f02.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=zminmax.hf02[,1],  type="l", lwd=3,lty=2,ylim=range(0:6),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=zminmax.hf02[,2],type="l", lwd=3)
abline(v=0.53, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=0.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.5,6,paste("AI"),cex=3) #runaway
text(0.63,6,paste("AII"),cex=3) #stable r
text(0.25,6,paste("AIII"),cex=3) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_g_f02.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=gminmax.hf02[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(h)), ylab="",frame=T,col="black")
points(x=hh, y=gminmax.hf02[,2],type="l", lwd=3)
abline(v=0.53, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=0.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.5,3,paste("AI"),cex=3) #runaway
text(0.63,3,paste("AII"),cex=3) #stable r
text(0.25,3,paste("AIII"),cex=3) #stable K
dev.off()

#N
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_N_f05.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Nminmax.hf05[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Nminmax.hf05[,2],type="l", lwd=3)
abline(v=0.97, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.43, col="darkgrey",lwd=3,lty=2)
abline(v=2, col="darkgrey",lwd=3,lty=1)#r=K
text(1.7,3,paste("AI"),cex=3) #runaway
text(1.2,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_P_f05.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Pminmax.hf05[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Pminmax.hf05[,2],type="l", lwd=3)
abline(v=0.97, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.43, col="darkgrey",lwd=3,lty=2)
abline(v=2, col="darkgrey",lwd=3,lty=1)#r=K
text(1.7,3,paste("AI"),cex=3) #runaway
text(1.2,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_z_f05.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=zminmax.hf05[,1],  type="l", lwd=3,lty=2,ylim=range(0:6),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=zminmax.hf05[,2],type="l", lwd=3)
abline(v=0.97, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.43, col="darkgrey",lwd=3,lty=2)
abline(v=2, col="darkgrey",lwd=3,lty=1)#r=K
text(1.7,6,paste("AI"),cex=3) #runaway
text(1.2,6,paste("AII"),cex=3) #stable r
text(0.5,6,paste("AIII"),cex=3) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_g_f05.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=gminmax.hf05[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(h)), ylab="",frame=T,col="black")
points(x=hh, y=gminmax.hf05[,2],type="l", lwd=3)
abline(v=0.97, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.43, col="darkgrey",lwd=3,lty=2)
abline(v=2, col="darkgrey",lwd=3,lty=1)#r=K
text(1.7,3,paste("AI"),cex=3) #runaway
text(1.2,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()

#N
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_N_f08.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Nminmax.hf08[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Nminmax.hf08[,2],type="l", lwd=3)
abline(v=1.18, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=1.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.8,3,paste("AI"),cex=3) #Runaway
text(1.45,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_P_f08.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Pminmax.hf08[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Pminmax.hf08[,2],type="l", lwd=3)
abline(v=1.18, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=1.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.8,3,paste("AI"),cex=3) #Runaway
text(1.45,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_z_f08.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=zminmax.hf08[,1],  type="l", lwd=3,lty=2,ylim=range(0:6),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=zminmax.hf08[,2],type="l", lwd=3)
abline(v=1.18, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=1.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.8,6,paste("AI"),cex=3) #Runaway
text(1.45,6,paste("AII"),cex=3) #stable r
text(0.5,6,paste("AIII"),cex=3) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_g_f08.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=gminmax.hf08[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(h)), ylab="",frame=T,col="black")
points(x=hh, y=gminmax.hf08[,2],type="l", lwd=3)
abline(v=1.18, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=1.73, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.8,3,paste("AI"),cex=3) #Runaway
text(1.45,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()

#N
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_N_f1.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Nminmax.hf1[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Nminmax.hf1[,2],type="l", lwd=3)
abline(v=1.27, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.84, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.9,3,paste("AI"),cex=3) #runaway
text(1.6,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()

#P
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_P_f1.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=Pminmax.hf1[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=Pminmax.hf1[,2],type="l", lwd=3)
abline(v=1.27, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.84, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.9,3,paste("AI"),cex=3) #runaway
text(1.6,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()


#z
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_z_f1.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=zminmax.hf1[,1],  type="l", lwd=3,lty=2,ylim=range(0:6),
     xlab="", ylab="",frame=T,col="black")
points(x=hh, y=zminmax.hf1[,2],type="l", lwd=3)
abline(v=1.27, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.84, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.9,6,paste("AI"),cex=3) #runaway
text(1.6,6,paste("AII"),cex=3) #stable r
text(0.5,6,paste("AIII"),cex=3) #stable K
dev.off()


#g
jpeg(filename="C://Users//Logan//Documents//Laboratorio de Simulacoes//Dissertacao//figures3//B_h_g_f1.jpeg")
par(mar = c(4,6,0,0),cex.axis=1.75,cex.lab=2.75)
plot(x=hh, y=gminmax.hf1[,1],  type="l", lwd=3,lty=2,ylim=range(0:3),
     xlab=expression(italic(h)), ylab="",frame=T,col="black")
points(x=hh, y=gminmax.hf1[,2],type="l", lwd=3)
abline(v=1.27, col="darkgrey",lwd=3,lty=2) #Instability
abline(v=1.84, col="darkgrey",lwd=3,lty=2) #r=K
abline(v=2, col="darkgrey",lwd=3,lty=1)
text(1.9,3,paste("AI"),cex=3) #runaway
text(1.6,3,paste("AII"),cex=3) #stable r
text(0.5,3,paste("AIII"),cex=3) #stable K
dev.off()

