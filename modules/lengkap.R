require(xlsx)
library(nlme)
library(MASS) 

fungsi.lengkap<-function(y.contoh,x.contoh,x.populasi,vektor.kodearea.contoh,vektor.kodearea.populasi,monte.carlo=1000,boot.populasi=1,boot.contoh=100,method="eu",garis.kemiskinan = 342956)
{
  jumlah.kodearea<-length(unique(vektor.kodearea.contoh))
  vektor.kodearea<-unique(vektor.kodearea.contoh)
  jumlah.populasi<-table(vektor.kodearea.populasi)
  jumlah.contoh<-table(vektor.kodearea.contoh)
  myid<-1:sum(jumlah.populasi)
  
  myq.true.boot<-array(0,dim=c(4,jumlah.kodearea,boot.populasi))
  myarea.q.boot.r<-array(0,dim=c(4,jumlah.kodearea,boot.populasi,boot.contoh))
  myarea.q.boot<-array(0,dim=c(4,jumlah.kodearea,boot.populasi))
  myq.true.boot.m<-array(0,dim=c(4,jumlah.kodearea))
  myarea.q.boot.m<-array(0,dim=c(4,jumlah.kodearea))
  BIAS.boot<-matrix(0,4,jumlah.kodearea)
  VAR.boot<-matrix(0,4,jumlah.kodearea)
  MSE.boot<-matrix(0,4,jumlah.kodearea)
  CI.boot.m<-matrix(0,jumlah.kodearea,2)
  CI.boot.hcr<-matrix(0,jumlah.kodearea,2)
  CI.boot.pg<-matrix(0,jumlah.kodearea,2)
  CI.boot.pse<-matrix(0,jumlah.kodearea,2)
  estimate<-fungsi.p0.p1.p2(y.contoh,x.contoh,x.populasi,vektor.kodearea.contoh,vektor.kodearea.populasi,monte.carlo,jumlah.kodearea,vektor.kodearea,jumlah.populasi,jumlah.contoh,garis.kemiskinan)
  
  
  res.mq<-estimate$res.mq
  Eds.mq<-estimate$Eds.mq
  urut=c()
  urut[1] = 1
  for (i in 2:sum(jumlah.contoh))
  { 
    urut[i]=i
  }
  
  plot(urut, res.mq, main="Sisaan", xlab="Contoh ", ylab="Sisaan ", pch=19)
  hist(res.mq,xlab="Sisaan",breaks=20,main="Hist Sisaan")
  
  #Generate B bootstrap Population (size N)
  
  #Centering residuals for the whole sample (use this for area unconditioned approach)
  res.s.centered<-sort(res.mq-mean(res.mq))

  for(b in 1:boot.populasi)
  {
    #Population empirical density of residuals area unconditioned
    y.boot<-NULL
    y.boot.i<-NULL
    for (i in 1:jumlah.kodearea)
    {
      y.boot.i<-x.populasi[vektor.kodearea.populasi==vektor.kodearea[i],]%*%estimate$mycoef[,i]+sample (res.s.centered,jumlah.populasi[i],replace=TRUE)
      y.boot<-c(y.boot,y.boot.i)
    }
    for(ii in 1:jumlah.kodearea)
    {
      y.d.boot<-y.boot[vektor.kodearea.populasi==vektor.kodearea[ii]]
      y.d.boot[y.d.boot<0]<-0
      myq.true.boot[2,ii,b]<-(1/jumlah.populasi[ii])*sum(y.d.boot<garis.kemiskinan)
      myq.true.boot[3,ii,b]<-(1/jumlah.populasi[ii])*sum((1-(y.d.boot/garis.kemiskinan))*(y.d.boot<garis.kemiskinan))
      myq.true.boot[4,ii,b]<-(1/jumlah.populasi[ii])*sum((1-(y.d.boot/garis.kemiskinan))*(1-(y.d.boot/garis.kemiskinan))*(y.d.boot<garis.kemiskinan))
    }
    for(rr in 1:boot.contoh)
    {
      cat(date(),"Bootstrap populasi-",b,"Bootstrap contoh",rr,"\n")
      mysboot<-NULL
      s.boot.i<-NULL
      for(ii in 1:jumlah.kodearea)
      {
        s.boot.i<-sample(myid[vektor.kodearea.populasi==vektor.kodearea[ii]],jumlah.contoh[ii])
        mysboot<-c(mysboot,s.boot.i)
      }
      ys.boot<-y.boot[mysboot]
      x.s.boot<-x.populasi[mysboot,]
      estimate.boot<-fungsi.p0.p1.p2(ys.boot,x.s.boot,x.populasi,vektor.kodearea.contoh,vektor.kodearea.populasi,monte.carlo,jumlah.kodearea,vektor.kodearea,jumlah.populasi,jumlah.contoh,garis.kemiskinan)
      myarea.q.boot.r[2,,b,rr]<-estimate.boot$P0.MQ
      myarea.q.boot.r[3,,b,rr]<-estimate.boot$P1.MQ
      myarea.q.boot.r[4,,b,rr]<-estimate.boot$P2.MQ
    }
    for (ii in 1:jumlah.kodearea)
    {
      myarea.q.boot[2,ii,b]<-mean(myarea.q.boot.r[2,ii,b,])
      myarea.q.boot[3,ii,b]<-mean(myarea.q.boot.r[3,ii,b,])
      myarea.q.boot[4,ii,b]<-mean(myarea.q.boot.r[4,ii,b,])
    }
  }
  #B ends here
  
  for (i in 1:jumlah.kodearea)
  {
    myq.true.boot.m[1,i]<-mean(myq.true.boot[1,i,])
    myq.true.boot.m[2,i]<-mean(myq.true.boot[2,i,])
    myq.true.boot.m[3,i]<-mean(myq.true.boot[3,i,])
    myq.true.boot.m[4,i]<-mean(myq.true.boot[4,i,])
    myarea.q.boot.m[1,i]<-mean(myarea.q.boot[1,i,])
    myarea.q.boot.m[2,i]<-mean(myarea.q.boot[2,i,])
    myarea.q.boot.m[3,i]<-mean(myarea.q.boot[3,i,])
    myarea.q.boot.m[4,i]<-mean(myarea.q.boot[4,i,])
  }
  for (i in 1:jumlah.kodearea)
  { 
    BIAS.boot[1,i]<-myarea.q.boot.m[1,i]-myq.true.boot.m[1,i]
    BIAS.boot[2,i]<-myarea.q.boot.m[2,i]-myq.true.boot.m[2,i]
    BIAS.boot[3,i]<-myarea.q.boot.m[3,i]-myq.true.boot.m[3,i]
    BIAS.boot[4,i]<-myarea.q.boot.m[4,i]-myq.true.boot.m[4,i]
    aux.0<-matrix(0,boot.populasi,1)
    aux.1<-matrix(0,boot.populasi,1)
    aux.2<-matrix(0,boot.populasi,1)
    aux.3<-matrix(0,boot.populasi,1)
    for (b in 1:boot.populasi)
    {
      aux.0[b,1]<-(1/boot.contoh)*sum((myarea.q.boot.r[1,i,b,]-myarea.q.boot[1,i,b])^2) 
      aux.1[b,1]<-(1/boot.contoh)*sum((myarea.q.boot.r[2,i,b,]-myarea.q.boot[2,i,b])^2)
      aux.2[b,1]<-(1/boot.contoh)*sum((myarea.q.boot.r[3,i,b,]-myarea.q.boot[3,i,b])^2)
      aux.3[b,1]<-(1/boot.contoh)*sum((myarea.q.boot.r[4,i,b,]-myarea.q.boot[4,i,b])^2)
    }
    VAR.boot[1,i]<-(1/boot.populasi)*sum(aux.0[,1])
    VAR.boot[2,i]<-(1/boot.populasi)*sum(aux.1[,1])
    VAR.boot[3,i]<-(1/boot.populasi)*sum(aux.2[,1])
    VAR.boot[4,i]<-(1/boot.populasi)*sum(aux.3[,1])
    
  }
  for (i in 1:jumlah.kodearea)
  {
    MSE.boot[1,i]<-((BIAS.boot[1,i])^2)+VAR.boot[1,i]
    MSE.boot[2,i]<-((BIAS.boot[2,i])^2)+VAR.boot[2,i] 
    MSE.boot[3,i]<-((BIAS.boot[3,i])^2)+VAR.boot[3,i] 
    MSE.boot[4,i]<-((BIAS.boot[4,i])^2)+VAR.boot[4,i] 
    CI.boot.m[i,]<-quantile(c(myarea.q.boot.r[1,i,,]),prob=c(0.025,0.975))
    CI.boot.hcr[i,]<-quantile(c(myarea.q.boot.r[2,i,,]),prob=c(0.025,0.975)) 
    CI.boot.pg[i,]<-quantile(c(myarea.q.boot.r[3,i,,]),prob=c(0.025,0.975))
    CI.boot.pse[i,]<-quantile(c(myarea.q.boot.r[4,i,,]),prob=c(0.025,0.975))
    
  }
  P0.mq<-round(100*estimate$P0.MQ,digits=2)
  P1.mq<-round(100*estimate$P1.MQ,digits=2)
  P2.mq<-round(100*estimate$P2.MQ,digits=2)
  
  mse.p0.mq<-round(100*MSE.boot[2,],digits=2)
  mse.p1.mq<-round(100*MSE.boot[3,],digits=2)
  mse.p2.mq<-round(100*MSE.boot[4,],digits=2)
  
  rmse.p0.mq<-round(100*sqrt(MSE.boot[2,]),digits=2)
  rmse.p1.mq<-round(100*sqrt(MSE.boot[3,]),digits=2)
  rmse.p2.mq<-round(100*sqrt(MSE.boot[4,]),digits=2)
  
  rrmse.p0.mq<-round(rmse.p0.mq[]/estimate$P0.MQ[],digits=2)
  rrmse.p1.mq<-round(rmse.p1.mq[]/estimate$P1.MQ[],digits=2)
  rrmse.p2.mq<-round(rmse.p2.mq[]/estimate$P2.MQ[],digits=2)
  
  
  output<-cbind(P0.mq,mse.p0.mq,rmse.p0.mq,rrmse.p0.mq,P1.mq,mse.p1.mq,rmse.p1.mq,rrmse.p1.mq,P2.mq,mse.p2.mq,rmse.p2.mq,rrmse.p2.mq)
  write.xlsx(output, file="output_m_quantile.xlsx")
  write.xlsx(estimate$q.unit, file="koef_m_quantile_unit.xlsx")
  write.xlsx(estimate$q.unit.area, file="koef_m_quantile_area.xlsx")
  write.xlsx(estimate$mycoef, file="koef_m_quantile.xlsx")
  list(Eds.mq=Eds.mq,Koefisien_MQUantile_area=estimate$q.unit.area,Koefisien_Regresi_area=estimate$mycoef,Pendugaan_Kemiskinan_RMSE_RRMSE=output)
  
}