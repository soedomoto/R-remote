require(xlsx)
library(nlme)
library(MASS)

fungsi.IRLS<-function(x.contoh, y.contoh, w = rep(1, nrow(x.contoh)), fungsi.pembobot=psi.huber, maks.iterasi.IRLS=1000, q)
{
  irls.delta <-function(old, new)
  {
    sqrt(sum((old - new)^2)/max(1e-20, sum(old^2)))
  }
  
  model <- lm.wfit(x.contoh, y.contoh, w, method = "qr")
  #Mendapatkan koefisien regresi dari model
  koefisien.beta <-model$coef
  #Mendapatkan sisaan dari model
  sisaan <- model$resid
  
  done <- FALSE
  conv <- NULL
  
  n1 <- nrow(x.contoh) - ncol(x.contoh)
  vektor.penduga.beta <- matrix(0, nrow = ncol(x.contoh), ncol = length(q))
  vektor.w <- matrix(0, nrow = nrow(x.contoh), ncol = length(q))
  vektor.y.duga <- matrix(0, nrow = nrow(x.contoh), ncol = length(q))
  vektor.sisaan <- matrix(0, nrow = nrow(x.contoh), ncol = length(q)) 
  vektor.skala <- matrix(0, nrow = maks.iterasi.IRLS, ncol = length(q)) 
  
  for(i in 1:length(q))
  {
    #Lakukan Iterasi IRLS
    for (iiter in 1:maks.iterasi.IRLS)
    {
      sisaan_sebelumnya <- sisaan
      
      #Menghitung Skala Dengan Median Absolut Deviation
      skala <- 1.4826*median(abs(sisaan))
      vektor.skala[iiter, i] <- skala
      
      if (skala == 0)
      {
        done < - TRUE
        break
      }
      
      #Menerapkan FUngsi Pembobot Terhadeap Scaled Residualnya
      w <- fungsi.pembobot(sisaan/skala)
      
      ww <- 2 * (1 - q[i]) * w
      ww[sisaan > 0] <- 2 * q[i] * w[sisaan > 0]
      
      w <- ww
      
      model <- lm.wfit(x.contoh, y.contoh, w, method = "qr")
      koefisien.beta <- model$coef
      sisaan <- model$residuals
      convi <- irls.delta(sisaan_sebelumnya, sisaan)
      conv <- c(conv, convi)
      #Jika mengalami konvergensi maka done bernilai truue
      done <- (convi <= 0.0001)
      #jika done bernilai true maka proses diberhentikan/"break"
      if (convi <= 0.0001)
        break
    }
    if (!done) warning(paste("rlm failed to converge in", maks.iterasi.IRLS, "steps at q = ", q[i]))
    vektor.penduga.beta[, i] <- koefisien.beta
    vektor.w[, i] <- w
    vektor.y.duga[, i] <- model$fitted.values
    vektor.sisaan[,i] <- sisaan
  }
  list(vektor.skala=vektor.skala,vektor.y.duga = vektor.y.duga, vektor.sisaan = vektor.sisaan, vektor.q = q, vektor.w = vektor.w, vektor.penduga.beta = vektor.penduga.beta)
  
}

#	COMPUTE THE QUANTILE ORDER

#	COMPUTING OF THE QUANTILE-ORDERS

"zerovalinter"<-function(y, x)
{
  if(min(y) > 0)
  {
    xmin <- x[y == min(y)]
    if(length(xmin) > 0)
      xmin <- xmin[length(xmin)]
    xzero <- xmin
  }
  else
  {
    if(max(y) < 0)
    {
      xmin <- x[y == max(y)]
      if(length(xmin) > 0)
        xmin <- xmin[1]
      xzero <- xmin
      
    }
    else
    {
      y1 <- min(y[y > 0])
      if(length(y1) > 0)
        y1 <- y1[length(y1)]
      y2 <- max(y[y < 0])
      if(length(y2) > 0)
        y2 <- y2[1]
      x1 <- x[y == y1]
      if(length(x1) > 0)
        x1 <- x1[length(x1)]
      x2 <- x[y == y2] 
      if(length(x2) > 0)
        x2 <- x2[1]
      xzero <- (x2 * y1 - x1 * y2)/(y1 - y2)
      xmin <- x1
      if(abs(y2) < y1)
        xmin <- x2
    }
  }
  resu <- xzero
  resu
}

#Function for Finding the Quantile Orders by Linear Interpolation

#Assumes that "zerovalinter" function has been already loaded

#computing of the expectile-order of each observation of y by interpolation
gridfitinter<-function(y.contoh,vektor.y.duga,vektor.q)
{
  nq<-length(vektor.q)
  diff <- y.contoh %*% t(as.matrix(rep(1, nq))) - vektor.y.duga
  vectordest <- apply(diff, 1, zerovalinter,vektor.q)
}

fungsi.koefisien.mquantile<-function(x.contoh,y.contoh,vektor.kodearea.contoh,maks.iterasi.IRLS=1000)
{
  jumlah.kodearea<-length(unique(vektor.kodearea.contoh))
  jumlah.contoh<-sum(as.numeric(table(vektor.kodearea.contoh)))
  jumlah.peubah.penyerta<-dim(x.contoh)[2]
  
  #Membangkitkan nilai q unit dan q area
  ob<-fungsi.IRLS(x.contoh, y.contoh, maks.iterasi.IRLS = 1000,q=sort(c(seq(0.006,0.99,0.045),0.5,0.994,0.01,0.02,0.96,0.98)))
  qo<-matrix(c(gridfitinter(y.contoh,ob$vektor.y.duga,ob$vektor.q)),nrow= jumlah.contoh,ncol=1)
  koefisien.mquantile.unit<-matrix(c(qo,vektor.kodearea.contoh),nrow=length(vektor.kodearea.contoh),ncol=2)   ##Ini qjd kok e                 
  koefisien.mquantile.area<-aggregate(koefisien.mquantile.unit[,1],list(d2=koefisien.mquantile.unit[,2]),mean)[,2] ##Ini Theta di sini kok e
  
  #Membangkitkan nilai Beta (koefisien)
  ob1<-fungsi.IRLS(x.contoh, y.contoh,maks.iterasi.IRLS = 1000,q=c(koefisien.mquantile.area[1:jumlah.kodearea]))
  matriks.koefisien.beta.area<-matrix(c(t(ob1$vektor.penduga.beta)),nrow=jumlah.kodearea,ncol=jumlah.peubah.penyerta)
  
  
  # need to be ordered by area
  matriks.koefisien.beta.area<-t(matriks.koefisien.beta.area) ##Koefisien Regresi Beta
  
  #Kirim nilai q unit, q area dan beta (koefsien)
  list(koefisien.mquantile.unit=koefisien.mquantile.unit,koefisien.mquantile.area=koefisien.mquantile.area,matriks.koefisien.beta.area=matriks.koefisien.beta.area)
  
}

fungsi.p0.p1.p2<-function(y.contoh,x.contoh,x.populasi,vektor.kodearea.contoh,vektor.kodearea.populasi,monte.carlo,jumlah.kodearea,vektor.kodearea,jumlah.populasi,jumlah.contoh,garis.kemiskinan)
{
  f.MQ.0<-array(0,dim=c(jumlah.kodearea)) 
  f.MQ.1<-array(0,dim=c(jumlah.kodearea))
  f.MQ.2<-array(0,dim=c(jumlah.kodearea))
  res.mq<-NULL
  
  #Fit the model M-quantile
  model.mquantile=fungsi.koefisien.mquantile(x.contoh,y.contoh,vektor.kodearea.contoh)
  beta.mq<-model.mquantile$matriks.koefisien.beta.area
  q.unit.mq<-model.mquantile$koefisien.mquantile.unit
  q.area.mq<-model.mquantile$koefisien.mquantile.area
  
  for(i in 1:jumlah.kodearea)
  {
    #MQ
    ysd<-y.contoh[vektor.kodearea.contoh==vektor.kodearea[i]]
    x.sd<-x.contoh[vektor.kodearea.contoh==vektor.kodearea[i],] 
    Eds.mq<-x.sd%*%beta.mq[,i] #####Ini Eds.mq untuk menghitung yjd-cap
    res.d.mq<-ysd-Eds.mq ##Ini dia e untu kecamatan tertentu
    res.mq<-c(res.mq,res.d.mq) ##Ini dia e untuk semua kecamatan
  }
  for(i in 1:jumlah.kodearea)
  {
    x.rd<-x.populasi[vektor.kodearea.populasi==vektor.kodearea[i],]
    #Monte Carlo approximation to the best predictor of yi 
    F.0.hl.mq<-matrix(0,monte.carlo,1)
    F.1.hl.mq<-matrix(0,monte.carlo,1)
    F.2.hl.mq<-matrix(0,monte.carlo,1)
    for (l in 1:monte.carlo)
    {
      #MQ
      y.pred.mc<-x.rd%*%beta.mq[,i]+sample(res.mq,jumlah.populasi[i],replace=TRUE)
      y.pred.mc[y.pred.mc<0]<-0
      I.mc.mq<-y.pred.mc<garis.kemiskinan
      F.0.hl.mq[l,1]<-sum(I.mc.mq)/jumlah.populasi[i]
      F.1.hl.mq[l,1]<-(1/jumlah.populasi[i])*sum((1-y.pred.mc/garis.kemiskinan)*I.mc.mq)
      F.2.hl.mq[l,1]<-(1/jumlah.populasi[i])*sum((1-y.pred.mc/garis.kemiskinan)*(1-y.pred.mc/garis.kemiskinan)*I.mc.mq)
      
    }
    f.MQ.0[i]<-mean(F.0.hl.mq[,1])
    f.MQ.1[i]<-mean(F.1.hl.mq[,1])
    f.MQ.2[i]<-mean(F.2.hl.mq[,1])
  }
  #i ends here
  
  f.MQ.0[which(f.MQ.0>1)]<-1
  f.MQ.1[which(f.MQ.1>1)]<-1
  f.MQ.2[which(f.MQ.1>1)]<-1
  
  #list(P0.MQ=f.MQ.0,P1.MQ=f.MQ.1,P2.MQ=f.MQ.2,res.mq=res.mq,mycoef=beta.mq,q.unit=q.unit.mq,q.unit.area=q.area.mq)
  list(P0.MQ=f.MQ.0,P1.MQ=f.MQ.1,P2.MQ=f.MQ.2,res.mq=res.mq,Eds.mq=Eds.mq,mycoef=beta.mq,q.unit=q.unit.mq,q.unit.area=q.area.mq)
}
#Function fungsi.p0.p1.p2 ends here

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

#Membaca file csv yang berisi data non oversampling (hanya 504 ruta)
data.contoh<-read.csv("D:/sintaks - SP2010 X/contoh.csv",header=TRUE)
#data.contoh<-read.xlsx("D:/sintaks - SP2010 X/contoh.xlsx", sheetIndex = 1)
data.contoh <- subset(data.contoh,data.contoh[,"sumber"]=='ssn')

#Membaca file csv yang berisi data populasi (sekitar 900ribu ruta)
data.pop<-read.csv("D:/sintaks - SP2010 X/populasi.csv",header=TRUE)
#data.pop<-read.xlsx("D:/sintaks - SP2010 X/populasi.xlsx", sheetIndex = 1)

#Pengeluaran Perkapita Ruta (Sebanyak Sampel Susenas)
y.contoh<-data.contoh$perkapita

#Peubah penyerta untuk data contoh
x.contoh<-cbind(data.contoh$x0,data.contoh$art,data.contoh$lap_usaha,data.contoh$status_usaha)

#Vektor kode area untuk data contoh
vektor.kodearea.contoh<-data.contoh$kec

#Peubah penyerta untuk data populasi
x.populasi<-cbind(data.pop$x0,data.pop$art,data.pop$lap_usaha,data.pop$status_usaha)

#Vektor kode area untuk data populasi
vektor.kodearea.populasi<-data.pop$kec

fungsi.lengkap(y.contoh,x.contoh,x.populasi,vektor.kodearea.contoh,vektor.kodearea.populasi,monte.carlo=50,boot.populasi=1,boot.contoh=100,garis.kemiskinan = 342956)