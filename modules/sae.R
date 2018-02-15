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



# #Membaca file csv yang berisi data non oversampling (hanya 504 ruta)
# data.contoh<-read.csv("contoh.csv",header=TRUE)
# #data.contoh<-read.xlsx("contoh.xlsx", sheetIndex = 1)
# data.contoh <- subset(data.contoh,data.contoh[,"sumber"]=='ssn')

# #Membaca file csv yang berisi data populasi (sekitar 900ribu ruta)
# data.pop<-read.csv("populasi.csv",header=TRUE)
# #data.pop<-read.xlsx("populasi.xlsx", sheetIndex = 1)

# #Pengeluaran Perkapita Ruta (Sebanyak Sampel Susenas)
# y.contoh<-data.contoh$perkapita

# #Peubah penyerta untuk data contoh
# x.contoh<-cbind(data.contoh$x0,data.contoh$art,data.contoh$lap_usaha,data.contoh$status_usaha)

# #Vektor kode area untuk data contoh
# vektor.kodearea.contoh<-data.contoh$kec

# #Peubah penyerta untuk data populasi
# x.populasi<-cbind(data.pop$x0,data.pop$art,data.pop$lap_usaha,data.pop$status_usaha)

# #Vektor kode area untuk data populasi
# vektor.kodearea.populasi<-data.pop$kec

# fungsi.lengkap(y.contoh,x.contoh,x.populasi,vektor.kodearea.contoh,vektor.kodearea.populasi,monte.carlo=50,boot.populasi=1,boot.contoh=100,garis.kemiskinan = 342956)