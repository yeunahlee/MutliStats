boston.housing <-
function(job=1)
{
  dump("boston.housing","c:\\M7018\\boston.housing.r")
  da=read.csv("Boston.csv",header=T)
  nam=c("Crime.per.capita","Large.resident.lots","Large.business.lots",
        "Charles.River","Nitric.Oxyde","Roome.per.house","Houses.before.1940",
        "Distance.to.downtown","Railroad.acsess","Tax.rate","Pupil.per.teacher",
        "African.american","Poverty","House.price")
  names(da)=nam

if(job==1)
{
  par(mfrow=c(3,5),mar=c(4,3,2,1),omi=c(0,0,.25,0),cex.lab=1.5)
  for(i in 1:13)
  {
    x=da[,i];y=da$House.price
    plot(x,y,xlab=nam[i],ylab="")
    abline(lsfit(x=x,y=y),lwd=3,col=2)
    r=cor(x,y)
    title(paste("r=",round(r,2),sep=""))
  }
    mtext(side=3,outer=T,"Median house price in 506 census districtis of Boston versus district attribute",cex=1.25,line=.1)
  }
if(job==2)
{
  lmout=lm(House.price~Crime.per.capita+Large.resident.lots+Large.business.lots+Charles.River+Nitric.Oxyde+Roome.per.house+Houses.before.1940+Distance.to.downtown+Railroad.acsess+Tax.rate+Pupil.per.teacher+African.american+Poverty,data=da)
  print(summary(lmout))
  res=lmout$residuals
  res=res/sd(res)
  n=length(res)
  res=res[order(res)]
  plot(qnorm((1:n-.5)/n),res,xlim=c(-4,4),ylim=c(-4,4),ylab="Empirical quantiles",xlab="Theoretical quantiles")
  segments(-5,-5,5,5,col=2)
  
  lmout=lm(House.price~Crime.per.capita+Large.resident.lots+Charles.River+Nitric.Oxyde+Roome.per.house+Distance.to.downtown+Railroad.acsess+Tax.rate+Pupil.per.teacher+African.american+Poverty,data=da)
  print(summary(lmout))
  
  lmout=lm(log(House.price)~Crime.per.capita+Large.resident.lots+Charles.River+Nitric.Oxyde+Roome.per.house+Distance.to.downtown+Railroad.acsess+Tax.rate+Pupil.per.teacher+African.american+Poverty,data=da)
  print(summary(lmout))
  res=lmout$residuals
  res=res/sd(res)
  n=length(res)
  res=res[order(res)]
  par(mfrow=c(1,1),mar=c(4,4,3,1))
  plot(qnorm((1:n-.5)/n),res,xlim=c(-4,4),ylim=c(-4,4),ylab="Empirical quantiles",xlab="Theoretical quantiles")
  title("Residuals from housing regression")
  segments(-5,-5,5,5,col=2)
  print(vcov(lmout))
}
if(job==3)
  {
    #Original Model
    lmout=lm(log(House.price)~Crime.per.capita+Large.resident.lots+Charles.River+
               Nitric.Oxyde+Roome.per.house+Distance.to.downtown+Railroad.acsess+
               Tax.rate+Pupil.per.teacher+African.american+Poverty,data=da)

    new.df = data.frame(Crime.per.capita=10, Large.resident.lots=3, 
                        Large.business.lots=5, Charles.River=1, Nitric.Oxyde=.5, 
                        Roome.per.house=5, Houses.before.1940=35, Distance.to.downtown=8, 
                        Railroad.acsess=15, Tax.rate=300, Pupil.per.teacher=15, 
                        African.american=100, Poverty=5)
    a=coef(lmout)
    
    #Predicted Value
    pred = a[1]+ a[2]*10+ a[3]*3+ a[4]*1+ a[5]*.5+ a[6]*5+ a[7]*8+ a[8]*15+ a[9]*300+ a[10]*15+ a[11]*100+ a[12]*5
    print(pred)

    x = c(1,10,3,1,.5,5,8,15,300,15,100,5)
    
    res=lmout$residuals
    n=length(res)

    sereg=sqrt(sum(res^2)/(n-2))
    sep=sqrt(sereg+t(x%*%vcov(lmout)%*%x))
    
    #Calculating CI
    qt95 = qt(1-0.05/2,df=n-2)
    up = pred+qt95*sep
    low = pred-qt95*sep
    
    print(paste("Prediction in log dollars =",round(pred,4)))
    print(paste("Prediction in thousand dollars =", exp(3.138936)))
    
    print(paste("Lower 95% limit in log dollars =",round(low,4)))
    print(paste("Lower 95% limit in thousand dollars=",round(exp(2.2764),4)))
    
    print(paste("Upper 95% limit in log dollars =",round(up,4)))
    print(paste("Upper 95% limit in thousand dollars=",round(exp(4.0014),4)))
    
  }
}
