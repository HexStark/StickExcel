setwd('C:\\Users\\DELL\\Desktop')
x<-read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\seer数据库8.7\\9.7其他库.txt", header=T)
#读取四肢数据集，并把该数据集命名为y
x<-read.table("can.txt",header=T)
names(y)
require(survival)
library(survival)
library(survminer)
#设置生存时间和状态，并将该数据集命名为a
生存时间<-y$time
生存状态<-y$status
a<-Surv(生存时间,生存状态)
#多变量的cox分析
x$stage7<-factor(x$stage7,
                 levels=c(1,2,3,4,5))
x$size<-factor(x$size,
               levels=c(1,2,3,4))
res.cox<-coxph(Surv(time,status)~stage7,data=x)
summary(res.cox)#查看摘要
x$lp<-predict(res.cox,type="lp")

#读取四肢数据，命名为y
setwd('C:\\Users\\DELL\\Desktop')
y<-read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\验证化疗作用.txt", header=T)
y$stage<-factor(y$stage,
                levels=c(1,2,3,4))
y$stage<-as.numeric(y$stage)


#读取子宫数据，命名为g
setwd('C:\\Users\\DELL\\Desktop')
g<-read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\验证FIGO.txt", header=T)


#读取内脏的数据，命名为z
setwd('C:\\Users\\DELL\\Desktop')
z<-read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\验证NCCNT.txt", header=T)
names(z)
z$stage<-factor(z$stage,
                levels=c(1,2,3,4))
require(survival)
library(survival)
library(survminer)
#设置生存时间和状态，并将该数据集命名为a
生存时间<-z$time
生存状态<-z$status
z<-Surv(生存时间,生存状态)
#多变量的cox分析
z$stage<-factor(z$stage,
                levels=c(1,2,3,4))
res.cox<-coxph(Surv(time,status)~stage+linba,data=z)
summary(res.cox)#查看摘要
z$lp<-predict(res.cox,type="lp")




#调用survivalROC函数包
library(survivalROC)
library(pROC)
#五年的生存分析
cutoff<-60
yROC=survivalROC(Stime=y$time,status=y$status,marker=y$lp,
                 predict.time=cutoff,method="KM")
#四肢类
zROC=survivalROC(Stime=z$time,status=z$status,marker=z$lp,
                 predict.time=cutoff,method="KM")#脏器分期
gROC=survivalROC(Stime=g$time,status=g$status,marker=g$FIGO,
                 predict.time=cutoff,method="KM")#脏器分期
#三年的生存分析
cutoff<-36
yROC=survivalROC(Stime=y$time,status=y$status,marker=y$lp,
                 predict.time=cutoff,method="KM")#四肢类
bROC=survivalROC(Stime=y$time,status=y$status,marker=y$stage,
                 predict.time=cutoff,method="KM")
aROC=survivalROC(Stime=y$time,status=y$status,marker=y$FIGO,
                 predict.time=cutoff,method="KM")#四肢类
mROC=survivalROC(Stime=y$time,status=y$status,marker=y$stage7,
                 predict.time=cutoff,method="KM")#四肢类
zROC=survivalROC(Stime=z$time,status=z$status,marker=z$lp,
                 predict.time=cutoff,method="KM")#脏器分期
gROC=survivalROC(Stime=g$time,status=g$status,marker=g$FIGO,
                 predict.time=cutoff,method="KM")#脏器分期



#绘制ROC曲线5年生存
library(ggplot2)
install.packages("plotROC")
library(plotROC)
ggplot(yROC = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ t) +
  
  
  roc1=roc(yROC$FP,yROC$TP,aur=TRUE,smooth=TRUE)
plot(yROC$FP,yROC$TP,type="l",col="red",
     xlim=c(0,1),ylim=c(0,1),
     xlab=paste("FP","\n","AUC=",round(yROC$AUC,100)),
     ylab="TP",main="yROC,Method=KM\n year=5")
abline(0,1,col="gray",lty=2)
#分析3年的生存率
cutoff<-36
tROC=survivalROC(Stime=y$time,status=y$status,marker=y$lp,
                 predict.time=cutoff,method="KM")
#绘制ROC曲线3年生存
plot(tROC$FP,tROC$TP,type="l",col="red",
     xlim=c(0,1),ylim=c(0,1),
     xlab=paste("FP","\n","AUC=",round(tROC$AUC,3)),
     ylab="TP",main="tROC,Method=KM\n year=3")
abline(0,1,col="gray",lty=2)

#将三个曲线连接在一起
## 5年
library(ggplot2)
plot(bROC$FP, bROC$TP, ## x=FP,y=TP
     type="l",col="#BC3C29FF", ##线条设置
     xlim=c(0,1), ylim=c(0,1),   
     xlab=("1-specifisity"), ##连接
     ylab="Sensitivity",
     lwd=1.5,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2,
     main="Time dependent ROC")## \n换行符
abline(0,1,col="gray",lty=1.5)##线条颜色
legend(0.2,0.9,c(paste("Stage for extremities AUC  =",round(bROC$AUC,100))),
       x.intersp=1, y.intersp=0.8,
       lty= 1 ,lwd= 2,col=c("#BC3C29FF"),
       bty = "n",# bty框的类型
       seg.len=1.2,cex=0.8)




## 5年
#lines(mROC$FP, mROC$TP, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1),lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2)
lines(zROC$FP, zROC$TP, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1),lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2)
lines(gROC$FP, gROC$TP, type="l",col="#20854EFF",xlim=c(0,1), ylim=c(0,1),lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2)
legend(0.2,0.9,c(paste("Stage for extremities AUC  =",round(yROC$AUC,100)),
                 paste("Stage for visceral orange AUC  =",round(zROC$AUC,3)),
                 paste("FIGO AUC =",round(gROC$AUC,5))),
       x.intersp=1, y.intersp=0.8,
       lty= 1 ,lwd= 2,col=c("#BC3C29FF","#0072B5FF","#20854EFF"),
       bty = "n",# bty框的类型
       seg.len=1.2,cex=0.8)

#nomogram模型和大小肉瘤的分期比较
#自己的数据，以及seer中的数据
setwd('C:\\Users\\DELL\\Desktop')
y<-read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\seer数据库8.7\\all.txt", header=T)
cutoff<-60
yROC=survivalROC(Stime=y$time,status=y$status,marker=y$lp,
                 predict.time=cutoff,method="KM")#四肢类
cutoff<-36
mROC=survivalROC(Stime=y$time,status=y$status,marker=y$lp,
                 predict.time=cutoff,method="KM")#四肢类
plot(yROC$FP, yROC$TP, ## x=FP,y=TP
     type="l",col="#BC3C29FF", ##线条设置
     xlim=c(0,1), ylim=c(0,1),   
     xlab=("1-specifisity"), ##连接
     ylab="Sensitivity",
     lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2,
     main="Time dependent ROC")## \n换行符
abline(0,1,col="gray",lty=2)##线条颜色
lines(mROC$FP, mROC$TP, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1),lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2)

legend(0.5,1.2,c(paste("Nomogram 5-year AUC  =",round(yROC$AUC,5)),
                 paste("Nomogram 3-year AUC  =",round(mROC$AUC,3))),
       x.intersp=1, y.intersp=0.8,
       lty= 1 ,lwd= 2,col=c("#BC3C29FF","#0072B5FF"),
       bty = "n",# bty框的类型
       seg.len=1.2,cex=0.8)








#ggsci美化(还未实现)
require(ggsci)
library(ggsci)
library("scales")
pal_nejm("default")(8)
show_col(pal_nejm("default")(8))

## seer数据库
library(survivalROC)
#五年的生存分析
setwd('C:\\Users\\DELL\\Desktop')
y<-read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\seer数据库\\包含了原发部位手术信息.txt", header=T)
y$stage<-factor(y$stage,
                levels=c(1,2,3,4))
y$size<-factor(y$size,
               levels=c(1,2,3,4))
res.cox<-coxph(Surv(time,status)~size+grade2+stage+chemo,data=y)
summary(res.cox)#查看摘要
y$lp<-predict(res.cox,type="lp")

cutoff<-60
yROC=survivalROC(Stime=y$time,status=y$status,marker=y$lp,
                 predict.time=cutoff,method="KM")#四肢类
cutoff<-36
tROC=survivalROC(Stime=y$time,status=y$status,marker=y$stage,
                 predict.time=cutoff,method="KM")#四肢类

plot(yROC$FP, yROC$TP, ## x=FP,y=TP
     type="l",col="#BC3C29FF", ##线条设置
     xlim=c(0,1), ylim=c(0,1),   
     xlab=("1-specificity"), ##连接
     ylab="Sensitivity",
     lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2,
     main="Time dependent ROC")## \n换行符
abline(0,1,col="gray",lty=2)##线条颜色
lines(tROC$FP, tROC$TP, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1),
      lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2)
legend(0.6,0.6,c(paste("AUC of 5 =",round(yROC$AUC,5)),
                 paste("AUC of 3 =",round(tROC$AUC,3))),
       x.intersp=1, y.intersp=0.8,
       lty= 1 ,lwd= 2,col=c("#BC3C29FF","#0072B5FF"),
       bty = "n",# bty框的类型
       seg.len=1,cex=0.8)#
#seer数据库中子宫肉瘤做比较
y<-read.table("C:\\Users\\DELL\\Desktop\\seer.txt", header=T)
cutoff<-12
bROC=survivalROC(Stime=y$time,status=y$status,marker=y$stage,
                 predict.time=cutoff,method="KM")
aROC=survivalROC(Stime=y$time,status=y$status,marker=y$FIGO,
                 predict.time=cutoff,method="KM")#四肢类
plot(bROC$FP, bROC$TP, ## x=FP,y=TP
     type="l",col="#BC3C29FF", ##线条设置
     xlim=c(0,1), ylim=c(0,1),   
     xlab=("1-specificity"), ##连接
     ylab="Sensitivity",
     lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2,
     main="Time dependent ROC")## \n换行符
abline(0,1,col="gray",lty=2)##线条颜色
lines(aROC$FP, aROC$TP, type="l",col="#0072B5FF",xlim=c(0,1), ylim=c(0,1),
      lwd=2,cex.main=1,cex.lab=1.2,cex.axis=1.2,font=1.2)
legend(0.6,0.6,c(paste("AUC of stage =",round(bROC$AUC,1)),
                 paste("AUC of figo =",round(aROC$AUC,1))),
       x.intersp=1, y.intersp=0.8,
       lty= 1 ,lwd= 2,col=c("#BC3C29FF","#0072B5FF"),
       bty = "n",# bty框的类型
       seg.len=1,cex=0.8)#

#用timeROC包绘制ROC曲线。比survivalROC包方便快捷很多

library(timeROC)
ROC <- timeROC(T=x$time,   
               delta=x$status,   
               marker=x$lp,   
               cause=1,                #阳性结局指标数值
               weighting="marginal",   #计算方法，默认为marginal
               ROC= TRUE,
               times=c(36, 60),       #时间点，选取1年，3年和5年的生存率
               iid=TRUE)
plot(ROC, 
     time=36, col="#BC3C29FF", lwd=2, title = "")  
##连接
#time是时间点，col是线条颜色
plot(ROC,
     time=60, col="#0072B5FF", add=TRUE, lwd=2) 
legend("bottomright",
       c(paste0("AUC at 3 year: ",round(ROC[["AUC"]][1],5)), 
         paste0("AUC at 5 year: ",round(ROC[["AUC"]][2],5))), 
       col=c("#BC3C29FF", "#0072B5FF"),
       lty=1, lwd=2,bty = "n")
confint(ROC)$CI_AUC
#seer中的数据：
library(timeROC)
library(survival)
library(survminer)
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\seer数据库8.7\\9.7其他库.txt", header=T)

x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\9.6数据.txt", header=T)

x<-read.table("C:\\Users\\DELL\\Desktop\\癌肉瘤\\数据分析\\新建文本文档.txt", header=T)
#x$stage7<-factor(x$stage7,
                #levels=c(1,2,3,4,5)) 这里不能给标签,否则会出错
sROC <- timeROC(T=x$time,   
                delta=x$status2,   
                marker=x$stage2,   
                cause=1,                #阳性结局指标数值
                weighting="marginal",   #计算方法，默认为marginal
                ROC= TRUE,
                times=c(36, 60),       #时间点，选取1年，3年和5年的生存率
                iid=TRUE)
plot(sROC, 
     time=36, col="#BC3C29FF", lwd=2, title = "")   #time是时间点，col是线条颜色
plot(sROC,
     time=60, col="#0072B5FF", add=TRUE, lwd=2) 
legend("bottomright",
       c(paste0("AUC at 3 year: ",round(sROC[["AUC"]][1],3)), 
         paste0("AUC at 5 year: ",round(sROC[["AUC"]][2],3))), 
       col=c("#BC3C29FF", "#0072B5FF"),
       lty=1, lwd=2,bty = "n")
confint(sROC)$CI_AUC
