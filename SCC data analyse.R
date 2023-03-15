install.packages("xlsx")
#载入数据
file.choose()
head("cancer")
file.choose()
head(cancer)
#读取桌面上的文件，并把我们的数据集命名为X
setwd('C:\\Users\\DELL\\Desktop')
x <- read.table("C:\\Users\\DELL\\Desktop\\脏器肉瘤\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\9.6数据.txt", header=T)
#查看数据的前6行
head(x)
#寿命表查看生存率
fit <- survfit(Surv(time, status)~1, data=x)

# 获得的survial列就是生存率 
summary(fit)

#要载入R包才可进行运算
library(survminer)
library(survival)

#给变量赋值
x$FIGO<-factor(x$FIGO,
               levels=c(1,2,3,4))
x$size2<-factor(x$size2,
                levels=c(1,2,3,4))
x$bingli<-factor(x$bingli,
                 levels=c(1,2,4,5,6,7,8))
x$bingli2<-factor(x$bingli2,
                  levels=c(1,2,3,4))
x$xitong<-factor(x$xitong,
                 levels=c(1,2,3,4,7))
x$RT<-factor(x$RT,
             levels=c(2,1))
x$chemo<-factor(x$chemo,
                levels=c(2,1))
x$FIGO<-factor(x$FIGO,
               levels=c(1,2,3,4))

x$stage11<-factor(x$stage11,
                  levels=c(1.1,1.2,2,3.1,3.2,4))

x$stage10<-factor(x$stage10,
                  levels=c(1,2,3,4))

x$Tstage<-factor(x$Tstage,
                 levels=c(1,2,3,4))
x$size2<-factor(x$size2,
                levels=c(1,2,3,4))
x$FIGO<-factor(x$FIGO,
               levels=c(1,2,3,4))

str(x)
#单变量COX回归
read.cox <- coxph(Surv(time, status)~sex, data=x,method="efron")
summary(read.cox)#查看摘要

read.cox <- coxph(Surv(time, status)~stage11, data=x,method="efron")
summary(read.cox)

#注意每个变量的大小写。否则读取不出来的
stage.cox <- coxph(Surv(time, status)~stage10, data=x,method="efron")
summary(stage.cox)

#多变量的cox分析
mutil.cox<-coxph(Surv(time,status)~age+bingli2+FIGO,data=x)
summary(mutil.cox)#查看摘要

#可视化估计多因素情况下的生存时间分布
ggsurvplot(survfit(mutil.cox),color = "#00AFBB" ,data=x,
           ggtheme = theme_minimal())#设置背景颜色
#可视化估计单因素情况下的生存分布

#分析不同stage的差异
install.packages("showtext")
library(showtext)

ggsurvplot(survfit(Surv(time,status)~xitong,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("female genital","urinary system","digestive system","respiratory system","other"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()
) #更改背景颜色



savePlot(filename ="Rplot",
         type = c("png"),
         device = dev.cur(),
         restoreConsole = TRUE)
ggsave("p.eps",width = 6,height = 5)  ##ggplot 中直接保存
png("p.png",width = 1200,height = 1000,units = "px")
p
dev.off() ## png先创建文件再画图保存

#不同大小/淋巴/化疗/分级/的差异
#大小
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\验证化疗作用.txt", header=T)
ggsurvplot(survfit(Surv(time,status)~FIGO,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",
           #palette =c("#BC3C29FF","#0072B5FF"),
           #按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("StageI","StageII","StageIII","StageIV"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()
) #更改背景颜色
ggsurvplot(survfit(Surv(time,status)~stage10,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("StageI","StageII","StageIII","StageIV"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 

#淋巴转移的差异

ggsurvplot(survfit(Surv(time,status)~linba,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",
           #palette =c("#BC3C29FF","#0072B5FF"),
           #按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("NoLNM","LNM"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())

#T分期
ggsurvplot(survfit(Surv(time,status)~size2,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("T1","T2","T3","T4"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 
#大小的差异
ggsurvplot(survfit(Surv(time,status)~size2,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("0-5","5-10","10-15","dayu15"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())


#分级的差异
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\大小.txt", header=T)
ggsurvplot(survfit(Surv(time,status)~grade,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("Lowgrade","Highgrade"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())


#女性子宫的差异
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\女性生殖.txt", header=T)
x$stage7<-factor(x$stage7,
                 levels=c(1,2,3,4))
ggsurvplot(survfit(Surv(time,status)~FIGO,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("StageI","StageII","StageIII","StageIV"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 

ggsurvplot(survfit(Surv(time,status)~Stage10,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("StageI","StageII","StageIII","StageIV"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())

#术后化疗的差异
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\验证化疗作用.txt", header=T)

ggsurvplot(survfit(Surv(time,status)~chemo,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 3,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "lancet",#按照lancet方式调节颜色？
           surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.legend=c(10),
           legend.title=" ", #曲线标题为空
           legend.labs = c("Chemotherapy","No chemotherapy"),
           legend=c(0.9,0.78), #在纵轴0.7和0.8的位置添加变量信息
           ggtheme = theme_minimal())  

ggsave(filename, file='filename.pdf', width=12, height=10)
#淋巴转移对生存的影响
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\can1.txt", header=T)

ggsurvplot(survfit(Surv(time,status)~linba,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 3,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "lancet",#按照lancet方式调节颜色？
           surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.legend=c(10),
           legend.title=" ", #曲线标题为空
           legend.labs = c("No","Yes"),
           legend=c(0.9,0.8), #在纵轴0.7和0.8的位置添加变量信息
           ggtheme = theme_minimal())  
ggsave(filename='filename.pdf',plot=filename, width=12, height=10)
#不同部位的影响
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\验证化疗作用.txt", header=T)

ggsurvplot(survfit(Surv(time,status)~xitong,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           #pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("Respiratory system","Urinary system","Male genital system","Female genital system","Oropharynx","Digestive system"),
           legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())  

#验证女性FIGO分期
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\女性生殖.txt", header=T)
ggsurvplot(survfit(Surv(time,status)~stage7,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           #pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("StageI","StageII","StageIII","StageIV"),
           legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())  
#验证化疗
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\化疗和放疗.txt", header=T)
ggsurvplot(survfit(Surv(time,status)~chemo,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           xlab="Time(months)",
           pval=TRUE, #P值
           pval.size = 3,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.legend=c(10),
           legend.title=" ", #曲线标题为空
           legend.labs = c("chemotherapy","Nochemotherapy"),
           legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           ggtheme = theme_survminer())  

#验证肿瘤大小对病人预后的影响
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\使用NCCN脏器肉瘤的分析方法\\验证化疗作用.txt", header=T)
x$size2<-factor(x$size2,
                levels=c(1,2,3,4))
ggsurvplot(survfit(Surv(time,status)~size2,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           #pval=TRUE, #P值
           pval.size = 4,
           risk.table = TRUE,  #风险表
           tables.height = 0.3,
           risk.table.fontsize=3.5,
           tables.theme = theme_cleantable(),
           #theme_cleantable(),#表格的主题
           risk.table.y.text.col = TRUE,
           #palette = c("blue", "purple"),可讲曲线调节为自己想要的颜色
           palette = "jco",#按照lancet方式调节颜色？
           #surv.median.line = "hv",#显示出中位生存值
           #cumevents = TRUE,#显示出下边两个表格
           #ncensor.plot = TRUE,#显示出下边两个表格
           censor.shape = "+",#有1234几种选项
           risk.table.col = "strata", #风险表颜色
           #risk.table.y.text = FALSE, # 用条形图代替文字标注
           main = "Survival curve",#标题为survival curve
           font.main = c(12, "darkblue"),
           font.y=c(10),
           font.tickslab=10,
           font.legend=10,
           fontsize=10,
           legend.title=" ", #曲线标题为空
           legend.labs = c("g0-5cm","g>5-10cm","g>10-15cm","g>15cm"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 