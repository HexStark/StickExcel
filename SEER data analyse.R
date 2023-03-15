setwd('C:\\Users\\DELL\\Desktop')
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\seer数据库8.7\\8.16数据.txt", header=T)
library(survminer)
library(survival)

#分析不同的分期之间是否存在差异
x$size<-factor(x$size,
               levels=c(1,2,3))
x$stage<-factor(x$stage,
                levels=c(1,2,3,4))
x$site<-factor(x$site,
               levels=c(1,2,3,4,5))
x$bingli2<-factor(x$bingli2,
                  levels=c(1,2,3,4))
x$bingli<-factor(x$bingli,
                 levels=c(1,2,3,4,5,6,7))
x$Tstage<-factor(x$Tstage,
                 levels=c(1,2,3))
x$stage3<-factor(x$stage3,
                 levels=c(1.1,1.2,2,3.1,3.2,3.3,4,4.2))
x$FIGO<-factor(x$FIGO,
               levels=c(1,2,3,4))
x$stage2<-factor(x$stage2,
                 levels=c(1.1,1.2,2,3.1,3.2,4))

x$grade1<-factor(x$grade1,
                 levels=c(1,2,3,4))



#labels=c('Yes','No'))
str(x)
#寿命表查看生存率
fit <- survfit(Surv(time, status)~1, data=x)

# 获得的survial列就是生存率 
summary(fit)
#多因素方差分析
read.cox <- coxph(Surv(time, status)~age+bingli+stage2+site+sex,data=x,method="efron")
summary(read.cox)
#单因素方差分析
read.cox <- coxph(Surv(time, status)~stage+age+bingli2,data=x,method="efron")
summary(read.cox)

#分析不同分期是否存在差异
ggsurvplot(survfit(Surv(time,status)~stage3,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           break.x.by = 50,#横坐标间隔
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
           legend.labs = c("StageIA","StageIB","StageII","StageIIIA","StageIIIB","LNM","StageIV","LNM+M"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())  

ggsurvplot(survfit(Surv(time,status)~stage2,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           break.x.by = 50,#横坐标间隔
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
           legend.labs = c("StageIA","StageIB","StageII","StageIIIA","StageIIIB","StageIV"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())  

ggsurvplot(survfit(Surv(time,status)~Tstage,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           break.x.by = 50,#横坐标间隔
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
           legend.labs = c("T1","T2","T3+T4"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())

#分4期的分期
ggsurvplot(survfit(Surv(time,status)~FIGO,data=x),
           #conf.int=TRUE, #有置信区间
           #conf.int.style = "step",
           linetype = 1,
           xlab="Time(months)",
           break.x.by = 50,#横坐标间隔
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

#分析不同size的差异
ggsurvplot(survfit(Surv(time,status)~size,data=x),
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
           legend.labs = c("a0-5cm","a>5-10cm","a>10-15cm","a>15cm"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())  


#分析不同的大小之间是否存在差异
x$size<-factor(x$size,
               levels=c(1,2,3,4))
read.cox <- coxph(Surv(time, status)~size,data=x,method="efron")
summary(read.cox)

#绘图不同部位的差异
ggsurvplot(survfit(Surv(time,status)~site,data=x),
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
           legend.labs = c("femalegenital","urinarysystem","digestivesystem","respiratorysystem","other"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 

#分析化疗之间是否存在差异

read.cox <- coxph(Surv(time, status)~site,data=x,method="efron")
summary(read.cox)

#绘图
ggsurvplot(survfit(Surv(time,status)~grade1,data=x),
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
           legend.labs = c("Grade1","Grade2","Grade3","Grade4"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 

#分析淋巴转移之间是否存在差异
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
           legend.labs = c("No","Yes"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 
read.cox <- coxph(Surv(time, status)~radiation,data=x,method="efron")
summary(read.cox)

#绘图
ggsurvplot(survfit(Surv(time,status)~radiation,data=x),
           #conf.int=TRUE, #有置信区间\
           break.time.by=50,
           #conf.int.style = "step",
           xlab="Time in Month",
           pval=TRUE, #P值
           censor.shape = "+",
           pval.size = 3,
           pval.coord = c(2,0.1),
           risk.table = TRUE,  #风险表
           tables.height = 0.4,
           risk.table.fontsize=3,
           ggtheme = theme_minimal(),
           palette = "lancet",#按照lancet方式调节颜色？
           surv.median.line = "hv",#显示出中位生存值
           risk.table.col = "strata", #风险表颜色
           legend.title="Whether received radiotherapy", #曲线标题为空
           legend.labs = c("Yes","No")
           #legend=c(0.9,0.75), #在纵轴0.7和0.8的位置添加变量信息
)

#女性子宫分期
setwd('C:\\Users\\DELL\\Desktop')
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\seer数据库8.7\\女性生殖系统.txt", header=T)

read.cox <- coxph(Surv(time, status)~linba,data=x,method="efron")
summary(read.cox)

#绘图
ggsurvplot(survfit(Surv(time,status)~stage,data=x),
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
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 
#分期不同级别
ggsurvplot(survfit(Surv(time,status)~grade2,data=x),
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
           surv.median.line = "hv",#显示出中位生存值
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
           legend.labs = c("Low grade","High grade"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer())  
#不同部位
x <- read.table("C:\\Users\\DELL\\Desktop\\肉瘤病人分析\\seer数据库8.7\\8.10筛选后4650人数据.txt", header=T)
x$site<-factor(x$site,
               levels=c(1,2,3,4,5,6))
ggsurvplot(survfit(Surv(time,status)~site,data=x),
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
           legend.labs = c("Femalegenitalsystem","Urinarysystem","Digestivesystem","respiratorysystem","other"),
           #legend=c(0.92,0.78), #在纵轴0.7和0.8的位置添加变量信息
           #ggtheme = theme_minimal()没有边框线,还有light型
           ggtheme = theme_survminer()) 