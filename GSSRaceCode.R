#Code to analyze differences between blacks and whites
# on their respective beliefs about relative intelligence 
# between the races (net of underlying rates of intelligence ranking)
#Author: Chris Marcum <cmarcum@uci.edu>
#Date: 25 October 2018

#Work inspired by Phil Cohen


#Read in the latest data, which you can get from the GSS website
library(foreign)
gss<-read.spss('/home/marcumcs/Downloads/GSS_spss/GSS7216_R4.sav',to.data.frame=TRUE)

#Generate some relative INTL comparisons
gss$BGW<-as.numeric(gss$INTLWHTS)<as.numeric(gss$INTLBLKS)
gss$BLW<-as.numeric(gss$INTLWHTS)>as.numeric(gss$INTLBLKS)
gss$BEW<-as.numeric(gss$INTLWHTS)==as.numeric(gss$INTLBLKS)

#Subset out others, missing, and nulls, and then 
# recode some covars
gss.ss<-gss[which(gss$RACE!="OTHER"|is.na(gss$RACE)),]
gss.ss<-gss.ss[which(gss.ss$YEAR>=1996),]
gss.ss$INCOME2<-gss.ss$INCOME;
levels(gss.ss$INCOME2)<-c(rep("LOW",nlevels(gss.ss$INCOME)-1),"HIGH")

gss.ss$RACE2<-relevel(as.factor(as.character(gss.ss$RACE)),ref="WHITE")
gss.ss$EDUC2<-as.numeric(as.character(gss.ss$EDUC))
gss.ss$YEAR2<-as.factor(gss.ss$YEAR)

set.seed(8675309)
library(lme4)
m1.bgw<-glmer(BGW~RACE2+SEX+INCOME2+EDUC2+(1|YEAR2),data=gss.ss,family="binomial",control=glmerControl(optCtrl=list(maxfun=150000)))
#These fail to converge unless the convex hull is really tightly controlled (maybe local maxima problem)
m1.blw<-glmer(BLW~RACE2+SEX+INCOME2+EDUC2+(1|YEAR2),data=gss.ss,family="binomial",control=glmerControl(optCtrl=list(maxfun=150000),tolPwrss=1e-9,boundary.tol = 1e-8))
m1.bew<-glmer(BEW~RACE2+SEX+INCOME2+EDUC2+(1|YEAR2),data=gss.ss,family="binomial",control=glmerControl(optCtrl=list(maxfun=150000),tolPwrss=1e-5,boundary.tol = 1e-8))


#Make predictions by marginalizing data over model and taking inverse logit to get probability
res<-rbind(by(predict(m1.bgw),model.matrix(m1.bgw)[,2],function(x) mean(plogis(x))),
 by(predict(m1.blw),model.matrix(m1.blw)[,2],function(x) mean(plogis(x))),
 by(predict(m1.bew),model.matrix(m1.bew)[,2],function(x) mean(plogis(x)))
)

colnames(res)<-c("Whites","Blacks")
rownames(res)<-c("B>W","B<W","B=W")

png("RXIB.png",width=1200,height=1200,res=150)
par(xpd=TRUE,mar=c(4,4,6,1))
barplot(t(res),beside=TRUE,las=1,col=c("white","black"),ylab="Predicted Probability",ylim=c(0,1),main="Predicted Probabilities of Holding Particular Beliefs \n about Racial Differences in Intelligence by Race \n Models Adjusted for Gender, Income, and Education \n (with random effects intercepts by year, GSS Data 1996-2016)",xlab="Beliefs")
dev.off()



