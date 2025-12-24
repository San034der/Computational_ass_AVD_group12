
# setup -------------------------------------------------------------------

#general
rm(list=ls())
cat("\014")
set.seed(299792458)
library(ggplot2)
library(dplyr)
library(tidyr)

#data and variable definition
data<-read.csv("ScanRecords.csv")
data1<-data
colnames(data1)
colnames(data1)<-c("Date","Time","ScanTime","PatientType")
nrows<-nrow(data1)
data2<-data1%>%separate(col = 1,into = c("a","b","c"),sep = "/",extra = "merge")
data2$MO<-rep(0,nrows)
data2$MO[data2$b %in% c('4','11','18','25')]<-1
data2$TU<-rep(0,nrows)
data2$TU[data2$b %in% c('5','12','19','26')]<-1
data2$WE<-rep(0,nrows)
data2$WE[data2$b %in% c('6','13','20','27')]<-1
data2$TH<-rep(0,nrows)
data2$TH[data2$b %in% c('7','14','21','28')]<-1
data2$FR<-rep(0,nrows)
data2$FR[data2$b %in% c('1','8','15','22','29')]<-1
data2$W1<-rep(0,nrows)
data2$W1[data2$b %in% c('1','4','5','6','7','8')]<-1
data2$W2<-rep(0,nrows)
data2$W2[data2$b %in% c('11','12','13','14','15')]<-1
data2$W3<-rep(0,nrows)
data2$W3[data2$b %in% c('18','19','20','21','22')]<-1
data2$W4<-rep(0,nrows)
data2$W4[data2$b %in% c('25','26','27','28','19')]<-1
g<-c(1,4:8,11:15,18:22,25:29)
k<-0
type1Freq<-vector()
type2Freq<-vector()
for(i in g){
  k<-k+1
  subset<-data2%>%filter(b==i)
  a<-as.data.frame(table(subset$PatientType))
  type1Freq[k]<-a[1,2]
  type2Freq[k]<-a[2,2]
}
data5<-data.frame(type1Freq,type2Freq)
data5<-data5%>%mutate(sum=type1Freq+type2Freq)
data5<-data5%>%mutate(ratio=type2Freq/type1Freq)
data2$Date<-data1$Date
data3<-data2[-c(1:3)]
data3$ScanTime<-data3$ScanTime*60

#preliminary investigation
summary(data3)
str(data3)
class(data3)
range(data3$Date)
range(data3$Time)
range(data3$ScanTime)
table(data3$PatientType)
table(data3$Date)
ggplot(data3, aes(x=ScanTime)) + geom_histogram(aes(y=..density..), colour="black", fill="steelblue")+geom_density(alpha=.2,fill= "red")+theme_minimal()+xlab("")+ylab("")
ggplot(data3, aes(x=Time)) + geom_histogram(aes(y=..density..), colour="black", fill="steelblue")+geom_density(alpha=.2,fill= "red")+theme_minimal()+xlab("")+ylab("")

    # analysis for types 1 & 2
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(PatientType ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(PatientType ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    #analysis for weeks 1 & 2 & 3 & 4 of august 
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(W1 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(W1 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()

    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(W2 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(W2 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(W3 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(W3 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(W4 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(W4 ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    #analysis for workdays MO & TU & WE & TH & FR of august 
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(MO ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(MO ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(TU ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(TU ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(WE ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(WE ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(TH ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(TH ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    
    ggplot(data3, aes(x=ScanTime))+geom_histogram(color="black", fill="steelblue")+facet_grid(FR ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()
    ggplot(data3, aes(x=Time))+geom_histogram(color="black", fill="steelblue")+facet_grid(FR ~ .)+xlab("")+ylab("")+labs(title =)+theme_minimal()

#functions
Bootstrap.moments.NP<-function(B,x,alpha){
q1<-1:B
q2<-1:B
q3<-1:B
q4<-1:B
q5<-1:B
q6<-1:B
q7<-1:B
nrows<-length(x)
  for(i in 1:B){
    boot<-sample.int(nrows,size = nrows,replace = T)
    boot<-x[boot]
    S<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^3)
    K<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^4)
    q1[i]<-mean(boot)
    q2[i]<-sd(boot)
    q3[i]<-S
    q4[i]<-K
    q5[i]<-median(boot)
    q6[i]<-quantile(boot,0.25)
    q7[i]<-quantile(boot,0.75)
}
#statistics
b1<-round(mean(x),4)
b2<-round(var(x)^0.5,4)
s1<-((sum((x-mean(x))^2))/(nrows-1))^0.5
se1<-s1*((nrows-1)/nrows)^0.5
b3<-round((1/nrows)*sum(((x-mean(x))/se1)^3),4)
b4<-round((1/nrows)*sum(((x-mean(x))/se1)^4),4)
JB<-nrows/6*(b3^2+0.25*(b4-3)^2)
P3<-round(pchisq(JB,df=2 ,F,F),4)
b5<-round(min(x),4)
b6<-round(max(x),4)
b7<-round(median(x),4)
b8<-round(quantile(x,0.25),4)
b9<-round(quantile(x,0.75),4)
#confidence interval
qsort1<-sort(q1,na.last = T)
qsort2<-sort(q2,na.last = T)
qsort3<-sort(q3,na.last = T)
qsort4<-sort(q4,na.last = T)
qsort5<-sort(q5,na.last = T)
qsort6<-sort(q6,na.last = T)
qsort7<-sort(q7,na.last = T)
#two sided interval mean
L1<-round(quantile(qsort1,alpha/2),4)
U1<-round(quantile(qsort1,1-alpha/2),4)
#two sided interval standard deviation
L2<-round(quantile(qsort2,alpha/2),4)
U2<-round(quantile(qsort2,1-alpha/2),4)
#two sided interval skewness
L3<-round(quantile(qsort3,alpha/2),4)
U3<-round(quantile(qsort3,1-alpha/2),4)
#two sided interval kurtosis
L4<-round(quantile(qsort4,alpha/2),4)
U4<-round(quantile(qsort4,1-alpha/2),4)
#two sided interval median q-25 and q-75
L5<-round(quantile(qsort5,alpha/2),4)
U5<-round(quantile(qsort5,1-alpha/2),4)
L6<-round(quantile(qsort6,alpha/2),4)
U6<-round(quantile(qsort6,1-alpha/2),4)
L7<-round(quantile(qsort7,alpha/2),4)
U7<-round(quantile(qsort7,1-alpha/2),4)
# results
print("output: bootstrap confidence interval of moments non-parametric")
print("")
print(paste("the mean of the data is:", b1))
print(paste("the standard deviation of the data is:", b2))
print(paste("the skewness of the data is:", b3))
print(paste("the kurtosis of the data is:", b4))
print(paste("the minimum of the data is:", b5))
print(paste("the maximum of the data is:", b6))
print(paste("the median of the data is:", b7))
print(paste("the 25% percentile of the data is:", b8))
print(paste("the 75% percentile of the data is:", b9))
print("")
print(paste("The P-value that the series is normally distributed:",P3))
print("")
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the mean is: [",L1,",",U1,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the standard deviation is : [",L2,",",U2,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the skewness is: [",L3,",",U3,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the kurtosis is: [",L4,",",U4,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the median is: [",L5,",",U5,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the 25-percentile is: [",L6,",",U6,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the 75-percentile is: [",L7,",",U7,"]"))
}
Bootstrap.moments.norm<-function(B,x,alpha){
 nrows<-length(x)
 a1<-mean(x)
 a2<-var(x)^0.5
 #bootstrap
 q1<-1:B
 q2<-1:B
 q3<-1:B
 q4<-1:B
 q5<-1:B
 q6<-1:B
 q7<-1:B
 nrows<-length(x)
 for(i in 1:B){
   boot<-rnorm(nrows,a1,a2)
   S<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^3)
   K<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^4)
   q1[i]<-mean(boot)
   q2[i]<-sd(boot)
   q3[i]<-S
   q4[i]<-K
   q5[i]<-median(boot)
   q6[i]<-quantile(boot,0.25)
   q7[i]<-quantile(boot,0.75)
 }
 #statistics
 b1<-round(mean(x),4)
 b2<-round(var(x)^0.5,4)
 s1<-((sum((x-mean(x))^2))/(nrows-1))^0.5
 se1<-s1*((nrows-1)/nrows)^0.5
 b3<-round((1/nrows)*sum(((x-mean(x))/se1)^3),4)
 b4<-round((1/nrows)*sum(((x-mean(x))/se1)^4),4)
 JB<-nrows/6*(b3^2+0.25*(b4-3)^2)
 P3<-round(pchisq(JB,df=2 ,F,F),4)
 b5<-round(min(x),4)
 b6<-round(max(x),4)
 b7<-round(median(x),4)
 b8<-round(quantile(x,0.25),4)
 b9<-round(quantile(x,0.75),4)
 #confidence interval
 qsort1<-sort(q1,na.last = T)
 qsort2<-sort(q2,na.last = T)
 qsort3<-sort(q3,na.last = T)
 qsort4<-sort(q4,na.last = T)
 qsort5<-sort(q5,na.last = T)
 qsort6<-sort(q6,na.last = T)
 qsort7<-sort(q7,na.last = T)
 #two sided interval mean
 L1<-round(quantile(qsort1,alpha/2),4)
 U1<-round(quantile(qsort1,1-alpha/2),4)
 #two sided interval standard deviation
 L2<-round(quantile(qsort2,alpha/2),4)
 U2<-round(quantile(qsort2,1-alpha/2),4)
 #two sided interval skewness
 L3<-round(quantile(qsort3,alpha/2),4)
 U3<-round(quantile(qsort3,1-alpha/2),4)
 #two sided interval kurtosis
 L4<-round(quantile(qsort4,alpha/2),4)
 U4<-round(quantile(qsort4,1-alpha/2),4)
 #two sided interval median q-25 and q-75
 L5<-round(quantile(qsort5,alpha/2),4)
 U5<-round(quantile(qsort5,1-alpha/2),4)
 L6<-round(quantile(qsort6,alpha/2),4)
 U6<-round(quantile(qsort6,1-alpha/2),4)
 L7<-round(quantile(qsort7,alpha/2),4)
 U7<-round(quantile(qsort7,1-alpha/2),4)
 # results
 print("output: bootstrap confidence interval of moments parametric normal")
 print("")
 print(paste("the mean of the data is:", b1))
 print(paste("the standard deviation of the data is:", b2))
 print(paste("the skewness of the data is:", b3))
 print(paste("the kurtosis of the data is:", b4))
 print(paste("the minimum of the data is:", b5))
 print(paste("the maximum of the data is:", b6))
 print(paste("the median of the data is:", b7))
 print(paste("the 25% percentile of the data is:", b8))
 print(paste("the 75% percentile of the data is:", b9))
 print("")
 print(paste("The P-value that the series is normally distributed:",P3))
 print("")
 print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the mean is: [",L1,",",U1,"]"))
 print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the standard deviation is : [",L2,",",U2,"]"))
 print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the skewness is: [",L3,",",U3,"]"))
 print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the kurtosis is: [",L4,",",U4,"]"))
 print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the median is: [",L5,",",U5,"]"))
 print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the 25-percentile is: [",L6,",",U6,"]"))
 print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the 75-percentile is: [",L7,",",U7,"]"))
}
Bootstrap.moments.poiss<-function(B,x,alpha){
nrows<-length(x)
a1<-mean(x)
#bootstrap
q1<-1:B
q2<-1:B
q3<-1:B
q4<-1:B
q5<-1:B
q6<-1:B
q7<-1:B
nrows<-length(x)
for(i in 1:B){
  boot<-rpois(nrows,a1)
  S<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^3)
  K<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^4)
  q1[i]<-mean(boot)
  q2[i]<-sd(boot)
  q3[i]<-S
  q4[i]<-K
  q5[i]<-median(boot)
  q6[i]<-quantile(boot,0.25)
  q7[i]<-quantile(boot,0.75)
}
#statistics
b1<-round(mean(x),4)
b2<-round(var(x)^0.5,4)
s1<-((sum((x-mean(x))^2))/(nrows-1))^0.5
se1<-s1*((nrows-1)/nrows)^0.5
b3<-round((1/nrows)*sum(((x-mean(x))/se1)^3),4)
b4<-round((1/nrows)*sum(((x-mean(x))/se1)^4),4)
JB<-nrows/6*(b3^2+0.25*(b4-3)^2)
P3<-round(pchisq(JB,df=2 ,F,F),4)
b5<-round(min(x),4)
b6<-round(max(x),4)
b7<-round(median(x),4)
b8<-round(quantile(x,0.25),4)
b9<-round(quantile(x,0.75),4)
#confidence interval
qsort1<-sort(q1,na.last = T)
qsort2<-sort(q2,na.last = T)
qsort3<-sort(q3,na.last = T)
qsort4<-sort(q4,na.last = T)
qsort5<-sort(q5,na.last = T)
qsort6<-sort(q6,na.last = T)
qsort7<-sort(q7,na.last = T)
#two sided interval mean
L1<-round(quantile(qsort1,alpha/2),4)
U1<-round(quantile(qsort1,1-alpha/2),4)
#two sided interval standard deviation
L2<-round(quantile(qsort2,alpha/2),4)
U2<-round(quantile(qsort2,1-alpha/2),4)
#two sided interval skewness
L3<-round(quantile(qsort3,alpha/2),4)
U3<-round(quantile(qsort3,1-alpha/2),4)
#two sided interval kurtosis
L4<-round(quantile(qsort4,alpha/2),4)
U4<-round(quantile(qsort4,1-alpha/2),4)
#two sided interval median q-25 and q-75
L5<-round(quantile(qsort5,alpha/2),4)
U5<-round(quantile(qsort5,1-alpha/2),4)
L6<-round(quantile(qsort6,alpha/2),4)
U6<-round(quantile(qsort6,1-alpha/2),4)
L7<-round(quantile(qsort7,alpha/2),4)
U7<-round(quantile(qsort7,1-alpha/2),4)
# results
print("output: bootstrap confidence interval of moments parametric poiss")
print("")
print(paste("the mean of the data is:", b1))
print(paste("the standard deviation of the data is:", b2))
print(paste("the minimum of the data is:", b5))
print(paste("the maximum of the data is:", b6))
print(paste("the median of the data is:", b7))
print(paste("the 25% percentile of the data is:", b8))
print(paste("the 75% percentile of the data is:", b9))
print("")
print(paste("The P-value that the series is normally distributed:",P3))
print("")
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the mean is: [",L1,",",U1,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the standard deviation is : [",L2,",",U2,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the median is: [",L5,",",U5,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the 25-percentile is: [",L6,",",U6,"]"))
print(paste0("a ",(1-alpha)*100,"% bootstrapped two sided confidance interval for the 75-percentile is: [",L7,",",U7,"]"))
}
Monte.carlo.sim.norm<-function(B,sim,sample,alpha){
r.simple.mean<-rep(0,sim)
r.method.mean<-rep(0,sim)
r.simple.sd<-rep(0,sim)
r.method.sd<-rep(0,sim)
r.simple.S<-rep(0,sim)
r.method.S<-rep(0,sim)
r.simple.K<-rep(0,sim)
r.method.K<-rep(0,sim)
r.simple.median<-rep(0,sim)
r.method.median<-rep(0,sim)
#simulate data

for(i in 1:sim){
x<-rnorm(sample,0,1)
nrows<-length(x)
#statistics
b1<-mean(x)
b2<-var(x)^0.5
s1<-((sum((x-mean(x))^2))/(nrows-1))^0.5
se1<-s1*((nrows-1)/nrows)^0.5
b3<-(1/nrows)*sum(((x-mean(x))/se1)^3)
b4<-(1/nrows)*sum(((x-mean(x))/se1)^4)
b5<-median(x)

#confidence intervals
L1.S<-b1-qnorm(1-alpha/2)*b2/(nrows)^0.5
U1.S<-b1+qnorm(1-alpha/2)*b2/(nrows)^0.5
L2.S<-b2*((nrows-1)/qchisq(1-alpha/2,nrows-1))^0.5
U2.S<-b2*((nrows-1)/qchisq(alpha/2,nrows-1))^0.5
L3.S<-b3-qnorm(1-alpha/2)*((6*nrows*(nrows-1))/((nrows-2)*(nrows+1)*(nrows+3)))^0.5/(nrows)^0.5
U3.S<-b3+qnorm(1-alpha/2)*((6*nrows*(nrows-1))/((nrows-2)*(nrows+1)*(nrows+3)))^0.5/(nrows)^0.5
L4.S<-b4-qnorm(1-alpha/2)*((24*nrows*(nrows-1)^2)/((nrows-3)*(nrows-2)*(nrows+3)*(nrows+5)))^0.5/(nrows)^0.5
U4.S<-b4+qnorm(1-alpha/2)*((24*nrows*(nrows-1)^2)/((nrows-3)*(nrows-2)*(nrows+3)*(nrows+5)))^0.5/(nrows)^0.5
L5.S<-b5-qnorm(1-alpha/2)/(2*dnorm(0)*(nrows)^0.5)
U5.S<-b5+qnorm(1-alpha/2)/(2*dnorm(0)*(nrows)^0.5)

if (0 >=L1.S  && 0 <= U1.S) {
  r.simple.mean[i]<-1
}
if (1 >=L2.S  && 1 <= U2.S) {
  r.simple.sd[i]<-1
}
if (0 >=L3.S  && 0 <= U3.S) {
  r.simple.S[i]<-1
}
if (3 >=L4.S  && 3 <= U4.S) {
  r.simple.K[i]<-1
}
if (0 >=L5.S  && 0 <= U5.S) {
  r.simple.median[i]<-1
}
#bootstrap method
                  q1<-1:B
                  q2<-1:B
                  q3<-1:B
                  q4<-1:B
                  q5<-1:B
                  nrows<-length(x)
                  for(j in 1:B){
                    boot<-sample.int(nrows,size = nrows,replace = T)
                    boot<-x[boot]
                    S<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^3)
                    K<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^4)
                    q1[j]<-mean(boot)
                    q2[j]<-sd(boot)
                    q3[j]<-S
                    q4[j]<-K
                    q5[j]<-median(boot)
                  }
              qsort1<-sort(q1,na.last = T)
              qsort2<-sort(q2,na.last = T)
              qsort3<-sort(q3,na.last = T)
              qsort4<-sort(q4,na.last = T)
              qsort5<-sort(q5,na.last = T)
              #two sided interval mean
              L1.B<-quantile(qsort1,alpha/2)
              U1.B<-quantile(qsort1,1-alpha/2)
              #two sided interval standard deviation
              L2.B<-quantile(qsort2,alpha/2)
              U2.B<-quantile(qsort2,1-alpha/2)
              #two sided interval skewness
              L3.B<-quantile(qsort3,alpha/2)
              U3.B<-quantile(qsort3,1-alpha/2)
              #two sided interval kurtosis
              L4.B<-quantile(qsort4,alpha/2)
              U4.B<-quantile(qsort4,1-alpha/2)
              #two sided interval median q-25 and q-75
              L5.B<-quantile(qsort5,alpha/2)
              U5.B<-quantile(qsort5,1-alpha/2)
              if (0 >=L1.B  && 0 <= U1.B) {
                r.method.mean[i]<-1
              }
              if (1 >=L2.B  && 1 <= U2.B) {
                r.method.sd[i]<-1
              }
              if (0 >=L3.B  && 0 <= U3.B) {
                r.method.S[i]<-1
              }
              if (3 >=L4.B  && 3 <= U4.B) {
                r.method.K[i]<-1
              }
              if (0 >=L5.B  && 0 <= U5.B) {
                r.method.median[i]<-1
              }
  }
ERF.S.mean<-1-round(mean(r.simple.mean),4)
ERF.B.mean<-1-round(mean(r.method.mean),4)
ERF.S.SD<-1-round(mean(r.simple.sd),4)
ERF.B.SD<-1-round(mean(r.method.sd),4)
ERF.S.S<-1-round(mean(r.simple.S),4)
ERF.B.S<-1-round(mean(r.method.S),4)
ERF.S.K<-1-round(mean(r.simple.K),4)
ERF.B.K<-1-round(mean(r.method.K),4)
ERF.S.median<-1-round(mean(r.simple.median),4)
ERF.B.median<-1-round(mean(r.method.median),4)
# results
print("output: comparison empirical rejection Bootstrap and normality")
print("")
print(paste("the empirical rejection freq. for the mean using Bootstrap is:",ERF.B.mean))
print(paste("the empirical rejection freq. for the mean using normality is:",ERF.S.mean))
print("")
print(paste("the empirical rejection freq. for the standard deviation using Bootstrap is:",ERF.B.SD))
print(paste("the empirical rejection freq. for the standard deviation using normality is:",ERF.S.SD))
print("")
print(paste("the empirical rejection freq. for the skewness using Bootstrap is:",ERF.B.S))
print(paste("the empirical rejection freq. for the skewness using normality is:",ERF.S.S))
print("")
print(paste("the empirical rejection freq. for the kurtosis using Bootstrap is:",ERF.B.K))
print(paste("the empirical rejection freq. for the kurtosis using normality is:",ERF.S.K))
print("")
print(paste("the empirical rejection freq. for the median using Bootstrap is:",ERF.S.median))
print(paste("the empirical rejection freq. for the median using normality is:",ERF.B.median))
print("")
}
Monte.carlo.sim.pois<-function(B,sim,sample,alpha){
  r.simple.mean<-rep(0,sim)
  r.method.mean<-rep(0,sim)
  r.simple.mean2<-rep(0,sim)
  r.simple.sd<-rep(0,sim)
  r.simple.sd2<-rep(0,sim)
  r.method.sd<-rep(0,sim)
  r.simple.S<-rep(0,sim)
  r.method.S<-rep(0,sim)
  r.simple.K<-rep(0,sim)
  r.method.K<-rep(0,sim)
  r.simple.median<-rep(0,sim)
  r.method.median<-rep(0,sim)
  #simulate data
  
  for(i in 1:sim){
    x<-rpois(sample,1)
    nrows<-length(x)
    #statistics
    b1<-mean(x)
    b2<-var(x)^0.5
    s1<-((sum((x-mean(x))^2))/(nrows-1))^0.5
    se1<-s1*((nrows-1)/nrows)^0.5
    b3<-(1/nrows)*sum(((x-mean(x))/se1)^3)
    b4<-(1/nrows)*sum(((x-mean(x))/se1)^4)
    b5<-median(x)
    
    #confidence intervals
    L1.S<-b1-qnorm(1-alpha/2)*b2/(nrows)^0.5
    U1.S<-b1+qnorm(1-alpha/2)*b2/(nrows)^0.5
    L1.S2<-qgamma(alpha/2,sum(x),1)/nrows
    U1.S2<-qgamma(1-alpha/2,sum(x)+1,1)/nrows
    L2.S<-b2*((nrows-1)/qchisq(1-alpha/2,nrows-1))^0.5
    U2.S<-b2*((nrows-1)/qchisq(alpha/2,nrows-1))^0.5
    L2.S2<-(qgamma(alpha/2,sum(x),1)/nrows)^0.5
    U2.S2<-(qgamma(1-alpha/2,sum(x)+1,1)/nrows)^0.5
    L3.S<-b3-qnorm(1-alpha/2)*((6*nrows*(nrows-1))/((nrows-2)*(nrows+1)*(nrows+3)))^0.5/(nrows)^0.5
    U3.S<-b3+qnorm(1-alpha/2)*((6*nrows*(nrows-1))/((nrows-2)*(nrows+1)*(nrows+3)))^0.5/(nrows)^0.5
    L4.S<-b4-qnorm(1-alpha/2)*((24*nrows*(nrows-1)^2)/((nrows-3)*(nrows-2)*(nrows+3)*(nrows+5)))^0.5/(nrows)^0.5
    U4.S<-b4+qnorm(1-alpha/2)*((24*nrows*(nrows-1)^2)/((nrows-3)*(nrows-2)*(nrows+3)*(nrows+5)))^0.5/(nrows)^0.5
    L5.S<-b5-qnorm(1-alpha/2)/(2*dnorm(0)*(nrows)^0.5)
    U5.S<-b5+qnorm(1-alpha/2)/(2*dnorm(0)*(nrows)^0.5)
    
    if (1 >=L1.S  && 1 <= U1.S) {
      r.simple.mean[i]<-1
    }
    if (1 >=L1.S2  && 1 <= U1.S2) {
      r.simple.mean2[i]<-1
    }
    if (1 >=L2.S  && 1 <= U2.S) {
      r.simple.sd[i]<-1
    }
    if (1 >=L2.S2  && 1 <= U2.S2) {
      r.simple.sd2[i]<-1
    }
    if (1 >=L3.S  && 1 <= U3.S) {
      r.simple.S[i]<-1
    }
    if (4 >=L4.S  && 4 <= U4.S) {
      r.simple.K[i]<-1
    }
    if (1 >=L5.S  && 1 <= U5.S) {
      r.simple.median[i]<-1
    }
    #bootstrap method
    q1<-1:B
    q2<-1:B
    q3<-1:B
    q4<-1:B
    q5<-1:B
    nrows<-length(x)
    for(j in 1:B){
      boot<-sample.int(nrows,size = nrows,replace = T)
      boot<-x[boot]
      S<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^3)
      K<-(1/nrows)*sum(((boot-mean(boot))/sd(boot))^4)
      q1[j]<-mean(boot)
      q2[j]<-sd(boot)
      q3[j]<-S
      q4[j]<-K
      q5[j]<-median(boot)
    }
    qsort1<-sort(q1,na.last = T)
    qsort2<-sort(q2,na.last = T)
    qsort3<-sort(q3,na.last = T)
    qsort4<-sort(q4,na.last = T)
    qsort5<-sort(q5,na.last = T)
    #two sided interval mean
    L1.B<-quantile(qsort1,alpha/2)
    U1.B<-quantile(qsort1,1-alpha/2)
    #two sided interval standard deviation
    L2.B<-quantile(qsort2,alpha/2)
    U2.B<-quantile(qsort2,1-alpha/2)
    #two sided interval skewness
    L3.B<-quantile(qsort3,alpha/2)
    U3.B<-quantile(qsort3,1-alpha/2)
    #two sided interval kurtosis
    L4.B<-quantile(qsort4,alpha/2)
    U4.B<-quantile(qsort4,1-alpha/2)
    #two sided interval median q-25 and q-75
    L5.B<-quantile(qsort5,alpha/2)
    U5.B<-quantile(qsort5,1-alpha/2)
    if ( 1>=L1.B  && 1 <= U1.B) {
      r.method.mean[i]<-1
    }
    if (1 >=L2.B  && 1 <= U2.B) {
      r.method.sd[i]<-1
    }
    if (1 >=L3.B  && 1 <= U3.B) {
      r.method.S[i]<-1
    }
    if (4 >=L4.B  && 4 <= U4.B) {
      r.method.K[i]<-1
    }
    if (1 >=L5.B  && 1 <= U5.B) {
      r.method.median[i]<-1
    }
  }
  ERF.S.mean<-1-round(mean(r.simple.mean),4)
  ERF.S.mean2<-1-round(mean(r.simple.mean2),4)
  ERF.B.mean<-1-round(mean(r.method.mean),4)
  ERF.S.SD<-1-round(mean(r.simple.sd),4)
  ERF.S.SD2<-1-round(mean(r.simple.sd2),4)
  ERF.B.SD<-1-round(mean(r.method.sd),4)
  ERF.S.S<-1-round(mean(r.simple.S),4)
  ERF.B.S<-1-round(mean(r.method.S),4)
  ERF.S.K<-1-round(mean(r.simple.K),4)
  ERF.B.K<-1-round(mean(r.method.K),4)
  ERF.S.median<-1-round(mean(r.simple.median),4)
  ERF.B.median<-1-round(mean(r.method.median),4)
  # results
  print("output: comparison empirical rejection Bootstrap and normality")
  print("")
  print(paste("the empirical rejection freq. for the mean using Bootstrap is:",ERF.B.mean))
  print(paste("the empirical rejection freq. for the mean using normality is:",ERF.S.mean))
  print(paste("the empirical rejection freq. for the mean using correct CI is:",ERF.S.mean2))
  print("")
  print(paste("the empirical rejection freq. for the standard deviation using Bootstrap is:",ERF.B.SD))
  print(paste("the empirical rejection freq. for the standard deviation using normality is:",ERF.S.SD))
  print(paste("the empirical rejection freq. for the standard deviation using correct CI is:",ERF.S.SD2))
  print("")
  print(paste("the empirical rejection freq. for the skewness using Bootstrap is:",ERF.B.S))
  print(paste("the empirical rejection freq. for the skewness using normality is:",ERF.S.S))
  print("")
  print(paste("the empirical rejection freq. for the kurtosis using Bootstrap is:",ERF.B.K))
  print(paste("the empirical rejection freq. for the kurtosis using normality is:",ERF.S.K))
  print("")
  print(paste("the empirical rejection freq. for the median using Bootstrap is:",ERF.S.median))
  print(paste("the empirical rejection freq. for the median using normality is:",ERF.B.median))
  print("")
}
Monte.carlo.sim.norm(B = 999, sim = 200, sample = 50, alpha = 0.05)
Monte.carlo.sim.pois(B = 999, sim = 200, sample = 50, alpha = 0.05)
# Analysis Treatment 1: duration scan and number of patients----------------------------------------------------

data4<-data3%>%filter(PatientType=="Type 1")
data4W1<-data4%>%filter(W1==1)
data4W2<-data4%>%filter(W2==1)
data4W3<-data4%>%filter(W3==1)
data4W4<-data4%>%filter(W4==1)
data4MO<-data4%>%filter(MO==1)
data4TU<-data4%>%filter(TU==1)
data4WE<-data4%>%filter(WE==1)
data4TH<-data4%>%filter(TH==1)
data4FR<-data4%>%filter(FR==1)
#non parametric duration scan
      #full sample
          Bootstrap.moments.NP(9999,data4$ScanTime,0.05)
      #weeks
          Bootstrap.moments.NP(9999,data4W1$ScanTime,0.05)
          Bootstrap.moments.NP(9999,data4W2$ScanTime,0.05)
          Bootstrap.moments.NP(9999,data4W3$ScanTime,0.05)
          Bootstrap.moments.NP(9999,data4W4$ScanTime,0.05)
      #days
          Bootstrap.moments.NP(9999,data4MO$ScanTime,0.05)
          Bootstrap.moments.NP(9999,data4TU$ScanTime,0.05)
          Bootstrap.moments.NP(9999,data4WE$ScanTime,0.05)
          Bootstrap.moments.NP(9999,data4TH$ScanTime,0.05)
          Bootstrap.moments.NP(9999,data4FR$ScanTime,0.05)
          
#parametric duration scan
      #full sample
          Bootstrap.moments.norm(9999,data4$ScanTime,0.05)
      #week
          Bootstrap.moments.norm(9999,data4W1$ScanTime,0.05)
          Bootstrap.moments.norm(9999,data4W2$ScanTime,0.05)
          Bootstrap.moments.norm(9999,data4W3$ScanTime,0.05)
          Bootstrap.moments.norm(9999,data4W4$ScanTime,0.05)
      #days
          Bootstrap.moments.norm(9999,data4MO$ScanTime,0.05)
          Bootstrap.moments.norm(9999,data4TU$ScanTime,0.05)
          Bootstrap.moments.norm(9999,data4WE$ScanTime,0.05)
          Bootstrap.moments.norm(9999,data4TH$ScanTime,0.05)
          Bootstrap.moments.norm(9999,data4FR$ScanTime,0.05)
#non parametric number of patients
          patients<-data5$type1Freq
          Bootstrap.moments.NP(9999,patients,0.05)
#parametric number of patients
          Bootstrap.moments.poiss(9999,patients,0.05)
          
# Analysis Treatment 2:duration and number of patients ----------------------------------------------------
data6<-data3%>%filter(PatientType=="Type 2")
data6W1<-data6%>%filter(W1==1)
data6W2<-data6%>%filter(W2==1)
data6W3<-data6%>%filter(W3==1)
data6W4<-data6%>%filter(W4==1)
data6MO<-data6%>%filter(MO==1)
data6TU<-data6%>%filter(TU==1)
data6WE<-data6%>%filter(WE==1)
data6TH<-data6%>%filter(TH==1)
data6FR<-data6%>%filter(FR==1)
#non parametric duration scan
      #full sample
            Bootstrap.moments.NP(9999,data6$ScanTime,0.05)
      #weeks
            Bootstrap.moments.NP(9999,data6W1$ScanTime,0.05)
            Bootstrap.moments.NP(9999,data6W2$ScanTime,0.05)
            Bootstrap.moments.NP(9999,data6W3$ScanTime,0.05)
            Bootstrap.moments.NP(9999,data6W4$ScanTime,0.05)
      #days
            Bootstrap.moments.NP(9999,data6MO$ScanTime,0.05)
            Bootstrap.moments.NP(9999,data6TU$ScanTime,0.05)
            Bootstrap.moments.NP(9999,data6WE$ScanTime,0.05)
            Bootstrap.moments.NP(9999,data6TH$ScanTime,0.05)
            Bootstrap.moments.NP(9999,data6FR$ScanTime,0.05)


#non parametric number of patients
      patients<-data5$type2Freq
      Bootstrap.moments.NP(9999,patients,0.05)
#parametric number of patients
      Bootstrap.moments.poiss(9999,patients,0.05)
      
# Analysis ratio: number of patients ----------------------------------------------------
Bootstrap.moments.NP(9999,data5$ratio,0.05)

# monte-carlo study non-parametric methods used treatment 2 -------------------------------------------

#under normality of data
      Monte.carlo.sim.norm(B = 999, sim = 200, sample = 50, alpha = 0.05)
#under poisson(1) of data
      Monte.carlo.sim.pois(B = 999, sim = 200, sample = 50, alpha = 0.05)
      