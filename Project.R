getwd()
setwd("D:\\R\\Project\\R Project")
install.packages("readxl")
library(readxl)
child=read_excel("childrenperwomen.xlsx", na="NA")
#child=read.csv("childrenperwomen.csv")
str(child)
summary(child)
head(child)
child[1,]
child[,1]
child=na.omit(child)
child=child[complete.cases(child),]
str(child)
dim(child)
install.packages("ggplot2")
library("ggplot2")
par()
par(cex.lab=1.5,cex.axis=1,  cex.main=2, mar=c(7,2,7,5))

#---------------------------------------------------------------------------------------------------------
#1) Grouping into consecutive years by their mean values. So reducing 217 columns into 23 columns.
women<-data.frame(child[,1])
#women<-child[,1]
women
i=1
x=2
y=11
for (i in child) {
  if (x==212){
    y=y-4
    women=cbind(women,rowMeans(child[,x:y],na.rm=TRUE))
    break
  }else{
    women=cbind(women,rowMeans(child[,x:y],na.rm=TRUE))
    y=y+10
    x=x+10
  }
}

dim(women)
colnames(women)
colnames(women[2])
colnames(women)<-c("Total.fertility.rate","1800-1809","1810-1819","1820-1829","1830-1839","1840-1849",
                   "1850-1859","1860-1869","1870-1879","1880-1889","1890-1899","1900-1909","1910-1919",
                   "1920-1929","1930-1939","1940-1949","1950-1959","1960-1969","1970-1979","1980-1989",
                   "1990-1999","2000-2009","2010-2015")
women<-cbind(women[,1], round(women[,2:23]))

#----------------------------------------------------------------------------------------------------------------------------
#2) Countries with more number of children per women in 1800 and their current values in 2015

Lowest_1800=women[order(women["1800-1809"]),]
Highest_1800=women[order(-women["1800-1809"]),]
Highest_1800[,1:2]
head(Highest_1800[,1], n=10)
tail(Highest_1800[,1], n=10)

pdf("changes.pdf")

k=head(Highest_1800[,c(1,2,23)])
matplot(t(k), type="b", pch=c(1,4), col=c("aquamarine","darkorchid1","violet","hotpink1","blue","red"), 
        ylab="Number of Children per Women", xaxt='n',yaxt='n', lwd=4, main="Comparison of Number of Children per Women
        in 1800 and 2015")
legend("bottomleft", legend=k[,1], lwd=3,lty=1:2,pch=c(1,4), cex=1.5, col=c("aquamarine","darkorchid1","violet","hotpink1","blue",
                                                                "violetred1","red"))
axis(side=1, at=2:3, labels=c("1800","2015"), cex.axis=1.5)
axis(side=2, cex.axis=1.5)
dev.off()


#--------------------------------------------------------------------------------------------------------------------------
#3) Countries with more number of children per women in 2015

Highest_2015=women[order(-women["2010-2015"]),]
Highest_2015[,c(1,23)]
head(Highest_2015[,1], n=10)
tail(Highest_2015[,1], n=10)
m=head(Highest_2015[,c(1,2,23)])
pdf("Changes1.pdf")
matplot(t(m), type="b", pch=c(1,4), col=c("aquamarine","darkorchid1","violet","hotpink1","blue","red"), 
        ylab="Number of Children per Women",xaxt='n',yaxt='n', lwd=4, main="Comparison of Number of Children per Women 
        in 1800 and 2015")
legend("bottomleft", legend=m[,1], lwd=3,lty=1:2,pch=c(1,4), cex=1.5, col=c("aquamarine","darkorchid1","violet","hotpink1","blue",
                                                                            "violetred1","red"))
axis(side=1, at=2:3, labels=c("1800","2015"), cex.axis=1.5)
axis(side=2, cex.axis=1.5)
dev.off()

#-----------------------------------------------------------------------------------------------------------------------------
#4) Barchart for countries against the number of kids/women in 2015

count<-table(women$`2010-2015`)
pdf("Barchart2015.pdf")
barplot(count, main="Countries with Number of Children per Women 
        in 2015", 
        names.arg = c(1:8), density=50, col=c("red","blue"), angle = c(45,90), 
        xlab = "Number of Children per Women", cex.axis=1.5, cex.names = 1.5, xlim=c(0,10), ylim=c(0,100))
dev.off()
#-----------------------------------------------------------------------------------------------------------------------------
#5) Barchart for countries against the number of kids/women in 1800

count1800<-table(women$`1800-1809`)
pdf("Barchart1800.pdf")
barplot(count1800, main="Countries with Number of Children per Women 
        in 1800", 
        names.arg = c(4:8), col=c("green","violet"), density = 50, angle = c(45,0,90,45,0),
        xlab = "Number of Children per Women", cex.axis=1.5, cex.names = 1.5, xlim=c(0,10), ylim=c(0,100))
dev.off()

#-----------------------------------------------------------------------------------------------------------------------------
#6) Monitored the changes in number of children per women for each country through the period 1800-2015

#apply(women[2:23],1,range)
a=apply(women[2:23],1,max)
b=apply(women[2:23],1,min)

p=women[which((a-b)==1),1]
q=women[which((a-b)==2),1]
r=women[which((a-b)==3),1]
s=women[which((a-b)==4),1]
t=women[which((a-b)==5),1]
u=women[which((a-b)==6),1]
pdf("piechart.pdf")
slices <- c(length(p),length(q),length(r),length(s),length(t),length(u))
labls <- c("1", "2", "3", "4", "5","6")
percentage <- round(slices/sum(slices)*100)
labls <- paste(labls, percentage, sep = " - ")
labls <- paste(labls,"%",sep="") 
pie(slices,labels = labls, col=rainbow(length(labls)), main="Decreasing Factor")
dev.off()

#-----------------------------------------------------------------------------------------------------------------------
#7) Countries with drastical change in number of children per women
install.packages("plotrix")
library(plotrix)
pdf("pie3D.pdf")
percentage <- round(slices/sum(slices)*100)
labs <- paste(percentage, "%" ,sep = "")

pie3D(slices,labels=labs,explode=0.15,main="Countries with Reduction in Number of Children per Women 
      through the Period 1800-2015", radius=0.8, 
      col = c("aquamarine","darkorchid1","chartreuse2","hotpink2","plum1","violetred1"),
      theta = 1)
legend(x=-0.9,y=1, ncol = 3, legend=labls, cex=1, fill=c("aquamarine","darkorchid1","chartreuse2","hotpink2","plum1","violetred1"))

dev.off()
u=women[which((a-b)==6),1]
u
z=women[u,1:23]
pdf("linechart.pdf")
matplot(t(z), type="b", pch=c(1,4), col=c("aquamarine","darkorchid1","violet","hotpink1","blue","violetred1","red"), 
        ylab="Number of Children per women",lwd=2, xaxt='n', main="Countries with Drastic Change in the Number
        of Children per Women from 1800-2015")
legend("bottomleft", legend=u, lty=1:2,pch=c(1,4), cex=1, col=c("aquamarine","darkorchid1","violet","hotpink1","blue",
                                                       "violetred1","red"))
axis(side=1, at=2:23, labels=colnames(women[2:23]), las=2, cex=0.9)
dev.off()
warnings()

demo("colors")

#-----------------------------------------------------------------------------------------------------------------------------
#8) Proved that the number of children per women changed for the all the countries through this period: 1800-2015
options(max.print=1000)
View(women)
women
i=1
for (i in women) {
  a=all(women[i, 2:23] == women[i, 2])
  if (a==1){
    View(women[i,])
  } else {
    print("No")
  }
  
}

#-----------------------------------------------------------------------------------------------------------------------------
#9) Get the data for a country

f=readline(prompt="Enter Country Name:")
women[which(f==women[,1]),c(1,23)]


#-----------------------------------------------------------------------------------------------------------------------------
#10) Getting input through a pop-up window

install.packages("svDialogs")
library(svDialogs)

user <- dlgInput("Enter the Country Name:", Sys.info()["user"])$res
women[which(user==women[,1]),c(1,23)]

rm(list = ls())
