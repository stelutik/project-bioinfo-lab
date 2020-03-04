directory<-"C:/Users/stela/OneDrive/Desktop/Medical_Bioinfo/progetto/Notorieta_11_2019"
setwd(dir = directory)
not_18<-read.csv(file = "notorieta_2018_05_11.csv", header = T, sep = ";")
not_19<-read.csv("notorieta_2019_11_15.csv", header = T, sep=";")
not_18_unique<-unique(not_18[,c(2,7)])#select the variables ai_name and soc_name_en
not_19_unique<-unique(not_19[,c(2,7)])

## Read all the files 

#Chi3
dyn<- read.delim(paste(directory,"/tuples/Chi3/tuples_dynamic.txt", sep=""),header=T,sep='\t')
static<- read.delim(paste(directory,"/tuples/Chi3/tuples_static.txt",sep=""),header=T,sep='\t')
monthly<- read.delim(paste(directory,"/tuples/Chi3/tuples_static_month.txt",sep=""),header=T,sep='\t')
quarterly<- read.delim(paste(directory,"/tuples/Chi3/tuples_static_quarterly.txt",sep=""),header=T,sep='\t')

#Chi5
dyn<- read.delim(paste(directory,"/tuples/Chi5/tuples_dynamic.txt", sep=""),header=T,sep='\t')
static<- read.delim(paste(directory,"/tuples/Chi5/tuples_static.txt",sep=""),header=T,sep='\t')
monthly<- read.delim(paste(directory,"/tuples/Chi5/tuples_static_month.txt",sep=""),header=T,sep='\t')
quarterly<- read.delim(paste(directory,"/tuples/Chi5/tuples_static_quarterly.txt",sep=""),header=T,sep='\t')

#CI3
dyn<- read.delim(paste(directory,"/tuples/CI3/tuples_dynamic.txt", sep=""),header=T,sep='\t')
static<- read.delim(paste(directory,"/tuples/CI3/tuples_static.txt",sep=""),header=T,sep='\t')
monthly<- read.delim(paste(directory,"/tuples/CI3/tuples_static_month.txt",sep=""),header=T,sep='\t')
quarterly<- read.delim(paste(directory,"/tuples/CI3/tuples_static_quarterly.txt",sep=""),header=T,sep='\t')


#CI5
dyn<- read.delim(paste(directory,"/tuples/CI5/tuples_dynamic.txt", sep=""),header=T,sep='\t')
static<- read.delim(paste(directory,"/tuples/CI5/tuples_static.txt",sep=""),header=T,sep='\t')
monthly<- read.delim(paste(directory,"/tuples/CI5/tuples_static_month.txt",sep=""),header=T,sep='\t')
quarterly<- read.delim(paste(directory,"/tuples/CI5/tuples_static_quarterly.txt",sep=""),header=T,sep='\t')

dyn<-unique(dyn[,1:2]) #select the first variables drug and soc
static<-unique(static[,1:2]) #select the first variables drug and soc
monthly<-unique(monthly[,1:2]) #select the first variables drug and soc
quarterly<-unique(quarterly[,1:2]) #select the first variables drug and soc


#merging the dyn with not_18
m1_dyn<-merge(dyn,not_18_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))
m2_static<-merge(static,not_18_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))
m3_monthly<-merge(monthly,not_18_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))
m4_quaterly<-merge(quarterly,not_18_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))


#merging the dyn with not_19
m1_dyn<-merge(dyn,not_19_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))
m2_static<-merge(static,not_19_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))
m3_monthly<-merge(monthly,not_19_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))
m4_quaterly<-merge(quarterly,not_19_unique,by.x=c("drug","soc"), by.y = c("ai_name","soc_name_en"))


## Generation of Venn Diagrams
#Comparisons Tedar vs PRR statico mensile, 
#            Tedar vs PRR statico trimestrale, 
#            Tedar vs PRR statico annuale

install.packages('VennDiagram')
library(VennDiagram)

m1_dyn_obs<-nrow(m1_dyn)
m2_static_obs<-nrow(m2_static)
dyn_stat_merged<-merge(m1_dyn,m2_static, by=c("drug","soc"))
intersect<-nrow(dyn_stat_merged)

grid.newpage()
#Tedar vs PRR statico annuale
plot<- draw.pairwise.venn(m1_dyn_obs,m2_static_obs,intersect, c("TEDAR","PRR Annuale"), scaled=FALSE,
        lty = rep("blank",2), fill = c("light blue", "pink"), alpha = rep(0.5, 2), 
        cat.pos = c(-10,10), cat.dist = rep(0.025, 2));

#Tedar vs PRR statico mensile
m3_monthly_obs<-nrow(m3_monthly)
dyn_month_merged<-merge(m1_dyn,m3_monthly, by = c("drug","soc"))
intersect<-nrow(dyn_month_merged)
grid.newpage()
plot<- draw.pairwise.venn(m1_dyn_obs,m3_monthly_obs,intersect, c("TEDAR","PRR Mensile"),inverted = FALSE, scaled=FALSE,
                          lty = rep("blank",2), fill = c("light blue", "pink"), alpha = rep(0.5, 2), 
                          cat.pos = c(-10,10), cat.dist = rep(0.025, 2));

#Tedar vs PRR statico trimestrale
m4_quaterly_obs<-nrow(m4_quaterly)
dyn_quart_merged<-merge(m1_dyn,m4_quaterly, by=c("drug","soc"))
intersect<-nrow(dyn_quart_merged)
grid.newpage()
plot<- draw.pairwise.venn(m1_dyn_obs,m4_quaterly_obs,intersect, c("TEDAR","PRR Trimestrale"), scaled=FALSE,
                          lty = rep("blank",2), fill = c("light blue", "pink"), alpha = rep(0.5, 2), 
                          cat.pos = c(-10,10), cat.dist = rep(0.025, 2));

## sensitivity and precision for TEDAR

library(dplyr)

#notorietà 18
m1_dyn<-merge(dyn,not_18_unique,by=c("drug","soc"))
colnames(not_18_unique)<-c("drug","soc")
tp1<-nrow(m1_dyn) #true positive
tp1
fn1<-nrow(anti_join(not_18_unique, dyn, by=c("drug","soc"))) #false negative
fn1
fp1<-nrow(anti_join(dyn,not_18_unique,by=c("drug","soc"))) #false positive
fp1

sensitivity= tp1/(tp1+fn1)
sensitivity
precision=tp1/(tp1+fp1)
precision

#notorietà 19
m1_dyn<-merge(dyn,not_19_unique,by = c("drug","soc"))
colnames(not_19_unique)<-c("drug","soc")
tp1<-nrow(m1_dyn) #true positive
tp1
fn1<-nrow(anti_join(not_19_unique, dyn, by=c("drug","soc"))) #false negative
fn1
fp1<-nrow(anti_join(dyn,not_19_unique,by=c("drug","soc"))) #false positive
fp1

sensitivity= tp1/(tp1+fn1)
sensitivity
precision=tp1/(tp1+fp1)
precision
