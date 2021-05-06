library(randomForest)
library(ggplot2)
library(dplyr)
library(tree)
library(cvTools)

setwd("/Users/mrunalsawant/Desktop/Project/project 2/")

ld_test=read.csv("store_test.csv",stringsAsFactors = F)

ld_train= read.csv("store_train.csv",stringsAsFactors = F)
ld_test$store=NA

ld_test$data='test'
ld_train$data='train'

ld_all=rbind(ld_test,ld_train)
glimpse(ld_all)
summary(ld_all)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

#ld_all=CreateDummies(ld_all ,"state_alpha",100)
#ld_all=CreateDummies(ld_all ,"country",100)
#ld_all=CreateDummies(ld_all ,"store_Type",100)

ld_all=ld_all %>% 
  select(-storecode,-CouSub,-country,
         -State,-Id)

#lapply(ld_all,function(x) length(unique(x)))
#sum(is.na(ld_all$CouSub))

#sort(table(ld_all$state_alpha,ld_all$CouSub),decreasing = T) [1:10]

#tapply(ld_all$CouSub,ld_all$state_alpha, mean)

#lapply(ld_all,function(x) sum(is.na(x)))
names(ld_all)[sapply(ld_all,function(x) is.character(x))]

cat_cols=c("countyname","countytownname",
           "state_alpha","store_Type","Areaname")

for(cat in cat_cols){
  ld_all=CreateDummies(ld_all,cat,50)
}
sum(sapply(ld_all,function(x) is.character(x)))

z=unique(ld_all$countyname)
for (i in z) {
  ld_all[ld_all$countyname==i ,"countyname"]= round(mean(ld_all[ld_all$countyname==i,"CouSub"]))
  
}



for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","store"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[ld_all$data=='test',col],na.rm=T)
  }
  
}

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-store)

set.seed(2)
s=sample(1:nrow(ld_train),0.8*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]

library(car)

for_vif=lm(store~.,data=ld_train1)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(store~.-sales1
           -sales2-sales3
           -sales4,
           data=ld_train1)
sort(vif(for_vif),decreasing = T)[1:3]

log_fit=glm(store~.-sales1
            -sales2-sales3
            -sales4,
            data=ld_train1,family = "binomial")
log_fit=step(log_fit)
formula(log_fit)

log_fit=glm (store ~ sales0 + sales1 + sales2 + sales3 + sales4 + population + 
                        countyname_WorcesterCounty + countyname_PenobscotCounty + 
                        countyname_MiddlesexCounty + countyname_AroostookCounty + 
                        countyname_FranklinCounty + countyname_WashingtonCounty + 
                        state_alpha_WV + state_alpha_MT + state_alpha_CA + state_alpha_NY + 
                        state_alpha_CO + state_alpha_LA + state_alpha_SD + state_alpha_AL + 
                        state_alpha_FL + state_alpha_PA + state_alpha_WI + state_alpha_AR + 
                        state_alpha_OK + state_alpha_PR + state_alpha_MS + state_alpha_MI + 
                        state_alpha_MN + state_alpha_OH + state_alpha_IN + state_alpha_NE + 
                        state_alpha_TN + state_alpha_IA + state_alpha_NC + state_alpha_IL + 
                        state_alpha_KS + state_alpha_MO + state_alpha_KY + state_alpha_VA + 
                        state_alpha_GA + state_alpha_CT + state_alpha_TX + state_alpha_VT + 
                        state_alpha_NH + state_alpha_MA + state_alpha_ME + store_Type_SupermarketType3 + 
                        store_Type_GroceryStore + store_Type_SupermarketType1 + `Areaname_PenobscotCounty,ME(part)HUDMetroFMRArea` + 
                        `Areaname_AroostookCounty,ME` + `Areaname_Boston_Cambridge_Quincy,MA_NHHUDMetroFMRArea`,
  data=ld_train,family='binomial')
summary(log_fit)

library(pROC)

val.score=predict(log_fit,newdata = ld_train2,type='response')

auc(roc(ld_train2$store,val.score))

for_vif=lm(store~.-sales1
              -sales2-sales3
              -sales4,
              data=ld_train1)
sort(vif(for_vif),decreasing = T)[1:3]
           
log.fit.final=glm(store ~ sales0 + countyname_WorcesterCounty + countyname_PenobscotCounty + 
                    `Areaname_Boston_Cambridge_Quincy,MA_NHHUDMetroFMRArea` + 
                    state_alpha_WV + state_alpha_CO + state_alpha_LA + state_alpha_AL + 
                    state_alpha_WI + state_alpha_AR + state_alpha_OK + state_alpha_PR + 
                    state_alpha_MS + state_alpha_MI + state_alpha_OH + state_alpha_IN + 
                    state_alpha_TN + state_alpha_IL + state_alpha_MO + state_alpha_KY + 
                    state_alpha_GA + state_alpha_CT + state_alpha_TX + state_alpha_VT + 
                    state_alpha_NH + state_alpha_MA + state_alpha_ME,
                  data=ld_train,family='binomial')

log.fit.final=step(log.fit.final)

summary(log.fit.final)

formula(log.fit.final)

test.prob.score= predict(log_fit,newdata = ld_test,type='response')

write.csv(test.prob.score,"mrunal_sawant_p2_part2.csv",row.names = F)

train.score=predict(log.fit.final,newdata = ld_train,type='response')

real=ld_train$store
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=M))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)

ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

















































