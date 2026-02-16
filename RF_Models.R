###############################################################
# RANDOM FOREST MODELS FOR RIVER FLOW INTERMITTENCY
# Binary (Stage 1) and Multiclass (Stage 2) Classification
# Author: Axel Belemtougri
# Description:
# This script trains Random Forest models to classify river
# flow intermittency using repeated data partitioning (n = 100),
# cross-validation, and performance evaluation.
###############################################################


###############################################################
# Required Packages
###############################################################
library(caret)
library(randomForest)
library(ggplot2)
library(ggpubr)
library(pdp)
library(doParallel)
library(parallel)
library(iml) 
library(foreach)
library(dplyr)
library("sf")
library(scales)
library(ggside)
library(gridExtra)


# Ouverture espace données
setwd("D:/Données/Données_interm_AF/traitement stations/")
Sys.setenv(TZ='GMT')


Dataset <- read.csv("D:/Données/Données_interm_AF/traitement stations/Data/Gauges_stations_summary.csv",
                    header = TRUE, sep=";",dec = ",",
                    stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8"
)


Dataset$Classe_bin<- factor(Dataset$Classe_bin, levels = c("Perennial","Non_perennial"))
Dataset_bin<-Dataset[,c(13:23,11)]

##################### ------------------- Modèle binaire - BC  --------------------#################################

# Detecter le nombre de coeur
cores=detectCores()
cl <- makeCluster(cores[1]-4) #not to overload your computer
registerDoParallel(cl)
# Minuterie
start.time<-proc.time()


set.seed(104) 
training.samples <- createDataPartition(Dataset_bin$Classe_bin ,p = 0.8, list = FALSE, times=100)

Train.recap<-list()
Test.recap<-list()
gc()

tuneGrid <- expand.grid(.mtry = c(1:11))
ctrl <- trainControl(method ="cv",number=10,verboseIter = TRUE,savePredictions=T,summaryFunction = multiClassSummary)


 for (i in 1:100){

train.data <- Dataset_bin[training.samples[,i],]
test.data <- Dataset_bin[-training.samples[,i],]

##### BC-WOR ######
model<- train(Classe_bin ~., data=train.data, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Balanced_Accuracy",
              trControl=ctrl,allowParallel = TRUE, importance = TRUE)

# ##### BC-UNS ######
# model<- train(Classe_bin ~., data=train.data, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Balanced_Accuracy",strata = data2$Classe,sampsize = rep(min(table(data2$Classe)),2),
#               trControl=ctrl,allowParallel = TRUE, importance = TRUE)
# ###################
# 
# 
# ##### BC-OVS ######
# train.data <- upSample(x =  train.data[,1:11],
#                        y =  train.data$Classe_bin,
#                        ## keep the class variable name the same:
#                        yname = "Classe_bin")
# 
# model<- train(Classe_bin ~., data=train.data, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Balanced_Accuracy",
#               trControl=ctrl,allowParallel = TRUE, importance = TRUE)
# ###################

## Best tuning parameter
mtry<-model$bestTune
result<-model$results
res <- result[result$mtry==mtry$mtry,]
m<-paste0("model",i)
res<-cbind(res,m)

Train.recap <- rbind(Train.recap,res)

# Make predictions on the test data
test.data$predict <- model %>% predict(test.data)
# Compute the average prediction error RMSE
se <-confusionMatrix(data=test.data$predict ,reference =test.data$Classe_bin)
BAccuracy <- se[["byClass"]][["Balanced Accuracy"]]
Kappa<-se[["overall"]][["Kappa"]]


res1<-data.frame(cbind(BAccuracy,Kappa))
m1<-paste0("model",i)
res1<-cbind(res1,m1)
Test.recap<-rbind(Test.recap,res1)

}  
gc()
stop.time<-proc.time()
run.time<-stop.time-start.time
run.time/60
on.exit(stopCluster(cl))

# Export des résultats
# write.table(x=Train.recap,"D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_bin1.csv", row.names = FALSE,sep = ";", dec=',')
# write.table(x=Test.recap,"D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_bin1.csv", row.names = FALSE,sep = ";", dec=',')

### Lecture des fiches de performances des modéles 

model1.train<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_bin1.csv", header = TRUE, sep = ";", dec = ",")
model2.train<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_bin2.csv", header = TRUE, sep = ";", dec = ",")
model3.train<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_bin3.csv", header = TRUE, sep = ";", dec = ",")

M1<- data.frame(cbind(mean(model1.train$Balanced_Accuracy),mean(model1.train$Kappa),mean(model2.train$Balanced_Accuracy),mean(model2.train$Kappa),mean(model3.train$Balanced_Accuracy),mean(model3.train$Kappa)))
colnames(M1)<-c("M1_Balanced_Accuracy","M1_Kappa","M2_Balanced_Accuracy","M2_Kappa","M3_Balanced_Accuracy","M3_Kappa")
M1$type<-"Train"

sd(model3.test$BAccuracy)
sd(model3.train$Kappa)

model1.test<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_bin1.csv", header = TRUE, sep = ";", dec = ",")
model2.test<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_bin2.csv", header = TRUE, sep = ";", dec = ",")
model3.test<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_bin3.csv", header = TRUE, sep = ";", dec = ",")

M2<- data.frame(cbind(mean(model1.test$BAccuracy),mean(model1.test$Kappa),mean(model2.test$BAccuracy),mean(model2.test$Kappa),mean(model3.test$BAccuracy),mean(model3.test$Kappa)))
colnames(M2)<-c("M1_Balanced_Accuracy","M1_Kappa","M2_Balanced_Accuracy","M2_Kappa","M3_Balanced_Accuracy","M3_Kappa")
M2$type<-"Test"


#---------------------- Graphique comparatif des performances en test -------------------------------------

model1.test$model<-"Model 1"
model2.test$model<-"Model 2"
model3.test$model<-"Model 3"

model<-rbind(model1.test,model2.test,model3.test)

model$model<- factor(model$model, levels =  c("Model 1","Model 2","Model 3"))

# mtry_train_test <-cbind(model1.train[,1],model1.test)
# names(mtry_train_test)[names(mtry_train_test) == "model1.train[, 1]"] <- 'mtry'
# 
# graph <- group_by(mtry_train_test,mtry) %>%
#   summarize(Cost=mean(Cost), Kappa=mean(Kappa),n=n())

# Graphique du Balanced accuracy

d1<-ggplot(model, aes(x=model,y=BAccuracy,na.rm=TRUE))+geom_violin(trim=FALSE, fill='#f0f0f0', color="#525252")+geom_boxplot(aes(color=model), fill="#f7f7f7",lwd=1,fatten=1.5)+ labs(x="", y = "Stage 1 \n Balanced_Accuracy (-)") + stat_summary(fun= mean,geom="point",color = "black",fill= "black",shape=22, size=3) +
  labs(color ="")+guides(color=guide_legend(ncol=2))+
  theme_bw()+theme_bw()+guides(color=guide_legend(ncol =3))+theme(legend.position="none",legend.title = element_text(face="bold", color="black",size=15),legend.text = element_text(face="bold", color="black",size=15), axis.title.x = element_text(color="black", size=15, face="bold"),axis.title.y = element_text(color="black", size=20, face="bold"), axis.text.x = element_text(face="bold", color="black",size=16),
                                                                  axis.text.y = element_text(face="bold", color="black",size=15))  

d1


d2<-ggplot(model, aes(x=model,y=Kappa,na.rm=TRUE))+geom_violin(trim=FALSE, fill='#f0f0f0', color="#525252")+geom_boxplot(aes(color=model), fill="#f7f7f7",lwd=1,fatten=1.5)+ labs(x="", y = "Kappa (-)") + stat_summary(fun= mean,geom="point",color = "black",fill= "black",shape=22, size=3) +
  labs(color ="")+guides(color=guide_legend(ncol=2))+
  theme_bw()+theme_bw()+guides(color=guide_legend(ncol =3))+theme(legend.position="none",legend.title = element_text(face="bold", color="black",size=15),legend.text = element_text(face="bold", color="black",size=15), axis.title.x = element_text(color="black", size=15, face="bold"),axis.title.y = element_text(color="black", size=20, face="bold"), axis.text.x = element_text(face="bold", color="black",size=16),
                                                                  axis.text.y = element_text(face="bold", color="black",size=15))  

d2

library(ggpubr)
library("gridExtra")

A <- ggarrange(d1,d2,labels = c("a","b"), font.label = list(size = 20, color = "#404040", face = "bold", family = NULL),ncol=2) 

A  

ggsave("D:/Cartes et graphes/Interm_Afrique/eval_performance.png",A, width =12, height =7, dpi = 800)



##################### ------------------- Modèle Multiclasse - MC  --------------------#################################

Dataset_multi<-Dataset[Dataset$Ndry_round>1, c(12:23)]
Dataset_multi$Classe_multi<- factor(Dataset_multi$Classe_multi)
Dataset_multi$Classe_multi<- factor(Dataset_multi$Classe_multi, levels = c("Weakly_intermittent","Highly_intermittent","Ephemeral"))

set.seed(104)
# Detecter le nombre de coeur
cores=detectCores()
cl <- makeCluster(cores[1]-3) #not to overload your computer
registerDoParallel(cl)
# Minuterie
start.time<-proc.time()

set.seed(104)
training.samples <- createDataPartition(Dataset_multi$Classe_multi ,p = 0.8, list = FALSE, times=100)

Train.recap<-list()
Test.recap<-list()
gc()

tuneGrid <- expand.grid(.mtry = c(1:11))
ctrl <- trainControl(method ="cv",number=10,verboseIter = TRUE,savePredictions=T,summaryFunction = multiClassSummary)

for (i in 1:100){

train.data <- Dataset_multi[training.samples[,i],]
test.data <- Dataset_multi[-training.samples[,i],]


##### MC-WOR ######
model<- train(Classe_multi ~., data=train.data, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Balanced_Accuracy",
              trControl=ctrl,allowParallel = TRUE, importance = TRUE)

# ##### MC-UNS ######
# model<- train(Classe_multi ~., data=train.data, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Balanced_Accuracy",strata = data2$Classe,sampsize = rep(min(table(data2$Classe)),2),
#               trControl=ctrl,allowParallel = TRUE, importance = TRUE)
# ###################
# 
# 
# ##### MC-OVS ######
# train.data <- upSample(x =  train.data[,1:11],
#                        y =  train.data$Classe_bin,
#                        ## keep the class variable name the same:
#                        yname = "Classe_multi")
# 
# model<- train(Classe_multi ~., data=train.data, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Balanced_Accuracy",
#               trControl=ctrl,allowParallel = TRUE, importance = TRUE)
# ###################


# strata = train.data$Classe,sampsize = rep(min(table(train.data$Classe)),3),
## Best tuning parameter
mtry<-model$bestTune
result<-model$results
res <- result[result$mtry==mtry$mtry,]
m<-paste0("model",i)
res<-cbind(res,m)

Train.recap <- rbind(Train.recap,res)

# Make predictions on the test data
test.data$predict <- model %>% predict(test.data)
# Compute the average prediction error RMSE
se <-confusionMatrix(data=test.data$predict ,reference =test.data$Classe)
Perform<- data.frame(se[["byClass"]])
MBAccuracy<- mean(Perform$Balanced.Accuracy)
Kappa<-se[["overall"]][["Kappa"]]

res1<-data.frame(cbind(MBAccuracy,Kappa))
m1<-paste0("model",i)
res1<-cbind(res1,m1)

Test.recap<-rbind(Test.recap,res1)

}  

gc()
stop.time<-proc.time()
run.time<-stop.time-start.time
run.time/60
# stopCluster(cl)
on.exit(stopCluster(cl))

# Export des résultats
write.table(x=Train.recap,"D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_multi1.csv", row.names = FALSE,sep = ";", dec=',')
write.table(x=Test.recap,"D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_multi1.csv", row.names = FALSE,sep = ";", dec=',')


#------------------- Lecture des fiches de performances des modèles 

model1.train_bis<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_multi1.csv", header = TRUE, sep = ";", dec = ",")
model2.train_bis<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_multi2.csv", header = TRUE, sep = ";", dec = ",")
model3.train_bis<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Train.recap_class_multi3.csv", header = TRUE, sep = ";", dec = ",")

M1bis<- data.frame(cbind(mean(model1.train_bis$Mean_Balanced_Accuracy),mean(model1.train_bis$Kappa),mean(model2.train_bis$Mean_Balanced_Accuracy),mean(model2.train_bis$Kappa),mean(model3.train_bis$Mean_Balanced_Accuracy),mean(model3.train_bis$Kappa)))
colnames(M1bis)<-c("M1_MBalanced_Accuracy","M1_Kappa","M2_MBalanced_Accuracy","M2_Kappa","M3_MBalanced_Accuracy","M3_Kappa")
M1$type<-"Train"

sd(model3.test_bis$MBAccuracy)
sd(model3.test_bis$Kappa)

model1.test_bis<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_multi1.csv", header = TRUE, sep = ";", dec = ",")
model2.test_bis<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_multi2.csv", header = TRUE, sep = ";", dec = ",")
model3.test_bis<-read.table("D:/Données/Données_interm_AF/traitement stations/Periode58_98bis/Test.recap_class_multi3.csv", header = TRUE, sep = ";", dec = ",")

M2bis<- data.frame(cbind(mean(model1.test_bis$MBAccuracy),mean(model1.test_bis$Kappa),mean(model2.test_bis$MBAccuracy),mean(model2.test_bis$Kappa),mean(model3.test_bis$MBAccuracy),mean(model3.test_bis$Kappa)))
colnames(M2bis)<-c("M1_MBalanced_Accuracy","M1_Kappa","M2_MBalanced_Accuracy","M2_Kappa","M3_MBalanced_Accuracy","M3_Kappa")
M2$type<-"Test"


model1.test_bis$model<-"Model_bis 1"
model2.test_bis$model<-"Model_bis 2"
model3.test_bis$model<-"Model_bis 3"

model_bis<-rbind(model1.test_bis,model2.test_bis,model3.test_bis)

model_bis$model<- factor(model_bis$model, levels =  c("Model_bis 1","Model_bis 2","Model_bis 3"))


# Graphique du Balanced accuracy

d1_bis<-ggplot(model_bis, aes(x=model,y=MBAccuracy,na.rm=TRUE))+geom_violin(trim=FALSE, fill='#f0f0f0', color="#525252")+geom_boxplot(aes(color=model), fill="#f7f7f7",lwd=1,fatten=1.5)+ labs(x="", y = "Stage 2 \n Balanced_Accuracy (-)") + stat_summary(fun= mean,geom="point",color = "black",fill= "black",shape=22, size=3) +
  labs(color ="")+guides(color=guide_legend(ncol=2))+
  theme_bw()+theme_bw()+guides(color=guide_legend(ncol =3))+theme(legend.position="none",legend.title = element_text(face="bold", color="black",size=15),legend.text = element_text(face="bold", color="black",size=15), axis.title.x = element_text(color="black", size=15, face="bold"),axis.title.y = element_text(color="black", size=20, face="bold"), axis.text.x = element_text(face="bold", color="black",size=16),
                                                                  axis.text.y = element_text(face="bold", color="black",size=15))  

d1_bis


d2_bis<-ggplot(model_bis, aes(x=model,y=Kappa,na.rm=TRUE))+geom_violin(trim=FALSE, fill='#f0f0f0', color="#525252")+geom_boxplot(aes(color=model), fill="#f7f7f7",lwd=1,fatten=1.5)+ labs(x="", y = "Kappa (-)") + stat_summary(fun= mean,geom="point",color = "black",fill= "black",shape=22, size=3) +
  labs(color ="")+guides(color=guide_legend(ncol=2))+
  theme_bw()+theme_bw()+guides(color=guide_legend(ncol =3))+theme(legend.position="none",legend.title = element_text(face="bold", color="black",size=15),legend.text = element_text(face="bold", color="black",size=15), axis.title.x = element_text(color="black", size=15, face="bold"),axis.title.y = element_text(color="black", size=20, face="bold"), axis.text.x = element_text(face="bold", color="black",size=16),
                                                                  axis.text.y = element_text(face="bold", color="black",size=15))  

d2_bis


library(ggpubr)
library("gridExtra")

A1 <- ggarrange(d1, d2, d1_bis,d2_bis,labels = c("a","b","c","d"), font.label = list(size = 20, color = "#404040", face = "bold", family = NULL),ncol=2,nrow=2) 

A1  

ggsave("D:/Cartes et graphes/Interm_Afrique/eval_performance1.png",A1, width =13, height =13, dpi = 800)




