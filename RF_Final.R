###############################################################
# River Flow Intermittency Modeling in Africa
# Author: Axel Belemtougri
# Description:
# This script develops two Random Forest models:
# 1) Binary classification model (BC-UNS)
# 2) Multiclass classification model (MC-WOR)
# It also generates variable importance plots,
# partial dependence plots, and spatial predictions.
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

################## ------------------- Modèle binaire BC-UNS ------------------- ##################
set.seed(115) 

tuneGrid <- expand.grid(.mtry = c(5))
ctrl <- trainControl(method ="cv",number=10,verboseIter = TRUE,savePredictions=T,summaryFunction = multiClassSummary)

## BC-UNS ##
model<- train(Classe_bin ~., data=Dataset_bin, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Balanced_Accuracy",strata = Dataset_bin$Classe_bin,sampsize = rep(min(table(Dataset_bin$Classe_bin)),2),
              trControl=ctrl,allowParallel = TRUE, importance = TRUE)

View(model[["results"]])

# Matrice de Confusion
cfm1<-confusionMatrix(model)
data_cfm1<-as.data.frame(cfm1$table)
data_cfm1$number<-(data_cfm1$Freq/100)*1269
names(data_cfm1)[names(data_cfm1) == 'Reference'] <- "Observed_classes"
names(data_cfm1)[names(data_cfm1) == 'Prediction'] <- "Predicted_classes"


data_cfm1$Predicted_classes <- factor(data_cfm1$Predicted_classes, levels = c("Perennial","Non_perennial"))
data_cfm1$Observed_classes <- factor(data_cfm1$Observed_classes , levels = c("Non_perennial","Perennial"))
data_cfm1$Freq1<- c(91,9,29,71)
data_cfm1$Sens<- c("(0.92)"," "," ","(0.71)")


p <- ggplot(data = data_cfm1 ,aes(x = Predicted_classes , y = Observed_classes)) +theme_classic()+ ggtitle("Type 1 model - BC-UNS: BACC = 0.81 ; Kappa = 0.65") +
  geom_tile(aes(fill = Freq1),alpha = 0.6,color = "black",lwd =0.85,linetype = 2) + xlab("Predicted_classes \n") +
  scale_fill_gradient(low = 'White', high = "green") +guides(fill = guide_colourbar(barwidth = 2,barheight = 10, title = "Fraction \nObserved \n      (%)"))+
  geom_text(aes(x = Predicted_classes, y = Observed_classes, label = number), size= 26) +
  geom_text(aes(label = Sens),vjust = 5, nudge_y = 0.5, size= 18) +
  scale_x_discrete(expand=c(0,0),position = "top")+
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.background=element_blank(), legend.title = element_text(face="bold", color="black",size=18),legend.text = element_text(face="bold", color="black",size=24),
        plot.title = element_text(color="#99000d", size=27, face="bold.italic",hjust = 0.5),panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        axis.title.x = element_text(color="black", size=28, face="bold"),axis.ticks=element_line(size=1.5),axis.ticks.length = unit(0.2, "cm"),legend.spacing.y = unit(1.0, 'cm'),
        axis.title.y = element_text(color="black", size=28, face="bold"),axis.text.x = element_text(face="bold", color="black",size=26, angle =0,hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face="bold", color="black",size=26))


p

# --------------- Ordre d'importance des variables dans le modèle Random Forest POUR BC-UNS ----------------------------

VI_F=randomForest::importance(model$finalModel)

imp <- as.data.frame(VI_F)
imp <-imp[,c(3,4)]

imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
colnames(imp)<-c("Mean_Decrease_Accuracy","Mean_Decrease_Gini","varnames")
imp$varnames<-as.factor(imp$varnames)
imp<-imp[order(-imp$Mean_Decrease_Gini),]

imp$cor<- "Climat"
imp$cor[1]<- "Climate"
imp$cor[2]<- "Topography"
imp$cor[3]<- "Climate"
imp$cor[4]<- "Geology and soil properties"
imp$cor[5]<- "Topography"
imp$cor[6]<- "Climate"
imp$cor[7]<- "Topography"
imp$cor[8]<- "Topography"
imp$cor[9]<-"Geology and soil properties"
imp$cor[10]<- "Anthropogenic influence"
imp$cor[11]<- "Anthropogenic influence"

imp$varnames<- factor(imp$varnames, levels = c("Damstor","HFP","Grwstor","Tci","Slope","Tmoy","Elv","Perm","PET","Area","Arid"))
imp$cor<- factor(imp$cor, levels = c("Climate","Topography","Geology and soil properties","Anthropogenic influence"))


# Graphe des variables par importance 
gra <- ggplot(imp, aes(x=varnames, y= Mean_Decrease_Gini,fill=as.factor(cor))) + theme_classic()+
  geom_col(color="black", size=1)+ylab("Mean_Decrease_Gini")+scale_fill_manual(name = "Categories",values = c("#74add1","#bf812d","#abdda4","#de77ae","#f46d43"))+
  xlab("Explanatory environmental variables") + coord_flip()+ guides(fill=guide_legend(ncol=2))+
  scale_y_continuous(breaks=seq(0,100,20),expand = c(0,0.5), limits = c(0,100))+
  theme(legend.position="top",legend.title = element_text(face="bold", color="black",size=16),legend.text = element_text(face="bold", color="black",size=16),
        axis.line = element_line(color = "black", size = 1, linetype = "solid"),plot.title = element_text(color="black", size=18, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=19, face="bold"),axis.ticks=element_line(size=1.5),axis.ticks.length = unit(0.2, "cm"),
        axis.title.y = element_text(color="black", size=19, face="bold"))+ 
  theme(axis.text.x = element_text(face="bold", color="black",size=16),axis.text.y = element_text(face="bold", color="black", size=16)) 



gra

round((sum(imp$Mean_Decrease_Gini[1:4])/sum(imp$Mean_Decrease_Gini))*100,1)


# ----------------- Graphe de dépendance partielle de BC-UNS ----------------------
# Packages
library(pdp)
library(parallel)

# Nombre de coeurs (laisser 1 coeur libre)
ncore <- detectCores() - 1
cl <- makeCluster(ncore)

# Export des objets nécessaires vers les workers
clusterExport(cl, c("model", "var", "class"))
clusterEvalQ(cl, library(pdp))

# Fonction à paralléliser
compute_partial <- function(j){
  
  Tab_recap <- list()
  
  for (i in 1:length(class)){
    
    partial_plot_data <- pdp::partial(
      model,
      pred.var = var[j],
      plot = FALSE,                 # IMPORTANT: pas de plot en parallèle
      type = "classification",
      grid.resolution = 200,
      which.class = class[i],
      prob = TRUE
    )
    
    vf <- partial_plot_data
    colnames(vf) <- c("value", "yhat")
    vf$classe <- class[i]
    vf$Var <- var[j]
    
    Tab_recap <- rbind(Tab_recap, vf)
  }
  
  return(Tab_recap)
}

# Exécution parallèle sur les variables
Tab_list <- parLapply(cl, 1:length(var), compute_partial)

# Stop cluster
stopCluster(cl)

# Combinaison finale
Tab <- do.call(rbind, Tab_list)

Tab$classe <- factor(Tab$classe, levels =  c("Perennial","Non_perennial"))

Tab$Var<-gsub("Arid","Arid(-)",Tab$Var)
Tab$Var<-gsub("Area","Area(km^2)",Tab$Var)
Tab$Var<-gsub("HFP","HFP(-)",Tab$Var)
Tab$Var<-gsub("PET","PET(mm)",Tab$Var)
Tab$Var<-gsub("Tmoy","Tmoy(°C)",Tab$Var)
Tab$Var<-gsub("Silt","Silt(%)",Tab$Var)
Tab$Var<-gsub("Slope","Slope(°)",Tab$Var)
Tab$Var<-gsub("Elv","Elv(m)",Tab$Var)
Tab$Var<-gsub("Tci","Tci_ln(m)",Tab$Var)
Tab$Var<-gsub("Grwstor","Grwstor(mm)",Tab$Var)
Tab$Var<-gsub("Perm","log10[Perm(m^2)]x100",Tab$Var)
Tab$Var<-gsub("Damstor","Damstor(Mm^3)",Tab$Var)

Tab$Var<- factor(Tab$Var, levels =  c("Arid(-)","Area(km^2)","PET(mm)","log10[Perm(m^2)]x100","Elv(m)","Tmoy(°C)","Slope(°)","Tci_ln(m)","Grwstor(mm)","HFP(-)","Damstor(Mm^3)"))
Tab$value<- round(Tab$value,3)
tr<-split(Tab,Tab$Var)
# tr<-Tab_recap
library(scales) 

plot_list = list()

for (i in 1:11) {
  
  temp <- tr[[i]]
  temp[temp == 0] <- NA
  temp <- temp[complete.cases(temp), ]
  
  Code <- paste0("graphe", i)
  
  p <- ggplot(temp, aes(x = value, y = yhat, color = classe)) +
    theme_bw() +
    geom_line(size = 1.5) +
    xlab(unique(as.character(temp$Var))) +
    ylab("Probability of prediction") +
    geom_rug(data = temp, aes(x = value),
             color = "#878787", inherit.aes = FALSE) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
    guides(color = guide_legend(ncol = 2,
                                override.aes = list(size = 4))) +
    scale_color_manual(name = "Classes",
                       values = c('#3300CC','#FF6600')) +
    expand_limits(x = 0, y = 0) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 16),
      legend.text = element_text(face = "bold", size = 17),
      plot.title = element_text(size = 18,
                                face = "bold.italic", hjust = 0.5),
      axis.title.x = element_text(size = 18, face = "bold"),
      axis.ticks = element_line(size = 1.5),
      axis.ticks.length = unit(0.2, "cm"),
      panel.border = element_rect(colour = "black",
                                  fill = NA, size = 1.5),
      axis.title.y = element_text(size = 17, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 16)
    )
  
  if (i %in% c(2, 11)) {
    p <- p + scale_x_log10(
      breaks = trans_breaks("log10", function(x) 10^x)
    )
  } else {
    p <- p + scale_x_continuous(
      limits = c(min(temp$value), max(temp$value))
    )
  }
  
  plot_list[[Code]] <- p
}

# Exemple : afficher un graphe
plot_list[["graphe6"]]

A1<- ggarrange(plot_list[["graphe1"]],plot_list[["graphe2"]],plot_list[["graphe3"]],plot_list[["graphe4"]],labels = c("a","b","c","d"), font.label = list(size = 18, color = "black", face = "bold", family = NULL),common.legend = TRUE,ncol = 2,nrow=2)

A1  

A1 <- annotate_figure(
  A1,
  top = text_grob("Type 1 model (BC-UNS)", 
                  color = "white", face = "bold", size = 8)
)


A1 <- annotate_figure(
  A1,
  top = text_grob("Type 1 model (BC-UNS)", 
                  color = "#a50f15", face = "bold", size = 20)
)

A1

A <- ggarrange(plot_list[["graphe5"]],plot_list[["graphe6"]],plot_list[["graphe7"]],plot_list[["graphe8"]],plot_list[["graphe9"]],plot_list[["graphe10"]],plot_list[["graphe11"]],labels = c("a","b","c","d","e","f","g"), font.label = list(size = 18, color = "black", face = "bold", family = NULL),common.legend = TRUE,ncol = 2,nrow=4)

A  

ggsave("D:/Cartes et graphes/Interm_Afrique/Pdp.png",A1, width = 11, height = 9, dpi = 800)

# -------------- Prédiction du modèle binaire (BC-UNS) sur le réseau LCS ----------------------
# Chemin vers la geodatabase
gdb_path <- "D:/Arcgis/Data_interm_AFR/LCS_Flow_Intermittency_2025_v0.gdb"

# Voir les couches disponibles dans la GDB
st_layers(gdb_path)

data_sf <- st_read(dsn = gdb_path, layer = "LCS_line")
data_sf <-sf::st_drop_geometry(data_sf)
gc()

data_sf$Binary_Class<- model %>% predict(data_sf)

sum(data_sf$Length)

proportion_bin <- group_by(data_sf,Binary_Class) %>%
  summarize(long=round((sum(Length)/20680964)*100),0)

proportion_bin







########################### ------------------- Modèle Multiclasse- MC-WOR ------------------- ###########################
Dataset_multi<-Dataset[Dataset$Ndry_round>1, c(12:23)]
Dataset_multi$Classe_multi<- factor(Dataset_multi$Classe_multi)
Dataset_multi$Classe_multi<- factor(Dataset_multi$Classe_multi, levels = c("Weakly_intermittent","Highly_intermittent","Ephemeral"))

set.seed(104)

tuneGrid <- expand.grid(.mtry = c(1))
ctrl <- trainControl(method ="cv",number=10,verboseIter = TRUE,savePredictions=T,summaryFunction = multiClassSummary)

model1<- train(Classe_multi ~., data= Dataset_multi, method ="rf", ntree = 800,tuneGrid=tuneGrid,metric = "Mean_Balanced_Accuracy",
              trControl=ctrl,allowParallel = TRUE, importance = TRUE)

View(model1[["results"]])

# ----------------------- Matrice de confusion POUR MC-WOR -----------------
cfm<-confusionMatrix(model1)
data_cfm<-as.data.frame(cfm$table)
data_cfm$number<-(data_cfm$Freq/100)*380
names(data_cfm)[names(data_cfm) == 'Reference'] <- "Observed_classes"
names(data_cfm)[names(data_cfm) == 'Prediction'] <- "Predicted_classes"

data_cfm$Predicted_classes <- factor(data_cfm$Predicted_classes, levels = c("Weakly_intermittent","Highly_intermittent","Ephemeral"))
data_cfm$Observed_classes <- factor(data_cfm$Observed_classes , levels = c("Ephemeral","Highly_intermittent","Weakly_intermittent"))

data_cfm$Freq1<- c(73,27,0,45,45,10,9,32,59)
data_cfm$Sens<- c("(0.73 / 0.65)"," "," "," ","(0.45 / 0.71)", " ", " ", " ","(0.58 / 0.95)")


p1 <- ggplot(data = data_cfm ,aes(x = Predicted_classes , y = Observed_classes)) +theme_classic()+ ggtitle("Type 2 model - MC-WOR: BACC = 0.68 ; Kappa = 0.35") +
  geom_tile(aes(fill = Freq1),alpha = 0.6,color = "black",lwd =0.85,linetype = 2) + xlab("Predicted_classes \n") +
  scale_fill_gradient(low = 'White', high = "green") +guides(fill = guide_colourbar(barwidth = 2,barheight = 10, title = "Fraction \nObserved \n      (%)"))+
  geom_text(aes(x = Predicted_classes, y = Observed_classes, label = number), size= 20) +
  geom_text(aes(label = Sens),vjust = 7, nudge_y = 0.5, size= 12) +
  scale_x_discrete(expand=c(0,0),position = "top")+
  scale_y_discrete(expand=c(0,0)) +
  theme(legend.background=element_blank(), legend.title = element_text(face="bold", color="black",size=18),legend.text = element_text(face="bold", color="black",size=24),
        plot.title = element_text(color="#99000d", size=38, face="bold.italic",hjust = 0.5),panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        axis.title.x = element_text(color="black", size=28, face="bold"),axis.ticks=element_line(size=1.5),axis.ticks.length = unit(0.2, "cm"),legend.spacing.y = unit(1.0, 'cm'),
        axis.title.y = element_text(color="black", size=28, face="bold"),axis.text.x = element_text(face="bold", color="black",size=26, angle =0,hjust=0.5,vjust=0.5),
        axis.text.y = element_text(face="bold", color="black",size=26))



p1


library(ggpubr)
library("gridExtra")

A2 <- ggarrange(
  p,
  NULL,
  p1,
  labels = c("a", "", "b"),
  font.label = list(size = 40, color = "#404040", face = "bold"),
  ncol = 1,
  heights = c(1, 0.08, 1)
)

A2

ggsave("D:/Cartes et graphes/Interm_Afrique/Cfm2.png",A2, width =18, height =22, dpi = 800)

# ----------------------- Importance des variables pour MC-WOR -----------------

VI_F1=randomForest::importance(model1$finalModel)

imp1 <- as.data.frame(VI_F1)
imp1 <-imp1[,c(3,4)]

imp1$varnames <- rownames(imp1) # row names to column
rownames(imp1) <- NULL  
# imp1$var_categ <- rep(1:2, 6) # random var category
colnames(imp1)<-c("Mean_Decrease_Accuracy","Mean_Decrease_Gini","varnames")
imp1$varnames<-as.factor(imp1$varnames)
imp1<-imp1[order(-imp1$Mean_Decrease_Gini),]

imp1$cor<- "Climat"
imp1$cor[1]<- "Climate"
imp1$cor[2]<- "Climate"
imp1$cor[3]<- "Anthropogenic influence"
imp1$cor[4]<- "Topography"
imp1$cor[5]<-"Climate"
imp1$cor[6]<- "Anthropogenic influence"
imp1$cor[7]<- "Topography"
imp1$cor[8]<- "Topography"
imp1$cor[9]<-"Topography"
imp1$cor[10]<- "Geology and soil properties"
imp1$cor[11]<- "Geology and soil properties"

imp1$varnames<- factor(imp1$varnames, levels = c("Perm","Grwstor","Elv","Slope","Area","Damstor","Tmoy","Tci","HFP","PET","Arid"))
imp1$cor<- factor(imp1$cor, levels = c("Climate","Topography","Geology and soil properties","Anthropogenic influence"))


# Graphe des variables par imp1ortance 
gra1 <- ggplot(imp1, aes(x=varnames, y= Mean_Decrease_Gini,fill=as.factor(cor))) + theme_classic()+
  geom_col(color="black", size=1)+ylab("Mean_Decrease_Gini")+scale_fill_manual(name = "Categories",values = c("#74add1","#bf812d","#abdda4","#de77ae","#f46d43"))+
  xlab("Explanatory environmental variables") + coord_flip()+ guides(fill=guide_legend(ncol=2))+
  scale_y_continuous(breaks=seq(0,30,5),expand = c(0,0.2), limits = c(0,30))+
  theme(legend.position="top",legend.title = element_text(face="bold", color="black",size=16),legend.text = element_text(face="bold", color="black",size=16),
        axis.line = element_line(color = "black", size = 1, linetype = "solid"),plot.title = element_text(color="black", size=18, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="black", size=19, face="bold"),axis.ticks=element_line(size=1.5),axis.ticks.length = unit(0.2, "cm"),
        axis.title.y = element_text(color="black", size=19, face="bold"))+ 
  theme(axis.text.x = element_text(face="bold", color="black",size=16),axis.text.y = element_text(face="bold", color="black", size=16)) 



gra1

library(ggpubr)
library("gridExtra")

A3 <- ggarrange(gra,gra1, labels = c("a","b"), font.label = list(size = 20, color = "#404040", face = "bold", family = NULL),nrow=2,common.legend = TRUE, legend="top") 

A3  


ggsave("D:/Cartes et graphes/Interm_Afrique/Importance.png",A3, width =13, height =13, dpi = 800)

round((sum(imp1$Mean_Decrease_Gini[1:4])/sum(imp1$Mean_Decrease_Gini))*100,1)


# ----------------- Graphe de dépendance partielle multi-modèle MC-WOR----------------------

Tab_recap<- list()
Tab<-list()


# Packages
library(pdp)
library(doParallel)
library(foreach)

# Nombre de coeurs (laisser 1 libre)
ncore <- parallel::detectCores() - 1
cl <- makeCluster(ncore)
registerDoParallel(cl)

# Variables
var   <- as.character(imp1$varnames)
class <- c("Weakly_intermittent",
           "Highly_intermittent",
           "Ephemeral")

# Boucle parallèle sur j
Tab <- foreach(j = 1:length(var),
               .combine = rbind,
               .packages = "pdp") %dopar% {
                 
                 Tab_recap <- NULL
                 
                 for (i in 1:length(class)) {
                   
                   partial_plot_data <- pdp::partial(
                     model1,
                     pred.var = var[j],
                     plot = FALSE,   # IMPORTANT en parallèle
                     type = "classification",
                     grid.resolution = 200,
                     which.class = class[i],
                     prob = TRUE
                   )
                   
                   vf <- partial_plot_data
                   colnames(vf) <- c("value", "yhat")
                   vf$classe <- class[i]
                   vf$Var    <- var[j]
                   
                   Tab_recap <- rbind(Tab_recap, vf)
                 }
                 
                 Tab_recap
               }

# Stop cluster
stopCluster(cl)

library(ggplot2)
library(scales)
library(dplyr)
# Facteurs
Tab$classe <- factor(Tab$classe,
                     levels = c("Weakly_intermittent",
                                "Highly_intermittent",
                                "Ephemeral"))

# Renommage variables (plus propre avec recodage vectorisé)
Tab$Var <- recode(Tab$Var,
                  "Arid"    = "Arid(-)",
                  "PET"     = "PET(mm)",
                  "HFP"     = "HFP(-)",
                  "Tci"     = "Tci_ln(m)",
                  "Tmoy"    = "Tmoy(°C)",
                  "Damstor" = "Damstor(Mm^3)",
                  "Area"    = "Area(km^2)",
                  "Slope"   = "Slope(°)",
                  "Elv"     = "Elv(m)",
                  "Grwstor" = "Grwstor(mm)",
                  "Perm"    = "log10[Perm(m^2)]x100"
)

Tab$Var <- factor(Tab$Var,
                  levels = c("Arid(-)",
                             "PET(mm)",
                             "HFP(-)",
                             "Tci_ln(m)",
                             "Tmoy(°C)",
                             "Damstor(Mm^3)",
                             "Area(km^2)",
                             "Slope(°)",
                             "Elv(m)",
                             "Grwstor(mm)",
                             "log10[Perm(m^2)]x100"))

Tab$value <- round(Tab$value, 3)

# Split
tr <- split(Tab, Tab$Var)

# Nombre réel de graphes
nplot <- length(tr)

plot_list <- list()

for (i in 1:nplot) {
  
  temp <- tr[[i]]
  temp[temp == 0] <- NA
  temp <- temp[complete.cases(temp), ]
  
  if (nrow(temp) == 0) next
  
  Code <- paste0("graphe", i)
  
  p <- ggplot(temp, aes(x = value, y = yhat, color = classe)) +
    theme_bw() +
    geom_line(size = 1.5) +
    xlab(unique(as.character(temp$Var))) +
    ylab("Probability of prediction") +
    geom_rug(aes(x = value),
             color = "#878787",
             inherit.aes = FALSE) +
    scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75)) +
    guides(color = guide_legend(ncol = 2,
                                override.aes = list(size = 4))) +
    scale_color_manual(name = "Classes",
                       values = c('#009900','#FFCC33','#FF6600')) +
    expand_limits(x = 0, y = 0) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 20),
      legend.text  = element_text(face = "bold", size = 26),
      axis.title.x = element_text(face = "bold", size = 18),
      axis.title.y = element_text(face = "bold", size = 17),
      axis.text.x  = element_text(face = "bold", size = 16),
      axis.text.y  = element_text(face = "bold", size = 16),
      panel.border = element_rect(colour = "black",
                                  fill = NA, size = 1.5)
    )
  
  # condition log uniquement pour i = 2 et 11
  if (i %in% c(2,6)) {
    p <- p + scale_x_log10(
      breaks = trans_breaks("log10", function(x) 10^x)
    )
  } else {
    p <- p + scale_x_continuous(
      limits = c(min(temp$value), max(temp$value))
    )
  }
  
  plot_list[[Code]] <- p
}

plot_list[["graphe11"]]



A<- ggarrange(plot_list[["graphe1"]],plot_list[["graphe2"]],plot_list[["graphe3"]],plot_list[["graphe4"]],plot_list[["graphe5"]],plot_list[["graphe6"]],plot_list[["graphe7"]],plot_list[["graphe8"]],plot_list[["graphe9"]],plot_list[["graphe10"]],plot_list[["graphe11"]],labels = c("a","b","c","d","e","f","g","h","i","j","k"), font.label = list(size = 20, color = "black", face = "bold", family = NULL),common.legend = TRUE,ncol = 2,nrow=6)

A  

A <- annotate_figure(
  A,
  top = text_grob("Type 2 model (MC-WOR)", 
                  color = "white", face = "bold", size = 32)
)


A <- annotate_figure(
  A,
  top = text_grob("Type 2 model (MC-WOR)", 
                  color = "#a50f15", face = "bold", size = 32)
)


# Exporter la figure
ggsave(filename = "D:/Cartes et graphes/Interm_Afrique/pdp_multiclasse.png",   # ou .pdf, .jpeg, etc.
       plot = A,
       width =21, height = 27, dpi = 700)

# ----------------------- Prédiction combinaison de BC-UNS et MC-WOR-----------------

data_sf$Multi_Class <- model1 %>% predict(data_sf)
gc()

LCS_PR<-data_sf[data_sf$Binary_Class=="Perennial",]
LCS_PR$Multi_Class<-"Perennial"
LCS_NPR<-data_sf[data_sf$Binary_Clas=="Non_perennial",]

LCS_flow_interm<-rbind(LCS_PR,LCS_NPR)
sum(LCS_flow_interm$Length)

#  Proportion de l'intermittence pour le Réseau  LCS 
proportion_multi <- group_by(LCS_flow_interm,Multi_Class) %>%
  summarize(long=round((sum(Length)/20680964)*100),0)

proportion_multi 

#  Proportion de l'intermittence pour le Réseau  LCS drainant plus de 100 km2 
LCS_flow_interm_100<- data.frame(LCS_flow_interm[LCS_flow_interm$Area>=100,])

sum(LCS_flow_interm_100$Length)

proportion1 <- group_by(LCS_flow_interm_100,Multi_Class) %>%
  summarize(long=round((sum(Length)/2641065)*100),0)

proportion1





