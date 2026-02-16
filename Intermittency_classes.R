# Ouverture espace données
setwd("D:/Données/Données_interm_AF/traitement stations/")
Sys.setenv(TZ='GMT')

# Lecture des données de débits filtrés sur la période 1958-1998
Data<-read.table("D:/Données/Données_interm_AF/traitement stations/Data/Q_Month_1259gauges_filter.csv", header = TRUE, sep = ";", dec = ",")

#  Décompte du nombre de mois à  débit nul par an pour chaque station
Q1<-group_by(Data,Code,year)%>%
  summarize(number=sum(mean_Q==0))  

#  Moyenne long terme du nombre de mois à débit nul par an pour chaque station
Q2<-group_by(Q1,Code)%>%
  summarize(Ndry_mean= mean(number))

Q2$Ndry_round<-round(Q2$Ndry_mean,0)

# Classification binaire de l'intermittence 
Data1<- Q2
Data1$Classe_bin<- Data1$Ndry_round
Data1$Classe_bin[Data1$Classe_bin>1] <- "Non_perennial"
Data1$Classe_bin[Data1$Classe_bin<=1] <- "Perennial"
Data1$Classe_bin <- factor(Data1$Classe_bin, levels = c("Perennial","Non_perennial"))

# Multi_Classification  de l'intermittence 
Data1$Classe_multi<- Data1$Ndry_round
Data1$Classe_multi[Data1$Classe_multi>7] <- "Ephemeral"
Data1$Classe_multi[Data1$Classe_multi>4 & Data1$Classe_multi<=7] <- "Highly_intermittent"
Data1$Classe_multi[Data1$Classe_multi>1 & Data1$Classe_multi<=4] <- "Weakly_intermittent"
Data1$Classe_multi[Data1$Classe_multi<=1] <- "Perennial"
Data1$Classe_multi <- factor(Data1$Classe_multi, levels = c("Perennial","Weakly_intermittent", "Highly_intermittent","Ephemeral"))

# Décompte du nombre de station par classe d'intermittence 
table(Data1$Classe_bin)
table(Data1$Classe_multi)

