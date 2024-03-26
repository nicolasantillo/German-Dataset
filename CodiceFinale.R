# librerie 
library(ggplot2)
library(plotly)
library(NbClust)
library(factoextra)
library(FactoMineR)
library(ContaminatedMixt)
library(mclust)
library(vscc)
library(teigen)
library(clustvarsel)
library(grid)
library(gridExtra)
library(gridtext)
library(SelvarMix)
library(FactoMineR)
library(FactoInvestigate)
library(Factoshiny)
library(factoextra)
library(data.table)
library(DT)
library(lattice)
library(tidyverse)

german_dataMod = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
colnames(german_dataMod)=c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")

#Tipologia di dati del dataset e summary
summary(german_dataMod)
str(german_dataMod)
Investigate(german_dataMod)

#ANALISI DESCRITTIVA

#UNIVARIATA
#Analizzo le variabili continue 
boxplot(german_dataMod$duration, col = "green", xlab="Duration")
boxplot(german_dataMod$amount, col = "red", xlab="Amount")
boxplot(german_dataMod$installment_rate, col = "purple", xlab="Installment Rate")
boxplot(german_dataMod$present_resid, col = "yellow", xlab="Present resid")
boxplot(german_dataMod$age, col = "orange", xlab="Age")
#quantile(german_dataMod$age,seq(0,1,0.01))
#german_dataMod$age[which(german_dataMod$age > 67)] <- 67

boxplot(german_dataMod$n_credits, col = "white", xlab="Num Credits")
boxplot(german_dataMod$n_people, col = "lightblue", xlab="Num People")


#analizzo variabili età(age), durata(duration), importo(amount) tramite ggplot 
#in relazione ai parametri 1 e 2 di response. 1= GOOD/ROSSO 2= BAD /BLU

#good vs bad
ggplot(german_dataMod, aes(x = response)) + geom_bar(fill = 'blue') + labs(title = 'Quantità Good/Bad nel Dataset ', x = 'classification', y = 'Count')


#Chi richiede un prestito, solitamente ha un età compresa tra i 20 e 40 anni. 
#Importo del prestito molto basso, tra i 0 e i 5000euro. 
#La durata di prestiti in, per la maggior parte dei casi, arriva fino ai 30 mesi. 2 anni e 6 mesi

par(mfrow = c(1,3))
# Frequency distribution of the credit amount
brk <- 30

hist(german_dataMod$age, breaks = brk , main = "Age", xlab = "Age", col = "darkmagenta", panel.first = grid())

hist(german_dataMod$duration, breaks = brk , main = "Duration", xlab = "Months", col = "darkgreen", panel.first = grid())


hist(german_dataMod$amount, breaks = brk , main = "Credit amount", xlab = "Amount", col = "skyblue", panel.first = grid())




#Relazione tra sesso e credit amount 
#log1p per normalizzare il mio grafico al centro
#in questo grafico a densità possiamo dire che : 
#gli uomini sposati o vedovi sono quelli che chiedono prestiti più bassi in assoluto
#idem le donne sposate o single. 
#invece le donne sepatate/divorziate sono molto di meno. Anche se possiamo notare un piccolissimo rialzo
#tra i 5000 e 10000, magari per spese legate al divorzio o alla cura della famiglia. 
#per  gli uomini single anxhe la richiesta è tra i 0 e i 5000 euro.

ggplot(german_data, aes(x=log1p(amount), fill=sex)) +
  geom_density(alpha=0.3) +
  ggtitle('Relazione tra Amount e Personal Status ') +
  xlab("Amount") + 
  ylab("Density") 

#Proporzione tra good/bad loan e sex
#in questo caso vediamo gli esiti delle richieste di prestito: 
#in genere, compando bad e good tra i vari stati personali, è più probabile che
#venga accettato il prestito che il contrario. L'unica differenza MENO sostanziale è
#per i maschi sepati o divorziati.
#nel dataset possiamo vedere come le donne single, siano in maggioranza, e che è molto
#più probabile che abbiano avuto paree positivo che negativo.
ggplot(german_data,aes(x=response, fill=sex)) +
  geom_bar(position='dodge') + 
  ggtitle("Proporzione tra good/bad loan e sex ")+ 
  
  theme_bw()+
  theme(axis.text = element_text(size=12,face = "bold"),
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
#amount vs sex vs età
ggplot(german_data, aes(x = age, y = amount, fill = response)) + geom_bar(stat = "identity") + facet_wrap(~ sex) 

#AGE VS CREDIT AMOUNT
#la maggior parte dei prestiti vengono richiesti per comprarsi una nuova auto, per arredamento, televisione o radio, o per business
#principalmente giovani.
#Relazione tra età è prestito: Oiù è bassa età , più è alto l'importo del prestito. 
#Anche chi acquista auto usate ha una valutazione negativa. 
xyplot(amount ~ age|purpose, german_dataMod, grid = TRUE, group = response,auto.key = list(points = FALSE, rectangles = TRUE, space = "right"),main="Scopo del Credito ,Age vs Amount ")


#Relazione tra età e situazione sentimentale
#A91:maschi divorziati/separati -> a 30 gli individui del dataset hanno avuto sperienze matrimoniali
#A92 : femmine divorziate separate o sposate -> idem maschi, ma in maggior numero. Difficilmente si lasciano dopo i 40
#A93: maschi single: tra i 30-40 anni, poi tendono a diminuire
#A94: maschi sposati/vedovi: picco tra i 20-30. Penso più sposati. Vedovi più verso destra pari a 0, dataset con età max 74 
histogram(amount ~ age | sex, data = german_dataMod, xlab = "Age",main="Confronto Età e Stato Personale")




#Analisi univariata, in relazione alla variabile response
#A32: crediti esistenti rimborsati -> nel nostro dataset la maggior parte delle volte sono stati accettati
credit_history <- ggplot(german_dataMod,aes(x=credit_his,fill =factor(response)))+geom_bar()+
  scale_x_discrete(limits=c("A30" , "A31","A32","A33","A34"))+guides(fill=FALSE)

#plot  purpose
#A43: come gi
#purpose <-  ggplot(german_dataMod,aes(x=purpose,fill=factor(response)))+geom_bar( )+
  #scale_x_discrete(limits=c("A43","A40","A42","A41","A49","A46","A45",
  #                          "A44","A410","A48"))+guides(fill=FALSE)

#plot credit amount

#il prestito viene accettato specialmente per valori molto bassi. 
#Tra 0 e 5000 la probabilità che il prestito venga accettato è molto alta.
#Anche se con poche osservazioni, più la richiesta aumenta più la risposta è negativa. 
credit_amount <-  ggplot(german_dataMod,aes(x= amount,
                                         fill=factor(response)))+geom_histogram(binwidth = 500)+guides(fill=FALSE)

#plot Installment_rate
#Rate per restituire il credito 
#Per la maggior parte di osservazioni le rate sono tra 1 e 4. Senza alcuna partiocolarità,
#ossia tutte le 'tranche' vengono accettate nella maggior parte dei casi.  

Installment_rate <- ggplot(german_dataMod,aes(x=installment_rate,
                                           fill=factor(response)))+geom_bar()+
  guides(fill=guide_legend(reverse=TRUE))+scale_fill_discrete(labels=c("Good","Bad"))+
  labs(fill='Loan status')+scale_x_discrete(limits=c("4","2","3","1"))

grid.arrange(credit_history, credit_amount,Installment_rate, ncol=3,
             top = "Analisi Univariata")



#A121: immobiliare  -> 
propery <-  ggplot(german_dataMod,aes(x=property,fill=factor(response)))+geom_bar( )+
  scale_x_discrete(limits=c("A123","A121","A122","A124"))+guides(fill=FALSE)

#plot housing      
#A152:  La casa è di proprietà. I prestiti, hanno avuto una buona risposta in relazione alle osservazioni ottenute. Solamente un terzo sono state ritenute bad
#A153 : Alloggio gratuito, in questo caso le risposte potrebbero dipendere da altri fattori. In quanto non c'è una netta differenza
#A152: casa in affitto. rispetto a A153 c'è qualche osservazione in più ritenuta good, ma anche qui altri fattori potrebbero essere essenziali per avere una buona proposta

housing <-  ggplot(german_dataMod,aes(x=housing,fill=factor(response)))+geom_bar( )+
  scale_x_discrete(limits=c("A152","A151","A153"))+guides(fill=FALSE)

#plot job
#LAVORI: 
#A173: impiegato skillato/ funzionario
#A172: Non skillato residente :
#A174: Impiegato di alto livello/ufficiale/manager:
job <-  ggplot(german_dataMod,aes(x=job,fill=factor(response)))+geom_bar( )+
  scale_x_discrete(limits=c("A173","A172","A174","A171"))+guides(fill=FALSE)+scale_fill_discrete(labels=c("Good","Bad"))



grid.arrange(housing,job, propery ,ncol=3,
             top = "")



#CONTI CORRENTI :  
#A11: se hai meno di 0 DM -> le osservazioni del nostro dataset ci dicono che la richiesta 
#può essere valutata sia good che bad. Pertanto la risposta definitiva può essere data in base ad altri fattori
#a partire da chi ha i 0 e 200 in poi, la risposta è nella maggior parte dei casi positiva
loan_checking_account <- ggplot(german_dataMod,aes(x=chk_acct,fill=factor(response)))+geom_bar( )+
  scale_x_discrete(limits=c("A14","A11","A12","A13"))+guides(fill=FALSE)

#BUONI RISPARMI: 
#A&1: <100DM come buoni. In questo caso moloto spesso le richieste vengono accolte, ma ci sono anche casi (un terzo)
#in cui la richiesta è stata rifiutata. 
#Per il resto dei casi la risposta è stata per lo più positiva, andando avanti con l'aumentare dei bonds
loan_savings_bonds <- ggplot(german_dataMod,aes(x= saving_acct,fill=factor(response)))+geom_bar( )+
  scale_x_discrete(limits=c("A61","A65","A62","A63","A64"))+guides(fill=FALSE)

#DURATA DEL PRESTITO:  
#il prestito , in mesi, viene accettato se è tra gli 0 e 20 mesi. Andando avanti ci sono sempre più 
#meno richieste ma anche meno accettate. Magari iil soggetto che richiede una durata così lunga
#non ha i requisiti giusti per la banca (prorpietà/conto corrente ì/ecc)
loan_duration <- ggplot(german_dataMod,aes(x=duration,fill=factor(response)))+geom_histogram(binwidth = 5)+
  guides(fill=FALSE)


#CREDITI VERSO ALTRI DEBITORI
#A101: NON CI SONO -> Facile che venga approvato. 
#A102, co-richiedente, e A103 (garante). Anche se le osservazioni sono poche vengono spesso accettate quelle con 
#garante, mentre con co-richiedente la probabilità è più bassa.

loan_debtors <- ggplot(german_dataMod, aes(x=other_debtor,fill=factor(response)))+geom_bar( )+
  guides(fill=guide_legend(reverse=TRUE))+scale_x_discrete(limits=c("A101","A103","A102"))+
  labs(fill='Loan status')+scale_fill_discrete(labels=c("Bad","Good"))


grid.arrange(loan_checking_account,loan_debtors,loan_duration,
             loan_savings_bonds,ncol=2,top = "")



#MODELLI DI REGRESSIONE 


# più significative installment_rate , saving_acctA65 , purposeA43  , purposeA41  , chk_acctA14 
fit1<-glm(as.factor(response) ~ . ,family=binomial(),data=german_dataMod)
#AIC: 993.82
summary(fit1)


#Null factor
fit2<-glm(as.factor(response) ~ 1. ,family=binomial(),data=german_dataMod)
#AIC: 1223.7
summary(fit2)





#SELEZIONI DELLE VARIABILI
#dataset per modelli

german_mod <- german_data[,c(2,5,8,11,13,16,18)]

#vscc

x.data <- german_mod
#variabili selezionate:  n_people , n_credits, amount ,age ,present_resid ,installment_rate
clust.vscc <- vscc(x.data, G=1:9, automate = "mclust", initial = NULL, train = NULL, forcereduction = FALSE)
head(clust.vscc$topselected)

# clustvarsel 

clust.f <- clustvarsel(x.data,G=1:5) #  variabili selezionate: n_credits, installment_rate ,duration,amount, age 
clust.f$subset
summary(clust.f)
table(clust.f$model$classification,german_mod[, 1]) #NOIMPO
table(clust.f$model$classification,german_mod[, 2])#NOIMPO


#variabii selezionate: duration , amount , installment_rate , age ,n_credits 
clust.b <- clustvarsel(x.data,G=1:5,direction = "backward") 

clust.b$subset
table(clust.f$model$classification,german_mod[, 1])
table(clust.f$model$classification,german_mod[, 2])


#creo 3 dataset con le variabili fornite da clustvarsel e vscc, e confronto gli indici


german_2fb <- german_mod[,c(1,2,3,5,6 )]
#adjustedRandindex: 0.0766
german.teigen <- teigen(german_2fb, G=1:9, models = "all", verbose = TRUE)
adjustedRandIndex(german_2fb[,1],german.teigen$iclresults$classification) 

german_vscc <- german_mod[, c(2,3,4,5,6,7)]
#adjustedRandindex: 0.02544
german.teigen <- teigen(german_mod, G=1:9, models = "all", verbose = TRUE)
adjustedRandIndex(german_mod[,1],german.teigen$iclresults$classification) 


# lasso, NON Funziona

lambda <- seq(0.1, 100, length = 0.5)
rho <- seq(1, 2, length=0.5)
lasso <- SelvarClustLasso(x= x.data , nbcluster=1:3,criterio="ICL")

#Dopo aver confrontato gli indici restringo il modello a 5 variabili, modifico pertanto il dataset:  

german <- german_data [, c (1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21)]


#ANALISI CLUSTERING

#CLUSTERING 
#ELimino dalle features di german_mod le colonne 11,18  in quanto scartate dalla variable selection

german_mod <- german_data [,c(2,5,8,11,13,16,18)]
#levo  n_credits, installment_rate poichè non importanti.
german_clus1 <- german_data[,c(2,5,13)]
#scalo i dati
german.scaled1 <- data.frame(scale(german_clus1))


result <- Factoshiny(german.scaled1)
Factoshiny(result)

# con NbCluster controllo quanti possibili cluster ci sono: distanza euclidea e metodo ward.D
ris <- NbClust(data = german.scaled1, method = 'ward.D')
# analizzo l'indice CH: 543.9664 cluster: 2
ris$All.index

# Plot con la partizione migliore: 3 cluster in questo caso
plot(german.scaled1, col= ris$Best.partition)

#--------
# NbCluster distranza minkowski e metodo ward.D

ris.min <- NbClust(data = german.scaled1, distance = 'minkowski', method = 'ward.D')

# anilizzo l'indice CH: 543.9664 cluster: 2
ris.min$All.index

# plot con la partizione migliore: 3 cluster 
plot(german.scaled1, col= ris.min$Best.partition)

#----------
# NbCluster distranza manhattan e metodo ward.D

ris.man <- NbClust(data = german.scaled1, distance = 'manhattan', method = 'ward.D')

# anilizzo l'indice CH: 527.9957 cluster 3
ris.man$All.index

# cluster 3
plot(german.scaled1, col= ris.man$Best.partition)


#---------

#NbCluster distranza euclidea e metodo single

ris.sin <- NbClust(data = german.scaled1, method = 'single')

# anilizzo l'indice CH: 8.9553 cluster  2
ris.sin$All.index

# cluster 2
plot(german.scaled1, col= ris.sin$Best.partition)


#------------------------------
# NbCluster distranza euclidea e metodo complete

ris.complete <- NbClust(data = german.scaled1, method = 'complete')

# anilizzo l'indice CH: 481.5231 cluster 2
ris.complete$All.index

# cluster 2
plot(german.scaled1, col= ris.complete$Best.partition)

#-----------------

#hierachical clustering
#tolti response, telephone e foreign, 
german.hier <- german[, c(1,2,3,4,5,6,7,9,10,12,16)]

d <- dist(german.scaled1)
fitH <- hclust(d, "ward.D2")
plot(fitH)
rect.hclust(fitH, k = 2, border = "red")
clusters <- cutree(fitH,2)
clusters
plot(german.hier, col = clusters)


#----------
# NbCluster distranza euclidea e metodo kmeans
ris.k <- NbClust(data = german.scaled1, method = 'kmeans')

# anilizzo l'indice CH: 508.5921 cluster 3
ris$All.index

germanKmeans<-eclust(german.scaled1, "kmeans", hc_metric="euclidean",k=2)




#Indice di Calinski-Harabasz più alto è :  254.7727 con distanza Euclidea e metodo 'complete'
#e quindi 2 cluster.


#Grafici per vedere il  cluster ottimali con metodi: Silhouette, elbow
fviz_nbclust(german.scaled1, kmeans, method = "silhouette") #2 cluster

fviz_nbclust(german.scaled1, kmeans, method = "wss") #guardsando l'andamento della curva direi 3

fviz_nbclust(german.scaled1, kmeans, method = "gap_stat")
scal<-kmeans(german.scaled1,2)

scal

#MCA
result <- Factoshiny(german_data[,-21])
Factoshiny(result)

#


res.MCA<-MCA(german_data[,-21], level.ventil = 0.10, quanti.sup= c(2,5,8,11,13,16,18), quali.sup= c(4,7,9,20) , method = "Burt")
#summary  description MCA
summary(res.MCA)
dimdesc(res.MCA)

# Graph with some labels
plot(res.MCA, label=c("var","quali.sup"), cex=0.7)
# Graphs with selection of elements


plot(res.MCA, invisible=c("quali.sup", "ind"), cex=0.7)
#confrontare con gli individui
plot(res.MCA, invisible=c("var", "ind"), cex=0.7)
plot(res.MCA, invisible=c("ind"), cex=0.7)


plot(res.MCA, invisible=c("ind","quali.sup"),autoLab="y",cex=0.7,title="Active categories")
plot(res.MCA, invisible=c("ind","var"),autoLab="y",cex=0.7,title="Supplementary categories")



#Contrinuto delle variabili
#dimensione 1
fviz_contrib(res.MCA, choice = 'var', axes = 1:2, top = 15)
#dimensione 2
fviz_contrib(res.MCA, choice = 'var', axes = 2, top = 15)
#dimensione 3
fviz_contrib(res.MCA, choice = 'var', axes = 3:4, top = 15)
#dimensione 4
fviz_contrib(res.MCA, choice = 'var', axes = 4, top = 15)


Investigate(res.MCA)



#------FINE!
# Selection of some categories
plot(res.MCA, invisible="ind",autoLab="y",cex=0.7,selectMod="cos2 10")
plot(res.MCA, invisible="ind",autoLab="y",cex=0.7,selectMod="contrib 20")

# Selection of some individuals
plot(res.MCA, invisible=c("var","quali.sup"),autoLab="y",cex=0.7,select="cos2 20")

# Selection of some categories and some individuals
plot(res.MCA, autoLab="y",cex=0.7, select="cos2 20", selectMod="cos2 10")

# Graphs of the variables
plot(res.MCA, choix="var",xlim=c(0,0.6),ylim=c(0,0.6))
plot(res.MCA, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"))

# Graphs on dimensions 3 and 4
plot(res.MCA,invisible=c("var","quali.sup"),cex=0.7,select="contrib 20",axes=3:4)
plot(res.MCA, invisible="ind",autoLab="y",cex=0.7,selectMod="cos2 20",axes=3:4)



