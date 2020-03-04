setwd("~/Desktop/MedicalBioinformatics/Varie/Progetto Antonino/Notorieta_11_2019/drug")
file <-read.csv("drugfreq.csv", header = TRUE)

library(ggplot2)

p <- ggplot(file, aes(x=Sum.Col8.)) + 
  geom_density(color="red", fill="red")  +
  geom_vline(aes(xintercept=mean(Sum.Col8.)), color="blue", linetype="dashed", size=1)
p
#in scala logaritmica

#Limito i valori dell'asse y. Sono compresi tra 0.000 e 0.0005
p1 <- p + ylim(0.000,0.0005)
p1

#Limito i valori dell'asse x. Sono compresi tra 0.000 e 5000
p2 <- p + xlim(0.000,5000)
p2


#Faccio la stessa cosa per la coppia drug-soc
file2<- read.csv("drugsCoupled.csv", header = FALSE)

g <- ggplot(file2, aes(x=V3)) + 
  geom_density(color="blue", fill="blue")  +
  geom_vline(aes(xintercept=mean(V3)), color="red", linetype="dashed", size=1)
g

#Limito i valori dell'asse y. Sono compresi tra 0.000 e 0.0005
g1 <- g + ylim(0.000,0.0005)
g1

#Limito i valori dell'asse x. Sono compresi tra 0.000 e 5000
g2 <- g + xlim(0.000,5000)
g2

  

    