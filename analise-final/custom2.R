library(ggplot2)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
options("scipen"=999, "digits"=4)
data_db <- read.csv("exercicio.csv", header=T)

# ESTCIVIL por INSTRUCAO--------------------------------------------------------
column_names <- c("idade", "ocorrencias")

idade_civil_1 <- as.data.frame(table(subset(data_db, ESTCIVIL==1)$ANO))
idade_civil_2 <- as.data.frame(table(subset(data_db, ESTCIVIL==2)$ANO))

colnames(idade_civil_1) <- column_names
colnames(idade_civil_2) <- column_names

# idade_civil_1[,1]
# solteiros_casados_instrucao.png
idade_civil_1

# solt_chart <- 
id1<-ggplot(idade_civil_1,
       aes(x=idade, 
           y=ocorrencias,
           fill=idade, label=idade, colour=idade,
           group=1)) + 
  geom_line(aes(x=idade, y=ocorrencias)) +
  geom_point() +
  labs(y="Número de Ocorrências de Solteiros por Idade")

id2<-ggplot(idade_civil_2,
       aes(x=idade, 
           y=ocorrencias,
           fill=idade, label=idade, colour=idade,
           group=1)) + 
  geom_line(aes(x=idade, y=ocorrencias)) +
  geom_point() +
  labs(y="Número de Ocorrências de Casados por Idade")

grid.arrange(id1,id2,ncol=2)

#------------------------------------------------------------------------------

