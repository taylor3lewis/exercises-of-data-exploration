# install.packages("ggplot2")
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
options("scipen"=999, "digits"=4)
data_db <- read.csv("exercicio.csv", header=T)

# SOLTEIROS VS CASADOS--------------------------------------------------------
solteiros_percent <- (NROW(subset(data_db, ESTCIVIL==1)) / NROW(data_db)) * 100
casados_percent <- (NROW(subset(data_db, ESTCIVIL==2)) / NROW(data_db)) * 100
solt_cas_pcent <- data.frame(status=c("casados", "solteiros"),
                             percentagem=c(casados_percent, solteiros_percent))
ggplot(solt_cas_pcent, aes(x=status, y=percentagem, fill=status, 
                           label=paste(round(percentagem, digits=2),"%"))) +
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Proporção entre Solteiros e Casados") +
  geom_label(position="stack", size=3)
#------------------------------------------------------------------------------


# GRAU DE INSTRUÇÃO------------------------------------------------------------
instru1 <- (NROW(subset(data_db, INSTRUCAO==1)) / NROW(data_db)) * 100
instru2 <- (NROW(subset(data_db, INSTRUCAO==2)) / NROW(data_db)) * 100
instru3 <- (NROW(subset(data_db, INSTRUCAO==3)) / NROW(data_db)) * 100
ensino_data <- data.frame(grauDeEnsino=c("Médio", "Superior incompleto", "Superior completo"),
                          percentagem=c(instru1, instru2, instru3))

ggplot(ensino_data, aes(x=reorder(grauDeEnsino, percentagem), y=percentagem, fill=percentagem, 
                           label=paste(round(percentagem, digits=2),"%\n", grauDeEnsino))) +
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Proporção entre graus de instrução.") +
  geom_label(position="stack", size=3, color="white")
#------------------------------------------------------------------------------

# NÚMEROS DE FILHOS ENTRE CASADOS VS SOLTEIROS---------------------------------
solteiros_media_filhos <- sum(subset(data_db, ESTCIVIL==1)$FILHOS) / NROW(subset(data_db, ESTCIVIL==1))
casados_media_filhos <- sum(subset(data_db, ESTCIVIL==2)$FILHOS) / NROW(subset(data_db, ESTCIVIL==2))
media_filhos_data <- data.frame(status=c("solteiros", "casados"),
                                percentagem=c(solteiros_media_filhos, casados_media_filhos))
ggplot(media_filhos_data, aes(x=status, y=percentagem, fill=status, 
                              label=paste(round(percentagem, digits=2)," filhos"))) +
  geom_bar(position='dodge', stat='identity', color='white') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Média de Filhos entre Casados e Solteiros") +
  geom_label(position="stack", size=3)
#------------------------------------------------------------------------------

