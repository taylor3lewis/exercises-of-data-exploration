library(ggplot2)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
options("scipen"=999, "digits"=4)
data_db <- read.csv("exercicio.csv", header=T)

# ESTCIVIL por INSTRUCAO--------------------------------------------------------
column_names <- c("instrucao", "ocorrencias")

instru_civil_1 <- as.data.frame(table(subset(data_db, ESTCIVIL==1)$INSTRUCAO))
instru_civil_2 <- as.data.frame(table(subset(data_db, ESTCIVIL==2)$INSTRUCAO))

colnames(instru_civil_1) <- column_names
colnames(instru_civil_2) <- column_names

# instru_civil_1[,1]
# solteiros_casados_instrucao.png

solt_chart <- ggplot(instru_civil_1, 
                     aes(x=instrucao, 
                         y=ocorrencias, 
                         fill=c("Ensino\nMédio","Superior\nIncompleto","Superior\nCompleto"), 
                         label=c("Ensino\nMédio","Superior\nIncompleto","Superior\nCompleto"))) + 
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Número de Solteiros por grau de instrução") +
  geom_label(position="stack", size=3) +
  guides(fill=guide_legend(title=NULL))


casa_chart <- ggplot(instru_civil_2, 
                     aes(x=instrucao, 
                         y=ocorrencias, 
                         fill=c("Ensino\nMédio","Superior\nIncompleto","Superior\nCompleto"), 
                         label=c("Ensino\nMédio","Superior\nIncompleto","Superior\nCompleto"))) + 
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Número de Casados por grau de instrução") +
  geom_label(position="stack", size=3) +
  guides(fill=guide_legend(title=NULL))

grid.arrange(solt_chart, casa_chart, ncol = 2)

#------------------------------------------------------------------------------

