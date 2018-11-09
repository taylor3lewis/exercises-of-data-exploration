library(ggplot2)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
options("scipen"=999, "digits"=4)
data_db <- read.csv("exercicio.csv", header=T)

# Diferença de SALARIO ENTRE CASADOS VS SOLTEIROS------------------------------
solteiros_media_sal <- sum(subset(data_db, ESTCIVIL==1)$SALARIO) / NROW(subset(data_db, ESTCIVIL==1))
casados_media_sal <- sum(subset(data_db, ESTCIVIL==2)$SALARIO) / NROW(subset(data_db, ESTCIVIL==2))
if(solteiros_media_sal>casados_media_sal){
  sup_mean <- "solteiros"  
  inf_mean <- "casados"  
}else{
  sup_mean <- "casados"
  inf_mean <- "solteiros"
}
# max bound
max_bound <- max(solteiros_media_sal, casados_media_sal)

# absolutos
media_sal_data <- data.frame(status=c("Casados", "Solteiros"),
                             value=c(casados_media_sal, solteiros_media_sal))
graph1 <- ggplot(media_sal_data, 
                 aes(x=reorder(status, -value), 
                     y=value, fill=status, 
                     label=paste(status,"\nR$",round(value, digits = 3)))) + 
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Média salarial entre solteiros e casados (em milhares de reais)") +
  geom_label(position="stack", size=3, color="black") + 
  scale_y_continuous(limits = c(0, max_bound)) 

# difference
difference_sal <- max(solteiros_media_sal, casados_media_sal) - min(solteiros_media_sal, casados_media_sal)
diff_sal_pcent <- min(solteiros_media_sal, casados_media_sal) / max(solteiros_media_sal, casados_media_sal) * 100

graph2 <- ggplot(data.frame(legenda=c(paste("Diferença de sálario de", sup_mean, "\nem relação aos", inf_mean)), 
                            value=c(difference_sal)), 
                 aes(x=reorder(legenda, -value), 
                     y=value, fill=legenda, 
                     label=paste("R$",round(value, digits = 3)))) + 
  geom_bar(position='dodge', stat='identity', color='black', width=0.3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom") +
  scale_fill_manual("legend", values = c("#6666FF")) +
  geom_label(position="stack", size=3, color="black") +
  scale_y_continuous(limits = c(0, max_bound)) 

grid.arrange(graph1, graph2, ncol=2)

#------------------------------------------------------------------------------
ggplot(media_filhos_data, aes(x=reorder(grauDeEnsino, percentagem), y=percentagem, fill=percentagem, 
                              label=paste(round(percentagem, digits=2),"%\n", grauDeEnsino))) +
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Proporção entre graus de instrução.") +
  geom_label(position="stack", size=3, color="white")
#------------------------------------------------------------------------------

# Diferença de SALARIO ENTRE GRAUS DE INSTRUÇÂO------------------------------
instrucao1 <- sum(subset(data_db, INSTRUCAO==1)$SALARIO) / NROW(subset(data_db, INSTRUCAO==1))
instrucao2 <- sum(subset(data_db, INSTRUCAO==2)$SALARIO) / NROW(subset(data_db, INSTRUCAO==2))
instrucao3 <- sum(subset(data_db, INSTRUCAO==3)$SALARIO) / NROW(subset(data_db, INSTRUCAO==3))
instrucao_media_data <- data.frame(status=c("Ensino Médio", "Superior Incompleto", "Superior Completo"),
                             value=c(instrucao1, instrucao2, instrucao3))
ggplot(instrucao_media_data, 
                 aes(x=reorder(status, -value), 
                     y=value, fill=status, 
                     label=paste(status,"\nR$",round(value, digits = 3)))) + 
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Média salarial por grau de instrução.") +
  geom_label(position="stack", size=2.5, color="black") +
  guides(fill=guide_legend(title=NULL))
