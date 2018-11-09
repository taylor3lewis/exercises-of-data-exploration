library(ggplot2)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
options("scipen"=999, "digits"=4)
data_db <- read.csv("exercicio.csv", header=T)

# Em qual região estão presentes a maior concentração de casados------------------------------
regioes_estado_civil <- data.frame(regions=c("Região 1","Região 2","Região 3"),
                                   value=c(NROW(subset(subset(data_db, REGIAO==1), ESTCIVIL==2)),
                                           NROW(subset(subset(data_db, REGIAO==2), ESTCIVIL==2)),
                                           NROW(subset(subset(data_db, REGIAO==3), ESTCIVIL==2))))

ggplot(regioes_estado_civil, 
       aes(x=regions, 
           y=value, fill=regions, 
           order=regions,
           label=regions)) + 
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Pessoas Casadas por Região.") +
  geom_label(position="stack", size=2.5, color="black") +
  guides(fill=guide_legend(title=NULL))
#----------------------------------------------------------------------------------------------

# Qual é o salário médio das pessoas em cada região -------------------------------------------
sal_regioes <- data.frame(regions=c("Região 1","Região 2","Região 3"),
                          value=c(mean(subset(data_db, REGIAO==1)$SALARIO),
                                  mean(subset(data_db, REGIAO==2)$SALARIO),
                                  mean(subset(data_db, REGIAO==3)$SALARIO)))

ggplot(sal_regioes, 
       aes(x=regions, 
           y=value, 
           fill=regions, 
           order=regions,
           label=paste(regions, "\nR$", round(value, digits = 3)))) + 
  geom_bar(position='dodge', stat='identity', color='black') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Média salarial por por Região (em R$1.000)") +
  geom_label(position="stack", size=2.5, color="black") +
  guides(fill=guide_legend(title=NULL))

#----------------------------------------------------------------------------------------------

# Histograma Salário --------------------------------------------------------------------------
ggplot(data=data_db, aes(SALARIO)) + 
  geom_histogram(
    col="#ffffff", 
    # alpha = .5,
    aes(fill=..count..)) + 
  scale_fill_gradient(low = "#CCFFDD", high = "#007733") +
  labs(title="Distribuição de Valores de Salário") +
  labs(x="Valor do Salário (em milhares de reais)", y="Número de Ocorrências")
#----------------------------------------------------------------------------------------------
