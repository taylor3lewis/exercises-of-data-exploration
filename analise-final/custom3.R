library(ggplot2)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
options("scipen"=999, "digits"=4)
data_db <- read.csv("exercicio.csv", header=T)

# SALARIO por ANO--------------------------------------------------------
ggplot(data_db, aes(x=SALARIO, 
                    y=FILHOS, 
                    fill=FILHOS,
                    colour="teste")) + 
  geom_point(aes(y=FILHOS, colour="Número de filhos")) +
  geom_point(aes(y=INSTRUCAO, colour="Grau de instrução")) +
  scale_fill_continuous(guide=FALSE) + 
  labs(y="Número de filhos / Grau de Instrução",
       x="Salário\n(em milhares de reais)")

