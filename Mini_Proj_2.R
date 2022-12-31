### Mini-Projeto 2 - Análise Financeira com Linguagem SQL e Regressão Linear em Linguagem R

# Definindo o Diretório de Trabalho

setwd("C:/Users/marcos/Documents/Cientista_de_Dados/BigDataRAzure/Cap18")
getwd()


# Carregando Bibliotecas
library(tidyverse)
library(RSQLite)
library(ggplot2)
library(sqldf)


# Carregando Banco de Dados

df <- read_csv("dataset.csv")


# Verificando o tipo de Variáveis do dataset

str(df)


# Visualisando o Dataframe e suas dimensões

View(df)
dim(df)


# Função Para verificar Valores Ausêntes

verifica_na <- function(x){
  colSums(is.na(x))
}
verifica_na(df)


# Omitindo Valores NA
df <- na.omit(df)


# Verificando o as dimensões do Dataframe
dim(df)



### Abaixo as perguntas que devem ser respondidas na análise exploratória de dados com Linguagem SQL:

# 1- Quantas raças estão representadas no dataset?

sqldf("select distinct RACE from df")


# 2- Qual a idade média dos pacientes?

sqldf("select avg(AGE) from df")

# 3- Qual a moda da idade dos pacientes?

sqldf("(SELECT AGE, count(AGE) as cont
      FROM df
      GROUP BY AGE
      ORDER BY cont desc)
      LIMIT 1")


# 4- Qual a variância da coluna idade?

sqldf("select variance(AGE) from df")


# 5 - Qual o gasto total com internações hospitalares por idade?

sqldf("selecT sum(TOTCHG) from df group by AGE")


# 6- Qual idade gera o maior gasto total com internações hospitalares?

sqldf("select AGE, sum(TOTCHG) as soma
      from df
      group by AGE
      order by soma desc
      limit 1")


# 7- Qual o gasto total com internações hospitalares por gênero?

sqldf("select FEMALE, sum(TOTCHG) as soma
      from df
      group by FEMALE
      order by soma desc")


# 8- Qual a média de gasto com internações hospitalares por raça do paciente?
sqldf("select RACE, avg(TOTCHG)
      from df
      group by RACE
      order by RACE")


# 9- Para pacientes acima de 10 anos, qual a média de gasto total com internações
# hospitalares?

sqldf("select AGE, avg(TOTCHG)
      from df
      where AGE > 10
      group by AGE")
      

# 10- Considerando o item anterior, qual idade tem média de gastos superior a 3000?

sqldf("select AGE, avg(TOTCHG) as res
      from df
      where AGE > 10
      group by AGE
      having res > 3000")




### Abaixo as perguntas que devem ser respondidas na análise estatística:

# 1-Qual a distribuição da idade dos pacientes que frequentam o hospital?

# Gerando um Histograma
hist(df$AGE)


# 2- Qual faixa etária tem o maior gasto total no hospital?
faixa_etaria <- df %>%
  select(AGE, TOTCHG) %>%
  group_by(AGE) %>%
  summarise(total_gasto = sum(TOTCHG))
  
# Gerando um  Gráfico de Barras     
barplot(faixa_etaria$total_gasto ~ faixa_etaria$AGE)


# 3- Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?

# Obtendo os valores únicos da variável APRDRG
length(unique(df$APRDRG))

diagnostico <- df %>%
  select(TOTCHG, APRDRG) %>%
  group_by(APRDRG) %>%
  summarise(total_gato = sum(TOTCHG)) %>%
  arrange(desc(total_gato))


print(diagnostico[1,])


# 4- A raça do paciente tem relação com o total gasto em internações no hospital?

total_raca <- df%>%
  select(RACE, TOTCHG)%>%
  group_by(RACE)%>%
  summarise(total_gasto = sum(TOTCHG))

# Visualizando o resultado da operação 
View(total_raca)

# Gerando um Gráfico de Barras
barplot(total_raca$total_gasto ~ total_raca$RACE)


# 5- A combinação de idade e gênero dos pacientes influencia no gasto total em internações
# no hospital?

# Carregando pacote para visualização de relações entre as variáveis
library(corrplot)
corrplot(cor(df$TOTCHG, df$AGE, df$RACE))

# Carregando pacote RandomForest para aferir as relações entre as variáveis
library(randomForest)
modelo <- randomForest(TOTCHG ~ AGE + RACE, data = df, nodesize = 10, importance = T)
modelo

# Visualizando o resultado do modelo
varImpPlot(modelo)

### Resposta da Pergunta 5:
# As duas variáveis AGE e RACE, juntas não apresentam uma forte correlação com o Total de Gastos


# 6- Como o tempo de permanência é o fator crucial para pacientes internados, desejamos
# descobrir se o tempo de permanência pode ser previsto a partir de idade, gênero e raça.

modelo2 <- randomForest(LOS ~ AGE + RACE + FEMALE, data = df, importance = T)

# Criando um modelo de Arvore de Decisão

# Criando os dados de treino e de teste

modelo2.1 <- randomForest(LOS ~ AGE + RACE + FEMALE, data = treino)
summary(modelo2.1)
previsao2.1 <- predict(modelo2.1, teste)
previsao2.1

# Avaliando o Modelo 

# Compare os resultados com a primeira versao do modelo.
avaliacao <- previsao2.1 == teste$LOS
table(avaliacao)
prop.table(table(avaliacao))

varImpPlot(modelo2)


# Pacotes para visualizar a análise de correlação.
library(corrplot)
library(corrgram)

df_6 <- df[, 1:4]
data_cor <- cor(df_6)

# Criando um corrplot
corrplot(data_cor, method = 'color')

# Criando um corrgram
corrgram(df)
corrgram(df, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt)

### Resposta da Pergunta 6:
### Não há uma Correlação entre as Variáveis, a precisão do modelo está muito baixa 0.0078


# 7- Quais variável têm maior impacto nos custos de internação hospitalar?

modelo3 <- randomForest(TOTCHG ~ ., data = df, importance = T)
varImpPlot(modelo3)

### Resposta da Pergunta 7: 
### As variáveis com maior impacto no custo de internação, são LOS, APRDRG, AGE.










