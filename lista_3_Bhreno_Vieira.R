####################################################################################
#######################   Universidade Federal de Pernambuco #######################
#######################    Departamento de Ciência Política  #######################
#######################             Mestrado 2019            #######################
#######################           Análise de Dados           #######################
#######################            Bhreno Vieira             #######################
#######################               Lista 3                #######################


#### Questão 1 ####

#### Questão 2 ####

# Declare duas variáveis (x e y) e valores números maiores que 1 a elas: 

x <- 13

y <- 12

# Confirmando o valor de x e y

x

y

# O resultado da soma seja "z"

z <- x + y

# confirindo o resultado

z

# multiplique o valor de z pelo nº do seu CPF

w <- z * 07567034417

# confirmando o resultado

w

# Resultados
# x = 13, y= 12
# x + y = z 
# 13 + 12 = 25
# w = z * cpf
# w = 189175860425

#### Questão 3 ####

# solicitando a base de dados mtcars

head(mtcars)

# visualizando a base mtcars

View(mtcars)

# solicitando os nomes das variáveis para indificar as colunas do Banco

names(mtcars)

# tipos de variáveis da base

class(mtcars)

class(mtcars$mpg)
class(mtcars$cyl)
class(mtcars$disp)
class(mtcars$hp)
class(mtcars$drat)
class(mtcars$wt) 
class(mtcars$qsec) 
class(mtcars$vs)
class(mtcars$am)
class(mtcars$gear)
class(mtcars$carb)

## Todas as variáveis do banco são númericas. 

# número de dimensões

dim(mtcars)

# 32 11

# imprimir a 3ª coluna

mtcars[, "disp"]

# imprimir a 2ª linha

mtcars[, "cyl"]

mtcars["Mazda RX4 Wag",]

# o 4º elemento presente na variável "cyl"

mtcars["Hornet 4 Drive","cyl"]

# resumo descritivo da base

summary(mtcars)

#### Questão 4 ####

## use o pacote ffbase para carregar a base de dados "TURMAS.csv"

# primeiro, tenho que instalar o pacote "ffbase"

install.packages("ffbase", dependencies = TRUE)

# solicito o pacote

require(ffbase)

# Após isso,tendo mostrar ao R qual o diretório está sendo utilizado, pergunto onde posso encontrar os dados: 

getwd()

# após isso, indico o caminho: 

setwd("C:/Users/Bhr/Documents/dados_encontro_1_ufpe_certo")

# solicito a abertura dos dados:

turmas <- read.csv2.ffdf(file = "TURMAS.csv", sep = "|")

# conhecendo as dimensões da base: 

dim(turmas)

# verificando o nome das 10 primeiras colunas: 

names(turmas)[1:10]

# verificando as 6 primeiras linhas da base de dados

head(turmas[, 1:5])

# um resumo de informações

summary(turmas)


## filtre os registros referentes ao Estado de Pernambuco (CO_UF == "26")

turmas_pe <- subset(turmas, CO_UF == "26")

#conhecendo a nova base formada

dim(turmas_pe)

head(turmas_pe[, 1:5])

## Transforme em data.frame a base "turmas_pe"

turmas_pe <- as.data.frame(turmas_pe)

## salve a base em formato R.Data

getwd()

save(turmas_pe, file = "turmas_pe_censo_escolar_2016.RData")

#### Questão 5 ####

# carregue a base de dados referentes a turmas do Estado de Pernambuco

head(turmas_pe[, 1:5])

names(turmas_pe)

# apresente a média do número de matrícula por turma

mean(turmas_pe$NU_MATRICULAS)

# Média: 23.07089

summary(turmas_pe$NU_MATRICULAS)


#### Questão 6 ####

### Etapa I: pré-processamento dos dados

getwd()

docentes_ne <- read.csv2.ffdf(file = "DOCENTES_NORDESTE.csv", sep = "|", 
                              first.rows=100000)

# conhecendo a base "docentes_ne"

dim(docentes_ne)

head(docentes_ne[, 1:5])

# filtre os registros referentes ao Estado de Pernambuco (CO_UF == "26")

docentes_pe <- subset(docentes_ne, CO_UF == "26")

# transformando em data.frame e salvando em formato RData.

docentes_pe <- as.data.frame(docentes_pe)

getwd()

save(docentes_pe, file = "docentes_pe_censo_escolar_2016.RData")

# conhecendo a nova base

dim(docentes_pe)

names(docentes_pe)

### Etapa II: Qual o percentual de docentes do PE que não declaram cor ou raça?

table(docentes_pe$TP_COR_RACA)

prop.table(table(docentes_pe$TP_COR_RACA))*100

plot(prop.table(table(docentes_pe$TP_COR_RACA))*100)

# elaborando um gráfico

require(tidyverse)

ggplot(docentes_pe, aes(prop.table(docentes_pe$TP_COR_RACA)*100))+geom_bar()

# Resposta: cerca de 44% dos docentes não declararam sua cor. 

### Etapa III: Qual o percentual de docentes que se declaram pretos ou pardos? 

prop.table(table(docentes_pe$TP_COR_RACA))*100

# Apenas 3,5% dos docentes se declaram "pretos" e 27,79% se declaram "pardos". 

# A soma da porcetagem de pretos e pardos

3.5+27.79

# Resultado da porcetagem de pretos e pardos: 31,29%


