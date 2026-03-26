#==============================================================================
# Disciplina: Introdução à Ciência de Dados
#==============================================================================
# Arquivo:01_introducao.R
# Autor: Guilherme F. Oliveira
# Data: 2026-03-12
# Ojetivo: Usar RStudio, script R e alguns fundamento de R

# Atalho para criar seções de código: Ctrl + Shift + R
# DECORE ESTE ATALHO


# Configurações globais ---------------------------------------------------

# define opções globais para exibição de número
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
# LEMBRE-SE 
library(here)       # para usar caminhos relativos
library(tidyverse)  # metapacote: dplyr, readr, tidyr, ggplot2
library(skimr)     # para compreender os dados skim() e clean_names()
library(janitor)    # para limpar dados - nomes de colunas


# R como uma grande calculadora -------------------------------------------

# Operações aritmeticas básicas

# adição
15 + 7

#subtração
20 - 6

# multiplicação
8 * 9

# divisão
84 / 7

# potencialização
2 ^ 5

# Precedência de operações matemáticas
# Parenteses -> potencialização -> multiplicação e divisão -> adição e subração

(15 + 7) * 2
84 / (7+5)


# Exemplos de funções matemáticas -----------------------------------------

# logaritmo natural
log(100)

# logaritmo base 10
log10(1000)

# funções exponêncial: (e elevado a x) e ^ x
exp(1)

# valor absoluto (módulo)
abs(-45)

# raiz quadrada
sqrt (225)

# arredondamento para 2 casas decimais
round(3.14159, 2)



# Tipos atômicos e classes ----------------------------------------------------

# Tipos de dados definem como os dados são armazenados na memória.

# Tipo double e classe numeric
a <- 3.14
a

# Função que retorna o tipo do objeto
typeof(a)

#Função que retorna a classe do objeto
class(a)

# Characteres
b <- "João"
b

# Logical
c <- TRUE
c

d <- FALSE
d

# NaN (Note a Number) representa um valor indefinido
e <- 0 / 0
e

# Coerção de logical para numeric
# TRUE = 1 FALSE = 0
f <- as.numeric(c)
f



# Vetores numéricos -----------------------------------------------------------

# Cria dois vetores numéricos com dados de receita e custo diários

receita_diaria <- c(9200, 8700, 10100, 9800, 11050)
print (receita_diaria)


custo_diario <- c(6400, 6000, 7200, 6800, 7600)
custo_diario


# Vetorização: Operações elemento a elemento
lucro_diario <- receita_diaria - custo_diario
margem_diaria <- lucro_diario / receita_diaria



# Funções úteis para vetores númericos ----------------------------------------

# Logaritmo da receita diária
log(receita_diaria)

# Receita total da semana
sum(receita_diaria)

# Receita média da semana
mean(receita_diaria)


# Tamanho do vetor da receita
# Nesse caso é o número de dias registrados
length(receita_diaria)

# Receita mínima da semana
min(receita_diaria)

# Receita máxima da semana
max(receita_diaria)

# Vendo a ajuda de uma função
?mean
?length



# Vetores de caracteres e lógicos

# Vetores devem conter o mesmo tipo de dados, ou seja, 
# todos os elementos devem ser do mesmo tipo

# Vetor de caracteres
nome_empresa <- c("Loja A", "Loja B", "Loja C")

# exibe o vetor criado
nome_empresa

# Vetor lógico (booleano) indicando se a meta de vendas foi batida
meta_batida <- c(TRUE, FALSE, TRUE)

# Exibe o vetor criado
meta_batida



# Fatores ---------------------------------------------------------------------

# Fatores são usados para armazenar variáveis categóricas nominais ou ordinais

# Vetor de caracteres com meses do ano
meses <- c("Dezembro", "Abril", "Janeiro", "Março")
meses

# Um vetor de caracteres é ordenado por ordem alfabética
sort(meses)



# Definindo os níveis dos meses em ordem cronológica
niveis_meses <- c(
"Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
"Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
)

# Converte o vetor meses para fator, usando os níveis definidos
meses <- factor(meses, levels = niveis_meses)
meses

# Ordena os meses
sort(meses)


# Importa arquivos de dados ---------------------------------------------------

# Define o caminho relativo para o arquivo csv
# Usando função here() do pacore here
caminho_csv <- here("data/raw/dados_vendas.csv")

# Importa o arquivo csv com a função read_csv do pacote readr 
# e armazena no objeto dados_vendas
dados_vendas <- read_csv(caminho_csv)

# Compreendendo os dados ------------------------------------------------------

## Exibe visão geral dos dados
glimpse(dados_vendas)

## Visualiza as primeiras linhas da tabela
head(dados_vendas)

## Visão detalhada dos dados
skim(dados_vendas)

# Preparação dos dados para análise -------------------------------------------

## Limpa os nomes das colunas
dados_vendas <- dados_vendas |>
  clean_names()
  
# Visão geral dos dados
glimpse(dados_vendas)


## Converte as colunas cidade, representante e produto para fatores
dados_vendas_limpos <- dados_vendas |>
  mutate(
    cidade = as.factor(cidade),
    representante = as.factor(representante),
    produto = as.factor(produto)
)

## Verifica os dados tratados e limpos transformados characteres em factor
glimpse(dados_vendas_limpos)


## Resumo estatístico do objeto
summary(dados_vendas_limpos)
select(data, cidade, representante)

# Salva os dados limpos -------------------------------------------------------

# Define o caminho relativo da pasta onde o arquivo limpo será salvo
caminho_csv_limpo <- here("data/clean/dados_vendas.rds")

# Salva o objeto dados_vendas_limpos no formado rds
readr::write_rds(dados_vendas_limpos, caminho_csv_limpo)

# Que perguntas de negócio você faria para esse conjunto de dados?


# Manipulação / análise com pacote dplyr

# Exemplo 01
# Pergunta de negócio: quero apenas as vendas realizadas em formiga

dados_vendas_limpos |>
filter(cidade == "Formiga")


# Exemplo 02
# Pergunta de negócio: quero apenas as vendas realizadas em Formiga que 
# geraram receia maior que 1000

dados_vendas_limpos |>
filter(cidade == "Formiga" & receita > 1000)


# Exemplo 03
# Pergunta de negócio: quero apenas as colunas cidade e receita

dados_vendas_limpos |>
  select(cidade, receita)
  
# Exemplo 04 
# Pergunta de negócio: quero saber a receita total por cidade
  receita_por_cidade <- dados_vendas_limpos |>
  group_by(cidade)|>
  summarise(receita_total = sum(receita))

# Exibe o objeto criado
receita_por_cidade

# Exemplo 05
# Pergunta de negócio: quero saber a receita total do produto

dados_vendas_limpos|>
  group_by(produto)|>
  summarise(receita_total = sum(receita))
  
# Exemplo 06
# Pergunta de negócio: quero saber a receita total por cliente em ordem 
# decrescente e salvar o resultado em outro objeto

receita_por_cidade_produto <- dados_vendas_limpos|>
  group_by(cidade)|>
  summarise(receita_total = sum(receita))|>
  arrange(desc(receita_total))
  
# Exibe o objeto criado
receita_por_cidade_produto


# Exemplo 06
# Pergunta de negócio: quero saber a receita total por cidade e representante,
# em ordem decrescente de receita

dados_vendas_limpos|>
  group_by(cidade, representante)|>
  summarise(receita_total = sum(receita))|>
  arrange(desc(receita_total))


# Exemplo 07
# Pergunta de negócio: Quero saber a receita total por cidade e produto
# em ordem decrescente

receita_por_cidade_produto <- dados_vendas_limpos|>
  group_by(cidade, produto)|>
  summarise(receita_total = sum(receita))|>
  arrange(desc(receita_total))

# Exibe o objeto criado
receita_por_cidade_produto

# Resolução dos Exercícios ----------------------------------------------------

# Exercício 01
# Criando o vetor
custos_semanais <- c(5400, 6100, 5900, NA, 6300, 6000)

# Apresentando o vetor
custos_semanais


# Calculo a soma dos custos semanais
total_semanal <- sum(custos_semanais, na.rm = TRUE)
total_semanal

# Calculo do custo médio
custo_medio <- mean(custos_semanais, na.rm = TRUE)
custo_medio

# Calculo Menor e Maior valor dos custos
custo_min <- min(custos_semanais, na.rm = TRUE)
custo_max <- max(custos_semanais, na.rm = TRUE)
custo_min
custo_max

total_semanal             
custo_medio
custo_min
custo_max

# Exercício 02
# Filtrando apenas vendas do Produto A
dados_vendas_limpos |> 
  filter(produto == "Produto A")
#  group_by(produto)|>
#  summarise(receita_total = sum(receita, na.rm = TRUE))|>
#  arrange(desc(receita_total))

# Filtrando apenas vendas realizadas em Piumhi com mais de 10 unidades
dados_vendas_limpos|>
  filter(cidade == "Piumhi" & unidades > 10)
  
#Exercício 06
# Calcula o total de unidades por produto
dados_vendas_limpos |>
  group_by(produto)|>
  summarise(sum(unidades))

# Exercicio 07
# Calcular a receita media por cidade
dados_vendas_limpos |>
  group_by (cidade)|>
  summarise(mean(receita))

# Exercicio 08
# Calcular a total por representante
dados_vendas_limpos |>
  group_by((representante)) |>
  summarise(sum(receita))

# Exercício 09
# Calcular o menor preço unitário por produto
dados_vendas_limpos |>
  group_by(produto) |>
  summarise(min(preco_unitario))
            
# Exercício 10 
resultado <- dados_vendas_limpos |>
  select(cidade, produto)
resultado
str(dados_vendas_limpos)