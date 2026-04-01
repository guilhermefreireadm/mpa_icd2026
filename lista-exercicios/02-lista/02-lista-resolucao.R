# ============================================================
# Disciplina: Introdução à Ciência de Dados
# ============================================================
# Arquivo: 02-lista-resolucao.R
# Autor(a): Guilherme Freire
# Data: 19/03/2026
# Objetivo: Resolução da lista de exercícios 2

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminho relativo
library(tidyverse) # meta-pacote que inclui readr, dplyr..
library(gapminder) # contém os dados gapminder

# carrega os dados do pacote gapminder
data(gapminder)
dplyr::glimpse(gapminder)

caminho_gapminder_csv <- here("data/gapminder")

glimpse(gapminder)

## -----------------------------------------------------------------------------
## Exercício 01
# Define o caminho relativo para o arquivo 
# usando a função here do pacote here

caminho_productionlog_csv <- here("data/raw/productionlog_sample.csv")

# Importo o arquivo csv com a função read_csv() do pacote readr 
# e armazeno no objeto productionlog
registro_producao <- read_csv(caminho_productionlog_csv)

# Inspecionando o objeto que armazenou os dados importados usando a função
# glimpse() do pacote dplyr
glimpse(registro_producao)

#------------------------------------------------------------------------------
# Exercício 02
# Selecionando apenas o país, ano e expectativa de vida 
dados_expectativa <- gapminder |>
  select(country, year, lifeExp)

# Apresenta as seleções
dados_expectativa

#-------------------------------------------------------------------------------
# Exercício 03
# Verificar todas as variáveis exceto população e PIB per capita

exceto_pop_gdppercap <- gapminder |>
  select(-pop, -gdpPercap)

# Apresenta as variáveis exceto população e PIB per capita
exceto_pop_gdppercap

#-------------------------------------------------------------------------------
# Exercício 04
# Verificar apenas as variáveis que começam com a letra "C"

variaveis_com_c <- gapminder|>
  select(starts_with("c"))

# Apresenta variáveis com C
variaveis_com_c

#-------------------------------------------------------------------------------
# Exercício 05
# Selecionar todas as variáveis desde coutry até pop (em sequência na tabela) 

contry_ate_pop <- gapminder|>
  select(country:pop)

# Apresenta a sequência de variáveis
contry_ate_pop

#-------------------------------------------------------------------------------
# Exercício 06
# Selecionar variáveis usando 2 métodos diferentes na mesma instrução:
# variavéis que contenham a letra "p" ou que termine com a letra "p"

var_letra_p <- gapminder|>
  select(contains("p") | ends_with("p"))

# Apresenta as variáveis com letra p
var_letra_p

#-------------------------------------------------------------------------------
# Exercício 07
# Filtrar apenas países do continente Americas no ano de 2007

paises_america_2007 <- gapminder |>
  filter(continent == "Americas" & year == 2007)

# Apresenta as vaiáveis filtradas
paises_america_2007

#-------------------------------------------------------------------------------
# Exercício 08
# Filtrar data frame para mostrar apenas os dados do Brasil
# salvar em um objeto

dados_brasil <- gapminder|>
  filter(country == "Brazil")

# Apresenta o Filtro apenas para dados do Brasil
dados_brasil

#-------------------------------------------------------------------------------
# Exercício 09
# Filtrar Países do continente asiático, população acima de 50 milhões 
# nos dados de 2007

dados_asiaticos <- gapminder|>
  filter(
    continent == "Asia", 
    pop > 50000000 & 
    year == 2007
    )

# Apresenta o filtro 
dados_asiaticos

#-------------------------------------------------------------------------------
# Exercício 10
# Filtrar países com expectativa de vida > 75 anos, mas PIB per capita < 10000
# dólares em 2007

filtro_paises <- gapminder|>
  filter(
    lifeExp > 75, 
    gdpPercap < 10000, 
    year == 2007
    )

# Países com expectativa de vida > 75 anos, mas PIB < $ 10000 dados de 2007
filtro_paises

#-------------------------------------------------------------------------------
# Exercício 11
# Criando nova variável, Converter população para milhões, com função de mutate

gapminder |>
  mutate(pop_em_milhoes = pop / 1000000) ## |>
  ## select(country, continent, year, lifeExp, pop, pop_em_milhoes) ## (TESTE)

#-------------------------------------------------------------------------------
# Exercício 12
# Criar uma nova variável de receita total com função mutate

gapminder |>
  mutate(receita_total = gdpPercap * pop) ##|> # PIB do país
    ## select(country, gdpPercap, pop, receita_total) ## (TESTE)

#-------------------------------------------------------------------------------
# Exercício 13
# Cria uma variável chamada economia_grande sendo Sim quando a população
# maior que 50 milhões e não caso contrario

gapminder|>
  mutate(
    economia_grande = ifelse(pop > 50000000, "sim", "não")) ## |>
    ## filter(pop > 48000000 & pop < 52000000)|>  # Criei apenas para teste
    ## select(country, pop, economia_grande)  # visualisação da variável criada

#-------------------------------------------------------------------------------
# Exercício 14
# Cria uma variável que classifica países em 3 categorias
# baseada na expectativa de vida:
# Baixa: menos de 60 anos
# Média: entre 60 e 75 anos
# Alta: mais de 75 anos

expectativa_de_vida <- gapminder |>
  filter(year == 2007)|>
  mutate(expectativa_vida_var = case_when(
    lifeExp < 60 ~ "baixa",
    lifeExp <= 75 ~ "media",
    lifeExp > 75 ~ "alta"
  )) |>
  select(country, year, lifeExp, expectativa_vida_var)

expectativa_de_vida

#-------------------------------------------------------------------------------
# Exercício 15
# Calcular a expectativa de vida por continente

expectativa_por_continente <- gapminder|>
  group_by(continent)|>
  summarise(expectativa_media = mean(lifeExp))

expectativa_por_continente

#-------------------------------------------------------------------------------
# Exercício 16
# Calcula a população total por continente no ano de 2007

pop_total_cont_2007 <- gapminder|>
  filter(year == 2007)|>
  group_by(continent)|>
  summarise(populacao_total_2007 = sum(pop)) 

pop_total_cont_2007

#-------------------------------------------------------------------------------
# Exercício 17
# Imaginando que cada país representa uma empresa. 
# Cria um objeto que mostre, para cada continente:
# O número de filiais (países)
# O PIB percapita médio (indicador de desempenho)
# O PIB percatpita da melhor filial (máximo)

empresa_ficticia <- gapminder|>
  group_by(continent)|>
  summarise(
    numero_filiais = n(), # Considerando que cada país seria uma filial
    receita_media = median(gdpPercap), # Considere receita média das "filiais"
    maior_receita = max(gdpPercap) # Maior receita dentre as "filiais"
  )

empresa_ficticia

#-------------------------------------------------------------------------------
# Exercício 18
# Expectativa média de vida do continente americano ao longo dos anos

evolucao_expectativa_vida <- gapminder |>
  filter(continent == "Americas") |>
  group_by(continent, year) |>
  summarise(mean(lifeExp))
  
evolucao_expectativa_vida

#-------------------------------------------------------------------------------
# Exercício 19
# Ordenar países por expectativa de vida (decrescente)

paises_ordenados <- gapminder |>
  filter(year == 2007) |>
  arrange(desc(lifeExp))

paises_ordenados

#-------------------------------------------------------------------------------
# Exercício 20
# Cria o ranking dos menores PIB Per capita em 2007

ranking_menores_gdppercap <- gapminder|>
  filter(year == 2007)|>
  arrange(gdpPercap)|> # Ordena de forma crescente
  head(5) |> # Estabelece as 5 últimas linhas
  arrange(desc(gdpPercap)) # reordena em ordem decrescente

ranking_menores_gdppercap

## Foi utilizado dois arrange porque se usar apenas em ordem decrescente
## apresenta os maiores PIB, se utilizar apenas o crescente apresenta de forma
## dos menores para o maior, utilizando os 2 um busca os 5 menores e depois 
## reordena dos maiores para menores

#-------------------------------------------------------------------------------
# Exercício 21
# Lista dos países das Américas ordenados por população em ordem decrescente
# em 2007

paises_americanos_desc_2007 <- gapminder|>
  filter(continent == "Americas", year == 2007) |>
  arrange(desc(pop))

paises_americanos_desc_2007

#-------------------------------------------------------------------------------
# Exercício 22
# Ranking dos continentes baseado na expectativa média de vida de seus países
# no ano de 2007

ranking_cont_expvida <- gapminder|>
  filter(year == 2007) |>
  group_by(continent, year) |>
  summarise(media_vida = mean(lifeExp)) |>
  arrange(desc(media_vida))

ranking_cont_expvida
