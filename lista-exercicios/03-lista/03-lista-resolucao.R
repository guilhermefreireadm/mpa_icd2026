# Arquivo: 03-lista-resolucao.R
# Autor(a): Guilherme Freire
# Data: 26/03/2026
# Objetivo: Resolução da lista de exercícios 3

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, tidyr...


# Exercício 1 ---------------------------------------------------------------


# a. importa os arquivos
#-------------------------------------------------------------------------------
# Cria o caminho relativo para o arquivo produtos.csv
caminho_produtos_csv <- here ("data/raw/produtos.csv")

# Cria o objeto dados_produtos
dados_produtos <- read_csv(caminho_produtos_csv) # Criando o objeto

#-------------------------------------------------------------------------------
# Cria o caminho relativo para o aquivo vendas.csv
caminho_vendas_csv <- here ("data/raw/vendas.csv")

# Cria o objeto dados_vendas
dados_vendas <- read_csv(caminho_vendas_csv) # Criando o objeto


#-------------------------------------------------------------------------------
# Cria o caminho relativo para arquivo clientes.csv
caminho_clientes_csv <- here("data/raw/clientes.csv")

# Cria o objeto dados_clientes
dados_clientes <- read_csv(caminho_clientes_csv) # Criando o objeto


# analisa os objetos importados

glimpse(dados_produtos) # Analisando os dados dos produtos

glimpse(dados_vendas) # Analisando os dados das vendas

glimpse(dados_clientes) # Analisando os dados dos clientes

#-------------------------------------------------------------------------------
# b. junção dos objetos vendas com produtos e clientes

relatorio_vendas <- dados_vendas |> # criando o objeto relatorio de vendas
  left_join(dados_produtos, by = "codigo_produto") |> # junção variáveis
  left_join(dados_clientes, by = "id_cliente") # Segue a junção entre variáveis

# Objeto criado pela junção de vendas, produtos e clientes
relatorio_vendas

#-------------------------------------------------------------------------------
# c. Selecionando as variáveis 
relatorio_vendas <- relatorio_vendas |> 
  select(
    id_venda, 
    data_venda, 
    nome_produto, 
    categoria, 
    quantidade, 
    nome_cliente,
    cidade
    )
# Visualizando o novo objeto 
relatorio_vendas

# Exibindo a estrutura do novo objeto
glimpse(relatorio_vendas) 

#-------------------------------------------------------------------------------
# d. No id_venda 5, o nome_produto e categoria estão como NA porque o código 
# do produto não existe na tabela 'dados_produtos'. 
# No id_venda 6, o nome do cliente está como NA porque o id_cliente 
# não foi encontrado na tabela 'dados_clientes'.
# Isso indica dados inconsistentes ou cadastros faltantes nas tabelas
# de referência

#-------------------------------------------------------------------------------
novo_relatorio <- dados_vendas |> 
  full_join(dados_produtos, by = "codigo_produto")

relatorio_vendas
novo_relatorio

# Com a junção completa feita pela função full_join, temos mais variáveis
# e observações a serem analisadas, podemos notar mais dados inconsistentes
# e mais cadastros faltantes nas tabelas. Temos uma visão mais global 
# do novo data frame, o que possibilita uma análise mais profunda e assim 
# identificar mais possíveis falhas ou até mesmo fraudes nos dos dados.


# Exercício 2 ------------------------------------------------------------------


# a. importa os arquivos
#-------------------------------------------------------------------------------
# Cria o caminho relativo para o arquivo governanca.csv
caminho_dados_governanca_csv <- here("data/raw/governanca.csv")

# Cria o objeto dados_governanca
dados_governanca <- read_csv(caminho_dados_governanca_csv)

# Cria o caminho relativo para o aquivo risco.csv
caminho_dados_risco_csv <- here("data/raw/risco.csv")

# Cria o objeto dados_risco
dados_risco <- read_csv(caminho_dados_risco_csv)

# Cria o caminho relativo para o arquivo contabeis.csv
caminho_dados_contabeis <- here("data/raw/contabeis.csv")

# Cria o objeto dados_contabeis
dados_contabeis <- read_csv(caminho_dados_contabeis)

#-------------------------------------------------------------------------------
# analisa os objetos importados
glimpse(dados_governanca)

glimpse(dados_risco)

glimpse(dados_contabeis)

#-------------------------------------------------------------------------------
# b. combina governança, risco e dados contábeis
# Devido a variável ano constar tanto no objeto dados_risco e dados_contabeis,
# é preciso solicitar a chave de junção pelas 2 variáveis, caso contrário
# apareceria 2 ano, ano_x e ano_y.

analise_integrada <- dados_governanca |> 
  left_join(dados_risco, by = "codigo_negociacao") |> 
  left_join(dados_contabeis, by = c("codigo_negociacao", "ano")) 

# c. Selecionando as variáveis
analise_integrada <- analise_integrada |> 
  select(
    empresa,
    codigo_negociacao,
    ano,
    indice_governanca,
    tipo_controlador,
    comite_auditoria,
    retorno_anual,
    volatilidade,
    beta,
    roa,
    alavancagem,
    tamanho_ativo
  )

# exibe a estrutura do resultado
glimpse(analise_integrada)

#-------------------------------------------------------------------------------
# d. Porque com a junção dos objetos pode ocorrer de faltar dados em alguma
# observação, logo a função aponta NA - (Note Avaliable / dado faltante).
# Porque a função left_join() irá manter sempre os dados do lado esquerdo,
# isso definirá a ordem por qual objeto iniciar a utilização da 
# função left_join(), o que impactará em quais as observações ficará 
# para a análise dos dados.

# Exercício 3 ---------------------------------------------------------------


# a. importa os arquivos
caminho_acoes_csv <- here("data/raw/acoes.csv")
dados_acoes <- read_csv(caminho_acoes_csv)

caminho_eventos_corporativos_csv <- here("data/raw/eventos_corporativos.csv")
dados_eventos_corporativos <- read_csv(caminho_eventos_corporativos_csv)

# analisa os objetos importados
glimpse(dados_acoes)

glimpse(dados_eventos_corporativos)

#-------------------------------------------------------------------------------
# b. constrói a base do estudo de eventos
# Fusão dos 2 objetos, dados_eventos_corporativos e dados_acoes utilizando chave
# em 2 variáveis, devido a data e data_anuncio representarem datas e deixando 
# objeto mais limpo para uma analise mais eficaz
dados_estudo_eventos <- dados_acoes |> 
  inner_join(                          
    dados_eventos_corporativos,   
    by = c("ticker", "data" = "data_anuncio") 
    ) |> 
#-------------------------------------------------------------------------------

# c. Seleciona as variáveis
    select(ticker,data, tipo_evento, valor, retorno_diario, volume)

# exibe o objeto final
glimpse(dados_estudo_eventos)

#-------------------------------------------------------------------------------
# d. Porque a função inner_join() trabalha por integração, vai manter apenas
# as observações que existirem em comum em ambos os data frame.
# Porque a função inner_join() atua por integração do data frame por
# interseção, ou seja vai permanecer as observações que existir em comum
# nos data frame.
# 
# ------------------------- FIM ---------------------------------------------#
