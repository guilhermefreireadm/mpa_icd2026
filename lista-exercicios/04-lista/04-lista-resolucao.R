# Arquivo: 04-lista-resolucao.R
# Autor(a): Guilherme Freire de Oliveira
# Data: 15/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 ------------------------------------------------
# Criando uma função calcular_montante_mensal, para juros compostos

calcular_montante_mensal <- function(capital, taxa_anual, meses) {
  montante_final <- capital * (1 + (taxa_anual/12))^meses
  return(montante_final)
}

# Teste da fórmula: Para capital = R$5.000,00, taxa anual = 10% e 36 meses
calcular_montante_mensal(capital=5000, taxa_anual=0.10, meses=36)


# Exercício 2 ------------------------------------------------
# Função avaliar_investimento para receber número decimal e retorne
# classificação textual.
avaliar_investimento <- function(retorno){
  if (retorno > 0.15){
    return("Excelente")
  } else if (retorno > 0.05) {
      return("Bom")
  } else if (retorno == 0){
    return("Sem Retorno")
  } else {
    return("Retorno Negativo")
  }
}

# Teste da função para retorno = 0.20, 0.08, 0.02 e -0.05

avaliar_investimento (0.20)

avaliar_investimento(0.08)

avaliar_investimento (0.02)

avaliar_investimento (-0.05)


# Exercício 3 ------------------------------------------------

analisar_carteira <- function(dados) {
  dados |> 
    mutate (
      retorno = (preco_atual / preco_compra - 1) * 100,
      valor_investido = preco_compra * quantidade,
      valor_atual = preco_atual * quantidade,
      situacao = if_else ( retorno >= 0, "Ganho", "Perda")
    )
}

carteira <- tibble(
    ativo        = c("PETR4", "VALE3", "ITUB4", "WEGE3"),
    preco_compra = c(28.50, 68.20, 32.00, 45.00),
    preco_atual  = c(31.00, 65.40, 33.60, 48.50),
    quantidade   = c(200, 100, 300, 150)
  )

resultado <- analisar_carteira(carteira) # crio o objeto resultado p/ visualizar

resultado

# Exercício 4 ------------------------------------------------
calcular_valor_futuro <- function ( valor_presente, taxa, periodos = 1){
  valor_futuro <- (valor_presente * (1 + taxa) ^ periodos)
  return(valor_futuro)
}

taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)

vf_20_anos <- map_dbl (
  taxas_anuais,
  \(taxa) calcular_valor_futuro(10000, taxa, 20)
  )

vf_20_anos

comparacao_cenarios <- tibble (
  taxa = c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16),
  taxa_percantual = taxas_anuais * 100,
  valor_futuro = vf_20_anos,
  ganho_liquido = vf_20_anos - 10000
  
)

comparacao_cenarios

# Exercício 5 ------------------------------------------------
calcular_vpl <- function(investimento, fluxos_caixa, 
                         taxas_desconto, 
                         valor_resdiual = 0){
  n <- length(fluxos_caixa)
  t <- seq_along(fluxos_caixa)
  
  vpl <- -investimento + 
    sum(fluxos_caixa / ( 1 + taxas_desconto)^t) +
    valor_resdiual / (1 + taxas_desconto)^n
  return(vpl)
}

fluxos_caixa = c(80000, 95000, 110000, 100000)

taxas_desconto = c(0.08, 0.10, 0.12, 0.14, 0.16, 0.18)

vpl_por_taxa <- map_dbl(
  taxas_desconto,
  \(taxa) calcular_vpl (investimento = 300000, fluxos_caixa, taxa,
  valor_resdiual = 30000)
)

analise_projeto <- tibble(
  taxa_pct = taxas_desconto * 100,
  vpl = vpl_por_taxa,
  decisao = if_else (vpl_por_taxa > 0, "Viavel", "Inviavel")
)

analise_projeto

# Exercício 6 ------------------------------------------------
# (resolver em arquivo .qmd separado)


# Exercício 7 (Desafio) --------------------------------------
calcular_vpl <- function(investimento, fluxos_caixa, 
                         taxas_desconto, 
                         valor_resdiual = 0){
  n <- length(fluxos_caixa)
  t <- seq_along(fluxos_caixa)
  
  vpl <- -investimento + 
    sum(fluxos_caixa / ( 1 + taxas_desconto)^t) +
    valor_resdiual / (1 + taxas_desconto)^n
  return(vpl)
}


