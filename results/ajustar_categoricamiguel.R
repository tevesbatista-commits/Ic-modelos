ajustar_categorica <- function(dados_base, variavel_tirar, modelo) {
  library(broom.mixed)

  dados_substituto <- dados_base
  modelo_novo      <- modelo

  # primeiro nível alfabético — será o intercepto médio consolidado
  cazela <- dados_base %>%
    select(variavel_tirar) %>%
    distinct() %>%
    pull(variavel_tirar) %>%
    sort() %>%
    .[1]

  while (TRUE) {

    tabela_estatisticas <- tidy(modelo_novo, effects = "fixed") %>%
      arrange(desc(p.value)) %>%
      filter(p.value >= 0.05, str_starts(term, variavel_tirar))

    # condição de saída: não há mais níveis não significativos
    if (nrow(tabela_estatisticas) == 0) {
      return(list(modelo_atualizado = modelo_novo,
                  dados_atualizado  = dados_substituto))
    }

    # ambiente com maior p-value não significativo
    valor_tirar <- tabela_estatisticas %>%
      pull(term) %>%
      .[1] %>%
      str_remove(variavel_tirar)

    cat("Fundindo ambiente:", valor_tirar, "→", cazela, "\n")

    # funde o ambiente não significativo no intercepto de referência
    dados_substituto <- dados_substituto %>%
      mutate(
        !!variavel_tirar := if_else(
          .data[[variavel_tirar]] == valor_tirar,
          cazela,
          .data[[variavel_tirar]]
        )
      )

    # correção do bug: atualiza modelo_novo, não modelo original
    modelo_novo <- update(modelo_novo, data = dados_substituto)
  }
}









