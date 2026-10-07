# ============================================================
# Faixas ambientais efetivamente observadas na campanha
# ============================================================

pasta_tabelas <- file.path("resultados", "faixas_campanha", "tabelas")
pasta_resumos <- file.path("resultados", "faixas_campanha", "resumos")
dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_et <- file.path("dados", "entrada", "Dados Sensor ET - Página1.csv")
arquivo_simepar <- file.path("dados", "entrada", "dados_simepar_25264916.csv")
saida_tabela <- file.path(pasta_tabelas, "faixas_ambientais_campanha.csv")
saida_resumo <- file.path(pasta_resumos, "resumo_faixas_ambientais.txt")

converter_numero <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  tem_virgula <- grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub("\\.", "", x[tem_virgula])
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)
  as.numeric(x)
}

alinhar_15min <- function(data_hora) {
  as.POSIXct(
    floor(as.numeric(data_hora) / 900) * 900,
    origin = "1970-01-01",
    tz = "America/Sao_Paulo"
  )
}

resumir <- function(nome, unidade, x) {
  x <- x[is.finite(x)]
  data.frame(
    variavel = nome,
    unidade = unidade,
    n = length(x),
    minimo = min(x),
    q05 = unname(quantile(x, 0.05)),
    mediana = median(x),
    q95 = unname(quantile(x, 0.95)),
    maximo = max(x),
    stringsAsFactors = FALSE
  )
}

et <- read.csv(arquivo_et, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
simepar <- read.csv(arquivo_simepar, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")

et$data_hora_et <- as.POSIXct(et[["Data e Horário"]], format = "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo")
et$horario_alinhado <- alinhar_15min(et$data_hora_et)
et <- et[order(et$data_hora_et), ]
et <- et[!duplicated(et$horario_alinhado), ]

et$temperatura_ar <- converter_numero(et[["Bulbo Seco"]])
et$umidade_relativa <- converter_numero(et[["RU"]])
et$temperatura_globo <- converter_numero(et[["globo"]])
et$indice_termico_estimado <- converter_numero(et[["IBUTG"]])
et$aquecimento_relativo <- et$temperatura_globo - et$temperatura_ar

simepar$horario_alinhado <- as.POSIXct(substr(simepar$datahora, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
simepar$radiacao_solar <- converter_numero(simepar[["6"]])

dados <- merge(
  et[, c("horario_alinhado", "temperatura_ar", "umidade_relativa", "temperatura_globo", "indice_termico_estimado", "aquecimento_relativo")],
  simepar[, c("horario_alinhado", "radiacao_solar")],
  by = "horario_alinhado",
  all.x = TRUE,
  sort = TRUE
)

faixas <- do.call(rbind, list(
  resumir("temperatura_ar_ET", "celsius", dados$temperatura_ar),
  resumir("umidade_relativa_ET", "percentual", dados$umidade_relativa),
  resumir("temperatura_globo_ET", "celsius", dados$temperatura_globo),
  resumir("aquecimento_relativo_globo", "celsius", dados$aquecimento_relativo),
  resumir("indice_termico_estimado", "celsius", dados$indice_termico_estimado),
  resumir("radiacao_solar_simepar", "unidade_do_arquivo_fonte", dados$radiacao_solar)
))

write.csv2(faixas, saida_tabela, row.names = FALSE, fileEncoding = "UTF-8")

sink(saida_resumo, split = TRUE)
cat("Faixas ambientais da campanha apos deduplicacao por slot\n")
cat("=======================================================\n")
cat("Periodo: ", format(min(dados$horario_alinhado)), " a ", format(max(dados$horario_alinhado)), "\n", sep = "")
print(faixas, row.names = FALSE)
cat("\nA coluna radiacao preserva a unidade fornecida no arquivo Simepar; a documentacao do produto deve ser citada no artigo.\n")
sink()
