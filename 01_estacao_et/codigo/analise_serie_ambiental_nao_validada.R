# ============================================================
# Registro gráfico simples do sensor ambiental instalado no poste
#
# Esta rotina não realiza validação, calibração ou comparação.
# Os registros não são adequados para verificação de desempenho,
# pois o sensor não estava sob sol direto e a ventilação era inadequada.
# ============================================================

pasta_dados <- file.path("dados", "entrada_serie_ambiental")
pasta_resultados <- file.path("resultados", "serie_ambiental_nao_validada")
pasta_graficos <- file.path(pasta_resultados, "graficos")
pasta_resumos <- file.path(pasta_resultados, "resumos")

dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_entrada <- file.path(pasta_dados, "Dados Sensor ET - Página1.csv")
saida_grafico <- file.path(pasta_graficos, "serie_temporal_sensor_ambiental_poste.png")
saida_resumo <- file.path(pasta_resumos, "periodo_e_escopo.txt")

converter_numero <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  tem_virgula <- grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub("\\.", "", x[tem_virgula])
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

if (!file.exists(arquivo_entrada)) {
  stop("Arquivo de entrada não encontrado: ", arquivo_entrada)
}

dados <- read.csv(
  arquivo_entrada,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

colunas_necessarias <- c(
  "Data e Horário",
  "RU",
  "Bulbo Umido (stull)",
  "globo",
  "Bulbo Seco",
  "IBUTG"
)

faltantes <- setdiff(colunas_necessarias, names(dados))
if (length(faltantes) > 0) {
  stop("Colunas ausentes: ", paste(faltantes, collapse = ", "))
}

dados$data_hora <- as.POSIXct(
  dados[["Data e Horário"]],
  format = "%d/%m/%Y %H:%M:%S",
  tz = "America/Sao_Paulo"
)
dados$ru <- converter_numero(dados[["RU"]])
dados$bulbo_umido_est <- converter_numero(dados[["Bulbo Umido (stull)"]])
dados$globo <- converter_numero(dados[["globo"]])
dados$bulbo_seco <- converter_numero(dados[["Bulbo Seco"]])
dados$ibutg <- converter_numero(dados[["IBUTG"]])

dados <- dados[!is.na(dados$data_hora), ]
dados <- dados[order(dados$data_hora), ]

if (nrow(dados) == 0) {
  stop("Nenhum registro com data/hora válida foi encontrado.")
}

inicio <- min(dados$data_hora)
fim <- max(dados$data_hora)
inicio_eixo <- as.POSIXct(format(inicio, "%Y-%m-%d 00:00:00"), tz = "America/Sao_Paulo") + 86400
marcas_tempo <- seq(inicio_eixo, fim, by = "2 days")

cor_azul <- "#4285F4"
cor_vermelho <- "#EA4335"
cor_amarelo <- "#F9AB00"
cor_verde <- "#34A853"
cor_grade <- "#D9D9D9"
cor_eixo <- "#666666"

adicionar_eixo_tempo <- function(mostrar_rotulos = FALSE) {
  rotulos <- if (mostrar_rotulos) format(marcas_tempo, "%d/%m") else FALSE
  axis(
    1,
    at = marcas_tempo,
    labels = rotulos,
    las = 2,
    col = cor_eixo,
    col.ticks = cor_eixo,
    cex.axis = 0.78
  )
}

preparar_painel <- function(titulo, y_lim, y_rotulo, mostrar_rotulos = FALSE) {
  plot(
    NA,
    xlim = c(inicio, fim),
    ylim = y_lim,
    xlab = "",
    ylab = y_rotulo,
    axes = FALSE,
    main = ""
  )
  abline(v = marcas_tempo, col = cor_grade, lwd = 1)
  abline(h = pretty(y_lim, n = 5), col = cor_grade, lwd = 1)
  axis(2, las = 1, col = cor_eixo, col.ticks = cor_eixo, cex.axis = 0.82)
  adicionar_eixo_tempo(mostrar_rotulos)
  box(bty = "l", col = cor_eixo)
  mtext(titulo, side = 3, line = 0.45, adj = 0, font = 2, cex = 1.05)
}

limite_temperatura <- max(
  c(dados$globo, dados$bulbo_seco, dados$bulbo_umido_est),
  na.rm = TRUE
)
limite_temperatura <- max(5, ceiling(limite_temperatura / 5) * 5)

limite_ibutg <- max(dados$ibutg, na.rm = TRUE)
limite_ibutg <- max(5, ceiling(limite_ibutg / 5) * 5)

png(
  saida_grafico,
  width = 2400,
  height = 2100,
  res = 190,
  bg = "white",
  type = "cairo-png"
)

layout(matrix(1:3, ncol = 1), heights = c(1, 1.12, 1))
par(
  family = "sans",
  mar = c(2.0, 5.0, 3.0, 1.2),
  oma = c(2.8, 0.5, 1.0, 0.5),
  mgp = c(2.8, 0.8, 0),
  tcl = -0.25
)

preparar_painel("Umidade relativa × data/hora", c(0, 100), "Umidade relativa (%)")
lines(dados$data_hora, dados$ru, col = cor_azul, lwd = 1.15)

preparar_painel("Temperaturas × data/hora", c(0, limite_temperatura), "Temperatura (°C)")
lines(dados$data_hora, dados$globo, col = cor_vermelho, lwd = 1.05)
lines(dados$data_hora, dados$bulbo_seco, col = cor_amarelo, lwd = 1.05)
lines(dados$data_hora, dados$bulbo_umido_est, col = cor_verde, lwd = 1.05)
legend(
  "top",
  legend = c("Globo", "Bulbo seco", "Bulbo úmido estimado (Stull)"),
  col = c(cor_vermelho, cor_amarelo, cor_verde),
  lwd = 2,
  horiz = TRUE,
  bty = "n",
  cex = 0.82,
  inset = c(0, 0.01)
)

par(mar = c(5.0, 5.0, 3.0, 1.2))
preparar_painel("IBUTG registrado × data/hora", c(0, limite_ibutg), "IBUTG (°C)", TRUE)
lines(dados$data_hora, dados$ibutg, col = cor_azul, lwd = 1.15)
mtext("Tempo (data local)", side = 1, outer = TRUE, line = 1.4, cex = 0.95)

dev.off()

resumo <- c(
  "REGISTRO GRÁFICO DO SENSOR AMBIENTAL DO POSTE",
  "",
  paste0("Arquivo de entrada: ", arquivo_entrada),
  paste0("Registros com data/hora válida: ", nrow(dados)),
  paste0("Início: ", format(inicio, "%d/%m/%Y %H:%M:%S")),
  paste0("Fim: ", format(fim, "%d/%m/%Y %H:%M:%S")),
  "",
  "Escopo:",
  "- Figura exclusivamente descritiva das séries brutas.",
  "- Nenhuma validação, calibração ou comparação foi realizada.",
  "- Os dados não são válidos para verificação de desempenho porque o sensor",
  "  não estava sob sol direto e as condições de ventilação eram inadequadas.",
  "",
  paste0("Gráfico: ", saida_grafico)
)

writeLines(resumo, saida_resumo, useBytes = TRUE)
cat(paste(resumo, collapse = "\n"), "\n")
