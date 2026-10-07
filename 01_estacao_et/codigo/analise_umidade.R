# ============================================================
# Comparacao entre RU da estacao ET e umidade do Simepar
#
# Uso:
#   source("analise_umidade.R")
#
# Para atualizar a analise, substitua os dois CSVs abaixo por arquivos
# novos com o mesmo formato/nome e rode o script novamente.
# ============================================================

pasta_dados <- "dados/entrada"
pasta_resultados <- file.path("resultados", "umidade")
pasta_tabelas <- file.path(pasta_resultados, "tabelas")
pasta_graficos <- file.path(pasta_resultados, "graficos")
pasta_resumos <- file.path(pasta_resultados, "resumos")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_et <- file.path(pasta_dados, "Dados Sensor ET - Página1.csv")
arquivo_simepar <- file.path(pasta_dados, "dados_simepar_25264916.csv")

saida_pareada <- file.path(pasta_tabelas, "comparacao_umidade_RU_simepar.csv")
saida_intervalos <- file.path(pasta_tabelas, "intervalos_confianca_umidade_bootstrap_blocos.csv")
saida_resumo <- file.path(pasta_resumos, "resumo_comparacao_umidade.txt")
saida_grafico_series <- file.path(pasta_graficos, "grafico_comparacao_umidade.png")
saida_grafico_diferenca <- file.path(pasta_graficos, "grafico_diferenca_umidade.png")

# ---------- Funcoes auxiliares ----------
converter_numero <- function(x) {
  # Converte tanto "63,6" quanto "63.6" para numerico.
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  tem_virgula <- grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub("\\.", "", x[tem_virgula])
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)
  as.numeric(x)
}

alinhar_15min <- function(data_hora) {
  # Arredonda para baixo na grade de 15 minutos.
  as.POSIXct(
    floor(as.numeric(data_hora) / 900) * 900,
    origin = "1970-01-01",
    tz = "America/Sao_Paulo"
  )
}

fmt <- function(x, casas = 2) {
  format(round(x, casas), decimal.mark = ",", nsmall = casas, trim = TRUE)
}

# ---------- Leitura ----------
if (!file.exists(arquivo_et)) stop("Arquivo ET nao encontrado: ", arquivo_et)
if (!file.exists(arquivo_simepar)) stop("Arquivo Simepar nao encontrado: ", arquivo_simepar)

# O arquivo ET e CSV separado por virgula, mas os decimais usam virgula entre aspas.
et <- read.csv(arquivo_et, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")

# O Simepar e CSV separado por virgula e usa ponto decimal.
simepar <- read.csv(arquivo_simepar, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")

# ---------- Preparacao ----------
et$data_hora_et <- as.POSIXct(et[["Data e Horário"]], format = "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo")
et$horario_alinhado <- alinhar_15min(et$data_hora_et)
et$RU_ET <- converter_numero(et[["RU"]])
et$status <- if ("Código de Erro (ok)" %in% names(et)) et[["Código de Erro (ok)"]] else NA

# Remove o -03:00 do texto, mantendo o horario local da serie Simepar.
simepar$horario_alinhado <- as.POSIXct(substr(simepar$datahora, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
simepar$umidade_simepar <- converter_numero(simepar$umidade)

# ---------- Deduplicacao e pareamento por horario ----------
# O CSV pode conter mais de uma linha para o mesmo slot nominal. Como essas
# linhas representam a mesma janela ambiental, apenas a primeira linha
# cronologica de cada slot entra nas metricas; as duplicidades continuam
# contabilizadas separadamente em analise_transmissao_lora.R.
et2 <- et[order(et$data_hora_et), c("data_hora_et", "horario_alinhado", "status", "RU_ET")]
et2 <- et2[!duplicated(et2$horario_alinhado), ]
simepar2 <- simepar[, c("horario_alinhado", "umidade_simepar")]

comparacao <- merge(et2, simepar2, by = "horario_alinhado", all.x = TRUE, sort = TRUE)
comparacao$diferenca <- comparacao$RU_ET - comparacao$umidade_simepar
comparacao$abs_diferenca <- abs(comparacao$diferenca)

validos <- comparacao[!is.na(comparacao$RU_ET) & !is.na(comparacao$umidade_simepar), ]

# ---------- Medidas principais e incerteza temporal ----------
source("funcoes_estatisticas.R")
metricas <- metricas_pareadas(validos$RU_ET, validos$umidade_simepar)
intervalos <- bootstrap_blocos_pareados(validos$RU_ET, validos$umidade_simepar)
cor_pearson <- metricas$pearson
r2 <- metricas$r2
mae <- metricas$mae
rmse <- metricas$rmse
vies <- metricas$vies
limite_concordancia_inferior <- metricas$limite_concordancia_inferior
limite_concordancia_superior <- metricas$limite_concordancia_superior

# ---------- Saidas ----------
write.csv2(comparacao, saida_pareada, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(intervalos, saida_intervalos, row.names = FALSE, fileEncoding = "UTF-8")

png(saida_grafico_series, width = 1400, height = 800)
plot(validos$horario_alinhado, validos$RU_ET,
     type = "l", col = "blue", lwd = 2,
     xlab = "Data/hora alinhada", ylab = "Umidade relativa (%)",
     main = "RU estacao ET vs umidade Simepar")
lines(validos$horario_alinhado, validos$umidade_simepar, col = "red", lwd = 2)
legend("topright", legend = c("RU estacao ET", "Umidade Simepar"),
       col = c("blue", "red"), lwd = 2, bty = "n")
grid()
dev.off()

png(saida_grafico_diferenca, width = 1400, height = 800)
plot(validos$horario_alinhado, validos$diferenca,
     type = "h", col = "darkgreen", lwd = 2,
     xlab = "Data/hora alinhada", ylab = "RU ET - Simepar (pontos percentuais)",
     main = "Diferenca entre RU ET e umidade Simepar")
abline(h = 0, col = "black", lwd = 2)
abline(h = c(-5, 5), col = "orange", lty = 2)
grid()
dev.off()

# ---------- Resultado no terminal e em TXT ----------
sink(saida_resumo, split = TRUE)

cat("\nResumo estatistico: RU ET vs umidade Simepar\n")
cat("============================================\n")
cat("Pares validos analisados: ", nrow(validos), "\n\n", sep = "")

cat("Estatisticas calculadas:\n")
cat("- Pearson:          ", fmt(cor_pearson, 4), "\n", sep = "")
cat("- R2:               ", fmt(r2, 4), "\n", sep = "")
cat("- Diferenca media:  ", fmt(vies, 2), " p.p.  (RU ET - Simepar)\n", sep = "")
cat("- MAE:              ", fmt(mae, 2), " p.p.\n", sep = "")
cat("- RMSE:             ", fmt(rmse, 2), " p.p.\n", sep = "")
cat("- Limites de concordancia: ", fmt(limite_concordancia_inferior, 2), " a ", fmt(limite_concordancia_superior, 2), " p.p.\n", sep = "")
cat("- IC95% por bootstrap em blocos de 24 h: consultar ", saida_intervalos, "\n\n", sep = "")

cat("O que cada estatistica mede:\n")
cat("- Pearson: mede o quanto as duas series variam juntas de forma linear. Quanto mais perto de 1, mais parecidas sao as variacoes.\n")
cat("- R2: e o quadrado de Pearson nesta comparacao; indica a fracao da variacao linear explicada pela outra serie.\n")
cat("- Diferenca media: mostra o vies medio entre as series. Valor negativo indica RU ET menor que Simepar em media.\n")
cat("- MAE: erro medio absoluto em pontos percentuais; mostra o tamanho medio da diferenca sem considerar o sinal.\n")
cat("- RMSE: erro quadratico medio; penaliza mais os erros grandes que o MAE.\n\n")

cat("Arquivos de saida:\n")
cat("- ", saida_pareada, "\n", sep = "")
cat("- ", saida_intervalos, "\n", sep = "")
cat("- ", saida_resumo, "\n", sep = "")
cat("- ", saida_grafico_series, "\n", sep = "")
cat("- ", saida_grafico_diferenca, "\n", sep = "")

sink()
