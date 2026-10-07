# ============================================================
# Comparacao entre Bulbo Seco da estacao ET e temperatura Simepar
#
# Uso:
#   source("analise_temperatura.R")
#
# Para atualizar a analise, substitua os CSVs em dados/entrada/
# por arquivos novos com o mesmo formato/nome e rode o script novamente.
# ============================================================

pasta_dados <- "dados/entrada"
pasta_resultados <- file.path("resultados", "temperatura")
pasta_tabelas <- file.path(pasta_resultados, "tabelas")
pasta_graficos <- file.path(pasta_resultados, "graficos")
pasta_resumos <- file.path(pasta_resultados, "resumos")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_et <- file.path(pasta_dados, "Dados Sensor ET - Página1.csv")
arquivo_simepar <- file.path(pasta_dados, "dados_simepar_25264916.csv")

saida_pareada <- file.path(pasta_tabelas, "comparacao_temperatura_bulbo_seco_simepar.csv")
saida_intervalos <- file.path(pasta_tabelas, "intervalos_confianca_temperatura_bootstrap_blocos.csv")
saida_resumo <- file.path(pasta_resumos, "resumo_comparacao_temperatura.txt")
saida_grafico_series <- file.path(pasta_graficos, "grafico_comparacao_temperatura.png")
saida_grafico_diferenca <- file.path(pasta_graficos, "grafico_diferenca_temperatura.png")
saida_grafico_dispersao <- file.path(pasta_graficos, "grafico_dispersao_temperatura.png")

# ---------- Funcoes auxiliares ----------
converter_numero <- function(x) {
  # Converte tanto "14,1" quanto "14.1" para numerico.
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

et <- read.csv(arquivo_et, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
simepar <- read.csv(arquivo_simepar, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")

# ---------- Validacao minima ----------
colunas_et <- c("Data e Horário", "Bulbo Seco")
colunas_simepar <- c("datahora", "temperatura")

faltantes_et <- setdiff(colunas_et, names(et))
faltantes_simepar <- setdiff(colunas_simepar, names(simepar))

if (length(faltantes_et) > 0) stop("Colunas faltando no ET: ", paste(faltantes_et, collapse = ", "))
if (length(faltantes_simepar) > 0) stop("Colunas faltando no Simepar: ", paste(faltantes_simepar, collapse = ", "))

# ---------- Preparacao ----------
et$data_hora_et <- as.POSIXct(et[["Data e Horário"]], format = "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo")
et$horario_alinhado <- alinhar_15min(et$data_hora_et)
et$bulbo_seco_ET <- converter_numero(et[["Bulbo Seco"]])
et$status <- if ("Código de Erro (ok)" %in% names(et)) et[["Código de Erro (ok)"]] else NA

# Remove o -03:00 do texto, mantendo o horario local da serie Simepar.
simepar$horario_alinhado <- as.POSIXct(substr(simepar$datahora, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
simepar$temperatura_simepar <- converter_numero(simepar$temperatura)

# ---------- Deduplicacao e pareamento por horario ----------
# O CSV pode conter mais de uma linha para o mesmo slot nominal. Como essas
# linhas representam a mesma janela ambiental, apenas a primeira linha
# cronologica de cada slot entra nas metricas; as duplicidades continuam
# contabilizadas separadamente em analise_transmissao_lora.R.
et2 <- et[order(et$data_hora_et), c("data_hora_et", "horario_alinhado", "status", "bulbo_seco_ET")]
et2 <- et2[!duplicated(et2$horario_alinhado), ]
simepar2 <- simepar[, c("horario_alinhado", "temperatura_simepar")]

comparacao <- merge(et2, simepar2, by = "horario_alinhado", all.x = TRUE, sort = TRUE)
comparacao$diferenca <- comparacao$bulbo_seco_ET - comparacao$temperatura_simepar
comparacao$abs_diferenca <- abs(comparacao$diferenca)

validos <- comparacao[!is.na(comparacao$bulbo_seco_ET) & !is.na(comparacao$temperatura_simepar), ]

# ---------- Medidas principais e incerteza temporal ----------
source("funcoes_estatisticas.R")
metricas <- metricas_pareadas(validos$bulbo_seco_ET, validos$temperatura_simepar)
intervalos <- bootstrap_blocos_pareados(validos$bulbo_seco_ET, validos$temperatura_simepar)
cor_pearson <- metricas$pearson
r2 <- metricas$r2
vies <- metricas$vies
mae <- metricas$mae
rmse <- metricas$rmse
sd_diferenca <- sd(validos$diferenca)
limite_concordancia_inferior <- metricas$limite_concordancia_inferior
limite_concordancia_superior <- metricas$limite_concordancia_superior

# ---------- Saidas ----------
write.csv2(comparacao, saida_pareada, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(intervalos, saida_intervalos, row.names = FALSE, fileEncoding = "UTF-8")

png(saida_grafico_series, width = 1400, height = 800)
plot(validos$horario_alinhado, validos$bulbo_seco_ET,
     type = "l", col = "blue", lwd = 2,
     xlab = "Data/hora alinhada", ylab = "Temperatura (°C)",
     main = "Bulbo Seco ET vs temperatura Simepar")
lines(validos$horario_alinhado, validos$temperatura_simepar, col = "red", lwd = 2)
legend("topright", legend = c("Bulbo Seco ET", "Temperatura Simepar"),
       col = c("blue", "red"), lwd = 2, bty = "n")
grid()
dev.off()

png(saida_grafico_diferenca, width = 1400, height = 800)
plot(validos$horario_alinhado, validos$diferenca,
     type = "h", col = "darkgreen", lwd = 2,
     xlab = "Data/hora alinhada", ylab = "Bulbo Seco ET - Simepar (°C)",
     main = "Diferenca entre Bulbo Seco ET e temperatura Simepar")
abline(h = 0, col = "black", lwd = 2)
abline(h = c(-2, 2), col = "orange", lty = 2)
grid()
dev.off()

png(saida_grafico_dispersao, width = 1000, height = 800)
plot(validos$temperatura_simepar, validos$bulbo_seco_ET,
     pch = 19, col = rgb(0, 0, 1, 0.45),
     xlab = "Temperatura Simepar (°C)",
     ylab = "Bulbo Seco ET (°C)",
     main = "Relacao entre temperaturas ET e Simepar")
abline(lm(bulbo_seco_ET ~ temperatura_simepar, data = validos), col = "red", lwd = 2)
abline(a = 0, b = 1, col = "gray40", lwd = 2, lty = 2)
legend("topleft", legend = c("Regressao", "Linha 1:1"), col = c("red", "gray40"), lwd = 2, lty = c(1, 2), bty = "n")
grid()
dev.off()

# ---------- Resultado no terminal e em TXT ----------
sink(saida_resumo, split = TRUE)

cat("\nResumo estatistico: Bulbo Seco ET vs temperatura Simepar\n")
cat("========================================================\n")
cat("Pares validos analisados: ", nrow(validos), "\n\n", sep = "")

cat("Estatisticas calculadas:\n")
cat("- Pearson:                 ", fmt(cor_pearson, 4), "\n", sep = "")
cat("- R2:                      ", fmt(r2, 4), "\n", sep = "")
cat("- Diferenca media:         ", fmt(vies, 2), " °C  (Bulbo Seco ET - Simepar)\n", sep = "")
cat("- MAE:                     ", fmt(mae, 2), " °C\n", sep = "")
cat("- RMSE:                    ", fmt(rmse, 2), " °C\n", sep = "")
cat("- Desvio padrao diferenca: ", fmt(sd_diferenca, 2), " °C\n", sep = "")
cat("- Limites de concordancia:  ", fmt(limite_concordancia_inferior, 2), " a ", fmt(limite_concordancia_superior, 2), " °C\n", sep = "")
cat("- IC95% por bootstrap em blocos de 24 h: consultar ", saida_intervalos, "\n\n", sep = "")

cat("O que cada estatistica mede:\n")
cat("- Pearson: mede o quanto as duas series de temperatura variam juntas de forma linear.\n")
cat("- R2: e o quadrado de Pearson nesta comparacao; indica a fracao da variacao linear explicada pela outra serie.\n")
cat("- Diferenca media: mostra o vies medio. Valor positivo indica Bulbo Seco ET maior que Simepar em media.\n")
cat("- MAE: erro medio absoluto em °C; mostra o tamanho medio da diferenca sem considerar o sinal.\n")
cat("- RMSE: erro quadratico medio em °C; penaliza mais diferencas grandes que o MAE.\n")
cat("- Desvio padrao da diferenca: mostra a variabilidade das diferencas ao redor do vies medio.\n\n")

cat("Arquivos de saida:\n")
cat("- ", saida_pareada, "\n", sep = "")
cat("- ", saida_intervalos, "\n", sep = "")
cat("- ", saida_resumo, "\n", sep = "")
cat("- ", saida_grafico_series, "\n", sep = "")
cat("- ", saida_grafico_diferenca, "\n", sep = "")
cat("- ", saida_grafico_dispersao, "\n", sep = "")

sink()
