# ============================================================
# Comparacao entre temperatura de globo da estacao ET,
# temperatura em abrigo (Bulbo Seco) e radiacao solar Simepar.
#
# Ideia principal:
#   delta_globo = globo - Bulbo Seco
#
# Esse delta remove parte do efeito da temperatura do ar e destaca melhor
# o aquecimento radiativo do globo.
#
# Uso:
#   source("analise_globo_radiacao.R")
# ============================================================

pasta_dados <- "dados/entrada"
pasta_resultados <- file.path("resultados", "globo_radiacao")
pasta_tabelas <- file.path(pasta_resultados, "tabelas")
pasta_graficos <- file.path(pasta_resultados, "graficos")
pasta_resumos <- file.path(pasta_resultados, "resumos")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_et <- file.path(pasta_dados, "Dados Sensor ET - Página1.csv")
arquivo_simepar <- file.path(pasta_dados, "dados_simepar_25264916.csv")

saida_pareada <- file.path(pasta_tabelas, "comparacao_globo_radiacao_simepar.csv")
saida_intervalos <- file.path(pasta_tabelas, "intervalos_confianca_globo_radiacao_bootstrap_blocos.csv")
saida_lags <- file.path(pasta_tabelas, "teste_lag_globo_radiacao_interpolado_1min.csv")
saida_lags_sensibilidade <- file.path(pasta_tabelas, "sensibilidade_lag_limite_interpolacao.csv")
saida_lags_fases <- file.path(pasta_tabelas, "teste_lag_globo_radiacao_por_fase_interpolado_1min.csv")
saida_et_interpolado <- file.path(pasta_tabelas, "et_globo_interpolado_1min.csv")
saida_modelo_dinamico <- file.path(pasta_tabelas, "modelo_dinamico_primeira_ordem.csv")
saida_resumo <- file.path(pasta_resumos, "resumo_globo_radiacao.txt")
saida_grafico_series <- file.path(pasta_graficos, "grafico_globo_delta_radiacao_series.png")
saida_grafico_dispersao <- file.path(pasta_graficos, "grafico_delta_globo_radiacao_dispersao.png")
saida_grafico_lags_fases <- file.path(pasta_graficos, "grafico_lag_globo_radiacao_por_fase.png")
saida_grafico_dinamico <- file.path(pasta_graficos, "grafico_modelo_dinamico_primeira_ordem.png")

# ---------- Funcoes auxiliares ----------
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

fmt <- function(x, casas = 3) {
  format(round(x, casas), decimal.mark = ",", nsmall = casas, trim = TRUE)
}

calcular_metricas <- function(dados, nome_variavel) {
  dados <- dados[!is.na(dados[[nome_variavel]]) & !is.na(dados$radiacao_simepar), ]
  if (nrow(dados) < 3) return(NULL)

  modelo <- lm(dados[[nome_variavel]] ~ dados$radiacao_simepar)
  data.frame(
    variavel = nome_variavel,
    n = nrow(dados),
    pearson = cor(dados[[nome_variavel]], dados$radiacao_simepar, method = "pearson"),
    spearman = cor(dados[[nome_variavel]], dados$radiacao_simepar, method = "spearman"),
    r2 = summary(modelo)$r.squared,
    intercepto = coef(modelo)[1],
    inclinacao = coef(modelo)[2],
    stringsAsFactors = FALSE
  )
}

# ---------- Leitura ----------
if (!file.exists(arquivo_et)) stop("Arquivo ET nao encontrado: ", arquivo_et)
if (!file.exists(arquivo_simepar)) stop("Arquivo Simepar nao encontrado: ", arquivo_simepar)

et <- read.csv(arquivo_et, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
simepar <- read.csv(arquivo_simepar, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")

# ---------- Validacao minima ----------
colunas_et <- c("Data e Horário", "globo", "Bulbo Seco")
colunas_simepar <- c("datahora", "6")

faltantes_et <- setdiff(colunas_et, names(et))
faltantes_simepar <- setdiff(colunas_simepar, names(simepar))

if (length(faltantes_et) > 0) stop("Colunas faltando no ET: ", paste(faltantes_et, collapse = ", "))
if (length(faltantes_simepar) > 0) stop("Colunas faltando no Simepar: ", paste(faltantes_simepar, collapse = ", "))

# ---------- Preparacao ----------
et$data_hora_et <- as.POSIXct(et[["Data e Horário"]], format = "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo")
et$horario_alinhado <- alinhar_15min(et$data_hora_et)
et$globo_ET <- converter_numero(et[["globo"]])
et$bulbo_seco_ET <- converter_numero(et[["Bulbo Seco"]])
et$delta_globo_ET <- et$globo_ET - et$bulbo_seco_ET
et$status <- if ("Código de Erro (ok)" %in% names(et)) et[["Código de Erro (ok)"]] else NA

# Remove o -03:00 do texto, mantendo o horario local da serie Simepar.
simepar$horario_alinhado <- as.POSIXct(substr(simepar$datahora, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
simepar$radiacao_simepar <- converter_numero(simepar[["6"]])

# ---------- Deduplicacao e pareamento por horario ----------
# Uma unica observacao ambiental e mantida por slot nominal. As duplicidades
# permanecem no CSV bruto e sao contabilizadas na analise de disponibilidade,
# mas nao recebem peso adicional nas associacoes ambientais.
et2 <- et[order(et$data_hora_et), c("data_hora_et", "horario_alinhado", "status", "globo_ET", "bulbo_seco_ET", "delta_globo_ET")]
et2 <- et2[!duplicated(et2$horario_alinhado), ]
simepar2 <- simepar[, c("horario_alinhado", "radiacao_simepar")]

comparacao <- merge(et2, simepar2, by = "horario_alinhado", all.x = TRUE, sort = TRUE)
validos <- comparacao[!is.na(comparacao$radiacao_simepar), ]
dia <- validos[validos$radiacao_simepar > 0, ]

# ---------- Analise de associacao ----------
# Comparacoes principais, usando somente radiacao positiva para evitar que a noite domine a analise.
metricas_globo <- calcular_metricas(dia, "globo_ET")
metricas_delta <- calcular_metricas(dia, "delta_globo_ET")
source("funcoes_estatisticas.R")
intervalos_delta <- bootstrap_blocos_associacao(
  dia$delta_globo_ET,
  dia$radiacao_simepar,
  tamanho_bloco = 32
)

# ---------- Modelo dinamico simplificado ----------
# Interpreta a radiacao como forcante energetica e testa se ela explica
# a variacao futura do aquecimento relativo do globo, controlando pelo
# estado termico atual do globo. Usa somente pares consecutivos de 15 min.
dinamico <- dia[order(dia$horario_alinhado), ]
dinamico$horario_futuro <- c(dinamico$horario_alinhado[-1], as.POSIXct(NA, origin = "1970-01-01", tz = "America/Sao_Paulo"))
dinamico$delta_globo_futuro <- c(dinamico$delta_globo_ET[-1], NA)
dinamico$dt_min <- as.numeric(difftime(dinamico$horario_futuro, dinamico$horario_alinhado, units = "mins"))
dinamico$d_delta_globo_15min <- dinamico$delta_globo_futuro - dinamico$delta_globo_ET
dinamico <- dinamico[
  !is.na(dinamico$d_delta_globo_15min) &
    !is.na(dinamico$radiacao_simepar) &
    !is.na(dinamico$delta_globo_ET) &
    dinamico$dt_min == 15,
]

modelo_dinamico <- NULL
resumo_modelo_dinamico <- data.frame()
if (nrow(dinamico) >= 3) {
  modelo_dinamico <- lm(d_delta_globo_15min ~ radiacao_simepar + delta_globo_ET, data = dinamico)
  coef_din <- summary(modelo_dinamico)$coefficients
  resumo_modelo_dinamico <- data.frame(
    n = nrow(dinamico),
    r2 = summary(modelo_dinamico)$r.squared,
    r2_ajustado = summary(modelo_dinamico)$adj.r.squared,
    intercepto = coef(modelo_dinamico)["(Intercept)"],
    coef_radiacao = coef(modelo_dinamico)["radiacao_simepar"],
    coef_delta_atual = coef(modelo_dinamico)["delta_globo_ET"],
    p_intercepto = coef_din["(Intercept)", "Pr(>|t|)"],
    p_radiacao = coef_din["radiacao_simepar", "Pr(>|t|)"],
    p_delta_atual = coef_din["delta_globo_ET", "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
}

# ---------- Defasagem com interpolacao limitada e amostra fixa ----------
# A serie ET e interpolada apenas quando os dois pontos observados que cercam
# o instante-alvo estao separados por no maximo 30 minutos. Assim, uma unica
# leitura ausente pode ser interpolada, mas lacunas longas nao sao preenchidas.
max_gap_interpolacao_min <- 30

# A interpolacao parte da mesma serie ambiental deduplicada usada nas demais
# metricas: primeira observacao cronologica de cada slot de 15 minutos.
et_interpolacao <- et[order(et$data_hora_et), ]
et_interpolacao <- et_interpolacao[!duplicated(et_interpolacao$horario_alinhado), ]

preparar_base_interpolacao <- function(nome_variavel) {
  base <- et_interpolacao[
    !is.na(et_interpolacao$data_hora_et) & !is.na(et_interpolacao[[nome_variavel]]),
    c("data_hora_et", nome_variavel)
  ]
  base <- aggregate(
    base[[nome_variavel]],
    by = list(data_hora_et = base$data_hora_et),
    FUN = mean,
    na.rm = TRUE
  )
  names(base)[2] <- nome_variavel
  base[order(base$data_hora_et), ]
}

interpolar_et <- function(tempos_alvo, nome_variavel, max_gap_min = max_gap_interpolacao_min) {
  base <- preparar_base_interpolacao(nome_variavel)
  x <- as.numeric(base$data_hora_et)
  y <- base[[nome_variavel]]
  alvo <- as.numeric(tempos_alvo)
  saida <- rep(NA_real_, length(alvo))

  exatos <- match(alvo, x)
  tem_exato <- !is.na(exatos)
  saida[tem_exato] <- y[exatos[tem_exato]]

  idx_esquerda <- findInterval(alvo, x)
  candidatos <- !tem_exato & idx_esquerda >= 1 & idx_esquerda < length(x)
  if (any(candidatos)) {
    i <- idx_esquerda[candidatos]
    gap_min <- (x[i + 1] - x[i]) / 60
    permitidos <- gap_min <= max_gap_min
    if (any(permitidos)) {
      pos <- which(candidatos)[permitidos]
      ie <- i[permitidos]
      fracao <- (alvo[pos] - x[ie]) / (x[ie + 1] - x[ie])
      saida[pos] <- y[ie] + fracao * (y[ie + 1] - y[ie])
    }
  }
  saida
}

calcular_tabela_lags <- function(lags_min, max_gap_min) {
  matriz_delta <- sapply(lags_min, function(lag) {
    interpolar_et(
      simepar$horario_alinhado + lag * 60,
      "delta_globo_ET",
      max_gap_min = max_gap_min
    )
  })
  if (is.null(dim(matriz_delta))) matriz_delta <- matrix(matriz_delta, ncol = 1)

  comuns <- !is.na(simepar$radiacao_simepar) &
    simepar$radiacao_simepar > 0 &
    apply(!is.na(matriz_delta), 1, all)
  radiacao <- simepar$radiacao_simepar[comuns]

  resultado <- data.frame(
    lag_min = lags_min,
    n = sum(comuns),
    pearson = NA_real_,
    spearman = NA_real_,
    r2 = NA_real_
  )
  for (i in seq_along(lags_min)) {
    resposta <- matriz_delta[comuns, i]
    if (length(resposta) >= 3) {
      modelo <- lm(resposta ~ radiacao)
      resultado$pearson[i] <- cor(resposta, radiacao, method = "pearson")
      resultado$spearman[i] <- cor(resposta, radiacao, method = "spearman")
      resultado$r2[i] <- summary(modelo)$r.squared
    }
  }
  resultado
}

lags_min <- seq(0, 20, by = 1)
lags <- calcular_tabela_lags(lags_min, max_gap_interpolacao_min)
melhor_lag <- lags[which.max(lags$r2), ]

# Sensibilidade do lag otimo ao limite permitido entre pontos observados.
limites_gap_testados <- c(15, 30, 45)
lags_sensibilidade <- do.call(rbind, lapply(limites_gap_testados, function(limite) {
  tab <- calcular_tabela_lags(lags_min, limite)
  melhor <- tab[which.max(tab$r2), ]
  data.frame(
    max_gap_interpolacao_min = limite,
    lag_otimo_min = melhor$lag_min,
    n_comum = melhor$n,
    pearson = melhor$pearson,
    spearman = melhor$spearman,
    r2 = melhor$r2,
    stringsAsFactors = FALSE
  )
}))

# ---------- Defasagem por fase com classificacao independente do lag ----------
# A fase e definida uma unica vez no instante da radiacao t, pela variacao de
# Delta entre t-15 min e t. Os mesmos horarios permanecem em cada fase para
# todos os lags, evitando que a composicao da amostra mude durante a busca.
janela_fase_min <- 15
limiar_fase_delta <- 0.05
lags_fases_min <- seq(0, 60, by = 1)
fases_alvo <- c("aquecimento", "resfriamento", "estavel")

matriz_delta_fases <- sapply(lags_fases_min, function(lag) {
  interpolar_et(
    simepar$horario_alinhado + lag * 60,
    "delta_globo_ET",
    max_gap_min = max_gap_interpolacao_min
  )
})
delta_fase_atual <- interpolar_et(simepar$horario_alinhado, "delta_globo_ET")
delta_fase_anterior <- interpolar_et(
  simepar$horario_alinhado - janela_fase_min * 60,
  "delta_globo_ET"
)
variacao_delta_fase <- delta_fase_atual - delta_fase_anterior

comuns_fases <- !is.na(simepar$radiacao_simepar) &
  simepar$radiacao_simepar > 0 &
  !is.na(variacao_delta_fase) &
  apply(!is.na(matriz_delta_fases), 1, all)

fase_fixa <- ifelse(
  variacao_delta_fase > limiar_fase_delta,
  "aquecimento",
  ifelse(variacao_delta_fase < -limiar_fase_delta, "resfriamento", "estavel")
)

lags_fases <- data.frame()
for (i in seq_along(lags_fases_min)) {
  for (fase_atual in fases_alvo) {
    selecao <- comuns_fases & fase_fixa == fase_atual
    resposta <- matriz_delta_fases[selecao, i]
    radiacao <- simepar$radiacao_simepar[selecao]
    linha <- data.frame(
      fase = fase_atual,
      lag_min = lags_fases_min[i],
      n = length(resposta),
      pearson = NA_real_,
      spearman = NA_real_,
      r2 = NA_real_,
      variacao_delta_media_15min = if (length(resposta) > 0) mean(variacao_delta_fase[selecao]) else NA_real_,
      stringsAsFactors = FALSE
    )
    if (length(resposta) >= 3) {
      modelo_lag_fase <- lm(resposta ~ radiacao)
      linha$pearson <- cor(resposta, radiacao, method = "pearson")
      linha$spearman <- cor(resposta, radiacao, method = "spearman")
      linha$r2 <- summary(modelo_lag_fase)$r.squared
    }
    lags_fases <- rbind(lags_fases, linha)
  }
}

melhores_lags_fases <- do.call(
  rbind,
  lapply(c("aquecimento", "resfriamento"), function(fase_atual) {
    sub <- lags_fases[lags_fases$fase == fase_atual & !is.na(lags_fases$r2), ]
    if (nrow(sub) == 0) return(NULL)
    sub[which.max(sub$r2), ]
  })
)

# Tabela auxiliar em grade de 1 minuto para auditoria da interpolacao.
grade_1min <- seq(
  from = min(et$data_hora_et, na.rm = TRUE),
  to = max(et$data_hora_et, na.rm = TRUE),
  by = 1 * 60
)
et_interpolado_1min <- data.frame(
  data_hora_et_interpolada = grade_1min,
  globo_ET_interpolado = interpolar_et(grade_1min, "globo_ET"),
  bulbo_seco_ET_interpolado = interpolar_et(grade_1min, "bulbo_seco_ET"),
  delta_globo_ET_interpolado = interpolar_et(grade_1min, "delta_globo_ET")
)

# ---------- Saidas ----------
write.csv2(comparacao, saida_pareada, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(intervalos_delta, saida_intervalos, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(lags, saida_lags, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(lags_sensibilidade, saida_lags_sensibilidade, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(lags_fases, saida_lags_fases, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(et_interpolado_1min, saida_et_interpolado, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(resumo_modelo_dinamico, saida_modelo_dinamico, row.names = FALSE, fileEncoding = "UTF-8")

png(saida_grafico_series, width = 1400, height = 800)
par(mar = c(5, 5, 4, 5) + 0.1)
plot(validos$horario_alinhado, validos$delta_globo_ET,
     type = "l", col = "purple", lwd = 2,
     xlab = "Data/hora alinhada", ylab = "Delta globo-ar (°C)",
     main = "Delta globo-ar ET e radiacao solar Simepar")
par(new = TRUE)
plot(validos$horario_alinhado, validos$radiacao_simepar,
     type = "l", col = "orange", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(side = 4)
mtext("Radiacao solar Simepar", side = 4, line = 3)
legend("topright",
       legend = c("Delta globo-ar", "Radiacao Simepar"),
       col = c("purple", "orange"), lwd = 2, bty = "n")
grid()
dev.off()

png(saida_grafico_dispersao, width = 1000, height = 800)
plot(dia$radiacao_simepar, dia$delta_globo_ET,
     pch = 19, col = rgb(0.45, 0, 0.75, 0.45),
     xlab = "Radiacao solar Simepar",
     ylab = "Delta globo-ar ET (°C)",
     main = "Radiacao solar vs delta globo-ar")
abline(lm(delta_globo_ET ~ radiacao_simepar, data = dia), col = "blue", lwd = 2)
grid()
dev.off()

png(saida_grafico_lags_fases, width = 1800, height = 1200, res = 160)
plot(NA,
     xlim = range(lags_fases_min), ylim = range(lags_fases$r2, na.rm = TRUE),
     xlab = "Defasagem testada (min)", ylab = expression(R^2),
     main = "Defasagem do aquecimento relativo por fase termica",
     cex.axis = 1.15, cex.lab = 1.2, cex.main = 1.2)
grid()
for (fase_atual in c("aquecimento", "resfriamento")) {
  sub <- lags_fases[lags_fases$fase == fase_atual, ]
  cor_linha <- if (fase_atual == "aquecimento") "#B2182B" else "#2166AC"
  tipo_linha <- if (fase_atual == "aquecimento") 1 else 2
  simbolo <- if (fase_atual == "aquecimento") 16 else 17
  lines(sub$lag_min, sub$r2, type = "b", pch = simbolo,
        col = cor_linha, lwd = 2.2, lty = tipo_linha)
}
legend("bottomright",
       legend = c("Aquecimento", "Resfriamento"),
       col = c("#B2182B", "#2166AC"), lwd = 2.2,
       lty = c(1, 2), pch = c(16, 17), bty = "n", cex = 1.1)
dev.off()

if (!is.null(modelo_dinamico)) {
  png(saida_grafico_dinamico, width = 1000, height = 800)
  plot(fitted(modelo_dinamico), dinamico$d_delta_globo_15min,
       pch = 19, col = rgb(0.1, 0.35, 0.8, 0.45),
       xlab = "Variacao prevista de Delta globo-ar em 15 min (°C)",
       ylab = "Variacao observada de Delta globo-ar em 15 min (°C)",
       main = "Modelo dinamico de primeira ordem")
  abline(0, 1, col = "red", lwd = 2, lty = 2)
  grid()
  dev.off()
}

# ---------- Resultado no terminal e em TXT ----------
sink(saida_resumo, split = TRUE)

cat("\nResumo estatistico: globo, bulbo seco e radiacao solar\n")
cat("======================================================\n")
cat("Pares validos analisados: ", nrow(validos), "\n", sep = "")
cat("Pares com radiacao > 0:   ", nrow(dia), "\n\n", sep = "")

cat("Estatisticas calculadas com radiacao > 0:\n")
cat("- Globo direto vs radiacao:\n")
cat("  Pearson:  ", fmt(metricas_globo$pearson, 4), "\n", sep = "")
cat("  Spearman: ", fmt(metricas_globo$spearman, 4), "\n", sep = "")
cat("  R2:       ", fmt(metricas_globo$r2, 4), "\n\n", sep = "")

cat("- Delta globo-ar = globo - Bulbo Seco vs radiacao:\n")
cat("  Pearson:  ", fmt(metricas_delta$pearson, 4), "\n", sep = "")
cat("  Spearman: ", fmt(metricas_delta$spearman, 4), "\n", sep = "")
cat("  R2:       ", fmt(metricas_delta$r2, 4), "\n", sep = "")
cat("  Modelo:   delta = ", fmt(metricas_delta$intercepto, 4), " + ", fmt(metricas_delta$inclinacao, 6), " * radiacao\n", sep = "")
cat("  IC95% por bootstrap em blocos de 32 observacoes diurnas: consultar ", saida_intervalos, "\n\n", sep = "")

cat("Modelo dinamico simplificado de primeira ordem:\n")
cat("  Forma: Delta(t+15min) - Delta(t) = a + b * radiacao(t) + c * Delta(t)\n")
if (!is.null(modelo_dinamico)) {
  cat("  Pares consecutivos de 15 min usados: ", nrow(dinamico), "\n", sep = "")
  cat("  R2:          ", fmt(summary(modelo_dinamico)$r.squared, 4), "\n", sep = "")
  cat("  R2 ajustado: ", fmt(summary(modelo_dinamico)$adj.r.squared, 4), "\n", sep = "")
  cat("  Intercepto:  ", fmt(coef(modelo_dinamico)["(Intercept)"], 6), "\n", sep = "")
  cat("  b radiacao:  ", fmt(coef(modelo_dinamico)["radiacao_simepar"], 8), "\n", sep = "")
  cat("  c Delta(t):  ", fmt(coef(modelo_dinamico)["delta_globo_ET"], 6), "\n\n", sep = "")
} else {
  cat("  Pares insuficientes para ajustar o modelo.\n\n")
}

cat("Teste de defasagem do aquecimento relativo com interpolacao limitada:\n")
cat("  Limite entre observacoes usado na analise principal: ", max_gap_interpolacao_min, " min.\n", sep = "")
cat("  Os mesmos horarios sao usados em todos os lags de 0 a 20 min.\n")
print(lags)
cat("Melhor defasagem pelo R2: ", melhor_lag$lag_min, " min\n\n", sep = "")
cat("Sensibilidade ao limite de interpolacao:\n")
print(lags_sensibilidade)
cat("\n")

cat("Teste exploratorio de defasagem separado por fase termica:\n")
cat("  A fase e definida uma unica vez por Delta(t) - Delta(t-", janela_fase_min,
    "min), com limiar de ", fmt(limiar_fase_delta, 2), " °C.\n", sep = "")
cat("  Os mesmos horarios de cada fase sao usados em todos os lags.\n")
cat("  Defasagens testadas por fase: ", min(lags_fases_min), " a ", max(lags_fases_min), " min.\n", sep = "")
if (!is.null(melhores_lags_fases) && nrow(melhores_lags_fases) > 0) {
  cat("  Melhores lags por fase pelo R2:\n")
  for (i in seq_len(nrow(melhores_lags_fases))) {
    linha <- melhores_lags_fases[i, ]
    cat("  - ", linha$fase,
        ": lag ", linha$lag_min, " min; n = ", linha$n,
        "; Pearson = ", fmt(linha$pearson, 4),
        "; Spearman = ", fmt(linha$spearman, 4),
        "; R2 = ", fmt(linha$r2, 4),
        "; variacao media de Delta em 15 min = ", fmt(linha$variacao_delta_media_15min, 4), " °C.\n",
        sep = "")
  }
  cat("  Top 5 lags por fase, segundo R2:\n")
  for (fase_atual in c("aquecimento", "resfriamento")) {
    sub <- lags_fases[lags_fases$fase == fase_atual & !is.na(lags_fases$r2), ]
    sub <- sub[order(sub$r2, decreasing = TRUE), ][seq_len(min(5, nrow(sub))), ]
    cat("  - ", fase_atual, ": ", sep = "")
    cat(paste(paste0(sub$lag_min, " min (R2=", fmt(sub$r2, 4), ")"), collapse = "; "))
    cat(".\n")
  }
}
cat("  A tabela completa por fase foi salva em: ", saida_lags_fases, "\n\n", sep = "")

cat("O que cada estatistica mede:\n")
cat("- Pearson: mede relacao linear entre temperatura/delta e radiacao. Quanto mais perto de 1, mais forte a associacao linear positiva.\n")
cat("- Spearman: mede relacao monotona; e util quando a resposta aumenta com a radiacao, mas nao exatamente em linha reta.\n")
cat("- R2: fracao da variacao explicada pelo modelo linear simples.\n")
cat("- Modelo linear: estima quanto o delta globo-ar aumenta para cada unidade de radiacao.\n")
cat("- Modelo dinamico: testa se a variacao futura do delta globo-ar e explicada pela radiacao atual e pelo delta atual.\n")
cat("- Defasagem: testa se o aquecimento relativo do globo em t + defasagem se associa melhor a radiacao observada em t.\n")
cat("  A interpolacao linear e permitida apenas entre observacoes separadas por no maximo ", max_gap_interpolacao_min, " min.\n", sep = "")
cat("  A analise geral testa 0 a 20 min com amostra fixa; a analise exploratoria por fase testa 0 a 60 min com fase e amostra fixas.\n\n")

cat("Arquivos de saida:\n")
cat("- ", saida_pareada, "\n", sep = "")
cat("- ", saida_intervalos, "\n", sep = "")
cat("- ", saida_lags, "\n", sep = "")
cat("- ", saida_lags_sensibilidade, "\n", sep = "")
cat("- ", saida_lags_fases, "\n", sep = "")
cat("- ", saida_et_interpolado, "\n", sep = "")
cat("- ", saida_modelo_dinamico, "\n", sep = "")
cat("- ", saida_resumo, "\n", sep = "")
cat("- ", saida_grafico_series, "\n", sep = "")
cat("- ", saida_grafico_dispersao, "\n", sep = "")
cat("- ", saida_grafico_lags_fases, "\n", sep = "")
cat("- ", saida_grafico_dinamico, "\n", sep = "")

sink()
