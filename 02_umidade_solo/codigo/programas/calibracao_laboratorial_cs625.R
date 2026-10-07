# Calibração laboratorial exclusiva do CS625
#
# Resposta: VWC independente pela pesagem integral da coluna, relativa ao ponto 1.
# Preditor: período bruto do CS625 em microssegundos.
# Comparação: curva específica do ensaio versus equação padrão Campbell.
#
# Execute da raiz do projeto:
# Rscript programas/calibracao_laboratorial_cs625.R

options(stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 1. CAMINHOS E FUNÇÕES
# -----------------------------------------------------------------------------
args <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("^--file=", "", args[grep("^--file=", args)])
base_dir <- if (length(arquivo_script) == 1) {
  normalizePath(file.path(dirname(arquivo_script), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
entrada <- file.path(base_dir, "dados", "calibracao_laboratorial", "leituras_laboratorio_cs625.csv")
saida <- file.path(base_dir, "resultados", "calibracao_laboratorial_cs625")
tabelas <- file.path(saida, "tabelas")
graficos <- file.path(saida, "graficos")
resumos <- file.path(saida, "resumos")

ler_numero <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  tem_virgula <- !is.na(x) & grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub(".", "", x[tem_virgula], fixed = TRUE)
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae <- function(obs, pred) mean(abs(obs - pred))
bias <- function(obs, pred) mean(pred - obs)
eficiencia <- function(obs, pred) 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

campbell <- function(periodo_us) {
  -0.0663 - 0.0063 * periodo_us + 0.0007 * periodo_us^2
}

loocv <- function(formula, dados) {
  pred <- rep(NA_real_, nrow(dados))
  for (i in seq_len(nrow(dados))) {
    ajuste <- lm(formula, data = dados[-i, , drop = FALSE])
    pred[i] <- predict(ajuste, newdata = dados[i, , drop = FALSE])
  }
  pred
}

fmt <- function(x, n = 6) formatC(x, digits = n, format = "f")

# -----------------------------------------------------------------------------
# 2. LEITURA E VALIDAÇÃO
# -----------------------------------------------------------------------------
obrigatorias <- c(
  "ensaio_id", "ordem_ponto", "vwc_alvo_pct", "agua_adicionada_no_ponto_g",
  "massa_recipiente_com_solo_antes_g", "massa_recipiente_com_solo_depois_g",
  "massa_recipiente_vazio_g", "massa_solo_seco_total_g", "metodo_massa_seca",
  "diametro_interno_cm", "altura_solo_cm", "volume_solo_cm3",
  "tempo_equilibrio_min", "temperatura_solo_c", "data_hora_leitura", "ordem_medicao",
  "sensor_id", "variavel_leitura", "unidade_leitura", "repeticao", "leitura_sensor",
  "observacoes_ponto", "observacoes_leitura"
)
if (!file.exists(entrada)) stop("Arquivo não encontrado: ", entrada)
brutas <- read.csv2(
  entrada, colClasses = "character", check.names = FALSE,
  na.strings = c("", "NA"), quote = '"', fileEncoding = "UTF-8-BOM"
)
faltantes <- setdiff(obrigatorias, names(brutas))
if (length(faltantes)) stop("CSV incompatível; colunas ausentes: ", paste(faltantes, collapse = ", "))
if (!nrow(brutas)) stop("O CSV do CS625 está vazio.")

numericas <- c(
  "ordem_ponto", "vwc_alvo_pct", "agua_adicionada_no_ponto_g",
  "massa_recipiente_com_solo_antes_g", "massa_recipiente_com_solo_depois_g",
  "massa_recipiente_vazio_g", "massa_solo_seco_total_g", "diametro_interno_cm",
  "altura_solo_cm", "volume_solo_cm3", "tempo_equilibrio_min",
  "temperatura_solo_c", "ordem_medicao", "repeticao", "leitura_sensor"
)
for (nm in numericas) brutas[[nm]] <- ler_numero(brutas[[nm]])
for (nm in c("ensaio_id", "metodo_massa_seca", "sensor_id", "variavel_leitura",
             "unidade_leitura", "observacoes_ponto", "observacoes_leitura")) {
  brutas[[nm]] <- trimws(ifelse(is.na(brutas[[nm]]), "", brutas[[nm]]))
}
brutas$unidade_leitura[tolower(brutas$unidade_leitura) %in% c("µs", "μs", "usec")] <- "us"

if (length(unique(brutas$ensaio_id)) != 1 || !nzchar(brutas$ensaio_id[1])) {
  stop("Use um único ensaio_id não vazio.")
}
metadados <- unique(brutas[, c("sensor_id", "variavel_leitura", "unidade_leitura")])
if (nrow(metadados) != 1 || metadados$variavel_leitura != "periodo_us" || metadados$unidade_leitura != "us") {
  stop("O arquivo deve conter somente um CS625, como periodo_us/us.")
}
if (any(!is.finite(brutas$leitura_sensor)) || any(brutas$leitura_sensor <= 0)) {
  stop("Há período inválido; todas as leituras devem ser positivas e estar em µs.")
}
if (any(brutas$leitura_sensor < 10 | brutas$leitura_sensor > 50)) {
  warning("Há períodos fora da faixa ampla de conferência 10–50 µs.")
}
if (any(!is.finite(brutas$ordem_ponto)) || any(brutas$ordem_ponto != floor(brutas$ordem_ponto))) {
  stop("ordem_ponto deve conter inteiros.")
}

# -----------------------------------------------------------------------------
# 3. REFERÊNCIA GRAVIMÉTRICA E MÉDIAS POR NÍVEL
# -----------------------------------------------------------------------------
campos_num <- c(
  "vwc_alvo_pct", "agua_adicionada_no_ponto_g", "massa_recipiente_com_solo_antes_g",
  "massa_recipiente_com_solo_depois_g", "massa_recipiente_vazio_g",
  "massa_solo_seco_total_g", "diametro_interno_cm", "altura_solo_cm",
  "volume_solo_cm3", "tempo_equilibrio_min", "temperatura_solo_c"
)
campos_txt <- c("metodo_massa_seca", "observacoes_ponto")
extrair_unico <- function(d, nm) {
  v <- unique(d[[nm]])
  if (length(v) != 1) stop("Ponto ", d$ordem_ponto[1], ": valores diferentes para ", nm, ".")
  v
}
pontos <- do.call(rbind, lapply(split(brutas, brutas$ordem_ponto), function(d) {
  vals <- c(lapply(campos_num, function(nm) extrair_unico(d, nm)),
            lapply(campos_txt, function(nm) extrair_unico(d, nm)))
  names(vals) <- c(campos_num, campos_txt)
  as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
}))
pontos$ordem_ponto <- as.numeric(rownames(pontos))
rownames(pontos) <- NULL
pontos <- pontos[order(pontos$ordem_ponto), c("ordem_ponto", campos_num, campos_txt)]
for (nm in campos_num) pontos[[nm]] <- as.numeric(pontos[[nm]])

if (!all(pontos$ordem_ponto == seq_len(nrow(pontos)))) stop("Os pontos devem ser sequenciais, sem saltos.")
obrigatorios <- setdiff(campos_num, "temperatura_solo_c")
if (any(!sapply(pontos[, obrigatorios, drop = FALSE], function(x) all(is.finite(x))))) {
  stop("Há pesagem, geometria ou valor obrigatório ausente nos pontos usados.")
}
if (nrow(pontos) < 5) stop("São necessários ao menos cinco níveis para LOOCV por nível.")
if (abs(pontos$agua_adicionada_no_ponto_g[1]) > 0.001) stop("O ponto 1 deve ter água adicionada igual a zero.")

constantes <- c("massa_recipiente_vazio_g", "massa_solo_seco_total_g", "diametro_interno_cm",
               "altura_solo_cm", "volume_solo_cm3", "metodo_massa_seca")
if (any(sapply(pontos[, constantes, drop = FALSE], function(x) length(unique(x)) != 1))) {
  stop("Tara, massa seca, geometria e método devem permanecer constantes.")
}
volume_geometrico <- pi * (pontos$diametro_interno_cm / 2)^2 * pontos$altura_solo_cm
if (any(abs(pontos$volume_solo_cm3 - volume_geometrico) > pmax(0.5, 0.001 * volume_geometrico))) {
  stop("O volume informado não corresponde à geometria cilíndrica.")
}

massa_zero <- pontos$massa_recipiente_com_solo_depois_g[1]
pontos$massa_zero_ponto1_g <- massa_zero
pontos$massa_agua_referencia_g <- pontos$massa_recipiente_com_solo_depois_g - massa_zero
pontos$vwc_referencia_m3_m3 <- pontos$massa_agua_referencia_g / pontos$volume_solo_cm3
pontos$agua_adicionada_acumulada_g <- cumsum(pontos$agua_adicionada_no_ponto_g)
pontos$diferenca_pesagem_menos_agua_adicionada_g <-
  pontos$massa_agua_referencia_g - pontos$agua_adicionada_acumulada_g
pontos$ganho_massa_etapa_g <- pontos$massa_recipiente_com_solo_depois_g -
  pontos$massa_recipiente_com_solo_antes_g
pontos$continuidade_com_ponto_anterior_g <- c(
  pontos$massa_recipiente_com_solo_antes_g[1] - massa_zero,
  pontos$massa_recipiente_com_solo_antes_g[-1] -
    pontos$massa_recipiente_com_solo_depois_g[-nrow(pontos)]
)
pontos$continuidade_ok_0_5g <- abs(pontos$continuidade_com_ponto_anterior_g) <= 0.5
pontos$balanco_ok <- abs(pontos$diferenca_pesagem_menos_agua_adicionada_g) <=
  pmax(2, 0.03 * pmax(pontos$agua_adicionada_acumulada_g, 1))

if (any(pontos$massa_agua_referencia_g < -0.5) || any(pontos$vwc_referencia_m3_m3 > 0.90)) {
  stop("A referência gravimétrica ficou fisicamente inválida.")
}
if (diff(range(pontos$vwc_referencia_m3_m3)) < 0.10) {
  stop("A amplitude de VWC é menor que 0,10 m³/m³.")
}

contagens <- aggregate(repeticao ~ ordem_ponto, brutas, length)
if (any(contagens$repeticao < 3)) stop("Cada nível deve ter ao menos três leituras do CS625.")
for (p in pontos$ordem_ponto) {
  d <- brutas[brutas$ordem_ponto == p, ]
  if (!all(sort(as.integer(d$repeticao)) == seq_len(nrow(d)))) {
    stop("Ponto ", p, ": repetições devem ser 1, 2, 3... sem duplicatas.")
  }
}

ag <- aggregate(leitura_sensor ~ ordem_ponto, brutas,
                function(x) c(n = length(x), media = mean(x), dp = sd(x), minimo = min(x), maximo = max(x)))
medias <- data.frame(
  ordem_ponto = ag$ordem_ponto,
  n_leituras = ag$leitura_sensor[, "n"],
  periodo_medio_us = ag$leitura_sensor[, "media"],
  periodo_dp_us = ag$leitura_sensor[, "dp"],
  periodo_minimo_us = ag$leitura_sensor[, "minimo"],
  periodo_maximo_us = ag$leitura_sensor[, "maximo"]
)
medias <- merge(pontos, medias, by = "ordem_ponto", all = TRUE, sort = TRUE)

# -----------------------------------------------------------------------------
# 4. CURVAS E COMPARAÇÃO COM A FÁBRICA
# -----------------------------------------------------------------------------
modelo_linear <- lm(vwc_referencia_m3_m3 ~ periodo_medio_us, data = medias)
modelo_quadratico <- lm(vwc_referencia_m3_m3 ~ periodo_medio_us + I(periodo_medio_us^2), data = medias)

medias$vwc_linear_m3_m3 <- predict(modelo_linear, medias)
medias$vwc_quadratica_especifica_m3_m3 <- predict(modelo_quadratico, medias)
medias$vwc_fabrica_campbell_m3_m3 <- campbell(medias$periodo_medio_us)
medias$residuo_linear_ponto_percentual <- 100 * (medias$vwc_linear_m3_m3 - medias$vwc_referencia_m3_m3)
medias$residuo_quadratica_ponto_percentual <- 100 * (medias$vwc_quadratica_especifica_m3_m3 - medias$vwc_referencia_m3_m3)
medias$residuo_fabrica_ponto_percentual <- 100 * (medias$vwc_fabrica_campbell_m3_m3 - medias$vwc_referencia_m3_m3)

pred_loocv_linear <- loocv(vwc_referencia_m3_m3 ~ periodo_medio_us, medias)
pred_loocv_quadratica <- loocv(vwc_referencia_m3_m3 ~ periodo_medio_us + I(periodo_medio_us^2), medias)

metricas <- data.frame(
  modelo = c("linear_especifica_diagnostica", "quadratica_especifica", "quadratica_fabrica_campbell"),
  rmse_ajuste_m3_m3 = c(
    rmse(medias$vwc_referencia_m3_m3, medias$vwc_linear_m3_m3),
    rmse(medias$vwc_referencia_m3_m3, medias$vwc_quadratica_especifica_m3_m3),
    rmse(medias$vwc_referencia_m3_m3, medias$vwc_fabrica_campbell_m3_m3)
  ),
  mae_ajuste_m3_m3 = c(
    mae(medias$vwc_referencia_m3_m3, medias$vwc_linear_m3_m3),
    mae(medias$vwc_referencia_m3_m3, medias$vwc_quadratica_especifica_m3_m3),
    mae(medias$vwc_referencia_m3_m3, medias$vwc_fabrica_campbell_m3_m3)
  ),
  bias_predito_menos_referencia_m3_m3 = c(
    bias(medias$vwc_referencia_m3_m3, medias$vwc_linear_m3_m3),
    bias(medias$vwc_referencia_m3_m3, medias$vwc_quadratica_especifica_m3_m3),
    bias(medias$vwc_referencia_m3_m3, medias$vwc_fabrica_campbell_m3_m3)
  ),
  eficiencia_1_menos_sse_sst = c(
    eficiencia(medias$vwc_referencia_m3_m3, medias$vwc_linear_m3_m3),
    eficiencia(medias$vwc_referencia_m3_m3, medias$vwc_quadratica_especifica_m3_m3),
    eficiencia(medias$vwc_referencia_m3_m3, medias$vwc_fabrica_campbell_m3_m3)
  ),
  rmse_loocv_m3_m3 = c(
    rmse(medias$vwc_referencia_m3_m3, pred_loocv_linear),
    rmse(medias$vwc_referencia_m3_m3, pred_loocv_quadratica),
    NA_real_
  )
)
metricas$rmse_ajuste_ponto_percentual <- 100 * metricas$rmse_ajuste_m3_m3
metricas$mae_ajuste_ponto_percentual <- 100 * metricas$mae_ajuste_m3_m3
metricas$bias_ponto_percentual <- 100 * metricas$bias_predito_menos_referencia_m3_m3
metricas$rmse_loocv_ponto_percentual <- 100 * metricas$rmse_loocv_m3_m3

cl <- coef(modelo_linear)
cq <- coef(modelo_quadratico)
faixa_periodo <- range(medias$periodo_medio_us)
grade_periodo <- seq(faixa_periodo[1], faixa_periodo[2], length.out = 500)
derivadas <- cq[2] + 2 * cq[3] * grade_periodo
pred_grade_quad <- cq[1] + cq[2] * grade_periodo + cq[3] * grade_periodo^2
forma_quadratica <- data.frame(
  coeficiente_quadratico_positivo = is.finite(cq[3]) && cq[3] > 0,
  derivada_positiva_em_toda_faixa = all(derivadas > 0),
  crescente_em_toda_faixa = all(diff(pred_grade_quad) > 0),
  vwc_plausivel_em_toda_faixa = all(pred_grade_quad >= -0.02 & pred_grade_quad <= 0.90),
  periodo_minimo_us = faixa_periodo[1],
  periodo_maximo_us = faixa_periodo[2],
  derivada_minima = min(derivadas),
  derivada_maxima = max(derivadas)
)
forma_quadratica$adotavel_segundo_forma_campbell <- with(
  forma_quadratica,
  coeficiente_quadratico_positivo & derivada_positiva_em_toda_faixa &
    crescente_em_toda_faixa & vwc_plausivel_em_toda_faixa
)

coeficientes <- data.frame(
  modelo = c("linear_especifica_diagnostica", "quadratica_especifica", "quadratica_fabrica_campbell"),
  C0 = c(cl[1], cq[1], -0.0663),
  C1 = c(cl[2], cq[2], -0.0063),
  C2 = c(0, cq[3], 0.0007),
  periodo_minimo_us = faixa_periodo[1],
  periodo_maximo_us = faixa_periodo[2],
  vwc_referencia_minima_m3_m3 = min(medias$vwc_referencia_m3_m3),
  vwc_referencia_maxima_m3_m3 = max(medias$vwc_referencia_m3_m3)
)

curvas <- data.frame(
  periodo_us = grade_periodo,
  vwc_linear_especifica_m3_m3 = cl[1] + cl[2] * grade_periodo,
  vwc_quadratica_especifica_m3_m3 = pred_grade_quad,
  vwc_fabrica_campbell_m3_m3 = campbell(grade_periodo)
)

# Sensibilidade ao limite superior da faixa: mantém todos os pontos na análise
# principal, mas mostra objetivamente em qual nível a forma Campbell deixa de ser
# atendida. Uma subfaixa não é validação independente nem autoriza extrapolação.
avaliar_subfaixa <- function(n) {
  d <- medias[seq_len(n), , drop = FALSE]
  mod <- lm(vwc_referencia_m3_m3 ~ periodo_medio_us + I(periodo_medio_us^2), data = d)
  cf <- coef(mod)
  gx <- seq(min(d$periodo_medio_us), max(d$periodo_medio_us), length.out = 300)
  gy <- cf[1] + cf[2] * gx + cf[3] * gx^2
  deriv <- cf[2] + 2 * cf[3] * gx
  pred <- predict(mod, d)
  pred_cv <- if (n >= 5) loocv(
    vwc_referencia_m3_m3 ~ periodo_medio_us + I(periodo_medio_us^2), d
  ) else rep(NA_real_, n)
  forma_ok <- cf[3] > 0 && all(deriv > 0) && all(diff(gy) > 0) &&
    all(gy >= -0.02 & gy <= 0.90)
  data.frame(
    pontos_inicial_final = paste0("1-", n),
    n_niveis = n,
    vwc_maxima_referencia_pct = 100 * max(d$vwc_referencia_m3_m3),
    periodo_maximo_us = max(d$periodo_medio_us),
    C0 = cf[1], C1 = cf[2], C2 = cf[3],
    rmse_ajuste_ponto_percentual = 100 * rmse(d$vwc_referencia_m3_m3, pred),
    rmse_loocv_ponto_percentual = if (n >= 5) 100 * rmse(d$vwc_referencia_m3_m3, pred_cv) else NA_real_,
    coeficiente_quadratico_positivo = cf[3] > 0,
    derivada_positiva_em_toda_faixa = all(deriv > 0),
    forma_campbell_atendida = forma_ok,
    candidata_exploratoria = n >= 5 && forma_ok
  )
}
sensibilidade_subfaixas <- do.call(rbind, lapply(4:nrow(medias), avaliar_subfaixa))
candidatas_subfaixa <- sensibilidade_subfaixas[
  sensibilidade_subfaixas$candidata_exploratoria, , drop = FALSE
]
melhor_subfaixa <- if (nrow(candidatas_subfaixa)) {
  candidatas_subfaixa[which.max(candidatas_subfaixa$n_niveis), , drop = FALSE]
} else NULL
curva_subfaixa <- if (!is.null(melhor_subfaixa)) {
  x <- seq(min(medias$periodo_medio_us), melhor_subfaixa$periodo_maximo_us, length.out = 250)
  data.frame(
    periodo_us = x,
    vwc_m3_m3 = melhor_subfaixa$C0 + melhor_subfaixa$C1 * x + melhor_subfaixa$C2 * x^2
  )
} else data.frame(periodo_us = numeric(), vwc_m3_m3 = numeric())

# -----------------------------------------------------------------------------
# 5. SAÍDAS
# -----------------------------------------------------------------------------
for (d in c(tabelas, graficos, resumos)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
write.csv(brutas, file.path(tabelas, "leituras_brutas_cs625.csv"), row.names = FALSE)
write.csv(pontos, file.path(tabelas, "pontos_vwc_referencia.csv"), row.names = FALSE)
write.csv(medias, file.path(tabelas, "medias_predicoes_residuos_cs625.csv"), row.names = FALSE)
write.csv(coeficientes, file.path(tabelas, "coeficientes_curvas_cs625.csv"), row.names = FALSE)
write.csv(metricas, file.path(tabelas, "metricas_comparacao_curvas_cs625.csv"), row.names = FALSE)
write.csv(forma_quadratica, file.path(tabelas, "validacao_forma_quadratica_especifica.csv"), row.names = FALSE)
write.csv(curvas, file.path(tabelas, "curvas_estimadas_grade_cs625.csv"), row.names = FALSE)
write.csv(sensibilidade_subfaixas, file.path(tabelas, "sensibilidade_limite_superior_cs625.csv"), row.names = FALSE)

cores <- c(
  referencia = "#17352F", completa = "#D97828", subfaixa = "#2D5F8B",
  fabrica = "#6750A4", linear = "#13796C"
)
png(file.path(graficos, "curva_cs625_especifica_vs_fabrica.png"), width = 1800, height = 1200, res = 180)
par(mar = c(5.3, 5.5, 4.0, 1.5), las = 1)
ylim <- range(c(medias$vwc_referencia_m3_m3, curvas[-1], curva_subfaixa$vwc_m3_m3), finite = TRUE)
plot(
  medias$periodo_medio_us, medias$vwc_referencia_m3_m3,
  pch = 19, cex = 1.25, col = cores["referencia"], ylim = ylim,
  xlab = "Período médio do CS625 (µs)",
  ylab = "Conteúdo volumétrico de água, VWC (m³ m⁻³)",
  main = "CS625: curva específica do ensaio e curva padrão Campbell"
)
grid(col = "gray88")
segments(
  medias$periodo_medio_us - medias$periodo_dp_us, medias$vwc_referencia_m3_m3,
  medias$periodo_medio_us + medias$periodo_dp_us, medias$vwc_referencia_m3_m3,
  col = cores["referencia"], lwd = 1.4
)
lines(curvas$periodo_us, curvas$vwc_quadratica_especifica_m3_m3, col = cores["completa"], lwd = 2.4)
if (nrow(curva_subfaixa)) {
  lines(curva_subfaixa$periodo_us, curva_subfaixa$vwc_m3_m3, col = cores["subfaixa"], lwd = 3.2, lty = 4)
}
lines(curvas$periodo_us, curvas$vwc_fabrica_campbell_m3_m3, col = cores["fabrica"], lwd = 2.6, lty = 2)
points(medias$periodo_medio_us, medias$vwc_referencia_m3_m3, pch = 19, cex = 1.25, col = cores["referencia"])
legenda_curva <- c("VWC por pesagem", "quadrática completa (diagnóstico)")
cores_curva <- c(cores["referencia"], cores["completa"])
lty_curva <- c(NA, 1)
lwd_curva <- c(NA, 2.4)
pch_curva <- c(19, NA)
if (nrow(curva_subfaixa)) {
  legenda_curva <- c(legenda_curva, paste0("candidata pontos ", melhor_subfaixa$pontos_inicial_final))
  cores_curva <- c(cores_curva, cores["subfaixa"])
  lty_curva <- c(lty_curva, 4)
  lwd_curva <- c(lwd_curva, 3.2)
  pch_curva <- c(pch_curva, NA)
}
legenda_curva <- c(legenda_curva, "padrão Campbell")
cores_curva <- c(cores_curva, cores["fabrica"])
lty_curva <- c(lty_curva, 2)
lwd_curva <- c(lwd_curva, 2.6)
pch_curva <- c(pch_curva, NA)
legend(
  "topleft", bty = "n", cex = 0.88,
  legend = legenda_curva, col = cores_curva, pch = pch_curva,
  lty = lty_curva, lwd = lwd_curva
)
dev.off()

png(file.path(graficos, "residuos_cs625_especifica_vs_fabrica.png"), width = 1800, height = 1050, res = 180)
par(mar = c(5.3, 5.5, 4.0, 1.5), las = 1)
yr <- range(c(medias$residuo_quadratica_ponto_percentual,
              medias$residuo_fabrica_ponto_percentual,
              medias$residuo_linear_ponto_percentual), finite = TRUE)
plot(
  medias$vwc_referencia_m3_m3 * 100, medias$residuo_quadratica_ponto_percentual,
  type = "b", pch = 19, lwd = 2.5, col = cores["completa"], ylim = yr,
  xlab = "VWC de referência por pesagem (%)",
  ylab = "Predição − referência (pontos percentuais)",
  main = "Resíduos das curvas do CS625"
)
grid(col = "gray88")
abline(h = 0, col = "gray35", lty = 3)
lines(medias$vwc_referencia_m3_m3 * 100, medias$residuo_fabrica_ponto_percentual,
      type = "b", pch = 17, lwd = 2.2, lty = 2, col = cores["fabrica"])
lines(medias$vwc_referencia_m3_m3 * 100, medias$residuo_linear_ponto_percentual,
      type = "b", pch = 15, lwd = 1.8, lty = 3, col = cores["linear"])
legend(
  "topleft", bty = "n", cex = 0.9,
  legend = c("quadrática completa (diagnóstico)", "padrão Campbell", "linear diagnóstica"),
  col = c(cores["completa"], cores["fabrica"], cores["linear"]),
  pch = c(19, 17, 15), lty = c(1, 2, 3), lwd = c(2.5, 2.2, 1.8)
)
dev.off()

m_quad <- metricas[metricas$modelo == "quadratica_especifica", ]
m_fabrica <- metricas[metricas$modelo == "quadratica_fabrica_campbell", ]
m_linear <- metricas[metricas$modelo == "linear_especifica_diagnostica", ]
alertas <- c(
  if (pontos$diametro_interno_cm[1] < 10 || pontos$altura_solo_cm[1] < 35)
    "NOTA: recipiente menor que o exemplo Campbell de 10 cm × 35 cm; resultados específicos deste arranjo." else NULL,
  if (any(!pontos$continuidade_ok_0_5g))
    paste0("ALERTA de continuidade (>0,5 g) nos pontos: ", paste(pontos$ordem_ponto[!pontos$continuidade_ok_0_5g], collapse = ", "), ".") else NULL,
  if (any(!pontos$balanco_ok))
    paste0("ALERTA de balanço hídrico nos pontos: ", paste(pontos$ordem_ponto[!pontos$balanco_ok], collapse = ", "), ".") else NULL,
  if (any(is.na(pontos$temperatura_solo_c)))
    "ALERTA: temperatura do solo não foi registrada nos níveis analisados." else NULL
)
if (!length(alertas)) alertas <- "Nenhum alerta operacional gerado."
status_quad <- if (forma_quadratica$adotavel_segundo_forma_campbell) {
  "A quadrática específica atende aos critérios de forma Campbell dentro da faixa medida."
} else {
  "A quadrática específica NÃO atende integralmente aos critérios de forma Campbell; trate-a como diagnóstico, não como curva adotada."
}
linha_subfaixa <- if (!is.null(melhor_subfaixa)) {
  d <- melhor_subfaixa
  sprintf(
    paste0(
      "Maior subfaixa exploratória com ao menos cinco níveis e forma Campbell: pontos %s, ",
      "até %.3f%% VWC (T até %.4f µs); VWC = %.10f %+.10f*T %+.10f*T²; ",
      "LOOCV RMSE = %.3f p.p. Requer validação independente."
    ),
    d$pontos_inicial_final, d$vwc_maxima_referencia_pct, d$periodo_maximo_us,
    d$C0, d$C1, d$C2, d$rmse_loocv_ponto_percentual
  )
} else {
  "Nenhuma subfaixa com ao menos cinco níveis atendeu integralmente à forma Campbell."
}

resumo <- c(
  "CALIBRAÇÃO LABORATORIAL EXCLUSIVA DO CS625", "",
  paste0("Ensaio: ", brutas$ensaio_id[1]),
  sprintf("Base: %d leituras brutas em %d níveis completos (pontos 1–%d).", nrow(brutas), nrow(medias), max(medias$ordem_ponto)),
  sprintf("Faixa de referência: %.3f%% a %.3f%% VWC.", 100 * min(medias$vwc_referencia_m3_m3), 100 * max(medias$vwc_referencia_m3_m3)),
  sprintf("Faixa de período médio: %.4f a %.4f µs.", min(medias$periodo_medio_us), max(medias$periodo_medio_us)),
  sprintf("Volume fixo: %.3f cm³; massa zero do ponto 1: %.3f g.", pontos$volume_solo_cm3[1], massa_zero),
  "",
  "Equações (T = período bruto médio em µs; resultado em m³/m³):",
  sprintf("Quadrática específica: VWC = %.10f %+.10f*T %+.10f*T²", cq[1], cq[2], cq[3]),
  sprintf("Linear diagnóstica: VWC = %.10f %+.10f*T", cl[1], cl[2]),
  "Padrão Campbell: VWC = -0.0663000000 -0.0063000000*T +0.0007000000*T²",
  "",
  "Comparação no conjunto de sete níveis:",
  sprintf("Quadrática específica: RMSE de ajuste = %.3f p.p.; MAE = %.3f p.p.; LOOCV RMSE = %.3f p.p.",
          m_quad$rmse_ajuste_ponto_percentual, m_quad$mae_ajuste_ponto_percentual, m_quad$rmse_loocv_ponto_percentual),
  sprintf("Curva Campbell: RMSE = %.3f p.p.; MAE = %.3f p.p.; viés = %+.3f p.p.",
          m_fabrica$rmse_ajuste_ponto_percentual, m_fabrica$mae_ajuste_ponto_percentual, m_fabrica$bias_ponto_percentual),
  sprintf("Linear diagnóstica: RMSE de ajuste = %.3f p.p.; LOOCV RMSE = %.3f p.p.",
          m_linear$rmse_ajuste_ponto_percentual, m_linear$rmse_loocv_ponto_percentual),
  status_quad,
  linha_subfaixa,
  "RMSE/MAE da curva específica no mesmo conjunto são medidas de ajuste, não validação independente.",
  "LOOCV é validação interna por nível; uma nova sequência/coluna é necessária para validação independente.",
  "Não extrapolar fora da faixa de período e VWC observada.",
  "",
  "Critérios da quadrática específica:",
  paste0("C2 positivo: ", forma_quadratica$coeficiente_quadratico_positivo),
  paste0("Derivada positiva em toda a faixa: ", forma_quadratica$derivada_positiva_em_toda_faixa),
  paste0("Predição crescente em toda a faixa: ", forma_quadratica$crescente_em_toda_faixa),
  paste0("VWC plausível em toda a faixa: ", forma_quadratica$vwc_plausivel_em_toda_faixa),
  "",
  "Alertas:", alertas,
  "",
  "Fonte da curva padrão: Campbell Scientific, manual CS616/CS625, equação mineral padrão."
)
writeLines(resumo, file.path(resumos, "resumo_calibracao_cs625.txt"), useBytes = TRUE)
message("Concluído. Resultados em: ", saida)
