# Calibração laboratorial de três sensores de umidade do solo
#
# Referência: VWC independente por pesagem integral da mesma coluna de solo.
# CS625: período bruto (us) -> VWC, quadrática específica do solo.
# Outros sensores: porcentagem bruta -> VWC, ajuste empírico linear/quadrático.
#
# Entrada persistente:
#   dados/calibracao_laboratorial/leituras_laboratorio.csv
#
# Execute da raiz do projeto:
# Rscript programas/calibracao_laboratorial_3_sensores.R

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
entrada_dir <- file.path(base_dir, "dados", "calibracao_laboratorial")
arquivo_leituras <- file.path(entrada_dir, "leituras_laboratorio.csv")
saida_dir <- file.path(base_dir, "resultados", "calibracao_laboratorial_3_sensores")
tabelas_dir <- file.path(saida_dir, "tabelas")
graficos_dir <- file.path(saida_dir, "graficos")
resumos_dir <- file.path(saida_dir, "resumos")

ler_numero <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  tem_virgula <- !is.na(x) & grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub(".", "", x[tem_virgula], fixed = TRUE)
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

ler_data_hora <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x <- gsub("T", " ", x, fixed = TRUE)
  resultado <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
  faltou_segundo <- is.na(resultado) & !is.na(x)
  resultado[faltou_segundo] <- as.POSIXct(x[faltou_segundo], format = "%Y-%m-%d %H:%M", tz = "America/Sao_Paulo")
  resultado
}

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae <- function(obs, pred) mean(abs(obs - pred))

loocv_rmse <- function(formula, d) {
  if (nrow(d) < 5) return(NA_real_)
  pred <- rep(NA_real_, nrow(d))
  for (i in seq_len(nrow(d))) {
    ajuste <- try(lm(formula, data = d[-i, , drop = FALSE]), silent = TRUE)
    if (inherits(ajuste, "try-error")) return(NA_real_)
    pred[i] <- suppressWarnings(tryCatch(
      as.numeric(predict(ajuste, newdata = d[i, , drop = FALSE])),
      error = function(e) NA_real_
    ))
    if (!is.finite(pred[i])) return(NA_real_)
  }
  rmse(d$vwc_referencia_m3_m3, pred)
}

avaliar_forma <- function(modelo, d, exigir_crescente = TRUE, exigir_concava_cima = FALSE) {
  x <- seq(min(d$leitura_media_sensor), max(d$leitura_media_sensor), length.out = 400)
  y <- as.numeric(predict(modelo, newdata = data.frame(leitura_media_sensor = x)))
  crescente <- all(diff(y) >= -1e-10)
  plausivel <- all(is.finite(y)) && all(y >= -0.02 & y <= 0.90)
  cf <- coef(modelo)
  concava_cima <- !exigir_concava_cima || (length(cf) >= 3 && is.finite(cf[3]) && cf[3] > 0)
  derivada_positiva <- TRUE
  if (exigir_concava_cima && length(cf) >= 3) {
    derivada_positiva <- all(cf[2] + 2 * cf[3] * x > 0)
  }
  list(
    crescente = crescente,
    plausivel = plausivel,
    concava_cima = concava_cima,
    derivada_positiva = derivada_positiva,
    utilizavel = crescente && plausivel && concava_cima && derivada_positiva
  )
}

normalizar_unidade <- function(x) {
  y <- tolower(trimws(as.character(x)))
  y[y %in% c("µs", "μs", "usec", "microseconds", "microsegundos")] <- "us"
  y[y %in% c("pct", "percent", "porcentagem")] <- "%"
  y
}

# -----------------------------------------------------------------------------
# 2. LEITURA E VALIDAÇÃO DO ESQUEMA
# -----------------------------------------------------------------------------
colunas_obrigatorias <- c(
  "ensaio_id", "ordem_ponto", "vwc_alvo_pct", "agua_adicionada_no_ponto_g",
  "massa_recipiente_com_solo_antes_g", "massa_recipiente_com_solo_depois_g",
  "massa_recipiente_vazio_g", "massa_solo_seco_total_g", "metodo_massa_seca",
  "diametro_interno_cm", "altura_solo_cm", "volume_solo_cm3",
  "tempo_equilibrio_min", "temperatura_solo_c", "data_hora_leitura", "ordem_medicao",
  "sensor_id", "variavel_leitura", "unidade_leitura", "repeticao", "leitura_sensor",
  "observacoes_ponto", "observacoes_leitura"
)
if (!file.exists(arquivo_leituras)) stop("Arquivo não encontrado: ", arquivo_leituras)
leituras_brutas <- read.csv2(
  arquivo_leituras, colClasses = "character", check.names = FALSE,
  na.strings = c("", "NA"), quote = '"', fileEncoding = "UTF-8-BOM"
)
faltantes <- setdiff(colunas_obrigatorias, names(leituras_brutas))
if (length(faltantes)) stop("CSV incompatível; colunas ausentes: ", paste(faltantes, collapse = ", "))
if (!nrow(leituras_brutas)) stop("A tabela de leituras está vazia.")

colunas_numericas <- c(
  "ordem_ponto", "vwc_alvo_pct", "agua_adicionada_no_ponto_g",
  "massa_recipiente_com_solo_antes_g", "massa_recipiente_com_solo_depois_g",
  "massa_recipiente_vazio_g", "massa_solo_seco_total_g", "diametro_interno_cm",
  "altura_solo_cm", "volume_solo_cm3", "tempo_equilibrio_min",
  "temperatura_solo_c", "ordem_medicao", "repeticao", "leitura_sensor"
)
for (nm in colunas_numericas) leituras_brutas[[nm]] <- ler_numero(leituras_brutas[[nm]])
for (nm in c("ensaio_id", "metodo_massa_seca", "sensor_id", "variavel_leitura", "unidade_leitura",
             "observacoes_ponto", "observacoes_leitura")) {
  leituras_brutas[[nm]] <- trimws(ifelse(is.na(leituras_brutas[[nm]]), "", leituras_brutas[[nm]]))
}
leituras_brutas$unidade_leitura <- normalizar_unidade(leituras_brutas$unidade_leitura)
leituras_brutas$data_hora <- ler_data_hora(leituras_brutas$data_hora_leitura)

if (length(unique(leituras_brutas$ensaio_id)) != 1 || !nzchar(leituras_brutas$ensaio_id[1])) {
  stop("Use um único ensaio_id não vazio por arquivo.")
}
if (any(!is.finite(leituras_brutas$ordem_ponto)) || any(leituras_brutas$ordem_ponto < 1) ||
    any(leituras_brutas$ordem_ponto != floor(leituras_brutas$ordem_ponto))) {
  stop("ordem_ponto deve conter inteiros positivos.")
}
if (any(!is.finite(leituras_brutas$ordem_medicao)) || any(leituras_brutas$ordem_medicao < 1) ||
    any(leituras_brutas$ordem_medicao != floor(leituras_brutas$ordem_medicao))) {
  stop("ordem_medicao deve conter inteiros positivos.")
}
if (any(!is.finite(leituras_brutas$repeticao)) || any(leituras_brutas$repeticao < 1) ||
    any(leituras_brutas$repeticao != floor(leituras_brutas$repeticao))) {
  stop("repeticao deve conter inteiros positivos.")
}
if (any(!is.finite(leituras_brutas$leitura_sensor)) || any(!nzchar(leituras_brutas$sensor_id))) {
  stop("Há leitura de sensor inválida ou sensor_id vazio.")
}
if (any(is.na(leituras_brutas$data_hora))) stop("Todas as leituras precisam de data_hora_leitura válida.")

metadados <- unique(leituras_brutas[, c("sensor_id", "variavel_leitura", "unidade_leitura")])
if (any(duplicated(metadados$sensor_id))) stop("Cada sensor_id deve ter uma única variável/unidade em todo o ensaio.")
if (nrow(metadados) != 3) stop("O arquivo deve conter exatamente três sensores.")
cs <- metadados$variavel_leitura == "periodo_us" & metadados$unidade_leitura == "us"
outros <- metadados$variavel_leitura == "umidade_percentual" & metadados$unidade_leitura == "%"
if (sum(cs) != 1 || sum(outros) != 2) {
  stop("Metadados inválidos: exatamente um sensor deve ser periodo_us/us e dois devem ser umidade_percentual/%.")
}
sensor_cs625 <- metadados$sensor_id[cs]
nomes_sensores <- unique(leituras_brutas$sensor_id)

idx_cs <- leituras_brutas$sensor_id == sensor_cs625
if (any(leituras_brutas$leitura_sensor[idx_cs] <= 0)) stop("O período do CS625 deve ser positivo e estar em us.")
if (any(leituras_brutas$leitura_sensor[idx_cs] < 10 | leituras_brutas$leitura_sensor[idx_cs] > 50)) {
  warning("Há períodos CS625 fora da faixa ampla de conferência 10–50 us; confira unidade e digitação.")
}
if (any(leituras_brutas$leitura_sensor[!idx_cs] < 0 | leituras_brutas$leitura_sensor[!idx_cs] > 100)) {
  stop("As leituras percentuais de plugfild/azul devem estar entre 0 e 100%.")
}

# -----------------------------------------------------------------------------
# 3. PONTOS, PESAGEM, GEOMETRIA E CONTROLES DA SEQUÊNCIA
# -----------------------------------------------------------------------------
campos_ponto_num <- c(
  "vwc_alvo_pct", "agua_adicionada_no_ponto_g", "massa_recipiente_com_solo_antes_g",
  "massa_recipiente_com_solo_depois_g", "massa_recipiente_vazio_g", "massa_solo_seco_total_g",
  "diametro_interno_cm", "altura_solo_cm", "volume_solo_cm3",
  "tempo_equilibrio_min", "temperatura_solo_c"
)
campos_ponto_txt <- c("metodo_massa_seca", "observacoes_ponto")
extrair_unico <- function(d, nm) {
  v <- unique(d[[nm]])
  if (length(v) != 1) stop("Ponto ", d$ordem_ponto[1], ": valores diferentes para ", nm, ".")
  v
}
pontos_lista <- split(leituras_brutas, leituras_brutas$ordem_ponto)
pontos <- do.call(rbind, lapply(pontos_lista, function(d) {
  vals <- c(lapply(campos_ponto_num, function(nm) extrair_unico(d, nm)),
            lapply(campos_ponto_txt, function(nm) extrair_unico(d, nm)))
  names(vals) <- c(campos_ponto_num, campos_ponto_txt)
  as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
}))
pontos$ordem_ponto <- as.numeric(rownames(pontos))
rownames(pontos) <- NULL
pontos <- pontos[order(pontos$ordem_ponto), c("ordem_ponto", campos_ponto_num, campos_ponto_txt)]
for (nm in campos_ponto_num) pontos[[nm]] <- as.numeric(pontos[[nm]])
if (!all(pontos$ordem_ponto == seq_len(nrow(pontos)))) stop("ordem_ponto deve ser 1, 2, 3, ... sem saltos.")

obrigatorios_finitos <- setdiff(campos_ponto_num, "temperatura_solo_c")
if (any(!sapply(pontos[, obrigatorios_finitos, drop = FALSE], function(x) all(is.finite(x))))) {
  stop("Há constantes, pesagens ou valores de ponto vazios/inválidos.")
}
if (any(pontos$vwc_alvo_pct < 0 | pontos$vwc_alvo_pct > 100) ||
    any(pontos$agua_adicionada_no_ponto_g < 0) || any(pontos$massa_recipiente_vazio_g < 0) ||
    any(pontos$massa_solo_seco_total_g <= 0) || any(pontos$diametro_interno_cm <= 0) ||
    any(pontos$altura_solo_cm <= 0) || any(pontos$volume_solo_cm3 <= 0) ||
    any(pontos$tempo_equilibrio_min < 0)) {
  stop("Há valores fisicamente inválidos nas constantes ou nos pontos.")
}

constantes <- c("massa_recipiente_vazio_g", "massa_solo_seco_total_g", "diametro_interno_cm",
               "altura_solo_cm", "volume_solo_cm3", "metodo_massa_seca")
if (any(sapply(pontos[, constantes, drop = FALSE], function(x) length(unique(x)) != 1))) {
  stop("Tara, massa seca, geometria e método de massa seca devem permanecer constantes.")
}
metodos_validos <- c("estufa_105C_massa_constante", "massa_seca_equivalente_subamostra")
if (!all(pontos$metodo_massa_seca %in% metodos_validos)) {
  stop("metodo_massa_seca deve ser estufa_105C_massa_constante ou massa_seca_equivalente_subamostra.")
}

volume_geometrico <- pi * (pontos$diametro_interno_cm / 2)^2 * pontos$altura_solo_cm
tol_volume <- pmax(0.5, volume_geometrico * 0.001)
if (any(abs(pontos$volume_solo_cm3 - volume_geometrico) > tol_volume)) {
  stop("volume_solo_cm3 não corresponde a pi*(diametro/2)^2*altura; confira as dimensões internas.")
}
pontos$volume_geometrico_cm3 <- volume_geometrico
pontos$geometria_exemplo_campbell_atendida <- pontos$diametro_interno_cm >= 10 & pontos$altura_solo_cm >= 35

pontos$tolerancia_pesagem_g <- 0.5
if (abs(pontos$agua_adicionada_no_ponto_g[1]) > 0.001) {
  stop("O ponto 1 deve ter agua_adicionada_no_ponto_g igual a zero, pois define a referência hídrica zero.")
}

# O ponto 1 foi preparado sem adição de água. Sua massa após o preparo é o zero
# experimental da coluna; tara e massa seca permanecem como controles independentes.
massa_base_zero_g <- pontos$massa_recipiente_com_solo_depois_g[1]
pontos$massa_base_zero_g <- massa_base_zero_g
pontos$massa_base_menos_tara_solo_seco_g <- massa_base_zero_g -
  pontos$massa_recipiente_vazio_g - pontos$massa_solo_seco_total_g
pontos$massa_agua_referencia_g <- pontos$massa_recipiente_com_solo_depois_g - massa_base_zero_g
pontos$vwc_referencia_m3_m3 <- pontos$massa_agua_referencia_g / pontos$volume_solo_cm3
if (any(pontos$massa_agua_referencia_g < -pontos$tolerancia_pesagem_g) ||
    any(pontos$vwc_referencia_m3_m3 > 0.90)) {
  stop("Água/VWC de referência inválida em relação ao ponto 1; confira as pesagens e o volume.")
}

# Controles que não substituem a referência obtida em relação ao ponto 1.
pontos$ganho_massa_na_etapa_g <- pontos$massa_recipiente_com_solo_depois_g -
  pontos$massa_recipiente_com_solo_antes_g
pontos$diferenca_agua_adicionada_menos_ganho_etapa_g <-
  pontos$agua_adicionada_no_ponto_g - pontos$ganho_massa_na_etapa_g
pontos$continuidade_com_ponto_anterior_g <- c(
  pontos$massa_recipiente_com_solo_antes_g[1] - massa_base_zero_g,
  pontos$massa_recipiente_com_solo_antes_g[-1] -
    pontos$massa_recipiente_com_solo_depois_g[-nrow(pontos)]
)
pontos$continuidade_pesagem_ok <-
  abs(pontos$continuidade_com_ponto_anterior_g) <= pontos$tolerancia_pesagem_g

pontos$agua_acumulada_adicionada_g <- cumsum(pontos$agua_adicionada_no_ponto_g)
pontos$diferenca_balanco_pesada_menos_adicionada_g <-
  pontos$massa_agua_referencia_g - pontos$agua_acumulada_adicionada_g
pontos$tolerancia_balanco_g <- pmax(2, 0.03 * pmax(pontos$agua_acumulada_adicionada_g, 1))
pontos$balanco_hidrico_ok <-
  abs(pontos$diferenca_balanco_pesada_menos_adicionada_g) <= pontos$tolerancia_balanco_g

pontos$densidade_aparente_seca_g_cm3 <- pontos$massa_solo_seco_total_g / pontos$volume_solo_cm3
pontos$porosidade_estimada <- 1 - pontos$densidade_aparente_seca_g_cm3 / 2.65
pontos$vwc_compativel_porosidade_estimada <- pontos$vwc_referencia_m3_m3 <= pontos$porosidade_estimada + 0.03

# Matriz completa, repetições e ordem temporal.
contagens <- aggregate(repeticao ~ ordem_ponto + sensor_id, leituras_brutas, length)
grade <- expand.grid(ordem_ponto = pontos$ordem_ponto, sensor_id = nomes_sensores, stringsAsFactors = FALSE)
contagens <- merge(grade, contagens, by = c("ordem_ponto", "sensor_id"), all.x = TRUE)
contagens$repeticao[is.na(contagens$repeticao)] <- 0
if (any(contagens$repeticao < 3)) {
  ruins <- contagens[contagens$repeticao < 3, ]
  stop("Exija ao menos três leituras por ponto/sensor. Incompletos: ",
       paste(paste0("p", ruins$ordem_ponto, "/", ruins$sensor_id, "=", ruins$repeticao), collapse = "; "))
}
chaves_repeticao <- interaction(leituras_brutas$ordem_ponto, leituras_brutas$sensor_id, drop = TRUE)
for (chave in unique(chaves_repeticao)) {
  d <- leituras_brutas[chaves_repeticao == chave, ]
  if (!all(sort(as.integer(d$repeticao)) == seq_len(nrow(d)))) stop("Repetições devem ser 1, 2, 3... sem duplicatas por ponto/sensor.")
  if (length(unique(d$ordem_medicao)) != 1) stop("Cada sensor deve ter uma única ordem_medicao por ponto.")
}
for (p in pontos$ordem_ponto) {
  d <- leituras_brutas[leituras_brutas$ordem_ponto == p, ]
  ordens <- tapply(d$ordem_medicao, d$sensor_id, unique)
  if (length(ordens) != 3 || !all(sort(as.numeric(ordens)) == 1:3)) stop("Ponto ", p, ": as ordens dos três sensores devem ser 1, 2 e 3.")
  tempos <- tapply(as.numeric(d$data_hora), d$sensor_id, min)
  if (any(diff(tempos[order(as.numeric(ordens))]) < 0)) stop("Ponto ", p, ": horários não seguem ordem_medicao.")
}

niveis_distintos <- length(unique(round(pontos$vwc_referencia_m3_m3, 6)))
amplitude_vwc <- diff(range(pontos$vwc_referencia_m3_m3))
if (nrow(pontos) < 5 || niveis_distintos < 5) stop("São necessários ao menos cinco níveis distintos para LOOCV e seleção de modelos.")
if (amplitude_vwc < 0.10) stop("A amplitude de VWC é menor que 0,10 m3/m3; amplie a faixa experimental.")

# -----------------------------------------------------------------------------
# 4. MÉDIAS E MODELOS
# -----------------------------------------------------------------------------
leituras <- merge(leituras_brutas, pontos, by = "ordem_ponto", all.x = TRUE, sort = FALSE,
                  suffixes = c("", "_ponto"))
medias_ag <- aggregate(leitura_sensor ~ sensor_id + variavel_leitura + unidade_leitura + ordem_ponto + vwc_referencia_m3_m3,
                       data = leituras,
                       FUN = function(x) c(n = length(x), media = mean(x), dp = sd(x), minimo = min(x), maximo = max(x)))
medias <- data.frame(
  sensor_id = medias_ag$sensor_id, variavel_leitura = medias_ag$variavel_leitura,
  unidade_leitura = medias_ag$unidade_leitura, ordem_ponto = medias_ag$ordem_ponto,
  vwc_referencia_m3_m3 = medias_ag$vwc_referencia_m3_m3,
  n_leituras = medias_ag$leitura_sensor[, "n"], leitura_media_sensor = medias_ag$leitura_sensor[, "media"],
  desvio_padrao_leituras = medias_ag$leitura_sensor[, "dp"], leitura_minima_sensor = medias_ag$leitura_sensor[, "minimo"],
  leitura_maxima_sensor = medias_ag$leitura_sensor[, "maximo"]
)
medias <- merge(medias, pontos, by = c("ordem_ponto", "vwc_referencia_m3_m3"), all.x = TRUE, sort = FALSE)
medias <- medias[order(match(medias$sensor_id, nomes_sensores), medias$ordem_ponto), ]

ajustar_sensor <- function(d) {
  d <- d[order(d$ordem_ponto), ]
  linear <- lm(vwc_referencia_m3_m3 ~ leitura_media_sensor, data = d)
  quadratica <- lm(vwc_referencia_m3_m3 ~ leitura_media_sensor + I(leitura_media_sensor^2), data = d)
  eh_cs625 <- unique(d$variavel_leitura) == "periodo_us"
  av_lin <- avaliar_forma(linear, d, exigir_crescente = TRUE, exigir_concava_cima = FALSE)
  av_quad <- avaliar_forma(quadratica, d, exigir_crescente = TRUE, exigir_concava_cima = eh_cs625)
  geometria_exemplo_ok <- if (eh_cs625) all(d$geometria_exemplo_campbell_atendida) else TRUE
  candidatos <- data.frame(
    sensor_id = d$sensor_id[1], variavel_leitura = d$variavel_leitura[1], unidade_leitura = d$unidade_leitura[1],
    modelo = c("linear", "quadratica"),
    rmse_ajuste_m3_m3 = c(rmse(d$vwc_referencia_m3_m3, predict(linear)), rmse(d$vwc_referencia_m3_m3, predict(quadratica))),
    mae_ajuste_m3_m3 = c(mae(d$vwc_referencia_m3_m3, predict(linear)), mae(d$vwc_referencia_m3_m3, predict(quadratica))),
    rmse_loocv_m3_m3 = c(loocv_rmse(vwc_referencia_m3_m3 ~ leitura_media_sensor, d),
                         loocv_rmse(vwc_referencia_m3_m3 ~ leitura_media_sensor + I(leitura_media_sensor^2), d)),
    r2_ajustado = c(summary(linear)$adj.r.squared, summary(quadratica)$adj.r.squared),
    crescente = c(av_lin$crescente, av_quad$crescente),
    vwc_plausivel = c(av_lin$plausivel, av_quad$plausivel),
    concava_cima = c(NA, av_quad$concava_cima),
    derivada_positiva = c(NA, av_quad$derivada_positiva),
    geometria_exemplo_campbell_atendida = geometria_exemplo_ok,
    estatisticamente_utilizavel = c(av_lin$utilizavel, av_quad$utilizavel),
    modelo_adotavel = FALSE,
    modelo_recomendado = FALSE,
    stringsAsFactors = FALSE
  )
  if (eh_cs625) {
    candidatos$modelo_adotavel[2] <- candidatos$estatisticamente_utilizavel[2]
    candidatos$modelo_recomendado[2] <- candidatos$modelo_adotavel[2]
    candidatos$observacao <- c(
      "linear apenas como diagnóstico; manual Campbell prioriza quadrática",
      if (!candidatos$estatisticamente_utilizavel[2]) "NÃO ADOTAR: quadrática não é crescente/côncava/plausível"
      else if (!geometria_exemplo_ok) "quadrática adotável para o arranjo ensaiado; recipiente abaixo do exemplo Campbell"
      else "quadrática específica adotável conforme critérios Campbell"
    )
  } else {
    candidatos$modelo_adotavel <- candidatos$estatisticamente_utilizavel
    elegiveis <- which(candidatos$modelo_adotavel & is.finite(candidatos$rmse_loocv_m3_m3))
    if (length(elegiveis)) {
      lin_ok <- 1 %in% elegiveis; quad_ok <- 2 %in% elegiveis
      vencedor <- if (lin_ok && quad_ok) {
        if (candidatos$rmse_loocv_m3_m3[2] < 0.98 * candidatos$rmse_loocv_m3_m3[1]) 2 else 1
      } else elegiveis[which.min(candidatos$rmse_loocv_m3_m3[elegiveis])]
      candidatos$modelo_recomendado[vencedor] <- TRUE
    }
    candidatos$observacao <- ifelse(candidatos$modelo_recomendado,
      "selecionado por LOOCV, forma física e parcimônia",
      "diagnóstico; não selecionado")
  }
  cl <- coef(linear); cq <- coef(quadratica)
  candidatos$intercepto <- c(cl[1], cq[1])
  candidatos$coeficiente_leitura <- c(cl[2], cq[2])
  candidatos$coeficiente_leitura_quadrado <- c(0, cq[3])
  candidatos$n_pontos <- nrow(d)
  candidatos$leitura_minima_faixa <- min(d$leitura_media_sensor)
  candidatos$leitura_maxima_faixa <- max(d$leitura_media_sensor)
  candidatos$vwc_minima_faixa <- min(d$vwc_referencia_m3_m3)
  candidatos$vwc_maxima_faixa <- max(d$vwc_referencia_m3_m3)
  d$predicao_linear_m3_m3 <- predict(linear, d)
  d$predicao_quadratica_m3_m3 <- predict(quadratica, d)
  d$residuo_linear_m3_m3 <- d$predicao_linear_m3_m3 - d$vwc_referencia_m3_m3
  d$residuo_quadratico_m3_m3 <- d$predicao_quadratica_m3_m3 - d$vwc_referencia_m3_m3
  list(dados = d, coeficientes = candidatos)
}
resultado <- lapply(split(medias, factor(medias$sensor_id, levels = nomes_sensores)), ajustar_sensor)
medias_preditas <- do.call(rbind, lapply(resultado, `[[`, "dados"))
coeficientes <- do.call(rbind, lapply(resultado, `[[`, "coeficientes"))

# Equação-padrão Campbell apenas para diagnóstico; não substitui a curva específica.
d_cs <- medias_preditas[medias_preditas$sensor_id == sensor_cs625, ]
d_cs$vwc_fabrica_campbell_m3_m3 <- -0.0663 - 0.0063 * d_cs$leitura_media_sensor + 0.0007 * d_cs$leitura_media_sensor^2
d_cs$residuo_fabrica_m3_m3 <- d_cs$vwc_fabrica_campbell_m3_m3 - d_cs$vwc_referencia_m3_m3
metricas_fabrica <- data.frame(
  sensor_id = sensor_cs625, modelo = "quadratica_fabrica_campbell",
  rmse_m3_m3 = rmse(d_cs$vwc_referencia_m3_m3, d_cs$vwc_fabrica_campbell_m3_m3),
  mae_m3_m3 = mae(d_cs$vwc_referencia_m3_m3, d_cs$vwc_fabrica_campbell_m3_m3),
  bias_m3_m3 = mean(d_cs$residuo_fabrica_m3_m3)
)

# -----------------------------------------------------------------------------
# 5. PLANEJAMENTO DE ÁGUA E SAÍDAS
# -----------------------------------------------------------------------------
vol <- pontos$volume_solo_cm3[1]
tara <- pontos$massa_recipiente_vazio_g[1]
solo_seco <- pontos$massa_solo_seco_total_g[1]
planejamento <- data.frame(vwc_alvo_pct = seq(0, 60, 5))
planejamento$agua_acumulada_teorica_solo_seco_g <- vol * planejamento$vwc_alvo_pct / 100
planejamento$agua_adicionar_desde_ponto_anterior_g <- c(0, diff(planejamento$agua_acumulada_teorica_solo_seco_g))
planejamento$massa_total_teorica_recipiente_solo_agua_g <- tara + solo_seco + planejamento$agua_acumulada_teorica_solo_seco_g

for (d in c(tabelas_dir, graficos_dir, resumos_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
write.csv(pontos, file.path(tabelas_dir, "pontos_e_vwc_referencia.csv"), row.names = FALSE)
write.csv(leituras, file.path(tabelas_dir, "leituras_brutas_sequenciais.csv"), row.names = FALSE)
write.csv(contagens, file.path(tabelas_dir, "contagem_repeticoes_ponto_sensor.csv"), row.names = FALSE)
write.csv(medias_preditas, file.path(tabelas_dir, "medias_predicoes_residuos.csv"), row.names = FALSE)
write.csv(coeficientes, file.path(tabelas_dir, "coeficientes_e_validacao_modelos.csv"), row.names = FALSE)
write.csv(d_cs, file.path(tabelas_dir, "comparacao_equacao_fabrica_cs625.csv"), row.names = FALSE)
write.csv(metricas_fabrica, file.path(tabelas_dir, "metricas_equacao_fabrica_cs625.csv"), row.names = FALSE)
write.csv(planejamento, file.path(tabelas_dir, "planejamento_agua_vwc_0_a_60.csv"), row.names = FALSE)

cores <- c(pontos = "#17352F", linear = "#13796C", quadratica = "#E58B3D", fabrica = "#6750A4")
png(file.path(graficos_dir, "curvas_calibracao_vwc.png"), width = 2400, height = 900, res = 150)
par(mfrow = c(1, 3), mar = c(5.4, 5.2, 4.2, 1.0), las = 1)
for (nome in nomes_sensores) {
  d <- medias_preditas[medias_preditas$sensor_id == nome, ]
  x <- seq(min(d$leitura_media_sensor), max(d$leitura_media_sensor), length.out = 300)
  cf <- coeficientes[coeficientes$sensor_id == nome, ]
  lin <- cf[cf$modelo == "linear", ]; qua <- cf[cf$modelo == "quadratica", ]
  y_lin <- lin$intercepto + lin$coeficiente_leitura * x
  y_qua <- qua$intercepto + qua$coeficiente_leitura * x + qua$coeficiente_leitura_quadrado * x^2
  extra <- if (nome == sensor_cs625) -0.0663 - 0.0063 * x + 0.0007 * x^2 else numeric()
  ylim <- range(c(d$vwc_referencia_m3_m3, y_lin, y_qua, extra), finite = TRUE)
  unidade_x <- if (nome == sensor_cs625) "Período médio (µs)" else "Leitura média do sensor (%)"
  plot(d$leitura_media_sensor, d$vwc_referencia_m3_m3, pch = 19, col = cores["pontos"], ylim = ylim,
       xlab = unidade_x, ylab = "VWC de referência (m³ m⁻³)", main = nome)
  grid(col = "gray85"); lines(x, y_lin, col = cores["linear"], lwd = 2.2)
  lines(x, y_qua, col = cores["quadratica"], lwd = 2.5, lty = 2)
  leg <- c("média por ponto", "linear", "quadrática específica")
  cols <- c(cores["pontos"], cores["linear"], cores["quadratica"]); ltys <- c(NA, 1, 2); pchs <- c(19, NA, NA)
  if (nome == sensor_cs625) {
    lines(x, extra, col = cores["fabrica"], lwd = 2, lty = 3)
    leg <- c(leg, "quadrática de fábrica"); cols <- c(cols, cores["fabrica"]); ltys <- c(ltys, 3); pchs <- c(pchs, NA)
  }
  legend("topleft", bty = "n", cex = .73, legend = leg, col = cols, pch = pchs, lty = ltys, lwd = c(NA, rep(2.2, length(leg) - 1)))
}
dev.off()

png(file.path(graficos_dir, "residuos_modelos_recomendados.png"), width = 2400, height = 900, res = 150)
par(mfrow = c(1, 3), mar = c(5.4, 5.2, 4.2, 1.0), las = 1)
for (nome in nomes_sensores) {
  d <- medias_preditas[medias_preditas$sensor_id == nome, ]
  cfs <- coeficientes[coeficientes$sensor_id == nome & coeficientes$modelo_recomendado, ]
  modelo <- if (nrow(cfs)) cfs$modelo[1] else "quadratica"
  res <- if (modelo == "linear") d$residuo_linear_m3_m3 else d$residuo_quadratico_m3_m3
  plot(d$leitura_media_sensor, res, pch = 19, col = cores["pontos"],
       xlab = if (nome == sensor_cs625) "Período médio (µs)" else "Leitura média (%)",
       ylab = "Resíduo predito − referência (m³ m⁻³)", main = paste(nome, modelo))
  grid(col = "gray85"); abline(h = 0, lty = 2, col = "#A43D31")
}
dev.off()

recomendados <- coeficientes[coeficientes$modelo_recomendado, ]
linhas_modelos <- if (!nrow(recomendados)) "Nenhum modelo foi marcado como adotável." else unlist(lapply(seq_len(nrow(recomendados)), function(i) {
  d <- recomendados[i, ]
  sprintf("%s: %s | VWC = %.8f + %.8f × leitura + %.8f × leitura² | LOOCV RMSE = %.5f | faixa %.5f–%.5f",
          d$sensor_id, d$modelo, d$intercepto, d$coeficiente_leitura, d$coeficiente_leitura_quadrado,
          d$rmse_loocv_m3_m3, d$leitura_minima_faixa, d$leitura_maxima_faixa)
}))
alertas <- c(
  if (!all(pontos$geometria_exemplo_campbell_atendida)) "NOTA: recipiente abaixo do exemplo Campbell de 10 cm de diâmetro × 35 cm; curva válida somente para o arranjo ensaiado." else NULL,
  if (any(!pontos$continuidade_pesagem_ok)) paste0("ALERTA de continuidade entre pesagens nos pontos: ", paste(pontos$ordem_ponto[!pontos$continuidade_pesagem_ok], collapse = ", "), ". A referência continua sendo calculada pela massa após o equilíbrio em relação ao ponto 1.") else NULL,
  if (any(!pontos$balanco_hidrico_ok)) paste0("ALERTA no balanço entre água pesada e água acumulada adicionada nos pontos: ", paste(pontos$ordem_ponto[!pontos$balanco_hidrico_ok], collapse = ", "), ".") else NULL,
  if (abs(pontos$massa_base_menos_tara_solo_seco_g[1]) > pontos$tolerancia_pesagem_g[1]) "ALERTA: a massa zero do ponto 1 não coincide com tara + massa seca informadas." else NULL,
  if (any(!pontos$vwc_compativel_porosidade_estimada)) "ALERTA: VWC excede a porosidade mineral estimada em pelo menos um ponto." else NULL,
  if (any(is.na(pontos$temperatura_solo_c))) "ALERTA: temperatura do solo não foi registrada em todos os pontos." else NULL
)
if (!length(alertas)) alertas <- "Nenhum alerta de protocolo gerado."
writeLines(c(
  "CALIBRAÇÃO LABORATORIAL — VWC POR PESAGEM DA MESMA COLUNA", "",
  paste0("Ensaio: ", unique(leituras_brutas$ensaio_id)),
  sprintf("Geometria: %.2f cm de diâmetro × %.2f cm de solo = %.2f cm3.", pontos$diametro_interno_cm[1], pontos$altura_solo_cm[1], vol),
  sprintf("Massa zero do ponto 1: %.2f g; referência em cada ponto = massa após equilíbrio − massa zero.", massa_base_zero_g),
  sprintf("Densidade aparente seca: %.4f g/cm3; porosidade mineral estimada: %.4f.", pontos$densidade_aparente_seca_g_cm3[1], pontos$porosidade_estimada[1]),
  "CS625: período bruto em us; quadrática específica e crescente/côncava para cima.",
  "Outros sensores: porcentagem bruta; seleção linear/quadrática por LOOCV, forma física e parcimônia.",
  "Todos os sensores foram ajustados separadamente contra a mesma VWC independente do solo.",
  "LOOCV é validação interna; uma segunda sequência/coluna continua necessária para validação independente.",
  "Não extrapolar além das faixas observadas.", "", "Alertas:", alertas, "", "Modelos adotáveis:", linhas_modelos
), file.path(resumos_dir, "resumo_calibracao_vwc.txt"), useBytes = TRUE)
message("Concluído. Resultados em: ", saida_dir)
