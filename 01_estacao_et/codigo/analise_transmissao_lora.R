# ============================================================
# Analise de disponibilidade temporal do registro ponta a ponta
#
# Uso:
#   source("analise_transmissao_lora.R")
#
# Este script avalia a regularidade dos registros presentes no CSV final.
# A analise usa slots nominais de 15 minutos e quantifica disponibilidade
# global, por dia, hora e janelas. Uma ausencia pode ter ocorrido no sensor,
# enlace LoRa, repetidor, receptor, Wi-Fi, requisicao HTTP ou planilha; sem
# logs intermediarios, ela nao e atribuida exclusivamente ao radio.
# ============================================================

pasta_dados <- "dados/entrada"
pasta_resultados <- file.path("resultados", "transmissao_lora")
pasta_tabelas <- file.path(pasta_resultados, "tabelas")
pasta_graficos <- file.path(pasta_resultados, "graficos")
pasta_resumos <- file.path(pasta_resultados, "resumos")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_et <- file.path(pasta_dados, "Dados Sensor ET - Página1.csv")

saida_slots <- file.path(pasta_tabelas, "slots_transmissao_lora.csv")
saida_resumo_global <- file.path(pasta_tabelas, "resumo_global_transmissao_lora.csv")
saida_cobertura_diaria <- file.path(pasta_tabelas, "cobertura_diaria_lora.csv")
saida_cobertura_horaria <- file.path(pasta_tabelas, "cobertura_horaria_lora.csv")
saida_janelas <- file.path(pasta_tabelas, "cobertura_janelas_horarias_lora.csv")
saida_lacunas <- file.path(pasta_tabelas, "lacunas_temporais_lora.csv")
saida_status <- file.path(pasta_tabelas, "status_recepcao_lora.csv")
saida_resumo <- file.path(pasta_resumos, "resumo_transmissao_lora.txt")
saida_grafico_diario <- file.path(pasta_graficos, "grafico_perda_diaria_lora.png")
saida_grafico_horario <- file.path(pasta_graficos, "grafico_perda_por_hora_lora.png")
saida_grafico_janelas <- file.path(pasta_graficos, "grafico_perda_janelas_horarias_lora.png")
saida_grafico_lacunas <- file.path(pasta_graficos, "grafico_lacunas_lora.png")
saida_grafico_heatmap <- file.path(pasta_graficos, "grafico_heatmap_perda_hora_dia_lora.png")
saida_grafico_status <- file.path(pasta_graficos, "grafico_status_recepcao_lora.png")

intervalo_min <- 15
intervalo_seg <- intervalo_min * 60

# Janelas horarias para testar a hipotese de relacao com maior trafego/circulacao.
# Os limites usam hora decimal em [0, 24). A hipotese atual considera manha,
# comeco da tarde e meio da tarde de dias uteis como maior funcionamento da UFPR.
janelas_horarias <- data.frame(
  janela = c(
    "madrugada",
    "manha_uteis",
    "comeco_tarde_uteis",
    "meio_tarde_uteis",
    "fim_tarde",
    "noite"
  ),
  inicio_hora = c(0, 6, 12, 14, 17, 19),
  fim_hora = c(6, 12, 14, 17, 19, 24),
  hipotese_trafego_ufpr = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# ---------- Funcoes auxiliares ----------
fmt <- function(x, casas = 2) {
  format(round(x, casas), decimal.mark = ",", nsmall = casas, trim = TRUE)
}

parse_data_et <- function(x) {
  as.POSIXct(x, format = "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo")
}

alinhar_15min <- function(data_hora) {
  as.POSIXct(
    floor(as.numeric(data_hora) / intervalo_seg) * intervalo_seg,
    origin = "1970-01-01",
    tz = "America/Sao_Paulo"
  )
}

inicio_dia <- function(data_hora) {
  as.POSIXct(format(data_hora, "%Y-%m-%d 00:00:00"), tz = "America/Sao_Paulo")
}

fim_dia <- function(data_hora) {
  inicio_dia(data_hora) + 24 * 3600 - intervalo_seg
}

hora_decimal <- function(data_hora) {
  as.integer(format(data_hora, "%H")) + as.integer(format(data_hora, "%M")) / 60
}

is_dia_util <- function(data_hora) {
  # POSIXlt$wday: domingo=0, segunda=1, ..., sabado=6
  w <- as.POSIXlt(data_hora)$wday
  w >= 1 & w <= 5
}

classificar_janela <- function(hdec) {
  out <- rep(NA_character_, length(hdec))
  for (i in seq_len(nrow(janelas_horarias))) {
    sel <- hdec >= janelas_horarias$inicio_hora[i] & hdec < janelas_horarias$fim_hora[i]
    out[sel] <- janelas_horarias$janela[i]
  }
  out
}

resumir_slots <- function(df, grupos) {
  agg <- aggregate(
    cbind(esperado = rep(1, nrow(df)), recebido = as.integer(df$recebido), perdido = as.integer(!df$recebido)),
    by = df[grupos],
    FUN = sum
  )
  agg$perda_pct <- ifelse(agg$esperado > 0, 100 * agg$perdido / agg$esperado, NA)
  agg$cobertura_pct <- ifelse(agg$esperado > 0, 100 * agg$recebido / agg$esperado, NA)
  agg
}

# ---------- Leitura e preparacao ----------
if (!file.exists(arquivo_et)) stop("Arquivo ET nao encontrado: ", arquivo_et)

et <- read.csv(arquivo_et, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
if (!("Data e Horário" %in% names(et))) stop("Coluna 'Data e Horário' nao encontrada no CSV ET.")

et$data_hora_et <- parse_data_et(et[["Data e Horário"]])
et <- et[!is.na(et$data_hora_et), ]
if (nrow(et) == 0) stop("Nenhuma data valida encontrada no CSV ET.")

et$slot_15min <- alinhar_15min(et$data_hora_et)
et$status_recepcao <- if ("Código de Erro (ok)" %in% names(et)) trimws(as.character(et[["Código de Erro (ok)"]])) else NA_character_

inicio <- min(et$slot_15min, na.rm = TRUE)
fim <- max(et$slot_15min, na.rm = TRUE)
grade_slots <- seq(from = inicio, to = fim, by = intervalo_seg)

slots_recebidos <- unique(et$slot_15min)
slots <- data.frame(slot_15min = grade_slots)
slots$recebido <- slots$slot_15min %in% slots_recebidos
slots$data <- as.Date(slots$slot_15min, tz = "America/Sao_Paulo")
slots$hora <- as.integer(format(slots$slot_15min, "%H"))
slots$hora_decimal <- hora_decimal(slots$slot_15min)
slots$dia_semana_num <- as.POSIXlt(slots$slot_15min)$wday
slots$dia_semana <- weekdays(slots$slot_15min)
slots$dia_util <- is_dia_util(slots$slot_15min)
slots$janela_horaria <- classificar_janela(slots$hora_decimal)
slots$hipotese_trafego_ufpr <- slots$janela_horaria %in% janelas_horarias$janela[janelas_horarias$hipotese_trafego_ufpr] & slots$dia_util

leituras_por_slot <- aggregate(
  data_hora_et ~ slot_15min,
  data = et,
  FUN = length
)
names(leituras_por_slot)[2] <- "n_leituras_no_slot"
slots <- merge(slots, leituras_por_slot, by = "slot_15min", all.x = TRUE, sort = TRUE)
slots$n_leituras_no_slot[is.na(slots$n_leituras_no_slot)] <- 0
slots$leitura_extra_no_slot <- pmax(slots$n_leituras_no_slot - 1, 0)

# ---------- Resumos ----------
esperados <- nrow(slots)
recebidos <- sum(slots$recebido)
perdidos <- esperados - recebidos
cobertura_pct <- 100 * recebidos / esperados
perda_pct <- 100 * perdidos / esperados
leituras_total <- nrow(et)
slots_duplicados <- sum(slots$n_leituras_no_slot > 1)
leituras_extras <- sum(slots$leitura_extra_no_slot)

resumo_global <- data.frame(
  inicio_slot = inicio,
  fim_slot = fim,
  intervalo_min = intervalo_min,
  leituras_linhas_csv = leituras_total,
  slots_esperados = esperados,
  slots_com_leitura = recebidos,
  slots_sem_leitura = perdidos,
  cobertura_pct = cobertura_pct,
  perda_pct = perda_pct,
  slots_com_multiplas_leituras = slots_duplicados,
  leituras_extras_em_slots_duplicados = leituras_extras,
  stringsAsFactors = FALSE
)

cobertura_diaria <- resumir_slots(slots, c("data", "dia_semana", "dia_util"))
cobertura_diaria <- cobertura_diaria[order(cobertura_diaria$data), ]

cobertura_horaria <- resumir_slots(slots, c("hora", "dia_util"))
cobertura_horaria <- cobertura_horaria[order(cobertura_horaria$dia_util, cobertura_horaria$hora), ]

cobertura_janelas <- resumir_slots(slots, c("janela_horaria", "dia_util", "hipotese_trafego_ufpr"))
ordem_janelas <- match(cobertura_janelas$janela_horaria, janelas_horarias$janela)
cobertura_janelas <- cobertura_janelas[order(cobertura_janelas$dia_util, ordem_janelas), ]

# Agrupamento especial: slots de hipotese em dias uteis vs demais slots.
cobertura_hipotese <- resumir_slots(slots, c("hipotese_trafego_ufpr"))
cobertura_hipotese$grupo <- ifelse(cobertura_hipotese$hipotese_trafego_ufpr,
                                   "janelas_hipotese_dias_uteis",
                                   "demais_horarios")

# Status recebidos.
if (all(is.na(et$status_recepcao))) {
  status_recepcao <- data.frame(status_recepcao = NA_character_, n = nrow(et), percentual_linhas = 100)
} else {
  status_recepcao <- as.data.frame(table(et$status_recepcao), stringsAsFactors = FALSE)
  names(status_recepcao) <- c("status_recepcao", "n")
  status_recepcao$percentual_linhas <- 100 * status_recepcao$n / sum(status_recepcao$n)
  status_recepcao <- status_recepcao[order(-status_recepcao$n), ]
}

# Lacunas entre slots recebidos consecutivos.
slots_recebidos_ord <- sort(slots_recebidos)
if (length(slots_recebidos_ord) >= 2) {
  lacunas <- data.frame(
    inicio_ultima_leitura = slots_recebidos_ord[-length(slots_recebidos_ord)],
    fim_proxima_leitura = slots_recebidos_ord[-1]
  )
  lacunas$duracao_min <- as.numeric(difftime(lacunas$fim_proxima_leitura, lacunas$inicio_ultima_leitura, units = "mins"))
  lacunas$slots_ausentes <- pmax(round(lacunas$duracao_min / intervalo_min) - 1, 0)
  lacunas <- lacunas[lacunas$slots_ausentes > 0, ]
  lacunas$data_inicio <- as.Date(lacunas$inicio_ultima_leitura, tz = "America/Sao_Paulo")
  lacunas$hora_inicio <- as.integer(format(lacunas$inicio_ultima_leitura, "%H"))
  lacunas$dia_util_inicio <- is_dia_util(lacunas$inicio_ultima_leitura)
  lacunas <- lacunas[order(-lacunas$slots_ausentes, -lacunas$duracao_min), ]
} else {
  lacunas <- data.frame()
}

# Hora do dia com mais perdas: duas visoes, absoluta e percentual.
horaria_global <- resumir_slots(slots, c("hora"))
hora_mais_perdas_abs <- horaria_global[which.max(horaria_global$perdido), ]
hora_maior_perda_pct <- horaria_global[which.max(horaria_global$perda_pct), ]

horaria_uteis <- cobertura_horaria[cobertura_horaria$dia_util == TRUE, ]
hora_uteis_mais_perdas_abs <- horaria_uteis[which.max(horaria_uteis$perdido), ]
hora_uteis_maior_perda_pct <- horaria_uteis[which.max(horaria_uteis$perda_pct), ]

janela_uteis <- cobertura_janelas[cobertura_janelas$dia_util == TRUE, ]
janela_uteis_mais_perdas <- janela_uteis[which.max(janela_uteis$perdido), ]
janela_uteis_maior_perda_pct <- janela_uteis[which.max(janela_uteis$perda_pct), ]

# ---------- Escrita de tabelas ----------
write.csv2(slots, saida_slots, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(resumo_global, saida_resumo_global, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(cobertura_diaria, saida_cobertura_diaria, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(cobertura_horaria, saida_cobertura_horaria, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(cobertura_janelas, saida_janelas, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(lacunas, saida_lacunas, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(status_recepcao, saida_status, row.names = FALSE, fileEncoding = "UTF-8")

# ---------- Graficos ----------
png(saida_grafico_diario, width = 1400, height = 800)
par(mar = c(8, 5, 4, 2) + 0.1)
barplot(cobertura_diaria$perda_pct,
        names.arg = format(cobertura_diaria$data, "%d/%m"), las = 2,
        col = ifelse(cobertura_diaria$dia_util, "tomato", "steelblue"),
        ylab = "Slots ausentes (%)", main = "Ausencia diaria no registro final")
legend("topright", legend = c("Dia útil", "Fim de semana"), fill = c("tomato", "steelblue"), bty = "n")
grid(nx = NA, ny = NULL)
dev.off()

png(saida_grafico_horario, width = 1700, height = 950)
par(mar = c(6, 6, 4, 3) + 0.1)
ymax_horario <- max(c(horaria_global$perda_pct, horaria_uteis$perda_pct), na.rm = TRUE)
ylim_horario <- c(0, max(10, ymax_horario * 1.18))
plot(horaria_global$hora, horaria_global$perda_pct,
     type = "n", pch = 19, lwd = 2,
     xlim = c(0, 23), ylim = ylim_horario, xaxt = "n",
     xlab = "Hora do dia", ylab = "Slots ausentes (%)",
     main = "Ausencias no registro final por hora do dia")
usr <- par("usr")
rect(6, usr[3], 17, usr[4], col = rgb(1, 0.85, 0.85, 0.25), border = NA)
grid()
axis(1, at = 0:23, labels = sprintf("%02dh", 0:23), las = 2, cex.axis = 0.85)
lines(horaria_global$hora, horaria_global$perda_pct, type = "b", pch = 19, lwd = 2)
if (nrow(horaria_uteis) > 0) {
  lines(horaria_uteis$hora, horaria_uteis$perda_pct, type = "b", pch = 17, col = "red", lwd = 2)
}
abline(v = c(6, 12, 14, 17), col = "gray60", lty = 3)
legend("topright",
       legend = c("Todos os dias", "Dias úteis", "Hipótese: manhã + começo/meio da tarde"),
       col = c("black", "red", rgb(1, 0.65, 0.65, 0.8)),
       pch = c(19, 17, 15), lwd = c(2, 2, NA), bty = "n", cex = 0.9)
dev.off()

png(saida_grafico_janelas, width = 1400, height = 800)
par(mar = c(9, 5, 4, 2) + 0.1)
jan_uteis_plot <- cobertura_janelas[cobertura_janelas$dia_util == TRUE, ]
cols <- ifelse(jan_uteis_plot$hipotese_trafego_ufpr, "tomato", "gray70")
rotulos_janelas <- c(
  madrugada = "Madrugada",
  manha_uteis = "Manhã",
  comeco_tarde_uteis = "Começo da tarde",
  meio_tarde_uteis = "Meio da tarde",
  fim_tarde = "Fim da tarde",
  noite = "Noite"
)
barplot(jan_uteis_plot$perda_pct,
        names.arg = unname(rotulos_janelas[jan_uteis_plot$janela_horaria]),
        las = 2, col = cols,
        ylab = "Slots ausentes (%)",
        main = "Ausências no registro final por janela em dias úteis")
legend("topright", legend = c("Janelas exploratórias", "Demais janelas"),
       fill = c("tomato", "gray70"), bty = "n")
grid(nx = NA, ny = NULL)
dev.off()

png(saida_grafico_lacunas, width = 1900, height = 1100)
par(mar = c(5, 14, 4, 3) + 0.1, xpd = FALSE)
if (nrow(lacunas) > 0) {
  top_lacunas <- head(lacunas, 20)
  labels <- paste(
    format(top_lacunas$inicio_ultima_leitura, "%d/%m %H:%M"),
    "→",
    format(top_lacunas$fim_proxima_leitura, "%d/%m %H:%M")
  )
  xmax_lacunas <- max(top_lacunas$slots_ausentes, na.rm = TRUE) * 1.18
  barplot(rev(top_lacunas$slots_ausentes),
          names.arg = rev(labels), horiz = TRUE, las = 1,
          xlim = c(0, xmax_lacunas), cex.names = 0.82,
          col = "darkorange", xlab = "Slots de 15 min ausentes",
          main = "Maiores lacunas temporais")
  grid(nx = NULL, ny = NA)
} else {
  plot.new(); title("Nenhuma lacuna temporal identificada")
}
dev.off()

png(saida_grafico_heatmap, width = 1400, height = 850)
mat <- xtabs(perdido ~ hora + data, data = resumir_slots(slots, c("data", "hora")))
x_idx <- seq_len(ncol(mat))
y_idx <- seq_len(nrow(mat))
image(x = x_idx, y = y_idx, z = t(mat),
      xlab = "Dia", ylab = "Hora", main = "Slots perdidos por hora e dia",
      col = colorRampPalette(c("white", "gold", "red", "darkred"))(20), axes = FALSE)
axis(1, at = x_idx, labels = format(as.Date(colnames(mat)), "%d/%m"), las = 2)
axis(2, at = y_idx, labels = rownames(mat), las = 1)
box()
dev.off()

png(saida_grafico_status, width = 1100, height = 750)
par(mar = c(7, 5, 4, 2) + 0.1)
barplot(status_recepcao$percentual_linhas,
        names.arg = status_recepcao$status_recepcao, las = 2,
        col = "steelblue", ylab = "Percentual das linhas recebidas (%)",
        main = "Status dos registros recebidos")
grid(nx = NA, ny = NULL)
dev.off()

# ---------- Resumo TXT ----------
sink(saida_resumo, split = TRUE)
cat("\nResumo: disponibilidade temporal do registro ponta a ponta\n")
cat("=========================================================\n")
cat("Arquivo analisado: ", arquivo_et, "\n", sep = "")
cat("Primeiro slot analisado: ", format(inicio), "\n", sep = "")
cat("Ultimo slot analisado:   ", format(fim), "\n", sep = "")
cat("Intervalo nominal:       ", intervalo_min, " min\n", sep = "")
cat("Linhas recebidas no CSV: ", leituras_total, "\n\n", sep = "")

cat("1) Cobertura global\n")
cat("-------------------\n")
cat("Slots esperados:       ", esperados, "\n", sep = "")
cat("Slots com leitura:     ", recebidos, "\n", sep = "")
cat("Slots sem registro:    ", perdidos, "\n", sep = "")
cat("Disponibilidade final: ", fmt(cobertura_pct, 2), " %\n", sep = "")
cat("Ausencia no registro:  ", fmt(perda_pct, 2), " %\n", sep = "")
cat("Slots com duplicidade: ", slots_duplicados, "\n", sep = "")
cat("Leituras extras em slots ja cobertos: ", leituras_extras, "\n\n", sep = "")

cat("2) Status dos registros recebidos\n")
cat("----------------------------------\n")
print(status_recepcao, row.names = FALSE)
cat("\n")

cat("3) Hora do dia com mais perdas\n")
cat("------------------------------\n")
cat("Considerando todos os dias:\n")
cat("- Maior numero absoluto de slots perdidos: hora ", hora_mais_perdas_abs$hora,
    "h, perdidos=", hora_mais_perdas_abs$perdido,
    ", perda=", fmt(hora_mais_perdas_abs$perda_pct, 2), " %\n", sep = "")
cat("- Maior percentual de perda: hora ", hora_maior_perda_pct$hora,
    "h, perdidos=", hora_maior_perda_pct$perdido,
    ", perda=", fmt(hora_maior_perda_pct$perda_pct, 2), " %\n\n", sep = "")
cat("Considerando apenas dias uteis:\n")
cat("- Maior numero absoluto de slots perdidos: hora ", hora_uteis_mais_perdas_abs$hora,
    "h, perdidos=", hora_uteis_mais_perdas_abs$perdido,
    ", perda=", fmt(hora_uteis_mais_perdas_abs$perda_pct, 2), " %\n", sep = "")
cat("- Maior percentual de perda: hora ", hora_uteis_maior_perda_pct$hora,
    "h, perdidos=", hora_uteis_maior_perda_pct$perdido,
    ", perda=", fmt(hora_uteis_maior_perda_pct$perda_pct, 2), " %\n\n", sep = "")

cat("4) Teste exploratorio da hipotese de trafego/circulacao na UFPR\n")
cat("----------------------------------------------------------------\n")
cat("Janelas tratadas como hipotese em dias uteis:\n")
print(janelas_horarias[janelas_horarias$hipotese_trafego_ufpr, c("janela", "inicio_hora", "fim_hora")], row.names = FALSE)
cat("\nCobertura por janelas horarias em dias uteis:\n")
print(janela_uteis[, c("janela_horaria", "esperado", "recebido", "perdido", "perda_pct", "cobertura_pct", "hipotese_trafego_ufpr")], row.names = FALSE)
cat("\nComparacao agregada: janelas da hipotese em dias uteis vs demais horarios:\n")
print(cobertura_hipotese[, c("grupo", "esperado", "recebido", "perdido", "perda_pct", "cobertura_pct")], row.names = FALSE)
cat("\nJanela de dias uteis com mais perdas absolutas: ", janela_uteis_mais_perdas$janela_horaria,
    " (", janela_uteis_mais_perdas$perdido, " slots perdidos; ", fmt(janela_uteis_mais_perdas$perda_pct, 2), " %)\n", sep = "")
cat("Janela de dias uteis com maior percentual de perda: ", janela_uteis_maior_perda_pct$janela_horaria,
    " (", janela_uteis_maior_perda_pct$perdido, " slots perdidos; ", fmt(janela_uteis_maior_perda_pct$perda_pct, 2), " %)\n\n", sep = "")

cat("Interpretacao cautelosa:\n")
cat("- O resultado mede disponibilidade no CSV final, nao taxa de sucesso exclusiva do enlace LoRa.\n")
cat("- A distribuicao horaria e exploratoria e nao prova causalidade nem interferencia por circulacao no campus.\n")
cat("- Sem logs locais em cada no e sem RSSI/SNR por pacote, nao e possivel separar falhas de sensor, alimentacao, LoRa, repetidor, receptor, Wi-Fi, HTTP ou planilha.\n")
cat("- A hipotese de influencia da atividade no campus exige repeticao, telemetria de radio e experimento controlado.\n\n")

cat("5) Maiores lacunas temporais\n")
cat("----------------------------\n")
if (nrow(lacunas) > 0) {
  print(head(lacunas[, c("inicio_ultima_leitura", "fim_proxima_leitura", "duracao_min", "slots_ausentes", "dia_util_inicio")], 15), row.names = FALSE)
} else {
  cat("Nenhuma lacuna temporal foi identificada.\n")
}
cat("\n")

cat("6) Arquivos de saida\n")
cat("--------------------\n")
cat("- ", saida_slots, "\n", sep = "")
cat("- ", saida_resumo_global, "\n", sep = "")
cat("- ", saida_cobertura_diaria, "\n", sep = "")
cat("- ", saida_cobertura_horaria, "\n", sep = "")
cat("- ", saida_janelas, "\n", sep = "")
cat("- ", saida_lacunas, "\n", sep = "")
cat("- ", saida_status, "\n", sep = "")
cat("- ", saida_resumo, "\n", sep = "")
cat("- ", saida_grafico_diario, "\n", sep = "")
cat("- ", saida_grafico_horario, "\n", sep = "")
cat("- ", saida_grafico_janelas, "\n", sep = "")
cat("- ", saida_grafico_lacunas, "\n", sep = "")
cat("- ", saida_grafico_heatmap, "\n", sep = "")
cat("- ", saida_grafico_status, "\n", sep = "")

sink()
