# Resultados de umidade do solo — versão pública

Cada subpasta identifica uma campanha ou análise histórica. Este diretório contém tabelas agregadas, resumos e, quando disponíveis, gráficos de métricas, disponibilidade ou correlação por lag.

Não contém séries por timestamp, predições/resíduos por observação, leituras brutas de laboratório ou imagens que exponham essas observações. Esses materiais permanecem no ambiente privado.

- `analise_conjunta_tres_sensores/`: comparação histórica dos três sensores a partir de um TOA5 único.
- `comparacao_cs625_pc03_fim_de_semana/`: comparação exclusiva do fim de semana de 19–20/09/2026, com CS do TOA5 e PC03 SIMEPAR.
- `calibracao_laboratorial_cs625/`: coeficientes, métricas e diagnósticos agregados do ensaio de laboratório; não inclui as leituras do ensaio.

A comparação posterior de 02–07/10/2026 está em `../contribuicoes/validacao_pc01_pc03_cs_20261002_a_20261007/`, com instruções de reexecução e ressalvas específicas.

Os resultados históricos não devem ser confundidos com a contribuição de outubro. Reexecuções exigem as fontes privadas de cada campanha e revisão dos caminhos nos scripts de origem.
