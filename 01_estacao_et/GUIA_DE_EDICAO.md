# Guia — Estação ET

## Pastas

- `codigo/`: scripts R ativos e o downloader Python. `funcoes_estatisticas.R` centraliza métricas e bootstrap; os arquivos `analise_*.R` geram cada conjunto de saídas. Edite código aqui, mantendo os nomes de entrada esperados.
- `dados/entrada_atual/`: CSVs usados pelas análises correntes de temperatura, umidade, globo/radiação, IBUTG estimado, calibração e LoRa. Substitua os CSVs somente quando houver nova exportação compatível.
- `dados/serie_ambiental_longa/`: série ambiental mais longa disponível; ela serve à análise descritiva não validada e não deve ser confundida com a campanha de intercomparação.
- `resultados/<analise>/tabelas`: dados pareados, métricas, coeficientes e intervalos calculados.
- `resultados/<analise>/graficos`: figuras finais derivadas das tabelas.
- `resultados/<analise>/resumos`: leitura técnica da análise.

## Reexecução

Abra o projeto R na raiz original ou defina essa pasta como diretório de trabalho. Execute os scripts `analise_*.R` na ordem documentada no README copiado. Cada script sobrescreve somente seu próprio diretório de resultado.
