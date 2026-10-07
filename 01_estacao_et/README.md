# Analise_ET

Projeto reprodutível para avaliar a estação ET por intercomparação com uma estação meteorológica de verificação próxima.

## Escopo

As análises:

- mantêm uma única observação ambiental por slot nominal de 15 minutos;
- preservam as duplicidades somente no diagnóstico do registro ponta a ponta;
- comparam temperatura e umidade em unidades equivalentes;
- avaliam a associação entre o aquecimento relativo do globo (`globo - Bulbo Seco`) e a radiação solar positiva;
- tratam o valor calculado com a aproximação de Stull como **índice térmico estimado**, não como IBUTG normativo;
- usam bootstrap em blocos temporais para intervalos de confiança;
- separam disponibilidade do CSV final de uma taxa de sucesso exclusiva do enlace LoRa.

## Estrutura

```text
Analise_ET/
├── dados/
│   └── entrada/                  # não incluída: séries confidenciais
├── resultados/
│   ├── umidade/
│   ├── temperatura/
│   ├── globo_radiacao/
│   ├── ibutg_stull/
│   ├── calibracao_empirica/
│   ├── transmissao_lora/
│   └── faixas_campanha/
│       └── {tabelas,graficos,resumos}/ conforme a análise
├── analise_umidade.R
├── analise_temperatura.R
├── analise_globo_radiacao.R
├── analise_ibutg_stull.R
├── analise_calibracao_empirica.R
├── analise_transmissao_lora.R
├── analise_faixas_campanha.R
├── funcoes_estatisticas.R
└── Analise_ET.Rproj
```

`funcoes_estatisticas.R` contém métricas pareadas, limites de concordância e bootstrap em blocos compartilhados pelos demais scripts.

## Arquivos de entrada

Os scripts esperam arquivos CSV de entrada, deliberadamente removidos do repositório público por conterem séries confidenciais.

Para uma nova campanha privada, forneça arquivos com esses nomes e esquemas de colunas. Os nomes históricos `Bulbo Umido (stull)` e `IBUTG` são preservados no código por compatibilidade.

## Execução reproduzível

Abra `Analise_ET.Rproj`, mantenha o diretório de trabalho na raiz do projeto e execute, nesta ordem:

```r
source("analise_umidade.R")
source("analise_temperatura.R")
source("analise_globo_radiacao.R")
source("analise_ibutg_stull.R")
source("analise_calibracao_empirica.R")
source("analise_transmissao_lora.R")
source("analise_faixas_campanha.R")
```

As saídas são gravadas em `resultados/<analise>/{tabelas,graficos,resumos}`. Cada execução sobrescreve as saídas correspondentes para que elas permaneçam sincronizadas com os CSVs de entrada.

## Interpretação cautelosa

- A estação meteorológica não é um padrão metrológico co-localizado.
- A comparação do índice reutiliza o mesmo globo ET e a mesma fórmula de Stull; portanto, é análise de sensibilidade, não validação independente de IBUTG.
- A defasagem globo--radiação é exploratória e depende do limite de interpolação e da amostra válida.
- Slots ausentes no CSV podem decorrer de qualquer etapa entre sensor, alimentação, rádio, repetidor, receptor, Wi-Fi, HTTP e planilha.
