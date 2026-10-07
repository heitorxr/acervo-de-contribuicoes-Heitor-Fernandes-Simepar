# Guia dos modelos 3D

`.stl` é uma malha para impressão; `.FCStd`, `.3mf` e `.blend` são fontes de projeto; `.svg` é uma fonte vetorial auxiliar.

## Projetos

| Pasta | Conteúdo e finalidade |
|---|---|
| `01_abrigo_meteorologico` | Teto, base, copo, centralizadores, espaçadores e arruelas do abrigo. |
| `02_botoes_relevo` | Botões Meteoblue e SIMEPAR de 60 mm, completos e com relevos separados. |
| `03_relevo_sao_paulo_com_imas` | Relevo de São Paulo em 6 e 9 peças; `componentes_dazzling` pertence ao mesmo mapa. |
| `04_caixa_sonar_18650` | CAD FreeCAD, base, tampa e histórico da caixa de sonar para bateria 18650. |
| `05_estacao_et_e_globo_termico` | Globo térmico, suportes da estação ET e variantes históricas. |
| `06_sensor_de_deslizamento_graciosa` | Caixa, bateria, painéis solares, estabilizadores, sensor de umidade e suporte ESP do sensor de deslizamento da Graciosa. |
| `07_prancha` | Estrutura, direção, hidrodinâmica, coleta de água e propulsão da prancha. |
| `08_satelite` | Base angular de servo, rosca e engrenagens do mecanismo de satélite. |
| `09_antena` | Base, bola, escada e topo que formam o conjunto de antena. |
| `10_anemometro` | Capas V1–V3, encaixes V1–V3 e engrenagem do anemômetro. |
| `11_caixa_gps` | Corpos `caixinha_gps`, `caixa_gps2` e tampas V2–V6. |
| `12_bucha_ima` | Buchas para ímã V1–V4. |
| `13_caixa_datalog` | Caixa independente para datalogger. |
| `14_misselenius` | Última pasta: itens sem associação segura a projeto identificado. |

## Organização interna

### `06_sensor_de_deslizamento_graciosa`

- `bateria`, `caixa_quebraca`, `cogumelo`, `estabilizador`, `painel_solar` e `sensor_de_humidade` preservam os subsistemas e suas séries de versões;
- `suporte_esp/iteracoes` reúne as 14 revisões do suporte ESP que pertence a este sensor.

### `07_prancha`

- `hidrodinamica/barbatana`: barbatana V1;
- `estrutura/encaixes`: encaixes V1–V5;
- `direcao/leme`: base/template e encaixes de servo V1–V3;
- `coleta_de_agua`: coletor, engrenagens, tubo, quadrantes, testes, SVG e `acionamento_roda`, que contém os encaixes de suporte da engrenagem que movimenta a roda de coleta de água;
- `propulsao/base_motor`: variantes V05/V06, históricos de base e motor e testes de encaixe/engrenagem.

### `08_satelite`

- `base_do_servo_angular.stl` e `rosca.stl`: elementos de suporte e fixação;
- `engrenagem_satelite`: `eng_passo` e as cinco variações antes chamadas de engrenagem de radar, todas pertencentes ao mecanismo do satélite.

### `14_misselenius`

Os arquivos ficam diretamente na raiz da pasta, sem subpastas, porque não há relacionamento confirmado que justifique categorização adicional.

## Uso

- Escolher a revisão por compatibilidade física, não apenas por numeração.
- Verificar escala, orientação, suporte e encaixes no fatiador.
- Malhas identificadas como não estanques devem ser inspecionadas ou reparadas antes de impressão.
- Para alterar dimensões, priorizar CAD quando disponível; STLs são exportações de malha.
