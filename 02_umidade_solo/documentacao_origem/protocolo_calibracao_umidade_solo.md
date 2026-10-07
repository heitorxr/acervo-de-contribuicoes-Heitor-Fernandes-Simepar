# Protocolo de calibração laboratorial — CS625, plugfild e azul/PC03

## Variável de referência

Os três sensores serão calibrados separadamente contra o **conteúdo volumétrico de água** (VWC, m³ m⁻³) da mesma coluna de solo. Nenhum sensor funciona como referência para os outros. Como o ponto 1 foi preparado sem adição de água, sua massa final define o zero hídrico experimental.

Com volume fixo e massa zero do ponto 1 conhecidos:

\[
m_{água,i}=m_{após,i}-m_{após,1},
\qquad
\theta_v=\frac{m_{água}}{V_{solo}}.
\]

Com massa em g, volume em cm³ e densidade da água aproximada por 1 g cm⁻³, o resultado é numericamente expresso em m³ m⁻³. A pesagem "antes" é feita antes de adicionar água; a pesagem "depois" é feita após adicionar, homogeneizar e equilibrar. A referência utiliza a segunda pesagem em relação ao ponto 1. A tara e a massa seca equivalente permanecem como controles independentes e são usadas no cálculo da densidade aparente.

A massa de solo deve ser seca em estufa até massa constante ou convertida para massa seca equivalente por subamostra. Massa apenas seca ao ar pode conter água residual e não pode ser tratada como massa seca sem correção.

## CS625 / CR350

O arquivo TOA5 do equipamento contém `Periodo_CS625_us` em `uSec` e `VWC_m3_m3`. Na calibração específica usa-se somente o **período bruto em µs** como preditor. A VWC pré-calculada pelo programa do CR350 não entra como referência.

O modelo primário do manual é:

\[
\theta_v=C_0+C_1P+C_2P^2,
\]

em que \(P\) é o período em µs. Para ser adotável na faixa ensaiada, a quadrática específica precisa ter \(C_2>0\), derivada positiva, resposta crescente e valores plausíveis. A equação padrão de fábrica (`−0,0663 − 0,0063P + 0,0007P²`) é exportada somente como diagnóstico comparativo.

O manual exige pelo menos quatro incrementos; este fluxo exige **cinco níveis distintos** para permitir LOOCV real. Cada ponto deve possuir ao menos três leituras do período.

## plugfild e azul/PC03

Enquanto não houver equação documentada dos fabricantes, as porcentagens brutas desses sensores são preditores empíricos:

\[
\theta_v=f(\text{porcentagem bruta do sensor}).
\]

O script compara modelos linear e quadrático por LOOCV, RMSE/MAE, resíduos, monotonicidade e plausibilidade. Quando ambos são elegíveis, só prefere a quadrática se sua LOOCV melhorar pelo menos 2%; caso contrário, escolhe a linear por parcimônia. A curva é específica do solo, densidade, recipiente, temperatura, geometria e faixa ensaiada.

## Mesmo solo, sensores um após o outro

A medição sequencial é aceitável somente se a referência permanecer essencialmente a mesma:

1. no ponto 1, registrar 0 g de água e pesar o conjunto para definir o zero;
2. nos demais pontos, pesar o conjunto antes da nova adição;
3. adicionar e pesar a água real;
4. homogeneizar e compactar até a marca fixa;
5. cobrir e aguardar o período do CS625 estabilizar;
6. pesar o conjunto após adição e equilíbrio; essa massa gera a VWC de referência;
7. ler um sensor de cada vez, registrando ordem e horário;
8. obter no mínimo três repetições por sensor;
9. depois de cada remoção, restaurar e recompactar a região perturbada, sem reutilizar vazio de haste;
10. cobrir entre as leituras.

O código exige ordens 1, 2 e 3 e verifica coerência temporal. A diferença entre a massa final de um ponto e a massa inicial do ponto seguinte é exportada como controle de continuidade. O balanço entre água acumulada adicionada e água de referência também é exportado para diagnosticar evaporação, derramamento, perda de solo ou diferenças de pesagem; esses diagnósticos não substituem a referência relativa ao ponto 1.

## Geometria informada

Para diâmetro interno de 8 cm e altura de solo de 29,5 cm:

\[
V=\pi(4)^2(29{,}5)=1.482{,}8317\ \text{cm}^3.
\]

As hastes do CS625 têm 300 mm, e o manual cita uma coluna de aproximadamente 10 cm de diâmetro por 35 cm de comprimento, além de exigir que o volume sensível esteja ocupado por solo e sem vazios junto às hastes. O script registra quando o recipiente é menor que esse exemplo, mas não bloqueia a curva somente pelas dimensões: o resultado deve ser interpretado como específico do arranjo de 8 × 29,5 cm, sem extrapolação para outra geometria.

## Planejamento de água

Para volume fixo de 1.482,8317 cm³, cada 5 pontos percentuais de VWC correspondem nominalmente a:

\[
1.482{,}8317\times0{,}05=74{,}1416\ \text{g de água}.
\]

O planejamento pressupõe água inicial igual a zero. Os valores efetivos da curva vêm das pesagens, não da meta. Pare antes de 60% se houver água livre, drenagem, extravasamento, alteração de volume ou aproximação da saturação.

## Fontes

1. Campbell Scientific. *CS616 and CS625 Water Content Reflectometers: Instruction Manual*, revisão 01/2026, Seção 8 e Apêndice D. https://s.campbellsci.com/documents/us/manuals/cs616.pdf
2. ISO 11465:1993. *Soil quality — Determination of dry matter and water content on a mass basis — Gravimetric method*. https://www.iso.org/standard/20886.html
3. ASTM D2216. *Standard Test Methods for Laboratory Determination of Water (Moisture) Content of Soil and Rock by Mass*. https://www.astm.org/d2216-19.html
4. Starr, J. L.; Paltineanu, I. C. Capacitance devices. In: Dane, J. H.; Topp, G. C. (eds.), *Methods of Soil Analysis, Part 4: Physical Methods*; procedimento citado pelo manual Campbell.
