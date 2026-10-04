# MEMÓRIA do projeto — tse_project

Este arquivo é o registro persistente das decisões lógicas de cada
procedimento do projeto. A cada nova etapa relevante, este arquivo deve ser
**atualizado** (não recriado) com uma nova seção, e consultado antes de
retomar qualquer script já documentado aqui.

---

## 2026-09-18 — `hexagon.R`: hexágonos H3 (res. 7, 8, 9) por seção eleitoral do DF

### Objetivo

Para cada linha da base de votação por seção do DF (TSE 2022, uma linha por
seção × cargo × candidato), descobrir em qual hexágono H3 (resoluções 7, 8 e
9) o local de votação cai, e então somar os votos de cada candidato por
hexágono.

### Por que mudou de abordagem (contexto)

Havia duas tentativas anteriores no projeto para achar a localização dos
locais de votação:

1. **`geocodificar_secoes_df.R` / `_teste_df.R`** — sem nenhuma coordenada
   pronta, precisou GEOCODIFICAR o endereço livre do TSE
   (`DS_LOCAL_VOTACAO_ENDERECO`) do zero: parsing de logradouro/número,
   casamento por nome com o Censo Escolar (para conseguir endereço
   estruturado), e só então rodar `{geocodebr}`. É um pipeline longo e
   sujeito a erro de casamento (ver comentários do próprio script sobre o
   risco de confundir "EC 41" com "EC 42").
2. **`geoloc_polling_places.R`** — um rascunho recente que tentava usar
   `read_polling_places()`. A primeira suposição foi que vinha do pacote
   `electionsBR`, mas a versão instalada (0.5.0) não exporta essa função.
   O usuário esclareceu que a função correta é **`geobr::read_polling_places()`**,
   que já entrega uma geometria de ponto (`sf`) pronta para cada local de
   votação — combinação da coordenada original do TSE com uma geocodificação
   de reforço via `{geocodebr}` feita pelo próprio pacote `geobr`.

Com o ponto em mãos, achar o hexágono H3 de cada linha vira um simples
*point-in-cell* (`h3jsr::point_to_cell`) — não há mais parsing de endereço
nem casamento fuzzy de nome. Esse é o sentido de "agora eu tenho os dados de
geometria, será muito mais fácil de localizar".

### Passo a passo (tutorial)

**Passo 1 — Ler a base nacional de votação e filtrar o DF**
A base `votacao_secao_2022_BR.csv` tem ~1,6 GB e cobre o Brasil inteiro.
Usamos `data.table::fread(..., select = cols_votacao)` para ler só as 26
colunas necessárias (evita carregar colunas que não usamos) e filtramos
`SG_UF == "DF"` depois da leitura — `fread` não faz *filter pushdown*, então
a leitura do arquivo inteiro é inevitável, mas selecionar colunas já reduz
bastante o custo. Resultado: 80.439 linhas (seção × cargo × candidato) no DF.
Também reconvertemos colunas de texto para UTF-8 com `enc2utf8()` — o mesmo
cuidado dos scripts anteriores, porque `fread(encoding = "Latin-1")` só
*marca* a codificação, sem retranscodificar os bytes.

**Passo 2 — Extrair os locais de votação únicos**
A geometria é por LOCAL de votação, não por linha de voto. Extraímos os
pares únicos `(NR_ZONA, NR_LOCAL_VOTACAO)` — 617 locais únicos no DF — para
juntar com o `geobr` só uma vez por local, em vez de repetir a junção 80 mil
vezes.

**Passo 3 — Baixar a geometria oficial (`geobr::read_polling_places`)**
`geobr::read_polling_places(year = 2022, code_muni = "DF", output = "sf")`
devolve um objeto `sf` com 3.729 linhas para o DF. O CRS é SIRGAS 2000
(EPSG:4674). A documentação do pacote explica a lógica de coordenadas: usa a
coordenada original do TSE quando ela difere menos de 800 m da geocodificação
via `{geocodebr}`; caso contrário, prioriza `{geocodebr}` se a precisão for
melhor que 800 m. A coluna `coords_source` registra qual fonte venceu.

**Passo 4 — Entender e resolver a duplicação em `geo_pool`**
3.729 linhas para só 554 combinações únicas de `(nr_zona,
nr_local_votacao_original)` — ou seja, ~6,7 linhas por local em média.
Investigando, a duplicação é por **seção**: o `geobr` repete a mesma
geometria uma vez por seção dentro do local (mesmo padrão de "um endereço,
várias seções" das bases do TSE). Verificamos que a coordenada é **idêntica**
em todas as linhas de um mesmo `(nr_zona, nr_local_votacao_original)` — zero
grupos com mais de 1 coordenada distinta — então é seguro deduplicar
(`!duplicated(...)`) sem perder informação espacial.

Nesse mesmo passo reprojetamos de SIRGAS 2000 (EPSG:4674) para WGS84
(EPSG:4326) **antes** de extrair `lon`/`lat` como colunas numéricas simples
— H3 espera coordenadas em WGS84, e as duas referências são quase
coincidentes no Brasil, mas o jeito correto é reprojetar explicitamente.

*Decisão técnica importante:* convertemos a geometria `sf` em colunas `lon`/
`lat` numéricas **logo depois de deduplicar**, e a partir daí trabalhamos só
com `data.table` puro. Na primeira versão do script, tentamos fazer o
`merge()` de `data.table` diretamente com o objeto `sf` (mantendo a coluna de
geometria) e isso quebrou com `Erro: Not compatible with requested type:
[type=NULL; target=double]` — `data.table::merge` não sabe lidar com a
coluna-lista de geometria do `sf`. Extrair `lon`/`lat` cedo evita esse
problema e também é mais rápido (junções de `data.table` em colunas
numéricas simples, sem carregar geometria).

**Passo 5 — Juntar os locais únicos do DF com a geometria deduplicada**
`merge(..., all.x = TRUE)` — *left join* que preserva todos os 617 locais do
DF, mesmo os que o `geobr` não cobriu (ficam com `lon`/`lat` `NA`). Resultado
da checagem de qualidade: **64 / 617 locais (10,4%) sem geometria no
`geobr`** — ficam de fora do cálculo de hexágono, mas continuam na base
final (só sem `h3_res7/8/9` preenchido).

**Passo 6 — Calcular o hexágono H3 de cada local único, nas 3 resoluções**
`h3jsr::point_to_cell(data.frame(lon, lat), res = c(7, 8, 9), simple = TRUE)`
numa única chamada para as 3 resoluções (mais barato do que 3 chamadas
separadas, porque a varredura dos pontos é reaproveitada). Usamos `h3jsr`
(não `h3r`, que só existia no stub antigo do `hexagon.R`) porque é o pacote
já usado em todos os outros scripts de mapa do projeto
(`mapa_h3_res9_presidencial_df.R`, `mapa_plano_piloto_h3.R` etc.) — mantém a
convenção do projeto.

**Passo 7 — Espalhar os hexágonos de volta para a base completa (nível seção)**
`merge(secoes_df, locais_hex, by = c("NR_ZONA", "NR_LOCAL_VOTACAO"), all.x =
TRUE)` — cada uma das 80.439 linhas de voto herda o hexágono do LOCAL a que
pertence. Essa é a base **`secoes_df_geo`** pedida: uma linha por seção ×
cargo × candidato, com `h3_res7`, `h3_res8` e `h3_res9` preenchidos sempre
que o local tinha geometria.

Cobertura obtida: **71.346 / 80.439 linhas (88,7%)** com hexágono nas 3
resoluções (o restante são as linhas dos 64 locais sem geometria no
`geobr`).

**Passo 8 — Salvar `secoes_df_geo`**
`arrow::write_parquet()` em `dados/secoes_df_geo.parquet` — sobrescreve o
parquet antigo (que vinha do pipeline de geocodificação por endereço) com
essa versão baseada em geometria oficial do `geobr`.

**Passo 9 — Agregar votos por candidato dentro de cada hexágono**
Para cada resolução, `secoes_df_geo[!is.na(h3_resN), .(votos = sum(QT_VOTOS)),
by = .(h3, NR_TURNO, CD_CARGO, DS_CARGO, NR_VOTAVEL, NM_VOTAVEL)]`.

*Decisão de formato:* em vez de gerar 3 tabelas separadas (uma por
resolução) ou um `dcast` fixo para um único cargo (como o mapa antigo fazia
só para Lula × Bolsonaro no 2º turno), empilhamos as 3 resoluções num único
formato **longo**, com uma coluna `resolucao`. Isso cobre "cada candidato"
de forma genérica — qualquer cargo (presidente, governador, senador etc.) e
qualquer turno ficam na mesma tabela, filtrável por quem for usar depois,
sem fixar de antemão qual comparação (ex: Lula × Bolsonaro) importa.

**Passo 10 — Salvar `votos_por_hexagono` e checagem de consistência**
`arrow::write_parquet()` em `dados/votos_por_hexagono.parquet`. Checagem:
a soma de `votos` é **idêntica nas 3 resoluções (3.258.546)** e bate
exatamente com a soma de `QT_VOTOS` das linhas de `secoes_df_geo` que têm
`h3_res9` preenchido — confirma que a agregação não perde nem duplica voto
algum entre os locais com geometria.

### Resultado final (verificado rodando o script)

| Resolução | Hexágonos distintos no DF |
|---|---|
| 7 | 169 |
| 8 | 335 |
| 9 | 503 |

Arquivos gerados:
- `dados/secoes_df_geo.parquet` — nível seção × cargo × candidato, com
  `h3_res7`, `h3_res8`, `h3_res9`.
- `dados/votos_por_hexagono.parquet` — nível hexágono × resolução × turno ×
  cargo × candidato, com `votos` somado.

### Limitações conhecidas (documentar para não redescobrir depois)

- **10,4% dos locais (64 de 617)** não têm geometria no `geobr` e ficam sem
  hexágono — não investigamos ainda a causa linha a linha (pode ser local
  novo, código divergente entre a base de votação e a base do `geobr`,
  etc.). Se precisar subir a cobertura, o próximo passo natural é comparar
  esses 64 `NR_LOCAL_VOTACAO` com a lista completa do `geobr` (sem o filtro
  `code_muni = "DF"`, caso algum tenha código de município levemente
  diferente) antes de recorrer de novo à geocodificação por endereço.
- `geobr::read_polling_places()` só retorna a data do 1º turno
  (`dt_eleicao == "02/10/2022"`); assumimos que o local físico não muda
  entre 1º e 2º turno no DF (não verificado exaustivamente).
- O download do `geobr::read_polling_places()` demorou ~3–4 min mesmo com
  `cache = TRUE` em execuções repetidas nesta sessão — parece não reaproveitar
  cache entre chamadas de `Rscript` separadas (cada processo novo re-baixa).
  Se for rodar o script muitas vezes, considere salvar `geo_pool` localmente
  (ex: `saveRDS`) numa etapa separada.

### Hábito a manter

A partir de agora, **todo procedimento relevante deste projeto deve ser
registrado aqui**, em formato tutorial (passo a passo + por quê), antes de
considerar a tarefa concluída. Ao retomar um script já documentado, reler a
seção correspondente deste arquivo primeiro.

---

## 2026-09-18 — `mapa_hexagonos_lula_bolsonaro.R`: mapa interativo (res. 7, 8, 9)

### Objetivo

Plotar no mapa os hexágonos calculados em `hexagon.R`: vermelho onde Lula
teve mais votos, azul onde Bolsonaro teve mais votos, sobre fundo
OpenStreetMap, com legenda explicando as cores, usando `{mapgl}`.

### Decisão principal: só plotar, não recalcular

`hexagon.R` já deixou prontas as duas peças que este script precisa:
`dados/secoes_df_geo.parquet` (hexágono por linha) e
`dados/votos_por_hexagono.parquet` (votos por candidato já somados por
hexágono, nas 3 resoluções, formato longo). Este script só lê essas
tabelas, filtra Presidente/2º turno/Lula(13)×Bolsonaro(22) e desenha — evita
duplicar a lógica de agregação (que já está validada, ver seção anterior)
num segundo lugar do código.

### Passo a passo (tutorial)

**Passo 1 — Carregar `votos_por_hexagono.parquet` e filtrar o recorte**
Filtra `CD_CARGO == 1` (Presidente), `NR_TURNO == 2`, `NR_VOTAVEL %in%
c(13, 22)` — 13 é Lula (PT) e 22 é Bolsonaro (PL), mesma convenção já usada
nos mapas presidenciais anteriores do projeto (`mapa_h3_res9_presidencial_df.R`,
`mapa_df_osm_res8.R`).

**Passo 2 — Medir cobertura**
O denominador de "% dos votos representado" vem de `secoes_df_geo.parquet`
(soma de `QT_VOTOS` do recorte, incluindo as linhas sem hexágono); o
numerador é a soma de `votos` em qualquer uma das 3 resoluções — dá o mesmo
valor porque uma linha tem hexágono nas 3 resoluções ao mesmo tempo ou em
nenhuma (mesma condição `!is.na(lon)` do local). Resultado:
**89,1% dos votos (Lula+Bolsonaro, 2º turno) representados**.

**Passo 3 — Tabela larga + polígonos por resolução**
Para cada resolução: `dcast(h3 ~ NR_VOTAVEL)` vira colunas `lula`/
`bolsonaro`; `vencedor` = quem tem mais votos no hexágono (`Empate` quando
iguais); `h3jsr::cell_to_polygon(simple = FALSE)` converte o ID do hexágono
em polígono `sf` para desenhar. Resultado (2º turno, Lula×Bolsonaro):

| Resolução | Hexágonos | Bolsonaro | Lula |
|---|---|---|---|
| 7 | 169 | 154 | 15 |
| 8 | 335 | 304 | 31 |
| 9 | 503 | 460 | 43 |

**Passo 4 — Uma única camada por resolução, com controle de camadas**
Em vez de 3 mapas HTML separados, as 3 resoluções entram como 3
`add_fill_layer()` na MESMA instância de `maplibre()`, cada uma com sua
própria fonte (`add_source`). `add_layers_control()` (posição `top-right`)
lista as 3 como itens ligáveis/desligáveis — resolução 8 começa `visibility
= "visible"`, 7 e 9 começam `"none"` (ocultas, mas a um clique). Assim um
único arquivo cobre as 3 resoluções pedidas, sem forçar o usuário a escolher
uma de antemão.

**Passo 5 — Fundo OpenStreetMap e enquadramento**
`maplibre(style = openfreemap_style("liberty"))` é o padrão já estabelecido
no projeto para "fundo OpenStreetMap" (OpenFreeMap serve tiles vetoriais
derivados do OSM; o rodapé do mapa mostra "Data from OpenStreetMap").
Reaproveitamos a mesma decisão de `mapa_df_osm_res8.R`: `center` + `zoom`
fixos (9.6) em vez de `fit_bounds()`, porque com o DF inteiro o
`fit_bounds()` trava `save_map()`/`chromote` esperando a câmera "assentar"
antes do screenshot.

**Passo 6 — Legenda**
`add_categorical_legend()` com só duas entradas — `Lula` (`#c0392b`,
vermelho) e `Bolsonaro` (`#2563eb`, azul) — exatamente como pedido; os
hexágonos de empate continuam pintados de cinza no mapa mas não entram na
legenda (mesmo padrão dos mapas anteriores do projeto).

**Passo 7 — Salvar HTML interativo + PNG de conferência**
`saveWidget(..., selfcontained = TRUE)` gera
`mapa_hexagonos_lula_bolsonaro.html` (abrir num navegador para interagir
com o controle de camadas e os tooltips). `save_map()` tira um PNG estático
da camada visível no momento (resolução 8) para conferência rápida sem abrir
o navegador: `mapa_hexagonos_lula_bolsonaro.png`.

### Resultado verificado

Rodei o script de ponta a ponta: gerou o HTML e o PNG sem erro (só um aviso
inofensivo do `jsonlite` sobre vetor nomeado). Conferi visualmente o PNG:
fundo OpenStreetMap visível, hexágonos vermelhos concentrados perto do
centro de Brasília, azuis predominando no restante do DF (compatível com o
resultado conhecido da eleição de 2022 no DF), legenda no canto inferior
esquerdo com as cores corretas.

### Observação para o futuro

Se um dia quiser comparar Lula×Bolsonaro noutro cargo/turno (ex: 1º turno,
ou governador), é só trocar o filtro do Passo 1 — a base
`votos_por_hexagono.parquet` já tem todos os cargos/turnos/candidatos, não
só presidencial.
