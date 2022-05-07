#--------------------------------------------------------------------------------------------------#
#                            Trabalho Final - Curso Visualização de Dados I
#                                         Script de Análise
#                                          Professor: Julio
#                                       Aluno: Marcio Vakassugu
#--------------------------------------------------------------------------------------------------#



# 1) Pacotes ----------------------------------------------------------------------------------

library(tidyverse)
library(readr)
library(ggtext)
library(kableExtra)
library(knitr)
library(ggrepel)

# 2) Leitura da Base de Dados -----------------------------------------------------------------

jogos_tabuleiro <- read_rds("data/base_preparada.RDS")

glimpse(jogos_tabuleiro)

# 3 Primeiro gráfico -------------------------------------------------------------------------

# gráfico dos jogos maiores ranckeados por categoria possíveis de serem jogdos por uma pessoa
# Considerações:
## 1) maiores rankeados definido pela variável rank
## 2) número mínimo de jogadores = 1
## 3) o número minimo de usuário avaliados deve ser igual a 1000
## 4) Vamos utilizar o tipo de gráfico "lollypop"


cor <- c( # definimos as cores dos "segment" e dos "circles"
    "#8599EA", "#7DE2D1", "#FBEEFB", "#D3EEDF", "#BC8DA7",
    "#F6EBCB", "#BACADE", "#C4B8CC", "#E77478", "#91F5AD",
    "#EFDA7E", "#BD2554", "#4EAFD4", "#0DF205", "#EFDFD0",
    "#D3EEDF", "#632A50", "#C4B8CC", "#7DE2D1", "#FF6465"
)

jogos_tabuleiro |>       # filtrar por usuários que avaliaram > 5000 e mínimo de jogadores igual a 1
    filter(users_rated >= 10000 & !is.na(rank) & min_players == 1) |>
    arrange(rank) |>                                             # ordenar do primeiro para o último
    slice_head(n = 20) |>                                                  # pega os vinte primeiros
    ggplot(aes(
        x = fct_reorder(name, rank, .desc = TRUE),          # inverter a ordem das colunas no eixo y
        y = rank,
        label = round(bayes_average, 2)
    )) +
    geom_segment(aes(
        x = fct_reorder(name, rank, .desc = TRUE),                          # cria as linhas/colunas
        xend = name,
        y = 0,
        yend = rank
    ),
    color = cor,                         # a cor das linhas/colunas são as definidas na paleta "cor"
    size = 1.5
    ) +
    geom_point(aes(color = year),                                                  # faz os círculos
               size = 9,
               color = cor                     # a cor dos círculos são as definidas na paleta "cor"
    ) +
    geom_label(
        size = 3,                      # definir os padrões do label (valores das médias bayesianas)
        alpha = 0,
        label.size = NA,                 # lobel.size = NA remove a moldura retangular sobre o label
        fontface = "bold",                                                           # fonte negrita
        color = "#000000"                                                     # cor do label "preto"
    ) +
    geom_curve(aes(                                   # define a linha curva para indicar a anotação
        x = 20.2,                                           # valor de x em que inicia a linha curva
        y = 2.5,                                            # valor de y em que inicia a linha curva
        xend = 16,                                         # valor de x em que termina a linha curva
        yend = 24                                          # valor de y em que termina a linha curva
    ),
    arrow = arrow(                                               # cria a seta no fim da linha curva
        type = "closed",                                                              # tipo da seta
        length = unit(0.02, "npc")                                                 # tamanho da seta
    ),
    curvature = -0.2,                          # curvatura da linha curva ( se =0 torna-se uma reta)
    color = "#8599EA"                                                           # cor da linha curva
    ) +
    labs(                                               # edita o texto do título, subtítulo e eixos
        title = 
        "Jogos de Tabuleiro para quem for jogar *<span style = 'color:#FFC745;'>sozinho!!</span>*
        ",
        subtitle =
            "
    Os vinte melhores jogos de tabuleiro para jogar sozinho e suas posições no ranking geral de jogos
    ",
        y = "
        Ranking dos Jogos",
        x = ""
    ) +
    ggdark::dark_theme_classic() +                                   # tema escolhido para o gráfico
    scale_y_continuous(breaks = seq(1, 44, 2)) +       # eixo y começa em 1, termina em 44 de 2 em 2
    theme(                                        # otimizamos o gráfico, alterando diversos fatores
        panel.grid.minor.y = element_blank(),                                         # tipo do grid
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "#00121F"),                         # fundo do painel
        plot.background = element_rect(fill = "#00121F"),                      # mesmo fundo do plot
        plot.title = element_markdown(
            size = 64,                                              # usamos o element tipo markdown
            face = "plain",                                                    # tiço da letra plain
            family = "FrankyOutline",                                              # fonte do título
            hjust = 0
        ),                                                   # posição horizontal do texto do título
        plot.subtitle = element_text(                             # define os fatores para os textos
            family = "LexieReadable-Regular",                         # define o tipo da fonte usada
            color = "#38CF6D",
            size = 9,
            hjust = 0.5
        ),
        axis.text.x = element_text(
            color = "#38CF6D",                                          # padrões do texto do eixo x
            size = 8,
            face = "bold"
        ),
        axis.ticks.x = element_line(color = "white"),                  # padrões dos ticks do eixo x
        axis.line.x = element_line(color = "white"),                          # eixo x na cor branca
        axis.text.y = element_markdown(
            color = "#FFFFFF",                                        # padrões das letras do eixo y
            size = 8,
            face = "bold",
            family = "LexieReadable-Regular"
        ),                                                                 
        axis.ticks.y = element_line(color = "white"),                  # padrões dos ticks do eixo x
        axis.line.y = element_line(
            color = "white",                                                  # eixo y na cor branca
            size = 0.4
        ),
        axis.title = element_text(
            face = "bold",                                    # padrões da letra do titulo do eixo x
            size = 9,
            color = "#38CF6D"
        ),
        legend.position = "none"                                       # exclui a legenda do gráfico
    ) +
    coord_flip() +                                                   # inverte o eixo x com o eixo y
    annotate(                                                              # faz anotação no gráfico
        "text",
        x = 14.2, y = 32, label = "Precisando jogar sozinho?
        Considerando o ranking, Gloomhavem é a
        melhor indicação. Ele ocupa a 1ª posição
        no rankig geral entre jogos de tabuleiro
        com nota de 8.51", 
        color = "#8599EA", 
        size = 2.7, 
        family = "LexieReadable-Regular",
        fontface = "bold"
    )

# 4) Segundo gráfico --------------------------------------------------------------------------

jogos_tabuleiro |>       # filtrar por usuários que avaliaram > 5000 e mínimo de jogadores igual a 1
    filter(users_rated >= 10000 & !is.na(rank) & min_players == 1) |>
    arrange(desc(wishing)) |>                                    # ordenar do primeiro para o último
    slice_head(n = 20) |>                                                  # pega os vinte primeiros
    ggplot(aes(
        x = fct_reorder(name, wishing, .desc = FALSE),      # inverter a ordem das colunas no eixo y
        y = wishing,
        label = round(rank, 2)
    )) +
    geom_segment(aes(
        x = fct_reorder(name, wishing, .desc = FALSE),                      # cria as linhas/colunas
        xend = name,
        y = 0,
        yend = wishing
    ),
    color = cor,                         # a cor das linhas/colunas são as definidas na paleta "cor"
    size = 1.5
    ) +
    geom_point(aes(color = year),                                                  # faz os círculos
               size = 8.2,
               color = cor                     # a cor dos círculos são as definidas na paleta "cor"
    ) +
    geom_label(
        size = 3,                      # definir os padrões do label (valores das médias bayesianas)
        alpha = 0,
        label.size = NA,                 # lobel.size = NA remove a moldura retangular sobre o label
        fontface = "bold",                                                           # fonte negrita
        color = "#000000"                                                     # cor do label "preto"
    ) +
    geom_curve(aes(                                   # define a linha curva para indicar a anotação
        x = 20,                                             # valor de x em que inicia a linha curva
        y = 19900,                                          # valor de y em que inicia a linha curva
        xend = 5.5,                                        # valor de x em que termina a linha curva
        yend = 19000                                       # valor de y em que termina a linha curva
    ),
    arrow = arrow(                                               # cria a seta no fim da linha curva
        type = "closed",                                                              # tipo da seta
        length = unit(0.02, "npc")                                                 # tamanho da seta
    ),
    curvature = -0.2,                          # curvatura da linha curva ( se =0 torna-se uma reta)
    color = "#8599EA"                                                           # cor da linha curva
    ) +
    labs(                                               # edita o texto do título, subtítulo e eixos
        title = "Jogos de Tabuleiro para quem for jogar *<span style = 'color:#FFC745;'>sozinho!!</span>*",
        subtitle ="Os vinte jogos mais desejados com suas posições no ranking geral",
        y = "Ranking dos Jogos",
        x = ""
    ) +
    ggdark::dark_theme_classic() +                                   # tema escolhido para o gráfico
    scale_y_continuous(breaks = seq(0, 22000, 5000)) + # eixo y começa em 1, termina em 44 de 2 em 2
    theme(                                        # otimizamos o gráfico, alterando diversos fatores
        panel.grid.minor.y = element_blank(),                                         # tipo do grid
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "#00121F"),                         # fundo do painel
        plot.background = element_rect(fill = "#00121F"),                      # mesmo fundo do plot
        plot.margin = unit(c(1, 1, 1, -0.3), "cm"),     # aumenta a distância do gráfico das margens
        plot.title = element_markdown(
            size = 42,                                              # usamos o element tipo markdown
            face = "plain",                                                    # tiço da letra plain
            family = "FrankyOutline",                                              # fonte do título
            hjust = 0,
            margin = unit(c(0, 0, 0.5, 0), "cm")                      # afasta o título do subtítulo
        ),                                                   # posição horizontal do texto do título
        text = element_text(                                      # define os fatores para os textos
            family = "LexieReadable-Regular",                         # define o tipo da fonte usada
            color = "#38CF6D",
            size = 10,
            hjust = 0,
            face = "bold"
        ),
        axis.text.x = element_text(
            color = "#38CF6D",                                          # padrões do texto do eixo x
            size = 8,
            face = "bold",
            margin = unit(c(0.3, 0, 0.5, 0), "cm")    # afasta o texto do eixo x e do titulo do eixo
        ),
        axis.ticks.x = element_line(color = "white"),                  # padrões dos ticks do eixo x
        axis.line.x = element_line(color = "white"),                          # eixo x na cor branca
        axis.text.y = element_markdown(
            color = "#FFFFFF",                                        # padrões das letras do eixo y
            size = 8,
            face = "bold",
            family = "LexieReadable-Regular", 
        ),                                                                 
        axis.ticks.y = element_line(color = "white"),                  # padrões dos ticks do eixo x
        axis.line.y = element_line(
            color = "white",                                                  # eixo y na cor branca
            size = 0.4,
        ),
        axis.title = element_text(
            face = "bold",                                    # padrões da letra do titulo do eixo x
            size = 10,
            hjust = 0.5,
        ),
        legend.position = "none"                                       # exclui a legenda do gráfico
    ) +
    coord_flip() +                                                   # inverte o eixo x com o eixo y
    annotate(                                                              # faz anotação no gráfico
        "text",
        x = 4, y = 15500, label = "Ainda na dúvida? 
    Agora  temos  os  jogos  mais desejados  e  seus
    rankings.  Observe  que ainda  os  jogos  Scythe,
    Terraforming  Mars,  Gioomhaven,  Spirit  Island,
    Gala Project e Nemesis da lista acima continuam 
    em destaque, mas Wingspan e Everdell precisam
    ser observados!                                                      ",
        color = "#8599EA", size = 2.7,
        family = "LexieReadable-Regular",
        fontface = "bold")
        

# 5) Tabela de jogos solo selecionados --------------------------------------------------------

jogos_selecao01 <- c(
    "Gloomhaven",
    "Scythe",
    "Terraforming Mars",
    "Spirit Island",
    "Gaia Project",
    "Nemesis",
    "Wingspan",
    "Everdell",
    "A Feast for Odin",
    "Robinson Crusoe: Adventures on the Cursed Island"
)

selecao01 <- jogos_tabuleiro |>
    select(
        name,
        year,
        min_players,
        min_age,
        playing_time,
        min_play_time,
        max_play_time
    ) |>
    filter(jogos_tabuleiro$name %in% jogos_selecao01) |>
    kable() |>
    kable_styling(
        html_font = "get_schwifty",
        font_size = 12,
        position = "center",
        full_width = FALSE,
        htmltable_class = "lightable-hover"
    ) |> 
    column_spec(1,  bold = FALSE, background = "#00121F", color = "#8599EA") |> 
    column_spec(2,  bold = FALSE, background = "#00121F", color = "#8599EA") |>
    column_spec(3,  bold = FALSE, background = "#00121F", color = "#8599EA") |>
    column_spec(4,  bold = FALSE, background = "#00121F", color = "#8599EA") |>
    column_spec(5,  bold = FALSE, background = "#00121F", color = "#8599EA") |>
    column_spec(6,  bold = FALSE, background = "#00121F", color = "#8599EA") |>
    column_spec(7,  bold = FALSE, background = "#00121F", color = "#8599EA") |> 
    row_spec(1, align = "left") |> 
    row_spec(2, align = "left") |> 
    row_spec(3, align = "left") |> 
    row_spec(4, align = "left") |> 
    row_spec(5, align = "left") |> 
    row_spec(6, align = "left") |> 
    row_spec(7, align = "left") |>
    row_spec(8, align = "left") |> 
    row_spec(9, align = "left") |> 
    row_spec(10, align = "left")

selecao01


# 6) Tabela para acesso da descrição dos jogos ------------------------------------------------

descricao <- jogos_tabuleiro |> 
    filter(name == "Terraforming Mars") |> 
    select(description) |> 
    kable() |>
    kable_styling(
        html_font = "get_schwifty",
        font_size = 12,
        position = "center",
        full_width = FALSE,
        htmltable_class = "lightable-hover"
    )
    
descricao 


# 7) Grafico com novas variáveis --------------------------------------------------------------

# Melhores jogos para jogar sozinho por tema, considerando o ranking, os mais desejados, menor tempo
# médio de jogo

# 7.1) Vamos separar a coluna description em outras 5 colunas
jogos_tabuleiro_categoria <- jogos_tabuleiro |> 
    separate(col = board_game_category,
             into = c("category1",
                      "category2",
                      "category3",
                      "category4",
                      "category5"),
        sep = "\\,",
        remove = TRUE
    )

# 7.1) Vamos pivotar a base de wide para long e então analisar as categorias.

jogos_tabuleiro_categoria <- jogos_tabuleiro_categoria |> 
    pivot_longer(
        cols = c("category1", "category2", "category3",
                 "category4", "category5"),
        names_to = "category_number",
        values_to = "category",
        values_drop_na = TRUE
    )


#7.2) os melhores por categoria

## categoria - economic

best_for_category <- function(.category_name) {
    jogos_tabuleiro_categoria |> 
        select(name, min_play_time, wishing, category, rank, owned, min_players) |>
        filter(category == .category_name & min_play_time <=160 & min_players>=1 & owned >=5000) |> 
        arrange(desc(wishing)) |> 
        head(2) |>
        slice_max(wishing)
}

# second_best_for_category <- function(.category_name) {
#     jogos_tabuleiro_categoria |> 
#         select(name, min_play_time, wishing, category, rank, owned, min_players) |>
#         filter(category == .category_name & min_play_time <=120 & min_players>=1 & owned >=5000) |> 
#         arrange(desc(wishing)) |> 
#         head(2) |> 
#         slice_min(wishing) 
# }

anotar <- function(.best_category_name) {
    annotate("label",
             x = .best_category_name$wishing,
             y = .best_category_name$category,
             label = .best_category_name$name,
             hjust = -0.1,
             vjust = 0.5,
             size = 3,
             color = "#2B32FF",
             fill = "#FFEFB8",
             alpha = 0.8
    )
}

anotar_1_ranking <- function(.name_game) {
    annotate("label",
             x = .name_game$wishing,
             y = .name_game$category,
             label = .name_game$name,
             hjust = 1,
             vjust = -0.5,
             size = 3,
             color = "#2B32FF",
             fill = "#C1EEFF",
             alpha = 0.8
    )
}

anotar_2_ranking <- function(.name_game) {
    annotate("label",
             x = .name_game$wishing,
             y = .name_game$category,
             label = .name_game$name,
             hjust = 1,
             vjust = 1.4,
             size = 3,
             color = "#2B32FF",
             fill = "#C1EEFF",
             alpha = 0.8
    )
}


# categorias selecionadas
categorias <- c("Ancient", "Card Game", "City Building", "Civilization", "Economic",
                "Trains", "Deduction", "Party Game", "Spies/Secret Agents",
                "Word Game", "Environmental","Industry / Manufacturing", "Science Fiction",
                "Space Exploration", "Animals", "Farming", "Renaissance", "Fighting","Miniatures",
                "Fantasy", "Abstract Strategy", "Puzzle", "Dice", "Movies / TV / Radio theme",
                "Educational", "Humor", "Bluffing", "Adventure", "Exploration", 
                "Prehistoric", "Medical", "Modern Warfare", "Horror")
    
 # os melhores por categoria

best_ancient <- best_for_category(categorias[1])
best_card_game <- best_for_category(categorias[2])
best_city_building <- best_for_category(categorias[3])
best_civilization <- best_for_category(categorias[4])
best_economic <- best_for_category(categorias[5])
best_trains<- best_for_category(categorias[6])
best_deduction <- best_for_category(categorias[7])
best_party_game <- best_for_category(categorias[8])
best_spies_secrets_agents <- best_for_category(categorias[9])
best_word_game <- best_for_category(categorias[10])
best_environmental <- best_for_category(categorias[11])
best_industry_manufacturing <- best_for_category(categorias[12])
best_science_fiction <- best_for_category(categorias[13])
best_space_exploration <- best_for_category(categorias[14])
best_animals <- best_for_category(categorias[15])
best_farming <- best_for_category(categorias[16])
best_renaissance <- best_for_category(categorias[17])
best_fighting <- best_for_category(categorias[18])
best_miniatures <- best_for_category(categorias[19])
best_fantasy <- best_for_category(categorias[20])
best_abstract_strategy <- best_for_category(categorias[21])
best_puzzle <- best_for_category(categorias[22])
best_dice <- best_for_category(categorias[23])
best_movies_tv_radio_theme <- best_for_category(categorias[24])
best_educational <- best_for_category(categorias[25])
best_humor <- best_for_category(categorias[26])
best_bluffing <- best_for_category(categorias[27])
best_adventure <- best_for_category(categorias[28])
best_exploration <- best_for_category(categorias[29])
best_prehistoric <- best_for_category(categorias[30])
best_medical <- best_for_category(categorias[31])
best_modern_warfare <- best_for_category(categorias[32])
best_horror <- best_for_category(categorias[33])


# os melhores do ranking fora da lista
terraforming_mars <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Terraforming Mars") |> 
    arrange(desc(category)) |> 
    head(1)

gloomhaven <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Gloomhaven") |> 
    arrange(desc(category)) |> 
    head(1)

gloomhaven_jaws_of_the_lion <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Gloomhaven: Jaws of the Lion") |> 
    arrange(desc(category)) |> 
    head(1)

gaia_project <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Gaia Project") |> 
    arrange(desc(category)) |> 
    head(1)

dune_imperium <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Dune: Imperium") |> 
    arrange(desc(category)) |> 
    head(1)

nemesis <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Nemesis") |> 
    arrange(desc(category)) |> 
    head(1)

a_feast_for_odin<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "A Feast for Odin") |> 
    arrange(desc(category)) |> 
    head(1)

wingspan<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Wingspan") |> 
    arrange(desc(category)) |> 
    head(1)

everdell<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Everdell") |> 
    arrange(desc(category)) |> 
    head(1)


# seleção dos melhores por categoria, lista de desejo e tempo mínimo de jogo


jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players, trading, min_play_time) |>
    filter(category %in% categorias & min_play_time <=160 & min_players>=1 & owned >=5000) |> 
    arrange(desc(wishing)) |>
    ggplot() +
    aes(x=wishing, y = category, color = min_play_time) +        # define o mapeamento de x, y e cor
    geom_point(size = 2.4)+
    labs(title = "Jogos Destaques por Categoria para Jogar *<span style = 'color:#FFC745;'>sozinho!!</span>*",
         subtitle = "Os mais desejados considerando categoria, desejo de ganhar como presente e tempo mínimo de jogo",
         x = "Número de Pessoas que Desejam o Jogo",           # define o texto para o titulo eixo x
         y = "Categoria")+                                  # define o texto para o título do eixo y
    scale_x_continuous(breaks = seq(0, 20000,1000),            # eixo x - minimo, máximo e intervalo
                       limits = c(0,20000))+
    theme_bw()+                                                          # tema para o gráfico
    theme(                                                       # altera as características do tema
        panel.background = element_rect(fill = "#FFFFFF"),                         # fundo do painel
        plot.background = element_rect(fill = "#1F1F1F"),                      # mesmo fundo do plot
        plot.title = element_markdown(
            size = 30,                                              # usamos o element tipo markdown
            face = "plain",                                                    # tiço da letra plain
            family = "FrankyOutline",                                              # fonte do título
            hjust = 0.5
        ),                                                   # posição horizontal do texto do título
        plot.subtitle = element_text(                             # define os fatores para os textos
            family = "LexieReadable-Regular",                         # define o tipo da fonte usada
            color = "#38CF6D",
            size = 9,
            hjust = 0.5
        ),
        plot.margin = unit(c(1, 1, 1, 0), "cm"),     # aumenta a distância do gráfico das margens
        text = element_text(                                      # define os fatores para os textos
            family = "LexieReadable-Regular",                         # define o tipo da fonte usada
            color = "#38CF6D",
            size = 10,
            hjust = 0,
            face = "bold"
        ),
        axis.text.x = element_text(
            color = "#38CF6D",                                          # padrões do texto do eixo x
            size = 8,
            face = "bold",
            margin = unit(c(0.3, 0, 0.5, 0), "cm")    # afasta o texto do eixo x e do titulo do eixo
        ),
        axis.ticks.x = element_line(color = "white"),                  # padrões dos ticks do eixo x
        axis.line.x = element_line(color = "white"),                          # eixo x na cor branca
        axis.text.y = element_markdown(
            color = "#FFFFFF",                                        # padrões das letras do eixo y
            size = 8,
            face = "bold",
            family = "LexieReadable-Regular", 
        ),                                                                 
        axis.ticks.y = element_line(color = "white"),                  # padrões dos ticks do eixo x
        axis.line.y = element_line(
            color = "white",                                                  # eixo y na cor branca
            size = 0.4,
        ),
        axis.title = element_text(
            face = "bold",                                    # padrões da letra do titulo do eixo x
            size = 10,
            hjust = 0.5,
        ),
        legend.position = "right",
        legend.background = element_rect("#1F1F1F")
    )+
    
    scale_color_gradient2(low = "#4B21FF", mid = "black", high = "red",  # altera cor gradiente legenda
                          midpoint = mean(jogos_tabuleiro_categoria$min_play_time))+
    anotar(best_ancient)+
    anotar(best_card_game)+
    anotar(best_city_building)+
    anotar(best_civilization)+
    anotar(best_economic)+
    anotar(best_trains)+
    anotar(best_deduction)+
    anotar(best_party_game)+
    anotar(best_spies_secrets_agents)+
    anotar(best_word_game)+
    anotar(best_environmental)+
    anotar(best_industry_manufacturing)+
    anotar(best_science_fiction)+
    anotar(best_space_exploration)+
    anotar(best_animals)+
    anotar(best_farming)+
    anotar(best_renaissance)+
    anotar(best_fighting)+
    anotar(best_miniatures)+
    anotar(best_fantasy)+
    anotar(best_abstract_strategy)+
    anotar(best_puzzle)+
    anotar(best_dice)+
    anotar(best_movies_tv_radio_theme)+
    anotar(best_educational)+
    anotar(best_humor)+
    anotar(best_bluffing)+
    anotar(best_exploration)+
    anotar(best_prehistoric)+
    anotar(best_medical)+
    anotar(best_modern_warfare)+
    anotar(best_horror)+
    anotar_1_ranking(terraforming_mars)+
    anotar_1_ranking(gloomhaven)+
    anotar_1_ranking(gloomhaven_jaws_of_the_lion)+
    anotar_2_ranking(gaia_project)+
    anotar_1_ranking(nemesis)+
    anotar_1_ranking(a_feast_for_odin)+
    anotar_1_ranking(wingspan)+
    anotar_2_ranking(everdell)

