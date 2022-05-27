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
library(plotly)

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
cor <- c(                                          # pallet de cores para os elementos gráficos.
    "#C2D9FF", "#871607", "#081121", "#EA250B", "#30292F",
    "#CCAA00", "#CB8589", "#811192", "#EF7B45", "#4BB3FD",
    "#735CDD", "#BD2554", "#829DB0", "#0DF205", "#37000A",
    "#201E1F", "#632A50", "#390099", "#F2542D", "#0A369D"
)

jogos_tabuleiro |>
    filter(users_rated >= 10000 & !is.na(rank) & min_players == 1) |>
    arrange(rank) |>                                                      # ordenar por ranking.
    slice_head(n = 20) |>                                           # vinte primeiros elementos.
    ggplot(aes(
        x = fct_reorder(name,
                        rank,
                        .desc = TRUE
        ),                                 # reordenar as colunas como fatores.
        y = rank,
        label = round(bayes_average, 2)
    )
    ) +
    geom_segment(aes(x = fct_reorder(name,
                                     rank,
                                     .desc = TRUE),                # criar o elemento segmento.
                     xend = name,
                     y = 0,
                     yend = rank
    ),
    color = cor,   
    size = 1.5
    ) +
    geom_point(aes(color = year),                                    # criar o elemento círculo.
               size = 8.5,
               color = cor    
    ) +
    geom_label(                                                            # formatar os labels.
        size = 3, 
        alpha = 0,
        label.size = NA,    
        fontface = "bold",    
        color = "#FFFFFF"  
    ) +
    geom_curve(aes(y = 2.5,                                      # formatar curva para anotação.
                   x = 20,  
                   xend = 17,     
                   yend = 25   
    ),
    arrow = arrow(                                        # formatar a seta da curva.
        type = "closed",          
        length = unit(0.02, "npc")     
    ),
    curvature = -0.1,  
    color = "#C2D9FF"   
    ) +
    labs(                                         # editar o texto do título, subtítulo e eixos.
        title = "Jogos de Tabuleiro para quem for jogar 
        *<span style = 'color:#D7CF07;'>sozinho!!</span>*",
        subtitle ="Os vinte melhores jogos com suas posições no ranking geral e notas médias",
        y = "Ranking dos Jogos",
        x = ""
    ) +
    theme_classic() +                                              # seleção do tema do gráfico.
    scale_y_continuous(breaks = seq(1, 46, 2),                      # formatar escala do eixo x.
                       limits = c(0,46)) +                    
    theme(                                                          # customizar:
        panel.background = element_rect(fill = "#237CA9"),                   # fundo do gráfico.
        plot.background = element_rect(fill = "#237CA9"),         # fundo da moldura retangular.
        plot.margin = unit(c(1, 2, 1, 0), "cm"),                     # distância das margens.
        plot.title = element_markdown(                                      # título do gráfico.
            size = 18,  
            face = "plain",    
            family = "",                                                      # fonte do título.
            hjust = 0,
            margin = unit(c(0, 0, 0.5, 0.5), "cm")                          # margens do título.
        ), 
        plot.subtitle = element_text(
            size = 10,
            family = "",   
            hjust = 0,
        ),
        text = element_text(                                                # textos do gráfica.
            family = "",    
            color = "#FFFFFF",
            size = 10,
            hjust = 0,
            face = "bold"
        ),
        axis.text.x = element_text(                                           # texto do eixo x.
            color = "#FFFFFF",       
            size = 7.5,
            face = "bold",
            margin = unit(c(0.3, 0, 0.5, 0), "cm")  
        ),
        axis.text.y = element_text(                                           # texto do eixo y.
            color = "#FFFFFF",    
            size = 9.5,
            face = "bold",
            family = "", 
            margin = unit(c(0, 0.5, 0, 0.5), "cm")
        ),
        axis.ticks.x = element_line(color = "#1F1F1F"),                         # ticks do eixo x.
        axis.line.x = element_line(color = "#1F1F1F"),                   # cor da linha do eixo x.
        axis.ticks.y = element_line(color = "#1F1F1F"),             # padrões dos ticks do eixo x.
        axis.line.y = element_line(
            color = "#000000",                                           # cor da linha do eixo x.
            size = 0.4,
        ),
        axis.title = element_text(                                    # texto do título do eixo x.
            face = "bold",    
            size = 10,
            hjust = 0.5,
        ),
        legend.position = "none"                                               # exclui a legenda.
    ) +
    coord_flip() +                                                       # inverte o eixo x e y.
    annotate( "text",
              x = 15, y = 33, label = " 
               Considerando o ranking, Gloomhavem é a
               melhor indicação. Ele ocupa a 1ª  posição
               no rankig geral entre jogos  de  tabuleiro  
               com a nota 8.51.                                          
              ",
              color = "#C2D9FF", 
              size = 3.2,
              family = "",
              fontface = "bold"
    )


# 4) Segundo gráfico --------------------------------------------------------------------------

jogos_tabuleiro |>
    filter(users_rated >= 10000 & !is.na(rank) & min_players == 1) |>
    arrange(desc(wishing)) |>
    slice_head(n = 20) |>
    ggplot(aes(
        x = fct_reorder(name,
                        wishing,
                        .desc = FALSE
        ),                     # reordenar as colunas como fatores.
        y = wishing,
        label = round(rank, 2)
    )
    )+
    geom_segment(aes(
        x = fct_reorder(name,
                        wishing,
                        .desc = FALSE
        ),                                          # criar o elemento segmento.
        xend = name,
        y = 0,
        yend = wishing
    ),
    color = cor,         
    size = 1.5
    ) +
    geom_point(aes(color = year),                                    # criar o elemento círculo.
               size = 8.5,
               color = cor                     
    ) +
    geom_label(                                                             # formata os labels.
        size = 3,    
        alpha = 0,
        label.size = NA,
        fontface = "bold",
        color = "#FFFFFF"
    ) +
    geom_curve(aes(                                              # formatar curva para anotação.
        x = 20,
        y = 21000,
        xend = 6,
        yend = 19200
    ),
    arrow = arrow(                                                   # formatar a seta da curva.
        type = "closed",                                                             
        length = unit(0.02, "npc")
    ),
    curvature = -0.4,                             # editar o texto do título, subtítulo e eixos.
    color = "#C2D9FF"
    ) +
    labs(                                         # editar o texto do título, subtítulo e eixos.
        title = "Jogos de Tabuleiro para quem for jogar *<span style = 'color:#FFC745;'>
        sozinho!!</span>*",
        subtitle ="Os vinte jogos mais desejados com suas posições e ranking",
        y = "Ranking dos Jogos",
        x = ""
    ) +
    theme_classic() +                                              # seleção do tema do gráfico.
    scale_y_continuous(breaks = seq(0, 27000, 2000),
                       limits = c(0, 27000)) +
    theme(                                                    # customizar:
        panel.background = element_rect(fill = "#237CA9"),                   # fundo do gráfico.
        plot.background = element_rect(fill = "#237CA9"),         # fundo da moldura retangular.
        plot.margin = unit(c(1, 1, 1, -0.3), "cm"),
        plot.title = element_markdown(                                      # título do gráfico.
            size = 18,
            face = "plain",
            family = "",
            hjust = 0,
            margin = unit(c(0, 0, 0.5, 0), "cm")
        ),
        plot.subtitle = element_text   (                                  # padrões do subítulo.
            size = 10,
            family = "",
            hjust = 0,
        ),
        text = element_text(                                                # textos do gráfica.
            family = "",
            color = "#FFFFFF",
            size = 10,
            hjust = 0,
            face = "bold"
        ),
        
        axis.text.x = element_text(                                           # texto do eixo x.
            color = "#FFFFFF",
            size = 7.5,
            face = "bold",
            margin = unit(c(0.3, 0, 0.5, 0), "cm")
        ),
        axis.text.y = element_text    (                                       # texto do eixo y.
            color = "#FFFFFF",
            size = 9.5,
            face = "bold",
            family = "",
        ),
        axis.ticks.x = element_line(color = "#1F1F1F"),                       # ticks do eixo x.
        axis.line.x = element_line(color = "#1F1F1F"),                 # cor da linha do eixo x.
        axis.ticks.y = element_line(color = "#1F1F1F"),           # padrões dos ticks do eixo x.
        axis.line.y = element_line(                                # padrões da linha do eixo x.
            color = "#000000",
            size = 0.4,
        ),
        axis.title = element_text(                                  # texto do título do eixo x.
            face = "bold",
            size = 10,
            hjust = 0.5,
        ),
        legend.position = "none"                                  # exclui a legenda do gráfico.
    ) +
    coord_flip() +                                              # inverte o eixo x com o eixo y.
    annotate(                                                         # faz anotação no gráfico.
        "text",
        x = 3.5, y = 19800, label = "
    Agora  temos  os  jogos  mais desejados  e seus
    rankings.  Observe  que os  jogos Scythe,           
    Terraforming  Mars,  Gloomhaven,  Spirit Island, 
    Gaia Project e Nemesis continuam em destaque,
    mas Wingspan e Everdell também se                  
    sobressaíram!                                                      
    ",
        color = "#C2D9FF", size = 3.1,
        family = "",
        fontface = "bold"
    )
        



# Os melhores por categoria

best_for_category <- function(.category_name) {
    jogos_tabuleiro_categoria |> 
        select(name, min_play_time, wishing, category, rank, owned, min_players) |>
        filter(category == .category_name & min_play_time <=160 & min_players ==1 & owned >=500)|> 
        arrange(desc(wishing)) |> 
        head(2) |>
        slice_max(wishing)
}

# Inserção do nome do melhor jogo por categoria no gráfico

anotar <- function(.best_category_name) {
    annotate("label",
             family = "",
             x = .best_category_name$wishing,
             y = .best_category_name$category,
             label = .best_category_name$name,
             hjust = 0,
             vjust = 0,
             size = 3.2,
             color = "#000000",
             fill = "#FFEFB8",
             alpha = 1,
             fontface = "bold"
    )
}

# Inserção do nome do jogo percente ao ranking geral

anotar_1_ranking <- function(.name_game) {
    annotate("label",
             family = "",
             x = .name_game$wishing,
             y = .name_game$category,
             label = .name_game$name,
             hjust = 1,
             vjust = -0.5,
             size = 3.2,
             color = "#000000",
             fill = "#C1EEFF",
             alpha = 1,
             fontface = "bold"
    )
}

# Segunda opção do nome do jogo percente ao ranking geral para evitar sobreposição no gráfico

anotar_2_ranking <- function(.name_game) {
    annotate("label",
             family = "",
             x = .name_game$wishing,
             y = .name_game$category,
             label = .name_game$name,
             hjust = 1,
             vjust = 1.4,
             size = 3.2,
             color = "#000000",
             fill = "#C1EEFF",
             alpha = 1,
             fontface = "bold"
    )
}


# Separar a coluna description em outras 5 colunas
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

# Pivotar a base de wide para long e então analisar as categorias.

jogos_tabuleiro_categoria <- jogos_tabuleiro_categoria |>
    pivot_longer(
        cols = c("category1", "category2", "category3",
                 "category4", "category5"),
        names_to = "category_number",
        values_to = "category",
        values_drop_na = TRUE
    )



# categorias selecionadas (Selecionamos algumas categorias)
categorias <- c("Ancient", 
                "Card Game",
                "City Building",
                "Civilization", 
                "Economic",
                "Trains", 
                "Deduction", 
                "Party Game",
                "Spies/Secret Agents",
                "Word Game",
                "Environmental",
                "Industry / Manufacturing",
                "Science Fiction",
                "Space Exploration",
                "Animals",
                "Farming",
                "Renaissance",
                "Fighting",
                "Miniatures",
                "Fantasy",
                "Abstract Strategy",
                "Puzzle",
                "Dice", 
                "Movies / TV / Radio theme",
                "Educational",
                "Humor",
                "Bluffing", 
                "Adventure", 
                "Exploration", 
                "Prehistoric",
                "Medical",
                "Modern Warfare",
                "Horror")

# Obtenção dos melhores por categoria por meio da função best_for_category()

best_ancient <- best_for_category(categorias[1])
best_card_game <- best_for_category(categorias[2])
best_city_building <- best_for_category(categorias[3])
best_civilization <- best_for_category(categorias[4])
best_economic <- best_for_category(categorias[5])
best_trains <- best_for_category(categorias[6])
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
best_movie <- best_for_category(categorias[24])
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

scythe<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Scythe") |> 
    arrange(desc(category)) |> 
    head(1)

pandemic<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Pandemic") |> 
    arrange(desc(category)) |> 
    head(1)

caverna_the_caves_farmers<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Caverna: The Cave Farmers") |> 
    arrange(desc(category)) |> 
    head(1)

the_7_continent<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "The 7th Continent") |> 
    arrange(desc(category)) |> 
    head(1)


le_havre<- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Le Havre") |> 
    arrange(desc(category)) |> 
    head(1)



jogos_tabuleiro_categoria |> 
    select(name, min_play_time,
           wishing,
           category, 
           rank, 
           owned, 
           min_players, 
           trading,
           min_play_time) |>
    filter(category %in% categorias & min_play_time <=160 & min_players ==1) |> 
    arrange(desc(wishing)) |>
    ggplot() +
    aes(x=wishing,
        y = category,
        color = min_play_time) +
    geom_point(size = 3)+
    labs(                                                  # texto de título, subtítulo e eixos.
        title = "Jogos Destaques por Categoria para Jogar *<span style = 'color:#FFC745;'>sozinho!!
    </span>*",
        subtitle = "Por categoria, desejo de ganhar como presente e tempo mínimo de jogo",
        x = "Número de Pessoas que Desejam o Jogo",                                 
        y = "")+                                                                    
    scale_x_continuous(breaks = seq(0, 20000,5000),                            # padrões eixo x.
                       limits = c(0,20000))+
    # scale_y_discrete(labels = str_wrap(                        # quebra do texto y com str_wrap.
    #     categorias,
    #     indent = 1,
    #     width = 20))+                                              
    theme_classic() +                                                     # tema para o gráfico.
    theme(                                                       # customiza:
        panel.background = element_rect(fill = "#002642"),                   # fundo do gráfico.
        plot.background = element_rect(fill = "#237CA9"),              # fundo do frame externo.
        plot.margin = unit(c(1, 1, 1, -0.3), "cm"),                        # margens do gráfico.
        plot.title = element_markdown(                                      # padrões do título.
            size = 24,
            face = "plain",
            family = "",
            hjust = 0,
            margin = unit(c(0.2, 0, 0.5, 0), "cm")
        ),                                                   
        plot.subtitle = element_text    (                                # padrões do subtitulo.
            color = "#FFFFFF",
            size = 12,
            hjust = 0,
            face = "bold"
        ),
        text = element_text(                                   # padrões dos elementos de texto.
            family = "",                         
            color = "#FFFFFF",
            size = 10,
            hjust = 0,
            face = "bold"
        ),
        axis.text.x = element_text(                                # padrões do texto do eixo x.
            color = "#FFFFFF",                                          
            size = 8,
            face = "bold",
            margin = unit(c(0.3, 0, 0.5, 0.5), "cm"),
            family = "",
        ),
        axis.ticks.x = element_line(color = "#000000"),           # padrões dos ticks do eixo x.
        axis.line.x = element_line(color = "#000000"),                   # eixo x na cor branca.
        axis.text.y = element_text(                                # padrões do texto do eixo y.
            family = "",
            color = "#FFFFFF",
            size = 11,
            face = "bold",
            margin = unit(c(0, 0.3, 0.3, 0), "cm"),
        ),                                                                 
        axis.ticks.y = element_line(color = "#000000"),           # padrões dos ticks do eixo y.
        axis.line.y = element_line(
            color = "#000000",
            size = 0.4,
        ),
        axis.title.x = element_text(                     # padrões da letra do titulo do eixo x.
            face = "bold",                                    
            size = 11,
            hjust = 0.5,
        ),
        legend.title = element_text(size = 9.2,        # padrões da texto do  titulo da legenda.
                                    color = "#FFFFFF", 
                                    face = "bold", 
                                    family = ""),
        legend.position = c(0.80,.89),              # posição da legenda para dentro do gráfico.
        legend.background = element_rect(fill = "#237CA9"),   # padrões para o fundo da legenda.
        legend.text = element_text(size = 6,                      # padrões do texto da legenda.
                                   color = "#FFFFFF",
                                   face = "bold"),
        legend.direction = "horizontal"                                    # posição da legenda.
    )+
    scale_color_gradient(low = "#F68DA0",             # padrões do escala do gradiente de cores.
                         # mid = "#FF708F",
                         high = "#840B21", 
                         # midpoint =11000,
                         name = "tempo mínimo")+                    # altera o nome da legenda.
    anotar(best_ancient)+    # chama a função anotar para inserir os nomes dos jogos no gráfico.
    #anotar(best_card_game)+
    # anotar(best_city_building)+
    # #anotar(best_civilization)+
    # anotar(best_trains)+
    # anotar(best_deduction)+
    # anotar(best_party_game)+
    # anotar(best_spies_secrets_agents)+
    # anotar(best_word_game)+
    anotar(best_environmental)+
    # anotar(best_industry_manufacturing)+
    # anotar(best_science_fiction)+
    # anotar(best_space_exploration)+
    # anotar(best_animals)+
    # anotar(best_farming)+
    # anotar(best_renaissance)+
    # anotar(best_fighting)+
    # anotar(best_miniatures)+
    # anotar(best_fantasy)+
    # anotar(best_abstract_strategy)+
    # anotar(best_puzzle)+
# anotar(best_dice)+
# anotar(best_educational)+
# anotar(best_humor)+
# anotar(best_bluffing)+
# anotar(best_exploration)+
# anotar(best_prehistoric)+
# anotar(best_modern_warfare)+
# anotar(best_horror)+
# anotar_1_ranking(terraforming_mars)+
# anotar_1_ranking(gloomhaven)+
# anotar_1_ranking(gloomhaven_jaws_of_the_lion)+
# anotar_2_ranking(gaia_project)+
# anotar_1_ranking(nemesis)+
# anotar_1_ranking(a_feast_for_odin)+
# anotar_1_ranking(wingspan)+
# anotar_2_ranking(everdell)+
# anotar_2_ranking(scythe)+
# anotar_1_ranking(pandemic)+
# anotar_1_ranking(dune_imperium)+
# anotar_1_ranking(caverna_the_caves_farmers)+
# anotar_1_ranking(the_7_continent)+
# anotar_1_ranking(le_havre)+
annotate("rect",                                     # cria retângulo ao redor das legendas.
         xmin = 13400, 
         xmax = 20000, 
         ymin = 28, 
         ymax = 32 ,
         fill = "#237CA9",
         color = "#FFFFFF", 
         alpha = 1)+
    annotate("rect",               # cria retângulo para legenda "mais desejados por categoria".
             xmin = 18700, 
             xmax = 19600, 
             ymin = 30, 
             ymax = 30.6,
             fill = "#C1EEFF", 
             alpha = 1, 
             color = "#1F1F1F", 
             size = 0.3)+
    annotate("rect",                  # cria retângulo para legenda "melhores no ranking geral".
             xmin = 18700, 
             xmax = 19600, 
             ymin = 30.6, 
             ymax = 31.2,
             fill = "#FFEFB8", 
             alpha = 1, 
             color = "#000000", 
             size = 0.3)+
    annotate("text",                     # cria texto da legenda "mais desejados por categoria".
             x = 16100, 
             y = 30.7, 
             label = "mais desejados por categoria",
             color = "#FFFFFF", family = "",
             size = 3.2, 
             fontface = "bold")+
    annotate("text",                        # cria texto da legenda "melhores no ranking geral".
             x = 16350, 
             y = 30.1, 
             label = "melhores no ranking geral",
             color = "#FFFFFF", family = "",
             size = 3.2,
             fontface = "bold")


# Função retorna dados filtrados considerando tempo mínimo de jogo, mínimo de jogdores, mínimo
# adquiridos e idade mínima
best_for_category_ages<- function(.category_name) {
    jogos_tabuleiro_categoria |> 
        select(name,
               min_play_time,
               wishing,
               category,
               rank,
               owned, 
               min_players, 
               min_age) |>
        filter(category == .category_name & min_play_time <=160 & min_players>=1 & owned >=5000) 
}

fluidPage(
    theme = bslib::bs_theme(bootswatch = "united"),         # alterar a aparência do aplicativo.
    sidebarLayout(                                                      # usar o layout sidebar.
        sidebarPanel(width = 12.0,                            # largura do painel de input.
                     sliderInput(                              # recebe intervalo idade pelo slider.
                         inputId = "idade_minima",
                         label = "Informe a idade mínima do jogador",
                         min = min(jogos_tabuleiro_categoria$min_age, na.rm = TRUE),
                         max = 30,                 # idade mínima máxima da base = 25, definimos 30.
                         step = 1,
                         value = 8,                         # valor inicial escolhido para o slider.
                         sep = " ",
                         width = "450px"                      # largura do slider dentro do sidebar.
                     ),
                     selectInput(                          # recebe a categoria do jogo pelo select.
                         inputId = "categoria_jogo",
                         label = "Forneça a categoria do jogo",
                         choices = c("Abstract Strategy",
                                     "Adventure", 
                                     "Ancient", 
                                     "Animals", 
                                     "Bluffing",
                                     "Card Game", 
                                     "City Building",
                                     "Civilization",
                                     "Deduction",
                                     "Dice",
                                     "Economic",
                                     "Environmental", 
                                     "Exploration", 
                                     "Fantasy",
                                     "Farming", 
                                     "Fighting",
                                     "Horror", 
                                     "Medical",
                                     "Movies / TV / Radio theme",
                                     "Party Game", 
                                     "Science Fiction", 
                                     "Space Exploration", 
                                     "Trains"),
                         width = "450px"                # largura do select input dentro do sidebar.
                     )
        ),
        mainPanel = mainPanel(width = 12,                    # largura do painel principal.
                              fluidRow(
                                  column(
                                      width = 12,                   # largura da coluna ocupada no mainPanel.
                                      offset = 0,
                                      output$p1 <- renderPlotly({                      # renderiza o gráfico.
                                          jogos_tabuleiro_categoria |> 
                                              select(name,
                                                     min_play_time, 
                                                     wishing, 
                                                     category, 
                                                     rank, 
                                                     owned, 
                                                     min_players, 
                                                     min_age) |>
                                              filter(category == input$categoria_jogo &
                                                         min_play_time <=2000 &
                                                         min_players >=1 &
                                                         owned >=5000 & 
                                                         min_age <= input$idade_minima) |>
                                              group_by(min_age) |>
                                              arrange(desc(wishing)) |> 
                                              head(100) |> 
                                              ggplot(aes(x = rank, 
                                                         y = wishing, 
                                                         color = min_play_time,
                                                         label = name
                                              )
                                              )+
                                              geom_point() +
                                              theme_classic()+
                                              labs(
                                                  title = "JOGOS MAIS DESEJADOS POR IDADE PARA JOGAR 
<span style = 'color:#D7CF07;'>SOZINHO!!</span>",
                                                  x = "Ranking",
                                                  y = "Quantidade de usuários que desejam o jogo
                                     "
                                              )+
                                              theme(              # customiza o gráfico nos elementos abaixo.
                                                  panel.background = element_rect(fill = "#FFFFFF"),
                                                  plot.background = element_rect(fill = "#237CA9"), 
                                                  plot.margin = unit(c(1.5, 1, 1, 1), "cm"),   
                                                  plot.title = element_markdown(           
                                                      size = 12,
                                                      face = "plain",
                                                      hjust = 0.5
                                                  ),
                                                  text = element_text(     
                                                      color = "#000000",
                                                      size = 10,
                                                      hjust = 0,
                                                      face = "bold"
                                                  ),
                                                  axis.text.x = element_text(  
                                                      color = "#FFFFFF",
                                                      size = 8,
                                                      face = "plain"
                                                  ),
                                                  axis.text.y = element_markdown( 
                                                      color = "#FFFFFF",
                                                      size = 8,
                                                      face = "plain",
                                                  ),
                                                  axis.title = element_text(
                                                      size = 9,
                                                      hjust = 0.5
                                                  ),
                                                  legend.title = element_text(size = 9.2,
                                                                              color = "#FFFFFF",
                                                                              face = "bold", 
                                                                              family = "arial"),
                                                  legend.background = element_rect(fill = "#237CA9"), 
                                                  legend.text = element_text(size = 6,
                                                                             color = "#FFFFFF",
                                                                             face = "bold"),
                                              )+
                                              scale_color_gradient(name = "tempo mínimo")+  
                                              scale_y_continuous(breaks = seq(0, 20000, 2000))+  
                                              scale_x_continuous(breaks = seq(0, 22000, 2000)) 
                                      }
                                      ),
                                      output$t1 <- renderText(                  # renderiza o texto na saída.
                                          glue::glue("O intervalo de 0 a {input$idade_minima} anos")
                                      )
                                  )
                              )
        )
        
    )
)



jogos_selecao01 <- c(                               # relação dos jogos citados neste trabalho.
    "A Feast for Odin", "Agricola", "Arkham Horror: The Card Game",
    "Azul", "Beyond the Sun", "Blood Rage", "Burgle Bros.", "Carcassonne: Hunters and Gatherers",
    "Caverna: The Cave Farmers", "Dead of Winter: A Crossroads Game", "Dune: Imperium",
    "Everdell", "Gaia Project", "Gloomhaven", "Gloomhaven: Jaws of the Lion",
    "Horrified", "Just One","Le Havre", "Race for the Galaxy", "Lorenzo il Magnifico",
    "Lost Ruins of Arnak", "Mage Knight Board Game", "Mansions of Madness: Second Edition",
    "Maracaibo", "Marvel Champions: The Card Game", "Memoir '44", "Mysterium", "Nemesis",
    "Neuroshima Hex! 3.0", "Pandemic Legacy: Season 0",
    "Robinson Crusoe: Adventures on the Cursed Island", "Root", "Russian Railroads", "Scrabble",
    "Scythe", "Spirit Island", "Telestrations", "Terra Mystica", "Terraforming Mars",
    "The 7th Continent", "The Castles of Burgundy", "Ticket to Ride", "Twilight Struggle", 
    "Ubongo", "Village", "Viticulture Essential Edition", "Wingspan", "7 Wonders",
    "878 Vikings: Invasions of England"
)

selecao01 <- jogos_tabuleiro |>          # seleção das colunas com as informações para a tabela.
    select(
        name,
        year,
        min_players,
        min_age,
        playing_time,
        min_play_time,
        max_play_time
    ) |>
    filter(jogos_tabuleiro$name %in% jogos_selecao01) |>  # filt. os dados de acordo com selecao1.
    arrange(name) |> 
    kable(col.names = c("nome", "ano", "min jogadores",  #renom. na tabela os nomes das variáveis.
                        "min idade", "tempo jogo", "min tempo jogo",
                        "max tempo jogo"),
          align = "l"
    ) |> 
    kable_styling(                                          # altera as configurações da tabela.
        bootstrap_options = c("striped", 
                              "condensed"
        ),
        html_font = "",
        font_size = 10,
        full_width = TRUE, 
        fixed_thead = list(enabled = TRUE,
                           background = "#EDF6FD"
        ),
    )|> 
    kable_classic_2() |> 
    column_spec(1,                                          # altera as configurações das colunas.
                bold = FALSE,
                width = "10cm",
    ) |>      
    column_spec(2, 
                bold = FALSE,
                width = "2cm"
    ) |>
    column_spec(3,
                bold = FALSE,
                width = "4cm"
    ) |> 
    column_spec(4,
                bold = FALSE,
                width = "4cm"
    ) |> 
    column_spec(5,
                bold = FALSE,
                width = "4cm"
    ) |> 
    column_spec(6,
                bold = FALSE,
                width = "10cm"
    ) |> 
    column_spec(7,
                bold = FALSE,
                width = "10cm"
    ) |> 
    footnote(general = "Base de dados Board Games - disponível na plataforma Kaggle.",
             footnote_as_chunk = TRUE,
             fixed_small_size = TRUE,
             general_title = "Fonte:",
    )
selecao01       



fluidPage(                                 # selecionar o nome do filme para acesso à descrição.
    # h4("Descrição do Jogo de Tabuleiro",
    #    style = "color: #00121F;
    #    background:#E9EBED;
    #    width:455px;
    #    line-height: 2.5;"),
    sidebarLayout(                               # input dos dados à esquerda e saída à direita.
        sidebarPanel = sidebarPanel(width = 5.0,     # painel para o input dos nomes dos filmes.
                                    div(style = "margin-right: 35px; 
                font-size:18;
                text-align:left;",                        # aumentar a margem direita do painel.
                                        div(style = "text-align:left;",
                                            div(style = "text-align:left;color:#000000;",
                                                shiny::selectInput(
                                                    inputId = "nome",                          # cria a chave para acesso aos dados.
                                                    label = "Jogo:" ,         # label no painel.
                                                    choices = jogos_selecao01
                                                )
                                            )
                                        )
                                    )
        ),
        mainPanel = mainPanel(        # vamos definir a saída dos dados à direita do inputPanel.
            fluidRow(
                column(
                    width = 12,
                    offset =0,
                    div(style = "background-color:#ecf4ff;
                        width:850px;
                        color:#F3F6F6;
                        font-weight:normal;
                        line-height:1.8;",
                        output$tab1 <- renderTable(                        # renderiza a tabela.
                            {
                                descricao <- jogos_tabuleiro |>   
                                    filter(name == input$nome) |> 
                                    select(description) |> 
                                    rename("Descrição" = "description") 
                                descricao
                            },
                            width = "850px", # largura do texto para ocupar todo a largura do relatório.
                        )
                    )
                ),
                width = 12
            ),
            position = "justify",
        )
    )
)

#############################################################################################

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

# Os melhores por categoria

best_for_category <- function(.category_name) {
    jogos_tabuleiro_categoria |> 
        select(name, min_play_time, wishing, category, rank, owned, min_players) |>
        filter(category == .category_name & min_play_time <=160 & min_players ==1 & owned >=500)|> 
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


# Separar a coluna description em outras 5 colunas
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

# Pivotar a base de wide para long e então analisar as categorias.

jogos_tabuleiro_categoria <- jogos_tabuleiro_categoria |>
    pivot_longer(
        cols = c("category1", "category2", "category3",
                 "category4", "category5"),
        names_to = "category_number",
        values_to = "category",
        values_drop_na = TRUE
    )



# categorias selecionadas (Selecionamos algumas categorias)
categorias <- c("Ancient", 
                "Card Game",
                "City Building",
                "Civilization", 
                "Economic",
                "Trains", 
                "Deduction", 
                "Party Game",
                "Spies/Secret Agents",
                "Word Game",
                "Environmental",
                "Industry / Manufacturing",
                "Science Fiction",
                "Space Exploration",
                "Animals",
                "Farming",
                "Renaissance",
                "Fighting",
                "Miniatures",
                "Fantasy",
                "Abstract Strategy",
                "Puzzle",
                "Dice", 
                "Movies / TV / Radio theme",
                "Educational",
                "Humor",
                "Bluffing", 
                "Adventure", 
                "Exploration", 
                "Prehistoric",
                "Medical",
                "Modern Warfare",
                "Horror")

# Obtenção dos melhores por categoria por meio da função best_for_category()

best_ancient <- best_for_category(categorias[1])
best_card_game <- best_for_category(categorias[2])
best_city_building <- best_for_category(categorias[3])
best_civilization <- best_for_category(categorias[4])
best_economic <- best_for_category(categorias[5])
best_trains <- best_for_category(categorias[6])
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
best_movie <- best_for_category(categorias[24])
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

a_feast_for_odin <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "A Feast for Odin") |> 
    arrange(desc(category)) |> 
    head(1)

wingspan <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Wingspan") |> 
    arrange(desc(category)) |> 
    head(1)

everdell <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Everdell") |> 
    arrange(desc(category)) |> 
    head(1)

scythe <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Scythe") |> 
    arrange(desc(category)) |> 
    head(1)

caverna_the_caves_farmers <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Caverna: The Cave Farmers") |> 
    arrange(desc(category)) |> 
    head(1)

the_7_continent <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "The 7th Continent") |> 
    arrange(desc(category)) |> 
    head(1)

spirit_island <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Spirit Island") |> 
    arrange(desc(category)) |> 
    head(1)

wingspan <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Wingspan") |> 
    arrange(desc(category)) |> 
    head(1)

arkhan_horror <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Arkhan Horror: The Card Game") |> 
    arrange(desc(category)) |> 
    head(1)

viticulture <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Viticulture Essential Edition") |> 
    arrange(desc(category)) |> 
    head(1)

robinson_crusoe <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Robinson Crusoe: Adventures on the Cursed Island") |> 
    arrange(desc(category)) |> 
    head(1)

agricola <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players) |>
    filter(name == "Agricola") |> 
    arrange(desc(category)) |> 
    head(1)



# seleção dos melhores por categoria, lista de desejo e tempo mínimo de jogo
# Os melhores por categoria

best_for_category <- function(.category_name) {
    jogos_tabuleiro_categoria |> 
        select(name, min_play_time, wishing, category, rank, owned, min_players) |>
        filter(category == .category_name & min_play_time <=160 & min_players==1 & owned >=5000)|> 
        arrange(desc(wishing)) |> 
        head(2) |>
        slice_max(wishing)
}

jogos_tabuleiro_categoria |> 
    select(name, min_play_time,
           wishing,
           category, 
           rank, 
           owned, 
           min_players, 
           trading,
           min_play_time) |>
    filter(category %in% categorias & min_play_time <=160 & min_players ==1) |> 
    arrange(desc(wishing)) |>
    ggplot() +
    aes(x=wishing,
        y = category,
        color = min_play_time) +
    geom_point(size = 3)+
    labs(                                                  # texto de título, subtítulo e eixos.
        title = "Jogos Destaques por Categoria para Jogar *<span style = 'color:#FFC745;'>sozinho!!
    </span>*",
        subtitle = "Por categoria, desejo de ganhar como presente e tempo mínimo de jogo",
        x = "Número de Pessoas que Desejam o Jogo",                                 
        y = "")+                                                                    
    scale_x_continuous(breaks = seq(0, 20600, 5000),                            # padrões eixo x.
                       limits = c(0, 20600))+
    theme_classic() +                                                     # tema para o gráfico.
    theme(                                                       # customiza:
        panel.background = element_rect(fill = "#002642"),                   # fundo do gráfico.
        plot.background = element_rect(fill = "#237CA9"),              # fundo do frame externo.
        plot.margin = unit(c(1, 1, 1, -0.3), "cm"),                        # margens do gráfico.
        plot.title = element_markdown(                                      # padrões do título.
            size = 24,
            face = "plain",
            family = "",
            hjust = 0,
            margin = unit(c(0.2, 0, 0.5, 0), "cm")
        ),                                                   
        plot.subtitle = element_text (                                   # padrões do subtitulo.
            color = "#FFFFFF", 
            size = 12,
            hjust = 0,
            face = "bold"
        ),
        text = element_text(                                   # padrões dos elementos de texto.
            family = "",                         
            color = "#FFFFFF",
            size = 10,
            hjust = 0,
            face = "bold"
        ),
        axis.text.x = element_text(                                # padrões do texto do eixo x.
            color = "#FFFFFF",                                          
            size = 8,
            face = "bold",
            margin = unit(c(0.3, 0, 0.5, 0.5), "cm"),
            family = "",
        ),
        axis.ticks.x = element_line(color = "#000000"),           # padrões dos ticks do eixo x.
        axis.line.x = element_line(color = "#000000"),                   # eixo x na cor branca.
        axis.text.y = element_text(                                # padrões do texto do eixo y.
            family = "",
            color = "#FFFFFF",
            size = 11,
            face = "bold",
            margin = unit(c(0, 0.3, 0.3, 0), "cm"),
        ),                                                                 
        axis.ticks.y = element_line(color = "#000000"),           # padrões dos ticks do eixo y.
        axis.line.y = element_line(
            color = "#000000",
            size = 0.4,
        ),
        axis.title.x = element_text(                     # padrões da letra do titulo do eixo x.
            face = "bold",                                    
            size = 11,
            hjust = 0.5,
        ),
        legend.title = element_text(size = 9.2,        # padrões da texto do  titulo da legenda.
                                    color = "#FFFFFF", 
                                    face = "bold", 
                                    family = ""),
        legend.position = c(0.78,.86),              # posição da legenda para dentro do gráfico.
        legend.background = element_rect(fill = "#237CA9"),   # padrões para o fundo da legenda.
        legend.text = element_text(size = 6,                      # padrões do texto da legenda.
                                   color = "#FFFFFF",
                                   face = "bold"),
        legend.direction = "horizontal"                                    # posição da legenda.
    )+
    scale_color_gradient(low = "#F68DA0",             # padrões do escala do gradiente de cores.
                         # mid = "#FF708F",
                         high = "#840B21", 
                         # midpoint =11000,
                         name = "tempo mínimo")+                    # altera o nome da legenda.
    anotar(best_ancient)+    # chama a função anotar para inserir os nomes dos jogos no gráfico.
    anotar(best_card_game)+
    anotar(best_city_building)+
    anotar(best_civilization)+
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
    anotar(best_educational)+
    anotar(best_humor)+
    anotar(best_bluffing)+
    anotar(best_exploration)+
    anotar(best_prehistoric)+
    anotar(best_modern_warfare)+
    anotar(best_horror)+
    anotar(best_humor)+
    anotar_1_ranking(terraforming_mars)+
    anotar_1_ranking(gloomhaven)+
    anotar_1_ranking(gloomhaven_jaws_of_the_lion)+
    anotar_2_ranking(gaia_project)+
    anotar_1_ranking(nemesis)+
    anotar_1_ranking(a_feast_for_odin)+
    anotar_2_ranking(everdell)+
    anotar_2_ranking(scythe)+
    anotar_1_ranking(dune_imperium)+
    anotar_1_ranking(caverna_the_caves_farmers)+
    anotar_1_ranking(the_7_continent)+
    anotar_1_ranking(arkhan_horror)+
    anotar_2_ranking(viticulture)+
    anotar_2_ranking(robinson_crusoe)+
    anotar_2_ranking(agricola)+
    annotate("rect",                                     # cria retângulo ao redor das legendas.
             xmin = 13150, 
             xmax = 20600, 
             ymin = 28, 
             ymax = 32 ,
             fill = "#237CA9",
             color = "#FFFFFF", 
             alpha = 1)+
    annotate("rect",               # cria retângulo para legenda "mais desejados por categoria".
             xmin = 18900, 
             xmax = 19900, 
             ymin = 30, 
             ymax = 30.6,
             fill = "#C1EEFF", 
             alpha = 1, 
             color = "#1F1F1F", 
             size = 0.3)+
    annotate("rect",                  # cria retângulo para legenda "melhores no ranking geral".
             xmin = 18900, 
             xmax = 19900, 
             ymin = 30.6, 
             ymax = 31.2,
             fill = "#FFEFB8", 
             alpha = 1, 
             color = "#000000", 
             size = 0.3)+
    annotate("text",                     # cria texto da legenda "mais desejados por categoria".
             x = 16100, 
             y = 30.7, 
             label = "mais desejados por categoria",
             color = "#FFFFFF", family = "",
             size = 3.2, 
             fontface = "bold")+
    annotate("text",                        # cria texto da legenda "melhores no ranking geral".
             x = 16350, 
             y = 30.1, 
             label = "melhores no ranking geral",
             color = "#FFFFFF", family = "",
             size = 3.2,
             fontface = "bold")


fluidPage(
    theme = bslib::bs_theme(bootswatch = "united"),         # alterar a aparência do aplicativo.
    sidebarLayout(                                                      # usar o layout sidebar.
        sidebarPanel(width = 12.0,                            # largura do painel de input.
                     sliderInput(                              # recebe intervalo idade pelo slider.
                         inputId = "idade_minima",
                         label = "Informe a idade mínima do jogador",
                         min = min(jogos_tabuleiro_categoria$min_age, na.rm = TRUE),
                         max = 30,                 # idade mínima máxima da base = 25, definimos 30.
                         step = 1,
                         value = 16,                         # valor inicial escolhido para o slider.
                         sep = " ",
                         width = "450px"                      # largura do slider dentro do sidebar.
                     ),
                     selectInput(                          # recebe a categoria do jogo pelo select.
                         inputId = "categoria_jogo",
                         label = "Forneça a categoria do jogo",
                         choices = c("Ancient", 
                                     "Card Game",
                                     "City Building",
                                     "Civilization", 
                                     "Economic",
                                     "Deduction", 
                                     "Spies/Secret Agents",
                                     "Word Game",
                                     "Science Fiction",
                                     "Animals",
                                     "Farming",
                                     "Renaissance",
                                     "Fighting",
                                     "Miniatures",
                                     "Fantasy",
                                     "Abstract Strategy",
                                     "Puzzle",
                                     "Dice", 
                                     "Movies / TV / Radio theme",
                                     "Educational",
                                     "Humor",
                                     "Bluffing", 
                                     "Adventure", 
                                     "Exploration", 
                                     "Prehistoric",
                                     "Medical",
                                     "Modern Warfare",
                                     "Horror"),
                         width = "450px"                # largura do select input dentro do sidebar.
                     )
        ),
        mainPanel = mainPanel(width = 12,                    # largura do painel principal.
                              fluidRow(
                                  column(
                                      width = 12,                   # largura da coluna ocupada no mainPanel.
                                      offset = 0,
                                      output$p1 <- renderPlotly({                      # renderiza o gráfico.
                                          jogos_tabuleiro_categoria |> 
                                              select(name,
                                                     min_play_time, 
                                                     wishing, 
                                                     category, 
                                                     rank, 
                                                     owned, 
                                                     min_players, 
                                                     min_age) |>
                                              filter(category == input$categoria_jogo &
                                                         min_players ==1 &
                                                         min_play_time <= 160,
                                                     min_age <= input$idade_minima) |>
                                              group_by(min_age) |>
                                              arrange(desc(wishing)) |> 
                                              #head(200) |> 
                                              ggplot(aes(x = rank, 
                                                         y = wishing, 
                                                         color = min_play_time,
                                                         label = name
                                              )
                                              )+
                                              geom_point() +
                                              theme_classic()+
                                              labs(
                                                  title = "JOGOS MAIS DESEJADOS POR IDADE PARA JOGAR 
<span style = 'color:#D7CF07;'>SOZINHO!!</span>",
                                                  x = "Ranking",
                                                  y = "Quantidade de usuários que desejam o jogo
                                     "
                                              )+
                                              theme(              # customiza o gráfico nos elementos abaixo.
                                                  panel.background = element_rect(fill = "#FFFFFF"),
                                                  plot.background = element_rect(fill = "#237CA9"), 
                                                  plot.margin = unit(c(1.5, 1, 1, 1), "cm"),   
                                                  plot.title = element_markdown(           
                                                      size = 12,
                                                      face = "plain",
                                                      hjust = 0.5
                                                  ),
                                                  text = element_text(     
                                                      color = "#000000",
                                                      size = 10,
                                                      hjust = 0,
                                                      face = "bold"
                                                  ),
                                                  axis.text.x = element_text(  
                                                      color = "#FFFFFF",
                                                      size = 8,
                                                      face = "plain"
                                                  ),
                                                  axis.text.y = element_markdown( 
                                                      color = "#FFFFFF",
                                                      size = 8,
                                                      face = "plain",
                                                  ),
                                                  axis.title = element_text(
                                                      size = 9,
                                                      hjust = 0.5
                                                  ),
                                                  legend.title = element_text(size = 9.2,
                                                                              color = "#FFFFFF",
                                                                              face = "bold", 
                                                                              family = "arial"),
                                                  legend.background = element_rect(fill = "#237CA9"), 
                                                  legend.text = element_text(size = 6,
                                                                             color = "#FFFFFF",
                                                                             face = "bold"),
                                              )+
                                              scale_color_gradient(name = "tempo mínimo")+  
                                              scale_y_continuous(breaks = seq(0, 20000, 2000))+  
                                              scale_x_continuous(breaks = seq(0, 22000, 2000)) 
                                      }
                                      ),
                                      output$t1 <- renderText(                  # renderiza o texto na saída.
                                          glue::glue("O intervalo de 0 a {input$idade_minima} anos")
                                      )
                                  )
                              )
        )
        
    )
)



# Gráfico considerando a faixa etária ---------------------------------------------------------

best_for_category_ages<- function(.category_name) {
    jogos_tabuleiro_categoria |> 
        select(name, min_play_time, wishing, category, rank, owned, min_players, min_age) |>
        filter(category == .category_name & min_play_time <=160 & min_players>=1 & owned >=5000) 
}

idade_minima <- 30
jogos_age <- jogos_tabuleiro_categoria |> 
    select(name, min_play_time, wishing, category, rank, owned, min_players, min_age) |>
    filter(category == "Economic" & min_play_time <=2000 & min_players >=1 & owned >=1000 & min_age <= idade_minima) |>
    group_by(min_age) |>
    arrange(desc(wishing)) |> 
    head(100) |> View()

p_age <- jogos_age |> 
    ggplot(aes(x = rank, y = wishing, color = min_age, label = name))+
    geom_point() +
    theme_classic()+
    labs(
        title = "Jogos mais desejados por <span style = 'color:#D7CF07;'>ranking geral</span> e <span style = 'color:#D7CF07;'>notas médias</span>",
        x = "Ranking",
        y = "Quantidade de usuários que desejam o jogo
        "
    )+
    
    theme(
        panel.background = element_rect(fill = "#FFFFFF"),                       # fundo do gráfico.
        plot.background = element_rect(fill = "#237CA9"),             # fundo da moldura retangular.
        plot.margin = unit(c(1, 1, 1, 1), "cm"),                         # distância das margens.
        plot.title = element_markdown(                                          # título do gráfico.
            size = 14,
            face = "plain",
            family = "FrankyOutline",                                             # fonte do título.
            hjust = 0.5
        ),
        plot.subtitle = element_markdown(
            size = 10,
            family = "LexieReadable",
            hjust = 1,
        ),
        text = element_text(                                                    # textos do gráfica.
            family = "LexieReadable",
            color = "#000000",
            size = 10,
            hjust = 0,
            face = "bold"
        ),
        axis.text.x = element_text(                                               # texto do eixo x.
            color = "#FFFFFF",
            size = 6,
            face = "plain"
        ),
        axis.text.y = element_markdown(                                           # texto do eixo y.
            color = "#FFFFFF",
            size = 6,
            face = "plain",
            family = "LexieReadable"
        ),
        axis.title = element_text(                                        # texto do título do eixo x.
            face = "bold",    
            size = 8,
            hjust = 0.5
        ),
        legend.title = element_text(size = 9.2,            # padrões da texto do  titulo da legenda.
                                    color = "#FFFFFF", 
                                    face = "bold", 
                                    family = "arial"),
        legend.background = element_rect(fill = "#237CA9"),       # padrões para o fundo da legenda.
        legend.text = element_text(size = 6,                          # padrões do texto da legenda.
                                   color = "#FFFFFF",
                                   face = "bold"),
        
    )+
    scale_color_gradient(name = "tempo mínimo")+                         # altera o nome da legenda.
    scale_y_continuous(breaks = seq(0, 20000, 2000))+                   # formatar escala do eixo x.
    scale_x_continuous(breaks = seq(0, 800, 100))                       # formatar escala do eixo x.
    

ggplotly(p_age)
    
    

# Seleção dos 48 jogos citados

jogos_selecao01 <- c(                               # relação dos jogos citados neste trabalho.
    "A Feast for Odin", "Agricola", "Arkham Horror: The Card Game", "Black Angel", "Black Orchestra",
    "Burgle Bros.", "Cascadia", "Caverna: The Cave Farmers", "Dune: Imperium", "Everdell",
    "Endless Winter: Paleoamericans", "Facts in Five", "Gaia Project",
    "Gandhi: The Decolonization of British India, 1917 – 1947", "Gloomhaven", "Gloomhaven: Jaws of the Lion",
    "Ghost Stories", "Gutenberg", "Horrified", "Le Havre", "Lost Ruins of Arnak", "Mage Knight Board Game",
    "Mansions of Madness: Second Edition", "Maracaibo", "Marvel Champions: The Card Game", "Nemesis",
    "Neuroshima Hex! 3.0", "Prolix", "Reykholt",
    "Robinson Crusoe: Adventures on the Cursed Island", "Room 25", "Sagrada", "Scythe", "Shakespeare",
    "Snowdonia", "Space Race", "Spirit Island", "Tang Garden", "Tapestry", "Teotihuacan: City of Gods",
    "Terraforming Mars", "The 7th Continent", "The Shores of Tripoli", "Ubongo", "UBOOT: The Board Game", 
    "Underwater Cities", "Viticulture Essential Edition", "Wingspan"
)

selecao01 <- jogos_tabuleiro |>          # seleção das colunas com as informações para a tabela.
    select(
        name,
        year,
        min_players,
        min_age,
        playing_time,
        min_play_time,
        max_play_time
    ) |>
    filter(jogos_tabuleiro$name %in% jogos_selecao01) |>  # filt. os dados de acordo com selecao1.
    arrange(name) |> 
    kable(col.names = c("nome", "ano", "min jogadores",  #renom. na tabela os nomes das variáveis.
                        "min idade", "tempo jogo", "min tempo jogo",
                        "max tempo jogo"),
          align = "l"
    ) |> 
    kable_styling(                                          # altera as configurações da tabela.
        bootstrap_options = c("striped", 
                              "condensed"
        ),
        html_font = "",
        font_size = 10,
        full_width = TRUE, 
        fixed_thead = list(enabled = TRUE,
                           background = "#EDF6FD"
        ),
    )|> 
    kable_classic_2() |> 
    column_spec(1,                                          # altera as configurações das colunas.
                bold = FALSE,
                width = "10cm",
    ) |>      
    column_spec(2, 
                bold = FALSE,
                width = "2cm"
    ) |>
    column_spec(3,
                bold = FALSE,
                width = "4cm"
    ) |> 
    column_spec(4,
                bold = FALSE,
                width = "4cm"
    ) |> 
    column_spec(5,
                bold = FALSE,
                width = "4cm"
    ) |> 
    column_spec(6,
                bold = FALSE,
                width = "10cm"
    ) |> 
    column_spec(7,
                bold = FALSE,
                width = "10cm"
    ) |> 
    footnote(general = "Base de dados Board Games - disponível na plataforma Kaggle.",
             footnote_as_chunk = TRUE,
             fixed_small_size = TRUE,
             general_title = "Fonte:",
    )
selecao01                                                          # mostra a tabela no retório.

jogos_tabuleiro |> 
    select(name, description) |> 
    filter(name %in% jogos_selecao01) |>
    write_excel_csv("data/filmes_descricao.csv",
                    delim = "/")

