#--------------------------------------------------------------------------------------------------#
#                            Trabalho Final - Curso Visualização de Dados I
#                                         Professor: Julio
#                                     Aluno: Marcio Vakassugu
#--------------------------------------------------------------------------------------------------#

# 1) Pacotes ----------------------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(readr)


# 2) Carregamento dos Dados -------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2022-01-25")
ratings <- tuesdata$ratings
details <- tuesdata$details


# 3) Consolidar as bases ----------------------------------------------------------------------


board_games <- left_join(ratings,
                         details,
                         by = "id",
                         copy = FALSE,
                         keep = FALSE,
                         )


# 4) Excluir colunas comuns/não usadas---------------------------------------------------------

board_games$yearpublished <-  NULL # coluna "yearpublished"igual a coluna "year"
board_games$num.y <- NULL  # utilizaremos a coluna "num.x"
board_games$primary <- NULL # utilizaremos a coluna "name"


# 5) Padronizar os nomes das colunas ----------------------------------------------------------

board_games <- board_games |> 
    rename("min_players" = "minplayers",
           "max_players" = "maxplayers",
           "playing_time" = "playingtime",
           "min_play_time" = "minplaytime",
           "max_play_time" = "maxplaytime",
           "min_age" = "minage",
           "board_game_category" ="boardgamecategory",
           "board_game_mechanic" = "boardgamemechanic",
           "board_game_family" = "boardgamefamily",
           "board_game_expansion" = "boardgameexpansion",
           "board_game_implementation" = "boardgameimplementation",
           "board_game_designer" = "boardgamedesigner",
           "board_game_artist" = "boardgameartist",
           "board_game_publisher" = "boardgamepublisher",
           "num" = "num.x"
           )



# 6) Eliminar os colchetes de colunas ---------------------------------------------------------

## para a coluna "board_game_category
str_replace_na(board_games$board_game_category)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_category[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_category <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_category[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_category <- nova_coluna

board_games$board_game_category <- board_games$board_game_category |> 
    str_replace_all("', '", ", ")

# para a coluna "board_game_mechanic

str_replace_na(board_games$board_game_mechanic)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_mechanic[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_mechanic <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_mechanic[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_mechanic<- nova_coluna

board_games$board_game_mechanic <- board_games$board_game_mechanic |> 
    str_replace_all("', '", ", ")


# para a coluna board_game_family

str_replace_na(board_games$board_game_family)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_family[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_family <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_family[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_family<- nova_coluna

board_games$board_game_family <- board_games$board_game_family |> 
    str_replace_all("', '", ", ")

# para a coluna board_game_expansion

str_replace_na(board_games$board_game_expansion)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_expansion[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_expansion <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_expansion[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_expansion<- nova_coluna

board_games$board_game_expansion <- board_games$board_game_expansion |> 
    str_replace_all("', '", ", ")

# Salvando a base para análise ----------------------------------------------------------------

write_rds(board_games, "data/base_preparada.RDS")


board_games <- read_rds("data/base_preparada.RDS")

# para a coluna board_game_implementation

str_replace_na(board_games$board_game_implementation)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_implementation[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_implementation <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_implementation[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_implementation<- nova_coluna

board_games$board_game_implementation <- board_games$board_game_implementation |> 
    str_replace_all("', '", ", ")


# para a coluna board_game_designer

str_replace_na(board_games$board_game_designer)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_designer[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_designer <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_designer[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_designer <- nova_coluna

board_games$board_game_designer <- board_games$board_game_designer |> 
    str_replace_all("', '", ", ")


# para a coluna board_game_artist

str_replace_na(board_games$board_game_artist)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_artist[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_artist <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_artist[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_artist <- nova_coluna

board_games$board_game_artist <- board_games$board_game_artist |> 
    str_replace_all("', '", ", ")

# para a coluna board_game_publisher

str_replace_na(board_games$board_game_publisher)

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_publisher[i] |>
        stringr::str_extract("(?<=\\[)(.*)(?=\\])")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_publisher <- nova_coluna

nova_coluna <- c()

for (i in seq_along(1:nrow(board_games))){
    texto <- board_games$board_game_publisher[i] |>
        stringr::str_extract("(?<=\\')(.*)(?=\\')")
    nova_coluna <- append(nova_coluna, texto)
}

board_games$board_game_publisher <- nova_coluna

board_games$board_game_publisher <- board_games$board_game_publisher |> 
    str_replace_all("', '", ", ")


# Salvando a base para análise ----------------------------------------------------------------

write_rds(board_games, "data/base_preparada.RDS")



