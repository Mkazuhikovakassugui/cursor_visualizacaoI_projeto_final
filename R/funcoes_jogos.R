# Inserção do nome do melhor jogo por categoria no gráfico

anotar <- function(.best_category_name) {
    annotate("label",
             family = "",
             x = .best_category_name$wishing,
             y = .best_category_name$category,
             label = .best_category_name$name,
             hjust = -0.2,
             vjust = 0.5,
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


