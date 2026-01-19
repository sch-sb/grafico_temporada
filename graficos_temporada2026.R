###############################################################################
# GRÁFICOS DE TEMPORADA – ATUALIZADO PARA 2026
# - Inclui temporadas: 2023/2024, 2024/2025, 2025/2026 e 2026/2027
# - Usa históricos 2023, 2024, 2025 + base atual (DENGUE.csv)
# - Remove duplicidade: “atuais” só entra a partir de 2026 (para não duplicar 2023–2025)
# - Mantém o eixo sazonal: SE 27..52 + SE 1..26
###############################################################################

pacman::p_load(tidyverse, lubridate)

pasta_historica <- "C:/Relatorios DC e Nowcasting/DC e Nowcasting_CSVs/Dengue"
pasta_atual     <- "C:/Relatorios DC e Nowcasting/DC e Nowcasting_CSVs/Dengue atual"

# Leitura (mantive seu padrão)
ler_dados_dengue <- function(arquivo, colunas) {
  read_delim(
    arquivo,
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    col_select = all_of(colunas),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
}

colunas_basicas <- c("DT_SIN_PRI", "SEM_PRI", "SG_UF", "CLASSI_FIN")

# ====== HISTÓRICOS (AGORA INCLUINDO 2025) ======
dados_2023 <- ler_dados_dengue(file.path(pasta_historica, "DENGUE2023.csv"), colunas_basicas) %>%
  mutate(fonte = "hist_2023")

dados_2024 <- ler_dados_dengue(file.path(pasta_historica, "DENGUE2024.csv"), colunas_basicas) %>%
  mutate(fonte = "hist_2024")

dados_2025 <- ler_dados_dengue(file.path(pasta_historica, "DENGUE2025.csv"), colunas_basicas) %>%
  mutate(fonte = "hist_2025")

# ====== ATUAL ======
dados_atuais <- ler_dados_dengue(file.path(pasta_atual, "DENGUE.csv"), colunas_basicas) %>%
  mutate(fonte = "atuais")

# UFs
uf_codigos <- c(
  "12" = "Acre", "27" = "Alagoas", "13" = "Amazonas", "16" = "Amapá",
  "29" = "Bahia", "23" = "Ceará", "53" = "Distrito Federal", "32" = "Espírito Santo",
  "52" = "Goiás", "21" = "Maranhão", "51" = "Mato Grosso", "50" = "Mato Grosso do Sul",
  "31" = "Minas Gerais", "15" = "Pará", "25" = "Paraíba", "41" = "Paraná",
  "26" = "Pernambuco", "22" = "Piauí", "33" = "Rio de Janeiro", "24" = "Rio Grande do Norte",
  "43" = "Rio Grande do Sul", "11" = "Rondônia", "14" = "Roraima", "42" = "Santa Catarina",
  "35" = "São Paulo", "28" = "Sergipe", "17" = "Tocantins"
)

# Região por UF (código numérico)
regiao_codigos <- c(
  # Norte
  "12" = "Norte", "13" = "Norte", "14" = "Norte", "15" = "Norte",
  "16" = "Norte", "17" = "Norte", "11" = "Norte",
  # Nordeste
  "21" = "Nordeste", "22" = "Nordeste", "23" = "Nordeste", "24" = "Nordeste",
  "25" = "Nordeste", "26" = "Nordeste", "27" = "Nordeste", "28" = "Nordeste", "29" = "Nordeste",
  # Sudeste
  "31" = "Sudeste", "32" = "Sudeste", "33" = "Sudeste", "35" = "Sudeste",
  # Sul
  "41" = "Sul", "42" = "Sul", "43" = "Sul",
  # Centro-Oeste
  "50" = "Centro-Oeste", "51" = "Centro-Oeste", "52" = "Centro-Oeste", "53" = "Centro-Oeste"
)

# ====== PROCESSA E DEFINE TEMPORADAS (ATUALIZADO) ======
dados <- bind_rows(dados_2023, dados_2024, dados_2025, dados_atuais) %>%
  mutate(
    SEM_PRI = gsub("[^0-9]", "", SEM_PRI),
    SEM_PRI = str_pad(SEM_PRI, 6, pad = "0"),
    sem_num = as.integer(SEM_PRI),
    ano     = sem_num %/% 100,
    semana  = sem_num %% 100
  ) %>%
  filter(!is.na(ano), !is.na(semana)) %>%
  # evita duplicidade com históricos:
  # - históricos cobrem 2023–2025, então "atuais" só entra a partir de 2026
  filter(!(fonte == "atuais" & ano <= 2025)) %>%
  # casos prováveis (remove descartados)
  filter(!CLASSI_FIN %in% c("5", "5.0", "13", "13.0") | is.na(CLASSI_FIN)) %>%
  mutate(
    temporada = case_when(
      (ano == 2023 & semana >= 27) | (ano == 2024 & semana <= 26) ~ "2023/2024",
      (ano == 2024 & semana >= 27) | (ano == 2025 & semana <= 26) ~ "2024/2025",
      (ano == 2025 & semana >= 27) | (ano == 2026 & semana <= 26) ~ "2025/2026",
      (ano == 2026 & semana >= 27) | (ano == 2027 & semana <= 26) ~ "2026/2027",
      TRUE ~ NA_character_
    ),
    # eixo sazonal: 27..52 vira 1..26 e 1..26 vira 27..52 (ordem 1..52)
    ordem_se = if_else(semana >= 27, semana - 26, semana + 26),
    UF_nome  = uf_codigos[as.character(SG_UF)],
    regiao   = regiao_codigos[as.character(SG_UF)]
  ) %>%
  filter(!is.na(temporada))

# Agregar por UF
dados_agg <- dados %>%
  group_by(temporada, SG_UF, UF_nome, semana, ordem_se) %>%
  summarise(casos = n(), .groups = "drop")

# Agregar por região
dados_regiao_agg <- dados %>%
  group_by(temporada, regiao, semana, ordem_se) %>%
  summarise(casos = n(), .groups = "drop")

# ====== CORES (ADICIONEI 2026/2027) ======
cores_temporada <- c(
  "2023/2024" = "steelblue",
  "2024/2025" = "darkgreen",
  "2025/2026" = "red",
  "2026/2027" = "purple"
)

# Função de gráfico (igual ao seu, mas deixei o eixo X mais “limpo” e robusto)
plotar_grafico <- function(df, uf = NULL, titulo_custom = NULL) {
  
  titulo <- if (!is.null(titulo_custom)) {
    titulo_custom
  } else if (is.null(uf)) {
    "Brasil"
  } else {
    first(na.omit(df$UF_nome[df$SG_UF == uf]))
  }
  
  df_plot <- df %>%
    group_by(temporada, ordem_se, semana) %>%
    summarise(casos = sum(casos), .groups = "drop") %>%
    arrange(temporada, ordem_se)
  
  labels_se <- df_plot %>%
    distinct(ordem_se, semana) %>%
    arrange(ordem_se)
  
  ggplot(df_plot, aes(x = ordem_se, y = casos, color = temporada, group = temporada)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cores_temporada, drop = FALSE) +
    scale_x_continuous(
      breaks = labels_se$ordem_se,
      labels = labels_se$semana,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Casos de Dengue -", titulo),
      x = "Semana Epidemiológica",
      y = "Número de Casos",
      color = "Temporada"
    ) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black")
    )
}

# ====== PASTAS DE SAÍDA (USANDO SUA LÓGICA DE PASTA POR SE) ======
se_atual     <- epiweek(today())
se_atual_str <- sprintf("SE %02d", se_atual)

base_out_temp <- file.path("graficos_temporada", se_atual_str)
dir_uf        <- file.path(base_out_temp, "graficos_uf")
dir_reg       <- file.path(base_out_temp, "graficos_regiao")

dir.create(dir_uf, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_reg, recursive = TRUE, showWarnings = FALSE)

# ====== GRÁFICO BRASIL ======
grafico_brasil <- dados_agg %>%
  group_by(temporada, semana, ordem_se) %>%
  summarise(casos = sum(casos), .groups = "drop") %>%
  mutate(SG_UF = NA, UF_nome = "Brasil") %>%
  plotar_grafico()

ggsave(
  filename = file.path(dir_uf, "grafico_dengue_Brasil.png"),
  plot = grafico_brasil,
  width = 12,
  height = 6
)

# ====== GRÁFICOS POR UF ======
ufs <- sort(unique(dados_agg$SG_UF))

for (uf in ufs) {
  p <- plotar_grafico(dados_agg %>% filter(SG_UF == uf), uf)
  
  nome_uf <- unique(na.omit(dados_agg$UF_nome[dados_agg$SG_UF == uf]))
  if (length(nome_uf) == 0) nome_uf <- paste0("UF_", uf)
  
  # sanitizar nome (para evitar problemas no Windows)
  nome_arquivo <- stringi::stri_trans_general(nome_uf[1], "Latin-ASCII")
  nome_arquivo <- gsub("[^A-Za-z0-9 _-]", "", nome_arquivo)
  
  ggsave(
    filename = file.path(dir_uf, paste0("grafico_dengue_", nome_arquivo, ".png")),
    plot = p,
    width = 12,
    height = 6
  )
}

# ====== GRÁFICOS POR REGIÃO ======
regioes <- sort(unique(dados_regiao_agg$regiao))

for (reg in regioes) {
  p_reg <- plotar_grafico(
    df = dados_regiao_agg %>% filter(regiao == reg),
    titulo_custom = paste("Região", reg)
  )
  
  ggsave(
    filename = file.path(dir_reg, paste0("grafico_dengue_regiao_", reg, ".png")),
    plot = p_reg,
    width = 12,
    height = 6
  )
}
