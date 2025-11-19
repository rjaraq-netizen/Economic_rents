# ============================================================
# Script: Analisis rentas mineria
# Autor: Roberto Jara
# Fecha: 2025-11-18
# Descripción: Carga paquetes, importa datos
# ============================================================

paquetes <- c("tidyverse", "data.table", "readxl", "haven")
lapply(paquetes, library, character.only = TRUE)


# Calculo de rentas siguiendo la medición del banco mundial
df <- read_excel("E:/R/Economic_rents/Mining_rents/data/P_Data_Extract_From_World_Development_Indicators.xlsx",sheet = "Data")
df <- df %>%
  pivot_longer(
    cols = matches("^\\d{4}"),       # columnas que empiezan con 4 dígitos
    names_to = "year",
    values_to = "value"
  )

df <- df %>%
  mutate(
    year= as.numeric(substr(year, 1, 4))   # extrae los primeros 4 dígitos
  )

df <- df %>%
  pivot_wider(
    id_cols = c(`Country Name`, year),
    names_from = `Series Code`,
    values_from = value
  )

df <- df |> 
  rename(
    mineral_rents = `NY.GDP.MINR.RT.ZS`,
    gdp = `NY.GDP.MKTP.KD`,
    total_rents = `NY.GDP.TOTL.RT.ZS`
  )

df <- df |> select(-`NA`)

df <- df %>%
  mutate(across(
    where(is.list),
    ~ purrr::map_chr(
      .x,
      ~ if (length(.x) == 0 || is.null(.x) || .x[1] %in% c("..", "")) {
        NA_character_
      } else {
        .x[1]
      }
    )
  ))

df <- df %>%
  mutate(
    mineral_rents = as.numeric(mineral_rents),
    total_rents   = as.numeric(total_rents),
    gdp           = as.numeric(gdp)
  )

df <- df %>%
  mutate(rents_gdp = mineral_rents * gdp)

world_years <- df$year[df$`Country Name` == "World"]
world_gdp   <- df$rents_gdp[df$`Country Name` == "World"]

df <- df %>% mutate(
  gdp_world = world_gdp[ match(year, world_years) ],
  rents_share_world = rents_gdp / gdp_world
)


# Productores de cobre como % del total mundial

df2 = read_excel("E:/R/Economic_rents/Mining_rents/data/production-of-copper-mine.xlsx")

df2 <- df2 %>%
  group_by(Year) %>%
  mutate(
    world_production = sum(Production, na.rm = TRUE),
    production_share = Production / world_production 
  ) %>%
  ungroup()
# muy importante fijar el plot_year 
plot_year = 2020
# --- 2) Filtrar año, el "n" indica el numero de paises top que se sacan
df_year <- df2 %>%
  filter(Year == plot_year) %>%
  filter(!is.na(production_share)) %>%  # quitar NA
  slice_max(order_by = production_share, n = 10, with_ties = FALSE) %>%
  arrange(desc(production_share)) %>%
  # forzar orden para el plot
  mutate(Country_plot = fct_reorder(Country, production_share))

library(ggplot2)

ggplot(df_year, aes(x = Country_plot, y = production_share)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Top 10 productores de cobre como % del total mundial (", plot_year, ")", sep = ""),
    x = "País",
    y = "Participación mundial",
    caption = "Fuente: KAPSARC – Production of Copper Mine dataset"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(
    plot.caption = element_text(hjust = 0, size = 9, color = "gray40")
  )

# Grafico 2 

name_fix <- c(
  "USA" = "United States",
  "Congo, Democratic Republic" = "Congo, Dem. Rep.",
  "Russia" = "Russian Federation"
)


df_year <- df_year %>%
  mutate(
    Country_std = dplyr::recode(Country, !!!name_fix)
  )

# --- 1) Obtener los top 6 países según producción 2023 ---
top_countries <- df_year %>%
  arrange(desc(production_share)) %>%
  slice_head(n = 6) %>%
  pull(Country_std)

# --- 2) Filtrar df principal para esos países y años 1970–2021 ---
df_plot <- df %>%
  filter(
    `Country Name` %in% top_countries,
    year >= 1970,
    year <= plot_year
  ) %>%
  mutate(
    Country = factor(`Country Name`, levels = top_countries)
  )

title <- paste0(
  "Mundo: Rentas anuales de la minería en los 6 países\n",
  "con mayor producción de cobre al año ", plot_year,
  " — 1970–", plot_year
)


ggplot(df_plot, aes(x = year, y = mineral_rents, color = Country)) +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_point(size = 1.3, alpha = 0.7) +
  labs(
    title = title,
    x = "Año",
    y = "Mineral rents",
    color = "País"
  ) +
  scale_x_continuous(
    breaks = seq(1970, plot_year, by = 10),
    limits = c(1970, plot_year)
  )  +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12)
  )



