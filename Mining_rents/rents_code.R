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

plot_year = 2023
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
    title = "Top 10 productores de cobre como % del total mundial (2024)",
    x = "País",
    y = "Participación mundial"
  ) +
  scale_y_continuous(labels = scales::percent_format())


