install.packages(c("tidyverse", "here", "modelsummary"))

library(here)
library(tidyverse)
library(modelsummary)

qog_path <- here("QoG_Cross_Sectional_Jan_2025.csv")
qog_raw <- read_csv(qog_path)
dir.create(here::here("Données"), showWarnings = FALSE)
qog <- qog_raw |>
  # Extraire l'année numérique finale de l'étiquette pays-année pour faciliter les filtrages ultérieurs
  mutate(year = stringr::str_extract(cname_year, "(\\d{4})$") |> as.integer()) |>
  # Conserver uniquement les identifiants pays/année essentiels ainsi que quelques indicateurs de gouvernance
  select(
    country_name = cname,
    year,
    niveau_idh = undp_hdi,
    region_country = ht_region,
    femmes_fertilite = wdi_fertility,
    wdi_gdp_per_capita_current_usd = wdi_gdpcapcur,
  )


qog_na_clean <- qog |>
  drop_na(
    femmes_fertilite,
    region_country,
    niveau_idh,
    wdi_gdp_per_capita_current_usd
  )

qog_na_clean <- qog_na_clean %>%
  mutate(region_country = case_when(
    region_country == 1 ~ "Europe de l'Est et Union Soviétique",
    region_country == 2 ~ "Amérique Latine",
    region_country == 3 ~ "Afrique du Nord et Moyen-Orient",
    region_country == 4 ~ "Afrique Subsaharienne",
    region_country == 5 ~ "Europe de l'Ouest et Amérique du Nord",
    region_country == 6 ~ "Asie de l'Est",
    region_country == 7 ~ "Asie du Sud-Est",
    region_country == 8 ~ "Asie du Sud",
    region_country == 9 ~ "Pacifique",
    region_country == 10 ~ "Caraïbes",
    TRUE ~ NA_character_  # Pour les valeurs manquantes
  ))

model_gdp <- lm(
  femmes_fertilite ~ niveau_idh,
  data = qog_na_clean
)

modelsummary(model_gdp)


#Graphique 
n_pays <- n_distinct(qog_new$country_name)
p<- ggplot(
  qog_na_clean,
  aes(x = niveau_idh,
      y = femmes_fertilite,
      color = region_country)
) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x = "Indice de développement humain (IDH)",
    y = "Taux de fertilité (enfants par femme)",
    color = "Région du monde",
    title = "Relation entre le développement humain et la fertilité",
    subtitle = "Analyse transversale à partir des données QoG",
    caption = paste0(
      "Source : Quality of Government (QoG), PNUD (IDH), Banque mondiale. ",
      "N = ", n_pays, " pays."
    )
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    plot.caption = element_text(size = 7, colour = "grey40"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 7)
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE)
  )

#Enregistrer le Tableau de significativité et le graphique

table_gt <- modelsummary(
  model_gdp,
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared", "AIC"),
  output = "gt"
)

ggsave(
  filename = here::here("Données", "idh_fertilite_qog.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

