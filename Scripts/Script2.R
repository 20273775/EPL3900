install.packages(c("tidyverse", "here", "modelsummary", "ggrepel", "ggplot2", "RColorBrewe"))
library(tidyverse)
library(here)
library(modelsummary)
library(ggplot2)
library(ggrepel)
library(RColorBrewer) # Pour des palettes de couleurs expertes

qog_path <- here("qog_std_ts_jan26.csv")
qog_raw <- read_csv(qog_path)
dir.create(here::here("Données"), showWarnings = FALSE)
qog_idh <- qog_raw |>
  
  mutate(year = stringr::str_extract(cname_year, "(\\d{4})$") |> as.integer()) |>
  filter(year >= 1970, year < 2016) |>
  # Conserver uniquement les identifiants pays/année essentiels ainsi que quelques indicateurs de gouvernance
  select(
    country_name = cname,
    year,
    region_country = ht_region, 
    urban_pop = wdi_popurb,
    pays_hdi = undp_hdi,
    femmes_fertilite = wdi_fertility,
    female_attainment = gea_ea2534f,
    infant_mort = wdi_mortinf, 
    wdi_gdp_per_capita_current_usd = wdi_gdpcapcon2015,
  )

#Séparer par régions
qog_idh <- qog_idh %>%
  mutate(region_name = case_when(
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

# Calcul des moyennes par région (en retirant les NA)
qog_regions_stats <- qog_idh |>
  group_by(region_name) |>
  summarise(
    mean_hdi = mean(pays_hdi, na.rm = TRUE),
    mean_fertility = mean(femmes_fertilite, na.rm = TRUE),
    n_obs = n()
  ) |>
  filter(region_name != "Inconnu")

# Création du graphique

# 1. Préparation des données (1990 - 2015)
qog_90_15 <- qog_regions_stats |> 
  filter(year >= 1990)

# 2. Création du graphique
ggplot(qog_90_15, aes(x = hdi_avg, y = fertility_avg, color = region_name)) +
  
  # Trajectoire (Ligne avec flèche)
  geom_path(
    arrow = arrow(length = unit(0.20, "cm"), type = "closed"),
    linewidth = 1.1, 
    alpha = 0.8
  ) +
  
  # Points pour marquer les années intermédiaires
  geom_point(size = 0.7, alpha = 0.3, show.legend = FALSE) +
  
  # ÉTIQUETTE UNIQUE : On ne prend que la première occurrence de 1990
  geom_text_repel(
    data = qog_90_15 |> filter(year == 1990) |> head(1), 
    aes(label = "1990"),
    size = 4,
    fontface = "bold",
    color = "black",
    nudge_y = 0.4, # Décale vers le haut pour être bien visible
    show.legend = FALSE
  ) +
  
  # --- STYLE ET COULEURS ---
  scale_color_brewer(palette = "Spectral") +
  theme_minimal(base_family = "sans") + 
  labs(
    title = "TRANSITION DÉMOGRAPHIQUE : 1990-2015",
    subtitle = "Le point d'origine (1990) est indiqué pour référence temporelle",
    x = "Indice de Développement Humain (Moyen)",
    y = "Enfants par femme (Moyenne)",
    color = "Régions du Monde",
    caption = "Source: QoG Dataset | Janvier 2026 "
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 9, face = "bold")
  ) +
  # Légende organisée sur 2 colonnes
  guides(color = guide_legend(ncol = 2, override.aes = list(linewidth = 3)))

#Sauvegarder
ggsave(
  filename = here("Données", "Transition_Demographique_90_15.png"),
  width = 12,        # Largeur généreuse pour la légende sur 2 colonnes
  height = 8,        # Hauteur équilibrée
  dpi = 300,         # Haute résolution (nettoyage des textes et flèches)
  bg = "white"       # Force le fond blanc pour éviter la transparence
)