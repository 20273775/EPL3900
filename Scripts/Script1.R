install.packages(c("tidyverse", "here", "modelsummary", "webshot2"))
library(tidyverse)
library(modelsummary)
library(webshot2)

qog_new <- dataset |>
  
  # Extraire l'année numérique finale de l'étiquette pays-année pour faciliter les filtrages ultérieurs
  mutate(year = stringr::str_extract(cname_year, "(\\d{4})$") |> as.integer()) |>
  filter(year >= 1970, year < 2016) |>
  # Conserver uniquement les identifiants pays/année essentiels ainsi que quelques indicateurs de gouvernance
  select(
    country_name = cname,
    year,
    region_country = ht_region, 
    urban_pop = wdi_popurb,
    femmes_fertilite = wdi_fertility,
    female_attainment = gea_ea2534f,
    infant_mort = wdi_mortinf, 
    wdi_gdp_per_capita_current_usd = wdi_gdpcapcon2015,
  )

#Séparer par régions
qog_new <- qog_new %>%
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

#Mettre le taux de mortalité en pourcentage 
qog_world <- qog_new 
qog_world <- qog_world %>%
  mutate(log_gdp = log(wdi_gdp_per_capita_current_usd),
         infant_mort = infant_mort/10)

# Évaluer le nombre d'observations distinctes dans la colonne (pays)
n_distinct(qog_world$country_name)

#Années disponibles, si non disponible retirer les observations problématiques
qog_world |> 
  mutate(has_edu = !is.na(female_attainment)) |> 
  group_by(year) |> 
  summarise(taux_dispo = mean(has_edu) * 100) |> 
  ggplot(aes(x = year, y = taux_dispo)) + geom_line()

# Analyse de la répartition régionale
repartition_regionale <- qog_world |>
  group_by(region_name) |>
  summarise(
    nb_observations = n(),
    nb_pays_uniques = n_distinct(country_name),
    # Calculer le taux de données manquantes par région pour l'éducation
    missing_edu = mean(is.na(female_attainment)) * 100
  ) |>
  arrange(desc(nb_observations))

print(repartition_regionale)

# Interaction model
model_interactions <- lm(
  femmes_fertilite ~ female_attainment + urban_pop + log_gdp + infant_mort + factor(region_name),
  output = here("Données", "resultats_regression.png"),
  title = "Tableau 1 : Déterminants de la fertilité (1970-2015)",
  data = qog_world
)


modelsummary(
  model_interactions,
  stars = TRUE,
)

#Télécharger le PNG pour visualiser
library(gt)


# 2. Création du tableau avec titre et renommage
table_visuelle <- modelsummary(
  model_interactions, 
  output = "gt",
  stars = TRUE,
  title = "Tableau 1. Analyse des déterminants de la fertilité mondiale (1970-2015)",
  coef_rename = c(
    "female_attainment" = "Nombre moyen d'années d'éducation des femmes",
    "pays_hdi" = "Indice de Développement Humain (IDH)",
    "year" = "Année de l'observation",
    "(Intercept)" = "Constante (Valeur de base)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
    list("raw" = "aic", "clean" = "AIC (Qualité du modèle)", "fmt" = 0) # On arrondit l'AIC à l'unité
  )
)
# 3. Personnalisation du style (On s'assure que le titre est bien ancré)
table_stylee <- table_visuelle |>
  # Note : Le titre est déjà défini dans modelsummary(title=...), 
  # mais on peut le renforcer ou ajouter le sous-titre ici
  tab_header(
    title = "Tableau 1. Analyse des déterminants de la fertilité mondiale (1970-2015)",
    subtitle = "Relation entre l'accès à l'éducation et l'indice de fécondité"
  ) |>
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = cells_body()
  ) |>
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(22),
    heading.title.font.weight = "bold", # On force le gras sur le titre
    heading.subtitle.font.size = px(16),
    heading.align = "left",             # Alignement propre à gauche
    column_labels.background.color = "#2c3e50",
    column_labels.font.weight = "bold",
    table.font.size = px(16)
  )

# 4. Sauvegarde PNG (On augmente vwidth pour laisser de la place au titre)
gtsave(
  table_stylee, 
  filename = here("Données", "Resultats_Regression_Education.png"),
  zoom = 3,
  vwidth = 1000, # Largeur virtuelle augmentée pour éviter les coupures
  expand = 20    # Marge de sécurité
)


#Graphique 

# 1. Calcul des moyennes (identique)
data_path <- qog_world %>%
  filter(!is.na(female_attainment), !is.na(femmes_fertilite), !is.na(region_name)) %>%
  group_by(region_name, year) %>%
  summarise(
    mean_edu = mean(female_attainment, na.rm = TRUE),
    mean_fert = mean(femmes_fertilite, na.rm = TRUE),
    .groups = 'drop'
  )


# 1. Calcul des moyennes
data_path <- qog_world %>%
  filter(!is.na(female_attainment), !is.na(femmes_fertilite), !is.na(region_name)) %>%
  group_by(region_name, year) %>%
  summarise(
    mean_edu = mean(female_attainment, na.rm = TRUE),
    mean_fert = mean(femmes_fertilite, na.rm = TRUE),
    .groups = 'drop'
  )

# 2. Création du graphique comparatif
graph_evolution <- ggplot(data_path, aes(x = mean_edu, y = mean_fert, color = region_name)) +
  
  geom_path(aes(group = region_name), linewidth = 1, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.5) +
  
  # MODIFICATION : Étiquettes temporelles sur une seule région pour ne pas surcharger
  geom_text_repel(
    data = data_path %>% 
      filter(year %in% c(1970, 2015) & region_name == "Afrique Subsaharienne"), # Région de référence pour les étiquettes
    aes(label = year),
    size = 4,
    fontface = "bold",
    color = "black",          # Met le texte en noir pour qu'il soit bien visible
    segment.color = 'grey30',
    nudge_x = 1.2,            # Décale légèrement les étiquettes
    nudge_y = 1,
    max.overlaps = 20
  ) +
  
  scale_color_brewer(palette = "Spectral") + 
  
  theme_bw() + 
  labs(
    title = "Trajectoire de la transition démographique (1970-2015)",
    subtitle = "\nRelation entre l'éducation des femmes (25-34 ans) et le niveau de fécondité",
    x = "\nNombre moyen d'années de scolarisation (Femmes)",
    y = "\nTaux de fertilité moyen (enfants par femme)",
    color = "Régions du monde",
    caption = "Source : QoG Standard | Janvier 2026"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  ) +
  guides(color = guide_legend(ncol = 3))

print(graph_evolution)

# 3. Sauvegarde
ggsave(here("Données", "Evolution_Education_70_15.png"), 
       width = 11, height = 8, dpi = 300, bg = "white")


# Modèle alternatif (beaucoup plus précis)

#Test Hausman
library(plm)

# 1. Déclarer les données comme un panel (ID = pays, Time = année)
p_data <- pdata.frame(qog_clean, index = c("country_name", "year"))

# 2. Estimer le modèle à Effets Fixes (Within)
model_fe <- plm(femmes_fertilite ~ female_attainment + urban_pop + log_gdp + infant_mort, 
                data = p_data, model = "within")

# 3. Estimer le modèle à Effets Aléatoires
model_re <- plm(femmes_fertilite ~ female_attainment + urban_pop + log_gdp + infant_mort + region_name, 
                data = p_data, model = "random")

# 4. Faire le test de Hausman pour choisir entre les deux
phtest(model_fe, model_re)

#Résultats = En faveur d'un modèle à effets fixes

# Modèle à Effets Fixes 
model_fe <- plm(femmes_fertilite ~ female_attainment + urban_pop + log_gdp + infant_mort, 
                data = p_data, 
                model = "within")

# Utiliser des écarts-types robustes pour corriger l'autocorrélation résiduelle
library(lmtest)
library(sandwich)
coeftest(model_fe, vcov = vcovHC(model_fe, method = "arellano", cluster = "group"))

#Enregistrer les comparaisons de tableaux
# 1. Préparer une liste de modèles pour la comparaison
# On ajoute des écarts-types robustes (Clustered SE) pour le modèle FE
models_list <- list(
  "MCO (Pooled)" = lm(femmes_fertilite ~ female_attainment + urban_pop + log_gdp + infant_mort, data = qog_clean),
  "Effets Fixes (FE)" = model_fe
)

# 2. Création du tableau comparatif
table_finale <- modelsummary(
  models_list,
  vcov = list(NULL, "HC1"), # HC1 applique les écarts-types robustes au modèle FE
  stars = TRUE,
  title = "Tableau 2 : Comparaison des déterminants de la fertilité (1970-2015)",
  coef_rename = c(
    "female_attainment" = "Éducation des femmes (années)",
    "urban_pop" = "Taux d'urbanisation (%)",
    "log_gdp" = "PIB par habitant (log)",
    "infant_mort" = "Mortalité infantile"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "Observations", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R² Within", "fmt" = 3)
  ),
  output = "gt"
)

# 3. Enregistrer en PNG (nécessite webshot2)
library(gt)
gtsave(table_finale, filename = here("Données", "Resultats_Panel_FE.png"))