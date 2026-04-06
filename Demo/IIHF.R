qog_new <- qog_raw |>
  # Extraire l'année numérique finale de l'étiquette pays-année pour faciliter les filtrages ultérieurs
  mutate(year = stringr::str_extract(cname_year, "(\\d{4})$") |> as.integer()) |>
  # Conserver uniquement les identifiants pays/année essentiels ainsi que quelques indicateurs de gouvernance
  select(
    country_name = cname,
    year,
    region_country = ht_region, 
    gendergap_education = gggi_eas,
    gendergap_sante = gggi_hss,
   gendergap_economic = gggi_pos,
    femmes_fertilite = wdi_fertility,
    wdi_gdp_per_capita_current_usd = wdi_gdpcapcur,
  )

qog_new <- qog_new %>%
  drop_na()




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


qog_world <- qog_new %>%
  mutate(
    GGGI_composite = ( gendergap_education * gendergap_sante * gendergap_economic)^(1/3)  # moyenne géométrique
  )
model_composite <- lm(femmes_fertilite ~ GGGI_composite, data = qog_world)
coef_ggi <- round(coef(model_composite)["GGGI_composite"], 3)
pval_ggi <- signif(summary(model_composite)$coefficients["GGGI_composite", "Pr(>|t|)"], 3)


library(ggplot2)
n_pays <- n_distinct(qog_world$country_name)
ggplot(qog_world, aes(x = GGGI_composite*100, y = femmes_fertilite, color = region_name)) +
  geom_point(size = 3, alpha = 0.7) +                        # points
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # droite de régression
  labs(
    x = "Indice composite d'égalité de genre (IIHF) (%)",
    y = "Taux de fertilité (enfants/femme)",
    color = "Sous-région",
    title = "Relation entre égalité de genre globale et fertilité dans le Monde",
    subtitle = paste0("Coefficient = ", coef_ggi, ", p-value = ", pval_ggi,
                      caption = paste0(
                        "Source : Quality of Government (QoG), Global Gender Gap Index (WEF).\n",
                        "N = ", n_pays, " pays, période 2006–2022.\n",
                        "Calculs et visualisation : auteur."
                      ))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(
      size = 7,                 # plus petit
      face = "bold",            # reste lisible
      colour = "grey35",        # visuellement plus léger
      lineheight = 0.9,         # lignes plus serrées
      hjust = 0,
      margin = margin(t = 8)
    )
  )
