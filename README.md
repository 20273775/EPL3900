
# Projet EPL3900 - Transition Démographique et Éducation (1970-2015)

## Description
Analyse de l'impact de l'éducation des femmes sur les taux de fertilité mondiaux. 
Ce projet utilise les données du *Quality of Government (QoG)*.

## Équipe de recherche
- Ely
- Dalia
- Zola
- [Votre Nom]

## Méthodologie (Référence : Arel-Bundock, 2021)
Le projet suit une démarche d'inférence causale rigoureuse :
1. **Visualisation** : Graphiques de trajectoires par pays.
2. **Diagnostic** : Test de Hausman (p < 0.001) validant l'utilisation des Effets Fixes.
3. **Estimation** : Modèle à Effets Fixes (Within) avec erreurs robustes clustérisées par pays.

## Structure
- `/Scripts` : Code R pour le nettoyage, les graphiques et la régression `plm`.
- `/Données` : Base de données nettoyée et exportations des tableaux (.png).

## Précisions avant d'entamer le projet
L'utilisateur doit *télécharger les données de panel* sur le site suivant : https://www.gu.se/en/quality-government/qog-data/data-downloads/standard-dataset 
