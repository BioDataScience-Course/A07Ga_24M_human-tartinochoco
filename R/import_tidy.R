# Étude de l'obésité - Importation et remaniement des données
# Auteur : Louis Carlier
# Date : 28/02/2025
###############################################################################

# Packages utiles
SciViews::R(lang = "fr")

# Importation des données brutes

## Création des dossier `data/` et `cache/`
fs::dir_create("data/cache")

## Importation du dictionnaire des données
biometry_metadata <- read$csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ14kFDtlqxUqJpfKIcZRHA2i3ZnCwSdT_bqcx7BWp3hk_fqEGk9JmBvRsHvdBZrI3KCACV-LHQ-tAv/pub?gid=0&single=true&output=csv",
  cache_file = "data/cache/biometry_metadata_raw.csv",
  force = FALSE
)

## Importation du tableau de données
biometry <- read$csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTTvgkHmrxPXHJ16cypzK7ooBhcSybscjBMc0obVSt1dvxlNL9rtage91lKD8Jec-3n4eX_6O-VdW7f/pub?gid=0&single=true&output=csv",
  cache_file = "data/cache/biometry_raw.csv",
  force = FALSE
)

# Exploration des données

skimr::skim(biometry)

# Modification des types de variables des données

unique(biometry$genre)
biometry$genre <- factor(biometry$genre, levels = c("H", "F"))

unique(biometry$alimentation)
biometry$alimentation <- factor(biometry$alimentation,
  levels = c("omnivore", "carnivore", "végétarien"))

unique(biometry$intolerance_lactose)
biometry$intolerance_lactose <- factor(biometry$intolerance_lactose,
  levels = c("N", "O"))

unique(biometry$intolerance_gluten)
biometry$intolerance_gluten <- factor(biometry$intolerance_gluten,
  levels = c("N", "O"))

unique(biometry$sucre)
# Correction de quelques niveaux
biometry$sucre[biometry$sucre == "souveny"] <- "souvent"
biometry$sucre[biometry$sucre == "régulierement"] <- "régulièrement"
# Transformation en facteur ordonné
biometry$sucre <- ordered(biometry$sucre,
  levels = c("jamais", "rarement", "régulièrement", "souvent"))

unique(biometry$cortisone)
biometry$cortisone <- factor(biometry$cortisone,
  levels = c("N", "O"))

# Correction, filtre, sélection sur le tableau des données

biometry %>.%
  smutate(., 
    # Calcul de l'age des individu
    age = as.numeric(difftime(date_mesure, date_naissance, units = "days")/365.25), 
    # Calcul de la masse corrigée
    masse_corr = masse * (masse_std_ref/masse_std)) %>.%
    sdrop_na(., masse_corr) -> 
    biometry

# Ajout des labels et des unités
vec_labs <- biometry_metadata$label_fr
names(vec_labs) <- biometry_metadata$variables

vec_units <- biometry_metadata$unités
names(vec_units) <- biometry_metadata$variables

# Ajout manuel de 2 nouveaux labels et unités pour les graphiques
vec_labs_extra <- c("age" = "Age", "masse_corr" = "Masse corrigée")
vec_units_extra <- c("age" = "Années", "masse_corr" = "kg")

# Combinaison des vecteurs existants avec les nouveaux labels et unités
vec_labs <- c(vec_labs, vec_labs_extra)
vec_units <- c(vec_units, vec_units_extra)

biometry <- labelise(biometry,
  label = as.list(vec_labs),
  units = as.list(vec_units))


# Sauvegarde local des données importantes 
write$rds(biometry, "data/biometry.rds", compress = "xz")
write$rds(biometry_metadata, "data/biometry_metadata.rds", compress = "xz")

# Élimination des objets de l'environnement global
rm(biometry_metadata, biometry)
