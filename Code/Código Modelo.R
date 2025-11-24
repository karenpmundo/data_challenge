# -----------------------------------------------------------------------------
# 1. CONFIGURACIÓN Y LIBRERÍAS
# -----------------------------------------------------------------------------
library(sf)
library(dplyr); library(readr); library(stringr); library(tidyr)
library(ggplot2); library(scales); library(units); library(viridis)
library(rjags); library(ggmcmc)

# Rutas
path_geojson <- "path/neighbourhoods.geojson"
path_listings <- "path/listings.csv"

# Funciones Auxiliares
robust_norm <- function(x) {
  (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
}

norm01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  (x - r[1]) / (r[2] - r[1])
}

# -----------------------------------------------------------------------------
# 2. INGESTA Y PROCESAMIENTO ESPACIAL
# -----------------------------------------------------------------------------

# A) Vecindarios (Polígonos)
nbh <- st_read(path_geojson, quiet = TRUE) %>%
  st_make_valid() 

# Crear versión métrica para cálculos (UTM 14N CDMX)
nbh_m <- nbh %>% 
  st_transform(32614) %>%
  mutate(area_km2 = as.numeric(set_units(st_area(.), km^2)))

# B) Listings (Puntos)
listings <- read_csv(path_listings, show_col_types = FALSE) %>%
  mutate(price_mxn = as.numeric(price)) %>%
  filter(!is.na(latitude), !is.na(longitude), price_mxn > 0) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(nbh_m)) # Asegurar mismo CRS métrico

# -----------------------------------------------------------------------------
# 3. JOIN ESPACIAL Y CÁLCULO DE KPIs
# -----------------------------------------------------------------------------

# Unir puntos a polígonos
air_join <- st_join(listings, nbh_m, left = FALSE)

# Calcular KPIs por Vecindario
kpis <- air_join %>%
  st_drop_geometry() %>%
  group_by(neighbourhood) %>%
  summarise(
    n_listings   = n(),
    adr          = mean(price_mxn, na.rm = TRUE),
    occ_proxy    = mean(1 - pmin(pmax(availability_365, 0), 365) / 365, na.rm = TRUE),
    revpar       = adr * occ_proxy,
    reviews_pm   = mean(reviews_per_month, na.rm = TRUE),
    revs_ltm     = sum(number_of_reviews_ltm, na.rm = TRUE),
    # Dummies para el modelo
    prop_private = mean(room_type == "Private room"),
    prop_hotel   = mean(room_type == "Hotel room"),
    prop_shared  = mean(room_type == "Shared room")
  )

# Unir KPIs al objeto espacial original
nbh_bayes <- nbh_m %>%
  left_join(kpis, by = "neighbourhood") %>%
  mutate(
    listings_per_km2 = n_listings / area_km2,
    # Preparación Bayesiana (Estandarización Robusta)
    Z_revpar  = robust_norm(revpar),
    Z_density = robust_norm(listings_per_km2),
    Z_quality = robust_norm(reviews_pm),
    Y         = asinh(revs_ltm) # Variable respuesta transformada
  ) %>%
  filter(!is.na(Y), !is.na(Z_revpar))

# -----------------------------------------------------------------------------
# 4. MODELADO BAYESIANO (JAGS)
# -----------------------------------------------------------------------------

# Datos para JAGS
data_jags <- list(
  J          = nrow(nbh_bayes),
  Y          = nbh_bayes$Y,
  Z_revpar   = nbh_bayes$Z_revpar,
  Z_density  = nbh_bayes$Z_density,
  Z_quality  = nbh_bayes$Z_quality,
  rt_private = nbh_bayes$prop_private,
  rt_shared  = nbh_bayes$prop_shared,
  rt_hotel   = nbh_bayes$prop_hotel
)

# Definición y corrida del modelo
mod <- jags.model(file = "airbnb_bayes_model.txt", data = data_jags, n.chains = 4, n.adapt = 1000)
update(mod, n.iter = 2000) # Burn-in

# Muestreo (Posterior)
params <- c("beta1", "beta2", "beta3", "sigma") # Solo betas de interés
samps <- coda.samples(mod, variable.names = params, n.iter = 5000, thin = 2)

# -----------------------------------------------------------------------------
# 5. RESULTADOS Y PESOS
# -----------------------------------------------------------------------------

# Extraer medias de los coeficientes
stats <- summary(samps)$statistics
betas <- stats[c("beta1", "beta2", "beta3"), "Mean"]
names(betas) <- c("revpar", "density", "quality")

# Calcular Pesos Bayesianos (Importancia Relativa)
w_norm <- abs(betas) / sum(abs(betas))
w_dir  <- sign(betas) * w_norm

# Calcular Score Final en el dataset
nbh_final <- nbh_bayes %>%
  mutate(
    score_bayes = w_dir["revpar"] * Z_revpar + 
                  w_dir["density"] * Z_density + 
                  w_dir["quality"] * Z_quality
  ) %>%
  arrange(desc(score_bayes))

# Top 3 para visualización
top3 <- head(nbh_final, 3)

# -----------------------------------------------------------------------------
# 6. VISUALIZACIÓN FINAL
# -----------------------------------------------------------------------------

ggplot(nbh_final) +
  # Mapa base con Score
  geom_sf(aes(fill = score_bayes), color = "white", linewidth = 0.1) +
  # Resaltar Top 3
  geom_sf(data = top3, fill = NA, color = "black", linewidth = 1.2) +
  scale_fill_viridis(option = "magma", name = "Score\nBayesiano") +
  labs(
    title = "Score de Atractivo Airbnb CDMX (Modelo Bayesiano)",
    subtitle = "Pesos derivados de la regresión posterior (Beta1, Beta2, Beta3)",
    caption = "Top 3 resaltado en negro"
  ) +
  theme_minimal()

# Verificación rápida de convergencia
ggs_traceplot(ggs(samps))