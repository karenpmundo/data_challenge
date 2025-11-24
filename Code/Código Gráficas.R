library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(stringi)
library(sf)
library(ggplot2)

# -------------------------------------------------------------------------
# 0. Ruta
# -------------------------------------------------------------------------
rm(list = ls())
# Asegúrate de que tu ruta sea correcta
setwd("Ruta") 

limpiar_texto <- function(x) {
  x <- stri_trans_general(str_to_upper(trimws(x)), "Latin-ASCII")
  return(x)
}

# -------------------------------------------------------------------------
# 1. Cargo base
# -------------------------------------------------------------------------

listings  <- read.csv("listings.csv")
listings_scraped <- read.csv("listings_scraped.csv")
costo_col <- read_excel("Costo_prom_col.xlsx", sheet = "Data")
Alcaldia  <- st_read("neighbourhoods.geojson", quiet = TRUE)

# -------------------------------------------------------------------------
# 2. Base
# -------------------------------------------------------------------------

base_listings <- listings %>%
  select(id, neighbourhood, price, latitude, longitude) %>%
  left_join(select(listings_scraped, id, bedrooms, estimated_revenue_l365d,
                   occupancy_day = estimated_occupancy_l365d, review_scores_rating,
                   room_type, accommodates, review_scores_location), by = "id") %>%
  mutate(neighbourhood = limpiar_texto(neighbourhood),
         occupancy_rate = ((occupancy_day / 365))*100) %>%
  drop_na(price, bedrooms, occupancy_rate, review_scores_rating, neighbourhood)

# -------------------------------------------------------------------------
# 3. PREPARACIÓN DEL MAPA
# -------------------------------------------------------------------------

# A) Cargar y Limpiar Mapa de Colonias
colonias <- st_read("colonias_iecm.shp", quiet = TRUE) %>% st_make_valid() 

if ("NOMUT" %in% names(colonias)) {
  colonias <- colonias %>% rename(colonia_oficial = NOMUT)
} else if ("colonia" %in% names(colonias)) {
  colonias <- colonias %>% rename(colonia_oficial = colonia)
} 

colonias <- colonias %>% mutate(colonia_join = limpiar_texto(colonia_oficial))

# B) Limpiar Precios (Excel)
costo_col_clean <- costo_col %>%
  mutate(colonia_join = limpiar_texto(Colonia)) %>% 
  select(colonia_join, Precio_promedio_por_colonia) %>% 
  group_by(colonia_join) %>%
  summarise(Precio_promedio_por_colonia = mean(Precio_promedio_por_colonia, na.rm = TRUE))

# C) Unir Precios al Mapa (Para graficar)
mapa_con_precios <- colonias %>%
  left_join(costo_col_clean, by = "colonia_join")

# -------------------------------------------------------------------------
# 4. Obtengo nombres de colonias y FILTRO MAESTRO
# -------------------------------------------------------------------------

# A) Convertir listings a puntos espaciales
listings_sf <- base_listings %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(colonias)) 

# B) Join Espacial: ¿En qué polígono cae cada punto?
listings_con_colonia <- st_join(listings_sf, colonias[, c("colonia_join", "colonia_oficial")], join = st_within)

# --- [NUEVO PASO CRUCIAL] ---
# C) Generar una lista de colonias VÁLIDAS (Con más de 15 observaciones)
# Esto se usará para filtrar TODAS las secciones siguientes.
colonias_validas <- listings_con_colonia %>%
  st_drop_geometry() %>%
  group_by(colonia_join) %>%
  summarise(Num_Listings = n()) %>%
  filter(Num_Listings >= 15) 

# D) Pegar el Precio Promedio del Excel a cada Listing
# (También filtramos aquí para que la base final esté limpia)
base_listings_final <- listings_con_colonia %>%
  st_drop_geometry() %>% 
  left_join(costo_col_clean, by = "colonia_join") %>%
  filter(colonia_join %in% colonias_validas$colonia_join) # Filtro aplicado

# -------------------------------------------------------------------------
# 5. GENERAR EL MAPA Precio Promedio de Vivienda por Colonia (EXCEL)
# -------------------------------------------------------------------------

# 1. Filtramos el mapa espacial usando nuestra lista de colonias válidas
mapa_precios_filtrado <- mapa_con_precios %>%
  inner_join(colonias_validas[, c("colonia_join")], by = "colonia_join")

mapa_plot <- ggplot() +
  
  # CAPA 1: FONDO BASE (Gris para lo excluido)
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  
  # CAPA 2: DATOS POR COLONIA (Filtrados)
  geom_sf(data = mapa_precios_filtrado, 
          aes(fill = Precio_promedio_por_colonia), 
          color = "grey90", 
          size = 0.05) +
  
  # CAPA 3: BORDES DE ALCALDÍAS
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "Pesos Mexicanos (MXN)", 
                       labels = scales::dollar_format(),
                       na.value = "grey90",
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(0, 15000000), 
                       oob = scales::squish) + 
  
  labs(title = "Precio Promedio de Vivienda por Colonia",
       subtitle = "",
       caption = "Fuente: Elaboración propia con datos del INFONAVIT") +
  
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = NA))

print(mapa_plot)
ggsave("Mapa_Precios_Colonia.png", plot = mapa_plot, width = 10, height = 10, dpi = 300)

# -------------------------------------------------------------------------
# 7. GENERAR EL MAPA Renta Promedio por Colonia
# -------------------------------------------------------------------------

precio_airbnb_por_colonia <- base_listings_final %>%
  group_by(colonia_join) %>%
  summarise(Precio_Airbnb = mean(price, na.rm = TRUE)) # Ya está filtrado en base_listings_final

mapa_airbnb <- colonias %>%
  inner_join(precio_airbnb_por_colonia, by = "colonia_join") # inner_join fuerza el filtro

mapa_airbnb_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_airbnb, aes(fill = Precio_Airbnb), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "Pesos Mexicanos (MXN)", 
                       labels = scales::dollar_format(),
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(0, 5000), # Ajusta el precio máximo visible
                       oob = scales::squish,
                       na.value = "grey90") +
  
  labs(title = "Renta Promedio por noche",
       subtitle = "",
       caption = "Fuente: listings.csv procesado") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_airbnb_plot)
ggsave("Mapa_Precios_Airbnb_Filtrado.png", plot = mapa_airbnb_plot, width = 10, height = 10, dpi = 300)

# -------------------------------------------------------------------------
# 8. MAPA DE CONTEO DE OBSERVACIONES (DENSIDAD)
# -------------------------------------------------------------------------

{conteo_por_colonia <- base_listings_final %>%
  group_by(colonia_join) %>% 
  summarise(Num_Listings = n()) # Ya filtrado > 15

mapa_conteo <- colonias %>%
  inner_join(conteo_por_colonia, by = "colonia_join")

mapa_conteo_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_conteo, aes(fill = Num_Listings), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "plasma", direction = -1, 
                       name = "Número de\nListings",
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(15, 500), # Ajusta el máximo de listings a mostrar
                       oob = scales::squish,
                       na.value = "grey90") +
  
  labs(title = "Concentración de Oferta Airbnb",
       subtitle = "",
       caption = "Fuente: listings.csv procesado") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_conteo_plot)
ggsave("Mapa_Conteo_Listings.png", plot = mapa_conteo_plot, width = 10, height = 10, dpi = 300)}

# -------------------------------------------------------------------------
# 9. MAPA DE OCUPACIÓN PROMEDIO POR COLONIA
# -------------------------------------------------------------------------

ocupacion_por_colonia <- base_listings_final %>%
  group_by(colonia_join) %>%
  summarise(Ocupacion_Promedio = mean(occupancy_rate, na.rm = TRUE))

mapa_ocupacion <- colonias %>%
  inner_join(ocupacion_por_colonia, by = "colonia_join")

mapa_ocupacion_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_ocupacion, aes(fill = Ocupacion_Promedio), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "plasma", direction = -1, 
                       name = "Porcentaje (%)",
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(0, 100), # Ocupación siempre es 0-100
                       oob = scales::squish,
                       na.value = "grey90") +
  
  labs(title = "Tasa de Ocupación por Colonia",
       subtitle = "",
       caption = "Fuente: listings.csv procesado") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_ocupacion_plot)
ggsave("Mapa_Ocupacion_Colonia.png", plot = mapa_ocupacion_plot, width = 10, height = 10, dpi = 300)

# -------------------------------------------------------------------------
# 10. MAPA DE REVENUE (INGRESO ESTIMADO)
# -------------------------------------------------------------------------

revenue_por_colonia <- base_listings_final %>%
  group_by(colonia_join) %>%
  summarise(Revenue_Promedio = mean(estimated_revenue_l365d, na.rm = TRUE))

mapa_revenue <- colonias %>%
  inner_join(revenue_por_colonia, by = "colonia_join")

mapa_revenue_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_revenue, aes(fill = Revenue_Promedio), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "viridis", direction = -1, 
                       name = "Revenue Estimado\nPromedio (MXN)",
                       labels = scales::dollar_format(),
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(0, 300000), # Ajusta según ingresos razonables
                       oob = scales::squish,
                       na.value = "grey90") +
  
  labs(title = "Ingreso Estimado Anual por Colonia",
       subtitle = "",
       caption = "Fuente: listings.csv procesado") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_revenue_plot)
ggsave("Mapa_Revenue_Colonia.png", plot = mapa_revenue_plot, width = 10, height = 10, dpi = 300)

# -------------------------------------------------------------------------
# 11. MAPA DE DÍAS DE OCUPACIÓN
# -------------------------------------------------------------------------

{dias_ocupacion_colonia <- base_listings_final %>%
  group_by(colonia_join) %>%
  summarise(Dias_Ocupacion_Promedio = mean(occupancy_day, na.rm = TRUE))

mapa_dias_ocupacion <- colonias %>%
  inner_join(dias_ocupacion_colonia, by = "colonia_join")

mapa_dias_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_dias_ocupacion, aes(fill = Dias_Ocupacion_Promedio), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "inferno", direction = -1, 
                       name = "Días Ocupados\nPromedio (Año)",
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(0, 250), # Ajusta el máximo de días
                       oob = scales::squish,
                       na.value = "grey90") +
  
  labs(title = "Días de Ocupación por Colonia",
       subtitle = "",
       caption = "Fuente: listings.csv procesado") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_dias_plot)
ggsave("Mapa_Dias_Ocupacion_Colonia.png", plot = mapa_dias_plot, width = 10, height = 10, dpi = 300)}

# -------------------------------------------------------------------------
# 12. MAPA POR SCORE RATING
# -------------------------------------------------------------------------

{rating_por_colonia <- base_listings_final %>%
  group_by(colonia_join) %>%
  summarise(Rating_Promedio = mean(review_scores_rating, na.rm = TRUE))

mapa_rating <- colonias %>%
  inner_join(rating_por_colonia, by = "colonia_join")

mapa_rating_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_rating, aes(fill = Rating_Promedio), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "viridis", direction = 1, 
                       name = "Calificación\nPromedio (0-5)",
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(4.0, 5.0), # Ajustado para ver diferencias entre buenos y excelentes
                       oob = scales::squish,
                       na.value = "grey90") +
  
  labs(title = "Calificación Promedio por Colonia",
       subtitle = "",
       caption = "Fuente: listings.csv procesado") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_rating_plot)
ggsave("Mapa_Rating_Colonia.png", plot = mapa_rating_plot, width = 10, height = 10, dpi = 300)}

# -------------------------------------------------------------------------
# 13. MAPA POR SCORE LOCATION
# -------------------------------------------------------------------------

{location_por_colonia <- base_listings_final %>%
  group_by(colonia_join) %>%
  summarise(Location_Score_Promedio = mean(review_scores_location, na.rm = TRUE))

mapa_location <- colonias %>%
  inner_join(location_por_colonia, by = "colonia_join")

mapa_location_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_location, aes(fill = Location_Score_Promedio), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "viridis", direction = 1, 
                       name = "Puntaje Ubicación\nPromedio (0-5)",
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(4.5, 5.0), # La ubicación suele ser alta, esto ayuda al contraste
                       oob = scales::squish,
                       na.value = "grey90") +
  
  labs(title = "Calificación de Ubicación por Colonia",
       subtitle = "",
       caption = "Fuente: listings.csv procesado") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_location_plot)
ggsave("Mapa_Location_Score_Colonia.png", plot = mapa_location_plot, width = 10, height = 10, dpi = 300)}

# -------------------------------------------------------------------------
# 14. MAPA DE RENTABILIDAD (YIELD / CAP RATE)
# -------------------------------------------------------------------------

rentabilidad_por_colonia <- base_listings_final %>%
  group_by(colonia_join) %>%
  summarise(
    Revenue_Anual_Promedio = mean(estimated_revenue_l365d, na.rm = TRUE),
    Precio_Venta_Promedio = mean(Precio_promedio_por_colonia, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Rentabilidad_Anual = Precio_Venta_Promedio / Revenue_Anual_Promedio) %>%
  filter(!is.na(Rentabilidad_Anual) & is.finite(Rentabilidad_Anual))

mapa_rentabilidad <- colonias %>%
  inner_join(rentabilidad_por_colonia, by = "colonia_join")

mapa_rentabilidad_plot <- ggplot() +
  geom_sf(data = Alcaldia, fill = "grey90", color = NA) +
  geom_sf(data = mapa_rentabilidad, aes(fill = Rentabilidad_Anual), color = "grey90", size = 0.05) +
  geom_sf(data = Alcaldia, fill = NA, color = "black", size = 0.5) +
  
  scale_fill_viridis_c(option = "turbo", direction = 1, 
                       name = "Ratio Precio/Ingreso\n(Años)",
                       labels = scales::number_format(accuracy = 0.01),
                       # --- LÍMITE DE LEYENDA ---
                       limits = c(0, 100), 
                       oob = scales::squish, 
                       na.value = "grey90") +
  
  labs(title = "Ratio de Recuperación de Inversión",
       subtitle = "",
       caption = "Fuente: listings.csv y Costo_prom_col.xlsx") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), legend.position = "right")

print(mapa_rentabilidad_plot)
ggsave("Mapa_Rentabilidad_Colonia_Limitado.png", plot = mapa_rentabilidad_plot, width = 10, height = 10, dpi = 300)


# -------------------------------------------------------------------------
# 15. PRECIO PROMEDIO GENERAL Y POR colonia (CORREGIDO)
# -------------------------------------------------------------------------
cat("Generando tabla maestra con rentabilidad...\n")

# 1. PREPARACIÓN ESPACIAL (Corrección de geometría)
sf_use_s2(FALSE) 
colonias_valid <- st_make_valid(colonias)
listings_sf_col <- listings_sf %>% st_transform(st_crs(colonias_valid))

# Unimos espacialmente
listings_completo_con_colonia <- st_join(listings_sf_col, colonias_valid, join = st_within)
sf_use_s2(TRUE) 

# 2. CÁLCULO DE INDICADORES + RENTABILIDAD
tabla_maestra_colonias <- listings_completo_con_colonia %>%
  st_drop_geometry() %>% 
  
  # --- PASO CLAVE: Pegamos el Costo de la Vivienda (Excel) ---
  # Asegúrate de haber corrido la sección 3B para tener 'costo_col_clean'
  left_join(costo_col_clean, by = "colonia_join") %>%
  
  mutate(room_type = trimws(room_type)) %>%
  
  group_by(colonia_oficial) %>%
  summarise(
    # --- Datos de Airbnb ---
    Precio_Promedio_Noche = mean(price, na.rm = TRUE),
    Ingreso_Airbnb_Anual  = mean(estimated_revenue_l365d, na.rm = TRUE),
    Tasa_Ocupacion        = mean(occupancy_rate, na.rm = TRUE),
    Rating_Promedio       = mean(review_scores_rating, na.rm = TRUE),
    
    # --- Datos de la Propiedad (Del Excel) ---
    # Usamos mean() porque el precio de venta es el mismo para toda la colonia
    Precio_Venta_Inmueble = mean(Precio_promedio_por_colonia, na.rm = TRUE),
    
    # --- Otros ---
    Pct_Entire_Home       = (sum(room_type == "Entire home/apt", na.rm = TRUE) / n()) * 100,
    Recamaras_Promedio    = mean(bedrooms, na.rm = TRUE),
    Num_Listings          = n()
  ) %>%
  
  # Filtro de representatividad
  filter(Num_Listings > 15) %>% 
  
  # --- CÁLCULO DE RENTABILIDAD ---
  mutate(
    # A) ¿En cuántos años se paga la casa con estos ingresos?
    Anios_Recuperacion = Precio_Venta_Inmueble / Ingreso_Airbnb_Anual,
    
    # B) Rentabilidad Anual (Yield / Cap Rate simplificado)
    Rentabilidad_Anual_Pct = (Ingreso_Airbnb_Anual / Precio_Venta_Inmueble) * 100
  ) %>%
  
  # --- FORMATO FINAL ---
  mutate(
    Precio_Promedio_Noche = round(Precio_Promedio_Noche, 2),
    Ingreso_Airbnb_Anual  = round(Ingreso_Airbnb_Anual, 0),
    Precio_Venta_Inmueble = round(Precio_Venta_Inmueble, 0),
    Tasa_Ocupacion        = round(Tasa_Ocupacion, 2),
    Rating_Promedio       = round(Rating_Promedio, 2),
    Pct_Entire_Home       = round(Pct_Entire_Home, 1),
    Recamaras_Promedio    = round(Recamaras_Promedio, 1),
    Anios_Recuperacion    = round(Anios_Recuperacion, 1),
    Rentabilidad_Anual_Pct= round(Rentabilidad_Anual_Pct, 2)
  ) %>%
  
  # Ordenamos por la mejor rentabilidad (Yield más alto arriba)
  arrange(desc(Rentabilidad_Anual_Pct))

# 3. VER Y GUARDAR
print(head(tabla_maestra_colonias))

write.csv(tabla_maestra_colonias, "Indicadores_Rentabilidad_Colonia.csv", row.names = FALSE)
cat("Archivo generado: Indicadores_Rentabilidad_Colonia.csv\n")















