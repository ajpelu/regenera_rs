library(tidyverse)
library(sf)
library(terra)

# leo puntos 442 plots 
# raw_pts <- st_read("data/geoinfo/parcelas_sinfonevada_lorena.shp")
# 
# r <- terra::rast("/Users/ajpelu/DATA/CLIMA/REDIAM/03_MENSUAL/PRECIP_MENSUAL/InfGeografica/InfRaster/COG/p_1951_01_COG.tif")
# p <- st_transform(raw_pts, crs = st_crs(r)) |> 
#   dplyr::select(plot_id)
# st_write(p, "data/geoinfo/parcelas_reprojected_clima.shp", append = FALSE)
# comprobar que estan en la misma proyeccion
#plot(r)
#plot(st_geometry(pts_rep), add = TRUE, colour="red") 



p <- st_read("data/geoinfo/parcelas_reprojected_clima.shp") 


### Read and extract PREC 
f <- "/Users/ajpelu/DATA/CLIMA/REDIAM/03_MENSUAL/PRECIP_MENSUAL/InfGeografica/InfRaster/COG/"
files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

extraidos <- map(files, function(f) {
  r <- terra::rast(f)
  val <- terra::extract(r, p, xy = FALSE)
  val
})

final_df <- reduce(extraidos, inner_join, by = c("ID"))

df <- final_df |> 
  pivot_longer(-c(ID)) |> 
  mutate(name = str_remove(name, "p_")) |>
  mutate(name = str_remove(name, "_COG")) |>
  separate(name, into = c("year", "mes")) |> 
  mutate(variable = "prec") 

df_prec <- df 


### Read and extract TMED
f <- "/Users/ajpelu/DATA/CLIMA/REDIAM/03_MENSUAL/TEMP_MEDIA_MENSUAL/InfGeografica/InfRaster/COG/"

files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

extraidos <- map(files, function(f) {
  r <- terra::rast(f)
  val <- terra::extract(r, p, xy = FALSE)
  val
})

final_df <- reduce(extraidos, inner_join, by = c("ID"))

df  <- final_df |> 
  pivot_longer(-c(ID)) |> 
  mutate(name = str_remove(name, "tmed_")) |>
  mutate(name = str_remove(name, "_COG")) |>
  separate(name, into = c("year", "mes")) |> 
  mutate(variable = "tmed") 

df_tmed <- df 


### Read and extract TMAX
f <- "/Users/ajpelu/DATA/CLIMA/REDIAM/03_MENSUAL/TEMP_MAXIMA_MENSUAL/InfGeografica/InfRaster/COG/"

files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

extraidos <- map(files, function(f) {
  r <- terra::rast(f)
  val <- terra::extract(r, p, xy = FALSE)
  val
})

final_df <- reduce(extraidos, inner_join, by = c("ID"))

df  <- final_df |> 
  pivot_longer(-c(ID)) |> 
  mutate(name = str_remove(name, "tm3_")) |>
  mutate(name = str_remove(name, "_COG")) |>
  separate(name, into = c("year", "mes")) |> 
  mutate(variable = "tmax") 

df_tmax <- df 



### Read and extract TMIN
f <- "/Users/ajpelu/DATA/CLIMA/REDIAM/03_MENSUAL/TEMP_MINIMA_MENSUAL/InfGeografica/InfRaster/COG/"

files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

extraidos <- map(files, function(f) {
  r <- terra::rast(f)
  val <- terra::extract(r, p, xy = FALSE)
  val
})

final_df <- reduce(extraidos, inner_join, by = c("ID"))

df  <- final_df |> 
  pivot_longer(-c(ID)) |> 
  mutate(name = str_remove(name, "tm2_")) |>
  mutate(name = str_remove(name, "_COG")) |>
  separate(name, into = c("year", "mes")) |> 
  mutate(variable = "tmin") 

df_tmin <- df 




saveRDS(df_prec, file = "data/climate/prec_mensual.rds")
saveRDS(df_tmed, file = "data/climate/tmed_mensual.rds")
saveRDS(df_tmax, file = "data/climate/tmax_mensual.rds")
saveRDS(df_tmin, file = "data/climate/tmin_mensual.rds")


### Compute annual means for temp and cummulative for precipitation 
prec_year <- df_prec |> 
  group_by(ID, year) |> 
  summarise(value = sum(value, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(variable = "prec")

tmed_year <- df_tmed |> 
  group_by(ID, year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  ungroup() |> 
  mutate(variable = "tmed")

tmin_year <- df_tmin |> 
  group_by(ID, year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  ungroup() |> 
  mutate(variable = "tmin")

tmax_year <- df_tmax |> 
  group_by(ID, year) |>
  summarise(value = mean(value, na.rm = TRUE)) |>
  ungroup() |> 
  mutate(variable = "tmax")


climate_annual <- bind_rows(prec_year, tmed_year, tmin_year, tmax_year)

saveRDS(climate_annual, file = "data/climate/climate_annual.rds")








##### esto es porque los fucking REDIAM fallan (no ejecutar)
# corregir_fuckingREDIAM <- function(file) { 
#   j <- terra::rast(file)
#   name_ok <- tools::file_path_sans_ext(basename(sources(j)))
#   names(j) <- name_ok
#   j2 <- j + 0  # This creates a copy of the raster with the same values
#   terra::writeRaster(j2, filename = file, overwrite = TRUE)
#   }
# 
# 
# files[877:888] |> 
#   map(corregir_fuckingREDIAM)



