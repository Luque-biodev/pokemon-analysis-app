# download.R
# Descarga TODO lo que necesita la app dentro de data/ (se ejecuta una vez):
#   data/pokemon.csv, data/sprites/, data/sprites_mini/, data/tipos/

dir.create("data", showWarnings = FALSE)

# ---- Datos -----------------------------------------------------------
pokemon <- read.csv(paste0(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/",
  "main/data/2025/2025-04-01/pokemon_df.csv"
))
write.csv2(pokemon, "data/pokemon.csv", row.names = FALSE)  # ";" y decimal ","

# ---- Función genérica de descarga (salta lo que ya existe) -------------
descargar <- function(url, destino) {
  if (file.exists(destino) && file.size(destino) > 0) return(TRUE)
  ok <- tryCatch(
    download.file(url, destino, mode = "wb", quiet = TRUE) == 0,
    error = function(e) FALSE, warning = function(w) FALSE
  )
  if (!ok && file.exists(destino)) file.remove(destino)
  ok
}

base_pokeapi <- "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/"

bajar_sprites <- function(carpeta, ruta_remota, ids) {
  dir.create(file.path("data", carpeta), showWarnings = FALSE)
  ok <- vapply(ids, function(id) {
    descargar(paste0(base_pokeapi, ruta_remota, id, ".png"),
              file.path("data", carpeta, paste0(id, ".png")))
  }, logical(1))
  message(carpeta, ": ", sum(ok), " descargados, ", sum(!ok), " no disponibles")
  invisible(ids[!ok])
}

# ---- Sprites normales y reducidos --------------------------------------
fallidos_normales <- bajar_sprites("sprites",      "pokemon/",                              pokemon$id)
fallidos_mini     <- bajar_sprites("sprites_mini", "pokemon/versions/generation-v/icons/",  pokemon$id)
# Los iconos de generation-v solo existen hasta el id 649: el resto se
# dibuja sin sprite en esa carpeta (la app lo avisa en "Opciones").

# ---- Iconos de tipo ----------------------------------------------------
tipos <- c("normal", "fighting", "flying", "poison", "ground", "rock", "bug",
           "ghost", "steel", "fire", "water", "grass", "electric", "psychic",
           "ice", "dragon", "dark")        # el orden es el id 1..17 de PokeAPI

dir.create("data/tipos", showWarnings = FALSE)
ok_tipos <- vapply(seq_along(tipos), function(i) {
  descargar(paste0(base_pokeapi, "types/generation-v/black-white/", i, ".png"),
            file.path("data/tipos", paste0(tipos[i], ".png")))
}, logical(1))
message("tipos: ", sum(ok_tipos), "/", length(tipos))

# ---- Comprobación ------------------------------------------------------
for (d in c("sprites", "sprites_mini", "tipos")) {
  message(d, ": ", length(list.files(file.path("data", d), pattern = "\\.png$")), " archivos")
}
