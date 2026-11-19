# app.R  (v8)
# Aplicación Shiny: análisis interactivo de Pokémon (TidyTuesday 2025-04-01)
#
# TODO se lee desde la carpeta data/ (la prepara download.R):
#   data/pokemon.csv        datos (separador ";" y decimal ",": write.csv2)
#   data/sprites/<id>.png   sprites normales   \ se elige cuál usar
#   data/sprites_mini/<id>.png  sprites reducidos / en la pestaña "Opciones"
#   data/tipos/<tipo>.png   iconos de los tipos (fire.png, water.png, ...)
#
# Instalar una sola vez, si es necesario:
# install.packages(c("shiny", "tidyverse", "ggbeeswarm", "DT", "scales",
#                    "ggtext", "png", "uwot", "ragg", "svglite"))
# - png:    decodifica los sprites (sustituye a ggimage/magick)
# - ggtext: dibuja los iconos de tipo en ejes y leyendas
# - uwot:   solo para la pestaña UMAP
# - ragg:   recomendado para que los emojis de las etiquetas se vean bien
#           y para guardar los .png
# - svglite: guarda los .svg (si falta, se usa grDevices::svg, que necesita cairo)

library(shiny)
library(tidyverse)
library(ggbeeswarm)
library(ggtext)
library(DT)
library(scales)
library(grid)

hay_umap <- requireNamespace("uwot", quietly = TRUE)

# Con ragg, renderPlot dibuja bien los emojis (❤️ ⚔️ 🛡️ ...)
if (requireNamespace("ragg", quietly = TRUE)) options(shiny.useragg = TRUE)

# -------------------------------------------------------------------
# 0. Recursos locales (todo dentro de data/) y opciones globales
# -------------------------------------------------------------------

carpeta_datos    <- "data"
carpeta_tipos    <- file.path(getwd(), carpeta_datos, "tipos")
carpetas_sprites <- c("sprites", "sprites_mini")

ruta_sprite <- function(ids, carpeta) {
  file.path(getwd(), carpeta_datos, carpeta, paste0(ids, ".png"))
}

# Ids disponibles en cada carpeta: se lista el disco UNA vez al arrancar
# (después ya no hace falta file.exists() por cada sprite).
ids_sprites <- setNames(lapply(carpetas_sprites, function(carpeta) {
  f <- list.files(file.path(carpeta_datos, carpeta), pattern = "\\.png$")
  suppressWarnings(as.integer(sub("\\.png$", "", f)))
}), carpetas_sprites)

# Solo se ofrecen las carpetas con contenido (si no hay ninguna, se ofrecen
# ambas para que la interfaz no quede vacía)
carpetas_sprites_ok <- carpetas_sprites[lengths(ids_sprites) > 0]
if (length(carpetas_sprites_ok) == 0) {
  message("No hay sprites en ", file.path(getwd(), carpeta_datos),
          " (ejecuta download.R). Se dibujará sin sprites.")
  carpetas_sprites_ok <- carpetas_sprites
}
carpeta_sprites_ini <- if ("sprites" %in% carpetas_sprites_ok) "sprites" else carpetas_sprites_ok[1]

if (!dir.exists(carpeta_tipos)) {
  message("No se encuentra ", carpeta_tipos, ": los tipos se mostrarán como texto.")
}

# --- Caché de sprites -------------------------------------------------
# ggimage::geom_image() abre y decodifica cada PNG (vía magick) en CADA
# dibujo. Aquí cada sprite se decodifica una sola vez, de forma perezosa
# (solo los que llegan a dibujarse), como `nativeRaster` (4 bytes/píxel:
# ~37 KB un sprite de 96x96, ~4 KB uno de 32x32) y se comparte entre todos
# los gráficos y sesiones. Tope de entradas: al llenarse se expulsa la
# mitad al azar, así la memoria nunca crece sin límite.
CACHE_SPRITES_MAX <- 800L
cache_sprites <- new.env(parent = emptyenv())

leer_sprite <- function(id, carpeta) {
  clave <- paste0(carpeta, "/", id)
  img <- cache_sprites[[clave]]
  if (is.null(img)) {
    img <- FALSE   # FALSE = no existe / no se pudo leer (también se cachea)
    if (id %in% ids_sprites[[carpeta]]) {
      img <- tryCatch(
        png::readPNG(ruta_sprite(id, carpeta), native = TRUE),
        error = function(e) FALSE
      )
    }
    if (length(cache_sprites) >= CACHE_SPRITES_MAX) {
      rm(list = sample(ls(cache_sprites), CACHE_SPRITES_MAX %/% 2), envir = cache_sprites)
    }
    assign(clave, img, envir = cache_sprites)
  }
  if (isFALSE(img)) NULL else img
}

# Geom propio: sustituye a geom_image(). Solo necesita la columna `id`
# (aes(sprite_id = id)); no hay que guardar rutas en los datos.
# `size` es la fracción del lado menor del panel (ajustado con FACTOR_TAM
# para que se parezca al tamaño que daba geom_image).
FACTOR_TAM <- 1.4

GeomSprite <- ggproto("GeomSprite", Geom,
  required_aes = c("x", "y", "sprite_id"),
  draw_key = draw_key_blank,
  draw_panel = function(data, panel_params, coord, size = 0.05, carpeta = "sprites") {
    co <- coord$transform(data, panel_params)
    ancho <- unit(size * FACTOR_TAM, "snpc")
    hijos <- lapply(seq_len(nrow(co)), function(i) {
      img <- leer_sprite(co$sprite_id[i], carpeta)
      if (is.null(img)) return(NULL)
      rasterGrob(
        img, x = unit(co$x[i], "npc"), y = unit(co$y[i], "npc"),
        width = ancho, height = ancho, interpolate = TRUE
      )
    })
    gTree(children = do.call(gList, Filter(Negate(is.null), hijos)))
  }
)

geom_sprite <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", ..., size = 0.05,
                        carpeta = "sprites", show.legend = FALSE,
                        inherit.aes = TRUE) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomSprite,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(size = size, carpeta = carpeta, na.rm = FALSE, ...)
  )
}

# Opciones que reciben todas las funciones de dibujo (argumento `opc`).
# El menú "Opciones" las modifica desde el servidor.
opciones_defecto <- list(fuente = 12, escala_sprites = 1, carpeta = carpeta_sprites_ini)

valor_o <- function(x, defecto) if (is.null(x)) defecto else x

# Tamaño de un sprite (o de su punto de fondo) tras aplicar la escala global
tam_sprite <- function(tam, opc = opciones_defecto) tam * opc$escala_sprites

# Tamaño de texto para geom_text/geom_label (mm) equivalente a `factor` veces la fuente base
tam_texto <- function(opc = opciones_defecto, factor = 1) factor * opc$fuente / .pt

# Ancho (px) de los iconos de tipo; crece con la fuente
tam_icono_tipo <- function(opc = opciones_defecto) round(1.8 * opc$fuente)

# -------------------------------------------------------------------
# 1. Carga y preparación de datos
# -------------------------------------------------------------------

archivo_datos <- file.path(carpeta_datos, "pokemon.csv")
if (!file.exists(archivo_datos)) {
  stop("No se encuentra ", archivo_datos, ". Ejecuta antes download.R.")
}

# download.R lo guarda con write.csv2 (separador ";" y decimal ",")
pokemon_raw <- read.csv2(archivo_datos, stringsAsFactors = FALSE)

stats_pca <- c(
  "hp", "attack", "defense",
  "special_attack", "special_defense", "speed"
)

columnas_necesarias <- c(stats_pca, "type_1", "id")
faltan <- setdiff(columnas_necesarias, names(pokemon_raw))

if (length(faltan) > 0) {
  stop("Faltan columnas necesarias en el CSV: ", paste(faltan, collapse = ", "))
}

pokemon <- pokemon_raw %>%
  filter(if_all(all_of(columnas_necesarias), ~ !is.na(.x))) %>%
  mutate(
    type_1 = as.factor(type_1),
    type_2 = as.character(type_2),
    generation_id = as.integer(generation_id),
    across(all_of(stats_pca), as.numeric)
  ) %>%
  filter(if_all(all_of(stats_pca), is.finite))

# Nombre del Pokémon (columna "pokemon" del CSV; si no existe, se usa el id)
if (!"pokemon" %in% names(pokemon)) pokemon$pokemon <- as.character(pokemon$id)
pokemon$pokemon <- as.character(pokemon$pokemon)

# Variables numéricas, excluyendo identificadores.
pokemon_num <- pokemon %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("id", "species_id", "generation_id")))

vars_numericas <- names(pokemon_num)

# PCA calculado una sola vez; después se filtra por generación/tipo.
pca <- prcomp(
  pokemon %>% select(all_of(stats_pca)),
  center = TRUE,
  scale. = TRUE
)

pokemon_plot <- pokemon %>%
  mutate(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    PC3 = pca$x[, 3]
  )

# Distancia de Mahalanobis (al cuadrado) sobre las 6 estadísticas de combate
X_stats <- as.matrix(pokemon[, stats_pca])
pokemon_plot$mahalanobis <- mahalanobis(X_stats, colMeans(X_stats), cov(X_stats))

# UMAP: se calcula una sola vez (requiere el paquete uwot)
if (hay_umap) {
  set.seed(42)
  emb <- uwot::umap(scale(X_stats), n_neighbors = 15, min_dist = 0.3)
  pokemon_plot$UMAP1 <- emb[, 1]
  pokemon_plot$UMAP2 <- emb[, 2]
}

max_stat <- max(X_stats)
nombres_pokemon <- sort(unique(pokemon_plot$pokemon))
vars_explorador <- c(vars_numericas, "PC1", "PC2", "PC3", "mahalanobis")

tipos <- sort(unique(as.character(pokemon_plot$type_1)))
generaciones <- sort(unique(na.omit(pokemon_plot$generation_id)))

# -------------------------------------------------------------------
# 1b. Colores por tipo, iconos de tipo y etiquetas de estadísticas
# -------------------------------------------------------------------

colores_tipo <- c(
  normal   = "#A8A77A",
  fire     = "#EE8130",
  water    = "#6390F0",
  electric = "#F7D02C",
  grass    = "#7AC74C",
  ice      = "#96D9D6",
  fighting = "#C22E28",
  poison   = "#A33EA1",
  ground   = "#E2BF65",
  flying   = "#A98FF3",
  psychic  = "#F95587",
  bug      = "#A6B91A",
  rock     = "#B6A136",
  ghost    = "#735797",
  dragon   = "#6F35FC",
  dark     = "#705746",
  steel    = "#B7B7CE",
  fairy    = "#D685AD"
)

# Si apareciera algún tipo no contemplado, se le asigna un gris para
# que no falle la escala.
tipos_sin_color <- setdiff(tipos, names(colores_tipo))
if (length(tipos_sin_color) > 0) {
  colores_tipo <- c(
    colores_tipo,
    setNames(rep("grey60", length(tipos_sin_color)), tipos_sin_color)
  )
}

# Etiquetas de las estadísticas (mismo orden que stats_pca)
iconos_stats <- c(
  "HP ❤️",
  "Attack ⚔️",
  "Defense 🛡️",
  "Sp. Atk ☄️",
  "Sp. Def 🧿",
  "Speed 🏃"
)
names(iconos_stats) <- stats_pca

# Etiqueta de cualquier variable: si es una estadística de combate se usa
# su icono; el resto (height, PC1, ...) se deja con su nombre.
etiqueta_variable <- function(v) {
  v <- as.character(v)
  ifelse(v %in% names(iconos_stats), unname(iconos_stats[v]), v)
}

# Etiqueta de un tipo: <img> con el icono de tipos/<tipo>.png (la renderiza
# ggtext::element_markdown). Si no existe el archivo, se usa el nombre.
etiqueta_tipo <- function(tipos, tam = 22) {
  tipos <- as.character(tipos)
  ruta <- normalizePath(
    file.path(carpeta_tipos, paste0(tipos, ".png")),
    winslash = "/", mustWork = FALSE
  )
  ifelse(file.exists(ruta), sprintf("<img src='%s' width='%d'/>", ruta, as.integer(tam)), tipos)
}

# Versión lista para usar en `labels =` de cualquier escala
etiquetas_tipo_fn <- function(opc = opciones_defecto) {
  function(x) etiqueta_tipo(x, tam_icono_tipo(opc))
}

escala_color_tipo <- function(nombre = "Tipo principal", opc = opciones_defecto) {
  scale_color_manual(values = colores_tipo, name = nombre, labels = etiquetas_tipo_fn(opc))
}

escala_fill_tipo <- function(nombre = "Tipo principal", opc = opciones_defecto) {
  scale_fill_manual(values = colores_tipo, name = nombre, labels = etiquetas_tipo_fn(opc))
}

# Eje X discreto cuyo grupo son parámetros (iconos_stats) o tipos (iconos)
escala_x_grupo <- function(variables, opc = opciones_defecto) {
  if (length(variables) > 1) {
    scale_x_discrete(labels = etiqueta_variable)
  } else {
    scale_x_discrete(labels = etiquetas_tipo_fn(opc))
  }
}

# -------------------------------------------------------------------
# 2. Funciones auxiliares
# -------------------------------------------------------------------

# Tema base. Con markdown = TRUE los ejes, la leyenda y las facetas admiten
# los iconos de tipo (<img>); con FALSE se usa texto plano (p. ej. radar).
tema_base <- function(opc = opciones_defecto, markdown = TRUE) {
  el <- if (markdown) element_markdown else element_text
  theme_minimal(base_size = opc$fuente) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = el(),
      axis.text.y = el(),
      legend.text = el(),
      strip.text = el(),
      # Hueco suficiente para que los iconos de la leyenda no se solapen
      legend.key.height = unit(tam_icono_tipo(opc) * 0.75 + 2, "pt")
    )
}

tema_pokemon <- function(opc = opciones_defecto, angulo_x = 45, markdown = TRUE) {
  tema_base(opc, markdown) +
    theme(axis.text.x = (if (markdown) element_markdown else element_text)(
      angle = angulo_x,
      hjust = if (angulo_x == 0) 0.5 else 1
    ))
}

grafico_pca <- function(datos, x = "PC1", y = "PC2", tam = 0.055,
                        mostrar_sprites = TRUE, vectores = FALSE,
                        titulo = "Análisis de componentes principales (PCA)",
                        opc = opciones_defecto) {
  tam <- tam_sprite(tam, opc)
  p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]]))
  
  if (mostrar_sprites) {
    # Punto de color por debajo del sprite para que el tipo siga visible
    p <- p +
      geom_point(aes(color = type_1), size = tam * 180, alpha = 0.35) +
      geom_sprite(carpeta = opc$carpeta, aes(sprite_id = id), size = tam)
  } else {
    p <- p + geom_point(aes(color = type_1), alpha = 0.8, size = 2.5)
  }
  
  # Biplot: flechas con la contribución de cada variable (solo para PCs)
  if (vectores && all(c(x, y) %in% colnames(pca$rotation))) {
    carga <- tibble(
      variable = rownames(pca$rotation),
      cx = pca$rotation[, x],
      cy = pca$rotation[, y]
    )
    esc <- 0.9 * min(
      max(abs(datos[[x]])) / max(abs(carga$cx)),
      max(abs(datos[[y]])) / max(abs(carga$cy))
    )
    p <- p +
      geom_segment(
        data = carga,
        aes(x = 0, y = 0, xend = cx * esc, yend = cy * esc),
        arrow = arrow(length = unit(0.25, "cm")),
        inherit.aes = FALSE, color = "grey20"
      ) +
      geom_text(
        data = carga,
        aes(x = cx * esc * 1.1, y = cy * esc * 1.1, label = etiqueta_variable(variable)),
        inherit.aes = FALSE, fontface = "bold", size = tam_texto(opc, 0.95)
      )
  }
  
  p +
    escala_color_tipo(opc = opc) +
    labs(title = titulo, x = x, y = y) +
    tema_pokemon(opc)
}

# Pasa a formato largo y define el grupo del eje X:
#  - varios tipos + 1 parámetro  -> el eje X son los tipos
#  - 1 tipo + varios parámetros  -> el eje X son los parámetros
preparar_largo <- function(datos, tipos_sel, variables) {
  largo <- datos %>%
    filter(as.character(type_1) %in% tipos_sel) %>%
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "valor"
    )
  
  if (length(variables) > 1) {
    largo %>% mutate(grupo = factor(variable, levels = variables))
  } else {
    largo %>% mutate(grupo = factor(as.character(type_1), levels = tipos_sel))
  }
}

titulo_comparacion <- function(tipos_sel, variables) {
  if (length(variables) > 1) {
    paste0("Comparación de parámetros (tipo ", tipos_sel[1], ")")
  } else {
    paste(etiqueta_variable(variables), "por tipo de Pokémon")
  }
}

grafico_boxplot_tipo <- function(datos, tipos_sel, variables, opc = opciones_defecto) {
  largo <- preparar_largo(datos, tipos_sel, variables)
  
  limites <- largo %>%
    group_by(grupo) %>%
    summarise(
      q1 = quantile(valor, 0.25, na.rm = TRUE),
      q3 = quantile(valor, 0.75, na.rm = TRUE),
      iqr = IQR(valor, na.rm = TRUE),
      inferior = q1 - 1.5 * iqr,
      superior = q3 + 1.5 * iqr,
      .groups = "drop"
    )
  
  atipicos <- largo %>%
    left_join(limites, by = "grupo") %>%
    filter(valor < inferior | valor > superior)
  
  # Misma posición (misma semilla) para el punto de color y el sprite
  pos <- position_jitter(width = 0.15, height = 0, seed = 1)
  
  ggplot(largo, aes(x = grupo, y = valor)) +
    geom_boxplot(
      aes(fill = type_1),
      outlier.shape = NA,
      alpha = 0.6
    ) +
    geom_point(
      data = atipicos,
      aes(color = type_1),
      size = tam_sprite(9, opc), alpha = 0.35,
      position = pos
    ) +
    geom_sprite(carpeta = opc$carpeta, 
      data = atipicos,
      aes(sprite_id = id),
      size = tam_sprite(0.085, opc),
      position = pos
    ) +
    escala_fill_tipo(opc = opc) +
    escala_color_tipo(opc = opc) +
    escala_x_grupo(variables, opc) +
    labs(
      title = titulo_comparacion(tipos_sel, variables),
      subtitle = "Los valores atípicos se muestran con sprites",
      x = if (length(variables) > 1) "Parámetro" else "Tipo principal",
      y = if (length(variables) > 1) "Valor" else etiqueta_variable(variables)
    ) +
    tema_pokemon(opc, angulo_x = 0) +
    theme(legend.position = "none")
}

grafico_violin <- function(datos, tipos_sel, variables, tam = 0.055,
                           opc = opciones_defecto) {
  largo <- preparar_largo(datos, tipos_sel, variables)
  
  ggplot(largo, aes(x = grupo, y = valor)) +
    geom_violin(
      aes(fill = type_1),
      scale = "width",
      alpha = 0.35,
      color = NA,
      trim = FALSE
    ) +
    geom_sprite(carpeta = opc$carpeta, 
      aes(sprite_id = id),
      size = tam_sprite(tam, opc),
      position = position_quasirandom(width = 0.3, groupOnX = TRUE)
    ) +
    escala_fill_tipo(opc = opc) +
    escala_x_grupo(variables, opc) +
    labs(
      title = sub("por tipo de Pokémon", "por tipo (violin)",
                  titulo_comparacion(tipos_sel, variables)),
      subtitle = "Violines y sprites distribuidos mediante quasirandom",
      x = if (length(variables) > 1) "Parámetro" else "Tipo principal",
      y = if (length(variables) > 1) "Valor" else etiqueta_variable(variables)
    ) +
    tema_pokemon(opc, angulo_x = 0) +
    theme(legend.position = "none")
}

# Explorador universal: cualquier X/Y, tamaño, facetas, sprites y recta
grafico_explorador <- function(datos, x, y, tam_var = "ninguno",
                               usar_sprites = FALSE, linea = FALSE,
                               facetas = "ninguna", opc = opciones_defecto) {
  p <- ggplot(datos, aes(x = .data[[x]], y = .data[[y]]))
  
  if (tam_var != "ninguno") {
    p <- p +
      geom_point(aes(color = type_1, size = .data[[tam_var]]), alpha = 0.7) +
      scale_size_continuous(range = c(1, 10), name = etiqueta_variable(tam_var))
  } else {
    p <- p + geom_point(aes(color = type_1), size = 3, alpha = 0.7)
  }
  
  if (usar_sprites) {
    p <- p + geom_sprite(carpeta = opc$carpeta, aes(sprite_id = id), size = tam_sprite(0.035, opc))
  }
  
  if (linea) {
    p <- p + geom_smooth(
      aes(group = 1), method = "lm", formula = y ~ x,
      se = FALSE, color = "grey20", linewidth = 0.8
    )
  }
  
  if (facetas == "tipo") {
    p <- p + facet_wrap(vars(type_1), labeller = as_labeller(etiquetas_tipo_fn(opc)))
  }
  if (facetas == "generación") {
    p <- p + facet_wrap(vars(generation_id), labeller = label_both)
  }
  
  p +
    escala_color_tipo(opc = opc) +
    labs(
      title = paste(etiqueta_variable(y), "frente a", etiqueta_variable(x)),
      x = etiqueta_variable(x), y = etiqueta_variable(y)
    ) +
    tema_base(opc)
}

# Raincloud: medio violín + boxplot + puntos (sprites en los outliers)
grafico_raincloud <- function(datos, tipos_sel, variable, opc = opciones_defecto) {
  d <- datos %>%
    filter(as.character(type_1) %in% tipos_sel) %>%
    mutate(
      grupo = factor(as.character(type_1), levels = tipos_sel),
      pos = as.numeric(grupo),
      valor = .data[[variable]]
    )
  
  nube <- d %>%
    group_by(grupo, pos, type_1) %>%
    filter(n() > 1, n_distinct(valor) > 1) %>%
    group_modify(~ {
      dd <- density(.x$valor, n = 128)
      tibble(y = dd$x, dens = dd$y / max(dd$y))
    }) %>%
    ungroup()
  
  atipicos <- d %>%
    group_by(grupo) %>%
    mutate(
      q1 = quantile(valor, 0.25),
      q3 = quantile(valor, 0.75),
      iqr = q3 - q1
    ) %>%
    ungroup() %>%
    filter(valor < q1 - 1.5 * iqr | valor > q3 + 1.5 * iqr)
  
  p <- ggplot(d, aes(x = pos, y = valor))
  
  if (nrow(nube) > 0) {
    p <- p + geom_ribbon(
      data = nube,
      aes(y = y, xmin = pos - 0.08 - 0.38 * dens, xmax = pos - 0.08,
          fill = type_1, group = grupo),
      orientation = "y", alpha = 0.6, inherit.aes = FALSE
    )
  }
  
  p +
    geom_boxplot(
      aes(group = grupo, fill = type_1),
      width = 0.12, outlier.shape = NA, alpha = 0.8
    ) +
    geom_point(
      aes(x = pos + 0.16, color = type_1),
      size = 1.4, alpha = 0.6,
      position = position_jitter(width = 0.07, height = 0, seed = 1)
    ) +
    geom_sprite(carpeta = opc$carpeta, 
      data = atipicos,
      aes(x = pos + 0.16, sprite_id = id),
      size = tam_sprite(0.06, opc)
    ) +
    scale_x_continuous(
      breaks = seq_along(tipos_sel),
      labels = etiqueta_tipo(tipos_sel, tam_icono_tipo(opc))
    ) +
    escala_fill_tipo(opc = opc) +
    escala_color_tipo(opc = opc) +
    labs(
      title = paste("Raincloud de", etiqueta_variable(variable)),
      subtitle = "Densidad · boxplot · observaciones (sprites en los outliers)",
      x = "Tipo principal", y = etiqueta_variable(variable)
    ) +
    tema_pokemon(opc, angulo_x = 0) +
    theme(legend.position = "none")
}

# ECDF por tipo, con la curva global como referencia opcional
grafico_ecdf <- function(datos, tipos_sel, variable, referencia = TRUE,
                         opc = opciones_defecto) {
  d <- datos %>% filter(as.character(type_1) %in% tipos_sel)
  
  p <- ggplot(d, aes(x = .data[[variable]], color = type_1))
  
  if (referencia) {
    p <- p + stat_ecdf(
      data = datos, aes(x = .data[[variable]]), inherit.aes = FALSE,
      color = "black", linetype = "dashed", linewidth = 0.8, pad = FALSE
    )
  }
  
  p +
    stat_ecdf(linewidth = 1, pad = FALSE) +
    escala_color_tipo(opc = opc) +
    labs(
      title = paste("ECDF de", etiqueta_variable(variable), "por tipo"),
      subtitle = if (referencia) "Línea negra discontinua: todos los Pokémon" else NULL,
      x = etiqueta_variable(variable), y = "Proporción acumulada"
    ) +
    tema_base(opc)
}

# Heatmap de correlaciones, con orden por clustering jerárquico
grafico_correlacion <- function(datos, variables, ordenar = TRUE,
                                opc = opciones_defecto) {
  m <- cor(datos[, variables], use = "pairwise.complete.obs")
  
  if (ordenar && ncol(m) > 2) {
    o <- hclust(as.dist(1 - m))$order
    m <- m[o, o]
  }
  
  largo <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
  names(largo) <- c("v1", "v2", "r")
  largo$v1 <- factor(largo$v1, levels = colnames(m))
  largo$v2 <- factor(largo$v2, levels = rev(colnames(m)))
  
  ggplot(largo, aes(x = v1, y = v2, fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(r, 2)), size = tam_texto(opc, 0.83)) +
    scale_fill_gradient2(
      low = "#3B4CC0", mid = "white", high = "#B40426",
      midpoint = 0, limits = c(-1, 1), name = "r"
    ) +
    scale_x_discrete(labels = etiqueta_variable) +
    scale_y_discrete(labels = etiqueta_variable) +
    labs(
      title = "Correlación entre variables",
      subtitle = if (ordenar) "Ordenadas por clustering jerárquico" else NULL,
      x = NULL, y = NULL
    ) +
    tema_pokemon(opc, angulo_x = 45)
}

# Heatmap clusterizado (z-score) con la barra lateral coloreada por tipo
# (gráfico de R base: `opc$fuente` solo escala el tamaño del texto)
dibujar_heatmap <- function(datos, variables, max_filas = 250,
                            opc = opciones_defecto) {
  set.seed(1)
  if (nrow(datos) > max_filas) datos <- datos[sample(nrow(datos), max_filas), ]
  f <- opc$fuente / opciones_defecto$fuente
  
  mat <- scale(as.matrix(datos[, variables]))
  mat <- mat[, colSums(is.na(mat)) == 0, drop = FALSE]
  rownames(mat) <- make.unique(datos$pokemon)
  tipos_fila <- as.character(datos$type_1)
  
  heatmap(
    mat,
    scale = "none",
    col = hcl.colors(60, "RdBu", rev = TRUE),
    RowSideColors = unname(colores_tipo[tipos_fila]),
    labRow = if (nrow(mat) <= 60) rownames(mat) else rep("", nrow(mat)),
    margins = c(9, 9),
    cexRow = (0.2 + 1 / log10(nrow(mat))) * f,
    cexCol = 1.1 * f,
    main = "Heatmap clusterizado (z-score)"
  )
  
  # Leyenda de tipos superpuesta sobre toda la figura
  op <- par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  on.exit(par(op))
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  presentes <- sort(unique(tipos_fila))
  legend(
    "topright", legend = presentes, fill = colores_tipo[presentes],
    cex = 0.75 * f, title = "Tipo", bg = "white"
  )
}

# Coordenadas paralelas (variables reescaladas a 0-1) con resalte opcional
grafico_paralelas <- function(datos, tipos_sel, variables, resaltar = "(ninguno)",
                              opc = opciones_defecto) {
  d <- datos %>%
    filter(as.character(type_1) %in% tipos_sel) %>%
    mutate(across(all_of(variables), ~ scales::rescale(.x)))
  
  largo <- d %>%
    select(id, pokemon, type_1, all_of(variables)) %>%
    pivot_longer(all_of(variables), names_to = "variable", values_to = "valor") %>%
    mutate(variable = factor(variable, levels = variables))
  
  p <- ggplot(largo, aes(x = variable, y = valor, group = id, color = type_1)) +
    geom_line(alpha = 0.35, linewidth = 0.5)
  
  if (resaltar != "(ninguno)") {
    destacado <- largo %>% filter(pokemon == resaltar)
    p <- p +
      geom_line(data = destacado, color = "black", linewidth = 1.8) +
      geom_point(data = destacado, color = "black", size = 3)
  }
  
  p +
    escala_color_tipo(opc = opc) +
    scale_x_discrete(labels = etiqueta_variable) +
    labs(
      title = "Coordenadas paralelas",
      subtitle = "Cada línea es un Pokémon; variables reescaladas a 0–1",
      x = NULL, y = "Valor reescalado"
    ) +
    tema_base(opc)
}

# Radar para comparar pocos Pokémon (color = tipo, línea distinta por Pokémon)
grafico_radar <- function(datos, nombres, variables = stats_pca,
                          opc = opciones_defecto) {
  d <- datos %>%
    filter(pokemon %in% nombres) %>%
    distinct(pokemon, .keep_all = TRUE)
  
  largo <- d %>%
    select(pokemon, type_1, all_of(variables)) %>%
    pivot_longer(all_of(variables), names_to = "variable", values_to = "valor") %>%
    mutate(variable = factor(variable, levels = variables))
  
  cols <- setNames(unname(colores_tipo[as.character(d$type_1)]), d$pokemon)
  
  ggplot(largo, aes(
    x = variable, y = valor, group = pokemon,
    color = pokemon, fill = pokemon, linetype = pokemon
  )) +
    geom_polygon(alpha = 0.15, linewidth = 1) +
    geom_point(size = 2) +
    coord_polar() +
    scale_color_manual(values = cols, name = NULL) +
    scale_fill_manual(values = cols, name = NULL) +
    scale_linetype_discrete(name = NULL) +
    scale_x_discrete(labels = etiqueta_variable) +
    scale_y_continuous(limits = c(0, max_stat)) +
    labs(title = "Radar de estadísticas de combate", x = NULL, y = NULL) +
    # Texto plano: aquí no hay iconos de tipo (la leyenda son nombres de Pokémon)
    tema_base(opc, markdown = FALSE)
}

# Mapa de outliers multivariantes (Mahalanobis) sobre el plano PC1-PC2
grafico_mahalanobis <- function(datos, alfa = 0.001, opc = opciones_defecto) {
  corte <- qchisq(1 - alfa, df = length(stats_pca))
  d <- datos %>% mutate(atipico = mahalanobis > corte)
  
  ggplot(d, aes(x = PC1, y = PC2)) +
    geom_point(data = filter(d, !atipico), aes(color = type_1), size = 2, alpha = 0.45) +
    geom_point(data = filter(d, atipico), aes(color = type_1),
               size = tam_sprite(11, opc), alpha = 0.35) +
    geom_sprite(carpeta = opc$carpeta, data = filter(d, atipico), aes(sprite_id = id),
               size = tam_sprite(0.06, opc)) +
    escala_color_tipo(opc = opc) +
    labs(
      title = "Outliers multivariantes (distancia de Mahalanobis)",
      subtitle = paste0(
        "Con sprite: distancia² > χ² (gl = ", length(stats_pca),
        ", p = ", alfa, ") = ", round(corte, 1)
      ),
      x = "Componente principal 1", y = "Componente principal 2"
    ) +
    tema_pokemon(opc)
}

grafico_boxplot_individual <- function(datos, variable, opc = opciones_defecto) {
  limites <- datos %>%
    summarise(
      q1 = quantile(.data[[variable]], 0.25, na.rm = TRUE),
      q3 = quantile(.data[[variable]], 0.75, na.rm = TRUE),
      iqr = IQR(.data[[variable]], na.rm = TRUE),
      inferior = q1 - 1.5 * iqr,
      superior = q3 + 1.5 * iqr
    )
  
  atipicos <- datos %>%
    filter(
      .data[[variable]] < limites$inferior |
        .data[[variable]] > limites$superior
    )
  
  pos <- position_jitter(width = 0.12, height = 0, seed = 1)
  
  ggplot(datos, aes(x = "Pokémon", y = .data[[variable]])) +
    geom_boxplot(
      outlier.shape = NA,
      fill = "lightblue",
      alpha = 0.6,
      width = 0.4
    ) +
    geom_point(
      data = atipicos,
      aes(x = "Pokémon", y = .data[[variable]], color = type_1),
      inherit.aes = FALSE,
      size = tam_sprite(8, opc), alpha = 0.35,
      position = pos
    ) +
    geom_sprite(carpeta = opc$carpeta, 
      data = atipicos,
      aes(x = "Pokémon", y = .data[[variable]], sprite_id = id),
      inherit.aes = FALSE,
      size = tam_sprite(0.07, opc),
      position = pos
    ) +
    escala_color_tipo(opc = opc) +
    labs(
      title = paste("Boxplot de", etiqueta_variable(variable)),
      subtitle = "Los valores atípicos se representan mediante sprites",
      x = NULL,
      y = etiqueta_variable(variable)
    ) +
    tema_base(opc) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

# -------------------------------------------------------------------
# 2b. Gráficos "bajo demanda": receta de cada pestaña y barra de botones
# -------------------------------------------------------------------
# Cada pestaña tiene tres botones:
#   Generar -> crea el gráfico TEMPORAL (sustituye al anterior). Se mantiene al
#              cambiar de pestaña hasta que se pulse Borrar (o Generar de nuevo).
#   Guardar -> escribe el gráfico temporal en outputs/ como .png y .svg.
#   Borrar  -> elimina el gráfico temporal (no toca los archivos ya guardados).
# El gráfico temporal NO es la imagen ni el objeto ggplot, solo los valores de
# los filtros en el momento de pulsar «Generar» (unos pocos bytes). Se vuelve a
# dibujar a partir de esa receta, y los sprites salen de la caché.

# Carpeta y formato de los archivos que crea «Guardar»
CARPETA_SALIDA    <- "outputs"
GUARDADO_ANCHO    <- 12    # pulgadas
GUARDADO_ALTO     <- 8     # pulgadas
GUARDADO_ALTO_ID  <- c(heatmap = 10)   # alto propio de algunas pestañas
GUARDADO_RES      <- 150   # ppp del .png

entradas_graficos <- list(
  box_tipo       = c("tipos_tipo", "vars_tipo", "gens_ataque"),
  violin         = c("tipos_violin", "vars_violin", "tam_violin"),
  raincloud      = c("tipos_rain", "var_rain"),
  ecdf           = c("tipos_ecdf", "var_ecdf", "ref_ecdf"),
  box_individual = c("variable_individual", "gens_individual"),
  expl           = c("x_expl", "y_expl", "tam_expl", "facet_expl", "tipos_expl",
                     "gens_expl", "linea_expl", "sprites_expl"),
  correlacion    = c("vars_cor", "ord_cor"),
  paralelas      = c("tipos_par", "vars_par", "res_par"),
  grafico_pca    = c("gens_pca", "eje_x", "eje_y", "tam_pca", "sprites_pca", "biplot_pca"),
  pca_tipo       = c("tipo_pca", "gens_tipo", "tam_tipo"),
  umap           = c("gens_umap", "tam_umap"),
  mahal          = c("alfa_mah", "gens_mah"),
  heatmap        = c("vars_heat", "tipos_heat", "max_heat"),
  radar          = c("pokes_radar")
)

filtrar_gen <- function(gens) {
  pokemon_plot %>% filter(generation_id %in% as.integer(gens))
}

# Datos de las pestañas que además alimentan tablas / títulos
datos_expl_p <- function(p) {
  filtrar_gen(p$gens_expl) %>% filter(as.character(type_1) %in% p$tipos_expl)
}
datos_pca_tipo_p <- function(p) {
  filtrar_gen(p$gens_tipo) %>%
    filter(as.character(type_1) == p$tipo_pca |
             (!is.na(type_2) & type_2 == p$tipo_pca))
}
datos_mah_p <- function(p) filtrar_gen(p$gens_mah)

# p = lista con los valores de los filtros guardados; opc = opciones globales
dibujos <- list(
  box_tipo = function(p, opc) {
    d <- filtrar_gen(p$gens_ataque) %>% filter(as.character(type_1) %in% p$tipos_tipo)
    validate(need(nrow(d) > 0 && length(p$vars_tipo) > 0,
                  "No hay datos para los filtros seleccionados."))
    grafico_boxplot_tipo(d, p$tipos_tipo, p$vars_tipo, opc)
  },
  violin = function(p, opc) {
    d <- pokemon_plot %>% filter(as.character(type_1) %in% p$tipos_violin)
    validate(need(nrow(d) > 0 && length(p$vars_violin) > 0,
                  "Selecciona al menos un tipo con observaciones y un parámetro."))
    grafico_violin(d, p$tipos_violin, p$vars_violin, p$tam_violin, opc)
  },
  raincloud = function(p, opc) {
    validate(need(length(p$tipos_rain) > 0, "Selecciona al menos un tipo."))
    grafico_raincloud(pokemon_plot, p$tipos_rain, p$var_rain, opc)
  },
  ecdf = function(p, opc) {
    validate(need(length(p$tipos_ecdf) > 0, "Selecciona al menos un tipo."))
    grafico_ecdf(pokemon_plot, p$tipos_ecdf, p$var_ecdf, isTRUE(p$ref_ecdf), opc)
  },
  box_individual = function(p, opc) {
    d <- filtrar_gen(p$gens_individual)
    validate(need(nrow(d) > 0, "No hay datos para las generaciones seleccionadas."))
    grafico_boxplot_individual(d, p$variable_individual, opc)
  },
  expl = function(p, opc) {
    d <- datos_expl_p(p)
    validate(need(nrow(d) > 0, "No hay datos para los filtros seleccionados."))
    grafico_explorador(d, p$x_expl, p$y_expl, p$tam_expl,
                       usar_sprites = isTRUE(p$sprites_expl),
                       linea = isTRUE(p$linea_expl),
                       facetas = p$facet_expl, opc = opc)
  },
  correlacion = function(p, opc) {
    vs <- p$vars_cor[vapply(pokemon_plot[p$vars_cor],
                            function(v) sd(v, na.rm = TRUE) > 0, logical(1))]
    validate(need(length(vs) >= 2, "Selecciona al menos dos variables con variación."))
    grafico_correlacion(pokemon_plot, vs, isTRUE(p$ord_cor), opc)
  },
  paralelas = function(p, opc) {
    validate(need(length(p$tipos_par) > 0 && length(p$vars_par) >= 2,
                  "Selecciona al menos un tipo y dos variables."))
    grafico_paralelas(pokemon_plot, p$tipos_par, p$vars_par, p$res_par, opc)
  },
  grafico_pca = function(p, opc) {
    d <- filtrar_gen(p$gens_pca)
    validate(need(nrow(d) > 0, "No hay datos para las generaciones seleccionadas."))
    grafico_pca(d, x = p$eje_x, y = p$eje_y, tam = p$tam_pca,
                mostrar_sprites = isTRUE(p$sprites_pca),
                vectores = isTRUE(p$biplot_pca), opc = opc)
  },
  pca_tipo = function(p, opc) {
    d <- datos_pca_tipo_p(p)
    validate(need(nrow(d) > 0, "No hay Pokémon para los filtros seleccionados."))
    grafico_pca(d, tam = p$tam_tipo, mostrar_sprites = TRUE, opc = opc) +
      labs(title = paste("PCA de Pokémon de tipo", p$tipo_pca))
  },
  umap = function(p, opc) {
    validate(need(hay_umap, "Instala el paquete uwot: install.packages(\"uwot\")"))
    d <- filtrar_gen(p$gens_umap)
    validate(need(nrow(d) > 0, "No hay datos para las generaciones seleccionadas."))
    grafico_pca(d, x = "UMAP1", y = "UMAP2", tam = p$tam_umap,
                mostrar_sprites = TRUE,
                titulo = "UMAP de las estadísticas de combate", opc = opc)
  },
  mahal = function(p, opc) {
    d <- datos_mah_p(p)
    validate(need(nrow(d) > 0, "No hay datos para las generaciones seleccionadas."))
    grafico_mahalanobis(d, as.numeric(p$alfa_mah), opc)
  },
  heatmap = function(p, opc) {
    d <- pokemon_plot %>% filter(as.character(type_1) %in% p$tipos_heat)
    validate(need(nrow(d) >= 2 && length(p$vars_heat) >= 2,
                  "Selecciona al menos 2 Pokémon (tipos) y 2 variables."))
    dibujar_heatmap(d, p$vars_heat, p$max_heat, opc)   # gráfico de R base
  },
  radar = function(p, opc) {
    validate(need(length(p$pokes_radar) > 0, "Selecciona al menos un Pokémon."))
    grafico_radar(pokemon_plot, p$pokes_radar, opc = opc)
  }
)

# Abre el dispositivo gráfico de un formato (png con ragg, svg con svglite)
abrir_dispositivo <- function(ext, archivo, ancho, alto) {
  if (ext == "png") {
    if (requireNamespace("ragg", quietly = TRUE)) {
      ragg::agg_png(archivo, width = ancho, height = alto, units = "in", res = GUARDADO_RES)
    } else {
      png(archivo, width = ancho, height = alto, units = "in", res = GUARDADO_RES, type = "cairo")
    }
  } else {
    if (requireNamespace("svglite", quietly = TRUE)) {
      svglite::svglite(archivo, width = ancho, height = alto)
    } else {
      svg(archivo, width = ancho, height = alto)
    }
  }
}

# Dibuja el gráfico temporal `id` (receta `p`) en outputs/<id>_<fecha>.png y .svg.
# Devuelve list(ok = TRUE/FALSE, archivos = rutas, error = mensaje).
guardar_grafico <- function(id, p, opc, dibujar) {
  dir.create(CARPETA_SALIDA, showWarnings = FALSE, recursive = TRUE)
  base <- sprintf("%s_%s", id, format(Sys.time(), "%Y%m%d_%H%M%S"))
  archivos <- file.path(CARPETA_SALIDA, paste0(base, c(".png", ".svg")))
  ancho <- GUARDADO_ANCHO
  alto  <- if (id %in% names(GUARDADO_ALTO_ID)) GUARDADO_ALTO_ID[[id]] else GUARDADO_ALTO
  
  tryCatch({
    for (f in archivos) {
      abrir_dispositivo(tools::file_ext(f), f, ancho, alto)
      tryCatch({
        r <- dibujar(p, opc)                 # el heatmap (R base) dibuja directamente
        if (inherits(r, "ggplot")) print(r)
      }, finally = dev.off())
    }
    list(ok = TRUE, archivos = archivos, error = NULL)
  }, error = function(e) {
    unlink(archivos)                         # no dejar archivos a medias
    list(ok = FALSE, archivos = character(0), error = conditionMessage(e))
  })
}

css_barra <- "
.barra-grafico { display: flex; gap: 6px; margin-bottom: 8px; }
.barra-grafico .btn { flex: 1; padding-left: 4px; padding-right: 4px; }
.estado-grafico { font-size: 0.85em; margin-bottom: 4px; }
"

# Barra superior de cada barra lateral: Generar · Guardar · Borrar + estado
barra_grafico <- function(id) {
  tagList(
    div(
      class = "barra-grafico",
      actionButton(paste0(id, "_gen"),  "Generar", icon = icon("play"),  class = "btn-primary btn-sm",
                   title = "Genera el gráfico temporal (se mantiene hasta que lo borres)"),
      actionButton(paste0(id, "_save"), "Guardar", icon = icon("save"),  class = "btn-success btn-sm",
                   title = "Guarda el gráfico como .png y .svg en la carpeta outputs/"),
      actionButton(paste0(id, "_del"),  "Borrar",  icon = icon("trash"), class = "btn-danger btn-sm",
                   title = "Borra el gráfico temporal (no borra los archivos guardados)")
    ),
    div(class = "estado-grafico", uiOutput(paste0(id, "_estado"))),
    tags$hr()
  )
}

# -------------------------------------------------------------------
# 3. Interfaz
# -------------------------------------------------------------------

ui <- navbarPage(
  title = "Pokémon · Análisis interactivo",
  id = "nav",
  # CSS dinámico (solo si se activa "Aplicar también a la interfaz")
  header = tagList(tags$style(HTML(css_barra)), uiOutput("estilo_fuente")),
  
  tabPanel(
    "Exploración",
    sidebarLayout(
      sidebarPanel(
        h4("Resumen de los datos"),
        helpText("Datos de TidyTuesday · 1 de abril de 2025."),
        selectInput(
          "explora_generacion", "Generación",
          choices = c("Todas", as.character(generaciones)),
          selected = "Todas"
        )
      ),
      mainPanel(
        fluidRow(
          column(4, wellPanel(h3(textOutput("n_pokemon")), "Pokémon")),
          column(4, wellPanel(h3(textOutput("n_tipos")), "Tipos principales")),
          column(4, wellPanel(h3(textOutput("n_generaciones")), "Generaciones"))
        ),
        h4("Primeras observaciones"),
        DTOutput("tabla_datos"),
        h4("Estadísticas descriptivas"),
        DTOutput("tabla_resumen"),
        h4("Valores perdidos en el conjunto original"),
        DTOutput("tabla_na")
      )
    )
  ),
  
  navbarMenu(
    "Distribuciones",
    tabPanel(
      "Ataque por tipo", value = "box_tipo",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("box_tipo"),
          selectizeInput(
            "tipos_tipo", "Tipos incluidos",
            choices = tipos,
            selected = tipos,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          selectizeInput(
            "vars_tipo", "Parámetros",
            choices = stats_pca,
            selected = "attack",
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          helpText(
            "Con un solo tipo puedes comparar varios parámetros;",
            "con varios tipos solo se representa un parámetro."
          ),
          checkboxGroupInput(
            "gens_ataque", "Generaciones",
            choices = generaciones,
            selected = generaciones
          )
        ),
        mainPanel(plotOutput("box_tipo", height = "650px"))
      )
    ),
    
    tabPanel(
      "Violin plot", value = "violin",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("violin"),
          selectizeInput(
            "tipos_violin", "Tipos incluidos",
            choices = tipos,
            selected = tipos,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          selectizeInput(
            "vars_violin", "Parámetros",
            choices = stats_pca,
            selected = "attack",
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          helpText(
            "Con un solo tipo puedes comparar varios parámetros;",
            "con varios tipos solo se representa un parámetro."
          ),
          sliderInput(
            "tam_violin", "Tamaño de los sprites",
            min = 0.015, max = 0.12, value = 0.055, step = 0.005
          )
        ),
        mainPanel(plotOutput("violin", height = "650px"))
      )
    ),
    
    tabPanel(
      "Raincloud", value = "raincloud",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("raincloud"),
          selectizeInput(
            "tipos_rain", "Tipos incluidos", choices = tipos, selected = head(tipos, 6), multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          selectInput("var_rain", "Variable", choices = vars_numericas, selected = "attack"),
          helpText("Medio violín (densidad) + boxplot + observaciones. Sprites en los outliers.")
        ),
        mainPanel(plotOutput("raincloud", height = "650px"))
      )
    ),
    
    tabPanel(
      "ECDF", value = "ecdf",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("ecdf"),
          selectizeInput(
            "tipos_ecdf", "Tipos incluidos", choices = tipos, selected = head(tipos, 6), multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          selectInput("var_ecdf", "Variable", choices = vars_numericas, selected = "speed"),
          checkboxInput("ref_ecdf", "Mostrar curva de referencia (todos)", TRUE),
          helpText("Distribución acumulada: no depende de bins ni de la densidad estimada.")
        ),
        mainPanel(plotOutput("ecdf", height = "600px"))
      )
    ),
    
    tabPanel(
      "Boxplots individuales", value = "box_individual",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("box_individual"),
          selectInput(
            "variable_individual", "Variable numérica",
            choices = vars_numericas,
            selected = "attack"
          ),
          checkboxGroupInput(
            "gens_individual", "Generaciones",
            choices = generaciones,
            selected = generaciones
          )
        ),
        mainPanel(plotOutput("box_individual", height = "600px"))
      )
    )
  ),
  
  navbarMenu(
    "Relaciones",
    tabPanel(
      "Explorador", value = "expl",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("expl"),
          selectInput("x_expl", "Eje X", choices = vars_explorador, selected = "attack"),
          selectInput("y_expl", "Eje Y", choices = vars_explorador, selected = "speed"),
          selectInput("tam_expl", "Tamaño según", choices = c("ninguno", vars_explorador), selected = "ninguno"),
          selectInput("facet_expl", "Facetas por", choices = c("ninguna", "tipo", "generación")),
          selectizeInput(
            "tipos_expl", "Tipos incluidos", choices = tipos, selected = tipos, multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          checkboxGroupInput("gens_expl", "Generaciones", choices = generaciones, selected = generaciones),
          checkboxInput("linea_expl", "Recta de regresión", FALSE),
          checkboxInput("sprites_expl", "Mostrar sprites", FALSE),
          helpText("Arrastra sobre el gráfico para seleccionar puntos y ver sus filas.")
        ),
        mainPanel(
          plotOutput("expl", height = "600px", brush = brushOpts(id = "brush_expl")),
          h4("Pokémon seleccionados"),
          DTOutput("tabla_expl")
        )
      )
    ),
    
    tabPanel(
      "Correlación", value = "correlacion",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("correlacion"),
          selectizeInput(
            "vars_cor", "Variables", choices = vars_numericas, selected = vars_numericas, multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          checkboxInput("ord_cor", "Ordenar por clustering jerárquico", TRUE)
        ),
        mainPanel(plotOutput("correlacion", height = "650px"))
      )
    ),
    
    tabPanel(
      "Coord. paralelas", value = "paralelas",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("paralelas"),
          selectizeInput(
            "tipos_par", "Tipos incluidos", choices = tipos, selected = tipos, multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          selectizeInput(
            "vars_par", "Variables", choices = vars_numericas, selected = stats_pca, multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          selectInput("res_par", "Resaltar un Pokémon", choices = c("(ninguno)", nombres_pokemon))
        ),
        mainPanel(plotOutput("paralelas", height = "650px"))
      )
    )
  ),
  
  navbarMenu(
    "Reducción dimensional",
    tabPanel(
      "PCA", value = "grafico_pca",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("grafico_pca"),
          checkboxGroupInput(
            "gens_pca", "Generaciones incluidas",
            choices = generaciones,
            selected = generaciones
          ),
          selectInput(
            "eje_x", "Componente eje X",
            choices = c("PC1", "PC2", "PC3"),
            selected = "PC1"
          ),
          selectInput(
            "eje_y", "Componente eje Y",
            choices = c("PC1", "PC2", "PC3"),
            selected = "PC2"
          ),
          sliderInput(
            "tam_pca", "Tamaño de los sprites",
            min = 0.015, max = 0.15, value = 0.055, step = 0.005
          ),
          checkboxInput("sprites_pca", "Mostrar sprites", TRUE),
          checkboxInput("biplot_pca", "Mostrar vectores de las variables (biplot)", FALSE)
        ),
        mainPanel(
          plotOutput("grafico_pca", height = "650px"),
          h4("Varianza explicada"),
          tableOutput("varianza_pca")
        )
      )
    ),
    
    tabPanel(
      "PCA por tipo", value = "pca_tipo",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("pca_tipo"),
          selectInput("tipo_pca", "Tipo Pokémon", choices = tipos, selected = "grass"),
          checkboxGroupInput(
            "gens_tipo", "Generaciones",
            choices = generaciones,
            selected = generaciones
          ),
          sliderInput(
            "tam_tipo", "Tamaño de los sprites",
            min = 0.03, max = 0.25, value = 0.12, step = 0.01
          )
        ),
        mainPanel(
          h4(textOutput("titulo_tipo")),
          plotOutput("pca_tipo", height = "650px")
        )
      )
    ),
    
    tabPanel(
      "UMAP", value = "umap",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("umap"),
          checkboxGroupInput("gens_umap", "Generaciones", choices = generaciones, selected = generaciones),
          sliderInput("tam_umap", "Tamaño de los sprites", min = 0.015, max = 0.15, value = 0.05, step = 0.005),
          helpText("UMAP sobre las 6 estadísticas de combate (estandarizadas). Requiere el paquete uwot.")
        ),
        mainPanel(plotOutput("umap", height = "650px"))
      )
    ),
    
    tabPanel(
      "Outliers multivariantes", value = "mahal",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("mahal"),
          selectInput("alfa_mah", "Nivel de significación (p)", choices = c(0.05, 0.01, 0.001, 0.0001), selected = 0.001),
          checkboxGroupInput("gens_mah", "Generaciones", choices = generaciones, selected = generaciones),
          helpText("Distancia calculada sobre las 6 estadísticas con todos los Pokémon; el corte usa una χ², por lo que es orientativo.")
        ),
        mainPanel(
          plotOutput("mahal", height = "600px"),
          h4("Los 15 más atípicos"),
          DTOutput("tabla_mah")
        )
      )
    )
  ),
  
  navbarMenu(
    "Perfiles",
    tabPanel(
      "Heatmap", value = "heatmap",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("heatmap"),
          selectizeInput(
            "vars_heat", "Variables", choices = vars_numericas, selected = stats_pca, multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          selectizeInput(
            "tipos_heat", "Tipos incluidos", choices = tipos, selected = tipos, multiple = TRUE,
            options = list(plugins = list("remove_button"))
          ),
          sliderInput("max_heat", "Máx. de Pokémon (muestra aleatoria)", min = 30, max = 600, value = 250, step = 10),
          helpText("Filas y columnas agrupadas por similitud; la barra lateral indica el tipo.")
        ),
        mainPanel(plotOutput("heatmap", height = "750px"))
      )
    ),
    
    tabPanel(
      "Radar", value = "radar",
      sidebarLayout(
        sidebarPanel(
          barra_grafico("radar"),
          selectizeInput(
            "pokes_radar", "Pokémon (máx. 5)", choices = nombres_pokemon,
            selected = intersect(c("pikachu", "charizard", "mewtwo"), nombres_pokemon),
            multiple = TRUE,
            options = list(maxItems = 5, plugins = list("remove_button"))
          ),
          helpText("Color = tipo del Pokémon; el trazo cambia entre Pokémon.")
        ),
        mainPanel(plotOutput("radar", height = "600px"))
      )
    )
  ),
  
  # ---- Opciones globales (afectan a todos los gráficos) ----
  tabPanel(
    "Opciones",
    fluidRow(
      column(
        6,
        wellPanel(
          h4("Texto"),
          sliderInput(
            "opt_fuente", "Tamaño de fuente de los gráficos (pt)",
            min = 8, max = 24, value = opciones_defecto$fuente, step = 1
          ),
          checkboxInput("opt_fuente_ui", "Aplicar también a la interfaz", FALSE),
          helpText("Los iconos de tipo de ejes y leyendas crecen con la fuente.")
        )
      ),
      column(
        6,
        wellPanel(
          h4("Sprites"),
          radioButtons(
            "opt_carpeta", "Cargar sprites desde",
            choices = carpetas_sprites_ok,
            selected = carpeta_sprites_ini
          ),
          sliderInput(
            "opt_escala_sprites", "Tamaño de los sprites generados (×)",
            min = 0.5, max = 2.5, value = opciones_defecto$escala_sprites, step = 0.1
          ),
          helpText(
            "Multiplica el tamaño de todos los sprites, además de los",
            "controles propios de cada pestaña."
          ),
          uiOutput("aviso_sprites")
        )
      )
    ),
    actionButton("opt_reset", "Restablecer valores por defecto"),
    if (!dir.exists(carpeta_tipos)) {
      div(class = "alert alert-warning", style = "margin-top: 15px;",
          "No se ha encontrado la carpeta 'tipos': los tipos se muestran como texto.")
    }
  )
)

# -------------------------------------------------------------------
# 4. Servidor
# -------------------------------------------------------------------

server <- function(input, output, session) {
  
  # ---- Opciones globales ----
  # Lista que se pasa a todas las funciones de dibujo (argumento `opc`)
  opc <- reactive({
    list(
      fuente = valor_o(input$opt_fuente, opciones_defecto$fuente),
      escala_sprites = valor_o(input$opt_escala_sprites, opciones_defecto$escala_sprites),
      carpeta = valor_o(input$opt_carpeta, carpeta_sprites_ini)
    )
  })
  
  output$estilo_fuente <- renderUI({
    req(isTRUE(input$opt_fuente_ui), input$opt_fuente)
    px <- round(input$opt_fuente * 14 / 12, 1)  # 12 pt <-> 14 px (tamaño por defecto)
    tags$style(HTML(sprintf(
      paste0("body, label, .form-control, .selectize-input, .selectize-dropdown, ",
             ".help-block, .table, .dataTables_wrapper, .navbar-nav > li > a ",
             "{ font-size: %spx !important; }"),
      px
    )))
  })
  
  output$aviso_sprites <- renderUI({
    carpeta <- valor_o(input$opt_carpeta, carpeta_sprites_ini)
    n_faltan <- sum(!pokemon$id %in% ids_sprites[[carpeta]])
    if (n_faltan > 0) {
      div(
        class = "alert alert-warning",
        sprintf("Faltan %d sprites en 'data/%s' (esos Pokémon se dibujan sin sprite).",
                n_faltan, carpeta)
      )
    }
  })
  
  observeEvent(input$opt_reset, {
    updateSliderInput(session, "opt_fuente", value = opciones_defecto$fuente)
    updateSliderInput(session, "opt_escala_sprites", value = opciones_defecto$escala_sprites)
    updateRadioButtons(session, "opt_carpeta", selected = carpeta_sprites_ini)
    updateCheckboxInput(session, "opt_fuente_ui", value = FALSE)
  })
  
  # ---- Gráficos bajo demanda: Generar / Guardar / Borrar ----
  # estado[[id]]          = NULL (no hay gráfico) o list(p = filtros): gráfico TEMPORAL.
  #                         Se mantiene al cambiar de pestaña hasta Borrar / Generar.
  # ultimo_guardado[[id]] = nombre del último archivo escrito en outputs/ (solo informativo;
  #                         está separado para que guardar NO redibuje el gráfico).
  estado <- reactiveValues()
  ultimo_guardado <- reactiveValues()
  
  registrar_grafico <- function(id) {
    force(id)
    entradas <- entradas_graficos[[id]]
    dibujar  <- dibujos[[id]]
    
    # Generar: crea el gráfico temporal y sustituye al anterior
    observeEvent(input[[paste0(id, "_gen")]], {
      estado[[id]] <- list(p = lapply(setNames(entradas, entradas), function(i) input[[i]]))
    })
    
    # Guardar: escribe el gráfico temporal en outputs/ (.png y .svg)
    observeEvent(input[[paste0(id, "_save")]], {
      e <- estado[[id]]
      if (is.null(e)) {
        showNotification("No hay ningún gráfico que guardar: pulsa «Generar».", type = "warning")
      } else {
        res <- withProgress(
          message = "Guardando .png y .svg...", value = 0.5,
          guardar_grafico(id, e$p, opc(), dibujar)
        )
        if (isTRUE(res$ok)) {
          ultimo_guardado[[id]] <- sub("\\.png$", "", basename(res$archivos[1]))
          showNotification(
            paste0("Guardado en ", CARPETA_SALIDA, "/: ", paste(basename(res$archivos), collapse = " y ")),
            type = "message", duration = 6
          )
        } else {
          showNotification(paste("No se pudo guardar el gráfico:", res$error),
                           type = "error", duration = 10)
        }
      }
    })
    
    # Borrar: elimina el gráfico temporal (los archivos de outputs/ no se tocan)
    observeEvent(input[[paste0(id, "_del")]], {
      estado[[id]] <- NULL
      ultimo_guardado[[id]] <- NULL
    })
    
    output[[paste0(id, "_estado")]] <- renderUI({
      e <- estado[[id]]
      if (is.null(e)) return(span(class = "text-muted", "Sin gráfico"))
      g <- ultimo_guardado[[id]]
      tagList(
        span(class = "text-success", icon("check"),
             " Gráfico temporal: se mantiene hasta que pulses Borrar"),
        if (!is.null(g)) tagList(
          br(),
          span(class = "text-muted", icon("file-image"),
               paste0(" Último guardado: ", CARPETA_SALIDA, "/", g, " (.png / .svg)"))
        )
      )
    })
    
    output[[id]] <- renderPlot({
      e <- estado[[id]]
      validate(need(!is.null(e), "Ajusta los filtros y pulsa «Generar»."))
      r <- dibujar(e$p, opc())
      if (inherits(r, "ggplot")) r else invisible(NULL)
    })
  }
  
  for (id in names(dibujos)) registrar_grafico(id)
  
  # ---- Regla: varios parámetros solo si hay un único tipo seleccionado ----
  # Si se rompe, se recorta la selección que el usuario NO acaba de tocar
  # (si añade un parámetro, se deja un solo tipo; si añade un tipo, un solo parámetro).
  limitar_seleccion <- function(id_tipos, id_vars) {
    observeEvent(input[[id_tipos]], {
      if (length(input[[id_tipos]]) > 1 && length(input[[id_vars]]) > 1) {
        updateSelectizeInput(session, id_vars, selected = input[[id_vars]][1])
        showNotification(
          "Con más de un tipo solo se puede representar un parámetro.",
          type = "warning"
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observeEvent(input[[id_vars]], {
      if (length(input[[id_vars]]) > 1 && length(input[[id_tipos]]) > 1) {
        updateSelectizeInput(session, id_tipos, selected = input[[id_tipos]][1])
        showNotification(
          "Con varios parámetros solo se puede representar un tipo; elige el que quieras.",
          type = "warning"
        )
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
  }
  
  limitar_seleccion("tipos_tipo", "vars_tipo")
  limitar_seleccion("tipos_violin", "vars_violin")
  
  # ---- Exploración (tablas; no usan Generar/Guardar/Borrar) ----
  datos_exploracion <- reactive({
    d <- pokemon
    if (!is.null(input$explora_generacion) &&
        input$explora_generacion != "Todas") {
      d <- d %>% filter(generation_id == as.integer(input$explora_generacion))
    }
    d
  })
  
  output$n_pokemon <- renderText(nrow(datos_exploracion()))
  output$n_tipos <- renderText(n_distinct(datos_exploracion()$type_1))
  output$n_generaciones <- renderText(n_distinct(datos_exploracion()$generation_id))
  
  output$tabla_datos <- renderDT({
    datatable(
      head(datos_exploracion(), 10),
      options = list(scrollX = TRUE, pageLength = 5)
    )
  })
  
  output$tabla_resumen <- renderDT({
    d <- datos_exploracion() %>% select(any_of(vars_numericas))
    resumen <- tibble(
      variable = names(d),
      media = map_dbl(d, ~ mean(.x, na.rm = TRUE)),
      desviacion = map_dbl(d, ~ sd(.x, na.rm = TRUE)),
      minimo = map_dbl(d, ~ min(.x, na.rm = TRUE)),
      mediana = map_dbl(d, ~ median(.x, na.rm = TRUE)),
      maximo = map_dbl(d, ~ max(.x, na.rm = TRUE))
    )
    datatable(resumen, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  output$tabla_na <- renderDT({
    na <- tibble(
      variable = names(pokemon_raw),
      valores_perdidos = map_int(pokemon_raw, ~ sum(is.na(.x)))
    ) %>% filter(valores_perdidos > 0)
    datatable(na, options = list(pageLength = 8))
  })
  
  # ---- Tablas / títulos ligados a un gráfico generado ----
  output$varianza_pca <- renderTable({
    v <- summary(pca)$importance
    tibble(
      Componente = colnames(v),
      `Varianza explicada (%)` = round(100 * v[2, ], 2),
      `Varianza acumulada (%)` = round(100 * v[3, ], 2)
    )
  })
  
  output$titulo_tipo <- renderText({
    e <- estado$pca_tipo
    req(e)
    paste0("PCA: Pokémon de tipo ", e$p$tipo_pca,
           " (", nrow(datos_pca_tipo_p(e$p)), " Pokémon)")
  })
  
  output$tabla_expl <- renderDT({
    e <- estado$expl
    req(e)
    sel <- brushedPoints(
      datos_expl_p(e$p), input$brush_expl,
      xvar = e$p$x_expl, yvar = e$p$y_expl
    )
    datatable(
      sel %>% select(any_of(c("id", "pokemon", "type_1", "type_2", "generation_id", stats_pca))),
      options = list(scrollX = TRUE, pageLength = 8)
    )
  })
  
  output$tabla_mah <- renderDT({
    e <- estado$mahal
    req(e)
    top <- datos_mah_p(e$p) %>%
      arrange(desc(mahalanobis)) %>%
      slice_head(n = 15) %>%
      mutate(mahalanobis = round(mahalanobis, 1)) %>%
      select(any_of(c("pokemon", "type_1", "generation_id", stats_pca, "mahalanobis")))
    datatable(top, options = list(scrollX = TRUE, pageLength = 8))
  })
  
}

shinyApp(ui = ui, server = server)
