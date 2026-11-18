# Pokémon · Análisis interactivo (Shiny)

Aplicación Shiny para explorar el dataset de Pokémon de
[TidyTuesday 2025-04-01](https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-04-01):
distribuciones, relaciones, PCA/UMAP, outliers multivariantes, heatmap y radar,
con sprites dibujados sobre los gráficos.

> **Versión v7.** Cambios respecto a `app_v6.R`: todo se lee desde `data/`,
> los sprites se cargan con una caché propia (sin `ggimage`/`magick`) y cada
> gráfico se genera bajo demanda con los botones **Generar / Guardar / Borrar**.

---

## Estructura

```
Mi_Pokedex/
├── app.R            ← app_v7.R renombrado
├── download.R       ← descarga los datos y las imágenes (se ejecuta una vez)
└── data/
    ├── pokemon.csv          (separador ";" y decimal ","; lo crea download.R)
    ├── sprites/<id>.png     sprites normales
    ├── sprites_mini/<id>.png sprites reducidos
    └── tipos/<tipo>.png     iconos de tipo (fire.png, water.png, ...)
```

La app **no accede a internet** al arrancar: si falta `data/pokemon.csv` se
detiene con un mensaje que indica ejecutar `download.R`.

## Puesta en marcha

1. Paquetes (una sola vez, en la consola, nunca dentro de `app.R`):

   ```r
   install.packages(c("shiny", "tidyverse", "ggbeeswarm", "DT", "scales",
                      "ggtext", "png", "uwot", "ragg"))
   ```

   `ggimage` y `magick` **ya no hacen falta**; se añade `png`. `uwot` solo
   es necesario para la pestaña UMAP y `ragg` para que los emojis se vean bien.

2. Descargar los datos (crea `data/`):

   ```r
   source("download.R")
   ```

   Los iconos *generation-v* de `sprites_mini` solo existen hasta el id 649;
   para el resto la app dibuja el punto de color sin sprite y lo avisa en la
   pestaña **Opciones**.

3. Lanzar la app:

   ```r
   shiny::runApp()
   ```

## Los tres botones: Generar · Guardar · Borrar

Cada pestaña con gráfico los tiene arriba del todo de la barra lateral:

| Botón | Qué hace |
|-------|----------|
| **Generar** | Dibuja el gráfico con los filtros actuales. **Sustituye siempre al anterior** (esté guardado o no). |
| **Guardar** | Marca el gráfico actual como guardado: **se conserva al moverte a otra pestaña** y al volver. |
| **Borrar** | Elimina el gráfico (guardado o no) y libera lo que ocupaba. |

Reglas:

- Cambiar un filtro **no redibuja nada**: solo se redibuja al pulsar *Generar*.
  Es el mayor ahorro de CPU, porque antes cada cambio de un filtro repetía todo
  el dibujo con sprites.
- Un gráfico **sin guardar se descarta al salir de la pestaña**; uno guardado
  se mantiene. Un indicador bajo los botones muestra el estado
  (*Sin gráfico / Sin guardar / Guardado*).
- Lo que se guarda **no es la imagen ni el objeto ggplot**, sino solo los valores
  de los filtros (unos pocos bytes). El gráfico se vuelve a dibujar desde esa
  receta; como los sprites salen de la caché, es rápido.
- Las **Opciones globales** (fuente, escala de sprites, carpeta de sprites) se
  aplican en vivo a todos los gráficos, también a los guardados.
- El estado es **por sesión**: cada usuario tiene sus propios gráficos guardados.
- La pestaña **Exploración** (tablas) no usa los botones.

## Cómo se ahorra memoria y cómputo con los sprites

`ggimage::geom_image()` abre y decodifica cada PNG con `magick` **en cada
dibujo**, y las rutas viajaban dentro de los datos. En v7:

1. **Geom propio `geom_sprite()`**: solo necesita la columna `id`
   (`aes(sprite_id = id)`); ya no hay una columna de rutas en los datos.
2. **Decodificación única y perezosa**: cada sprite se lee una sola vez, solo
   cuando llega a dibujarse, y se guarda como `nativeRaster`
   (4 bytes/píxel: ≈ 37 KB un sprite de 96×96, ≈ 4 KB uno de 32×32).
3. **Caché compartida con tope** (`CACHE_SPRITES_MAX = 800`): al llenarse se
   expulsa la mitad al azar, así la memoria nunca crece sin límite
   (peor caso ≈ 30 MB con sprites normales).
4. **Listado del disco una sola vez** al arrancar (`ids_sprites`), en lugar de
   un `file.exists()` por sprite; los ausentes también se cachean.
5. **Dibujo bajo demanda** (botón *Generar*), que evita redibujos innecesarios.
6. **Carpeta `sprites_mini`** (pestaña *Opciones*) como alternativa aún más ligera.

Ajustes disponibles al principio de `app.R`:

| Constante | Efecto |
|-----------|--------|
| `CACHE_SPRITES_MAX` | Máximo de sprites en memoria. Bájalo si el servidor tiene poca RAM. |
| `FACTOR_TAM` | Calibra el tamaño de los sprites respecto a v6 (que dependía del ancho del panel; ahora depende del lado menor). |

## Publicar en shinyapps.io

```r
rsconnect::deployApp(appDir = ".", appName = "mi-pokedex")
```

La carpeta `data/` se sube con la app. Comprueba que `png` figura entre las
dependencias detectadas (o usa `renv::snapshot()`). Cuanto más pequeña sea
`data/`, más rápido el despliegue: puedes borrar `data/sprites/` si solo usas
`sprites_mini`.

## Estado de la verificación

Los scripts se revisaron de forma estática (delimitadores equilibrados, todos
los `input` y `output` de la interfaz coinciden con los de la lógica), pero
**no se han podido ejecutar en R**. Primera comprobación recomendada:

- Que los sprites tengan un tamaño parecido al de v6; si no, ajusta `FACTOR_TAM`.
- Que el icono de *Guardar* (`icon("save")`) se muestre en tu versión de Shiny.
- Que la pestaña **Explorador** con *brush* rellene la tabla tras *Generar*.
