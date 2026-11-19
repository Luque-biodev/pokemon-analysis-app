# Pokémon · Análisis interactivo (Shiny)

Aplicación Shiny para explorar el dataset de Pokémon de https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-04-01:
distribuciones, relaciones, PCA/UMAP, outliers multivariantes, heatmap y radar.

Con sprites dibujados sobre los gráficos. https://github.com/PokeAPI

> **Versión v8.** Cambios respecto a v7: los tres botones tienen ahora un
> significado distinto. **Generar** crea un gráfico *temporal* que se mantiene
> al cambiar de pestaña hasta que lo borres, **Guardar** escribe el gráfico en
> la carpeta `outputs/` como `.png` y `.svg`, y **Borrar** elimina solo el
> temporal.
>
> Desde v7: todo se lee desde `data/`, los sprites se cargan con una caché
> propia (sin `ggimage`/`magick`) y cada gráfico se genera bajo demanda.

---

## Estructura

```
Mi_Pokedex/
├── app.R            ← app_v8.R renombrado
├── download.R       ← descarga los datos y las imágenes (se ejecuta una vez)
├── outputs/         ← lo crea la app al pulsar «Guardar» (.png y .svg)
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
                      "ggtext", "png", "uwot", "ragg", "svglite"))
   ```

   `ggimage` y `magick` **ya no hacen falta**; se añade `png`. `uwot` solo
   es necesario para la pestaña UMAP, `ragg` para que los emojis se vean bien
   y para guardar los `.png`, y `svglite` para guardar los `.svg` (si no está,
   la app usa `grDevices::svg`, que necesita cairo).

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
| **Generar** | Dibuja el gráfico con los filtros actuales como **gráfico temporal**. Sustituye siempre al temporal anterior. |
| **Guardar** | Escribe el gráfico temporal en la carpeta **`outputs/`** como **`.png` y `.svg`**. |
| **Borrar** | Elimina el gráfico **temporal** y libera lo que ocupaba. No borra los archivos de `outputs/`. |

Reglas:

- Cambiar un filtro **no redibuja nada**: solo se redibuja al pulsar *Generar*.
  Es el mayor ahorro de CPU, porque antes cada cambio de un filtro repetía todo
  el dibujo con sprites.
- El gráfico temporal **se mantiene al cambiar de pestaña** y al volver sigue
  ahí; solo desaparece si pulsas *Borrar* o lo sustituyes con otro *Generar*
  (o si se cierra la sesión).
- *Guardar* necesita un gráfico generado; si no hay, avisa. Guardar **no
  modifica** el gráfico temporal: sigue visible y puedes seguir guardando
  variantes.
- Los archivos se llaman `<pestaña>_<AAAAMMDD_HHMMSS>.png` / `.svg`
  (por ejemplo `grafico_pca_20261118_153012.png`), así que nunca se
  sobrescriben. El indicador bajo los botones muestra el estado
  (*Sin gráfico / Gráfico temporal*) y el nombre del último archivo guardado.
- Tamaño de los archivos: 12 × 8 pulgadas (10 en el heatmap) y 150 ppp en el
  `.png`. Se cambia con `GUARDADO_ANCHO`, `GUARDADO_ALTO`, `GUARDADO_ALTO_ID`
  y `GUARDADO_RES` al principio de la sección 2b de `app.R`. La carpeta de
  salida es `CARPETA_SALIDA` (`"outputs"`).
- Lo que se mantiene en memoria **no es la imagen ni el objeto ggplot**, sino
  solo los valores de los filtros (unos pocos bytes). El gráfico se vuelve a
  dibujar desde esa receta; como los sprites salen de la caché, es rápido.
  Al guardar se dibuja de nuevo desde la receta, con las opciones globales
  del momento.
- Las **Opciones globales** (fuente, escala de sprites, carpeta de sprites) se
  aplican en vivo al gráfico temporal y a los archivos que se guarden después.
- El estado temporal es **por sesión**: cada usuario tiene sus propios gráficos.
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

