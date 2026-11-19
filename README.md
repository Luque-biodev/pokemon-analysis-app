# Pokémon · Análisis interactivo (Shiny)

Aplicación Shiny para explorar el dataset de Pokémon de
[TidyTuesday 2025-04-01](https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-04-01):
distribuciones, relaciones, PCA/UMAP, outliers multivariantes, heatmap y radar,
con sprites dibujados sobre los gráficos.

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

## Análisis estadístico y gráficos: qué hace cada pestaña

Convenciones comunes a toda la app:

- **Estadísticas de combate** (`stats_pca`): HP, Attack, Defense, Sp. Atk,
  Sp. Def y Speed. Son la base del PCA, el UMAP y la distancia de Mahalanobis.
- **Variables numéricas**: todas las columnas numéricas del CSV salvo los
  identificadores (`id`, `species_id`, `generation_id`). Las que aparecen en
  los selectores de variable.
- **Criterio de valor atípico (outlier)**: regla de Tukey, es decir, fuera de
  `[Q1 − 1,5·IQR, Q3 + 1,5·IQR]`. Se calcula por grupo (por tipo, o por
  parámetro) y los atípicos se dibujan con su sprite sobre un punto del color
  del tipo.
- **Colores e iconos**: el color es siempre el **tipo principal** (`type_1`);
  los ejes y leyendas usan los iconos de `data/tipos/`.
- **Filtros**: se aplican al pulsar *Generar* (ver la sección de botones). El
  filtro «Generaciones» solo reduce las filas que se dibujan; no recalcula
  nada (ver el aviso del PCA y la distancia de Mahalanobis).

### Exploración (tablas)

Resumen de los datos, con filtro por generación: número de Pokémon, de tipos
y de generaciones; las 10 primeras observaciones; estadísticos descriptivos de
cada variable numérica (media, desviación típica, mínimo, mediana y máximo) y
la cuenta de **valores perdidos** del conjunto original, antes de excluir
filas incompletas. No usa los botones.

### Distribuciones

| Pestaña | Qué representa | Detalles estadísticos |
|---------|----------------|-----------------------|
| **Ataque por tipo** (boxplot) | Caja y bigotes de uno o varios parámetros, por tipo. | Outliers de Tukey por grupo, dibujados con sprite. Filtro por generaciones. |
| **Violin plot** | Densidad de cada grupo más todas las observaciones como sprites. | `scale = "width"` (todos los violines con el mismo ancho máximo, así se compara la forma y no el tamaño de la muestra) y `trim = FALSE` (la densidad se extiende más allá del rango observado). Los sprites se reparten con `ggbeeswarm::position_quasirandom`, sin solaparse. |
| **Raincloud** | Medio violín (densidad) + boxplot + observaciones. | Densidad por tipo con 128 puntos, normalizada a máximo 1. Un tipo con una sola observación o con todos los valores iguales no muestra densidad. Sprites solo en los outliers de cada tipo. Admite cualquier variable numérica. |
| **ECDF** | Función de distribución acumulada empírica por tipo. | No depende del número de *bins* ni del ancho de banda. Opcionalmente añade en negro discontinuo la curva de **todos** los Pokémon como referencia: una curva a la derecha de la de referencia indica valores más altos que la media global. |
| **Boxplots individuales** | Una variable numérica para todos los Pokémon. | Un único boxplot con los outliers de Tukey como sprites. Filtro por generaciones. |

**Regla tipos/parámetros** (boxplot y violín). Con **varios tipos** solo se
representa **un parámetro** (el eje X son los tipos). Con **un tipo** puedes
comparar **varios parámetros** (el eje X son los parámetros). Si rompes la
regla, la app recorta la selección que no acabas de tocar y avisa.

### Relaciones

| Pestaña | Qué representa | Detalles estadísticos |
|---------|----------------|-----------------------|
| **Explorador** | Diagrama de dispersión de cualquier par de variables. | X e Y pueden ser variables numéricas, `PC1–PC3` o `mahalanobis`. Opcionales: tamaño del punto según otra variable, facetas por tipo o por generación, sprites y recta de regresión lineal (`lm`, sin banda de confianza). Arrastrando sobre el gráfico (*brush*) la tabla inferior muestra las filas seleccionadas. |
| **Correlación** | Matriz de correlaciones (mapa de calor). | Correlación de **Pearson** con `pairwise.complete.obs`, escala de −1 a 1. Opción de ordenar variables por **clustering jerárquico** sobre `1 − r` (variables correlacionadas quedan juntas). Las variables constantes se descartan y se necesitan al menos dos. Usa todos los Pokémon. |
| **Coord. paralelas** | Una línea por Pokémon que cruza las variables. | Cada variable se reescala a **0–1** (mínimo–máximo) dentro de los tipos elegidos, para poder compararlas aunque tengan unidades distintas. Se puede resaltar un Pokémon concreto en negro. |

### Reducción dimensional y outliers multivariantes

| Pestaña | Qué representa | Detalles estadísticos |
|---------|----------------|-----------------------|
| **PCA** | Plano de dos componentes principales (PC1, PC2, PC3 a elegir). | `prcomp` sobre las 6 estadísticas, **centradas y estandarizadas**. Se calcula **una sola vez con todos los Pokémon**; filtrar por generación solo oculta puntos, no recalcula los componentes. El *biplot* añade flechas con las cargas de cada variable. La tabla inferior da la **varianza explicada** y acumulada por componente. |
| **PCA por tipo** | El mismo PCA, pero solo con los Pokémon de un tipo. | Incluye los Pokémon cuyo `type_1` **o** `type_2` sea el tipo elegido. Usa los componentes globales (no recalcula), por lo que se ve dónde cae ese tipo en el mapa general. El título indica cuántos hay. |
| **UMAP** | Proyección no lineal de las 6 estadísticas. | `uwot::umap` sobre las estadísticas estandarizadas, con `n_neighbors = 15`, `min_dist = 0.3` y semilla fija (42, reproducible). Se calcula al arrancar. Sirve para ver **grupos de Pokémon parecidos**; las distancias entre grupos y los ejes **no son interpretables** como en el PCA. Requiere el paquete `uwot`. |
| **Outliers multivariantes** | Pokémon atípicos en conjunto, sobre el plano PC1–PC2. | **Distancia de Mahalanobis al cuadrado** sobre las 6 estadísticas, con la media y la covarianza de todos los Pokémon; tiene en cuenta las correlaciones entre estadísticas. Con sprite: los que superan el cuantil de una **χ² con 6 gl** al nivel `p` elegido (0,05; 0,01; 0,001; 0,0001). Ese corte supone normalidad multivariante, que los datos no cumplen exactamente, por lo que es **orientativo**. La tabla da los 15 más atípicos. |

### Perfiles

| Pestaña | Qué representa | Detalles estadísticos |
|---------|----------------|-----------------------|
| **Heatmap** | Mapa de calor de Pokémon × variables. | Cada variable se estandariza (**z-score**); filas y columnas se agrupan con **clustering jerárquico** de `heatmap()` (distancia euclídea, enlace completo). La barra lateral coloreada indica el tipo. Si hay más Pokémon que el máximo elegido (30–600) se toma una **muestra aleatoria reproducible**; los nombres de fila solo se muestran con 60 filas o menos. Es un gráfico de R base. |
| **Radar** | Perfil de hasta 5 Pokémon en las 6 estadísticas. | Todos los radares comparten la misma escala, de 0 al máximo global de las estadísticas, así que son comparables entre sí. Color según el tipo y trazo distinto por Pokémon. Conviene recordar que el **área** de un radar depende del orden de los ejes: es mejor leerlo eje a eje. |

### Opciones globales

La pestaña **Opciones** cambia el tamaño de la fuente (con opción de aplicarlo
también a la interfaz), la escala de los sprites y la carpeta de la que se
cargan (`sprites` o `sprites_mini`). Se aplican en vivo al gráfico temporal y
a los archivos que se guarden después. No cambian ningún cálculo estadístico.

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

La carpeta `data/` se sube con la app. Comprueba que `png` y `svglite` figuran
entre las dependencias detectadas (o usa `renv::snapshot()`). En shinyapps.io
el disco es temporal: los archivos de `outputs/` se pierden al reiniciar la
instancia y quedan en el servidor, no en tu equipo; para conservarlos, ejecuta
la app en local. Cuanto más pequeña sea
`data/`, más rápido el despliegue: puedes borrar `data/sprites/` si solo usas
`sprites_mini`.

## Estado de la verificación

Los scripts se revisaron de forma estática (delimitadores equilibrados, todos
los `input` y `output` de la interfaz coinciden con los de la lógica), pero
**no se han podido ejecutar en R**. Primera comprobación recomendada:

- Que los sprites tengan un tamaño parecido al de v6; si no, ajusta `FACTOR_TAM`.
- Que el icono de *Guardar* (`icon("save")`) se muestre en tu versión de Shiny.
- Que *Guardar* cree `outputs/*.png` y `outputs/*.svg` en todas las pestañas
  (el heatmap usa gráficos de R base) y que los sprites y los emojis se vean
  bien en ambos formatos.
