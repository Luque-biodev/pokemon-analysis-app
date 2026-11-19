# Instalar renv si no está instalado
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Restaurar el entorno del proyecto
renv::restore()
