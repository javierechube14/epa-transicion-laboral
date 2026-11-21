# ==============================================================================
# AJUSTES AUTOMÁTICOS DE LA FASE 3
# ==============================================================================

archivo_script <- "scripts/03_operacionalizacion.R"
archivo_gitignore <- ".gitignore"

# 1. COMPROBAR QUE EXISTE EL SCRIPT ORIGINAL -------------------------------
if (!file.exists(archivo_script)) {
  stop("No se encuentra scripts/03_operacionalizacion.R. Comprueba que estás en la raíz del proyecto.")
}

# 2. CREAR COPIA DE SEGURIDAD -----------------------------------------------
archivo_backup <- paste0(archivo_script, ".bak")
if (!file.exists(archivo_backup)) {
  file.copy(archivo_script, archivo_backup)
  message("Copia de seguridad creada: ", archivo_backup)
}

# 3. RENOMBRAR LA VARIABLE EDUCATIVA ----------------------------------------
contenido <- readLines(archivo_script, encoding = "UTF-8", warn = FALSE)
texto <- paste(contenido, collapse = "\n")
apariciones <- lengths(gregexpr("educacion_3_niveles", texto, fixed = TRUE))

if (apariciones == 0) {
  message("El nombre antiguo ya no aparece. No hay nada que renombrar.")
} else {
  texto <- gsub("educacion_3_niveles", "educacion_4_niveles", texto, fixed = TRUE)
  writeLines(strsplit(texto, "\n", fixed = TRUE)[[1]], archivo_script, useBytes = TRUE)
  message("Variable renombrada. Apariciones sustituidas: ", apariciones)
}

# 4. ACTUALIZAR .GITIGNORE --------------------------------------------------
if (!file.exists(archivo_gitignore)) {
  writeLines("output/base/", archivo_gitignore)
  message(".gitignore creado.")
} else {
  lineas_gitignore <- readLines(archivo_gitignore, warn = FALSE)
  ya_ignorado <- any(trimws(lineas_gitignore) %in% c("output/base", "output/base/"))
  
  if (!ya_ignorado) {
    write("output/base/", file = archivo_gitignore, append = TRUE)
    message("Añadido output/base/ a .gitignore.")
  } else {
    message("output/base/ ya estaba incluido en .gitignore.")
  }
}

# 5. VERIFICAR EL CAMBIO ----------------------------------------------------
contenido_final <- paste(readLines(archivo_script, warn = FALSE), collapse = "\n")
if (grepl("educacion_3_niveles", contenido_final, fixed = TRUE)) {
  stop("Todavía aparece el nombre antiguo de la variable.")
}
if (!grepl("educacion_4_niveles", contenido_final, fixed = TRUE)) {
  stop("No se ha encontrado el nuevo nombre educativo.")
}
message("Verificación de cambios superada.")

# 6. EJECUTAR AUTOMÁTICAMENTE LA FASE 3 -------------------------------------
message("\nEjecutando de nuevo la operacionalización...")
source(archivo_script, encoding = "UTF-8")
message("\nAJUSTES COMPLETADOS.")