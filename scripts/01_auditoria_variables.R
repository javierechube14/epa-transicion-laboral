
# ==============================================================================
# FASE 1: AUDITORÍA DE MICRODATOS EPA 2021-2026
# Proyecto: Transición laboral de jóvenes de 25 a 34 años
# Objetivo: inspeccionar estructura, variables, factores y documentación.
# ==============================================================================

# 0. PAQUETES ---------------------------------------------------------------

packages <- c("tidyverse", "readxl", "janitor", "haven", "stringr")

missing_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(packages, library, character.only = TRUE))

# 1. RUTAS ------------------------------------------------------------------

# Ejecuta el script desde la raíz del proyecto:
# epa-transicion-laboral/

dir_micro <- "data-raw/microdatos"
dir_anexos <- "data-raw/factores_base_2021"
dir_docs <- "data-raw/documentacion"
dir_output <- "output/auditoria"

dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(dir_micro)) {
  stop("No encuentro la carpeta data-raw/microdatos.")
}

# 2. INVENTARIO DE ARCHIVOS -------------------------------------------------

archivos <- list.files(
  "data-raw",
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)

inventario <- tibble(
  ruta = archivos,
  archivo = basename(archivos),
  extension = tolower(tools::file_ext(archivos))
) %>%
  mutate(
    tipo = case_when(
      str_detect(tolower(ruta), "factores_base_2021") ~ "Anexo de factores",
      str_detect(tolower(ruta), "documentacion") ~ "Documentación",
      str_detect(tolower(ruta), "microdatos") ~ "Microdatos",
      TRUE ~ "Otro"
    )
  )

write_csv(
  inventario,
  file.path(dir_output, "01_inventario_archivos.csv")
)

cat("\n--- INVENTARIO DE ARCHIVOS ---\n")
print(count(inventario, tipo, extension))

# 3. IDENTIFICAR TRIMESTRE Y AÑO --------------------------------------------

extraer_periodo <- function(x) {
  
  coincidencia <- str_match(
    tolower(x),
    "([1-4])t(2[1-6])"
  )
  
  if (is.na(coincidencia[1, 1])) {
    return(NA_character_)
  }
  
  trimestre <- coincidencia[1, 2]
  anio <- paste0("20", coincidencia[1, 3])
  
  paste0(anio, "_T", trimestre)
}

inventario <- inventario %>%
  mutate(periodo = map_chr(ruta, extraer_periodo))

# 4. FUNCIÓN PARA LEER LOS FICHEROS -----------------------------------------

leer_tablas <- function(ruta) {
  
  extension <- tolower(tools::file_ext(ruta))
  
  if (extension %in% c("rdata", "rda")) {
    
    entorno <- new.env(parent = globalenv())
    
    objetos <- load(ruta, envir = entorno)
    
    tablas <- mget(
      objetos,
      envir = entorno,
      ifnotfound = list(NULL)
    )
    
    tablas <- tablas[
      map_lgl(tablas, is.data.frame)
    ]
    
    return(tablas)
  }
  
  if (extension == "rds") {
    
    objeto <- readRDS(ruta)
    
    if (is.data.frame(objeto)) {
      return(list(datos = objeto))
    }
    
    if (is.list(objeto)) {
      return(objeto[map_lgl(objeto, is.data.frame)])
    }
  }
  
  if (extension == "sav") {
    return(list(datos = haven::read_sav(ruta)))
  }
  
  if (extension == "dta") {
    return(list(datos = haven::read_dta(ruta)))
  }
  
  if (extension == "sas7bdat") {
    return(list(datos = haven::read_sas(ruta)))
  }
  
  if (extension == "csv") {
    return(list(datos = readr::read_delim(
      ruta,
      delim = ",",
      show_col_types = FALSE
    )))
  }
  
  if (extension == "txt") {
    return(list(datos = readr::read_delim(
      ruta,
      delim = "\t",
      show_col_types = FALSE
    )))
  }
  
  list()
}

# 5. CARGAR TABLAS DE MICRODATOS Y ANEXOS -----------------------------------

extensiones_datos <- c(
  "rdata", "rda", "rds", "sav", "dta",
  "sas7bdat", "csv", "txt"
)

ficheros_datos <- inventario %>%
  filter(
    extension %in% extensiones_datos,
    tipo %in% c("Microdatos", "Anexo de factores")
  ) %>%
  pull(ruta)

tablas <- list()
errores_lectura <- tibble(
  ruta = character(),
  error = character()
)

for (f in ficheros_datos) {
  
  resultado <- tryCatch(
    leer_tablas(f),
    error = function(e) {
      errores_lectura <<- bind_rows(
        errores_lectura,
        tibble(ruta = f, error = conditionMessage(e))
      )
      list()
    }
  )
  
  if (length(resultado) > 0) {
    
    for (nombre_objeto in names(resultado)) {
      
      if (is.data.frame(resultado[[nombre_objeto]])) {
        
        tablas[[length(tablas) + 1]] <- list(
          ruta = f,
          objeto = nombre_objeto,
          datos = resultado[[nombre_objeto]]
        )
      }
    }
  }
}

write_csv(
  errores_lectura,
  file.path(dir_output, "02_errores_lectura.csv")
)

cat("\n--- TABLAS LEÍDAS ---\n")
cat("Número de tablas:", length(tablas), "\n")

# 6. AUDITORÍA DE ESTRUCTURA -----------------------------------------------

estructura <- map_dfr(seq_along(tablas), function(i) {
  
  df <- tablas[[i]]$datos
  ruta <- tablas[[i]]$ruta
  
  tibble(
    ruta = ruta,
    objeto = tablas[[i]]$objeto,
    periodo = extraer_periodo(ruta),
    n_filas = nrow(df),
    n_columnas = ncol(df),
    es_anexo = str_detect(
      tolower(ruta),
      "factores_base_2021|_a\\."
    )
  )
})

write_csv(
  estructura,
  file.path(dir_output, "03_estructura_tablas.csv")
)

print(estructura, n = Inf)

# 7. AUDITORÍA DE VARIABLES -------------------------------------------------

auditar_variables <- function(df, ruta, objeto) {
  
  map_dfr(names(df), function(variable) {
    
    x <- df[[variable]]
    
    etiqueta <- attr(x, "label")
    
    if (is.null(etiqueta)) {
      etiqueta <- ""
    }
    
    valores <- unique(as.character(x[!is.na(x)]))
    
    tibble(
      ruta = ruta,
      objeto = objeto,
      periodo = extraer_periodo(ruta),
      variable = variable,
      etiqueta = as.character(etiqueta)[1],
      clase = paste(class(x), collapse = "/"),
      n_na = sum(is.na(x)),
      porcentaje_na = round(mean(is.na(x)) * 100, 2),
      valores_distintos = length(valores),
      ejemplos = paste(head(valores, 6), collapse = " | ")
    )
  })
}

diccionario_observado <- map_dfr(
  seq_along(tablas),
  function(i) {
    auditar_variables(
      tablas[[i]]$datos,
      tablas[[i]]$ruta,
      tablas[[i]]$objeto
    )
  }
)

write_csv(
  diccionario_observado,
  file.path(dir_output, "04_diccionario_observado.csv")
)

# 8. DETECTAR VARIABLES CANDIDATAS -----------------------------------------

# Buscamos tanto nombres como etiquetas.
# Los resultados son candidatos: no son todavía una selección definitiva.

diccionario_candidatos <- diccionario_observado %>%
  mutate(
    texto_busqueda = str_to_lower(
      paste(variable, etiqueta)
    ),
    dimension = case_when(
      
      str_detect(texto_busqueda, "edad|años cumplidos") ~
        "Edad",
      
      str_detect(texto_busqueda, "\\bsexo\\b") ~
        "Sexo",
      
      str_detect(texto_busqueda, "comunidad autonoma|ccaa|provincia") ~
        "Territorio",
      
      str_detect(texto_busqueda, "nivel.*estudio|estudios terminados|educacion") ~
        "Educación",
      
      str_detect(texto_busqueda, "situacion.*actividad|ocupad|parad|inactiv") ~
        "Situación laboral",
      
      str_detect(texto_busqueda, "tipo.*contrato|duracion.*contrato|temporal|indefin") ~
        "Contrato",
      
      str_detect(texto_busqueda, "tipo.*jornada|tiempo parcial|jornada parcial") ~
        "Jornada",
      
      str_detect(texto_busqueda, "razon.*parcial|no encontrar.*completo|tiempo completo") ~
        "Motivo de jornada parcial",
      
      str_detect(texto_busqueda, "factor|elevacion|peso") ~
        "Factor de elevación",
      
      str_detect(texto_busqueda, "nvivi") ~
        "Identificador de vivienda",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(dimension)) %>%
  select(-texto_busqueda)

write_csv(
  diccionario_candidatos,
  file.path(dir_output, "05_variables_candidatas.csv")
)

cat("\n--- VARIABLES CANDIDATAS ---\n")
print(
  diccionario_candidatos %>%
    distinct(dimension, variable, etiqueta) %>%
    arrange(dimension),
  n = Inf
)

# 9. COMPROBAR FACTORES E IDENTIFICADORES ----------------------------------

comprobar_campos <- function(df, ruta, objeto) {
  
  campos <- c(
    "NVIVI",
    "FACB2021",
    "FACTOREL",
    "EDAD",
    "EDADEST"
  )
  
  tibble(
    ruta = ruta,
    objeto = objeto,
    periodo = extraer_periodo(ruta),
    campo = campos,
    presente = campos %in% names(df)
  )
}

auditoria_factores <- map_dfr(
  seq_along(tablas),
  function(i) {
    comprobar_campos(
      tablas[[i]]$datos,
      tablas[[i]]$ruta,
      tablas[[i]]$objeto
    )
  }
)

write_csv(
  auditoria_factores,
  file.path(dir_output, "06_factores_y_campos.csv")
)

# 10. AUDITAR DISEÑOS DE REGISTRO EXCEL -------------------------------------

ficheros_excel <- list.files(
  dir_docs,
  pattern = "\\.xlsx$",
  full.names = TRUE,
  recursive = TRUE
)

filas_documentacion <- list()

for (f in ficheros_excel) {
  
  hojas <- readxl::excel_sheets(f)
  
  for (hoja in hojas) {
    
    contenido <- tryCatch(
      readxl::read_excel(
        f,
        sheet = hoja,
        col_types = "text"
      ),
      error = function(e) NULL
    )
    
    if (!is.null(contenido) && nrow(contenido) > 0) {
      
      texto_filas <- apply(
        as.data.frame(contenido),
        1,
        function(x) {
          paste(na.omit(x), collapse = " | ")
        }
      )
      
      filas_documentacion[[length(filas_documentacion) + 1]] <-
        tibble(
          archivo = basename(f),
          hoja = hoja,
          fila = seq_along(texto_filas),
          texto = texto_filas
        )
    }
  }
}

if (length(filas_documentacion) > 0) {
  
  auditoria_documentacion <- bind_rows(filas_documentacion)
  
  write_csv(
    auditoria_documentacion,
    file.path(dir_output, "07_documentacion_completa.csv")
  )
  
  patrones <- regex(
    paste(
      "edad",
      "sexo",
      "comunidad autonoma",
      "nivel de estudios",
      "estudios terminados",
      "tipo de contrato",
      "tipo de jornada",
      "tiempo parcial",
      "factor",
      "FACB2021",
      "FACTOREL",
      "NVIVI",
      sep = "|"
    ),
    ignore_case = TRUE
  )
  
  candidatos_documentacion <- auditoria_documentacion %>%
    filter(str_detect(texto, patrones))
  
  write_csv(
    candidatos_documentacion,
    file.path(dir_output, "08_busqueda_en_diseno_registro.csv")
  )
}

# 11. COMPROBACIÓN DE PERIODOS ----------------------------------------------

periodos_esperados <- crossing(
  anio = 2021:2026,
  trimestre = 1:4
) %>%
  filter(
    anio < 2026 |
      trimestre <= 3
  ) %>%
  mutate(
    periodo = paste0(anio, "_T", trimestre)
  )

periodos_detectados <- inventario %>%
  filter(tipo == "Microdatos") %>%
  filter(!is.na(periodo)) %>%
  distinct(periodo)

control_periodos <- periodos_esperados %>%
  left_join(
    periodos_detectados %>% mutate(encontrado = TRUE),
    by = "periodo"
  ) %>%
  mutate(encontrado = replace_na(encontrado, FALSE))

write_csv(
  control_periodos,
  file.path(dir_output, "09_control_periodos.csv")
)

cat("\n--- CONTROL DE PERIODOS ---\n")
print(control_periodos, n = Inf)

# 12. INFORME FINAL ---------------------------------------------------------

sink(file.path(dir_output, "10_resumen_auditoria.txt"))

cat("AUDITORÍA EPA 2021-2026\n")
cat("======================\n\n")

cat("Archivos encontrados:", nrow(inventario), "\n")
cat("Tablas de datos leídas:", length(tablas), "\n")
cat("Errores de lectura:", nrow(errores_lectura), "\n\n")

cat("Estructura de las tablas:\n")
print(estructura)

cat("\nVariables candidatas:\n")
print(
  diccionario_candidatos %>%
    distinct(dimension, variable, etiqueta)
)

cat("\nPresencia de factores e identificadores:\n")
print(auditoria_factores)

cat("\nControl de periodos:\n")
print(control_periodos)

cat("\nInformación de sesión:\n")
print(sessionInfo())

sink()

cat("\nAUDITORÍA FINALIZADA.\n")
cat("Revisa los resultados en:", dir_output, "\n")
