
# ==============================================================================
# FASE 2: CONSTRUCCIÓN Y ARMONIZACIÓN DE MICRODATOS EPA
# Periodo: 2021T1 - 2026T3
# Población objetivo: jóvenes de 25 a 34 años
# ==============================================================================

library(tidyverse)
library(haven)
library(janitor)
library(stringr)

# 1. CONFIGURACIÓN ------------------------------------------------------------

dir_micro <- "data-raw/microdatos"
dir_anexos <- "data-raw/factores_base_2021"
dir_output <- "output"

dir.create(
  file.path(dir_output, "base"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Periodos esperados: 2021T1 a 2026T3

periodos <- crossing(
  anio = 2021:2026,
  trimestre = 1:4
) %>%
  filter(
    anio < 2026 | trimestre <= 3
  ) %>%
  mutate(
    periodo = paste0(anio, "T", trimestre),
    anio = as.integer(anio),
    trimestre = as.integer(trimestre)
  )

# 2. FUNCIONES DE LECTURA -----------------------------------------------------

# Busca el dataframe de microdatos dentro de un RData.
# No utiliza el dataframe de metadatos como si fueran observaciones.

leer_rdata <- function(ruta, tipo = c("microdatos", "anexo")) {
  
  tipo <- match.arg(tipo)
  
  entorno <- new.env(parent = globalenv())
  objetos <- load(ruta, envir = entorno)
  
  tablas <- mget(
    objetos,
    envir = entorno
  )
  
  tablas <- tablas[
    map_lgl(tablas, is.data.frame)
  ]
  
  if (length(tablas) == 0) {
    stop("No hay dataframes dentro de: ", ruta)
  }
  
  if (tipo == "microdatos") {
    
    preferidos <- c("Microdatos", "df_microdatos")
    
    for (nombre in preferidos) {
      if (nombre %in% names(tablas)) {
        return(as_tibble(tablas[[nombre]]))
      }
    }
    
    # Respaldo: dataframe con mayor número de filas
    n_filas <- map_int(tablas, nrow)
    
    return(as_tibble(tablas[[which.max(n_filas)]]))
  }
  
  if (tipo == "anexo") {
    
    candidatos <- keep(
      tablas,
      ~ "FACB2021" %in% names(.x) &&
        "NVIVI" %in% names(.x)
    )
    
    if (length(candidatos) != 1) {
      stop(
        "No se ha podido identificar un único dataframe ",
        "con FACB2021 y NVIVI en: ", ruta
      )
    }
    
    return(as_tibble(candidatos[[1]]))
  }
}

# Lee microdatos de cualquier trimestre.

leer_microdatos <- function(anio, trimestre) {
  
  periodo <- paste0(anio, "T", trimestre)
  
  if (anio %in% c(2021, 2022)) {
    
    ruta <- file.path(
      dir_micro,
      as.character(anio),
      "STATA",
      paste0("EPA_", periodo, ".dta")
    )
    
    if (!file.exists(ruta)) {
      stop("No encuentro el fichero Stata: ", ruta)
    }
    
    datos <- haven::read_dta(ruta)
    
  } else {
    
    ruta <- file.path(
      dir_micro,
      as.character(anio),
      "R",
      paste0("EPA_", periodo, ".RData")
    )
    
    if (!file.exists(ruta)) {
      stop("No encuentro el fichero RData: ", ruta)
    }
    
    datos <- leer_rdata(
      ruta,
      tipo = "microdatos"
    )
  }
  
  as_tibble(datos)
}

# Lee el anexo correspondiente al trimestre.

leer_anexo <- function(anio, trimestre) {
  
  periodo <- paste0(anio, "T", trimestre)
  
  ruta <- file.path(
    dir_anexos,
    "R",
    paste0("EPAAnexo_", periodo, ".RData")
  )
  
  if (!file.exists(ruta)) {
    stop("No encuentro el anexo: ", ruta)
  }
  
  leer_rdata(
    ruta,
    tipo = "anexo"
  )
}

# 3. FUNCIONES AUXILIARES -----------------------------------------------------

# Conservamos los códigos como texto para evitar perder ceros
# iniciales en variables categóricas.

normalizar_codigos <- function(datos) {
  
  variables_caracter <- c(
    "CICLO", "CCAA", "PROV", "NVIVI",
    "NIVEL", "NPERS", "EDAD1", "SEXO1",
    "NFORMA", "AOI", "SITU",
    "DUCON1", "DUCON2", "DUCON3",
    "PARCO1", "PARCO2"
  )
  
  datos %>%
    mutate(
      across(
        any_of(variables_caracter),
        ~ as.character(haven::zap_labels(.x))
      )
    )
}

# Comprobación de variables mínimas.

variables_necesarias <- c(
  "CICLO", "CCAA", "PROV", "NVIVI",
  "NIVEL", "NPERS", "EDAD1", "SEXO1",
  "NFORMA", "AOI", "SITU",
  "DUCON1", "DUCON2", "DUCON3",
  "PARCO1", "PARCO2"
)

# 4. PROCESAR CADA TRIMESTRE --------------------------------------------------

bases_trimestrales <- list()
auditoria_trimestral <- list()

for (i in seq_len(nrow(periodos))) {
  
  anio <- periodos$anio[i]
  trimestre <- periodos$trimestre[i]
  periodo <- periodos$periodo[i]
  
  message("Procesando ", periodo)
  
  # 4.1 Leer microdatos
  datos <- leer_microdatos(anio, trimestre)
  
  # 4.2 Comprobar variables disponibles
  faltantes <- setdiff(
    variables_necesarias,
    names(datos)
  )
  
  if (length(faltantes) > 0) {
    stop(
      "Faltan variables en ", periodo, ": ",
      paste(faltantes, collapse = ", ")
    )
  }
  
  datos <- normalizar_codigos(datos)
  
  # 4.3 Incorporar el factor correcto
  if (anio <= 2023) {
    
    claves_anexo <- c("CICLO", "CCAA", "NVIVI", "NIVEL", "NPERS")
    
    anexo <- leer_anexo(anio, trimestre) %>%
      mutate(
        across(all_of(claves_anexo), ~ as.character(haven::zap_labels(.x)))
      ) %>%
      select(all_of(claves_anexo), FACB2021)
    
    duplicados_anexo <- anexo %>%
      count(across(all_of(claves_anexo))) %>%
      filter(n > 1)
    
    if (nrow(duplicados_anexo) > 0) {
      stop(
        "La clave completa no es única en el anexo de ", periodo,
        ". Revisar antes de unir."
      )
    }
    
    datos <- datos %>%
      left_join(
        anexo,
        by = claves_anexo,
        relationship = "one-to-one"
      )
    
    if (any(is.na(datos$FACB2021))) {
      stop(
        "Hay personas sin FACB2021 después de unir ",
        periodo, ". Revisar claves."
      )
    }
    
    datos <- datos %>%
      mutate(
        factor_elevacion = as.numeric(FACB2021),
        fuente_factor = "FACB2021"
      )
    
  } else {
    
    if (!"FACTOREL" %in% names(datos)) {
      stop("No existe FACTOREL en ", periodo)
    }
    
    datos <- datos %>%
      mutate(
        factor_elevacion = as.numeric(FACTOREL),
        fuente_factor = "FACTOREL"
      )
  }
  
  # 4.4 Añadir identificadores temporales
  datos <- datos %>%
    mutate(
      anio = anio,
      trimestre = trimestre,
      periodo = periodo
    )
  
  # 4.5 Seleccionar población de 25 a 34 años
  edad_num <- suppressWarnings(
    as.numeric(datos$EDAD1)
  )
  
  datos_jovenes <- datos %>%
    mutate(edad = edad_num) %>%
    filter(
      !is.na(edad),
      edad >= 25,
      edad <= 34
    )
  
  # 4.6 Crear indicadores sin modificar las variables originales
  datos_jovenes <- datos_jovenes %>%
    mutate(
      jornada_parcial = case_when(
        PARCO1 == "6" ~ 1L,
        PARCO1 == "1" ~ 0L,
        TRUE ~ NA_integer_
      ),
      parcialidad_involuntaria = case_when(
        PARCO1 == "6" & PARCO2 == "6" ~ 1L,
        PARCO1 == "6" & PARCO2 %in% c("1", "2", "3", "4", "5", "7", "8") ~ 0L,
        TRUE ~ NA_integer_
      ),
      contrato_temporal = case_when(
        DUCON1 == "6" ~ 1L,
        DUCON1 == "1" ~ 0L,
        TRUE ~ NA_integer_
      )
    )
  
  # 4.7 Guardar trimestre
  bases_trimestrales[[periodo]] <- datos_jovenes
  
  auditoria_trimestral[[periodo]] <- tibble(
    periodo = periodo,
    n_microdatos = nrow(datos),
    n_jovenes_25_34 = nrow(datos_jovenes),
    n_factores_na = sum(is.na(datos_jovenes$factor_elevacion)),
    n_factores_cero = sum(datos_jovenes$factor_elevacion <= 0, na.rm = TRUE),
    factor_utilizado = unique(datos_jovenes$fuente_factor)[1]
  )
}

# 5. UNIFICAR TRIMESTRES ------------------------------------------------------

base_jovenes <- bind_rows(bases_trimestrales) %>%
  arrange(anio, trimestre)

# 6. AUDITORÍA FINAL ----------------------------------------------------------

auditoria <- bind_rows(auditoria_trimestral)

write_csv(
  auditoria,
  file.path(dir_output, "base", "auditoria_construccion.csv")
)

# Comprobar que hay 23 trimestres.
control <- tibble(
  periodo_esperado = periodos$periodo,
  encontrado = periodos$periodo %in% unique(base_jovenes$periodo)
)

write_csv(
  control,
  file.path(dir_output, "base", "control_periodos_base.csv")
)

# Comprobar estructura de la base final.
resumen <- tibble(
  n_observaciones = nrow(base_jovenes),
  n_variables = ncol(base_jovenes),
  n_periodos = n_distinct(base_jovenes$periodo),
  n_pesos_na = sum(is.na(base_jovenes$factor_elevacion)),
  n_pesos_no_positivos = sum(
    base_jovenes$factor_elevacion <= 0,
    na.rm = TRUE
  )
)

print(resumen)
print(auditoria, n = Inf)

# 7. GUARDAR BASE ------------------------------------------------------------

saveRDS(
  base_jovenes,
  file.path(
    dir_output,
    "base",
    "epa_jovenes_25_34_2021_2026.rds"
  )
)

# CSV para inspección y reutilización en otros programas.
write_csv(
  base_jovenes,
  file.path(
    dir_output,
    "base",
    "epa_jovenes_25_34_2021_2026.csv"
  )
)

cat("\nCONSTRUCCIÓN FINALIZADA.\n")
cat("Base guardada en output/base/\n")
