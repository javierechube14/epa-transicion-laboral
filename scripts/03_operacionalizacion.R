
# ==============================================================================
# FASE 3. OPERACIONALIZACIÓN SOCIOLÓGICA
# EPA: población de 25-34 años, 2021T1-2026T3
#
# Objetivos:
#   1. Limpiar códigos y valores ausentes.
#   2. Construir indicadores con universos de referencia explícitos.
#   3. Armonizar el nivel educativo.
#   4. Guardar la base analítica y controles de calidad.
#
# Entrada:
#   output/base/epa_jovenes_25_34_2021_2026.rds
#
# Salidas:
#   output/base/epa_jovenes_25_34_analitica.rds
#   output/base/epa_jovenes_25_34_analitica.csv
#   output/auditoria/auditoria_operacionalizacion.csv
# ==============================================================================


# 0. PAQUETES Y CONFIGURACIÓN -----------------------------------------------

library(tidyverse)

ruta_entrada <- "output/base/epa_jovenes_25_34_2021_2026.rds"

ruta_base <- "output/base"
ruta_auditoria <- "output/auditoria"

dir.create(ruta_base, showWarnings = FALSE, recursive = TRUE)
dir.create(ruta_auditoria, showWarnings = FALSE, recursive = TRUE)


# 1. CARGA Y VALIDACIÓN INICIAL ----------------------------------------------

if (!file.exists(ruta_entrada)) {
  stop("No se encuentra la base de entrada: ", ruta_entrada)
}

base <- readRDS(ruta_entrada)

variables_necesarias <- c(
  "periodo",
  "EDAD1", "SEXO1", "CCAA", "NFORMA", "AOI",
  "SITU", "DUCON1", "DUCON2", "DUCON3",
  "PARCO1", "PARCO2", "factor_elevacion"
)

faltan <- setdiff(variables_necesarias, names(base))

if (length(faltan) > 0) {
  stop(
    "Faltan variables necesarias en la base: ",
    paste(faltan, collapse = ", ")
  )
}

n_inicial <- nrow(base)


# 2. LIMPIEZA DE CÓDIGOS -----------------------------------------------------

# Elimina espacios al principio y al final de todas las variables
# de texto. Convierte las cadenas vacías en NA.
#
# No modifica los códigos válidos: por ejemplo, "05" sigue siendo "05".

base <- base %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(trimws(.x), "")
    )
  )

# Aseguramos que las variables categóricas se traten como códigos
# de texto y no como números, preservando los ceros iniciales.

variables_codigo <- c(
  "EDAD1", "SEXO1", "CCAA", "NFORMA", "AOI",
  "SITU", "DUCON1", "DUCON2", "DUCON3",
  "PARCO1", "PARCO2"
)

base <- base %>%
  mutate(
    across(all_of(variables_codigo), as.character)
  )


# 3. VALIDACIÓN DE CÓDIGOS ---------------------------------------------------

codigos_validos <- list(
  EDAD1 = c("25", "30"),
  SEXO1 = c("1", "6"),
  CCAA = c(sprintf("%02d", 1:17), "51", "52"),
  NFORMA = c("AN", "P1", "P2", "S1", "SG", "SP", "SU"),
  AOI = c("03", "04", "05", "06", "07", "08", "09"),
  SITU = c("01", "03", "05", "06", "07", "08", "09"),
  DUCON1 = c("1", "6"),
  DUCON2 = c("1", "6"),
  DUCON3 = c(
    "00", "01", "02", "03", "04", "05",
    "06", "07", "08", "09"
  ),
  PARCO1 = c("1", "6"),
  PARCO2 = c("00", "01", "02", "03", "04", "05", "06", "07")
)

control_codigos <- map_dfr(
  names(codigos_validos),
  function(v) {
    observados <- unique(na.omit(base[[v]]))
    inesperados <- setdiff(observados, codigos_validos[[v]])
    
    tibble(
      variable = v,
      codigos_observados = paste(sort(observados), collapse = ", "),
      codigos_no_esperados = paste(sort(inesperados), collapse = ", "),
      n_codigos_no_esperados = length(inesperados)
    )
  }
)

if (any(control_codigos$n_codigos_no_esperados > 0)) {
  warning(
    "Se han encontrado códigos no contemplados en el diccionario. ",
    "Revisa control_codigos antes de interpretar los resultados."
  )
}

print(control_codigos, n = Inf)


# 4. DEFINICIÓN DE UNIVERSOS E INDICADORES -----------------------------------

base <- base %>%
  mutate(
    
    # ------------------------------------------------------------------------
    # 4.1. ACTIVIDAD ECONÓMICA
    # ------------------------------------------------------------------------
    
    # AOI:
    # 03 = Ocupados subempleados por insuficiencia de horas
    # 04 = Resto de ocupados
    # 05 = Parados que buscan primer empleo
    # 06 = Parados que han trabajado antes
    # 07-09 = Inactivos
    
    ocupado = case_when(
      AOI %in% c("03", "04") ~ 1L,
      AOI %in% c("05", "06", "07", "08", "09") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    parado_oit = case_when(
      AOI %in% c("05", "06") ~ 1L,
      AOI %in% c("03", "04") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    activo_oit = case_when(
      AOI %in% c("03", "04", "05", "06") ~ 1L,
      AOI %in% c("07", "08", "09") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    inactivo_oit = case_when(
      AOI %in% c("07", "08", "09") ~ 1L,
      AOI %in% c("03", "04", "05", "06") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    
    # ------------------------------------------------------------------------
    # 4.2. SITUACIÓN PROFESIONAL
    # ------------------------------------------------------------------------
    
    # Universo: personas ocupadas con situación profesional identificada.
    # 07 = Asalariado sector público
    # 08 = Asalariado sector privado
    
    asalariado = case_when(
      SITU %in% c("07", "08") ~ 1L,
      SITU %in% c("01", "03", "05", "06", "09") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    tipo_asalariado = case_when(
      SITU == "07" ~ "Público",
      SITU == "08" ~ "Privado",
      SITU %in% c("01", "03", "05", "06", "09") ~ NA_character_,
      TRUE ~ NA_character_
    ),
    
    asalariado_publico = case_when(
      SITU == "07" ~ 1L,
      SITU == "08" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    asalariado_privado = case_when(
      SITU == "08" ~ 1L,
      SITU == "07" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    
    # ------------------------------------------------------------------------
    # 4.3. TEMPORALIDAD CONTRACTUAL
    # ------------------------------------------------------------------------
    
    # Universo: asalariados (SITU 07 o 08).
    # DUCON1: 1 = indefinido; 6 = temporal.
    
    contrato_indefinido = case_when(
      asalariado == 1L & DUCON1 == "1" ~ 1L,
      asalariado == 1L & DUCON1 == "6" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    contrato_temporal = case_when(
      asalariado == 1L & DUCON1 == "6" ~ 1L,
      asalariado == 1L & DUCON1 == "1" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # DUCON2 solo se interpreta entre asalariados indefinidos.
    fijo_discontinuo_entre_indefinidos = case_when(
      contrato_indefinido == 1L & DUCON2 == "6" ~ 1L,
      contrato_indefinido == 1L & DUCON2 == "1" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Indicador para calcular el peso de fijos discontinuos
    # sobre el conjunto de asalariados.
    fijo_discontinuo_entre_asalariados = case_when(
      asalariado == 1L &
        DUCON1 == "1" &
        DUCON2 == "6" ~ 1L,
      
      asalariado == 1L &
        DUCON1 == "1" &
        DUCON2 == "1" ~ 0L,
      
      asalariado == 1L &
        DUCON1 == "6" ~ 0L,
      
      TRUE ~ NA_integer_
    ),
    
    
    # ------------------------------------------------------------------------
    # 4.4. JORNADA LABORAL
    # ------------------------------------------------------------------------
    
    # PARCO1:
    # 1 = Jornada completa
    # 6 = Jornada parcial
    #
    # Universo: personas ocupadas.
    
    jornada_parcial = case_when(
      ocupado == 1L & PARCO1 == "6" ~ 1L,
      ocupado == 1L & PARCO1 == "1" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    jornada_completa = case_when(
      ocupado == 1L & PARCO1 == "1" ~ 1L,
      ocupado == 1L & PARCO1 == "6" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # PARCO2:
    # 05 = No haber podido encontrar un trabajo a jornada completa.
    # 06 = No querer un trabajo a jornada completa.
    #
    # Universo: personas ocupadas a jornada parcial.
    
    parcialidad_involuntaria = case_when(
      PARCO1 == "6" & PARCO2 == "05" ~ 1L,
      PARCO1 == "6" &
        PARCO2 %in% c("00", "01", "02", "03", "04", "06", "07") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    
    # ------------------------------------------------------------------------
    # 4.5. NIVEL EDUCATIVO ARMONIZADO
    # ------------------------------------------------------------------------
    
    # Agrupación analítica:
    # AN, P1, P2 = Hasta educación primaria
    # S1 = Primera etapa de secundaria
    # SG = Segunda etapa secundaria general
    # SP = Segunda etapa secundaria profesional
    # SU = Educación superior
    #
    # Se conserva NFORMA original para permitir otras agrupaciones.
    
    educacion_armonizada = case_when(
      NFORMA %in% c("AN", "P1", "P2") ~ "Primaria o inferior",
      NFORMA == "S1" ~ "Secundaria primera etapa",
      NFORMA == "SG" ~ "Secundaria segunda etapa general",
      NFORMA == "SP" ~ "Secundaria segunda etapa profesional",
      NFORMA == "SU" ~ "Educación superior",
      TRUE ~ NA_character_
    ),
    
    educacion_armonizada = factor(
      educacion_armonizada,
      levels = c(
        "Primaria o inferior",
        "Secundaria primera etapa",
        "Secundaria segunda etapa general",
        "Secundaria segunda etapa profesional",
        "Educación superior"
      ),
      ordered = TRUE
    ),
    
    # Agrupación alternativa para análisis con menos categorías.
    # Se mantienen separadas las dos modalidades de segunda etapa
    # en educacion_armonizada.
    
    educacion_4_niveles = case_when(
      NFORMA %in% c("AN", "P1", "P2") ~ "Bajo",
      NFORMA == "S1" ~ "Medio-bajo",
      NFORMA %in% c("SG", "SP") ~ "Medio-alto",
      NFORMA == "SU" ~ "Alto",
      TRUE ~ NA_character_
    ),
    
    educacion_4_niveles = factor(
      educacion_4_niveles,
      levels = c("Bajo", "Medio-bajo", "Medio-alto", "Alto"),
      ordered = TRUE
    )
  )


# 5. VARIABLES TEMPORALES Y SOCIODEMOGRÁFICAS -------------------------------

# Convertimos los grupos de edad y sexo en etiquetas legibles,
# conservando también sus códigos originales.

base <- base %>%
  mutate(
    grupo_edad = recode(
      EDAD1,
      "25" = "25-29",
      "30" = "30-34",
      .default = NA_character_
    ),
    
    sexo = recode(
      SEXO1,
      "1" = "Hombre",
      "6" = "Mujer",
      .default = NA_character_
    )
  )


# 6. COMPROBACIONES DE CONSISTENCIA -----------------------------------------

# Las siguientes condiciones deberían cumplirse:
# - Todo ocupado debe estar clasificado como activo.
# - Todo parado debe estar clasificado como activo.
# - La temporalidad solo debe estar definida entre asalariados.
# - La parcialidad solo debe estar definida entre ocupados.
# - La parcialidad involuntaria solo debe estar definida entre parciales.

errores_consistencia <- tibble(
  control = c(
    "Ocupados clasificados como inactivos",
    "Parados clasificados como inactivos",
    "Temporalidad definida fuera de asalariados",
    "Parcialidad definida fuera de ocupados",
    "Involuntariedad definida fuera de parciales"
  ),
  
  n_errores = c(
    sum(base$ocupado == 1L & base$activo_oit != 1L, na.rm = TRUE),
    sum(base$parado_oit == 1L & base$activo_oit != 1L, na.rm = TRUE),
    sum(!is.na(base$contrato_temporal) & base$asalariado != 1L, na.rm = TRUE),
    sum(!is.na(base$jornada_parcial) & base$ocupado != 1L, na.rm = TRUE),
    sum(
      !is.na(base$parcialidad_involuntaria) &
        base$PARCO1 != "6",
      na.rm = TRUE
    )
  )
)

print(errores_consistencia, n = Inf)

if (any(errores_consistencia$n_errores > 0)) {
  stop(
    "Se han detectado errores de consistencia. ",
    "No se guardará la base hasta resolverlos."
  )
}


# 7. AUDITORÍA DE INDICADORES -----------------------------------------------

# Frecuencias de los indicadores y variables armonizadas.
# Los NA quedan contabilizados explícitamente.

variables_indicadores <- c(
  "ocupado",
  "parado_oit",
  "activo_oit",
  "inactivo_oit",
  "asalariado",
  "asalariado_publico",
  "asalariado_privado",
  "contrato_indefinido",
  "contrato_temporal",
  "fijo_discontinuo_entre_indefinidos",
  "fijo_discontinuo_entre_asalariados",
  "jornada_parcial",
  "jornada_completa",
  "parcialidad_involuntaria"
)

auditoria_indicadores <- map_dfr(
  variables_indicadores,
  function(v) {
    tibble(
      variable = v,
      n_validos = sum(!is.na(base[[v]])),
      n_na = sum(is.na(base[[v]])),
      n_unos = sum(base[[v]] == 1L, na.rm = TRUE),
      n_ceros = sum(base[[v]] == 0L, na.rm = TRUE)
    )
  }
)

auditoria_educacion <- base %>%
  count(educacion_armonizada, .drop = FALSE) %>%
  mutate(
    porcentaje = round(n / sum(n) * 100, 2)
  )

auditoria_periodos <- base %>%
  group_by(periodo) %>%
  summarise(
    n_observaciones = n(),
    n_ocupados = sum(ocupado == 1L, na.rm = TRUE),
    n_parados = sum(parado_oit == 1L, na.rm = TRUE),
    n_asalariados = sum(asalariado == 1L, na.rm = TRUE),
    n_temporalidad_validos = sum(!is.na(contrato_temporal)),
    n_parcialidad_validos = sum(!is.na(jornada_parcial)),
    n_parcialidad_involuntaria_validos =
      sum(!is.na(parcialidad_involuntaria)),
    n_factor_na = sum(is.na(factor_elevacion)),
    n_factor_no_positivo = sum(
      factor_elevacion <= 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# 8. GUARDAR AUDITORÍAS ------------------------------------------------------

write_csv(
  control_codigos,
  file.path(ruta_auditoria, "control_codigos_operacionalizacion.csv")
)

write_csv(
  errores_consistencia,
  file.path(ruta_auditoria, "control_consistencia_indicadores.csv")
)

write_csv(
  auditoria_indicadores,
  file.path(ruta_auditoria, "auditoria_indicadores.csv")
)

write_csv(
  auditoria_educacion,
  file.path(ruta_auditoria, "auditoria_educacion.csv")
)

write_csv(
  auditoria_periodos,
  file.path(ruta_auditoria, "auditoria_indicadores_periodo.csv")
)


# 9. GUARDAR BASE ANALÍTICA --------------------------------------------------

# Conservamos todas las variables originales y añadimos las nuevas.
# No se eliminan observaciones por tener NA en indicadores que no
# corresponden a su universo de referencia.

saveRDS(
  base,
  file.path(ruta_base, "epa_jovenes_25_34_analitica.rds")
)

write_csv(
  base,
  file.path(ruta_base, "epa_jovenes_25_34_analitica.csv"),
  na = ""
)


# 10. RESUMEN FINAL ----------------------------------------------------------

cat("\n============================================\n")
cat("FASE 3: OPERACIONALIZACIÓN FINALIZADA\n")
cat("============================================\n")

cat("Observaciones iniciales:", n_inicial, "\n")
cat("Observaciones finales:", nrow(base), "\n")
cat("Variables finales:", ncol(base), "\n")
cat("Periodos:", n_distinct(base$periodo), "\n")
cat(
  "Factores ausentes:",
  sum(is.na(base$factor_elevacion)),
  "\n"
)
cat(
  "Factores no positivos:",
  sum(base$factor_elevacion <= 0, na.rm = TRUE),
  "\n"
)

cat("\nArchivos generados:\n")
cat("- output/base/epa_jovenes_25_34_analitica.rds\n")
cat("- output/base/epa_jovenes_25_34_analitica.csv\n")
cat("- output/auditoria/auditoria_indicadores.csv\n")
cat("- output/auditoria/auditoria_educacion.csv\n")
cat("- output/auditoria/auditoria_indicadores_periodo.csv\n")
cat("- output/auditoria/control_codigos_operacionalizacion.csv\n")
cat("- output/auditoria/control_consistencia_indicadores.csv\n")
