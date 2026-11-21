
# ==============================================================================
# FASE 4B: ANALISIS COMPARATIVO - EPA JOVENES 25-34
# Periodo: 2021T1 - 2026T3
#
# Objetivos:
# 1. Evolucion de la ocupacion segun nivel educativo.
# 2. Evolucion de la temporalidad segun nivel educativo.
# 3. Diferencias laborales por sexo.
# 4. Analisis de la inactividad.
# 5. Comparaciones interanuales.
# 6. Control de tamanos muestrales.
#
# Las estimaciones utilizan los factores de elevacion de la EPA.
# No se calculan errores estandar ni intervalos de confianza.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. CONFIGURACION
# ------------------------------------------------------------------------------

library(tidyverse)

ruta_base <- "output/base/epa_jovenes_25_34_analitica.rds"

if (!file.exists(ruta_base)) {
  stop("No se encuentra la base analitica: ", ruta_base)
}

base <- readRDS(ruta_base)

# Variables necesarias
variables_necesarias <- c(
  "periodo",
  "sexo",
  "educacion_4_niveles",
  "factor_elevacion",
  "ocupado",
  "activo_oit",
  "parado_oit",
  "inactivo_oit",
  "asalariado",
  "contrato_temporal",
  "jornada_parcial",
  "parcialidad_involuntaria"
)

variables_faltantes <- setdiff(
  variables_necesarias,
  names(base)
)

if (length(variables_faltantes) > 0) {
  stop(
    "Faltan variables necesarias: ",
    paste(variables_faltantes, collapse = ", "),
    ". Revisa la Fase 3."
  )
}

# Comprobar factores de elevacion
if (
  any(is.na(base$factor_elevacion)) ||
  any(base$factor_elevacion <= 0)
) {
  stop("Hay factores de elevacion ausentes o no positivos.")
}

# Ordenar periodos cronologicamente
periodos <- sort(unique(as.character(base$periodo)))

base <- base %>%
  mutate(
    periodo = factor(
      as.character(periodo),
      levels = periodos,
      ordered = TRUE
    ),
    anio = as.integer(substr(as.character(periodo), 1, 4)),
    trimestre = substr(as.character(periodo), 6, 6)
  )

# Crear carpetas de resultados
dir.create(
  "output/fase_4b/tablas",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "output/fase_4b/graficos",
  recursive = TRUE,
  showWarnings = FALSE
)

cat("\n============================================\n")
cat("FASE 4B: INICIO DEL ANALISIS COMPARATIVO\n")
cat("============================================\n")
cat("Observaciones:", nrow(base), "\n")
cat("Periodos:", length(periodos), "\n")
cat("Periodo inicial:", periodos[1], "\n")
cat("Periodo final:", tail(periodos, 1), "\n")


# ------------------------------------------------------------------------------
# 2. FUNCION DE CALCULO DE TASAS PONDERADAS
# ------------------------------------------------------------------------------

tasa_ponderada <- function(datos, numerador, denominador = NULL) {
  
  x <- datos[[numerador]]
  peso <- datos$factor_elevacion
  
  validos <- !is.na(x) &
    !is.na(peso) &
    peso > 0
  
  if (!is.null(denominador)) {
    
    d <- datos[[denominador]]
    
    validos <- validos &
      !is.na(d) &
      d == 1
  }
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  sum(peso[validos] * (x[validos] == 1)) /
    sum(peso[validos])
}


# Funcion para contar el numero de observaciones
# que forman parte del denominador de cada indicador

n_denominador <- function(datos, variable, denominador = NULL) {
  
  x <- datos[[variable]]
  
  validos <- !is.na(x) &
    !is.na(datos$factor_elevacion) &
    datos$factor_elevacion > 0
  
  if (!is.null(denominador)) {
    
    d <- datos[[denominador]]
    
    validos <- validos &
      !is.na(d) &
      d == 1
  }
  
  sum(validos)
}


# ------------------------------------------------------------------------------
# 3. FUNCION GENERAL DE INDICADORES
# ------------------------------------------------------------------------------

calcular_indicadores <- function(datos) {
  
  tibble(
    
    n_muestra = nrow(datos),
    
    n_ocupacion = n_denominador(
      datos,
      "ocupado"
    ),
    
    n_paro = n_denominador(
      datos,
      "parado_oit",
      "activo_oit"
    ),
    
    n_temporalidad = n_denominador(
      datos,
      "contrato_temporal",
      "asalariado"
    ),
    
    n_parcialidad = n_denominador(
      datos,
      "jornada_parcial",
      "ocupado"
    ),
    
    n_parcialidad_involuntaria = n_denominador(
      datos,
      "parcialidad_involuntaria",
      "jornada_parcial"
    ),
    
    n_inactividad = n_denominador(
      datos,
      "inactivo_oit"
    ),
    
    tasa_ocupacion = tasa_ponderada(
      datos,
      "ocupado"
    ),
    
    tasa_paro = tasa_ponderada(
      datos,
      "parado_oit",
      "activo_oit"
    ),
    
    tasa_temporalidad = tasa_ponderada(
      datos,
      "contrato_temporal",
      "asalariado"
    ),
    
    tasa_parcialidad = tasa_ponderada(
      datos,
      "jornada_parcial",
      "ocupado"
    ),
    
    tasa_parcialidad_involuntaria = tasa_ponderada(
      datos,
      "parcialidad_involuntaria",
      "jornada_parcial"
    ),
    
    tasa_inactividad = tasa_ponderada(
      datos,
      "inactivo_oit"
    )
  )
}


# Funcion para resumir por grupos
resumir_por_grupos <- function(datos, variables) {
  
  datos %>%
    group_by(across(all_of(variables))) %>%
    group_modify(~ calcular_indicadores(.x)) %>%
    ungroup()
}


# ------------------------------------------------------------------------------
# 4. EVOLUCION SEGUN NIVEL EDUCATIVO
# ------------------------------------------------------------------------------

resultados_educacion <- resumir_por_grupos(
  base %>%
    filter(!is.na(educacion_4_niveles)),
  c("periodo", "educacion_4_niveles")
)

write_csv(
  resultados_educacion,
  "output/fase_4b/tablas/01_evolucion_educacion.csv"
)


# ------------------------------------------------------------------------------
# 5. EVOLUCION SEGUN SEXO
# ------------------------------------------------------------------------------

resultados_sexo <- resumir_por_grupos(
  base %>%
    filter(!is.na(sexo)),
  c("periodo", "sexo")
)

write_csv(
  resultados_sexo,
  "output/fase_4b/tablas/02_evolucion_sexo.csv"
)


# ------------------------------------------------------------------------------
# 6. EDUCACION Y SEXO: EVOLUCION CONJUNTA
# ------------------------------------------------------------------------------

resultados_educacion_sexo <- resumir_por_grupos(
  base %>%
    filter(
      !is.na(educacion_4_niveles),
      !is.na(sexo)
    ),
  c("periodo", "educacion_4_niveles", "sexo")
)

write_csv(
  resultados_educacion_sexo,
  "output/fase_4b/tablas/03_evolucion_educacion_sexo.csv"
)


# ------------------------------------------------------------------------------
# 7. COMPARACIONES INTERANUALES
# ------------------------------------------------------------------------------

# Compara cada trimestre con el mismo trimestre del ano anterior.
# Las diferencias se expresan en puntos porcentuales.

comparacion_interanual <- function(datos, grupos = character(0)) {
  
  datos %>%
    group_by(
      across(all_of(c(grupos, "trimestre")))
    ) %>%
    arrange(anio, .by_group = TRUE) %>%
    mutate(
      across(
        starts_with("tasa_"),
        ~ .x - lag(.x),
        .names = "variacion_interanual_{.col}"
      )
    ) %>%
    ungroup()
}


# 7.1. Comparacion nacional
resultados_nacionales <- resumir_por_grupos(
  base,
  "periodo"
) %>%
  mutate(
    anio = as.integer(substr(as.character(periodo), 1, 4)),
    trimestre = substr(as.character(periodo), 6, 6)
  )

interanual_nacional <- comparacion_interanual(
  resultados_nacionales
)

write_csv(
  interanual_nacional,
  "output/fase_4b/tablas/04_comparacion_interanual_nacional.csv"
)


# 7.2. Comparacion interanual por educacion
educacion_interanual <- resultados_educacion %>%
  mutate(
    anio = as.integer(substr(as.character(periodo), 1, 4)),
    trimestre = substr(as.character(periodo), 6, 6)
  )

interanual_educacion <- comparacion_interanual(
  educacion_interanual,
  "educacion_4_niveles"
)

write_csv(
  interanual_educacion,
  "output/fase_4b/tablas/05_comparacion_interanual_educacion.csv"
)


# 7.3. Comparacion interanual por sexo
sexo_interanual <- resultados_sexo %>%
  mutate(
    anio = as.integer(substr(as.character(periodo), 1, 4)),
    trimestre = substr(as.character(periodo), 6, 6)
  )

interanual_sexo <- comparacion_interanual(
  sexo_interanual,
  "sexo"
)

write_csv(
  interanual_sexo,
  "output/fase_4b/tablas/06_comparacion_interanual_sexo.csv"
)


# ------------------------------------------------------------------------------
# 8. ANALISIS DEL ULTIMO TRIMESTRE
# ------------------------------------------------------------------------------

ultimo_periodo <- tail(periodos, 1)

base_ultimo <- base %>%
  filter(as.character(periodo) == ultimo_periodo)

# Educacion y sexo
resultados_ultimo_educacion_sexo <- resumir_por_grupos(
  base_ultimo %>%
    filter(
      !is.na(educacion_4_niveles),
      !is.na(sexo)
    ),
  c("educacion_4_niveles", "sexo")
)

write_csv(
  resultados_ultimo_educacion_sexo,
  "output/fase_4b/tablas/07_educacion_sexo_ultimo_trimestre.csv"
)

# Sexo
resultados_ultimo_sexo <- resumir_por_grupos(
  base_ultimo %>%
    filter(!is.na(sexo)),
  "sexo"
)

write_csv(
  resultados_ultimo_sexo,
  "output/fase_4b/tablas/08_sexo_ultimo_trimestre.csv"
)


# ------------------------------------------------------------------------------
# 9. CONTROL DE TAMANOS MUESTRALES
# ------------------------------------------------------------------------------

control_tamanos <- resultados_educacion_sexo %>%
  mutate(
    grupo_muestral = case_when(
      n_muestra < 100 ~ "Muy pequeno: menos de 100",
      n_muestra < 300 ~ "Pequeno: 100-299",
      n_muestra < 500 ~ "Reducido: 300-499",
      TRUE ~ "Mayor: 500 o mas"
    )
  )

write_csv(
  control_tamanos,
  "output/fase_4b/tablas/09_control_tamanos_muestrales.csv"
)


# ------------------------------------------------------------------------------
# 10. GRAFICOS
# ------------------------------------------------------------------------------

# 10.1. Ocupacion segun educacion

grafico_ocupacion_educacion <- ggplot(
  resultados_educacion,
  aes(
    x = periodo,
    y = tasa_ocupacion,
    color = educacion_4_niveles,
    group = educacion_4_niveles
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Evolucion de la ocupacion segun nivel educativo",
    subtitle = "Poblacion de 25-34 anos, Espana, 2021T1-2026T3",
    x = "Trimestre",
    y = "Tasa de ocupacion",
    color = "Nivel educativo",
    caption = "Fuente: elaboracion propia a partir de microdatos EPA. Estimaciones ponderadas."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "output/fase_4b/graficos/01_ocupacion_educacion.png",
  grafico_ocupacion_educacion,
  width = 12,
  height = 6,
  dpi = 300
)


# 10.2. Temporalidad segun educacion

grafico_temporalidad_educacion <- ggplot(
  resultados_educacion,
  aes(
    x = periodo,
    y = tasa_temporalidad,
    color = educacion_4_niveles,
    group = educacion_4_niveles
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Evolucion de la temporalidad segun nivel educativo",
    subtitle = "Poblacion asalariada de 25-34 anos, Espana",
    x = "Trimestre",
    y = "Tasa de temporalidad",
    color = "Nivel educativo",
    caption = "Fuente: elaboracion propia a partir de microdatos EPA. Estimaciones ponderadas."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "output/fase_4b/graficos/02_temporalidad_educacion.png",
  grafico_temporalidad_educacion,
  width = 12,
  height = 6,
  dpi = 300
)


# 10.3. Ocupacion segun sexo

grafico_ocupacion_sexo <- ggplot(
  resultados_sexo,
  aes(
    x = periodo,
    y = tasa_ocupacion,
    color = sexo,
    group = sexo
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Evolucion de la ocupacion segun sexo",
    subtitle = "Poblacion de 25-34 anos, Espana",
    x = "Trimestre",
    y = "Tasa de ocupacion",
    color = "Sexo",
    caption = "Fuente: elaboracion propia a partir de microdatos EPA. Estimaciones ponderadas."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "output/fase_4b/graficos/03_ocupacion_sexo.png",
  grafico_ocupacion_sexo,
  width = 12,
  height = 6,
  dpi = 300
)


# 10.4. Inactividad segun educacion y sexo

grafico_inactividad <- ggplot(
  resultados_educacion_sexo,
  aes(
    x = periodo,
    y = tasa_inactividad,
    color = sexo,
    group = sexo
  )
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ educacion_4_niveles) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Evolucion de la inactividad por educacion y sexo",
    subtitle = "Poblacion de 25-34 anos, Espana",
    x = "Trimestre",
    y = "Tasa de inactividad",
    color = "Sexo",
    caption = "Fuente: elaboracion propia a partir de microdatos EPA. Estimaciones ponderadas."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "output/fase_4b/graficos/04_inactividad_educacion_sexo.png",
  grafico_inactividad,
  width = 13,
  height = 8,
  dpi = 300
)


# 10.5. Diferencias interanuales de ocupacion por educacion

grafico_variacion_educacion <- ggplot(
  interanual_educacion,
  aes(
    x = periodo,
    y = variacion_interanual_tasa_ocupacion,
    color = educacion_4_niveles,
    group = educacion_4_niveles
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1)
  ) +
  labs(
    title = "Variacion interanual de la ocupacion por educacion",
    subtitle = "Diferencia respecto al mismo trimestre del ano anterior",
    x = "Trimestre",
    y = "Variacion interanual",
    color = "Nivel educativo",
    caption = "Fuente: elaboracion propia a partir de microdatos EPA. Diferencias en puntos porcentuales."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  "output/fase_4b/graficos/05_variacion_interanual_educacion.png",
  grafico_variacion_educacion,
  width = 12,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 11. RESUMEN DE RESULTADOS
# ------------------------------------------------------------------------------

cat("\n============================================\n")
cat("FASE 4B: ANALISIS COMPLETADO\n")
cat("============================================\n")

cat("\n--- EDUCACION Y SEXO: ULTIMO TRIMESTRE ---\n")

print(
  resultados_ultimo_educacion_sexo %>%
    mutate(
      across(
        starts_with("tasa_"),
        ~ round(.x * 100, 2)
      )
    ),
  n = Inf,
  width = Inf
)

cat("\n--- INACTIVIDAD POR SEXO: ULTIMO TRIMESTRE ---\n")

print(
  resultados_ultimo_sexo %>%
    mutate(
      across(
        starts_with("tasa_"),
        ~ round(.x * 100, 2)
      )
    ),
  n = Inf,
  width = Inf
)

cat("\n--- CONTROL DE TAMANOS MUESTRALES ---\n")

print(
  control_tamanos %>%
    filter(
      as.character(periodo) == ultimo_periodo
    ) %>%
    select(
      periodo,
      educacion_4_niveles,
      sexo,
      n_muestra,
      grupo_muestral
    ),
  n = Inf
)

cat("\nArchivos guardados en:\n")
cat("- output/fase_4b/tablas/\n")
cat("- output/fase_4b/graficos/\n")

cat("\nLas tasas son ponderadas. Los tamanos muestrales son recuentos sin ponderar.\n")
cat("No se han calculado errores estandar ni intervalos de confianza.\n")
