
# ==============================================================================
# FASE 4: ANÁLISIS DESCRIPTIVO PONDERADO - EPA JÓVENES 25-34
# Periodo: 2021T1 - 2026T3
# ==============================================================================

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. CONFIGURACIÓN Y CARGA DE DATOS
# ------------------------------------------------------------------------------

ruta_base <- "output/base/epa_jovenes_25_34_analitica.rds"

if (!file.exists(ruta_base)) {
  stop("No se encuentra la base analítica. Comprueba la ruta: ", ruta_base)
}

base <- readRDS(ruta_base)

# Variables necesarias para el análisis
variables_necesarias <- c(
  "periodo",
  "CCAA",
  "sexo",
  "educacion_4_niveles",
  "factor_elevacion",
  "ocupado",
  "activo_oit",
  "parado_oit",
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
    "Faltan estas variables en la base analítica: ",
    paste(variables_faltantes, collapse = ", "),
    ". Revisa la Fase 3 antes de continuar."
  )
}

# Comprobaciones básicas
if (any(is.na(base$factor_elevacion)) ||
    any(base$factor_elevacion <= 0)) {
  stop("Se han detectado factores de elevación ausentes o no positivos.")
}

# Orden cronológico de los periodos
base <- base %>%
  mutate(
    periodo = factor(
      periodo,
      levels = sort(unique(as.character(periodo))),
      ordered = TRUE
    )
  )

# Último trimestre disponible
periodo_final <- tail(levels(base$periodo), 1)

cat("\nFASE 4: INICIO DEL ANÁLISIS\n")
cat("Observaciones:", nrow(base), "\n")
cat("Periodos:", nlevels(base$periodo), "\n")
cat("Último periodo:", periodo_final, "\n")


# ------------------------------------------------------------------------------
# 2. FUNCIÓN PARA CALCULAR TASAS PONDERADAS
# ------------------------------------------------------------------------------

# La función calcula una proporción ponderada.
# Si existe denominador, restringe el cálculo a ese universo.

tasa_ponderada <- function(datos, numerador, denominador = NULL) {
  
  x <- datos[[numerador]]
  peso <- datos$factor_elevacion
  
  validos <- !is.na(x) &
    !is.na(peso) &
    peso > 0
  
  if (!is.null(denominador)) {
    d <- datos[[denominador]]
    validos <- validos & !is.na(d) & d == 1
  }
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  sum(peso[validos] * (x[validos] == 1)) /
    sum(peso[validos])
}


# ------------------------------------------------------------------------------
# 3. FUNCIÓN PARA RESUMIR LOS CINCO INDICADORES
# ------------------------------------------------------------------------------

calcular_indicadores <- function(datos) {
  
  tibble(
    n_muestra = nrow(datos),
    
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
    )
  )
}


# Función para aplicar el cálculo a distintos grupos
resumir_por_grupos <- function(datos, variables) {
  
  datos %>%
    group_by(across(all_of(variables))) %>%
    group_modify(~ calcular_indicadores(.x)) %>%
    ungroup()
}


# ------------------------------------------------------------------------------
# 4. ANÁLISIS NACIONAL: EVOLUCIÓN TRIMESTRAL
# ------------------------------------------------------------------------------

resultados_nacionales <- resumir_por_grupos(
  base,
  c("periodo")
)

write_csv(
  resultados_nacionales,
  "output/auditoria/analisis_nacional_ponderado.csv"
)


# ------------------------------------------------------------------------------
# 5. ANÁLISIS POR NIVEL EDUCATIVO
# ------------------------------------------------------------------------------

resultados_educacion <- resumir_por_grupos(
  base,
  c("periodo", "educacion_4_niveles")
)

write_csv(
  resultados_educacion,
  "output/auditoria/analisis_educacion_ponderado.csv"
)


# ------------------------------------------------------------------------------
# 6. ANÁLISIS POR SEXO
# ------------------------------------------------------------------------------

resultados_sexo <- resumir_por_grupos(
  base,
  c("periodo", "sexo")
)

write_csv(
  resultados_sexo,
  "output/auditoria/analisis_sexo_ponderado.csv"
)


# ------------------------------------------------------------------------------
# 7. ANÁLISIS TERRITORIAL: ÚLTIMO TRIMESTRE
# ------------------------------------------------------------------------------

base_ultimo_periodo <- base %>%
  filter(as.character(periodo) == periodo_final)

resultados_ccaa <- resumir_por_grupos(
  base_ultimo_periodo,
  c("CCAA")
)

write_csv(
  resultados_ccaa,
  "output/auditoria/analisis_ccaa_ponderado.csv"
)


# ------------------------------------------------------------------------------
# 8. ANÁLISIS EDUCATIVO POR SEXO: ÚLTIMO TRIMESTRE
# ------------------------------------------------------------------------------

resultados_educacion_sexo <- resumir_por_grupos(
  base_ultimo_periodo,
  c("educacion_4_niveles", "sexo")
)

write_csv(
  resultados_educacion_sexo,
  "output/auditoria/analisis_educacion_sexo_ultimo_periodo.csv"
)


# ------------------------------------------------------------------------------
# 9. GRÁFICOS
# ------------------------------------------------------------------------------

dir.create(
  "output/graficos",
  recursive = TRUE,
  showWarnings = FALSE
)

# 9.1. Evolución nacional de la ocupación
grafico_ocupacion <- ggplot(
  resultados_nacionales,
  aes(
    x = periodo,
    y = tasa_ocupacion
  )
) +
  geom_line(aes(group = 1), linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Evolución de la tasa de ocupación",
    subtitle = "Población de 25-34 años, España, 2021T1-2026T3",
    x = "Trimestre",
    y = "Tasa de ocupación",
    caption = "Fuente: elaboración propia a partir de microdatos EPA. Estimaciones ponderadas."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "output/graficos/01_evolucion_ocupacion.png",
  grafico_ocupacion,
  width = 11,
  height = 6,
  dpi = 300
)


# 9.2. Ocupación según nivel educativo
grafico_educacion <- ggplot(
  resultados_educacion,
  aes(
    x = periodo,
    y = tasa_ocupacion,
    color = educacion_4_niveles,
    group = educacion_4_niveles
  )
) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Tasa de ocupación según nivel educativo",
    subtitle = "Población de 25-34 años, España, 2021T1-2026T3",
    x = "Trimestre",
    y = "Tasa de ocupación",
    color = "Nivel educativo",
    caption = "Fuente: elaboración propia a partir de microdatos EPA. Estimaciones ponderadas."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "output/graficos/02_ocupacion_educacion.png",
  grafico_educacion,
  width = 11,
  height = 6,
  dpi = 300
)


# 9.3. Temporalidad según nivel educativo
grafico_temporalidad <- ggplot(
  resultados_educacion,
  aes(
    x = periodo,
    y = tasa_temporalidad,
    color = educacion_4_niveles,
    group = educacion_4_niveles
  )
) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Tasa de temporalidad según nivel educativo",
    subtitle = "Población asalariada de 25-34 años, España",
    x = "Trimestre",
    y = "Tasa de temporalidad",
    color = "Nivel educativo",
    caption = "Fuente: elaboración propia a partir de microdatos EPA. Estimaciones ponderadas."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  "output/graficos/03_temporalidad_educacion.png",
  grafico_temporalidad,
  width = 11,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 10. RESUMEN DE RESULTADOS EN CONSOLA
# ------------------------------------------------------------------------------

cat("\n============================================\n")
cat("FASE 4: ANÁLISIS DESCRIPTIVO COMPLETADO\n")
cat("============================================\n")

cat("\n--- RESULTADOS NACIONALES ---\n")
print(
  resultados_nacionales %>%
    mutate(
      across(
        starts_with("tasa_"),
        ~ round(.x * 100, 2)
      )
    ),
  n = Inf
)

cat("\n--- RESULTADOS POR EDUCACIÓN: ÚLTIMO TRIMESTRE ---\n")
print(
  resultados_educacion %>%
    filter(as.character(periodo) == periodo_final) %>%
    mutate(
      across(
        starts_with("tasa_"),
        ~ round(.x * 100, 2)
      )
    ),
  n = Inf
)

cat("\n--- RESULTADOS POR CCAA: ÚLTIMO TRIMESTRE ---\n")
print(
  resultados_ccaa %>%
    mutate(
      across(
        starts_with("tasa_"),
        ~ round(.x * 100, 2)
      )
    ),
  n = Inf
)

cat("\nArchivos generados en output/auditoria y output/graficos.\n")
cat("Recuerda: las tasas están ponderadas; n_muestra es un recuento sin ponderar.\n")
