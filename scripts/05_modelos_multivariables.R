
# ==============================================================================
# FASE 5: MODELOS MULTIVARIABLES
# Proyecto: Transicion laboral de la poblacion de 25-34 anos (EPA, 2021-2026)
# ==============================================================================

library(tidyverse)

cat("\n============================================\n")
cat("FASE 5: INICIO DE LOS MODELOS MULTIVARIABLES\n")
cat("============================================\n\n")

# 1. CARGAR BASE ANALITICA -----------------------------------------------

ruta_base <- "output/base/epa_jovenes_25_34_analitica.rds"

if (!file.exists(ruta_base)) {
  stop("No se encuentra la base analitica: ", ruta_base)
}

datos <- readRDS(ruta_base)

cat("Observaciones originales:", nrow(datos), "\n")

# 2. COMPROBAR VARIABLES -------------------------------------------------

variables_necesarias <- c(
  "ocupado",
  "educacion_4_niveles",
  "sexo",
  "grupo_edad",
  "CCAA",
  "periodo",
  "factor_elevacion"
)

faltantes <- setdiff(variables_necesarias, names(datos))

if (length(faltantes) > 0) {
  stop(
    "Faltan estas variables en la base: ",
    paste(faltantes, collapse = ", ")
  )
}

# 3. PREPARAR MUESTRA ----------------------------------------------------

datos_modelo <- datos %>%
  transmute(
    ocupado = as.numeric(ocupado),
    educacion = factor(
      as.character(educacion_4_niveles),
      levels = c("Bajo", "Medio-bajo", "Medio-alto", "Alto")
    ),
    sexo = relevel(factor(sexo), ref = "Hombre"),
    grupo_edad = factor(grupo_edad),
    CCAA = factor(CCAA),
    periodo = factor(as.character(periodo)),
    factor_elevacion = as.numeric(factor_elevacion)
  ) %>%
  filter(
    ocupado %in% c(0, 1),
    !is.na(factor_elevacion),
    factor_elevacion > 0
  ) %>%
  drop_na(
    ocupado,
    educacion,
    sexo,
    grupo_edad,
    CCAA,
    periodo
  )

if (nrow(datos_modelo) == 0) {
  stop("La muestra del modelo esta vacia.")
}

if (n_distinct(datos_modelo$ocupado) != 2) {
  stop("La variable ocupado no contiene las dos categorias 0 y 1.")
}

# Se normalizan los pesos para evitar trabajar con cifras innecesariamente
# grandes. Esto no cambia los coeficientes estimados del modelo.
datos_modelo <- datos_modelo %>%
  mutate(
    peso_modelo = factor_elevacion / mean(factor_elevacion)
  )

cat("Observaciones utilizadas:", nrow(datos_modelo), "\n")
cat("Personas ocupadas (sin ponderar):",
    sum(datos_modelo$ocupado == 1), "\n")
cat("Personas no ocupadas (sin ponderar):",
    sum(datos_modelo$ocupado == 0), "\n")
cat("Peso medio original:",
    round(mean(datos_modelo$factor_elevacion), 2), "\n")

# 4. AUDITORIA DE LA MUESTRA ---------------------------------------------

auditoria_muestra <- tibble(
  indicador = c(
    "Observaciones originales",
    "Observaciones utilizadas",
    "Observaciones excluidas",
    "Porcentaje conservado"
  ),
  valor = c(
    nrow(datos),
    nrow(datos_modelo),
    nrow(datos) - nrow(datos_modelo),
    round(100 * nrow(datos_modelo) / nrow(datos), 2)
  )
)

# 5. ESTIMAR LOS CUATRO MODELOS -----------------------------------------

# M1: asociacion entre educacion y ocupacion
modelo_1 <- glm(
  ocupado ~ educacion,
  data = datos_modelo,
  family = quasibinomial(),
  weights = peso_modelo
)

# M2: incorporacion de controles demograficos
modelo_2 <- glm(
  ocupado ~ educacion + sexo + grupo_edad,
  data = datos_modelo,
  family = quasibinomial(),
  weights = peso_modelo
)

# M3: incorporacion de territorio y periodo
modelo_3 <- glm(
  ocupado ~ educacion + sexo + grupo_edad + CCAA + periodo,
  data = datos_modelo,
  family = quasibinomial(),
  weights = peso_modelo
)

# M4: interaccion entre educacion y sexo
modelo_4 <- glm(
  ocupado ~ educacion * sexo + grupo_edad + CCAA + periodo,
  data = datos_modelo,
  family = quasibinomial(),
  weights = peso_modelo
)

modelos <- list(
  M1_educacion = modelo_1,
  M2_demografico = modelo_2,
  M3_territorial_temporal = modelo_3,
  M4_interaccion_sexo = modelo_4
)


# 6. EXTRAER COEFICIENTES Y ODDS RATIOS -------------------------------

lista_resultados <- list()

for (nombre in names(modelos)) {
  
  ajuste <- modelos[[nombre]]
  
  tabla <- as.data.frame(
    summary(ajuste)$coefficients
  )
  
  tabla$termino <- rownames(tabla)
  rownames(tabla) <- NULL
  
  names(tabla)[1:2] <- c(
    "coeficiente",
    "error_estandar"
  )
  
  tabla$modelo <- nombre
  tabla$odds_ratio <- exp(tabla$coeficiente)
  
  lista_resultados[[nombre]] <- tabla %>%
    dplyr::select(modelo, termino, coeficiente, error_estandar, odds_ratio)
}

resultados_modelos <- bind_rows(lista_resultados)

# 7. RESUMEN GENERAL DE LOS MODELOS ------------------------------------

resumen_modelos <- map_dfr(
  names(modelos),
  function(nombre) {
    mod <- modelos[[nombre]]
    tibble(
      modelo = nombre,
      observaciones = length(mod$fitted.values),
      AIC = NA_real_,
      deviance = deviance(mod),
      grados_libertad_residuales = df.residual(mod)
    )
  }
)

# 8. CREAR CARPETAS Y GUARDAR RESULTADOS -------------------------------

dir.create(
  "output/fase_5/tablas",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  auditoria_muestra,
  "output/fase_5/tablas/auditoria_muestra_modelos.csv"
)

write_csv(
  resultados_modelos,
  "output/fase_5/tablas/coefficientes_modelos.csv"
)

write_csv(
  resumen_modelos,
  "output/fase_5/tablas/resumen_modelos.csv"
)

saveRDS(
  modelos,
  "output/fase_5/modelos_logisticos.rds"
)

# 9. MOSTRAR RESULTADOS --------------------------------------------------

cat("\n============================================\n")
cat("RESUMEN DE LOS MODELOS\n")
cat("============================================\n")

print(resumen_modelos)

cat("\n============================================\n")
cat("COEFICIENTES DEL MODELO 3\n")
cat("============================================\n")

resultados_modelos %>%
  filter(modelo == "M3_territorial_temporal") %>%
  as_tibble() %>%
  print(n = Inf)

cat("\n============================================\n")
cat("INTERACCIONES DEL MODELO 4\n")
cat("============================================\n")

resultados_modelos %>%
  filter(
    modelo == "M4_interaccion_sexo",
    str_detect(termino, ":")
  ) %>%
  as_tibble() %>%
  print(n = Inf)

cat("\n============================================\n")
cat("FASE 5: MODELOS ESTIMADOS\n")
cat("============================================\n")

cat("Archivos guardados en output/fase_5/\n")

# 10. PROBABILIDADES PREDICHAS POR EDUCACION Y SEXO ----------------------

combinaciones <- expand_grid(
  educacion = levels(datos_modelo$educacion),
  sexo = levels(datos_modelo$sexo)
)

lista_predicciones <- list()

for (i in seq_len(nrow(combinaciones))) {
  
  nivel_educativo <- combinaciones$educacion[i]
  categoria_sexo <- combinaciones$sexo[i]
  
  nuevos_datos <- datos_modelo
  
  nuevos_datos$educacion <- factor(
    nivel_educativo,
    levels = levels(datos_modelo$educacion)
  )
  
  nuevos_datos$sexo <- factor(
    categoria_sexo,
    levels = levels(datos_modelo$sexo)
  )
  
  predicciones <- predict(
    modelo_4,
    newdata = nuevos_datos,
    type = "response"
  )
  
  probabilidad <- weighted.mean(
    predicciones,
    w = nuevos_datos$peso_modelo
  )
  
  lista_predicciones[[i]] <- tibble(
    educacion = nivel_educativo,
    sexo = categoria_sexo,
    probabilidad_ocupacion = probabilidad
  )
}

probabilidades_predichas <- bind_rows(lista_predicciones) %>%
  mutate(
    porcentaje_ocupacion = probabilidad_ocupacion * 100
  )

cat("\n============================================\n")
cat("PROBABILIDADES PREDICHAS: EDUCACION Y SEXO\n")
cat("============================================\n")

print(
  as.data.frame(probabilidades_predichas),
  row.names = FALSE
)

# 11. GRAFICO DE PROBABILIDADES PREDICHAS -------------------------------

grafico_probabilidades <- ggplot(
  probabilidades_predichas,
  aes(
    x = educacion,
    y = porcentaje_ocupacion,
    group = sexo,
    color = sexo
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Probabilidad predicha de ocupacion por educacion y sexo",
    subtitle = "Modelo logistico M4, ajustado por edad, CCAA y periodo",
    x = "Nivel educativo",
    y = "Probabilidad predicha de ocupacion (%)",
    color = "Sexo",
    caption = paste(
      "Predicciones estandarizadas sobre la distribucion observada",
      "de edad, territorio y periodo. Estimaciones ponderadas."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

ggsave(
  "output/fase_5/graficos/probabilidades_educacion_sexo.png",
  grafico_probabilidades,
  width = 10,
  height = 6,
  dpi = 300
)

write_csv(
  probabilidades_predichas,
  "output/fase_5/tablas/probabilidades_predichas.csv"
)

# ==============================================================================
# FASE 5B. VALIDACIÓN FINAL Y EXPORTACIÓN DE RESULTADOS
# ==============================================================================

# 1. Seleccionar el modelo principal
modelo_final <- modelos[["M4_interaccion_sexo"]]

# 2. Calcular probabilidades predichas e intervalos de confianza
V <- vcov(modelo_final)
b <- coef(modelo_final)

niveles_educacion <- c("Bajo", "Medio-bajo", "Medio-alto", "Alto")
niveles_sexo <- c("Hombre", "Mujer")

resultados_ic <- list()

for (edu in niveles_educacion) {
  for (sex in niveles_sexo) {
    
    nuevos_datos <- datos_modelo
    
    nuevos_datos$educacion <- factor(
      edu,
      levels = levels(datos_modelo$educacion)
    )
    
    nuevos_datos$sexo <- factor(
      sex,
      levels = levels(datos_modelo$sexo)
    )
    
    X <- model.matrix(
      delete.response(terms(modelo_final)),
      data = nuevos_datos,
      contrasts.arg = modelo_final$contrasts
    )
    
    X <- X[, names(b), drop = FALSE]
    
    p <- plogis(drop(X %*% b))
    
    w <- nuevos_datos$peso_modelo
    w <- w / sum(w)
    
    estimacion <- sum(w * p)
    
    gradiente <- drop(
      crossprod(X, w * p * (1 - p))
    )
    
    error_estandar <- sqrt(
      drop(t(gradiente) %*% V %*% gradiente)
    )
    
    resultados_ic[[paste(edu, sex, sep = "_")]] <- data.frame(
      educacion = edu,
      sexo = sex,
      probabilidad = estimacion * 100,
      limite_inferior = max(
        0, estimacion - 1.96 * error_estandar
      ) * 100,
      limite_superior = min(
        1, estimacion + 1.96 * error_estandar
      ) * 100
    )
    
    rm(X, p, nuevos_datos)
  }
  
  invisible(gc())
}

tabla_probabilidades_ic <- dplyr::bind_rows(resultados_ic)

# 3. Calcular brechas de ocupación entre hombres y mujeres (corregido)
tabla_brechas <- tabla_probabilidades_ic |>
  dplyr::select(educacion, sexo, probabilidad) |>
  tidyr::pivot_wider(
    names_from = sexo,
    values_from = probabilidad
  ) |>
  dplyr::mutate(
    brecha_hombres_mujeres = Hombre - Mujer
  )

# 4. Comparar el modelo sin interacción y el modelo con interacción
comparacion_modelos <- anova(
  modelos[["M3_territorial_temporal"]],
  modelos[["M4_interaccion_sexo"]],
  test = "F"
)

# 5. Exportar resultados
dir.create(
  "output/fase_5/tablas",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  tabla_probabilidades_ic,
  "output/fase_5/tablas/probabilidades_predichas_ic.csv",
  row.names = FALSE
)

write.csv(
  tabla_brechas,
  "output/fase_5/tablas/brechas_educacion_sexo.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(comparacion_modelos),
  "output/fase_5/tablas/comparacion_modelos.csv"
)

# 6. Mostrar resultados principales
cat("\nPROBABILIDADES PREDICHAS E INTERVALOS DE CONFIANZA\n")
print(
  as.data.frame(
    dplyr::mutate(
      tabla_probabilidades_ic,
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 2)
      )
    )
  ),
  row.names = FALSE
)

cat("\nBRECHAS DE OCUPACIÓN POR NIVEL EDUCATIVO\n")
print(
  as.data.frame(
    dplyr::mutate(
      tabla_brechas,
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 2)
      )
    )
  ),
  row.names = FALSE
)

cat("\nCOMPARACIÓN M3-M4\n")
print(comparacion_modelos)

cat("\nFASE 5 COMPLETADA Y RESULTADOS EXPORTADOS\n")