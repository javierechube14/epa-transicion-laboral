# Educación y desigualdad laboral entre jóvenes adultos en España (2021–2026)

Análisis reproducible en R y Quarto sobre la relación entre nivel educativo, ocupación, sexo y territorio a partir de los microdatos de la Encuesta de Población Activa (EPA).

🔗 **[Ver informe interactivo completo en GitHub Pages](https://javierechube14.github.io/epa-transicion-laboral/)**

---

## 1. Descripción del proyecto

Este proyecto analiza la relación entre educación y situación laboral entre la población de **25 a 34 años** en España, utilizando microdatos trimestrales de la **Encuesta de Población Activa (EPA)** correspondientes al periodo **2021T1–2026T3**.

El objetivo es estudiar hasta qué punto el nivel educativo se asocia con la probabilidad de estar ocupado y cómo varía esta relación en función del sexo, la comunidad autónoma y el periodo temporal. El trabajo combina herramientas de análisis estadístico con una perspectiva sociológica de la desigualdad: no se limita a describir las diferencias entre grupos educativos, sino que incorpora modelos multivariables con términos de interacción para examinar si esas brechas se mantienen al controlar otras características sociodemográficas y territoriales.

El resultado es un flujo de trabajo reproducible elaborado con **R** y **Quarto**, acompañado de visualizaciones, tablas estadísticas e informe web compilado en HTML.

---

## 2. Pregunta de investigación y objetivos

> **¿En qué medida se asocia el nivel educativo con la ocupación de la población de 25 a 34 años y cómo varía esta relación según el sexo y el territorio?**

### Objetivos específicos

- Examinar la evolución reciente de la ocupación, el desempleo, la temporalidad y la parcialidad laboral juvenil entre 2021 y 2026.
- Analizar las diferencias en la situación laboral según el nivel educativo.
- Estudiar las desigualdades laborales entre hombres y mujeres.
- Comparar las diferencias territoriales entre comunidades autónomas.
- Estimar la asociación neta entre educación y ocupación mediante modelos de regresión logística ponderados.
- Comprobar mediante términos de interacción si la relación entre educación y ocupación presenta distinta intensidad según el sexo.

---

## 3. Datos y fuentes

La investigación utiliza los microdatos oficiales de la **Encuesta de Población Activa (EPA)**, elaborada por el Instituto Nacional de Estadística (INE).

| Característica | Descripción |
| :--- | :--- |
| **Fuente** | Encuesta de Población Activa (INE) |
| **Población analizada** | Personas de 25 a 34 años (grupos quinquenales 25–29 y 30–34) |
| **Cobertura geográfica** | España (17 comunidades autónomas y 2 ciudades autónomas) |
| **Periodo** | 2021T1–2026T3 (23 trimestres) |
| **Diseño** | Análisis de secciones transversales repetidas |
| **Observaciones** | 226.048 registros individuales |
| **Unidad de análisis** | Individuo |
| **Ponderación** | Factor de elevación de la EPA (`FACTOREL`) |

La base analítica reúne 23 trimestres de microdatos. Al tratarse de cortes transversales repetidos, el estudio no sigue longitudinalmente a las mismas personas ni reconstruye trayectorias laborales individuales.

---

## 4. Metodología

El análisis se estructura en dos bloques complementarios:

### 4.1. Análisis descriptivo

Se calculan indicadores laborales para el conjunto de la población de 25 a 34 años y se comparan por nivel educativo, sexo, comunidad autónoma y trimestre (incorporando variaciones interanuales para neutralizar la estacionalidad):

- **Ocupación:** proporción de personas ocupadas sobre el total de la población analizada.
- **Desempleo:** proporción de personas paradas según la clasificación oficial de la encuesta.
- **Temporalidad:** proporción de personas asalariadas con contrato temporal.
- **Parcialidad:** proporción de personas ocupadas que trabajan a tiempo parcial.
- **Parcialidad involuntaria:** proporción de personas ocupadas a tiempo parcial que declaran no haber podido encontrar un empleo a jornada completa.

### 4.2. Modelos multivariables

Se estiman cuatro modelos de regresión logística con familia `quasibinomial` y ponderación mediante los factores de elevación normalizados de la EPA. La variable dependiente es la situación de ocupación (`1` = persona ocupada; `0` = persona no ocupada, incluyendo desempleo e inactividad).

La variable educativa armonizada se organiza en cuatro categorías:

1. **Bajo:** educación primaria o inferior.
2. **Medio-bajo:** primera etapa de educación secundaria.
3. **Medio-alto:** segunda etapa de educación secundaria, general o profesional.
4. **Alto:** educación superior.

Se construyen cuatro especificaciones progresivas:

| Modelo | Variables explicativas incorporadas |
| :--- | :--- |
| **M1** | Nivel educativo |
| **M2** | Educación, sexo y grupo de edad |
| **M3** | Educación, sexo, grupo de edad, comunidad autónoma y periodo |
| **M4** | Variables del M3 + interacción entre educación y sexo (`educacion * sexo`) |

A partir del modelo principal (**M4**), se calculan **probabilidades predichas estandarizadas** (con intervalos de confianza aproximados mediante el método delta) y razones de ventaja (*odds ratios*).

---

## 5. Principales resultados

- **Mejora agregada de la ocupación:** la tasa de ocupación de la población de 25 a 34 años aumenta del **67,75 %** en 2021T1 al **77,66 %** en 2026T3, mientras que el paro desciende del **20,37 %** al **11,03 %** y la temporalidad cae del **37,41 %** al **20,72 %**.
- **Fuerte gradiente educativo:** en 2026T3, la ocupación alcanza el **83,95 %** entre quienes tienen educación superior, frente al **41,67 %** entre quienes cuentan con educación primaria o inferior. En el modelo ajustado M3, tener estudios superiores multiplica por **6,22** las *odds* de estar ocupado respecto al nivel bajo.
- **Interacción significativa entre educación y sexo:** el contraste de desviancia entre M3 y M4 ($F = 266,46$, $p < 0,001$) confirma que el rendimiento ocupacional de la educación difiere según el sexo.
- **Convergencia de género en niveles superiores:** las probabilidades predichas estandarizadas muestran que la brecha de ocupación entre hombres y mujeres se reduce en **23,08 puntos porcentuales** a medida que aumenta el nivel formativo, pasando de **25,83 puntos** en el nivel bajo ($55,73\ \%$ vs. $29,90\ \%$) a apenas **2,75 puntos** en el nivel alto ($83,22\ \%$ vs. $80,47\ \%$).
- **Heterogeneidad territorial:** se observan diferencias acusadas entre comunidades autónomas (con máximos en Illes Balears y La Rioja y mínimos en Ceuta y Melilla), condicionadas por la estructura productiva y los tamaños muestrales de cada territorio.

---

## 6. Tecnologías y herramientas

- **R:** procesamiento, limpieza, armonización y modelización estadística.
- **Tidyverse:** manipulación y transformación de datos (`dplyr`, `tidyr`, `readr`, `stringr`).
- **ggplot2:** visualización de tendencias temporales, brechas de género y probabilidades predichas.
- **Quarto:** generación del informe científico reproducible en HTML (`informe_epa.qmd`)[cite: 3, 5].
- **GLM (`quasibinomial`):** estimación de modelos logísticos ponderados y contraste de interacciones.

---

## 7. Estructura del repositorio

```text
epa-transicion-laboral/
│
├── data-raw/                     # Microdatos trimestrales originales de la EPA (ignorados en Git)
├── data-clean/                   # Bases consolidadas y armonizadas en .rds/.parquet (ignoradas en Git)
│
├── docs/
│   └── index.html                # Informe HTML compilado para GitHub Pages
│
├── output/
│   ├── graficos/                 # Visualizaciones descriptivas nacionales y educativas
│   ├── fase_4b/                  # Tablas y gráficos comparativos interanuales y territoriales
│   └── fase_5/                   # Tablas de coeficientes, probabilidades predichas, brechas y gráficos GLM
│
├── scripts/
│   ├── 01_auditoria_microdatos.R
│   ├── 02_construccion_base.R
│   ├── 03_operacionalizacion.R
│   ├── 04_analisis_descriptivo.R
│   └── 05_modelos_multivariables.R
## 8. Reproducibilidad

El flujo de análisis está modularizado en scripts secuenciales dentro de `scripts/` y consolidado en `informe_epa.qmd`[cite: 5]:

1. **Auditoría e integración:** lectura y validación de los 23 ficheros trimestrales de la EPA (`2021T1–2026T3`)[cite: 5].
2. **Construcción y operacionalización:** armonización de niveles educativos, situación laboral, contrato, jornada y factores de elevación.
3. **Análisis descriptivo y comparativo:** cálculo de tasas ponderadas, variaciones interanuales y desagregaciones por sexo y CCAA.
4. **Modelización multivariable:** estimación de los modelos `M1`–`M4`, contraste $F$ de interacción y cálculo de probabilidades predichas con intervalos de confianza.
5. **Compilación del informe:** renderizado de `informe_epa.qmd` con Quarto a partir de los resultados exportados en `output/`[cite: 5].

Para reproducir los cálculos desde cero, basta con abrir `epa-transicion-laboral.Rproj` en RStudio[cite: 5], descargar los microdatos trimestrales desde la web del INE en `data-raw/` y ejecutar los scripts en orden numérico.

---

## 9. Limitaciones metodológicas

- **Diseño transversal repetido:** la EPA no permite seguir longitudinalmente a los mismos individuos ni reconstruir transiciones laborales personales.
- **Definición de no ocupación:** la variable dependiente agrupa bajo la categoría `0` tanto a personas desempleadas como inactivas, por lo que mide la probabilidad global de estar ocupado y no exclusivamente el acceso al empleo entre la población activa.
- **Diseño muestral complejo:** aunque se emplean los factores de elevación de la encuesta, los modelos no reproducen íntegramente la estratificación y conglomeración del diseño muestral complejo; los errores estándar e intervalos de confianza deben interpretarse como aproximaciones del modelo.
- **Tamaños muestrales territoriales:** las estimaciones correspondientes a territorios con muestras reducidas (especialmente Ceuta y Melilla) presentan mayor incertidumbre estadística.
- **Alcance asociativo:** los resultados expresan asociaciones estadísticas ajustadas y no relaciones causales estrictas.

---

## 10. Referencias y autoría

- **Autor:** Javier Echube Bestué — Grado en Sociología, Universitat de València.
- **Fuente de datos:** Instituto Nacional de Estadística (INE). *Encuesta de Población Activa (EPA): metodología, diseño de registro y microdatos trimestrales (2021T1–2026T3)*[cite: 5].
- **Software:** R Core Team (*R: A Language and Environment for Statistical Computing*) y Posit (*Quarto*).
