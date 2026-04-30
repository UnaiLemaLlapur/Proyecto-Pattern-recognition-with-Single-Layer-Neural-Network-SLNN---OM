library(dplyr)
library(ggplot2)
library(tidyr)

# Cargar los datos
df <- read.csv("uo_nn_batch.csv", sep=";", header = TRUE, strip.white=TRUE)
datos_slnn <- df[, 1:10]
# Renombrar columnas para evitar problemas con los espacios del CSV original
colnames(datos_slnn) <- c("num_target", "la", "isd", "iter/epoc", "mbatch", "L_star", "ngL*", "tex", "tr_acc", "te_acc")

# Transformaciones clave
datos_clean <- datos_slnn %>%
  mutate(
    # Asignar nombres a los algoritmos
    Method = case_when(
      isd == 1 ~ "GM",
      isd == 3 ~ "QNM",
      isd == 7 ~ "SGM"
    ),
    Method = factor(Method, levels = c("GM", "QNM", "SGM")),
    
    # Calcular el número real de iteraciones (SGM usa mbatch, los demás iter_epoc)
    niter = ifelse(isd == 7, mbatch, `iter/epoc`),
    
    # Tiempo por iteración
    tex_niter = tex / niter,
    
    # Convertir dígitos y lambdas a factores para las gráficas
    Digit = as.factor(n),
    Lambda = as.factor(la)
  )

# Formato largo para comparar Training vs Test más fácilmente
datos_long <- datos_clean %>%
  pivot_longer(cols = c(tr_acc, te_acc), names_to = "Acc_Type", values_to = "Accuracy") %>%
  mutate(Acc_Type = factor(Acc_Type, levels = c("tr_acc", "te_acc"), labels = c("Training", "Test")))


# ==============================================================================
# GENERACIÓN DE GRÁFICOS PARA EL REPORTE
# ==============================================================================

# ---------------------------------------------------------
# GRÁFICO 1: Convergencia Global (L*) [Para apartado 1.a y 2.a]
# ---------------------------------------------------------
# Usamos escala logarítmica (log10) porque los valores de L* ahora serán muy pequeños.
# Separamos los 3 valores de Lambda en paneles (facet_wrap).
(p_Lstar <- ggplot(datos_clean, aes(x = Method, y = L_star, fill = Method)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~ Lambda, labeller = as_labeller(function(x) paste("Lambda =", x))) +
  scale_y_log10() + # Escala logarítmica vital para ver diferencias en L*
  theme_bw() +
  labs(title = "Convergencia Global: Valor Final de la Función Objetivo (L*)",
       subtitle = "Comparativa por algoritmo y nivel de regularización",
       x = "Algoritmo",
       y = "Valor de L* (escala logarítmica)",
       fill = "Algoritmo"))

#ggsave("Plot_1_Lstar_Global.pdf", p_Lstar, width = 10, height = 5)


# ---------------------------------------------------------
# GRÁFICO 2: Eficiencia Local (Tiempo por Iteración) [Para apartado 1.b]
# ---------------------------------------------------------
# También en escala logarítmica para poder ver la cajita del SGM.
(p_eficiencia <- ggplot(datos_clean %>% filter(la == 0), aes(x = Method, y = tex_niter, fill = Method)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Velocidad Local: Tiempo de ejecución por Iteración (Lambda = 0)",
       x = "Algoritmo",
       y = "Segundos por Iteración (tex / niter) [Escala Log]",
       fill = "Algoritmo"))

#ggsave("Plot_2_Eficiencia_L0.pdf", p_eficiencia, width = 8, height = 5)


# ---------------------------------------------------------
# GRÁFICO 3: Precisión por Dígito (Lambda = 0) [Para apartado 1.c]
# ---------------------------------------------------------
# Comparamos Training vs Test para cada número para ver si la red sufre con el 8 o el 9.
(p_digitos <- ggplot(datos_long %>% filter(la == 0), aes(x = Digit, y = Accuracy, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ Acc_Type) +
  theme_bw() +
  coord_cartesian(ylim = c(60, 100)) + # Hacemos "zoom" para no ver las barras desde 0
  labs(title = "Dependencia del Dígito: Precisión sin Regularización (Lambda = 0)",
       subtitle = "Evaluación de la dificultad intrínseca de cada número",
       x = "Dígito Objetivo",
       y = "Precisión / Accuracy (%)"))

#ggsave("Plot_3_Digitos_L0.pdf", p_digitos, width = 12, height = 5)


# ---------------------------------------------------------
# GRÁFICO 4: Efecto de Lambda en la Precisión [Para apartado 2.b]
# ---------------------------------------------------------
# Calculamos la media de todos los dígitos para ver el efecto general del sobreajuste
datos_medias <- datos_long %>%
  group_by(Method, Lambda, Acc_Type) %>%
  summarise(Mean_Accuracy = mean(Accuracy, na.rm = TRUE), .groups = 'drop')

(p_lambda_acc <- ggplot(datos_medias, aes(x = Lambda, y = Mean_Accuracy, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ Acc_Type) +
  theme_bw() +
  coord_cartesian(ylim = c(70, 100)) +
  labs(title = "Efecto de la Regularización en la Precisión Media",
       subtitle = "¿Ayuda Lambda > 0 a generalizar mejor (Test)?",
       x = "Valor de Lambda",
       y = "Precisión Media (%)"))

#ggsave("Plot_4_Efecto_Lambda.pdf", p_lambda_acc, width = 10, height = 5)