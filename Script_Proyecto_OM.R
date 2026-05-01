library(dplyr)
library(ggplot2)
library(tidyr)

# Cargar los datos
df <- read.csv("uo_nn_batch.csv", sep=";", header = TRUE, strip.white=TRUE)

# Renombrar columnas para evitar problemas con los espacios del CSV original
datos_slnn <- df %>% 
  mutate(num_target = n,lambda =la, iter_epoc=iter.epoc,L_star = L., ngL_star =ngL.) %>%
  select(num_target,lambda,isd,iter_epoc,mbatch,L_star,ngL_star,tex,tr_acc,te_acc) %>% 
  mutate(
    # Asignar nombres a los algoritmos
    method = case_when(
      isd == 1 ~ "GM",
      isd == 3 ~ "QNM",
      isd == 7 ~ "SGM"
    ),
    method = factor(method, levels = c("GM", "QNM", "SGM")),
    num_target = as.factor(num_target),
    lambda = as.factor(lambda),
    niter = case_when(
      isd < 7 ~ iter_epoc,
      isd >=7 ~ mbatch / iter_epoc,
    ),
    tex_niter = tex / niter,
    niter = as.integer(niter)) %>% 
  select(num_target,lambda,method,niter,tex_niter,L_star,ngL_star,tex,tr_acc,te_acc)

#mirar que hizo unai y arreglar gráficas
#mejorar graficas de unai y cambiar cosas por mi dataset, add grafico de NAN's para QNM + graficos en funcion de digito

# ---------------------------------------------------------
# GRÁFICO 1: Convergencia Global (L*) [Para apartado 1.a y 2.a]
# ---------------------------------------------------------
# Usamos escala logarítmica (log10) porque los valores de L* ahora serán muy pequeños.
# Separamos los 3 valores de Lambda en paneles (facet_wrap).

ggplot(datos_slnn, aes(x = method, y = L_star, fill = method)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~ lambda, labeller = as_labeller(function(x) paste("lambda =", x))) +
  scale_y_log10() + # Escala logarítmica vital para ver diferencias en L*
  theme_bw() +
  labs(title = "Convergencia Global: Valor Final de la Función Objetivo (L*)",
       subtitle = "Comparativa por algoritmo y nivel de regularización",
       x = "Algoritmo",
       y = "Valor de L* (escala logarítmica de 10)",
       fill = "Algoritmo")



# ---------------------------------------------------------
# GRÁFICO 2: Eficiencia Local (Tiempo por Iteración) [Para apartado 1.b]
# ---------------------------------------------------------
# También en escala logarítmica para poder ver la cajita del SGM.

ggplot(datos_slnn, aes(x = method, y = tex_niter, fill = method)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_log10() +
  facet_wrap(~ lambda, labeller = as_labeller(function(x) paste("lambda =", x))) +
  theme_minimal() +
  labs(title = "Velocidad Local: Tiempo de ejecución por Iteración en función del Algoritmo y valor de Lambda",
       x = "Algoritmo",
       y = "Segundos por Iteración (tex / niter) [Escala Log10]",
       fill = "Algoritmo")

#tamb se podría hacer uno para valroes de lambda concretos y uno general sin tener en cuenta lambda

# ---------------------------------------------------------
# GRÁFICO 3: Precisión por Dígito (Lambda = 0) [Para apartado 1.c]
# ---------------------------------------------------------
datos_train_test <- datos_slnn %>%
  pivot_longer(cols = c(tr_acc, te_acc), names_to = "acc_type", values_to = "accuracy") %>%
  mutate(acc_type = factor(acc_type, levels = c("tr_acc", "te_acc"), labels = c("Training", "Test")))

# Comparamos Training vs Test para cada número para ver si la red sufre con el 8 o el 9.
ggplot(datos_train_test %>% filter(lambda == 0,acc_type == "Training"), aes(x = num_target, y = accuracy, fill = method)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
   #facet_wrap(~ acc_type) +
   theme_bw() +
   coord_cartesian(ylim = c(90, 100)) + # Hacemos "zoom" para no ver las barras desde 0
   labs(title = "Dependencia del Dígito: Precisión sin Regularización (Lambda = 0) Training",
        subtitle = "Evaluación de la dificultad intrínseca de cada número",
        x = "Dígito Objetivo",
        y = "Accuracy Train (%)",
        fill= "Algoritmo") +
  scale_fill_brewer(palette = "Accent")

ggplot(datos_train_test %>% filter(lambda == 0,acc_type == "Test"), aes(x = num_target, y = accuracy, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  #facet_wrap(~ acc_type) +
  theme_bw() +
  coord_cartesian(ylim = c(90, 100)) + # Hacemos "zoom" para no ver las barras desde 0
  labs(title = "Dependencia del Dígito: Precisión sin Regularización (Lambda = 0) Test",
       subtitle = "Evaluación de la dificultad intrínseca de cada número",
       x = "Dígito Objetivo",
       y = "Accuracy Test(%)",
       fill= "Algoritmo") +
  scale_fill_brewer(palette = "Accent")


# ---------------------------------------------------------
# GRÁFICO 4: Efecto de Lambda en la Precisión [Para apartado 2.b]
# ---------------------------------------------------------
# Calculamos la media de todos los dígitos para ver el efecto general del sobreajuste
datos_medias <- datos_train_test %>%
  group_by(method, lambda, acc_type) %>%
  summarise(mean_accuracy = mean(accuracy, na.rm = TRUE), .groups = 'drop')

ggplot(datos_medias, aes(x = lambda, y = mean_accuracy, fill = method)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~ acc_type) +
    theme_bw() +
    coord_cartesian(ylim = c(70, 100)) +
    labs(title = "Efecto de la Regularización en la Accuracy Media",
         x = "Valor de Lambda",
         y = "Accuracy Media (%)")

# ---------------------------------------------------------
# GRÁFICO 5: Mas Efectos de Lambda: NA's 
# ---------------------------------------------------------

datos_na <- datos_slnn %>% mutate(
 has_na =case_when(
   rowSums(is.na(datos_slnn)) > 0 ~ 1,
   rowSums(is.na(datos_slnn)) <= 0 ~ 0
  )
) %>%
  select(lambda,method,has_na) %>% 
  group_by(method,lambda) %>% 
  summarise(n_missing = sum(has_na))


#Muestra cuantos missing values tienen los metodos en funcion de lambda
ggplot(datos_na, aes(x = method, y = n_missing, fill = method)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ lambda, labeller = as_labeller(function(x) paste("lambda =", x))) +
  theme_bw() +
  labs(title = "Efecto de Lambda en los Missing Values",
       x = "Algoritmo",
       y = "Número de Missing Values",
       fill = "Algoritmo")


datos_na_numbers <- datos_slnn %>% mutate(
  has_na =case_when(
    rowSums(is.na(datos_slnn)) > 0 ~ 1,
    rowSums(is.na(datos_slnn)) <= 0 ~ 0
  )
) %>% 
  filter(method == "QNM",lambda != 0) %>%
  select(lambda,num_target,has_na) %>% 
  group_by(num_target,lambda) %>% 
  summarise(n_missing = sum(has_na))
  
  
#Muestra cuantos missing values tienen los numeros en funcion de lambda
ggplot(datos_na_numbers, aes(x = num_target, y = n_missing, fill = num_target)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~ lambda, labeller = as_labeller(function(x) paste("lambda =", x))) +
  theme_bw() +
  labs(title = "Efecto de Lambda en los Missing Values",
       x = "Algoritmo",
       y = "Número de Missing Values",
       fill = "Números")