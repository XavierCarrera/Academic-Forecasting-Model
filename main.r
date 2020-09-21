# Importing dataset from project

library("saber")

library("dplyr")
library("saber")
library("nnet")
library("caret")
library("parallel")

# Data loading ----------------------------------------------------------

# data("SB11_20111") # 31707
data("SB11_20112")

table(SB11_20112$ECON_SN_INTERNET)

# SB11_20112 %>% names()

iterations <- 38
sample_size <- 27

plot(
mean(SB11_20112$MATEMATICAS_PUNT),
sd(SB11_20112$MATEMATICAS_PUNT),
pch = 20,
cex = 4,
col = "white"
)


for(i in seq_len(iterations)){
points(
  mean(sample(SB11_20112$MATEMATICAS_PUNT, sample_size)),
  sd(sample(SB11_20112$MATEMATICAS_PUNT, sample_size)),
  pch = 20
  
)
}

points(
  mean(SB11_20112$MATEMATICAS_PUNT),
  sd(SB11_20112$MATEMATICAS_PUNT),
  pch = 20,
  cex = 4,
  col = 2
)



# tidy approach -----------------------------------------------------------


library("dplyr")
library("ggplot2")
library("purrr")

colors_platzi <- c("#78D92A", "#002E4E", "#058ECD", "#ED2B05", "#F4F7F4")

tibble(
  samples = replicate(iterations, sample(SB11_20112[["MATEMATICAS_PUNT"]], sample_size), simplify = FALSE),
  mean_value = map_dbl(samples, mean),
  desv = map_dbl(samples, sd),
) %>% ggplot + 
  geom_point(aes(x = medias, y = desv)) +
  annotate(
    geom = "point", 
    x = mean(SB11_20112[["MATEMATICAS_PUNT"]]), 
    y = sd(SB11_20112[["MATEMATICAS_PUNT"]]),
    size = 4,
    colour = colores_platzi[4]) +
  theme_minimal()

# Correlation amid academic results and internet access 

library("saber")

data("SB11_20112")


table(SB11_20112$ECON_SN_INTERNET)


# intervalos de confianza de la media -------------------------------------


sample_size <- 30
iterations <- 100


pobla_A <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 0]
pobla_B <- SB11_20112$FISICA_PUNT[SB11_20112$ECON_SN_INTERNET == 1]
mean_pob_A <- mean(pobla_A, na.rm = TRUE)
mean_pob_B <- mean(pobla_B, na.rm = TRUE)

plot(mean_pob_A, mean_pob_B, col = 4, pch = 20)
abline(0,1)

for(i in seq_len(iterations)){
  
  samples <- sample(seq_len(nrow(SB11_20112)), sample_size)
  
  some_A <- seq_len(nrow(SB11_20112)) %in% sample & SB11_20112$ECON_SN_INTERNET == 0
  sample_A <- SB11_20112$FISICA_PUNT[some_A]
  
  some_B <- seq_len(nrow(SB11_20112)) %in% sample & SB11_20112$ECON_SN_INTERNET == 1
  sample_B <- SB11_20112$FISICA_PUNT[some_B]
  
  mean_sample_A <- mean(sample_A, na.rm = TRUE)
  t_test_A <- t.test(sample_A)
  interval_A <- t_test_A$conf.int
  LI_A <- min(interval_A)
  LS_A <- max(interval_A)
  
  mean_sample_B <- mean(sample_B, na.rm = TRUE)
  t_test_B <- t.test(sample_B, na.rm = TRUE)
  interval_B <- t_test_B$conf.int
  LI_B <- min(interval_B)
  LS_B <- max(interval_B)
  
  # points(media_muestra_A, media_muestra_B, col = 2, pch = 20)
  rect(LI_A, LI_B, LS_A, LS_B)
  
}


points(mean_pob_A, mean_pob_B, col = 4, pch = 20, cex = 2)



# Tidy approach -----------------------------------------------------------



library("dplyr")
library("purrr")
library("magrittr")
library("ggplot2")
library("LaCroixColoR")


colors <- lacroix_palette("Pamplemousse")


sample_size <- 30


tibble(
  sample = replicate(iterations, sample_n(SB11_20112, sample_size), simplify = FALSE),
  sample_A = map(sample, filter, ECON_SN_INTERNET == 0) %>% map(extract2, "FISICA_PUNT"),
  mean_sample_A = map_dbl(samĺpe_A, mean),
  t_test_A = map(sample_A, t.test),
  interval_A = map(t_test_A, extract2, "conf.int"),
  LI_A = map_dbl(interval_A, min),
  LS_A = map_dbl(interval_A, max),
  sample_B = map(sample, filter, ECON_SN_INTERNET == 1) %>% map(extract2, "FISICA_PUNT"),
  mean_sample_B = map_dbl(sample_B, mean),
  t_test_B = map(sample_B, t.test),
  interval_B = map(t_test_B, extract2, "conf.int"),
  LI_B = map_dbl(interval_B, min),
  LS_B = map_dbl(interval_B, max)
) -> analysis

mean_pob_A <- mean(pobla_A, na.rm = TRUE)

mean_pob_B <- mean(pobla_B, na.rm = TRUE)


analysis %>% 
  ggplot +
  geom_rect(aes(xmin = LI_A, xmax = LS_A, ymin = LI_B, ymax = LS_B), alpha = 0.2, fill = colores[6]) +

  annotate("point", mean_pob_A, mean_pob_B, colour = colors[4], size = 5) +
  geom_abline(intercept = 0, slope = 1, colour = colors[4], size = 1) +

  theme_minimal()

# Forecasting Neural Network

sample_size <- 2000

c(
  "ECON_PERSONAS_HOGAR",
  "ECON_CUARTOS",
  "ECON_SN_LAVADORA",
  "ECON_SN_NEVERA",
  "ECON_SN_HORNO",
  "ECON_SN_DVD",
  "ECON_SN_MICROHONDAS",
  "ECON_SN_AUTOMOVIL",
  "MATEMATICAS_PUNT"
) -> variables

sample_index <- seq_len(nrow(SB11_20112)) %in% sample(seq_len(nrow(SB11_20112)), sample_size)

sample <- subset(SB11_20112, subset = sample_index, select = variables)
sample <- na.omit(sample)

neuronal_network <- nnet(MATEMATICAS_PUNT ~., data = sample, size = 10, linout = TRUE) 

plot(sample$MATEMATICAS_PUNT ~ predict(neuronal_network))
lines(1:100, col = 2, lwd = 2)

# Cross Validation

rmse_fold <- function(pliegue, form, datos,  nn_size){
  pliegue_logic <- seq_len(nrow(datos)) %in% pliegue
  entrena <- subset(datos, !pliegue_logic)
  prueba <- subset(datos, pliegue_logic)
  model <- nnet(form, data = datos, size = nn_size, linout = TRUE, trace = FALSE)
  response_name <- setdiff(names(datos), modelo$coefnames)
  Y_pronosticado <- predict(modelo, newdata = prueba)
  rmse <- RMSE(Y_pronosticado, prueba[[response_name]])
  rmse
}


# Neuronal Network  ------------------------------------------------------------


sample_size <- 5000
neurons <- 10
n_folds <- 10


c(
  "ECON_PERSONAS_HOGAR",
  "ECON_CUARTOS",
  "ECON_SN_LAVADORA",
  "ECON_SN_NEVERA",
  "ECON_SN_HORNO",
  "ECON_SN_DVD",
  "ECON_SN_MICROHONDAS",
  "ECON_SN_AUTOMOVIL",
  "MATEMATICAS_PUNT"
) -> variables

sample_indexes <- seq_len(nrow(SB11_20112)) %in% sample(seq_len(nrow(SB11_20112)), sample_size)

samople <- subset(SB11_20112, subset = sample_indexes, select = variables)
sample <- na.omit(sample)

createFolds(sample$MATEMATICAS_PUNT, k = n_folds) -> folds

mclapply(
  folds,
  rmse_fold, 
  MATEMATICAS_PUNT ~., 
  sample, 
  nn_size = neurons, 
  mc.cores = floor(detectCores()*0.8)
) -> rmse_folds

rmse_folds <- unlist(rmse_folds)
mean(rmse_folds)

plot(rmse_folds, ylim = c(0, 12))
abline(h = mean(rmse_folds), col = 2, lwd = 2)

# Tidy approach -----------------------------------------------------------

library("dplyr")
library("magrittr")


sample_size <- 5000
neurons <- 10
n_folds <- 10

tibble(
  foldss = createFolds(sample$MATEMATICAS_PUNT, k = n_folds),
  rmse_folds = mclapply(
    folds,
    rmse_fold, 
    MATEMATICAS_PUNT ~., 
    sample, 
    nn_size = neurons, 
    mc.cores = floor(detectCores()*0.8)
  ) %>% unlist,
  sample_names = names(folds)
) -> validation

validation %$% mean(rmse_folds)

ggplot(validation) +
  geom_vline(aes(xintercept = 0), size = 1.5) +
  geom_segment(aes(x = 0, y = names, xend = rmse_folds, yend = names), colour = "grey75") +
  geom_point(aes(x = rmse_folds, y = names), size = 4) +
  theme_minimal()


