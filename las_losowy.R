library(tidymodels)
library(tidyverse)
set.seed(44)
data_split <- initial_split(LOL, prop = 0.7)
lol_tr <- training(data_split)
lol_te <- testing(data_split)

rec <- recipe(Region ~ Kills+Golds+Vs_kills+Duration_sec,
              data = lol_tr) %>% 
  step_normalize(all_numeric_predictors())

rf <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger", importance = "permutation")

params <- extract_parameter_set_dials(rf)
params <- finalize(params, lol_tr)

rs <- vfold_cv(lol_tr, v = 5)

meas <- metric_set(accuracy, roc_auc)

grid <- grid_latin_hypercube(params, size = 10)
grid %>% 
  flextable::flextable()

rf_wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf)

#load("C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\rf_res.rda")
library(doParallel)
registerDoParallel(cores = 4)
rf_res <-
     rf_wf %>%
     tune_grid(
       resamples = rs,
       grid = grid,
       metrics = meas
   )
#save(rf_res, file = "C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\rf_res.rda")

rf_res %>% 
  collect_metrics() %>% 
  flextable::flextable()

rf_best_param <- select_best(rf_res, metric = "roc_auc")

rf_final <- 
  rf_wf %>% 
  finalize_workflow(rf_best_param)

rf_fit <- rf_final %>% 
  last_fit(data_split, metrics = meas)

rf_fit %>% 
  collect_metrics()

library(caret)

conf_matrix_region <- rf_fit %>% 
  collect_predictions() %>% 
  select(.pred_class, Region)

confusionMatrix(table(conf_matrix_region$.pred_class, conf_matrix_region$Region))

save(conf_matrix, file = "C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\conf_matrix_region.rda")


### Przewidywanie wyniku meczu

LOL$Win <- as.factor(LOL$Win)

set.seed(44)
data_split <- initial_split(LOL, prop = 0.7)
lol_tr <- training(data_split)
lol_te <- testing(data_split)

rec <- recipe(Win ~ Kills+Golds+Vs_kills+Duration_sec,
              data = lol_tr) %>% 
  step_normalize(all_numeric_predictors())

rf <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>% 
  set_mode("classification") %>% 
  set_engine("ranger", importance = "permutation")

params <- extract_parameter_set_dials(rf)
params <- finalize(params, lol_tr)

rs <- vfold_cv(lol_tr, v = 5)

meas <- metric_set(accuracy, roc_auc)

grid <- grid_latin_hypercube(params, size = 10)
grid %>% 
  flextable::flextable()

rf_wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf)

load("C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\rf_res.rda")
# library(doParallel)
# registerDoParallel(cores = 4)
# rf_res <-
#      rf_wf %>%
#      tune_grid(
#        resamples = rs,
#        grid = grid,
#        metrics = meas
#    )
# save(rf_res, file = "C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\rf_res.rda")

rf_res %>% 
  collect_metrics() %>% 
  flextable::flextable()

rf_best_param <- select_best(rf_res, metric = "roc_auc")

rf_final <- 
  rf_wf %>% 
  finalize_workflow(rf_best_param)

rf_fit <- rf_final %>% 
  last_fit(data_split, metrics = meas)

rf_fit %>% 
  collect_metrics()

library(caret)

conf_matrix <- rf_fit %>% 
  collect_predictions() %>% 
  select(.pred_class, Win)

confusionMatrix(table(conf_matrix$.pred_class, conf_matrix$Win))

save(conf_matrix, file = "C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\conf_matrix.rda")
