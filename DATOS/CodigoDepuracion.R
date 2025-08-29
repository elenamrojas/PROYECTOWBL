library(tidyverse)

# 1) Cargar datos
df <- read_csv("DATOS/sg_law_indx.csv")

# 2) Detectar columnas de años 
year_cols <- grep("^\\d{4}$", names(df), value = TRUE)
if (length(year_cols) == 0) stop("No se detectaron columnas de años. Revisa names(df).")

# 3) Pasar a formato largo
df_clean <- df %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "wbl_index"
  ) %>%
  mutate(
    year = as.integer(year),
    wbl_index = as.numeric(wbl_index)
  ) %>%
  select(country, year, wbl_index) %>%
  arrange(country, year)

# 4) Guardar limpio
write_csv(df_clean, "DATOS/sg_law_indx_clean.csv")
cat("guardado: DATOS/sg_law_indx_clean.csv\n")

# 5) Comporbar
df_clean %>%
  group_by(year) %>%
  summarise(n = n(), min = min(wbl_index), median = median(wbl_index),
            mean = mean(wbl_index), max = max(wbl_index), .groups = "drop") %>%
  print(n = 10)




