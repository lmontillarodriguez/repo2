library(tidyverse)
datos <- read_csv("https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv")
view(read_csv)

datos <- read_csv("https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv")
view(read_csv)

datos_madrid <- 
  datos |> 
  drop_na(sexo) |> 
  filter(provincia_iso == "M" & fecha <= "2020-12-21" & sexo != "NC") |> 
  select(fecha, sexo, grupo_edad, num_casos) |> 
  summarise(num_casos = sum(num_casos), .by = c(fecha, sexo))

         
datos <- read_csv("https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv")
view(read_csv)

write_csv(datos_madrid, file = "./datos_madrid.csv")