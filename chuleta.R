install.packages("lubridate")
library(lubridate)
pesos <- c(89, 56, 120)
estaturas <- c(190, 183, 154)
nombres <- c("Juan", "Martina", "Ambrosio")
fechas_nac <- as_date (c("1989-09-10", "1994-04-13", "1960-11-20"))
tabla <- data.frame("mass" = pesos, "names" = nombres, "birth_date" = fechas_nac, "height" = estaturas, "BMI" = pesos  / ((estaturas/100)^2)
View(tabla)

library(tidyverse)

pesos <- c(89, 56, 120)
estaturas <- c(190, 183, 154)
nombres <- c("Juan", "Martina", "Ambrosio")
fechas_nac <- as_date (c("1989-09-10", "1994-04-13", "1960-11-20"))
tabla <- tibble("mass" = pesos, "names" = nombres, "birth_date" = fechas_nac, "height" = estaturas, "BMI" = mass  / ((height/100)^2)
view(tibble)            
                
install.packages("datapasta")

write_csv(tabla, file = "./datos.csv")
tabla <- read_csv(file = ".datos.csv")
tabla <- read_csv("https://datos.gob.es/es/catalogo/a10002983-covid-19-casos-confirmados-por-pcr-casos-pcr-en-los-ultimos-14-dias-y-personas-fallecidas-por-municipio-de-la-comunitat-valenciana-20211")

%>% 
|>
  
  
library(tidyr)
table4a |> 
  pivot_longer(cols = c("1999", "2000"), 
names_to = "year", values_to = "casos")
1:7

billboard |> 
  pivot_longer(cols = wk1:wk76, 
   
              
               
                           names_to = "week", values_to = "rank", values_drop_na = TRUE, names_prefix = "wk")
table2

table2 |> 
  pivot_wider(names_from = "type", values_from = "count")

library(tidyverse)

table3

table3 |> 
  separate(col = "rate", into = c("cases", "pop")) |> 
  mutate(cases = as.numeric(cases), pop = as.numeric(pop)))

table3 |> 
  separate(col = "rate", into = c("cases", "pop"), sep = "/", convert = TRUE)


table3 |> 
  separate(col = "rate", into = c("cases", "pop")) |> 
  mutate(cases = as.numeric(cases), pop = as.numeric (pop))

table3 |> 
  separate(col = "rate", into = c("cases", "pop")) |> 
  mutate(across(c(var1, var100:var200, var567), as.numeric))

starwars |> transmute(IMC = mass / ((height/100)^2))





library(tidyverse)

starwars |> 
  filter(species == "Human")

starwars |> 
  filter(mass >= 80 & species == "Human")


starwars |> 
  filter(xor (mass >= 80, species == "Human"))

starwars |> 
  filter(mass >= 80) |> 
  filter(species == "Human")

starwars |> 
  filter (mass > 80 & species == "Human") | sex == "female")

starwars |> 
  filter(mass >= 60 & mass <= 80)

starwars |> 
  filter(between(mass, 60, 80))

starwars |> 
  filter(eye_color == "brown" | eye_color == "blue")


starwars |> 
  filter(eye_color %in% c("brown", "blue", "yellow"))


starwars |> 
  filter(species == "Human") |>
  distinct(eye_color) |> 
  pull(eye_color)

starwars |> 
  filter(eye_color %in% color)

starwars |> 
  filter(!is.na(mass))

starwars |> 
  drop_na(mass)

starwars |> 
  drop_na(mass) |> 
  filter(species == "Human")

  
starwars |> 
  slice(c(1:10))

starwars |> 
  slice_head(n = 5)

starwars |> 
  slice_tail(n = 8)


starwars |> 
  slice_head(prop = 0.25)

starwars |> 
  arrange(desc(mass)) |> 
  slice_head(n = 5)


starwars |> 
  slice_max(n = 5, mass)

starwars |> 
  slice_min(n = 5, mass)

starwars |> 
  filter(sex %in% c("male", "female")) |> 
  count(sex)

starwars |> 
  filter(sex %in% c("male", "female")) |> 
  slice_sample(prop = 0.5, by = sex)
  

starwars |> 
  drop_na(mass) |> 
  slice_sample(n = 7, weight_by = mass)

starwars |> 
  distinct(eye_color, .keep_all = TRUE)

starwars |> 
  select(name, where(is.numeric))

starwars |> 
  select(-name)

starwars |> 
  select(name:gender)

starwars |> 
  select(ends_with("color"))

starwars |> 
  select(contains("color"))

billboard |> 
  select(num_range("wk", 7:40))

tabla <-
  starwars |> 
  select(name, contains("color", where(is.numeric))


starwars |> 
  rename (peso = mass, altura = height, color_ojo = eye_color) |> 
relocate(peso, .before = everything())

starwars |> 
  filter(mass > 80) |> 
  select (name, height)
