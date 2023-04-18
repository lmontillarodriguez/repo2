library(tidyverse)
starwars |> 
  mutate(height_m = height / 100,
         IMC = mass / (height_m^2), .after = name)

starwars |> 
 mutate(height_m = height / 100,
         IMC = mass / (height_m^2)) |> 
           relocate(height_m, .after = name) |> 
                      relocate(IMC, .after = sex)
         
                    
starwars |> 
  rowid_to_column(var = "hola")
starwars |> 
  mutate(hola = 1:nrow(starwars), .before = everything())

starwars |> 
  mutate(human = if_else(species == "Human", "Human", "Not Human", .after = name)

starwars |> 
  mutate(human = species == "Human", .after = name)

starwars |> distinct(species)

starwars |> 
  mutate(recat_human = if_else(species == "Human", "human",
                     if_else(species == "Droid", "robot", "otros")),
                     .after = name)
         

starwars |> 
  mutate(recat_human = case_when(species == "Human" ~ "human",
                                 species == "Droid" ~ "robot",
                                 TRUE ~ "otros"),
         .after = name)

starwars |> 
  mutate(recat_human = case_when(species == "Human" ~ "human",
                                 species == "Droid" ~ "robot",
                                 species != "Human" & species != "Drodid" ~ "otros"),
         .after = name)


starwars |> 
  mutate(recat_mass = case_when(mass <= 50 ~ "poco",
                                mass < 80 ~ "medio",
                                mass < 120 ~ "alto",
                                TRUE ~ "otro"),
         .after = name)


starwars |> 
  count(sex, gender, sort = TRUE) |> 
  mutate(porc = 100 * n / sum(n))


starwars |> 
  summarise(media_peso = mean(mass, na.rm = TRUE),
            mediana_peso = mean(mass, na.rm = TRUE))

starwars |> 
  group_by(sex) 
summarise(media_peso = mean(mass, na.rm = TRUE)) |> 
  ungroup()
            
starwars |> 
  summarise(percentiles = quantile(mass, na.rm = TRUE))


starwars |> 
  group_by(sex) |> 
summarise(media_peso = mean(mass, na.rm = TRUE)) |> 
  ungroup()

starwars |> 
  summarise(media_peso = mean(mass, na.rm = TRUE), .BY = sex)

tibble("year" = c(2022, 2022, 2022, 2023, 2023, 2023),
       "births" = c(140, 150, 160, 50, 80, 100),
       "pais" = c("esp", "fra", "ale", "esp", "fra", "ale")) |> 
  group_by(year) |> 
             mutate(porc_y_births = births / sum(births))



tibble("year" = c(2022, 2022, 2022, 2023, 2023, 2023),
       "births" = c(140, 150, 160, 50, 80, 100),
       "pais" = c("esp", "fra", "ale", "esp", "fra", "ale")) |> 
  group_by(year) |> 
  mutate(total_y_births = sum(births)) |> 
  ungroup() |> 
  mutate(porc = 100 * births / sum(births), .by = year)

tibble("year" = c(2022, 2022, 2022, 2023, 2023, 2023),
       "births" = c(140, 150, 160, 50, 80, 100),
       "pais" = c("esp", "fra", "ale", "esp", "fra", "ale")) |> 
  group_by(year) |> 
  mutate(total_y_births = sum(births)) |> 
  ungroup() |> 
  mutate(porc = 100 * births / sum(births), .by = year)


