library(tidyverse)



output_list <- list.files(pattern = "nmboot[0-9]+\\.out",full.names = TRUE)
output_numbers <- str_extract(output_list, "[0-9]+") |>  as.numeric()
ordered_output_list <- output_list[order(output_numbers)]





success <- function(output) {
  lines <- read_lines(output)
  
  
  
  if (any(str_detect(lines, "0MINIMIZATION SUCCESSFUL")) ) {
    theta_start <- which(str_detect(lines, "^\\s*THETA - VECTOR OF FIXED EFFECTS PARAMETERS")) + 1
    omega_start <- which(str_detect(lines, "^\\s*OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS")) + 1
    
    theta_lines <- lines[(theta_start + 1):(theta_start + 6)]
    omega_lines <- lines[(omega_start + 1):(omega_start + 14)]
    
    theta_values <- str_extract_all(theta_lines, "\\d+\\.\\d+E[+-]?\\d+") %>% unlist()
    
    omega_values <- str_extract_all(omega_lines, "[+\\s]*\\d+\\.\\d+E[+-]?\\d+") %>%
      unlist() %>%
      str_replace_all("\\s+", "") %>%
      str_extract("\\d+\\.\\d+E[+-]?\\d+") %>%
      na.omit()
    
    return(tibble(
      file = basename(output),
      theta = paste(theta_values, collapse = ", "),
      omega = paste(omega_values, collapse = ", ")
    ))
  } else {
    return(NULL)
  }
}



result1 <-map_df(ordered_output_list, success)


"nmboot.out"
results_tidy <- result1 |> 
  separate(theta, into = paste0("theta_", 1:6), sep = ",\\s*") |> 
  separate(omega, into = paste0("omega_", 1:6), sep = ",\\s*") |> 
  select(1:7,8,10,12,13) |> 
  mutate(across(-1, as.double)) |> 
  filter(!(file %in% c("nmboot104.out","nmboot134.out","nmboot139.out","nmboot16.out","nmboot181.out","nmboot218.out","nmboot226.out","nmboot275.out","nmboot317.out","nmboot339.out","nmboot34.out","nmboot357.out","nmboot36.out",
                       "nmboot362.out","nmboot380.out","nmboot425.out","nmboot458.out","nmboot52.out","nmboot54.out","nmboot57.out","nmboot87.out","nmboot480.out",
                       "nmboot497.out", "nmboot500.out"))
  )


colnames(results_tidy) <- c("OUTPUT","KA", "VC", "CL", "ALAG1","ADD", "PROP", "OM1", "OM2","OM23", "OM3")


results_tidy <- results_tidy |>  
  mutate(OM1_1 = 100*sqrt(exp(OM1)-1),
         OM2_1 = 100*sqrt(exp(OM2)-1),
         OM3_1 = 100*sqrt(exp(OM3)-1)
  ) |> 
  select(-6) |> 
  summarise(across(
    -1,
    .fns = list(
      median = ~median(.x, na.rm = TRUE),
      quantile_2.5 = ~quantile(.x, probs = 0.050, na.rm = TRUE),
      quantile_97.5 = ~quantile(.x, probs = 0.95, na.rm = TRUE)
    )
  )) 
results_tidy|> write.csv("table.csv")
#Bootstrap result
library(psych)

results_tidy |> 
  describe() |> 
  select(n,mean, sd, median) |>
  write.csv("table0228.csv")

