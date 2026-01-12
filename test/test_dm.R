devtools::load_all()

names_df <- read.csv("dm_test_names.csv",
                     stringsAsFactors = FALSE)

res <- t(sapply(names_df$name, double_metaphone))

out <- data.frame(
  name = names_df$name,
  primary_r = res[,1],
  secondary_r = res[,2],
  stringsAsFactors = FALSE
)

write.csv(out,
          "r_metaphone_output.csv",
          row.names = FALSE)

cat("R metaphone output saved\n")


