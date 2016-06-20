#df <- read.delim("data/ntap_processed_data.tsv",stringsAsFactors = FALSE)

df <- synGet("syn6171374")
df <- read.delim(df@filePath,stringsAsFactors = FALSE)
