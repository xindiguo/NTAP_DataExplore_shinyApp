library(synapseClient)
synapseLogin()

library(plyr)

fileparent='syn5522627'

##download files
headerfile='syn5522652'
qr<-synQuery(paste("select * from entity where parentId=='",fileparent,"'",sep=''))
dfiles<-qr[grep('csv',qr$entity.name),]

vals <- apply(dfiles,1,function(x){
  synId <- x["entity.id"]
  tissue <- x["entity.tissueSubtype"]
  res <- read.csv(synGet(synId)@filePath, stringsAsFactors = FALSE)
  if('CCLASS2'%in%colnames(res)){
    res <- plyr::rename(res,c("CCLASS2" = "CRC"))
  }
  if('Cell.Line'%in%colnames(res)){
    res <- plyr::rename(res,c("Cell.Line" = "Cell.line"))
  }
#   if(tissue == "HFF"){
#     res$Cell.line <- "HFF"
#   }
#   if(tissue == "MTC"){
#     res$Cell.line <- "MTC"
#   }
  res$Cell.line <- tissue
  res <- res[,c("Cell.line","name","target","CRC","LAC50","MAXR","TAUC","FAUC",
                "DATA0","DATA1","DATA2","DATA3","DATA4","DATA5","DATA6", 
                "DATA7", "DATA8","DATA9","DATA10","C0","C1","C2","C3","C4",
                "C5","C6","C7","C8","C9","C10")]
  return(res)
})

df.all <- do.call(rbind,vals)
df.all[!is.na(df.all$name) & df.all$name == "Stelazine\ntrifluoperzine",]$name <- "Stelazine trifluoperzine"

# Summarized/Processed Data
df1 <- df.all[,c("Cell.line","name","target","CRC","LAC50","MAXR","TAUC")]
colnames(df1) <- c("sample","drug","target","curveClass","AC50","maxResp","AUC")
df1$AC50 <- 10^df1$AC50*1e+06
df1$IC50 <- NA

# Raw Data
df2 <- df.all[,c("Cell.line","name","DATA0","DATA1","DATA2","DATA3","DATA4","DATA5",
                 "DATA6","DATA7", "DATA8","DATA9","DATA10","C0","C1","C2","C3","C4",
                 "C5","C6","C7","C8","C9","C10")]

df2 <- rename(df2,c("Cell.line" = "sample", "name"="drug"))

vals <- apply(df2,1,function(drug.dat){
  dvals<-as.numeric(drug.dat[grep('DATA[0-9+]',names(drug.dat))])
  norm.dvals <- dvals/max(dvals)
  cvals<-as.numeric(drug.dat[grep('^C[0-9+]',names(drug.dat))])
  cvals <- cvals/1e+6
  df <- data.frame(conc=cvals,normViability=norm.dvals)
  df$sample <- drug.dat["sample"]
  df$drug <- drug.dat["drug"]
  return(df)
})

df2 <- do.call(rbind,vals)

write.table(df1, "test/ntap_summarized_data.tsv",row.names = FALSE, sep = "\t",na = "")
write.table(df2, "test/ntap_raw_data.tsv",row.names = FALSE, sep = "\t",na = "")

file1 <- File("test/ntap_summarized_data.tsv",parentId = "syn6171357")
file2 <- File("test/ntap_raw_data.tsv",parentId = "syn6171357")

synStore(file1)
synStore(file2)
