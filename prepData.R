# library(synapseClient)
# synapseLogin()
# 
# library(plyr)

# files <- synQuery("SELECT id, name FROM file WHERE parentId == 'syn5522627'")
# 
# for (i in 1:length(files$file.id)){
#   synGet(files$file.id[i],downloadLocation = "./data/")
# }
# 
# filterColumns <- function(select_col, df){
#   names(df) <- tolower(names(df))
#   df <- df[,select_col]
#   return(df)
# }
# 
# ipNF2.3 <- read.csv("data/NTAP ipNF02.3 2l MIPE qHTS.csv", stringsAsFactors = FALSE)
# ipNF2.8 <- read.csv("data/NTAP ipNF02.8 MIPE qHTS.csv", stringsAsFactors = FALSE)
# ipNF5.5M <- read.csv("data/NTAP ipNF05.5 MC MIPE qHTS.csv", stringsAsFactors = FALSE)
# ipNF5.5S <- read.csv("data/NTAP ipNF05.5 SC MIPE qHTS.csv", stringsAsFactors = FALSE)
# ipNF6.2 <- read.csv("data/NTAP ipNF06.2A MIPE qHTS.csv", stringsAsFactors = FALSE)
# ipNF95.11b <- read.csv("data/NTAP ipNF95.11b C_T MIPE qHTS.csv",stringsAsFactors = FALSE)
# ipNF95.11c <- read.csv("data/NTAP ipNF95.11C MIPE qHTS.csv", stringsAsFactors = FALSE)
# ipNF95.6 <- read.csv("data/NTAP ipNF95.6 MIPE qHTS.csv",stringsAsFactors = FALSE)
# 
# select_col <- c("cell.line", "crc", "lac50","maxr","tauc","name","target")
# ipNF2.3 <- filterColumns(select_col,ipNF2.3)
# ipNF2.8 <- filterColumns(select_col,ipNF2.8)
# ipNF5.5M <- filterColumns(select_col,ipNF5.5M)
# ipNF5.5S <- filterColumns(select_col,ipNF5.5S)
# ipNF6.2 <- filterColumns(select_col,ipNF6.2)
# ipNF95.11b <- filterColumns(select_col,ipNF95.11b)
# ipNF95.6 <- filterColumns(select_col,ipNF95.6)
# 
# select_col <- c("cell.line", "cclass2", "lac50","maxr","tauc","name","target")
# ipNF95.11c <- filterColumns(select_col,ipNF95.11c)
# 
# HFF <- read.csv("data/s-ntap-HFF-1.csv", stringsAsFactors = FALSE)
# MTC <- read.csv("data/s-ntap-MTC-1.csv",stringsAsFactors = FALSE)
# 
# select_col <- c("cclass2", "lac50","maxr","tauc","name","target")
# HFF <- filterColumns(select_col,HFF)
# MTC <- filterColumns(select_col,MTC)
# 
# HFF$cell.line <- "HFF"
# MTC$cell.line <- "MTC"
# 
# #rename columns
# ipNF95.11c <- rename(ipNF95.11c,c("cclass2" = "crc"))
# HFF <- rename(HFF,c("cclass2" = "crc"))
# MTC <- rename(MTC,c("cclass2" = "crc"))
# 
# df <- do.call(rbind,list(ipNF2.3,ipNF2.8,ipNF5.5M,ipNF5.5S,ipNF6.2,ipNF95.11b,
#                         ipNF95.11c,ipNF95.6,HFF,MTC))
# 
# names(df) <- c("sample","curveClass","AC50","maxResp","AUC","drug","target")
# 
# df$AC50 <- 10^df$AC50*1e+06
# 
# write.table(df, "data/ntap_processed_data.tsv",row.names = FALSE, sep = "\t",na = "")


fileparent='syn5522627'

##download files
headerfile='syn5522652'
qr<-synQuery(paste("select * from entity where parentId=='",fileparent,"'",sep=''))
hind=which(qr$entity.id==headerfile)
header=read.table(synGet(qr[hind,'entity.id'])@filePath,sep='-',fill=T)
dfiles<-qr[grep('csv',qr$entity.name),]

test <- apply(dfiles,1,function(x){
  synId <- x["entity.id"]
  tissue <- x["entity.tissueSubtype"]
  res <- read.csv(synGet(synId)@filePath, stringsAsFactors = FALSE)
  if('CCLASS2'%in%colnames(res)){
    res <- rename(res,c("CCLASS2" = "CRC"))
  }
  if('Cell.Line'%in%colnames(res)){
    res <- rename(res,c("Cell.Line" = "Cell.line"))
  }
  if(tissue == "HFF"){
    res$Cell.line <- "HFF"
  }
  if(tissue == "MTC"){
    res$Cell.line <- "MTC"
  }
  res <- res[,c("Cell.line","name","target","CRC","LAC50","MAXR","TAUC","FAUC",
                "DATA0","DATA1","DATA2","DATA3","DATA4","DATA5","DATA6", 
                "DATA7", "DATA8","DATA9","DATA10","C0","C1","C2","C3","C4",
                "C5","C6","C7","C8","C9","C10")]
  return(res)
})

df.all <- do.call(rbind,test)
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


write.table(df1, "test/ntap_summarized_data.tsv",row.names = FALSE, sep = "\t",na = "")
write.table(df2, "test/ntap_raw_data.tsv",row.names = FALSE, sep = "\t",na = "")

file1 <- File("test/ntap_summarized_data.tsv",parentId = "syn6171357")
file2 <- File("test/ntap_raw_data.tsv",parentId = "syn6171357")

synStore(file1)
synStore(file2)
