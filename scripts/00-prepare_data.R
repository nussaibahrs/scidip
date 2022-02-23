
# Load Survey Data --------------------------------------------------------

fl <- list.files("confidential", "survey")

dat <- lapply(fl, function (x) {readxl::read_excel(file.path("confidential", x))})

names(dat) <- gsub("survey_|\\.xlsx", "", fl)

# # get english columns to create proper column names and categories
# cols_en <- colnames(dat$en)
# write.csv(cols_en, "data/col_names.csv", row.names = F)
cols_en <- read.csv("data/col_names.csv")

dat <- lapply(dat, function(x){ 
  colnames(x) <- cols_en$short
  return(x)})

lang <- names(dat)

dat <- lapply(1:3, function(x) {
  dat[[x]]$lang_survey <- lang[x]
  return(dat[[x]])})

dat <- do.call(rbind, dat)
dat <- dat[,-1] # remove timestamp

# Save email
emails <- dat$email 
dat$email <- NULL # remove from results
write.csv(emails[!is.na(emails)], "confidential/emails.csv", row.names = F)

# Change to English
dict <- lapply(1:8, function(x) readxl::read_excel("data/survey_dict.xlsx", sheet=x))
dict <- lapply(dict, function(x){
  colnames(x) <- c("en", "sp", "pt")
  return(x)})
dict <- do.call(rbind, dict)

dict <- reshape2::melt(dict, id.vars="en", value.name = "non_en")
dict <- na.omit(dict)

for(i in 1:ncol(dat)){
  dat[,i] <- plyr::mapvalues(unlist(dat[,i]), from = dict$non_en, to = dict$en,
                             warn_missing = F)  
}

cols_multi <- c("lang_comm", "collab_career", "collab_type", "issues",
                "collab_contr", "lang_pubs")


# translate column with multiple values
for(c in cols_multi){
  temp <- unlist(dat[,c])
  
  temp <- strsplit(temp, split=",") # split
  
  temp <- lapply(temp, function(x){
    t <- plyr::mapvalues(trimws(x), from=dict$non_en, to=dict$en,
                    warn_missing=F)
    paste(t, collapse=", ")
  })
  
  dat[,c] <- unlist(temp)

}


# Select only those that said yes 
dat <- dat[dat$lat_eu_collab == "Yes",]
write.csv(dat, "data/survey_results_raw.csv", row.names = F)

# To be translated
cols_tr <- c("meet", "challenges", "k_exchange_det", "conditions", "recommendations", "ethics")

library(xlsx)

wb = createWorkbook()

for(c in cols_tr){
  temp <- dat[,c("lang_survey", c)]
  temp <- temp[temp$lang_survey != "en",]
  temp <- na.omit(temp)
  temp$en <- NA
  sheet = createSheet(wb, c)
  
  addDataFrame(as.data.frame(temp), sheet=sheet, startColumn=1, row.names=FALSE)
  
}

saveWorkbook(wb, "data/cols_tr.xlsx")

# To be recoded
cols_code <- c(grep("country", colnames(dat), value=T),
  "funding", "proj_date", "lat_eu_det") 

wb = createWorkbook()

for(c in cols_code){
  temp <- dat[,c("lang_survey", c)]
  temp <- temp[temp$lang_survey != "en",]
  temp <- na.omit(temp)
  temp$new <- NA
  sheet = createSheet(wb, c)
  
  addDataFrame(as.data.frame(temp), sheet=sheet, startColumn=1, row.names=FALSE)
  
}

saveWorkbook(wb, "data/cols_recode.xlsx")
