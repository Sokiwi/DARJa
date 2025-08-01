# read the text file version of the file gotten from Isaev
lmd <- read.table(file="atlas_spravka_svod_unicode.txt", sep="\t", header=TRUE, 
                  quote="", na.strings="", comment.char="", fileEncoding="UTF-16LE")
names(lmd) <- c("nomer_np", "tom_atlasa", "tip_np", 
                "nazvanie_np", "oblast", "rajon", "god_obsledovanija")

# function, given an original ID in the format of the d object, 
# for example "79_sever", for getting meta data on 
# the location from the lmd object
get_metadata_location <- function(orig_id) {
  number <- strsplit(orig_id, "_")[[1]][1]
  dir <- strsplit(orig_id, "_")[[1]][2]
  dir_rus <- switch(dir, 
                    "vostok" = "восток", 
                    "zapad" = "запад", 
                    "sever" = "север", 
                    "severo-zapad" = "северо-запад", 
                    "jug" = "юг")
  w_num <- which(lmd$nomer_np==number)
  w_dir <- which(lmd$tom_atlasa==dir_rus)
  w_num_dir <- intersect(which(lmd$nomer_np==number), which(lmd$tom_atlasa==dir_rus))[1]
  return(lmd[w_num_dir,])
}
