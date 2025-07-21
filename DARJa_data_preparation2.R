# this prepared data from the original Excel files as posted at 
# https://kpfu.ru/atributivnaya-baza-dannyh-russkih-govorov-269324.html
# and downloaded into the folder KazanExcelFiles2022-03-30
# on the date 2022-03-30

# the metadata is consistent across these four files,
# except for MAPINFO_ID 628 and 2329. For both, the value in the column
# Номер_нп is -2 in the syntax and lexicon files but 0 in the phonetics 
# and morphology files

library(readxl)

# some of the columns are empty, so, given a data frame, 
# here is a function for showing which columns are empty

empty <- function(df) {
	count_content <- apply(df, 2, function(z) which(!is.na(z)) )
	elements <- unname(unlist(lapply(count_content, length)))
	out <- which(elements==0)
	return(out)
}

# read each file and get rid of empty columns
L <- read_excel("Leksika.xlsx")
L <- as.data.frame(L)
empty_cols_L <- empty(L)
if ( length(empty_cols_L) > 0 ) {
	L <- L[,-empty(L)]
}

P <- read_excel("Fonetika.xlsx")
P <- as.data.frame(P)
empty_cols_P <- empty(P)
if ( length(empty_cols_P) > 0 ) {
	P <- P[,-empty(P)]
}

M <- read_excel("Morfologiya.xlsx")
M <- as.data.frame(M)
empty_cols_M <- empty(M)
if ( length(empty_cols_M) > 0 ) {
	M <- M[,-empty(M)]
}

S <- read_excel("Cintaksis.xlsx")
S <- as.data.frame(S)
empty_cols_S <- empty(S)
if ( length(empty_cols_S) > 0 ) {
	S <- S[,-empty(S)]
}

# the data represent features and values 
# separated by a dot; there is room for confusion
# because a value can be encoded as, for instance, 
# .01 or .001 with the same meaning. And
# .1 can mean 10 or 100. Finally there are 
# some values which seem to be extraneous.
# So the following routines checks what values (suffixes) are present in 
# the different files.

# The following function takes one of the objects L, P, M, S and outputs
# feature values; runs as get_values(S), get_values(M), ...

get_values <- function(df) {
	# in all cases the first 6 columns contain metadata;
	# this is deleted and the object arbitrarily renamed A
	A <- df[,-c(1:6)]
	# make empty vector to hold the feature values (suffixes)
	suffixes <- c()
	# go through the rows of the data frame
	for (i in 1:nrow(A)) {
		# print information about progress to the console
		if ( i %% 100 == 0 ) {
			cat("doing", i, "out of", nrow(A), "\n")
		}
		# get rid of cells that are just a zero (meant to encode end of row)
		# and also empty cells
		d <- A[i,]
		w_empty_1 <- which(d == "0")
		if ( length(w_empty_1) > 0 ) {
			d <- d[-w_empty_1]
		}
		w_empty_2 <- which(d == "")
		if ( length(w_empty_2) > 0 ) {
			d <- d[-w_empty_2]
		}
		w_empty_3 <- which(is.na(d))
		if ( length(w_empty_3) > 0 ) {
			d <- d[-w_empty_3]
		}
		# little function to extract the suffix (stuff after dot)
		get_suf <- function(z) strsplit(as.character(z), "\\.")[[1]][2]
		# apply this function to the row
		s <- unique(unlist(lapply(d, get_suf)))
		# add the elements to the vector called suffixes, but only retaining
		# all the different (unique) elements
		suffixes <- unique(c(suffixes, s))
	}
	# output the different suffixes, sorted
	return(sort(suffixes))
}

# outputs of the above, with constructions of vectors that make more sense

L_val_orig <- get_values(L)
L_val_orig
L_val_new <- as.character(c(1:138, 140:154, 160, 171))
cbind(L_val_orig, L_val_new)


# Leksika.xlsx
#   [1] "001" "002" "003" "004" "005" "006" "007" "008" "009" "01"  "011" "012" "013" "014"
#  [15] "015" "016" "017" "018" "019" "02"  "021" "022" "023" "024" "025" "026" "027" "028"
#  [29] "029" "03"  "031" "032" "033" "034" "035" "036" "037" "038" "039" "04"  "041" "042"
#  [43] "043" "044" "045" "046" "047" "048" "049" "05"  "051" "052" "053" "054" "055" "056"
#  [57] "057" "058" "059" "06"  "061" "062" "063" "064" "065" "066" "067" "068" "069" "07" 
#  [71] "071" "072" "073" "074" "075" "076" "077" "078" "079" "08"  "081" "082" "083" "084"
#  [85] "085" "086" "087" "088" "089" "09"  "091" "092" "093" "094" "095" "096" "097" "098"
#  [99] "099" "1"   "101" "102" "103" "104" "105" "106" "107" "108" "109" "11"  "111" "112"
# [113] "113" "114" "115" "116" "117" "118" "119" "12"  "121" "122" "123" "124" "125" "126"
# [127] "127" "128" "129" "13"  "131" "132" "133" "134" "135" "136" "137" "138" "14"  "141"
# [141] "142" "143" "144" "145" "146" "147" "148" "149" "15"  "151" "152" "153" "154" "16" 
# [155] "171"

P_val_orig <- get_values(P)
P_val_orig
P_val_new <- as.character(c(1:39, 80, 90))
cbind(P_val_orig,P_val_new)

# Fonetika
#  [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "1"  "11" "12" "13" "14" "15" "16" "17"
# [18] "18" "19" "2"  "21" "22" "23" "24" "25" "26" "27" "28" "29" "3"  "31" "32" "33" "34"
# [35] "35" "36" "37" "38" "39" "8"  "9" 

M_val_orig <- get_values(M)
M_val_orig
M_val_new <- as.character(c(1:29, 33, 44, 55, 66, 88))
cbind(M_val_orig,M_val_new)

# Morfologiya
#  [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "1"  "11" "12" "13" "14" "15" "16" "17"
# [18] "18" "19" "2"  "21" "22" "23" "24" "25" "26" "27" "28" "29" "33" "44" "55" "66" "88"

S_val_orig <- get_values(S)
S_val_orig
S_val_new <- as.character(1:17)
cbind(S_val_orig, S_val_new)

# Cintaksis.xlsx
#  [1] "01" "02" "03" "04" "05" "06" "07" "08" "09" "1"  "11" "12" "13" "14" "15" "16" "17"

# The following routine produces a combined datasheet called data_file_3.txt
# based on L, P, M, S, as gotten above;
# it replaces the original feature values with those that make better sense

# get a mapping between Cyrillic and Latin for atlas volumes
dirs_cyr <- sort(unique(S[,6]))
dirs_lat <- c("vostok","zapad","sever","severo-zapad","jug")

# while the previous took a few minutes the next takes c. 1 hour;
# it produces data_file_3.txt, which can subsequently be read for
# further processing
cat("map_ID\tno_np\tdir\tcoor_x\tcoor_y\ttype\tfeat\tval\n", file="data_file_3.txt")
dfs <- list(L,P,M,S)
for (p in 1:4) {
	if ( p==1 ) {
		val_orig <- L_val_orig
		val_new <- L_val_new
		data_type <- "L"
	}

	if ( p==2 ) {
		val_orig <- P_val_orig
		val_new <- P_val_new
		data_type <- "P"
	}

	if ( p==3 ) {
		val_orig <- M_val_orig
		val_new <- M_val_new
		data_type <- "M"
	}

	if ( p==4 ) {
		val_orig <- S_val_orig
		val_new <- S_val_new
		data_type <- "S"
	}

	A <- dfs[[p]]
	for (i in 1:nrow(A)) {
		if ( i %% 100 == 0 ) {
			cat("doing data frame", p, "out of 4, and row", i, "out of", nrow(A), "\n")
		}
		d <- as.vector(A[i,])
		d_meta <- d[1:6]
		d <- d[-(1:6)]
		# get rid of cells that are just a zero (meant to encode end of row)
		# and also empty cells
		w_empty_1 <- which(d == "0")
		if ( length(w_empty_1) > 0 ) {
			d <- d[-w_empty_1]
		}
		w_empty_3 <- which(is.na(d))
			if ( length(w_empty_3) > 0 ) {
			d <- d[-w_empty_3]
		}
		# the following condition takes care of a case in S where a row is 
		# empty after having deleted the single cell with a value, which is 0,
		# and having removed NA cells
		if ( length(d) > 0 ) {
			w_empty_2 <- which(d == "")
			if ( length(w_empty_2) > 0 ) {
				d <- d[-w_empty_2]
			}
			for (j in 1:length(d)) {
				# extract features and values
				F_V <- strsplit(as.character(d[j]), "\\.")[[1]]
				# the following condition takes care of at least one case
				# (row 2070 of M) where the feature value is lacking
				if ( length(F_V) == 2 ) {
					# replace feature value with the more sensible ones
					original_feature_value <- F_V[2]
					F_V[2] <- val_new[which(val_orig==original_feature_value)]
					## put meta data n the first five columns
					# mapinfo_ID
					cat(unname(unlist(d_meta[1])), "\t", file="data_file_3.txt", sep="", append=TRUE)
					# number of village within compas region on map
					cat(unname(unlist(d_meta[2])), "\t", file="data_file_3.txt", sep="", append=TRUE)
					# volume (vostok, zapad, ...) transliterated
					cat(dirs_lat[match(d_meta[6],dirs_cyr)], "\t", file="data_file_3.txt", sep="", append=TRUE)
					# X coordinate
					cat(unname(unlist(d_meta[4])), "\t", file="data_file_3.txt", sep="", append=TRUE)
					# Y coordinate
					cat(unname(unlist(d_meta[5])), "\t", file="data_file_3.txt", sep="", append=TRUE)
					## continue with data
					# type (L, P, M or S)
					cat(data_type, "\t", file="data_file_3.txt", sep="", append=TRUE)
					# feature
					cat(F_V[1], file="data_file_3.txt", append=TRUE)
					cat("\t", file="data_file_3.txt", append=TRUE)
					# value
					cat(F_V[2], file="data_file_3.txt", append=TRUE)
					cat("\n", file="data_file_3.txt", append=TRUE)
				}
			}	
		}	
	}
}

# error-spotting, starting with data_file_3.txt and finding
# cases where a combination of no_np + direction and coordinates
# is different; yields 200+ errors, so it should be tried to 
# find errors from the Excel before merging with data_file_3.txt
df <- read.table(file="data_file_3.txt", sep="\t", header=TRUE)
length(unique(df$map_ID))
# paste together no_np, direction, x and y coordinates in
# an object called ndc
ndc <- paste(df$no_np, df$dir, df$coor_x, df$coor_y, sep="_")
length(unique(ndc))
ndcu <- unique(ndc)
# extract the unique no_np, dir, and coordinates; they get x for extraction
no_np_x <- unlist(lapply(ndcu, function(z) strsplit(z, "_")[[1]][1]))
dir_x <- unlist(lapply(ndcu, function(z) strsplit(z, "_")[[1]][2]))
coor_x_x <- unlist(lapply(ndcu, function(z) strsplit(z, "_")[[1]][3]))
coor_y_x <- unlist(lapply(ndcu, function(z) strsplit(z, "_")[[1]][4]))
no_np_dir_x <- paste(no_np_x, dir_x, sep="_")
reps <- sort(no_np_dir_x[duplicated(no_np_dir_x)==TRUE])
cat("no_np\tdir\tcoor_x\tcoor_y\n", file="errors.txt")
for (i in 1:length(reps)) {
	w_rep <- which(no_np_dir_x==reps[i])
	for (j in 1:length(w_rep)) {
		cat(no_np_x[w_rep[j]], file="errors.txt", append=TRUE)
		cat("\t", file="errors.txt", append=TRUE)
		cat(dir_x[w_rep[j]], file="errors.txt", append=TRUE)
		cat("\t", file="errors.txt", append=TRUE)
		cat(coor_x_x[w_rep[j]], file="errors.txt", append=TRUE)
		cat("\t", file="errors.txt", append=TRUE)
		cat(coor_y_x[w_rep[j]], file="errors.txt", append=TRUE)
		cat("\n", file="errors.txt", append=TRUE)
	}
}

### add coordinates identified in Tübingen
# to the basic data in data_file_3.txt
# read file produced by DARJa_data_preparation2.R above
a <- read.delim2("data_file_3.txt") 
# file with coordinates produced in Tübingen and corrections of no_np and dir values produced in Kiel
b <- read.csv("data-small-final2.csv") 
# the metadata should be added from the Tübingen
# file to data_file_3.txt based on the
# map_ID, which is the one consistent identifier
values <- a[,c("map_ID","type","feat","val")]
m <- match(values$map_ID, b$map_ID)
meta_expand <- b[m,]
data_file_4_material <- cbind(meta_expand, values[,c("type", "feat", "val")])
nas <- which(is.na(data_file_4_material),1)
if ( length(nas) > 0 ) {
	data_file_4_material <- data_file_4_material[-nas,]
}

# remove spurious locations; those are map_ID 628 and 2329. For the former
# there appears to be a dot, but there is no number, and no clues that the
# missing number is an actual omission; for the latter the dot appears to 
# belong to a border
del1 <- which(data_file_4_material$map_ID==628)
del2 <- which(data_file_4_material$map_ID==2329)
del <- c(del1, del2)
data_file_4_material <- data_file_4_material[-del,]

write.table(data_file_4_material, file="data_file_4.txt", sep="\t", quote=FALSE, row.names=FALSE)

# Read the data file and prepare an R object for subsequent analyses
x <- read.delim2("data_file_4.txt") # file produced by DARJa_data_preparation2.R
# each combination of category, features, and values is
# merged in a single data point called fv (for feature-value)
# this is separate from f, which is the feature
id <- x$map_ID
type <- x$type
orig_id <- paste(x$no_np, x$dir, sep="_")
f <- paste(x$type, x$feat, sep="_")
fv <- paste(x$type, x$feat, x$val, sep="_")
lon <- x$lon
lat <- x$lat
# id and fv are now put together in a data frame called d (for data)
d <- data.frame(id, orig_id, lon, lat, type, f, fv)
save(d, file="darja_data2.RData")
# remove x from memory, not needed anymore
rm(x)

x <- read.delim2("data_file_4.txt") # file produced by DARJa_data_preparation2.R

# each combination of category, features, and values is
# merged in a single data point called fv (for feature-value)
# this is separate from f, which is the feature
id <- x$map_ID
type <- x$type
orig_id <- paste(x$no_np, x$dir, sep="_")
f <- paste(x$type, x$feat, sep="_")
fv <- paste(x$type, x$feat, x$val, sep="_")
lon <- x$lon
lat <- x$lat
# id and fv are now put together in a data frame called d (for data)
d <- data.frame(id, orig_id, lon, lat, type, f, fv)
# save(d, file="darja_data2.RData")
erratic_row <- which(d$f=="L_14203")
d <- d[-erratic_row,]
save(d, file="darja_data3.RData")
write.table(d, file="darja_data3.txt", sep="\t", quote=FALSE)

