# Rebuild app/smiskis.csv from the Smiski Google Sheet.
#
# Run this from the project root (open smiskitracker.Rproj, then
# source("R/update_smiskis.R")) whenever new figures are added to the sheet.
# Then commit and push: the site republishes automatically.

sheet_id  <- "1U4l4ugaL8UG7wzEfQqQ1h_oYYX0s0tGpqw9XV_cxnms"
sheet_gid <- "236525206"
sheet_url <- sprintf(
  "https://docs.google.com/spreadsheets/d/%s/export?format=csv&gid=%s",
  sheet_id, sheet_gid
)

raw <- read.csv(sheet_url, header = FALSE, stringsAsFactors = FALSE,
                encoding = "UTF-8", check.names = FALSE)
raw[is.na(raw)] <- ""

squish <- function(x) trimws(gsub("[[:space:]]+", " ", x))

# Spelling fixes for names as they appear in the sheet
name_fixes <- c(
  "Thinkning"          = "Thinking",
  "Giving a Bouqet"    = "Giving a Bouquet",
  "Blanacing Boxes"    = "Balancing Boxes",
  "Vela'zquez"         = "Velázquez"
)
collection_fixes <- c(
  "Excersice Series"     = "Exercise Series",
  "Hanging Series- xmas" = "Hanging Series (Christmas)",
  "Smiski Strap- S1"     = "Strap Series 1",
  "Smiski Strap- S2"     = "Strap Series 2",
  "Smiski Strap- S3"     = "Strap Series 3",
  "Special"              = "Mini Surprise"
)

clean_figure <- function(x) {
  x <- squish(x)
  secret <- grepl("^(SECRET|SERCRET)", x, ignore.case = TRUE)
  x <- sub("^(SECRET|SERCRET)\\s*", "", x, ignore.case = TRUE)
  xmas <- grepl("Christmas version", x, ignore.case = TRUE)
  x <- sub("Christmas version\\s*", "", x, ignore.case = TRUE)
  x <- sub("^Series 1 Mini Surprise\\s*", "", x, ignore.case = TRUE)
  x <- squish(gsub('"', "", x))
  # Hippers names like "smiski dozing" -> "Dozing"
  x <- sub("^smiski\\s+(?!Tasting|Decorating)", "", x, ignore.case = TRUE, perl = TRUE)
  x <- if (x %in% names(name_fixes)) name_fixes[[x]] else x
  x <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  if (xmas) x <- paste(x, "(Christmas)")
  list(name = x, secret = secret)
}

slug <- function(x) {
  x <- gsub("['`^~\"]", "", iconv(tolower(x), to = "ASCII//TRANSLIT"))
  gsub("(^-|-$)", "", gsub("[^a-z0-9]+", "-", x))
}

# Each collection is a block whose header row has "Picture" in the next column.
headers <- which(raw == "Picture", arr.ind = TRUE)
rows <- list()
for (i in seq_len(nrow(headers))) {
  r <- headers[i, 1]
  c <- headers[i, 2] - 1
  collection <- squish(raw[r, c])
  if (collection == "" || tolower(collection) == "template") next
  if (collection %in% names(collection_fixes)) collection <- collection_fixes[[collection]]
  k <- r + 1
  while (k <= nrow(raw) && squish(raw[k, c]) != "") {
    fig <- clean_figure(raw[k, c])
    rows[[length(rows) + 1]] <- data.frame(
      collection = collection, figure = fig$name, secret = fig$secret
    )
    k <- k + 1
  }
}
smiskis <- do.call(rbind, rows)

# Display order: numbered series, themed series (A-Z), straps, Hippers, minis
group <- ifelse(grepl("^Series [0-9]", smiskis$collection), 1,
         ifelse(grepl("^Strap", smiskis$collection), 3,
         ifelse(smiskis$collection == "Hippers", 4,
         ifelse(smiskis$collection == "Mini Surprise", 5, 2))))
smiskis$order <- seq_len(nrow(smiskis))
smiskis <- smiskis[order(group, smiskis$collection, smiskis$order), ]
smiskis$order <- NULL

smiskis$id <- paste(slug(smiskis$collection), slug(smiskis$figure), sep = "--")
stopifnot(!anyDuplicated(smiskis$id))
smiskis <- smiskis[, c("id", "collection", "figure", "secret")]

write.csv(smiskis, "app/smiskis.csv", row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("Wrote %d figures in %d collections to app/smiskis.csv",
                nrow(smiskis), length(unique(smiskis$collection))))
