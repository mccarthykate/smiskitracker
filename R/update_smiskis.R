# Rebuild app/smiskis.csv and images/ from the Smiski Google Sheet.
#
# Run this from the project root (open smiskitracker.Rproj, then
# source("R/update_smiskis.R")) whenever new figures are added to the sheet.
# Then commit and push: the site republishes automatically.
#
# Needs the readxl and xml2 packages. On a Mac, pictures are shrunk with the
# built-in `sips` tool; elsewhere they are copied at full size.

library(xml2)

sheet_id <- "1U4l4ugaL8UG7wzEfQqQ1h_oYYX0s0tGpqw9XV_cxnms"
sheet_url <- sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=xlsx", sheet_id)
image_dir <- "images"
max_px <- 240

# ---- Download and unpack the workbook ----------------------------------------

xlsx <- tempfile(fileext = ".xlsx")
unz <- tempfile("sheet-")
download.file(sheet_url, xlsx, mode = "wb", quiet = TRUE)
unzip(xlsx, exdir = unz)

xml <- function(...) {
  x <- read_xml(file.path(unz, ...))
  xml_ns_strip(x)
  x
}
node <- function(name) sprintf("*[local-name()='%s']", name)
rels_of <- function(x) {
  r <- xml_find_all(x, paste0("//", node("Relationship")))
  setNames(xml_attr(r, "Target"), xml_attr(r, "Id"))
}

# Find the checklist tab ("4. Checklist") and its XML file
wb <- xml("xl", "workbook.xml")
sheets <- xml_find_all(wb, paste0("//", node("sheet")))
sheet_names <- xml_attr(sheets, "name")
pick <- grep("checklist", sheet_names, ignore.case = TRUE)
pick <- pick[!grepl("secret", sheet_names[pick], ignore.case = TRUE)][1]
stopifnot(!is.na(pick))
sheet_rid <- xml_attr(sheets[[pick]], "id")
sheet_file <- basename(rels_of(xml("xl", "_rels", "workbook.xml.rels"))[[sheet_rid]])

# ---- Cell values ------------------------------------------------------------

raw <- readxl::read_excel(
  xlsx, sheet = sheet_names[pick], col_names = FALSE, range = "A1:Z500",
  col_types = "text", .name_repair = "minimal"
)
raw <- as.data.frame(raw)
raw[is.na(raw)] <- ""

# ---- Picture positions ------------------------------------------------------

ws <- xml("xl", "worksheets", sheet_file)
def_ht <- as.numeric(xml_attr(xml_find_first(ws, paste0("//", node("sheetFormatPr"))), "defaultRowHeight"))
row_nodes <- xml_find_all(ws, paste0("//", node("row")))
row_ht <- setNames(as.numeric(xml_attr(row_nodes, "ht")), xml_attr(row_nodes, "r"))
row_emu <- function(r) {
  h <- row_ht[as.character(r)]
  (if (is.na(h)) def_ht else h) * 12700
}

ws_rels <- rels_of(xml("xl", "worksheets", "_rels", paste0(sheet_file, ".rels")))
drawing_file <- basename(ws_rels[grepl("drawings/drawing", ws_rels)][1])
drawing <- xml("xl", "drawings", drawing_file)
drawing_rels <- rels_of(xml("xl", "drawings", "_rels", paste0(drawing_file, ".rels")))

anchors <- xml_find_all(drawing, "//*[local-name()='oneCellAnchor' or local-name()='twoCellAnchor']")
pictures <- do.call(rbind, lapply(anchors, function(a) {
  from <- xml_find_first(a, paste0(".//", node("from")))
  col <- as.integer(xml_text(xml_find_first(from, node("col"))))
  row0 <- as.integer(xml_text(xml_find_first(from, node("row"))))
  y <- as.numeric(xml_text(xml_find_first(from, node("rowOff"))))
  cy <- as.numeric(xml_attr(xml_find_first(a, paste0(".//", node("ext"))), "cy"))
  embed <- xml_attr(xml_find_first(a, paste0(".//", node("blip"))), "embed")
  if (is.na(embed)) return(NULL)
  # The picture belongs to the row holding its vertical centre
  y <- y + (if (is.na(cy)) 0 else cy / 2)
  r <- row0 + 1
  while (y > row_emu(r)) {
    y <- y - row_emu(r)
    r <- r + 1
  }
  data.frame(row = r, col = col + 1, media = basename(drawing_rels[[embed]]))
}))
picture_at <- function(r, c) {
  m <- pictures$media[pictures$row == r & pictures$col == c]
  if (length(m)) m[1] else NA_character_
}

# ---- Clean names ------------------------------------------------------------

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

# ---- Walk the collection blocks ---------------------------------------------
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
      collection = collection, figure = fig$name, secret = fig$secret,
      media = picture_at(k, c + 1)
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

# ---- Pictures ---------------------------------------------------------------
# One file per distinct picture, named after the first figure that uses it
# (the strap series reuse the main series' pictures).

unlink(image_dir, recursive = TRUE)
dir.create(image_dir)
has_sips <- nzchar(Sys.which("sips"))
smiskis$image <- ""
seen <- character()
for (i in which(!is.na(smiskis$media))) {
  m <- smiskis$media[i]
  if (!m %in% names(seen)) {
    out <- paste0(smiskis$id[i], ".", tolower(tools::file_ext(m)))
    file.copy(file.path(unz, "xl", "media", m), file.path(image_dir, out))
    if (has_sips) {
      system2("sips", c("-Z", max_px, shQuote(file.path(image_dir, out))),
              stdout = FALSE, stderr = FALSE)
    }
    seen[m] <- out
  }
  smiskis$image[i] <- seen[[m]]
}

with_pic <- smiskis[smiskis$image != "", ]
shared <- tapply(paste(with_pic$collection, with_pic$figure, sep = ": "), with_pic$image,
                 function(f) if (length(unique(sub(".*: ", "", f))) > 1) paste(f, collapse = " / ") else NA)
shared <- shared[!is.na(shared)]
if (length(shared)) {
  message("Note: these figures share one picture in the sheet: ", paste(shared, collapse = "; "))
}
missing_pics <- smiskis$figure[smiskis$image == ""]
if (length(missing_pics)) message("No picture found for: ", paste(missing_pics, collapse = ", "))

smiskis <- smiskis[, c("id", "collection", "figure", "secret", "image")]
write.csv(smiskis, "app/smiskis.csv", row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("Wrote %d figures in %d collections to app/smiskis.csv, %d pictures to %s/",
                nrow(smiskis), length(unique(smiskis$collection)),
                length(list.files(image_dir)), image_dir))
