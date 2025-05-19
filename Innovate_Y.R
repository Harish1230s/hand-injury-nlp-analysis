library(data.table)
install.packages('bit64')
library(bit64)
install.packages("tidygeocoder")
library(tidygeocoder)

dt <- fread("Hackathon Dataset.csv", stringsAsFactors = FALSE)

################### Grouping Hand filter ###########################
hand_keywords <- c("hand", "finger", "fingertips", "elbow", "arm", "wrist", "forearm", "fingernail", "thumb", "palm", "upper arm")
pattern <- paste0("\\b(", paste(hand_keywords, collapse = "|"), ")\\b")
# Add new column
dt[, bodypartclass := NA_character_]
# Safe row-wise scan with encoding fix
dt[, bodypartclass := {
  # Combine all relevant columns into a single string for each row
  row_text <- paste(.SD, collapse = " ")
  # Clean encoding: convert to UTF-8, replace invalid characters
  clean_text <- iconv(row_text, from = "", to = "UTF-8", sub = " ")
  # Apply lowercase and search
  if (grepl(pattern, tolower(clean_text), ignore.case = TRUE)) "Hand" else NA_character_
}, by = .I, .SDcols = cols_to_check]
# View result
print(dt)

###################### Uniform Column Names ################################
setnames(dt, tolower(gsub(" ", "", names(dt))))
print(dt)

##################### Selecting Hand and Manufacturing Data ###############
dt_hand_mfg <- dt[
  bodypartclass == "Hand" & grepl("Manufacturing", naicsdesc, ignore.case = TRUE)
]
# View the new filtered data.table
print(dt_hand_mfg)

####################### Standardize Date Columns #############################
dt_hand_mfg[, eventdate := as.Date(eventdate, format = "%m/%d/%Y")] 

####################### Categorical Column: Injury_Category and severity #############################

injurytype <- unique(dt_hand_mfg$naturetitle)
injurytype

dt_hand_mfg[, injurycategory := fcase(
  grepl("amputat|severed", naturetitle, ignore.case = TRUE), "Amputation",
  grepl("avulsion|enucleation", naturetitle, ignore.case = TRUE), "Avulsion/Enucleation",
  grepl("crush|crushing", naturetitle, ignore.case = TRUE), "Crushing Injury",
  grepl("fracture|broken|crack", naturetitle, ignore.case = TRUE), "Fracture",
  grepl("dislocation", naturetitle, ignore.case = TRUE), "Dislocation",
  grepl("sprain|strain|tear", naturetitle, ignore.case = TRUE), "Sprain/Strain",
  grepl("trauma|traumatic", naturetitle, ignore.case = TRUE), "Traumatic Injury",
  grepl("cut|laceration|slice", naturetitle, ignore.case = TRUE), "Laceration",
  grepl("bruise|contusion", naturetitle, ignore.case = TRUE), "Contusion",
  grepl("puncture wound", naturetitle, ignore.case = TRUE), "Puncture Wound",
  grepl("burn|scald|thermal|chemical|electrical", naturetitle, ignore.case = TRUE), "Burn",
  grepl("sore|soreness|pain|hurt|inflammation|swelling", naturetitle, ignore.case = TRUE), "Soreness/Pain",
  grepl("poison|toxic|noxious|allergen", naturetitle, ignore.case = TRUE), "Poisoning",
  grepl("infection|cellulitis|abscess", naturetitle, ignore.case = TRUE), "Infection",
  grepl("paralysis|paraplegia|quadriplegia", naturetitle, ignore.case = TRUE), "Paralysis/Nerve",
  grepl("blister|rash|irritation", naturetitle, ignore.case = TRUE), "Skin/Surface Issue",
  grepl("concussion", naturetitle, ignore.case = TRUE), "Concussion",
  grepl("unspecified|n\\.e\\.c|unknown|nonclassifiable", naturetitle, ignore.case = TRUE), "Nonclassifiable/Other",
  default = "Other"
)]

dt_hand_mfg[, severitylevel := fcase(
  injurycategory %in% c("Amputation", "Avulsion/Enucleation", "Crushing Injury", "Fracture", "Paralysis/Nerve", "Poisoning", "Concussion"), "High",
  injurycategory %in% c("Burn", "Traumatic Injury", "Dislocation", "Sprain/Strain", "Laceration", "Puncture Wound", "Infection"), "Medium",
  injurycategory %in% c("Contusion", "Soreness/Pain", "Skin/Surface Issue", "Nonclassifiable/Other", "Other"), "Low"
)]

View(dt_hand_mfg)

####################### Categorical Column: Source_Category #############################
sourcetype <- unique(dt_hand_mfg$sourcetitle)
sourcetype

dt_hand_mfg[, sourcecategory := fcase(
  grepl("saw|drill|press|grinder|molding|extruding|machinery|lathe|boring|shearing", sourcetitle, ignore.case = TRUE), "Powered Machinery",
  grepl("handtool|knife|screwdriver|wrench|hammer|chisel|pliers|snips|sledge|axe|file", sourcetitle, ignore.case = TRUE), "Hand Tools",
  grepl("forklift|conveyor|hoist|jack|dolly|pallet|cart|loader|crane|lift", sourcetitle, ignore.case = TRUE), "Material Handling Equipment",
  grepl("truck|van|car|automobile|vehicle|atv|tractor|bus|motorboat|barge|train", sourcetitle, ignore.case = TRUE), "Vehicles & Mobile Equipment",
  grepl("floor|roof|wall|ceiling|stair|door|ladder|scaffold|platform|window|catwalk", sourcetitle, ignore.case = TRUE), "Building Structures",
  grepl("ground|sidewalk|walkway|ramp|irregularity|surface", sourcetitle, ignore.case = TRUE), "Workplace Surfaces",
  grepl("metal|pipe|rod|sheet|bar|beam|steel|rebar|grate", sourcetitle, ignore.case = TRUE), "Metal Objects & Materials",
  grepl("acid|solvent|chemical|compound|resin|fuel|gas|paint|glue|alcohol|vapors|oxygen|detergent|degreaser", sourcetitle, ignore.case = TRUE), "Chemical Substances",
  grepl("electric|electrical|transformer|wiring|cord|switch|fuse|light|power line", sourcetitle, ignore.case = TRUE), "Electrical Components",
  grepl("bottle|box|can|drum|container|wrapping|packaging|bin|jug|carton", sourcetitle, ignore.case = TRUE), "Containers & Packaging",
  grepl("table|desk|chair|cabinet|bench|shelf|rack|fixture|furniture", sourcetitle, ignore.case = TRUE), "Furniture & Fixtures",
  grepl("printing|stamping|bottling|forming|labeling|sealing|production|manufacturing", sourcetitle, ignore.case = TRUE), "Industrial Tools",
  grepl("dog|cat|animal|insect|spider|fish|bovine|porcine|snakes", sourcetitle, ignore.case = TRUE), "Animals & Insects",
  grepl("heat|cold|snow|water|ice|fire|tree|rock|terrain|natural", sourcetitle, ignore.case = TRUE), "Environmental/Natural",
  grepl("co-worker|person|suspect|assailant|student|work associate|client", sourcetitle, ignore.case = TRUE), "Person-Related",
  grepl("unspecified|n\\.e\\.c\\.|unknown|other", sourcetitle, ignore.case = TRUE), "Unspecified/Other",
  default = "Other"
)]

View(dt_hand_mfg)

################################ Count missing (NA) values per column#####################
missing_summary <- dt_hand_mfg[, lapply(.SD, function(x) sum(is.na(x)))]
print(missing_summary)


######################### Missing Cities ###########################################################
#Check which ones were empty
dt_hand_mfg[is.na(city) | trimws(city) == ""]
dt_hand_mfg[upa == "1085063"]

missing_city <- dt_hand_mfg[is.na(city) | trimws(city) == "", .(row_id = .I, upa, latitude, longitude)]

# Convert to a data.frame for tidygeocoder compatibility
geo_input <- as.data.frame(missing_city)

geo_results <- reverse_geocode(
  .tbl = geo_input,
  lat = latitude,
  long = longitude,
  method = "osm",
  full_results = FALSE,  # Only returns a simple address string
  address = "city_result"
)

geo_results$city_clean <- sapply(strsplit(geo_results$city_result, ","), function(parts) {
  parts <- trimws(parts)  # clean up whitespace
  
  # Try 3rd or 4th part, depending on address depth
  if (length(parts) >= 8) {
    return(parts[4])  # e.g., "Buffalo"
  } else if (length(parts) <= 7) {
    return(parts[3])
  } else {
    return(NA_character_)
  }
})

# Assign result to city
# First, create a lookup table that has UPA and resolved city
city_lookup <- missing_city[, .(upa, city_clean = geo_results$city_clean)]

# Set keys for join (optional but efficient)
setkey(dt_hand_mfg, upa)
setkey(city_lookup, upa)

# Update city in dt_hand_mfg where upa matches
dt_hand_mfg[city_lookup, city := i.city_clean]

#Drop if there's no lat and long
dt_hand_mfg[(is.na(city) | trimws(city) == "") & latitude == 0 & longitude == 0]
dt_hand_mfg <- dt_hand_mfg[!( (is.na(city) | trimws(city) == "") & latitude == 0 & longitude == 0 )]

#######################Amputation missing values############################
cols_to_check <- setdiff(names(dt), "amputation")
dt_hand_mfg[, amputation_found_in := NA_character_]

dt_hand_mfg[is.na(amputation) | amputation == 0, c("amputation", "amputation_found_in") := {
  found_in <- NA_character_
  for (col in cols_to_check) {
    val <- get(col)
    if (!is.na(val) && grepl("amputation", val, ignore.case = TRUE)) {
      found_in <- col
      break
    }
  }
  if (!is.na(found_in)) list(1, found_in) else list(0, NA_character_)
}, by = .I]

View(dt_hand_mfg)


################## Required Data #######################################
cols_to_exclude <- c(
  "source", "address2", "zip", "latitude", "longitude", "inspection",
  "nature", "partofbody", "event", "secondarysource", "secondarysourcetitle",
  "federalstate", "generalsecondarysource", "primarynaics", "amputation_found_in"
)

# Subset by excluding the columns listed above
dt_subset <- dt_hand_mfg[, !cols_to_exclude, with = FALSE]

fwrite(dt_subset,
       file = "Updated_Hackathon_dataset.csv")

################## NLP #######################################
install.packages(c("tm","topicmodels", "dplyr"))
library(tm)
library(topicmodels)
library(dplyr)

# Step 1: Filter your dataset
df_hand$finalnarrative <- iconv(df_hand$finalnarrative, from = "", to = "UTF-8", sub = "byte")

# Step 2: Build and clean corpus
corpus <- Corpus(VectorSource(df_hand$finalnarrative))
corpus_clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

# Step 3: Create Document-Term Matrix and reduce sparsity
dtm <- DocumentTermMatrix(corpus_clean)
dtm_sparse <- removeSparseTerms(dtm, 0.99)

# Step 4: Train LDA model
num_topics <- 5
lda_model <- LDA(dtm_sparse, k = num_topics, control = list(seed = 1234))

# Step 5: Define topic labels
topic_labels <- c(
  "Caught in Press",
  "Lathe Entanglement",
  "Chemical Exposure",
  "Tool Handling",
  "Blade/Sharp Object Incidents"
)

# Step 6: Define mapping to injury types and sources
topic_to_injury <- list(
  "Caught in Press" = list(type = "Crushing Injury", source = "Press Machine"),
  "Lathe Entanglement" = list(type = "Laceration", source = "Rotating Machinery"),
  "Chemical Exposure" = list(type = "Burn", source = "Chemicals"),
  "Tool Handling" = list(type = "Fracture", source = "Hand Tools"),
  "Blade/Sharp Object Incidents" = list(type = "Cut/Laceration or Amputation", source = "Sharp Object (e.g., Saw)")
)

# Step 7: Reusable functions to clean, predict, and categorize
clean_text <- function(text) {
  corpus <- Corpus(VectorSource(text))
  corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace)
}

get_dtm_for_text <- function(text) {
  corpus_new <- clean_text(text)
  DocumentTermMatrix(corpus_new, control = list(dictionary = Terms(dtm_sparse)))
}

predict_topic <- function(text) {
  dtm_new <- get_dtm_for_text(text)
  topic_probs <- posterior(lda_model, dtm_new)$topics
  assigned_topic <- which.max(topic_probs)
  topic_labels[assigned_topic]
}

predict_and_categorize <- function(text) {
  topic <- predict_topic(text)
  info <- topic_to_injury[[topic]]
  list(
    narrative = text,
    topic = topic,
    injury_type = info$type,
    injury_source = info$source
  )
}

# Step 8: Example use — single narrative
narrative <- "Employee was injured while operating a metal press. Hand was crushed between dies."
result <- predict_and_categorize(narrative)
print(result)

# Step 9: Bulk classify multiple narratives
narratives <- c(
  "Operator was adjusting the lathe when glove got caught.",
  "Chemical exposure occurred during cleaning with acid.",
  "Employee cut finger on saw blade.",
  "Fingers were crushed by press machine during stamping."
)

results <- lapply(narratives, predict_and_categorize)
df_results <- do.call(rbind, lapply(results, as.data.frame))
print(df_results)