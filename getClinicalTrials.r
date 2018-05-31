getCT <- function(sCond = "", sTerm = "", sResults = "", sType = "", sAgeNum = "", sAgeGroup = "", sGender = "", sInterv = "", sTitles = "", sOutcomes = "", sSponsors = "", sLeadSponsor = "", sID = "", sCountry = "", sFundType = "", sStartBegin = "", sStartEnd = "", perPage = 1000, fields = "all", fileFormat = "xml", startPage = 1, outfile) {
	message("Retrieving page ", startPage)
	theURL <- httr::GET("https://clinicaltrials.gov/ct2/results/download_fields", query = list(cond = sCond, term = sTerm, type = sType, rslt = sResults, age_v = sAgeNum, age = sAgeGroup, gndr = sGender, intr = sInterv, titles = sTitles, outc = sOutcomes, spons = sSponsors, lead = sLeadSponsor, id = sID, cntry = sCountry, fund = sFundType, strd_s = sStartBegin, strd_e = sStartEnd, down_count = perPage, down_flds = fields, down_fmt = fileFormat, down_chunk = startPage))
	theData <- httr::content(theURL, as = "text")
	if (httr::http_error(theURL) == TRUE) {
		message("HTTP error.")
		print(httr::http_status(theURL))
	}
	newData <- XML::xmlParse(theURL)
	resultCount <- as.numeric(XML::xpathSApply(newData, "//search_results", XML::xmlAttrs))
	pagesNeeded <- ceiling(resultCount / perPage)
	Sys.sleep(2)
	if (pagesNeeded > 1) {
	for (i in 2:pagesNeeded) {
		startPage <- i
		message("Retrieving page ", startPage, " of ", pagesNeeded)
		theURL <- httr::GET("https://clinicaltrials.gov/ct2/results/download_fields", query = list(cond = sCond, term = sTerm, type = sType, rslt = sResults, age_v = sAgeNum, age = sAgeGroup, gndr = sGender, intr = sInterv, titles = sTitles, outc = sOutcomes, spons = sSponsors, lead = sLeadSponsor, id = sID, cntry = sCountry, fund = sFundType, strd_s = sStartBegin, strd_e = sStartEnd, down_count = perPage, down_flds = fields, down_fmt = fileFormat, down_chunk = startPage))
		theData <- paste(theData, httr::content(theURL, as = "text"), sep = "\n")
		if (httr::http_error(theURL) == TRUE) {
			message("HTTP error.")
			print(httr::http_status(theURL))
		}
		Sys.sleep(2)
	}
	message("Finished retrieving pages. Formatting and saving results.")
	theData <- gsub("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "", theData, fixed = TRUE, useBytes = TRUE)
	theData <- gsub("<search_results.+?>", "", theData, useBytes = TRUE)
	theData <- gsub("</search_results>", "", theData, useBytes = TRUE)
	theData <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<search_results>", theData, "</search_results>", sep = "\n")
	}
	writeLines(theData, con = outfile)
	message("Done.")
	return(theData)
}

extractCT <- function(theFile) {
	library(XML)
	newData <- xmlParse(theFile)
	records <- getNodeSet(newData, "//study")
	#nctID <- xpathSApply(newData,"//nct_id", xmlValue)
	nctID <- lapply(records, xpathSApply, ".//nct_id", xmlValue)
	nctID[sapply(nctID, is.list)] <- NA
	nctID <- unlist(nctID)
	#studyTitle <- xpathSApply(newData,"//title", xmlValue)
	studyTitle <- lapply(records, xpathSApply, ".//title", xmlValue)
	studyTitle[sapply(studyTitle, is.list)] <- NA
	studyTitle <- unlist(studyTitle)
	#recruitment <- xpathSApply(newData,"//recruitment", xmlValue)
	recruitment <- lapply(records, xpathSApply, ".//recruitment", xmlValue)
	recruitment[sapply(recruitment, is.list)] <- NA
	recruitment <- unlist(recruitment)
	#studyResults <- xpathSApply(newData,"//study_results", xmlValue)
	studyResults <- lapply(records, xpathSApply, ".//study_results", xmlValue)
	studyResults[sapply(studyResults, is.list)] <- NA
	studyResults <- unlist(studyResults)
	conditions <- lapply(records, xpathSApply, ".//conditions/condition", xmlValue)
	conditions[sapply(conditions, is.list)] <- NA
	conditions <- sapply(conditions, paste, collapse = "|")
	interventionTypes <- lapply(records, xpathSApply, ".//interventions/intervention", xmlGetAttr, name = "type")
	interventionTypes[sapply(interventionTypes, is.list)] <- NA
	interventionTypes <- lapply(interventionTypes, unique)
	interventionTypes <- sapply(interventionTypes, paste, collapse = "|")
	interventions <- lapply(records, xpathSApply, ".//interventions/intervention", xmlValue)
	interventions[sapply(interventions, is.list)] <- NA
	interventions <- sapply(interventions, paste, collapse = "|")
	outcomeMeasures <- lapply(records, xpathSApply, ".//outcome_measures/outcome_measure", xmlValue)
	outcomeMeasures[sapply(outcomeMeasures, is.list)] <- NA
	outcomeMeasures <- sapply(outcomeMeasures, paste, collapse = "|")
	leadSponsor <- lapply(records, xpathSApply, ".//sponsors/lead_sponsor", xmlValue)
	leadSponsor[sapply(leadSponsor, is.list)] <- NA
	leadSponsor <- sapply(leadSponsor, paste, collapse = "|")
	allSponsors <- lapply(records, xpathSApply, ".//sponsors/*", xmlValue)
	allSponsors[sapply(allSponsors, is.list)] <- NA
	allSponsors <- sapply(allSponsors, paste, collapse = "|")
	gender <- lapply(records, xpathSApply, ".//gender", xmlValue)
	gender[sapply(gender, is.list)] <- NA
	gender <- unlist(gender)
	ageGroups <- lapply(records, xpathSApply, ".//age_groups/age_group", xmlValue)
	ageGroups[sapply(ageGroups, is.list)] <- NA
	ageGroups <- sapply(ageGroups, paste, collapse = "|")
	minAge <- lapply(records, xpathSApply, ".//min_age", xmlValue)
	minAge[sapply(minAge, is.list)] <- NA
	minAge <- unlist(minAge)
	maxAge <- lapply(records, xpathSApply, ".//max_age", xmlValue)
	maxAge[sapply(maxAge, is.list)] <- NA
	maxAge <- unlist(maxAge)
	phases <- lapply(records, xpathSApply, ".//phases/phase", xmlValue)
	phases[sapply(phases, is.list)] <- NA
	phases <- sapply(phases, paste, collapse = "|")
	enrollment <- lapply(records, xpathSApply, ".//enrollment", xmlValue)
	enrollment[sapply(enrollment, is.list)] <- NA
	enrollment <- as.numeric(unlist(enrollment))
	funders <- lapply(records, xpathSApply, ".//funded_bys/funded_by", xmlValue)
	funders[sapply(funders, is.list)] <- NA
	funders <- sapply(funders, paste, collapse = "|")
	studyType <- xpathSApply(newData,"//study_types", xmlValue)
	studyDesigns <- lapply(records, xpathSApply, ".//study_designs/study_design", xmlValue)
	studyDesigns[sapply(studyDesigns, is.list)] <- NA
	studyDesigns <- sapply(studyDesigns, paste, collapse = "|")
	otherIDs <- lapply(records, xpathSApply, ".//other_ids/other_id", xmlValue)
	otherIDs[sapply(otherIDs, is.list)] <- NA
	otherIDs <- sapply(otherIDs, paste, collapse = "|")
	startDate <- lapply(records, xpathSApply, ".//start_date", xmlValue)
	startDate[sapply(startDate, is.list)] <- NA
	startDate <- unlist(startDate)
	startYear <- as.numeric(gsub(".+ ", "", startDate))
	primaryCompleteDate <- lapply(records, xpathSApply, ".//primary_completion_date", xmlValue)
	primaryCompleteDate[sapply(primaryCompleteDate, is.list)] <- NA
	primaryCompleteDate <- unlist(primaryCompleteDate)
	primaryCompleteYear <- as.numeric(gsub(".+ ", "", primaryCompleteDate))
	completeDate <- lapply(records, xpathSApply, ".//completion_date", xmlValue)
	completeDate[sapply(completeDate, is.list)] <- NA
	completeDate <- unlist(completeDate)
	completeYear <- as.numeric(gsub(".+ ", "", completeDate))
	firstPostedDate <- lapply(records, xpathSApply, ".//study_first_posted", xmlValue)
	firstPostedDate[sapply(firstPostedDate, is.list)] <- NA
	firstPostedDate <- unlist(firstPostedDate)
	firstPostedYear <- as.numeric(gsub(".+ ", "", firstPostedDate))
	lastUpdatedDate <- lapply(records, xpathSApply, ".//last_update_posted", xmlValue)
	lastUpdatedDate[sapply(lastUpdatedDate, is.list)] <- NA
	lastUpdatedDate <- unlist(lastUpdatedDate)
	lastUpdatedYear <- as.numeric(gsub(".+ ", "", lastUpdatedDate))
	locations <- lapply(records, xpathSApply, ".//locations/location", xmlValue)
	locations[sapply(locations, is.list)] <- NA
	locations <- sapply(locations, paste, collapse = "|")
	#studyURL <- xpathSApply(newData,"//url", xmlValue)
	studyURL <- lapply(records, xpathSApply, ".//url", xmlValue)
	studyURL[sapply(studyURL, is.list)] <- NA
	studyURL <- sapply(studyURL, paste, collapse = "|")
	theDF <- data.frame(nctID, studyTitle, recruitment, studyResults, conditions, interventionTypes, interventions, outcomeMeasures, leadSponsor, allSponsors, gender, ageGroups, minAge, maxAge, phases, enrollment, funders, studyType, studyDesigns, otherIDs, startDate, primaryCompleteDate, completeDate, firstPostedDate, lastUpdatedDate, startYear, primaryCompleteYear, completeYear, firstPostedYear, lastUpdatedYear, locations, studyURL, stringsAsFactors = FALSE)
	return(theDF)
}

## note: the perPage value can only be set to 10, 100, 1000, or 10000; if set to 25, system rounds it up to 100

## use:
## influenza <- getCT("influenza", "influenza.xml")
## influenza <- extractCT(influenza)