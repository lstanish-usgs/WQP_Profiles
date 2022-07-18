##
## Script name: map_WQX3.0_csv_profiles
##
## Purpose of script: create a master table mapping WQX 3.0 profiles to .csv field names and order. Note: this script works for 
## most profiles, except for the Site and Organization profiles, which pull location metadata from a different source and therefore
## require a different mapping.
##
## Author: Lee F. Stanish
##
## Date Created: 2022-04-27
##
## Copyright (c) Lee Stanish, 2022
## Email: lstanish@usgs.gov
##
## ---------------------------
##

#### SET-UP: RUN FOR ALL SECTIONS ####
options(stringsAsFactors = FALSE)

library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
#### END SET-UP ####


#### SECTION 1: Results and Activity Level Profiles ####
#dir <- '/Users/lstanish/OneDrive - DOI/Documents/WQX/New_WQX3.0_WQP_Profiles_All/' # old directory to final profiles
dir <- '/Users/lstanish/Documents/GitHub/WQP_Profiles/'
fileList <- list.files(paste0(dir, "/WQX3.0_WQP_Profiles_All"), pattern="Final", full.names = T) 
# remove Site and Org profiles
fileList <- fileList[-grep("(?i)site|(?i)organization", fileList)]
#fileListAbbr <- list.files(paste0(dir,'/WQX3.0_WQP_Profiles_All'), pattern="Final", full.names = F)
det3 <- read.csv(paste0(dir,'/WQX_DET_v3.0.csv')) # det3.0 file
csvMapping <- read.csv(paste0(dir,'/WQX3.0_csv_fieldNames_andOrder_ResultsAndActivityProfiles.csv')) # manually-curated list of EPA-submitted field names for csv and order of fields for most 3.0 profiles (excluding Org and Site)
det3Mapping <- read.csv(paste0(dir,'/WQX3.0_mapping_final_20220404.csv')) # det3.0 file with EPA dump file mappings

# pull out profile name from file name
fileName <- gsub(".*/([a-zA-Z]+)_([a-zA-Z]+)_.*$", "\\2", fileList)

# remove extraneous fields from det3
det3 <- det3[, -grep("(?i)biodata", names(det3))]

for(i in 1:length(fileList)) {
  tmp <- read_excel(fileList[i])
  tmp$FileName <- fileName[i]
  #print(fileList[i])
  #test <- grepl("GroupSummaryWeightMeasureUnitCode", tmp)
  #print(test)
  if(i==1) {
    joined <- tmp
  } else {
    joined <- full_join(joined, tmp, by=c("ProfileFieldName", "FileName") )
  }
}

# widen the profiles table
joinedWide <- pivot_wider(data = joined, names_from = FileName, values_from = FileName)

# add leading zeroes to csvMapping and det3 Index fields (for some reason they won't stick)
csvMapping$Sort.Within.Category.Schema.Location[grep("^4.01", csvMapping$Sort.Within.Category.Schema.Location)] <- "04.01"
csvMapping$Sort.Within.Category.Schema.Location[grep("^4.03", csvMapping$Sort.Within.Category.Schema.Location)] <- "04.03"
csvMapping$Sort.Within.Category.Schema.Location[grep("^4.04", csvMapping$Sort.Within.Category.Schema.Location)] <- "04.04"
csvMapping$Sort.Within.Category.Schema.Location[grep("^4.05", csvMapping$Sort.Within.Category.Schema.Location)] <- "04.05"
csvMapping$Sort.Within.Category.Schema.Location[grep("^4.06", csvMapping$Sort.Within.Category.Schema.Location)] <- "04.06"
det3Mapping$Sort.Within.Category.Schema.Location[grep("^4.01", det3Mapping$Sort.Within.Category.Schema.Location)] <- "04.01"


# merge the det3.0 file with the csvMapping file and pray it works!
names(det3)
names(csvMapping)
schemaJoined <- full_join(det3, csvMapping, by=c('Sort.Within.Category.Schema.Location',
                                                 "Data.Element.XML.Tag"))

# merge the schema joined with the det mapping file and hope it works!
names(schemaJoined)
names(det3Mapping)
schemaMappedJoined <- full_join(schemaJoined, det3Mapping)


# merge the joined det table with the profile table and hope it works!
names(joinedWide)
names(schemaMappedJoined)
joinedCsvAndDet3 <- full_join(schemaMappedJoined, joinedWide, by=c("csvFieldName"="ProfileFieldName"))

# counts check to make sure all fields for each profile got mapped #
for(i in 1:length(fileList)) {
  tmp <- read_excel(fileList[i])
  if(i==1) {
    nFields <- nrow(tmp)
  } else {
    nFields <- c(nFields, nrow(tmp) )
  }
}
profileColInd <- which(names(joinedCsvAndDet3)=="ActivityMetric")
profileColEnd <- ncol(joinedCsvAndDet3)
for(a in profileColInd:profileColEnd) {
  tmp <- sum(!is.na(joinedCsvAndDet3[,a]))
  if(a==profileColInd) {
    nMappedFields <- tmp
  } else {
    nMappedFields <- c(nMappedFields, tmp)
  }
}
nFields-nMappedFields
# [1] 0 0 0 0 0 0 0 0 0, looks good, same number of fields in as out


# export mapping file #
write.csv(joinedCsvAndDet3, '/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_csv_mapping_template_ResultsAndActivityProfiles.csv',
          row.names = FALSE, na = "")


# create standardized field names

finalMapping <- joinedCsvAndDet3
# re-order object by schema order, rownumber and fieldorder
finalMapping <- finalMapping[order(finalMapping$Sort.Within.Category.Schema.Location, finalMapping$RowOrder), ] 
                                           
# add a field for storing the standardized csv field name
finalMapping$FinalCsvFieldNameStandardized <- ""
rowsToCheck <- which(!is.na(finalMapping$RowOrder)==TRUE)
hierarchInd <- which(str_count(finalMapping$Sort.Within.Category.Schema.Location, "\\.") >= 2)

for(b in rowsToCheck) {
  if(finalMapping$HeaderOrField[b]=="header") {
    header <- finalMapping$Data.Element.XML.Tag[b]
    #print(header)
  }
  if(b %in% hierarchInd) {
    finalMapping$FinalCsvFieldNameStandardized[b] <- paste(header, finalMapping$Data.Element.XML.Tag[b], sep="/")
  } else {
    finalMapping$FinalCsvFieldNameStandardized[b] <- finalMapping$csvFieldName[b]
  }
}


### Manual clean-up ###
# append _1, _2, _3 to FrequencyClassDescriptorCode and UnitCode
freqCode <- grep("FrequencyClassDescriptorCode", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[freqCode] <- paste(finalMapping$FinalCsvFieldNameStandardized[freqCode], 1:3, sep="_")
freqUnit <- grep("FrequencyClassDescriptorUnitCode", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[freqUnit] <- paste(finalMapping$FinalCsvFieldNameStandardized[freqUnit], 1:3, sep="_")
freqLBound <- grep("LowerClassBoundValue", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[freqLBound] <- paste(finalMapping$FinalCsvFieldNameStandardized[freqLBound], 1:3, sep="_")
freqUBound <- grep("UpperClassBoundValue", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[freqUBound] <- paste(finalMapping$FinalCsvFieldNameStandardized[freqUBound], 1:3, sep="_")

# remove unallowable characters (only know of spaces and #)
finalMapping$FinalCsvFieldNameStandardized <- gsub(" ", "", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized <- gsub("#", "_", finalMapping$FinalCsvFieldNameStandardized)
# fix wonky header name for HabitatSelectionMethod and ToxicityTestType
finalMapping$FinalCsvFieldNameStandardized <- gsub('CurrentSpeedMeasure/HabitatSelectionMethod', 
                                              "BiologicalActivityDescription/HabitatSelectionMethod", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized <- gsub('CurrentSpeedMeasure/ToxicityTestType', 
                                              "BiologicalActivityDescription/ToxicityTestType", finalMapping$FinalCsvFieldNameStandardized)
# map field name ActivityLocation/HorizontalCoordinateReferenceSystemDatumName, to the schema mapping in 05.01.06
# necessary so that this field appears in the right order, grouped with the lat/lon values
indToMap <- which(finalMapping$Sort.Within.Category.Schema.Location=="05.01.02outbound")
finalMapping$Data.Type[indToMap] <- "From domain values string min 1 string max 6"
finalMapping$Schema.3.0.File.Name[indToMap] <- "WQX_ActivityLocation_v3.0.xsd"
finalMapping$Tables.and.Columns.Foreign.KEYS[indToMap] <- "WQX.HORIZONTAL_REFERENCE_DATUM (WQX.ACTIVITY.HRDAT_UID)"
finalMapping$WQX.Table.Name[indToMap] <- "HORIZONTAL_REFERENCE_DATUM"
finalMapping$WQX.Field.Name[indToMap] <- "HRDAT_NAME"

# map CASnumber, which doesn't exist in the WQX 3.0 schema
indCAS <- which(finalMapping$Data.Element.XML.Tag=="CASNumber")
finalMapping$WQX.Table.Name[indCAS] <- 'CHARACTERISTIC'
finalMapping$WQX.Field.Name[indCAS] <- 'CHR_CAS_NUMBER'

# fix name of AlternateMonitoringLocation counts field
finalMapping$FinalCsvFieldNameStandardized[grep("AlternateMonitoringLocationIdentity/MonitoringLocationIdentifier",
                                           finalMapping$FinalCsvFieldNameStandardized)] <- 
  "AlternateMonitoringLocationIdentity/MonitoringLocationIdentifierCount"

### End Manual Clean-up ###


# check for duplicate final field names
finalFieldNames <- finalMapping$FinalCsvFieldNameStandardized[-which(finalMapping$FinalCsvFieldNameStandardized=="")]
any(duplicated(finalFieldNames) )  # FALSE, all good
finalFieldNames[which(duplicated(finalFieldNames))]


# re-order object by rownumber and delivery order
finalMapping <- finalMapping[order(finalMapping$RowOrder, finalMapping$DeliveryOrder), ] 

# compare lengths of epa and usgs field names
min(nchar(finalMapping$csvFieldName), na.rm = T)
max(nchar(finalMapping$csvFieldName), na.rm = T) #61
min(nchar(finalMapping$FinalCsvFieldNameStandardized), na.rm = T)
max(nchar(finalMapping$FinalCsvFieldNameStandardized), na.rm = T) #77


# export mapping file #
finalMapping %>% rename('EpaSuppliedCsvFieldName'='csvFieldName') %>%
  select(c('Sub.Category', 'Sort.Within.Category.Schema.Location', 'Data.Element.XML.Tag', 'Lookup.Table.Needed',
                          'Schema.3.0.File.Name', 'DeliveryOrder', 'Tables.and.Columns.Foreign.KEYS', 
                          'WQX.Table.Name', 'WQX.Field.Name', 'WQX.Mapping.Comments', 'ActivityMetric',
                          'BasicBiological', 'BasicPhysicalChemical', 'BiologicalHabitatIndex', 'FullBiological',
                          'FullPhysicalChemical', 'ResultDetectionQuantitationLimit', 'SamplingActivity',
                          'FinalCsvFieldNameStandardized', 'EpaSuppliedCsvFieldName')) %>%
  filter(!is.na(DeliveryOrder)) %>%
  write.csv(paste0(dir,'WQX3.0_csv_final_mapping_template_ResultsAndActivityProfiles.csv'),
          row.names = FALSE, na = "")

#### END SECTION 1: Results and Activity Level Profiles ####



#### SECTION 2: Site Profile ####

dir <- '/Users/lstanish/OneDrive - DOI/Documents/WQX/New_WQX3.0_WQP_Profiles_All/' # directory to final profiles
fileList <- list.files(dir, pattern="Final", full.names = T) 
# keep Site profile only
fileList <- fileList[grep("(?i)site", fileList)]
det3 <- read.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX_DET_v3.0.csv') # det3.0 file
csvMapping <- read.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_csv_fieldNames_andOrder_SiteProfile.csv') # manually-curated list of EPA-submitted field names for csv and order of fields for Site 3.0 profile
det3Mapping <- read.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_mapping_final_20220404.csv') # det3.0 file with EPA dump file mappings

# pull out profile name from file name
fileName <- "Site"

# remove extraneous fields from det3
det3 <- det3[, -grep("(?i)biodata", names(det3))]

joined <- read_excel(fileList[1])
joined$FileName <- fileName[1]
print(fileList[1])

# for consistency with Section 1, re-name object to match previous Section
joinedWide <- joined %>% dplyr::rename(Site=FileName)

# merge the det3.0 file with the csvMapping file and pray it works!
names(det3)
names(csvMapping)
schemaJoined <- full_join(det3, csvMapping, by=c('Sort.Within.Category.Schema.Location',
                                                 "Data.Element.XML.Tag"))

# merge the schema joined with the det mapping file and hope it works!
names(schemaJoined)
names(det3Mapping)
schemaMappedJoined <- full_join(schemaJoined, det3Mapping)


# merge the joined det table with the profile table and hope it works!
names(joinedWide)
names(schemaMappedJoined)
joinedCsvAndDet3 <- full_join(schemaMappedJoined, joinedWide, by=c("csvFieldName"="ProfileFieldName"))

# counts check to make sure all fields for each profile got mapped #
nFields <- nrow(joined)
nMappedFields <- sum(!is.na(joinedCsvAndDet3$Site))
nFields-nMappedFields
# [1] 0, looks good, same number of fields in as out


# export mapping file #
write.csv(joinedCsvAndDet3, '/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_csv_profiles_mapping_template_site.csv',
          row.names = FALSE, na = "")


# create standardized field names

finalMapping <- joinedCsvAndDet3
# re-order object by schema order, rownumber and fieldorder
finalMapping <- finalMapping[order(finalMapping$Sort.Within.Category.Schema.Location, finalMapping$RowOrder), ] 

# add a field for storing the standardized csv field name
finalMapping$FinalCsvFieldNameStandardized <- ""
rowsToCheck <- which(!is.na(finalMapping$RowOrder)==TRUE)
hierarchInd <- which(str_count(finalMapping$Sort.Within.Category.Schema.Location, "\\.") >= 2)

for(b in rowsToCheck) {
  if(finalMapping$HeaderOrField[b]=="header") {
    header <- finalMapping$Data.Element.XML.Tag[b]
    #print(header)
  }
  if(b %in% hierarchInd) {
    finalMapping$FinalCsvFieldNameStandardized[b] <- paste(header, finalMapping$Data.Element.XML.Tag[b], sep="/")
  } else {
    finalMapping$FinalCsvFieldNameStandardized[b] <- finalMapping$csvFieldName[b]
  }
}


### Manual clean-up ###
# append _1, _2, _3 to AlternateMonitoringLocationIdentity/MonitoringLocationIdentifier and Context fields
idCode <- grep("AlternateMonitoringLocationIdentity/MonitoringLocationIdentifier$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[idCode] <- paste(finalMapping$FinalCsvFieldNameStandardized[idCode], 1:3, sep="_")
idContext <- grep("AlternateMonitoringLocationIdentity/MonitoringLocationIdentifierContext", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[idContext] <- paste(finalMapping$FinalCsvFieldNameStandardized[idContext], 1:3, sep="_")

# remove unallowable characters (only know of spaces and #)
finalMapping$FinalCsvFieldNameStandardized <- gsub(" ", "", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized <- gsub("#", "_", finalMapping$FinalCsvFieldNameStandardized)
# fix wonky header name for HabitatSelectionMethod and ToxicityTestType
finalMapping$FinalCsvFieldNameStandardized <- gsub('CurrentSpeedMeasure/HabitatSelectionMethod', 
                                              "BiologicalActivityDescription/HabitatSelectionMethod", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized <- gsub('CurrentSpeedMeasure/ToxicityTestType', 
                                              "BiologicalActivityDescription/ToxicityTestType", finalMapping$FinalCsvFieldNameStandardized)
### End Manual Clean-up ###


# check for duplicate final field names
finalFieldNames <- finalMapping$FinalCsvFieldNameStandardized[-which(finalMapping$FinalCsvFieldNameStandardized=="")]
any(duplicated(finalFieldNames) )  # FALSE, all good
finalFieldNames[which(duplicated(finalFieldNames))]

# re-order object by rownumber and delivery order
finalMapping <- finalMapping[order(finalMapping$RowOrder, finalMapping$DeliveryOrder), ] 

# compare lengths of epa and usgs field names
min(nchar(finalMapping$csvFieldName), na.rm = T)
max(nchar(finalMapping$csvFieldName), na.rm = T) #61
min(nchar(finalMapping$FinalCsvFieldNameStandardized), na.rm = T)
max(nchar(finalMapping$FinalCsvFieldNameStandardized), na.rm = T) #77


# export mapping file #
finalMapping %>% rename('EpaSuppliedCsvFieldName'='csvFieldName') %>%
  select(c('Sub.Category', 'Sort.Within.Category.Schema.Location', 'Data.Element.XML.Tag', 'Lookup.Table.Needed',
           'Schema.3.0.File.Name', 'DeliveryOrder', 'Tables.and.Columns.Foreign.KEYS', 
           'WQX.Table.Name', 'WQX.Field.Name', 'WQX.Mapping.Comments', 'Site',
           'FinalCsvFieldNameStandardized', 'EpaSuppliedCsvFieldName')) %>%
  filter(!is.na(DeliveryOrder)) %>%
  write.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_csv_final_mapping_template_SiteProfile.csv',
            row.names = FALSE, na = "")
#### END SECTION 2: Site Level Profile ####



#### INCOMPLETE SECTION 3: Organization Profile ####

dir <- '/Users/lstanish/OneDrive - DOI/Documents/WQX/New_WQX3.0_WQP_Profiles_All/' # directory to final profiles
fileList <- list.files(dir, pattern="Final", full.names = T) 
# keep Organization profile only
fileList <- fileList[grep("(?i)organization", fileList)]
det3 <- read.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX_DET_v3.0.csv') # det3.0 file
csvMapping <- read.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_csv_fieldNames_andOrder_OrganizationProfile.csv') # manually-curated list of EPA-submitted field names for csv and order of fields for Org 3.0 profile
det3Mapping <- read.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_mapping_final_20220404.csv') # det3.0 file with EPA dump file mappings

# pull out profile name from file name
fileName <- "Organization"

# remove extraneous fields from det3
det3 <- det3[, -grep("(?i)biodata", names(det3))]

joined <- read_excel(fileList[1])
joined$FileName <- fileName[1]
print(fileList[1])

# for consistency with Section 1, re-name object to match previous Section
joinedWide <- joined %>% dplyr::rename(Organization=FileName)

# merge the det3.0 file with the csvMapping file and pray it works!
names(det3)
names(csvMapping)
schemaJoined <- full_join(det3, csvMapping, by=c('Sort.Within.Category.Schema.Location',
                                                 "Data.Element.XML.Tag"))

# merge the schema joined with the det mapping file and hope it works!
names(schemaJoined)
names(det3Mapping)
schemaMappedJoined <- full_join(schemaJoined, det3Mapping)


# merge the joined det table with the profile table and hope it works!
names(joinedWide)
names(schemaMappedJoined)
joinedCsvAndDet3 <- full_join(schemaMappedJoined, joinedWide, by=c("csvFieldName"="ProfileFieldName"))

# counts check to make sure all fields for each profile got mapped #
nFields <- nrow(joined)
nMappedFields <- sum(!is.na(joinedCsvAndDet3$Organization))
nFields-nMappedFields
# [1] 0, looks good, same number of fields in as out


# export mapping file #
write.csv(joinedCsvAndDet3, '/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_csv_profiles_mapping_template_organization.csv',
          row.names = FALSE, na = "")


# create standardized field names

finalMapping <- joinedCsvAndDet3
# re-order object by schema order, rownumber and fieldorder
finalMapping <- finalMapping[order(finalMapping$Sort.Within.Category.Schema.Location, finalMapping$RowOrder), ] 

# add a field for storing the standardized csv field name
finalMapping$FinalCsvFieldNameStandardized <- ""
rowsToCheck <- which(!is.na(finalMapping$RowOrder)==TRUE)
hierarchInd <- which(str_count(finalMapping$Sort.Within.Category.Schema.Location, "\\.") >= 2)

for(b in rowsToCheck) {
  if(finalMapping$HeaderOrField[b]=="header") {
    header <- finalMapping$Data.Element.XML.Tag[b]
    #print(header)
  }
  if(b %in% hierarchInd) {
    finalMapping$FinalCsvFieldNameStandardized[b] <- paste(header, finalMapping$Data.Element.XML.Tag[b], sep="/")
  } else {
    finalMapping$FinalCsvFieldNameStandardized[b] <- finalMapping$csvFieldName[b]
  }
}


### Manual clean-up ###
# append _1, _2, _3 to AddressTypeName, AddressText, SupplementalAddressText, LocalityName, StateCode, PostalCode, CountryCode,
# CountyCode fields
addType <- grep("/AddressTypeName$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[addType] <- paste(finalMapping$FinalCsvFieldNameStandardized[addType], 1:3, sep="_")
addText <- grep("/AddressText$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[addText] <- paste(finalMapping$FinalCsvFieldNameStandardized[addText], 1:3, sep="_")
addSupp <- grep("/SupplementalAddressText$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[addSupp] <- paste(finalMapping$FinalCsvFieldNameStandardized[addSupp], 1:3, sep="_")
localNm <- grep("/LocalityName$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[localNm] <- paste(finalMapping$FinalCsvFieldNameStandardized[localNm], 1:3, sep="_")
stateCd <- grep("/StateCode$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[stateCd] <- paste(finalMapping$FinalCsvFieldNameStandardized[stateCd], 1:3, sep="_")
postCd <- grep("/PostalCode$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[postCd] <- paste(finalMapping$FinalCsvFieldNameStandardized[postCd], 1:3, sep="_")
countryCd <- grep("/CountryCode$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[countryCd] <- paste(finalMapping$FinalCsvFieldNameStandardized[countryCd], 1:3, sep="_")
countyCd <- grep("/CountyCode$", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized[countyCd] <- paste(finalMapping$FinalCsvFieldNameStandardized[countyCd], 1:3, sep="_")

# remove unallowable characters (only know of spaces and #)
finalMapping$FinalCsvFieldNameStandardized <- gsub(" ", "", finalMapping$FinalCsvFieldNameStandardized)
finalMapping$FinalCsvFieldNameStandardized <- gsub("#", "_", finalMapping$FinalCsvFieldNameStandardized)
### End Manual Clean-up ###

# check for duplicate final field names
finalFieldNames <- finalMapping$FinalCsvFieldNameStandardized[-which(finalMapping$FinalCsvFieldNameStandardized=="")]
any(duplicated(finalFieldNames) )  # should be FALSE if all good
finalFieldNames[which(duplicated(finalFieldNames))]

# re-order object by rownumber and delivery order
finalMapping <- finalMapping[order(finalMapping$RowOrder, finalMapping$DeliveryOrder), ] 

# compare lengths of epa and usgs field names
min(nchar(finalMapping$csvFieldName), na.rm = T)
max(nchar(finalMapping$csvFieldName), na.rm = T) #61
min(nchar(finalMapping$FinalCsvFieldNameStandardized), na.rm = T)
max(nchar(finalMapping$FinalCsvFieldNameStandardized), na.rm = T) #77


# export mapping file #
finalMapping %>% rename('EpaSuppliedCsvFieldName'='csvFieldName') %>%
  select(c('Sub.Category', 'Sort.Within.Category.Schema.Location', 'Data.Element.XML.Tag', 'Lookup.Table.Needed',
           'Schema.3.0.File.Name', 'DeliveryOrder', 'Tables.and.Columns.Foreign.KEYS', 
           'WQX.Table.Name', 'WQX.Field.Name', 'WQX.Mapping.Comments', 'Organization',
           'FinalCsvFieldNameStandardized', 'EpaSuppliedCsvFieldName')) %>%
  filter(!is.na(DeliveryOrder)) %>%
  write.csv('/Users/lstanish/OneDrive - DOI/Documents/WQX/WQX3.0_csv_final_mapping_template_OrganizationProfile.csv',
            row.names = FALSE, na = "")

#### END SECTION 3 ####