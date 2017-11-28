setwd("/home/sergiy/Documents/Work/Nutricia/Rework")

library(data.table)
library(reshape2)
library(googlesheets)

##### EXTREMELY IMPORTANT !!! ####
## reading and transformation code can be sourced since it is used for many files

# Read all necessary files

data = fread("BFv42.csv", header = TRUE, stringsAsFactors = FALSE, data.table = TRUE)
data = data[, c("SKU", "Period", "Subbrand", "Size", "Age", "Scent", "ITEMS", "VALUE", "VOLUME", 
                "Channel", "Channel 2", "CDC", "Coef", "Correction") := NULL]
#houses = fread("houses.csv", stringsAsFactors = FALSE, data.table = TRUE)
#names(data)[7:9] = c("PS0", "PS2", "PS3") # Rename because somebody have added spaces...

# Transform to upper case since file consists of different letters' cases
data$PS0 = toupper(data$PS0)
data$PS2 = toupper(data$PS2)
data$PS3 = toupper(data$PS3)
data$PS = toupper(data$PS)
data$Brand = toupper(data$Brand)
data$Company = toupper(data$Company)
#data$Form = toupper(data$Form)
data$PriceSegment = toupper(data$PriceSegment)

# Select necessary data for report building
data = data[Form %in% c("POWDER", "PURE", "SOLID") & Ynb > 2013 & VALUEC > 0]

# Set current month
YTD.No = 9

dataTable = function(measure, level, linesToShow, filterSegments = NULL) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives, PriceSegment)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    
    
    
    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    
    nc = length(df)
    
    df$YTDLY = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No - 11):(nc - 12)]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    df$MATLY = df[, Reduce(`+`, .SD), .SDcols = (nc - 23):(nc - 12)]
    df$MAT = df[, Reduce(`+`, .SD), .SDcols = (nc - 11):nc]
    
    df = df[, .SD, .SDcols=c(1, nc-1, nc, (nc+1):(nc+4))]
    
    df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
            .SDcols = 2:length(df)]          
    
    df = df[, .SD, .SDcols = -c(2:7)]
    
    df$DeltaCM = df[, Reduce(`-`, .SD), .SDcols = c(3, 2)]
    df$DeltaYTD = df[, Reduce(`-`, .SD), .SDcols = c(5, 4)]
    df$DeltaMAT = df[, Reduce(`-`, .SD), .SDcols = c(7, 6)]
    
    setcolorder(df, c(1:3, 8, 4:5, 9, 6:7, 10))
    df = df[, .SD, .SDcols = -c(2, 5, 8)]
    
    result = head(df[order(-df[,4])], linesToShow)
    return(result)
}

dataChart = function(measure, level, linesToShow, filterSegments) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    
    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    
    nc = length(df)
    
    df$YTDLY = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No - 11):(nc - 12)]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    df$MATLY = df[, Reduce(`+`, .SD), .SDcols = (nc - 23):(nc - 12)]
    df$MAT = df[, Reduce(`+`, .SD), .SDcols = (nc - 11):nc]
    
    df = df[, .SD, .SDcols=c(1, (nc-12):nc, (nc+1):(nc+4))]
    
    df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
            .SDcols = 2:length(df)]          
    
    df = df[, .SD, .SDcols = -c(4:18)]
    df[,2] = NA
    df[,3] = NA
    names(df)[2:3] = c("Blank1", "Blank2")
    
    setcolorder(df, c(1, 19:20, 2, 17:18, 3, 4:16))
    
    result = head(df[order(-df[,6])], linesToShow)
    return(result)
}

dataSegmentTable = function(measure, level, linesToShow, filterSegments) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    
    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    
    nc = length(df)
    
    df$L3M = df[, Reduce(`+`, .SD), .SDcols = (nc - 2):nc]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    
    
    
    df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
            .SDcols = 2:length(df)]          
    
    df = df[, .SD, .SDcols = -c(2:(2*nc))]
    
    
    #setcolorder(df, c(1, 17:18, 19, 15:16, 20, 2:14))
    
    #result = head(df[order(-df[,6])], linesToShow)
    result = head(df[order(df[,1])], linesToShow)
    return(result)
}

dataSegmentChart = function(measure, level, linesToShow, filterSegments) {
    
    # Create subset which consists only segments we'll work with
    #df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
    #          by = .(Brand, PS2, PS3, PS, Company, Ynb, Mnb, Form, Additives)]
    
    df = data[eval(parse(text = filterSegments)), .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
              by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, PriceSegment)]
    
    if (level == "Company")  {df = data.table::dcast(df, Company~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Brand") {df = data.table::dcast(df, Brand~Ynb+Mnb, fun = sum, value.var = measure)}
    else if (level == "Segment") {df = data.table::dcast(df, PriceSegment~Ynb+Mnb, fun = sum, value.var = measure)}
    
    nc = length(df)
    
    df$YTDLY = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No - 11):(nc - 12)]
    df$YTD = df[, Reduce(`+`, .SD), .SDcols = (nc - YTD.No + 1):nc]
    df$MATLY = df[, Reduce(`+`, .SD), .SDcols = (nc - 23):(nc - 12)]
    df$MAT = df[, Reduce(`+`, .SD), .SDcols = (nc - 11):nc]
    
    df = df[, .SD, .SDcols=c(1, (nc-12):nc, (nc+1):(nc+4))]
    
    #df = df[, paste0(names(df)[2:length(df)], "_MS") := lapply(.SD, function(x) 100*x/sum(x)), 
    #        .SDcols = 2:length(df)]          
    
    #df = df[, .SD, .SDcols = -c(4:18)]
    df$Blank1 = NA
    df$Blank2 = NA
    
    setcolorder(df, c(1, 17:18, 19, 15:16, 20, 2:14))
    
    #result = head(df[order(-df[,6])], linesToShow)
    result = head(df[order(df[,1])], linesToShow)
    return(result)
}


## Update Google sheet

tableColnames1 = c("Company", "SEP 17", "vs PP, pp", "YTD 17", "vs PY, pp", "MAT 17", "vs PY, pp")
tableColnames2 = c("Brand", "SEP 17", "vs PP, pp", "YTD 17", "vs PY, pp", "MAT 17", "vs PY, pp")
tableColnames3 = c("Company", "MAT 16", "MAT 17", ".", "YTD 16", "YTD 17", ".",
                   "SEP 16", " OCT 16", "NOV 16", "DEC 16", "JAN 17", "FEB 17", "MAR 17",
                   "APR 17", "MAY 17", "JUN 17", "JUL 17", "AUG 17", "SEP 17")
tableColnames4 = c("Brand", "MAT 16", "MAT 17", ".", "YTD 16", "YTD 17", ".",
                   "SEP 16", " OCT 16", "NOV 16", "DEC 16", "JAN 17", "FEB 17", "MAR 17",
                   "APR 17", "MAY 17", "JUN 17", "JUL 17", "AUG 17", "SEP 17")

gs_title("Baby Food data")
gs_object <- gs_key("1efRfjHRRgtbLwwTznmveBoKiKwx_xMsBxMy4zlwhqDs")

### COMPANIES

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 11, "VALUEC > 0"), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A3")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 11, "VALUEC > 0"), 
              anchor="A4", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Cereal + Biscuits
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A103")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A104", 
              col_names=FALSE, 
              trim=FALSE)

#Dry Food
gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Volume", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Companies", input = tableColnames1, byrow=TRUE, anchor="A143")
gs_edit_cells(gs_object, ws="Companies", 
              input = dataTable("Value", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A144", 
              col_names=FALSE, 
              trim=FALSE)

## BRANDS

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, "VALUEC > 0"), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, "VALUEC > 0"), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

#IMF + Cereal + Biscuits
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A44")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A45", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A102")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A103", 
              col_names=FALSE, 
              trim=FALSE)

# IMF Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A142")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A143", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A163")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A182")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A183", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A203")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A222")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A223", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A243")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A262")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A263", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A283")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A302")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A303", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A323")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A342")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A343", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base Premium
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A363")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A382")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A383", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A403")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A422")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A423", 
              col_names=FALSE, 
              trim=FALSE)

# DC
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A443")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A462")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A463", 
              col_names=FALSE, 
              trim=FALSE)

# HA
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A483")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A502")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A503", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A523")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A524", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A542")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A543", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A563")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A582")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A583", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A603")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A604", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Brands", input = tableColnames2, byrow=TRUE, anchor="A622")
gs_edit_cells(gs_object, ws="Brands", 
              input = dataTable("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A623", 
              col_names=FALSE, 
              trim=FALSE)

### COMPANIES CHARTS


#IMF + Cereal + Biscuits
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A62")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS0 == "IMF"'), 
              anchor="A63", 
              col_names=FALSE, 
              trim=FALSE)

#Dry Food
gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Volume", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="CoCharts", input = tableColnames3, byrow=TRUE, anchor="A102")
gs_edit_cells(gs_object, ws="CoCharts", 
              input = dataChart("Value", "Company", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A103", 
              col_names=FALSE, 
              trim=FALSE)

## BRANDS CHARTS


#IMF + Cereal + Biscuits
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 11, 
                                'PS0 == "IMF" | PS3 == "CEREAL BISCUITS" | PS3 == "INSTANT CEREALS"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

#IMF
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A62")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF"'), 
              anchor="A63", 
              col_names=FALSE, 
              trim=FALSE)

# IMF Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A102")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS0 == "IMF" & PriceSegment == "PREMIUM"'), 
              anchor="A103", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A123")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A124", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A142")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A143", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A163")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A164", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A182")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "IF" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A183", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A203")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A204", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A222")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A223", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A243")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A244", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A262")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "FO" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A263", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A283")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A284", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A302")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A303", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base Premium
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A323")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A324", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A342")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "GUM" & PS3 == "BASE" & PriceSegment == "PREMIUM"'), 
              anchor="A343", 
              col_names=FALSE, 
              trim=FALSE)

# Specials
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A363")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A364", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A382")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "SPECIALS"'), 
              anchor="A383", 
              col_names=FALSE, 
              trim=FALSE)

# DC
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A403")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A404", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A422")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "DIGESTIVE COMFORT"'), 
              anchor="A423", 
              col_names=FALSE, 
              trim=FALSE)

# HA
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A443")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A444", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A462")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS == "HYPOALLERGENIC"'), 
              anchor="A463", 
              col_names=FALSE, 
              trim=FALSE)

# Dry Food
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A483")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A484", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A502")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS2 == "DRY FOOD"'), 
              anchor="A503", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A523")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A524", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A542")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A543", 
              col_names=FALSE, 
              trim=FALSE)

# Cereal Biscuits
gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A563")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Volume", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A564", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="BrandsCharts", input = tableColnames4, byrow=TRUE, anchor="A582")
gs_edit_cells(gs_object, ws="BrandsCharts", 
              input = dataChart("Value", "Brand", 10, 
                                'PS3 == "CEREAL BISCUITS"'), 
              anchor="A583", 
              col_names=FALSE, 
              trim=FALSE)

## SEGMENTS

# IMF
gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Volume", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A13")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Value", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A14", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Volume", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A32")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Value", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A33", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Volume", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A52")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Value", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A53", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Volume", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A72")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Value", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A73", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Volume", "Segment", 3, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="Segments", input = tableColnames1, byrow=TRUE, anchor="A92")
gs_edit_cells(gs_object, ws="Segments", 
              input = dataTable("Value", "Segment", 3, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A93", 
              col_names=FALSE, 
              trim=FALSE)

## SEGMENTS CHARTS

# IMF
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A4")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Volume", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A5", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames4, byrow=TRUE, anchor="A13")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Value", "Segment", 3, 
                                'PS0 == "IMF"'), 
              anchor="A14", 
              col_names=FALSE, 
              trim=FALSE)

# IF Base
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A23")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Volume", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A24", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A32")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Value", "Segment", 3, 
                                'PS2 == "IF" & PS3 == "BASE"'), 
              anchor="A33", 
              col_names=FALSE, 
              trim=FALSE)

# FO Base
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A43")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Volume", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A44", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A52")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Value", "Segment", 3, 
                                'PS2 == "FO" & PS3 == "BASE"'), 
              anchor="A53", 
              col_names=FALSE, 
              trim=FALSE)

# GUM Base
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A63")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Volume", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A64", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A72")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Value", "Segment", 3, 
                                'PS2 == "GUM" & PS3 == "BASE"'), 
              anchor="A73", 
              col_names=FALSE, 
              trim=FALSE)

# Instant Cereals
gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A83")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Volume", "Segment", 3, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A84", 
              col_names=FALSE, 
              trim=FALSE)

gs_edit_cells(gs_object, ws="SegmentsCharts", input = tableColnames2, byrow=TRUE, anchor="A92")
gs_edit_cells(gs_object, ws="SegmentsCharts", 
              input = dataChart("Value", "Segment", 3, 
                                'PS3 == "INSTANT CEREALS"'), 
              anchor="A93", 
              col_names=FALSE, 
              trim=FALSE)