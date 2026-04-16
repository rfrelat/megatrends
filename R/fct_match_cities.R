## Define functions to help find best matches -------------
#' Clean and homogenized city names
#' 
#' @description
#' Remove punctuation, accent, numbers, and unnecessary white space. 
#' Transform the names to uppercase.
#'
#' @param x a vector of `characters` with the city names.
#'
#' @returns A vector of `characters` with the homogenized names
#' 
#' @export
#'
#' @examples
#' clean_city_names("Florac-Trois-Rivières")
#' clean_city_names("AJACCIO CEDEX 9")
#' 
clean_city_names <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  
  # remove all - ' _ . and transform to upper case
  x <- gsub("[-'’_\\.]", " ", x) |> toupper()
  # remove numbers
  x <- gsub("[1-9]", "", x)
  # remove multiple white space
  x <- remove_multiple_spaces(x)
  # remove first space
  x <- gsub("^ ", "", x)
  # remove last space
  x <- gsub(" $", "", x)
  # replace Œ by OE (by default it is replaced by 'O' in iconv)
  x <- gsub("Œ", "OE", x)
  # remove accent 
  x <- iconv(x,from="UTF-8",to="ASCII//TRANSLIT")
  # remove cedex
  x <- gsub(" CEDEX$", "", x)
  
  return(x)
}

#' Remove multiple white spaces
#' 
#' @description
#' Remove multiple white spaces and replace them with single space.
#' This is an iterative function, modify with care.
#'
#' @param x a vector of `characters`
#'
#' @returns A vector of `characters` without multiple white space
#'  
#' @export
#'
#' @examples
#' remove_multiple_spaces(c("Try and    error", "a b  c", NA))
#' 
remove_multiple_spaces <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  
  # remove double space
  y <- gsub("  ", " ", x)
  if (any(grepl("  ", y))){
    y[grepl("  ", y)] <- remove_multiple_spaces(grep("  ", y, value=TRUE))
  }
  return(y)
}

#' Remove starting definite articles in french city names
#'
#' @param x a vector of `characters` with the city names.
#'
#' @returns A vector of `characters` without definite article
#' 
#' @export
#'
#' @examples
#' remove_first_leas(c("Florac", "Florac-Trois-Rivières"))
#' 
remove_first_leas <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  # remove all - or '
  x <- gsub("^L ", "", x)
  x <- gsub("^LE ", "", x)
  x <- gsub("^LA ", "", x)
  x <- gsub("^LES ", "", x)
  x <- gsub("^LOS ", "", x)
  x <- gsub("^LAS ", "", x)
  x <- gsub("^AUX ", "", x)
  x <- gsub("^AU ", "", x)
  return(x)
}

#' Complete saint abbreviation in names
#'
#' @param x a vector of `characters` with the city names.
#'
#' @returns A vector of `characters` with fully
#' 
#' @export
#'
#' @examples
#' remove_first_leas(c("Florac", "Florac-Trois-Rivières"))
#' 
simplify_saint <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  x <- gsub("^SAINT ", "ST ", x)
  x <- gsub("^SAINTE ", "ST ", x)
  x <- gsub("^STE ", "ST ", x)
  x <- gsub(" SAINT ", " ST ", x)
  x <- gsub(" SAINTE ", " ST ", x)
  x <- gsub(" STE ", " ST ", x)
  return(x)
}

#' Remove french articles in city names
#'
#' @param x a vector of `characters` with the city names.
#'
#' @returns A vector of `characters` without articles
#' 
#' @export
#' 
remove_article <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  # remove articles to facilitate matches
  x <- gsub(" LE ", " ", x)
  x <- gsub(" LA ", " ", x)
  x <- gsub(" L ", " ", x)
  x <- gsub(" LES ", " ", x)
  x <- gsub(" SUR ", " ", x)
  x <- gsub(" SS ", " ", x)
  x <- gsub(" SOUS ", " ", x)
  x <- gsub(" EN ", " ", x)
  x <- gsub(" DES ", " ", x)
  x <- gsub(" DE ", " ", x)
  x <- gsub(" D ", " ", x)
  x <- gsub(" DU ", " ", x)
  x <- gsub(" & ", " ", x)
  x <- gsub(" ET ", " ", x)
  x <- gsub(" A ", " ", x)
  x <- gsub(" AU ", " ", x)
  x <- gsub(" AUX ", " ", x)
  
  return(x)
}

#' Remove brackets and what is inside the brackets
#'
#' @param x a vector of `characters`.
#'
#' @returns A vector of `characters` without brackets
#' 
#' @export
#' 
remove_bracket <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  # remove the brackets (square or round)
  x <- gsub("\\((.+?)\\)", "", x)
  x <- gsub("\\[(.+?)\\]", "", x)
  return(x)
}

#' Remove brackets and keep what is inside the brackets
#'
#' @param x a vector of `characters`.
#'
#' @returns A vector of `characters` without brackets
#' 
#' @export
#' 
keep_bracket <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  # remove the brackets but keep what is inside
  x <- gsub("\\[", " ", x)
  x <- gsub("\\]", " ", x)
  x <- gsub("\\(", " ", x)
  x <- gsub("\\)", " ", x)
  # remove last white space (if any)
  x <- gsub(" $", " ", x)
  return(x)
}

#' Remove french plural forms of multiple words
#'
#' @param x a vector of `characters`.
#'
#' @returns A vector of `characters` without plural forms
#' 
#' @export
#' 
remove_plural <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  # split composed names
  lx <- strsplit(x, " ")
  # remove last s for all words and concatenate
  y <- lapply(lx, remove_word_plural) |> 
    sapply(paste, collapse=" ")
  return(y)
}

#' Remove french plural forms of single word
#'
#' @param x a vector of `characters`.
#'
#' @returns A vector of `characters` without plural forms
#' 
#' @export
#' 
remove_word_plural <- function(x){
  if (missing(x)) {
    stop("Argument 'x' is required", call. = FALSE)
  }
  if (!is.character(x)) {
    stop("Argument 'x' must be character", call. = FALSE)
  }
  # remove last s
  x <- gsub("S$", "", x)
  # remove last x
  x <- gsub("X$", "", x)
  return(x)
}

#' Check and harmonize french postal code
#'
#' @param code a vector with postal codes.
#'
#' @returns A vector of `characters` with 5 digit postal code
#' 
#' @export
#' 
check_postalcode <- function(code){
  if (missing(code)) {
    stop("Argument 'code' is required", call. = FALSE)
  }
  # if provided as number
  if (is.numeric(code)){
    # make sure it has 4 or 5 digit
    code[code<1000|code>99999] <- NA
    # transform it as string
    x <- as.character(code)
  } else {
    # remove everything that is not a number
    x <- as.character(code)
    x[grepl("[1-9]", x)] <- grep("[1-9]", x, value=TRUE)
    # remove if not 4 or 5 digit
    x[!nchar(x)%in%4:5] <- NA
  }
  # add a 0 when only four digits
  x[nchar(x)%in%4] <- paste0("0", x[nchar(x)%in%4])
  return(x)
}

#' Value matching from the last argument
#'
#' @param x a vector with the values to be matched
#' @param y a vector with the values to be matched against
#'
#' @returns A vector of the same length as `x` containing 
#' integers giving the position in `y` of the last match found (if any).
#' 
#' @export
#' 
match_from_last <- function(x, y){
  # match with the reverse vector
  m0 <- match(x, rev(y))
  # transform to get the original element order
  m1 <- length(y) - m0 + 1
  return(m1)
}

#' Testing if the match between two vectors is unique
#'
#' @param x a vector with the values to be matched
#' @param y a vector with the values to be matched against
#'
#' @returns A vector of the same length as `x` containing 
#' logical values indicating whether there are multiple matches in `y`
#' 
#' @export
#' 
unique_match <- function(x,y){
  # match with the same order
  m0 <- match(x, y)
  # match with opposite order
  m1 <- match_from_last(x, y)
  # compare both match
  return(m0==m1)
}


#' Value matching of french cities with names and postal codes separated by arobase
#'
#' @description
#' Looks for perfect match with names and postal codes
#' When postal codes are missing, it returns the matching city names if unique.
#'
#' @param x a vector with the values to be matched
#' @param y a vector with the values to be matched against
#'
#' @returns A vector of the same length as `x` containing 
#' integers giving the position in `y` of the last match found (if any).
#' 
#' @export
#' 
matcha <- function(x, y){
  m <- match(x, y)
  # match in case there is no code
  nocode <- grepl("@NA", x)
  if (sum(nocode)>0){
    xnocode <- substr(x[nocode], 1, regexpr("@", x[nocode])-1)
    ynocode <- substr(y, 1, regexpr("@", y)-1)
    mnocode <- match(xnocode, ynocode)
    # make sure there is no other cities with the same name (=unique match)
    mnocode[!unique_match(xnocode, ynocode)] <- NA
    m[nocode] <-mnocode
  }
  return(m)
}

#' Find matching city from a list of reference based on names and postal codes
#' 
#' @description
#' Two step process to find the matching city from a list of reference
#' Start with homogenizing the names (e.g. uppercase), removing article, 
#' completing abbreviations
#'
#' @param dfx a data.frame with `name` and `code` as columns.
#' @param dfref a data.frame with `name` and `code` as columns.
#'
#' @returns A integer vector of the same length as the number of rows in dfx.
#' The values correspond to the position in dfref of the first match if there is a match
#' 
#' @export
#'
simple_match_cities <- function(dfx, dfref){
  
  # basic checks of the arguments
  if (missing(dfx) | missing(dfref)) {
    stop("Arguments 'dfx' and 'dfref' are required", call. = FALSE)
  }
  if (!is.data.frame(dfx)) {
    stop("Argument 'dfx' must be a data.frame", call. = FALSE)
  }
  if (!is.data.frame(dfref)) {
    stop("Argument 'dfref' must be a data.frame", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfx)))  {
    stop("The columns 'name' and 'code' are not found in 'dfx'", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfref)))  {
    stop("The columns 'name' and 'code' are not found in 'dfref'", call. = FALSE)
  }
  
  # make sure the string have the same format
  dfx$name <- clean_city_names(dfx$name) |> 
    remove_first_leas() |>   simplify_saint()
  dfref$name <- clean_city_names(dfref$name) |> 
    remove_first_leas() |>   simplify_saint()
  
  # first simple match with cities and postal codes
  m <- matcha(paste(dfx$name, dfx$code, sep="@"),
              paste(dfref$name, dfref$code, sep="@"))
  # identify non-matching elements
  miss <- which(is.na(m))  
  
  # alternative 1: remove articles and remove plural forms
  x_temp <-  remove_article(dfx$name) |> 
    remove_plural() |> paste(dfx$code, sep="@")
  ref_temp <- remove_article(dfref$name) |> 
    remove_plural() |> paste(dfref$code, sep="@")
  m1 <- matcha(x_temp[miss], ref_temp)
  
  # update the matching results
  m[miss] <- m1
  miss <- which(is.na(m))
  
  # return the vector of matching items
  return(m)
}

#' Find fuzzy matching city from a list of reference based on names and postal codes
#' 
#' @description
#' Using the stringdist::amatch() function over names and postal codes combined
#' with the Jaro-Winkler distance with a prefix factor of `p`.
#' 
#' This function is faster than `fuzzy_match_cities()` but the results
#' are more prone to errors.
#'
#' @param dfx a data.frame with `name` and `code` as columns.
#' @param dfref a data.frame with `name` and `code` as columns.
#' @param dmax maximum Jaro-Winkler distance to be considered a match (0 = perfect match, 1= no match)
#' @param p prefix factor for the Jaro-Winkler distance 5must be between (0 and 0.25).
#'
#' @returns A integer vector of the same length as the number of rows in dfx.
#' The values correspond to the position in dfref of the best first match 
#' if there is a fuzzy match with a distance lower than `dmax`
#' 
#' @export
#'
fuzzy_match_cities_v0 <- function(dfx, dfref, dmax=0.2, p=0.1){
  # basic checks of the arguments
  if (missing(dfx) | missing(dfref)) {
    stop("Arguments 'dfx' and 'dfref' are required", call. = FALSE)
  }
  if (!is.data.frame(dfx)) {
    stop("Argument 'dfx' must be a data.frame", call. = FALSE)
  }
  if (!is.data.frame(dfref)) {
    stop("Argument 'dfref' must be a data.frame", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfx)))  {
    stop("The columns 'name' and 'code' are not found in 'dfx'", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfref)))  {
    stop("The columns 'name' and 'code' are not found in 'dfref'", call. = FALSE)
  }
  if (!is.numeric(dmax) | length(dmax)!=1 ){
    stop("Argument 'dmax' must a single numeric value", call. = FALSE)
  }
  if (dmax <0 | dmax>1){
    stop("'dmax' should be between 0 (perfect match) and 1 (no match)", call. = FALSE)
  }
  if (!is.numeric(p) | length(p)!=1 ){
    stop("Argument 'p' must a single numeric value", call. = FALSE)
  }
  if (p <0 | p>0.25){
    stop("'p' the prefix factor for the Jaro-Winkel distance must be between 0 and 0.25", call. = FALSE)
  }
  
  # make sure the string have the same format
  dfx$name <- clean_city_names(dfx$name) |> 
    remove_first_leas() |>   simplify_saint()
  dfref$name <- clean_city_names(dfref$name) |> 
    remove_first_leas() |>   simplify_saint()
  
  # simple match with cities and postal codes
  m <- stringdist::amatch(
    paste(dfx$name, dfx$code, sep="@"),
    paste(dfref$name, dfref$code, sep="@"),
    maxDist=dmax, method="jw", p=p
  )
  
  return(m)
}  

#' Find fuzzy matching city from a list of reference based on names and postal codes
#' 
#' @description
#' The function looks for fuzzy match separately for names and postal codes combined
#' with the Jaro-Winkler distance with a prefix factor of `p`.
#'
#' @param dfx a data.frame with `name` and `code` as columns.
#' @param dfref a data.frame with `name` and `code` as columns.
#' @param dmax maximum Jaro-Winkler distance to be considered a match (0 = perfect match, 1= no match)
#' @param dist.out whether the Jaro-Winkler distance is returned with the matching elements.
#' 
#' @returns When dist.out=FALSE, returns a integer vector of the same length as the number of rows in dfx.
#' The values correspond to the position in dfref of the first match if there is a match
#' 
#' When dist.out=TRUE, returns a data.frame with the same number of rows of `dfx`
#' with column `match` for the matching position, and `dist` for the Jaro-Winkler distance
#' 
#' @export
#'
fuzzy_match_cities <- function(dfx, dfref, dmax=0.3, dist.out=FALSE){
  # basic checks of the arguments
  if (missing(dfx) | missing(dfref)) {
    stop("Arguments 'dfx' and 'dfref' are required", call. = FALSE)
  }
  if (!is.data.frame(dfx)) {
    stop("Argument 'dfx' must be a data.frame", call. = FALSE)
  }
  if (!is.data.frame(dfref)) {
    stop("Argument 'dfref' must be a data.frame", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfx)))  {
    stop("The columns 'name' and 'code' are not found in 'dfx'", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfref)))  {
    stop("The columns 'name' and 'code' are not found in 'dfref'", call. = FALSE)
  }
  if (!is.numeric(dmax) | length(dmax)!=1 ){
    stop("Argument 'dmax' must a single numeric value", call. = FALSE)
  }
  if (dmax <0 | dmax>1){
    stop("'dmax' should be between 0 (perfect match) and 1 (no match)", call. = FALSE)
  }
  if (!is.logical(dist.out) | length(dist.out)!=1 ){
    stop("Argument 'dist.out' must a single logical value", call. = FALSE)
  }
  # check dmax, number
  # check dist.out logic
  # check initial data.frames
  
  # make sure the string have the same format
  dfx$name <- clean_city_names(dfx$name) |> 
    remove_first_leas() |>   simplify_saint()
  dfref$name <- clean_city_names(dfref$name) |> 
    remove_first_leas() |>   simplify_saint()
  
  # calculate distance among names
  d1 <- stringdist::stringdistmatrix(dfx$name, dfref$name, 
                                     method="jw", p=0.1)
                                     #nthread=1)
  
  # calculate distance among postal code
  d2 <- stringdist::stringdistmatrix(dfx$code, dfref$code, 
                                     method="jw", p=0.2)
                                     #nthread=1)
  
  # sum the distance with adjusted weights
  d <- d1 + d2
  
  # remove matches further than dmax
  d[d>dmax] <- NA
  
  out <- apply(d, 1, which.min) |> as.numeric()
  
  if (dist.out){
    out <- data.frame("match"=out,
                      "dist"=d[cbind(1:nrow(d), out)])
  } 
  return(out)
}  


#' Find matching cities from a list of reference based on names and postal codes
#' 
#' @description
#' The function looks for simple and fuzzy match considering popular misspelling errors.
#'
#' @param dfx the original data.frame with `name` and `code` as columns.
#' @param dfref the reference data.frame with `name` and `code` as columns.
#' @param dfsyn (optional) a data.frame with `name`, `code`, and `id` as columns.
#' @param dmax maximum Jaro-Winkler distance to be considered a match (0 = perfect match, 1= no match)
#' @param print.out whether printing the summary of the matching process
#' @param file.out name and path of the file to store and verify the fuzzy match and synonymes (if any). Set `file.out` to NULL to avoid creating the file.
#' 
#' @returns A data.frame with the matching information
#' `ori_name` and `ori_code` contains the information of the original city names and postal codes
#' `ref_name` and `ref_code` contains the information of the reference city names and postal codes
#' `ref_id`contains the id of the reference city(if provided in `dfref`)
#' If no match was found; all the columns for the reference are set to NA.
#' 
#' @export
#'
match_cities <- function(dfx, dfref, dfsyn=NULL, dmax = 0.2,
                            print.out=TRUE, file.out="fuzzy_match.csv"){
  # check arguments
  if (missing(dfx) | missing(dfref)) {
    stop("Arguments 'dfx' and 'dfref' are required", call. = FALSE)
  }
  if (!is.data.frame(dfx)) {
    stop("Argument 'dfx' must be a data.frame", call. = FALSE)
  }
  if (!is.data.frame(dfref)) {
    stop("Argument 'dfref' must be a data.frame", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfx)))  {
    stop("The columns 'name' and 'code' are not found in 'dfx'", call. = FALSE)
  }
  if (!all(c("name", "code") %in% names(dfref)))  {
    stop("The columns 'name' and 'code' are not found in 'dfref'", call. = FALSE)
  }
  if (!is.null(dfsyn)){
    if (!is.data.frame(dfsyn)) {
      stop("Argument 'dfsyn' must be a data.frame", call. = FALSE)
    }
    if (!all(c("name", "code") %in% names(dfref)))  {
      stop("The columns 'name' and 'code' are not found in 'dfsyn'", call. = FALSE)
    }
    if (!("id" %in% names(dfref) & "id" %in% names(dfsyn)))  {
      stop("The column 'id' is required in 'dfref' and 'dfsyn'", call. = FALSE)
    }
    if (!all(dfsyn$id %in% dfref$id))  {
      warnings("Some synonyms were not found in the reference list, they are discarded.")
      dfsyn <- dfsyn[dfsyn$id%in%dfref$id,]
    }
  }
  if (!is.numeric(dmax) | length(dmax)!=1 ){
    stop("Argument 'dmax' must a single numeric value", call. = FALSE)
  }
  if (dmax <0 | dmax>1){
    stop("'dmax' should be between 0 (perfect match) and 1 (no match)", call. = FALSE)
  }
  if (!is.logical(print.out) | length(print.out)!=1){
    stop("'print.out' should be a single logical value", call. = FALSE)
  }
  # make sure file.out is character and of length 1
  
  # check postal code (five digits) 
  dfx$code <- check_postalcode(dfx$code)
  dfref$code <- check_postalcode(dfref$code)
  
  # remove duplicates to speed up search
  ux <- dfx[!duplicated(dfx[,c("name", "code")]),]
  uref <- dfref[!duplicated(dfref[,c("name", "code")]),]
  
  # if synonyms, merge ref and syn
  if (!is.null(dfsyn)){
    # check postal code
    dfsyn$code <- check_postalcode(dfsyn$code)
    # remove duplicates
    usyn <- dfsyn[!duplicated(dfsyn[,c("name", "code")]),]
    # keep only the synonyms that can't be match in ref
    synm <- simple_match_cities(usyn, uref)
    # rbind the three important columns with uref
    uref <- data.frame(
      "name"=c(uref$name, usyn$name[is.na(synm)]),
      "code"=c(uref$code, usyn$code[is.na(synm)]),
      "id"=c(uref$id, usyn$id[is.na(synm)])
    )
  }
  
  # get matching by small transformations
  m <- simple_match_cities(ux, uref)
  # keep the type of match
  
  
  if (is.null(dfsyn)){
    typem <- ifelse(is.na(m), "no_match", "simple_match")

  } else {
    th <- nrow(uref) - sum(is.na(synm)) + 1
    typem <- ifelse(is.na(m), "no_match", 
                    ifelse(m<th, "simple_match", "verified_synonym"))
  }
  
  # select the non-matching items for fuzzy matching
  u1 <- ux[is.na(m),]
  
  if (dmax>0){
    fm <- fuzzy_match_cities(u1, uref, dmax = dmax, 
                             dist.out = TRUE)
    
    # keep track of fuzzy match in file.out
    if (!is.null(file.out)){
      fuzzy.out <- data.frame(
        "ori_name"= u1$name,
        "ori_code"= u1$code,
        "ref_name"= uref$name[fm$match],
        "ref_code"= uref$code[fm$match],
        "dist"=fm$dist
      )
      
      if ("id" %in% names(dfref)){
        fuzzy.out$ref_id <- uref$id[fm$match]
      }
      write.csv(fuzzy.out, file=file.out, row.names = FALSE)
    }
    m[is.na(m)] <- fm$match
    # keep the type of match
    typem <- ifelse(typem=="no_match" & !is.na(m), "fuzzy_match", typem)
  }
  

  if (print.out){
    n0 <- sum(typem=="simple_match")
    cat("Number of simple match: ", n0, "(", 
          round(n0/length(m)*100,2), "%)\n", sep="") 
    if (!is.null(dfsyn)){
      n0 <- sum(typem=="verified_synonym")
      cat("Number of verified synonyms: ", sum(n0, na.rm = TRUE), "(", 
          round(sum(n0, na.rm = TRUE)/length(m)*100,2), "%)\n", sep="")
    }
    if (dmax>0){
      n0 <- sum(typem=="fuzzy_match")
      cat("Number of fuzzy match: ", sum(!is.na(fm$match)), "(", 
          round(sum(!is.na(fm$match))/length(m)*100,2), "%)\n", sep="")
    }
    n0 <- sum(typem=="no_match")
    cat("Number of non-matching elements: ", sum(is.na(m)), "(", 
        round(sum(is.na(m))/length(m)*100, 2), "%)\n", sep="")
  }
  
  # match between unique element ux and full list dfx
  dfx$nameacode <- paste(dfx$name, dfx$code, sep="@")
  ux$nameacode <- paste(ux$name, ux$code, sep="@")
  fullmatch <- m[match(dfx$nameacode, ux$nameacode)]
  fulltype <- typem[match(dfx$nameacode, ux$nameacode)]
  
  # format the output
  out <- data.frame("ori_name"=dfx$name,
                    "ori_code"=dfx$code,
                    "ref_name"=uref$name[fullmatch],
                    "ref_code"=uref$code[fullmatch])
  
  # add the id if available
  if ("id" %in% names(dfref)){
    out$ref_id <- uref$id[fullmatch]
  }
  
  # get corrected names if synonyms
  if (!is.null(dfsyn)){
    # find the corresponding items from dfref
    out$ref_name <- dfref$name[match(out$ref_id, dfref$id)]
    out$ref_code <- dfref$code[match(out$ref_id, dfref$id)]
    out$ref_id <- uref$id[fullmatch]
  }
  
  out$type_match <-  fulltype
  
  return(out)
}