#' Parse a UCUM unit string
#'
#' INTERNAL helper.  Converts a raw unit string to a row compatible with
#' `units_df`.
#'
#' @keywords internal
parseUnit <- function(uStr,
                      units_df){
  # making up the necessary data
  cnv_units <- parsed_units_df[!is.na(parsed_units_df$cnv_),]
  arbitrary_units <- "\\[hp_X\\]|\\[hp_C\\]|\\[hp_M\\]|\\[hp_Q\\]|\\[kp_X\\]|\\[kp_C\\]|\\[kp_M\\]|\\[kp_Q\\]|\\[iU\\]|\\[IU\\]|\\[arb'U\\]|\\[USP'U\\]|\\[GPL'U\\]|\\[MPL'U\\]|\\[APL'U\\]|\\[beth'U\\]|\\[anti'Xa'U\\]|\\[todd'U\\]|\\[dye'U\\]|\\[smgy'U\\]|\\[bdsk'U\\]|\\[ka'U\\]|\\[knk'U\\]|\\[mclg'U\\]|\\[tb'U\\]|\\[CCID_50\\]|\\[TCID_50\\]|\\[EID_50\\]|\\[PFU\\]|\\[FFU\\]|\\[CFU\\]|\\[IR\\]|\\[BAU\\]|\\[AU\\]|\\[Amb'a'1'U\\]|\\[PNU\\]|\\[Lf\\]|\\[D'ag'U\\]|\\[FEU\\]|\\[ELU\\]|\\[EU\\]"
  si_prefixes <- data.frame(
    prefix_cs = c("Y", "Z", "E", "P", "T", "G", "M", "k", "h", "da", "d", "c", "m", "u", "n", "p", "f", "a", "z", "y"),
    prefix_ci = c("YA", "ZA", "EX", "PT", "TR", "GA", "MA", "K", "H", "DA", "D", "C", "M", "U", "N", "P", "F", "A", "ZO", "YO"),
    decimal   = c(1e24, 1e21, 1e18, 1e15, 1e12, 1e9, 1e6, 1e3, 100, 10, 0.1, 0.01, 0.001, 1e-6, 1e-9, 1e-12, 1e-15, 1e-18, 1e-21, 1e-24),
    stringsAsFactors = FALSE
  )

  # Checking whether the given unit exists inside the units_df table
  # and returning the found unit.

  # parsed_unit indicates that the given unit is parsed, and not a
  # unit expression.
  check_unit_existence <- function(unit, parsed_unit = TRUE){

    # Checking if unit exists as csCode_
    found_unit <- units_df[units_df$csCode_ == unit,]


    # No match based on given csCode_
    if(nrow(found_unit) < 1){

      #### !!!!!!!!!!! CHANGE 1: SI PREFIX PARCING DIRECTLY AFTER csCode_ AND BEFORE ciCode_ !!!!!!!!!!! ####
      if(isTRUE(parsed_unit)){

        #-----------------------------------------------#
        ####        Process SI Prefix Logic          ####
        #-----------------------------------------------#
        # Extracting the first character of the unit, which is assumed
        # to be an SI prefix
        unit_prefix <- substr(unit, start = 1, stop = 1)

        # Checking for a special SI condition where the SI prefix
        # might be "da" (deca) or "d" (deci)
        if(unit_prefix %in% c("d", "D", "Y", "Z", "E", "P", "T", "G", "M")){
          unit_prefix2 <- substr(unit, start = 1, stop = 2)

          if(unit_prefix2 %in% c("da", "DA", "YA", "YO", "ZA", "ZO", "EX",
                                 "PT", "TR", "GA", "MA")){
            unit_prefix <- unit_prefix2
          }
        }

        # Extracting the raw unit (base unit)
        if(unit_prefix %in% c("da", "DA", "YA", "YO", "ZA", "ZO", "EX",
                              "PT", "TR", "GA", "MA")){
          raw_unit <- substr(unit, start = 3, stop = nchar(unit))
        }else if(unit_prefix %in% c("Y", "Z", "E", "P", "T", "G", "M", "k",
                                    "K", "h", "H", "d", "D", "c", "C", "m",
                                    "M", "u", "U", "n", "N", "p", "P", "f",
                                    "F", "a", "A", "z", "y")){
          raw_unit <- substr(unit, start = 2, stop = nchar(unit))
        }else{
          raw_unit <- unit
        }

        # Checking if unit_prefix is a valid SI prefix based on case sensitivity
        # Note: "quetta - Q", "ronna - R", "ronto - r", "quecto - q"
        #       are not currently supported by UCUM (as of 25 March 2024)
        si_prefix <- si_prefixes[si_prefixes$prefix_cs == unit_prefix,]

        # No match found in case sensitive codes
        if(nrow(si_prefix) < 1){
          # Checking if unit_prefix is a valid SI prefix based on case insensitivity
          si_prefix <- si_prefixes[si_prefixes$prefix_ci == unit_prefix,]
        }

        # If the prefix doesn't exist, then there must be something wrong with
        # the given unit
        if(nrow(si_prefix) < 1){
          # stop(paste("The unit does not exist (SI-Prefix): ", unit))
        }

        # Check if the raw_unit exists as csCode_
        found_unit <- units_df[units_df$csCode_ == raw_unit,]

        # No match based on given csCode_
        if(nrow(found_unit) < 1){

          # Checking if the raw_unit exists as ciCode_
          found_unit <- units_df[units_df$ciCode_ == raw_unit,]
        }

        if(nrow(found_unit) < 1){
          # stop(paste("The unit does not exist: ", unit))
        }
        else{

          #### !!!!!!!!!!!! CHANGE 2: CHECKING SI PREFIX !!!!!!!!!!!! ####
          # If the found unit has no SI prefix, continue,
          # else return NULL
          if(isFALSE(found_unit$starts_with_si)){
            prefix_magnitude <- si_prefix$decimal
            unit_magnitude <- found_unit$magnitude_

            new_magnitude <- prefix_magnitude * unit_magnitude

            found_unit$csCode_ <- unit
            found_unit$ciCode_ <- unit
            found_unit$magnitude_ <- new_magnitude
          }
          else {
            return(NULL)
          }
        }

        if(nrow(found_unit) < 1){
          # stop(paste("The unit does not exist: ", unit))
        }
        else{
          return(found_unit)
        }
      }
      else{
        # Checking if unit exists as ciCode_
        found_unit <- units_df[units_df$ciCode_ == unit,]

        # No match based on given ciCode_
        if(nrow(found_unit) < 1){
          return(NULL)
        }
        # return the found unit based on the ciCode_
        else{
          return(found_unit)
        }
      }
    }
    # return the found unit based on the csCode_
    else{
      return(found_unit)
    }
  }

  #-----------------------------------------#
  #### Checking and Processing Functions ####
  #-----------------------------------------#

  # Checking if a unit string contains nested parentheses or not,
  # as well as whether it contains unclosed parentheses
  check_nested_parentheses <- function(input_string) {
    stack <- 0
    max_depth <- 0

    for (char in strsplit(input_string, "")[[1]]) {
      if (char == "(") {
        stack <- stack + 1
        if (stack > max_depth) {
          max_depth <- stack
        }
      } else if (char == ")") {
        if (stack == 0) {
          return(FALSE)  # Closing parenthesis without a corresponding opening parenthesis
        } else {
          stack <- stack - 1
        }
      }
    }

    if (stack > 0) {
      return(FALSE)  # Unclosed parentheses
    } else if (max_depth > 1) {
      return(TRUE)   # Nested parentheses
    } else {
      return(FALSE)  # No nested parentheses
    }
  }

  # Separating the input string based on the parentheses it contains.
  # A list is returned with each parenthesis segment being in a separate entry
  # inside the list.
  # eg. uStr <- "g/(11.h/(16.kg)).cm" gets converted to:
  #
  # "g/"
  # "(11.h/"
  # "(16.kg))"
  # ".cm"
  processParens <- function(uStr, nested_parens = FALSE){
    result <- c()
    character_pos <- 0
    character_start <- 1

    open_paren_pos <- 0
    open_paren_status <- 0

    close_paren_pos <- 0
    close_paren_status <- 0

    # The string contains nested parentheses
    if(isTRUE(nested_parens)){
      for(character in strsplit(uStr, "")[[1]]){

        # print(character)

        character_pos <- character_pos + 1

        # If an open parenthesis is found, add all characters until the parenthesis
        # to the result list.
        if(character == "("){
          open_paren_pos <- character_pos

          str_before_paren <- substr(uStr, character_start, open_paren_pos - 1)
          result <- c(result, str_before_paren)

          character_start <- character_pos
        }

        # If a closed parenthesis is found, check if the following character is
        # also a closed parenthesis. If yes, do nothing. if no, add all characters
        # until the parenthesis to the result list
        if(character == ")"){

          if(length(strsplit(uStr, "")[[1]]) != character_pos){
            if(strsplit(uStr, "")[[1]][character_pos + 1] != ")"){
              close_paren_pos <- character_pos

              str_before_paren <- substr(uStr, character_start, close_paren_pos)
              result <- c(result, str_before_paren)

              character_start <- character_pos + 1
            }
          }
        }

        # If character_pos has reached the end of the string
        # add all the remaining characters and add it to the result list.
        if(character_pos == length(strsplit(uStr, "")[[1]])){
          str_remaining <- substr(uStr, character_start, character_pos)
          result <- c(result, str_remaining)
        }
      }
    }

    # The string contains regular parentheses
    else{
      for(character in strsplit(uStr, "")[[1]]){
        character_pos <- character_pos + 1

        if(character == "("){
          open_paren_pos <- character_pos
          open_paren_status <- 1
        }

        if(character == ")"){
          close_paren_pos <- character_pos
          close_paren_status <- 1
        }

        # Once a parenthesis has opened and closed, add all characters before
        # the parenthesis to the results list
        # Then add the parenthesis to the results list, reset and continue
        # looking through the characters.
        if(open_paren_status == 1  & close_paren_status == 1){
          str_before_paren <- substr(uStr, character_start, open_paren_pos - 1)
          parenthesis <- substr(uStr, open_paren_pos, close_paren_pos)

          result <- c(result, str_before_paren, parenthesis)
          character_start <- close_paren_pos + 1
          open_paren_status <- 0
          close_paren_status <- 0
        }
      }

      if(close_paren_pos != character_pos){
        str_after_last_paren <- substr(uStr, close_paren_pos + 1, character_pos)
        result <- c(result, str_after_last_paren)
      }
    }

    return(as.list(result))
  }


  # Splitting the elements of the given list based on whether an operator
  # exists either as the first or last character of the element string
  processOperators <- function(uStr_list){

    result <- c()

    for(element in uStr_list){

      # Making sure that the string element is larger than 1
      if(length(strsplit(element, "")[[1]]) > 1){

        # Checking if string element starts AND ends with an operator
        if((startsWith(element, "\\*") | startsWith(element, "\\/")) &
           (endsWith(element, "\\*") | endsWith(element, "\\/"))){

          # Extracting first operator
          first_char <- substr(element, start = 1, stop = 1)
          new_element <- sub('.', '', element)

          # Extracting last operator
          last_char <- substr(element, start = -1, stop = -1)
          new_element <- gsub('.$', '', new_element)

          result <- c(result, first_char, new_element, last_char)
        }
        else if(startsWith(element, "\\*") | startsWith(element, "\\/")){

          # Extracting the first character of the element string
          # which should be an operator
          first_char <- substr(element, start = 1, stop = 1)
          new_element <- sub('.', '', element)

          result <- c(result, first_char, new_element)
        }
        else if(endsWith(element, "\\*") | endsWith(element, "\\/")){

          # Extracting the last character of the element string
          # which should be an operator
          last_char <- substr(element, start = -1, stop = -1)
          new_element <- gsub('.$', '', element)

          result <- c(result, new_element, last_char)
        }
        else{
          result <- c(result, element)
        }
      }
      else{
        result <- c(result, element)
      }
    }

    return(as.list(result))
  }


  # Detecting and processing parts of the string that contain
  # exponents in order to indicate that they are exponents
  processExponents <- function(uStr){

    # Extracting all strings that appear to contain an exponent
    # in either of the following formats:
    #
    # UnitDigit, Unit+Digit, Unit-Digit

    # pattern <- "\\b(?:[a-zA-Z]+[+|-]*\\d+)"
    # pattern <- "\\b(?:[a-zA-Z]+[+|-]*\\d+)|(?:[a-zA-Z]*\\[[a-zA-Z]*\\][+|-]*\\d+)"
    pattern <- "\\b(?!H2O\\b)(?:(?:[A-Za-z]+[+\\-]*\\d+)|(?:[A-Za-z]*\\[[A-Za-z]*\\][+\\-]*\\d+))\\b"
    matches <- gregexpr(pattern, uStr, perl = TRUE)
    exponent_matches <- unlist(regmatches(uStr, matches))

    # print(exponent_matches)

    # Extracting the remaining string parts
    remaining_string <- unlist(strsplit(uStr, pattern, perl = TRUE))

    # # Combining the exponent_matches with the remaining_string.
    # # The result should be exactly the same as uStr
    # result <- c(rbind(remaining_string, exponent_matches))

    # Combining the exponent_matches with the remaining_string.
    # The result should be exactly the same as uStr
    result <- character(length(uStr))
    for (i in seq_along(remaining_string)) {
      result[2*i - 1] <- remaining_string[i]
    }
    for (i in seq_along(exponent_matches)) {
      result[2*i] <- exponent_matches[i]
    }


    # Checking that the two strings are the same
    if(as.character(paste(result, collapse = "")) != uStr){
    }
    else{

      position <- 0
      for(element in result){
        position <- position + 1

        # Checking if exponent string contains a "+" (positive exponent)
        if(element %in% exponent_matches){
          if(grepl("\\+", element)){
            exponent_split <- strsplit(element, "\\+")
            new_element <- as.character(paste(exponent_split[[1]], collapse = "^"))

            result[position] <- new_element
          }

          # Checking if exponent string contains a "-" (negative exponent)
          else if(grepl("\\-", element)){
            exponent_split <- strsplit(element, "\\-")
            new_element <- as.character(paste(exponent_split[[1]], collapse = "^-"))

            result[position] <- new_element
          }

          # Assuming that exponent string doesn't contain anything (positive exponent)
          else{
            non_digits <- regmatches(element, gregexpr("\\D+", element))
            digits <- regmatches(element, gregexpr("\\d+", element))

            new_element <- paste0(non_digits, "^", digits)
            result[position] <- new_element
          }
        }
      }

      return(as.character(paste(result, collapse = "")))
    }
  }


  # Identifying whether the unit expression contains non-ratio units (cnv_)
  # and also checking for the expression's validity

  identify_cnv <- function(uStr_processed){

    # Setting the unit counters
    n_units <- 0
    n_cnv_units <- 0

    # Setting the default cnv_ to NA
    cnv_ <- NA

    csCode_ <- NA

    # Detecting operators inside the unit expression
    operators <- grepl("\\/|\\*", uStr_processed)
    division <- grepl("\\/", uStr_processed)


    # Splitting expression based on the operators (/,*)
    split <- t(strsplit(uStr_processed, "(?<=[\\/\\*])|(?=[\\/\\*])", perl = TRUE)[[1]])
    for(split_element in split){

      # Checking if the split element contains an open or closed parenthesis
      if(grepl("\\(", split_element) | grepl("\\)", split_element)){

        # Splitting on the parenthesis level
        split_parens <- t(strsplit(split_element, "(?<=[\\(\\)])|(?=[\\(\\)])", perl = TRUE)[[1]])
        for(split_parens_element in split_parens){

          # Checking the split elements that do not contain a whitespace
          # or a parenthesis
          if(split_parens_element != "" & split_parens_element != "(" &
             split_parens_element != ")"){

            # Checking if parenthesis element contains an exponent
            if(isTRUE(grepl("\\^", split_parens_element))){

              # Splitting based on the exponent
              exponent_split <- strsplit(split_parens_element, "\\^")

              # print(exponent_split)

              for(exp_split_element in exponent_split[[1]]){

                # Checking if the exponent element is non-numeric
                if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

                  # Checking if the exponent element is a unit that exists
                  # inside the cnv_units.
                  # If it exists, increase the n_cnv_units counter
                  if(exp_split_element %in% cnv_units$csCode_ |
                     exp_split_element %in% cnv_units$ciCode_){
                    n_cnv_units <- n_cnv_units + 1

                    # Filtering the cnv_units with the found unit to extract
                    # its cnv_ value
                    cnv_unit <- cnv_units[cnv_units$csCode_ == exp_split_element,]
                    if(nrow(cnv_unit) < 1){
                      cnv_unit <- cnv_units[cnv_units$ciCode_ == exp_split_element,]
                    }
                    cnv_ <- cnv_unit$cnv_
                    csCode_ <- cnv_unit$csCode_
                  }
                  # If it doesn't exist, check whether it exists inside the
                  # units_df as a unit
                  # If it exists, increase the n_units counter
                  else{
                    unit <- check_unit_existence(exp_split_element,
                                                 parsed_unit = TRUE)
                    if(nrow(unit) == 1){
                      n_units <- n_units + 1
                    }
                    # if it doesn't exist, throw an error message
                    else{
                      # stop(paste0("ERROR UNIT NOT FOUND: ",exp_split_element))
                    }
                  }
                }
              }
            }
            # If parenthesis element doesn't include an exponent,
            # check if the parenthesis element is a unit that exists
            # inside the cnv_units
            else{
              # If it exists, increase the n_cnv_units counter
              if(is.na(suppressWarnings(as.numeric(split_parens_element)))){
                if(split_parens_element %in% cnv_units$csCode_ |
                   split_parens_element %in% cnv_units$ciCode_){
                  n_cnv_units <- n_cnv_units + 1

                  # Filtering the cnv_units with the found unit to extract
                  # its cnv_ value
                  cnv_unit <- cnv_units[cnv_units$csCode_ == split_parens_element,]
                  if(nrow(cnv_unit) < 1){
                    cnv_unit <- cnv_units[cnv_units$ciCode_ == split_parens_element,]
                  }
                  cnv_ <- cnv_unit$cnv_
                  csCode_ <- cnv_unit$csCode_
                }
                # If it doesn't exist, check whether it exists inside the
                # units_df as a unit
                # If it exists, increase the n_units counter
                else{
                  unit <- check_unit_existence(split_parens_element,
                                               parsed_unit = TRUE)
                  if(nrow(unit) == 1){
                    n_units <- n_units + 1
                  }
                  # if it doesn't exist, throw an error message
                  else{
                    # stop(paste0("ERROR UNIT NOT FOUND: ",split_parens_element))
                  }
                }
              }
            }
          }
        }
      }
      # If split element doesn't contain a parenthesis
      else{
        # Check if split element contains an exponent
        if(isTRUE(grepl("\\^", split_element))){

          # Splitting based on the exponent
          exponent_split <- strsplit(split_element, "\\^")

          # print(exponent_split)

          for(exp_split_element in exponent_split[[1]]){

            # Checking if the exponent element is non-numeric
            if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

              # Checking if the exponent element is a unit that exists
              # inside the cnv_units.
              # If it exists, increase the n_cnv_units counter
              if(exp_split_element %in% cnv_units$csCode_ |
                 exp_split_element %in% cnv_units$ciCode_){
                n_cnv_units <- n_cnv_units + 1

                # Filtering the cnv_units with the found unit to extract
                # its cnv_ value
                cnv_unit <- cnv_units[cnv_units$csCode_ == exp_split_element,]
                if(nrow(cnv_unit) < 1){
                  cnv_unit <- cnv_units[cnv_units$ciCode_ == exp_split_element,]
                }
                cnv_ <- cnv_unit$cnv_
                csCode_ <- cnv_unit$csCode_
              }
              # If it doesn't exist, check whether it exists inside the
              # units_df as a unit
              # If it exists, increase the n_units counter
              else{
                unit <- check_unit_existence(exp_split_element,
                                             parsed_unit = TRUE)
                if(nrow(unit) == 1){
                  n_units <- n_units + 1
                }
                # if it doesn't exist, throw an error message
                else{
                  # stop(paste0("ERROR UNIT NOT FOUND: ",exp_split_element))
                }
              }
            }
          }
        }
        # If split element doesn't include an exponent,
        # check if the split element is a unit that exists
        # inside the cnv_units
        else{
          if(is.na(suppressWarnings(as.numeric(split_element)))){

            # If it exists, increase the n_cnv_units counter
            if(split_element %in% cnv_units$csCode_ |
               split_element %in% cnv_units$ciCode_){
              n_cnv_units <- n_cnv_units + 1

              # Filtering the cnv_units with the found unit to extract
              # its cnv_ value
              cnv_unit <- cnv_units[cnv_units$csCode_ == split_element,]
              if(nrow(cnv_unit) < 1){
                cnv_unit <- cnv_units[cnv_units$ciCode_ == split_element,]
              }
              cnv_ <- cnv_unit$cnv_
              csCode_ <- cnv_unit$csCode_
            }

            # If it doesn't exist, check that split element is not an operator,
            # then check whether it exists inside the units_df as a unit
            else{
              if(split_element != "/" & split_element != "*"){
                unit <- check_unit_existence(split_element,
                                             parsed_unit = TRUE)

                # If it exists, increase the n_units counter
                if(nrow(unit) == 1){
                  n_units <- n_units + 1
                }
                # if it doesn't exist, throw an error message
                else{
                  # stop(paste0("ERROR UNIT NOT FOUND: ",split_element))
                }
              }
            }
          }
        }
      }
    }

    # Throw an error if there are multiple cnv_ units inside the expression
    if(n_cnv_units > 1){
      # stop("Invalid Unit Expression")
    }

    # Throw an error if there are both cnv_ and other units inside the expression
    if(n_cnv_units == 1 & n_units > 0 & isTRUE(operators)){
      # stop("Unable to multiply or divide with non-ratio units")
    }

    # Throw an error if a division is occurring inside the expression
    if(n_cnv_units == 1 & isTRUE(division)){
      # stop("Attempt to divide with a non-ratio unit")
    }

    # Return TRUE only if the expression contains a single cnv_ unit
    if(n_cnv_units == 1 & n_units == 0){
      return(list(cnv_exists = TRUE,
                  cs_code = csCode_,
                  cnv_value = cnv_))
    }
    else{
      return(list(cnv_exists = FALSE,
                  cs_code = csCode_,
                  cnv_value = cnv_))
    }
  }
  #-----------------------------#
  #### Calculation Functions ####
  #-----------------------------#

  # Calculating the magnitude of a given unit expression
  calculate_magnitude <- function(uStr_processed){

    parentheses_check <- TRUE

    while(isTRUE(parentheses_check)){
      if(TRUE %in% grepl("\\(", uStr_processed)){
        parentheses_check <- TRUE
      }
      else{
        parentheses_check <- FALSE
      }

      if(isTRUE(parentheses_check)){

        position <- 0

        for(element in uStr_processed){

          position <- position + 1

          # Checking if element of the list is a parenthesis
          if(grepl("\\(", element) & grepl("\\)", element)){

            paren_pos <- position

            # Splitting the parenthesis based on the operators ("." or "/")
            split <- t(strsplit(element, "(?<=[\\/\\*])|(?=[\\/\\*])", perl = TRUE)[[1]])

            # print("==============")
            # print(split)
            # print("==============")

            expression <- ""

            for(split_element in split){

              # Removing opening parenthesis
              new_element <- gsub("\\(", "", split_element)

              if(isTRUE(grepl("\\)", new_element))){

                # Finding the number of closed parentheses
                matches <- gregexpr("\\)", new_element)
                n_close_parens <- sum(sapply(matches, function(x) ifelse(x[1] == -1, 0, length(x))))
                if(n_close_parens > 0){
                  n_close_parens <- n_close_parens - 1
                }

                # Removing closing parenthesis
                new_element <- gsub("\\)", "", new_element)
              }

              if(!is.na(suppressWarnings(as.numeric(new_element)))){
                expression <- paste0(expression,as.numeric(new_element))
              }
              else if(new_element == "*"){
                expression <- paste0(expression,"*")
              }
              else if(new_element == "/"){
                expression <- paste0(expression,"/")
              }
              else{

                if(isTRUE(grepl("\\^", new_element))){

                  exponent_split <- strsplit(new_element, "\\^")

                  position <- 0

                  for(exp_split_element in exponent_split[[1]]){
                    position <- position + 1

                    # check if exp_split_element is not a digit
                    if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

                      # Attempting to extract the magnitude
                      unit <- check_unit_existence(exp_split_element,
                                                   parsed_unit = TRUE)
                      exponent_split[[1]][position] <- unit$magnitude_
                    }
                  }

                  new_element <- as.character(paste(exponent_split[[1]], collapse = "^"))
                  expression <- paste0(expression, new_element)
                }
                else{
                  # Look for the unit inside units_df
                  unit <- check_unit_existence(new_element,
                                               parsed_unit = TRUE)

                  expression <- paste0(expression, unit$magnitude_)
                }
              }
            }
            # Parsing the expression (the result should be a digit)
            expression_result <- eval(parse(text = expression))

            # Adding the remaining closed parentheses to the expression result
            parentheses <- rep(")", times = n_close_parens)
            parentheses <- paste(parentheses, collapse = "")
            expression_result <- paste0(expression_result, parentheses)
            # Replacing the expression with the result
            uStr_processed[paren_pos] <- expression_result
          }
        }

        uStr_processed <- as.character(paste(uStr_processed, collapse = ""))
        uStr_processed <- processParens(uStr_processed, check_nested_parentheses(uStr_processed))
      }
      else{
        split <- t(strsplit(as.character(uStr_processed), "(?<=[\\/\\*])|(?=[\\/\\*])", perl = TRUE)[[1]])
        expression <- ""

        for(split_element in split){

          if(isTRUE(grepl("\\^", split_element))){

            exponent_split <- strsplit(split_element, "\\^")

            position <- 0

            for(exp_split_element in exponent_split[[1]]){
              position <- position + 1

              # check if exp_split_element is not a digit
              if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

                # Attempting to extract the magnitude
                unit <- check_unit_existence(exp_split_element,
                                             parsed_unit = TRUE)
                exponent_split[[1]][position] <- unit$magnitude_
              }
            }

            new_element <- as.character(paste(exponent_split[[1]], collapse = "^"))
            expression <- paste0(expression, new_element)
          }
          else if(!is.na(suppressWarnings(as.numeric(split_element)))){
            expression <- paste0(expression, as.numeric(split_element))
          }
          else if(split_element == "*" | split_element == "/"){
            expression <- paste0(expression, split_element)
          }
          else{
            unit <- check_unit_existence(split_element,
                                         parsed_unit = TRUE)

            expression <- paste0(expression, unit$magnitude_)
          }
        }

        # print(expression)

        magnitude <- eval(parse(text = expression))
        return(magnitude)
      }
    }
  }


  # Calculating the dimension of a given unit expression
  calculate_dimension <- function(uStr_processed){
    split <- t(strsplit(uStr_processed, "(?<=[\\/\\*])|(?=[\\/\\*])", perl = TRUE)[[1]])
    expression <- ""

    for(split_element in split){
      if(grepl("\\(", split_element) | grepl("\\)", split_element)){
        split_parens <- t(strsplit(split_element, "(?<=[\\(\\)])|(?=[\\(\\)])", perl = TRUE)[[1]])
        parens_position <- 0

        for(split_parens_element in split_parens){
          parens_position <- parens_position + 1
          if(split_parens_element != "" & split_parens_element != "(" &
             split_parens_element != ")"){
            if(isTRUE(grepl("\\^", split_parens_element))){
              exponent_split <- strsplit(split_parens_element, "\\^")

              position <- 0

              split_contains_unit <- FALSE

              for(exp_split_element in exponent_split[[1]]){
                position <- position + 1

                # check if exp_split_element is not a digit
                if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

                  # Attempting to extract the magnitude
                  unit <- check_unit_existence(exp_split_element,
                                               parsed_unit = TRUE)

                  exponent_split[[1]][position] <- as.character(unit$dim_[[1]])
                  split_contains_unit <- TRUE
                }
                else if(!is.na(suppressWarnings(as.numeric(exp_split_element))) & !split_contains_unit){
                  exponent_split[[1]][position] <- "c(0, 0, 0, 0, 0, 0, 0)"
                  split_contains_unit <- FALSE
                }
              }

              if(split_contains_unit){
                new_element <- as.character(paste(exponent_split[[1]], collapse = "*"))
              }
              else{
                new_element <- as.character(paste(exponent_split[[1]], collapse = "+"))
              }

              expression <- paste0(expression, new_element)
            }
            else if(!is.na(suppressWarnings(as.numeric(split_parens_element)))){
              expression <- paste0(expression, "c(0, 0, 0, 0, 0, 0, 0)")
            }
            else if (split_parens_element == "*"){
              expression <- paste0(expression, "+")
            }
            else if (split_parens_element == "/"){
              expression <- paste0(expression, "-")
            }
            else{
              unit <- check_unit_existence(split_parens_element,
                                           parsed_unit = TRUE)

              expression <- paste0(expression, as.character(unit$dim_[[1]]))
            }
          }
          else{
            expression <- paste0(expression, as.character(split_parens_element))
          }
        }
      }
      else{
        if(isTRUE(grepl("\\^", split_element))){

          exponent_split <- strsplit(split_element, "\\^")

          position <- 0

          split_contains_unit <- FALSE

          for(exp_split_element in exponent_split[[1]]){
            position <- position + 1

            # check if exp_split_element is not a digit
            if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

              # Attempting to extract the magnitude
              unit <- check_unit_existence(exp_split_element,
                                           parsed_unit = TRUE)

              exponent_split[[1]][position] <- as.character(unit$dim_[[1]])
              split_contains_unit <- TRUE
            }
            else if(!is.na(suppressWarnings(as.numeric(exp_split_element))) & !split_contains_unit){
              exponent_split[[1]][position] <- "c(0, 0, 0, 0, 0, 0, 0)"
              split_contains_unit <- FALSE
            }
          }

          if(split_contains_unit){
            new_element <- as.character(paste(exponent_split[[1]], collapse = "*"))
          }
          else{
            new_element <- as.character(paste(exponent_split[[1]], collapse = "+"))
          }

          expression <- paste0(expression, new_element)
        }
        else if(!is.na(suppressWarnings(as.numeric(split_element)))){
          expression <- paste0(expression, "c(0, 0, 0, 0, 0, 0, 0)")
        }
        else if (split_element == "*"){
          expression <- paste0(expression, "+")
        }
        else if (split_element == "/"){
          expression <- paste0(expression, "-")
        }
        else{
          unit <- check_unit_existence(split_element,
                                       parsed_unit = TRUE)

          expression <- paste0(expression, as.character(unit$dim_[[1]]))
        }
      }
    }

    # print(expression)
    dimension <- eval(parse(text = expression))
    # print(dimension)
    return(dimension)
  }
  # Calculating the mole exponent of a given unit expression
  calculate_moleExponent <- function(uStr_processed){
    split <- t(strsplit(uStr_processed, "(?<=[\\/\\*])|(?=[\\/\\*])", perl = TRUE)[[1]])
    expression <- ""

    for(split_element in split){

      if(grepl("\\(", split_element) | grepl("\\)", split_element)){

        split_parens <- t(strsplit(split_element, "(?<=[\\(\\)])|(?=[\\(\\)])", perl = TRUE)[[1]])

        parens_position <- 0

        for(split_parens_element in split_parens){

          parens_position <- parens_position + 1

          if(split_parens_element != "" & split_parens_element != "(" &
             split_parens_element != ")"){

            if(isTRUE(grepl("\\^", split_parens_element))){

              exponent_split <- strsplit(split_parens_element, "\\^")

              position <- 0

              split_contains_unit <- FALSE

              for(exp_split_element in exponent_split[[1]]){
                position <- position + 1

                # check if exp_split_element is not a digit
                if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

                  # Attempting to extract the mole exponent
                  unit <- check_unit_existence(exp_split_element,
                                               parsed_unit = TRUE)

                  exponent_split[[1]][position] <- as.character(unit$moleExp_[[1]])
                  split_contains_unit <- TRUE
                }
                else if(!is.na(suppressWarnings(as.numeric(exp_split_element))) & !split_contains_unit){
                  exponent_split[[1]][position] <- "0"
                  split_contains_unit <- FALSE
                }
                else{
                  exponent_split[[1]][position] <- "1"
                  split_contains_unit <- TRUE
                }
              }

              if(split_contains_unit){
                new_element <- as.character(paste(exponent_split[[1]], collapse = "*"))
              }
              else{
                new_element <- as.character(paste(exponent_split[[1]], collapse = "+"))
              }

              expression <- paste0(expression, new_element)
            }
            else if(!is.na(suppressWarnings(as.numeric(split_parens_element)))){
              expression <- paste0(expression, "0")
            }
            else if (split_parens_element == "*"){
              expression <- paste0(expression, "+")
            }
            else if (split_parens_element == "/"){
              expression <- paste0(expression, "-")
            }
            else{
              unit <- check_unit_existence(split_parens_element,
                                           parsed_unit = TRUE)

              expression <- paste0(expression, as.character(unit$moleExp_[[1]]))
            }
          }
          else{
            expression <- paste0(expression, as.character(split_parens_element))
          }
        }
      }
      else{
        if(isTRUE(grepl("\\^", split_element))){

          exponent_split <- strsplit(split_element, "\\^")

          # print(exponent_split)

          position <- 0

          split_contains_unit <- FALSE

          for(exp_split_element in exponent_split[[1]]){
            position <- position + 1

            # check if exp_split_element is not a digit
            if(is.na(suppressWarnings(as.numeric(exp_split_element)))){

              # Attempting to extract the mole exponent
              unit <- check_unit_existence(exp_split_element,
                                           parsed_unit = TRUE)

              exponent_split[[1]][position] <- as.character(unit$moleExp_[[1]])
              split_contains_unit <- TRUE
            }
            else if(!is.na(suppressWarnings(as.numeric(exp_split_element))) & !split_contains_unit){
              exponent_split[[1]][position] <- "0"
              split_contains_unit <- FALSE
            }
            else{
              exponent_split[[1]][position] <- "1"
              split_contains_unit <- TRUE
            }
          }

          if(split_contains_unit){
            new_element <- as.character(paste(exponent_split[[1]], collapse = "*"))
          }
          else{
            new_element <- as.character(paste(exponent_split[[1]], collapse = "+"))
          }

          expression <- paste0(expression, new_element)
        }
        else if(!is.na(suppressWarnings(as.numeric(split_element)))){
          expression <- paste0(expression, "0")
        }
        else if (split_element == "*"){
          expression <- paste0(expression, "+")
        }
        else if (split_element == "/"){
          expression <- paste0(expression, "-")
        }
        else{
          unit <- check_unit_existence(split_element,
                                       parsed_unit = TRUE)

          expression <- paste0(expression, as.character(unit$moleExp_[[1]]))
        }
      }
    }

    # print(expression)
    moleExp <- eval(parse(text = expression))
    # print(moleExp)
    return(moleExp)
  }


  # Calculating the magnitude_ and cnvPfx_ of cnv unit expressions
  calculate_cnv_mag_pfx <- function(uStr, cnv_csCode_){

    # Formatting cnv_csCode_ by revealing the escape characters
    # "\\" in order to properly process cases where
    # cnv_csCode_ contains "[...]"
    format_cnv_csCode_ <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", cnv_csCode_, perl = TRUE)

    # Establishing the pattern used for extracting the cnv_ from
    # the uStr expression containg any potential exponents
    pattern <- paste0(format_cnv_csCode_,"\\^?[\\+|\\-]?\\d*")

    # Extracting the cnv_ with the exponents from uStr
    extract <- regmatches(uStr, gregexpr(pattern, uStr))

    # Filtering the cnv_units for the cnv used inside the expression
    # and extracting its magnitude and cnvPfx_ value (a unit should ALWAYS be found)
    unit <- cnv_units[cnv_units$csCode_ == cnv_csCode_,]

    cnv_unit_magnitude <- unit$magnitude_
    cnvPfx_ <- unit$cnvPfx_

    # Replacing the cnv_ with it's magnitude and parsing the expression
    new_extract <- gsub(format_cnv_csCode_, as.character(cnv_unit_magnitude), extract)

    cnv_magnitude <- eval(parse(text = new_extract))



    # Splitting the uStr expression based on the defined pattern
    substring <- strsplit(uStr, pattern)

    expression <- ""

    for(string in substring[[1]]){
      # Removing any opening parenthesis at the end of the string
      if(endsWith(string, "\\(")){
        new_string <- gsub("\\(", "", string)
        expression <- paste0(expression, new_string)
      }
      # Removing any closing parentheses at the beginning of the string,
      # as well as any operator
      else if(grepl("\\)?[\\*|\\/]*", string)){
        new_string <- gsub("^\\)?[\\*|\\/]*", "", string)
        expression <- paste0(expression, new_string)
      }
      else{
        expression <- paste0(expression, string)
      }
    }

    expression <- paste0(expression,"*",cnvPfx_)
    cnvPfx_ <- eval(parse(text = expression))
    return(list(cnv_magnitude = cnv_magnitude,
                cnvPfx_ = cnvPfx_))
  }

  #------------------------#
  #### Parse Unit Logic ####
  #------------------------#

  original_uStr <- uStr

  #### !!!!!!!!!!!! CHANGE 3: HOTFIX FOR ANNOTATIONS !!!!!!!!!!!! ####
  # There was a unit which was something like this: {annotation}.unit (g.m/({hb}.m2))
  # Removing annotations
  # Replacing annotation with 1 if a multiplication exists
  # immediately after the annotation
  if(grepl("\\}\\.", uStr)){
    uStr <- gsub("[./]*\\{[^{}]*\\}", '1', uStr)
  }
  else{
    uStr <- gsub("[./]*\\{[^{}]*\\}", '', uStr)
  }
  if(uStr == ""){
    uStr <- "1"
  }

  # Fixing instances where units start with "/" by adding a 1 before the "/"
  if(grepl("^\\/", uStr)){
    uStr <- gsub("^\\/", "1/", uStr)
  }

  # Fixing exponents and multiplication symbols
  uStr <- gsub('\\*', '^', uStr)
  uStr <- gsub('\\.', '*', uStr)
  uStr <- processExponents(uStr)

  # Checking if uStr contains a non-ratio unit, which require
  # special treatment
  cnv_result <- identify_cnv(uStr)

  cnv <- cnv_result$cnv_exists
  cnv_csCode_ <- cnv_result$cs_code
  cnv_value <- cnv_result$cnv_value

  # Checking if unit expression contains an arbitrary unit
  if(grepl(arbitrary_units, uStr)){
    arbitrary <- TRUE
  }
  else{
    arbitrary <- FALSE
  }

  if(isTRUE(cnv)){

    # Calculate the dimension of the given unit expression
    unit_dimension <- calculate_dimension(uStr)

    unit_dimension <- paste0("c(",paste(unit_dimension, collapse = ", "),")")

    # Calculate the mole exponent of the given unit expression
    moleExp_ <- calculate_moleExponent(uStr)

    # Calculating cnv_ magnitude and Pfx_
    cnv_mag_pfx <- calculate_cnv_mag_pfx(uStr, cnv_csCode_)
    unit_mag <- cnv_mag_pfx$cnv_magnitude
    if(is.nan(unit_mag)){
      unit_mag <- 1
    }

    cnvPfx_ <- cnv_mag_pfx$cnvPfx_
    new_unit <- data.frame(csCode_ = original_uStr,
                           ciCode_ = original_uStr,
                           magnitude_ = unit_mag,
                           dim_ = unit_dimension,
                           cnv_ = cnv_value,
                           cnvPfx_ = cnvPfx_,
                           isArbitrary_ = arbitrary,
                           moleExp_ = moleExp_,
                           source = "user_added",
                           starts_with_si = NA,
                           flag = NA_character_)


    return(new_unit)

  }else{

    # Processing the unit expression by separating the parentheses
    uStr_processed <- processParens(uStr, check_nested_parentheses(uStr))
    # print(uStr_processed)

    # Processing the unit expression by separating the operators from the
    # beginning and end of each string element
    uStr_processed <- processOperators(uStr_processed)
    # print(uStr_processed)

    # Calculating the magnitude of the given unit expression
    unit_mag <- calculate_magnitude(uStr_processed)
    if(is.nan(unit_mag)){
      unit_mag <- 1
    }

    # Calculate the dimension of the given unit expression
    unit_dimension <- calculate_dimension(uStr)

    unit_dimension <- paste0("c(",paste(unit_dimension, collapse = ", "),")")

    # Calculate the mole exponent of the given unit expression
    moleExp_ <- calculate_moleExponent(uStr)

    # cnvPfx_ is set to 1
    cnvPfx_ <- 1

    new_unit <- data.frame(csCode_ = original_uStr,
                           ciCode_ = original_uStr,
                           magnitude_ = unit_mag,
                           dim_ = unit_dimension,
                           cnv_ = cnv_value,
                           cnvPfx_ = cnvPfx_,
                           isArbitrary_ = arbitrary,
                           moleExp_ = moleExp_,
                           source = "user_added",
                           starts_with_si = NA,
                           flag = NA_character_)

    return(new_unit)
  }
}
