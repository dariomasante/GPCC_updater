.First <- function(){
  ## Dario Masante - May 2017
  ## dario.masante@ext.ec.europa.eu (dario.masante@gmail.com)
  ## This set of functions starts a GUI to allow update of GPCC data.
  ## Downloads GPCC data and updates netcdf files and database, via a user-friendly GUI (point and click), 
  ## powered by the gWidgets package for R and relying solely on R installation. 
  ## Provided that R is already installed and that .RData files are opened with it by default, 
  ## double clicking the RData file will start up the application. No code typing whatsoever is required.
  ## A function wrapping up the application is loaded and visible in the R global environment as well, 
  ## allowing the application to be started in an already open R session: GPCC_updater()
  
  # Version number:
  vers = 'v2.2'
  
  install_missing()
  if(!require(gWidgets)) {utils::install.packages("gWidgets")}
  if(!require(gWidgetstcltk)) {utils::install.packages("gWidgetstcltk")}
  library(gWidgets)

  ## Start up the GUI ----
  # The following builds the user interface and the logic underneath, calling functions depending by user choices
  GUI = function(){
    win = gwindow(paste("GPCC data updater -", vers), visible = FALSE, width = 700)
    titleFrame = gframe("", container = win)
    glabel("This application downloads and stores data from GPCC. \nPlease follow execution and messages on the R console", 
           container = titleFrame)
    
    lastMonth = as.integer(format(Sys.Date(), "%m")) - 1
    lastMonth = ifelse(lastMonth == 0, 12, lastMonth)
    currentYear = ifelse(lastMonth == 12, as.integer(format(Sys.Date(), "%Y")) - 1, as.integer(format(Sys.Date(), "%Y")))
    
    periodFrame = gframe("Select month of interest", container = win)
    yr_lbl = glabel("Year: ", container = periodFrame)
    yr_txt = gedit(currentYear, container = periodFrame, width=5)
    mm_lbl = glabel("          Month: ", container = periodFrame)
    mm_sel = gdroplist(1:12, selected=lastMonth, container = periodFrame)
    
    #yr_sld <- gslider(from=1982, to = currentYear, by =1, value=currentYear, container=periodFrame)
    
    whichFrame = gframe("What GPCC data to use?", container = win)
    ticklist_what = c("First guess", "Monitoring (v6)")
    what_chk = gradio(ticklist_what, container = whichFrame)
    
    upFrame = gframe("What would you like to update?", container = win, horizontal=FALSE)
    ticklist = c("Netcdf file","Database")
    chk_sel = gcheckboxgroup(ticklist, container=upFrame, checked=TRUE)
    
    target_lbl = glabel('\nTarget netcdf file (.nc): ', container = upFrame)
    target_netcdf = gfilebrowse(text = '', type = "open", quote = TRUE, filter = list("All files"=list(patterns='*.nc')),
                               container = upFrame, toolkit = guiToolkit(), width = 35) 
    # Database inputs
    db_lbl = glabel('\nDatabase: ', container = upFrame)
    target_db = gedit('', container = upFrame)
    user_lbl = glabel('Username: ', container = upFrame)
    user_db = gedit('', container = upFrame)
    pass_lbl = glabel('Password: ', container = upFrame)
    pass_db = gedit('', container = upFrame)
    visible(pass_db) = FALSE 
    
    tick_interm = '\nSelect to remove the downloaded netcdf after update\n'
    interm_sel = gcheckboxgroup(tick_interm, container = win, checked=TRUE)
    glabel(" ", container = win)
    
    # Run button ----
    btnCalc = gbutton("\nRun\n", container = win, handler = function(h, ...) {
      cat('\n### STARTED ###  \n')
      yr = as.integer(svalue(yr_txt))
      mm = svalue(mm_sel)
      what = svalue(what_chk)
      chk = ifelse(svalue(chk_sel) == "Database", 'db', 'nc') 
      tnc = svalue(target_netcdf)
      if(length(chk) == 0){
        gmessage("Please select at least one item to update (netcdf or database)")
      }
      if(ticklist[1] %in% chk & nchar(tnc) == 0){ # When netcdf is selected
        gmessage("Please add the target netcdf to update.")
      }
      if(ticklist[2] %in% chk){ # When database is selected
        if(nchar(tdb) == 0 | nchar(pdb) == 0 | nchar(udb) == 0){
          gmessage("Please specify database to update, user and password.")
        }
      }
      GPCC_cmd_update(mm, yr, what, chk, tnc, tdb, udb, pdb)
    })
    
    glabel(" ", container = win)
    
    btnExit = gbutton("Cancel", handler = function(h,...) dispose(win), container=win)
    
    visible(win) = TRUE
    focus(win)
  }
  
  GUI()
  
}

GPCC_updater = .First # to make the function visible in the global environment as 'GPCC_updater'

# Author      : Dario Masante
# Date     : 2017-03-30
# Dependencies: GPCC monthly v6 and first guess FTPs   
# Reverse : GPCC_application.RData / GPCC_terminal_update.R / GPCC_self_update.R
# Requires: R 
# Purpose    : This set of functions allow update of precipitation data from GPCC.

## Install packages if not installed yet, then load
install_missing = function() {
  mirr = "https://cloud.r-project.org"
  if(!require(ncdf4)) {utils::install.packages("ncdf4", repos=mirr)}
  if(!require(RODBC)) {utils::install.packages("RODBC", repos=mirr)}
  if(!require(R.utils)) {utils::install.packages("R.utils", repos=mirr)}
  if(!require(GenKern)) {utils::install.packages("GenKern", repos=mirr)}
  if(!require(lmomco)) {utils::install.packages("lmomco", repos=mirr)}
  if(!require(chron)) {utils::install.packages("chron", repos=mirr)}
  if(!require(parallel)) {utils::install.packages("parallel", repos=mirr)}
  if(!require(snow)) {utils::install.packages("snow", repos=mirr)}
  if(!require(doSNOW)) {utils::install.packages("doSNOW", repos=mirr)}
  if(!require(foreach)) {utils::install.packages("foreach", repos=mirr)}
}

## Pop up messages for interactive interface only, copes with both simple mess. and confirm mess.
uiMessage = function(m, conf=NULL){
  if(suppressWarnings(library(gWidgetstcltk, logical.return = TRUE))){
    if(is.null(conf)){
      return( gmessage(m) )
    } else {
      conf = gconfirm(m)
    }
  }
  return(conf)
}

## Function to download data and unzip if necessary
# nc_link: url to the gpcc data (both netcdf or ascii)
download_and_check = function(nc_link){
  gz = paste(tempdir(), basename(nc_link), sep='/')
  try(download.file(nc_link, gz), silent=TRUE)
  if(file.exists(gz)){
    if(file.info(gz)$size < 1000){ # Check if file is smaller than 1000 byte: empty file
      remove_unavailable(gz)
    } else {
      gunzip(gz, overwrite=TRUE)
    }
  } else {
    remove_unavailable(gz)
  }
  gz = gsub('.gz','',gz)
  if(endsWith(gz,'.nc')){ # check it's netcdf before
    fixVarName(gz) 
  }
  return( gz )
}

## Fix GPCC bugged variable names
fixVarName = function(gz){
  bugfix = nc_open(gz, write=TRUE)
  if(any(grepl('var',names(bugfix[['var']])))){
    bugfix = ncvar_rename(bugfix, names(bugfix[['var']])[1], 'p')
    bugfix = ncvar_rename(bugfix, names(bugfix[['var']])[2], 's')
  }
  nc_close(bugfix)
}

## File remover when requested date is not available
remove_unavailable = function(zipfile){
  if(file.exists(zipfile)){ file.remove(zipfile) }
  uiMessage("Selected month not available from GPCC.\r\nPlease check available months at: https://opendata.dwd.de/climate_environment/GPCC/")
  stop('Selected month not available from GPCC.\r\nPlease check available months at: https://opendata.dwd.de/climate_environment/GPCC/')
}

## This function updates the target netcdf, returning a token (downloaded file)
# target: target netcdf to be updated
# what: monitoring (v6) or first guess?
# yr, mm: year YYYY, month MM
netcdf_update = function(target, what, yr, mm){
  # Retrieve monthly data from GPCC and check whether it exists in target netcdf
  mm = ifelse(mm %in% 1:9, paste0('0', mm), mm) # Make sure month string has the zero ahead
  target_nc = nc_open(target, write=TRUE) # open target netcdf
  tm = ncvar_get(target_nc, 'time') # Get time data
  dy = floor(tm / 12) # get years since origin
  dm = tm - 12 * dy + 1; dm = ifelse(dm %in% 1:9, paste0('0', dm), dm) # get months since origin
  org = as.Date(strsplit(target_nc$dim$time$units, ' ')[[1]][3]) # get origin date (15 January 1901)
  existingMonths = paste(as.integer(format(org, '%Y')) + dy, dm, sep='-') # get (formatted) months currently in target netcdf
  requestedMonth = paste(yr,mm,sep='-') # make a formatted string for requested month
  # Condition to assign time slice to add/edit
  if(requestedMonth %in% existingMonths){
    warning('Selected month is already present in the target netcdf file.')
    # Do not overwrite, default behaviour by automatic procedure
    conf = uiMessage("The month you requested is present in target netcdf already. \r\n\r\nWould you like to replace it?\r\n",
                     conf=FALSE)
    if(conf){
      timeSlice = which(requestedMonth == existingMonths)  # Get index of month to overwrite
      monthSince = tm[timeSlice]
      postpone_message = FALSE
    } else {
      nc_close(target_nc)
      return(FALSE) # Do nothing; this is required to keep the application running out of the function
    }
  } else {
    monthSince = (yr - as.integer(format(org, '%Y'))) * 12 + as.integer(mm) - 1
    timeSlice = monthSince - min(tm, na.rm = TRUE) + 1
    # Check if any months before the selected period were not updated. 
    postpone_message = ifelse(timeSlice - length(tm) > 1, TRUE, FALSE)
  }
  ## Download, check existence, unzip and extract data for selected month
  library(R.utils)
  if(what=="First guess"){ # first guess netcdf update ----
    # Data before 2013 and after 2018 is in ascii format only, afterwards in netcdf too (the latter is preferred when available)
    if(yr < 2013 | yr > 2018){
      src_nc = paste0('https://opendata.dwd.de/climate_environment/GPCC/first_guess/', yr, '/gpcc_first_guess_', mm, '_', yr, '.gz')
      gz = download_and_check(src_nc)
      newData = read.table(gz, skip = 8, sep = "") # Read ascii removing header (first 8 rows)
      newData[newData < 0] = NA # Substitute negative values (i.e. missing values) with NA (target_nc$var$p$missval in netcdf)
      prcp = newData[ ,1] # Vector of precipitation values
      prcpNew = matrix(prcp, nc=180) # make a matrix /grid of precip. values
      prcpNew = prcpNew[ ,ncol(prcpNew):1] # flip matrix to match netcdf format
      Ngauges = newData[ ,2] # Vector of gauges info
      NgaugesNew = matrix(Ngauges, nc=180) # make matrix
      NgaugesNew = NgaugesNew[ ,ncol(NgaugesNew):1] # flip
    } else {
      src_nc = paste0('https://opendata.dwd.de/climate_environment/GPCC/first_guess/', yr, '/first_guess_monthly_', yr, '_', mm, '.nc.gz')
      gz = download_and_check(src_nc)
      newData = nc_open(gz) # Open downloaded and unzipped netcdf
      prcpNew = ncvar_get(newData, 'p') # Extract precipitation values as matrix
      prcpNew = prcpNew[ ,ncol(prcpNew):1]  # flip matrix to match netcdf format
      NgaugesNew = ncvar_get(newData, 's') # extract gauges data
      NgaugesNew = NgaugesNew[ ,ncol(NgaugesNew):1] # flip
      nc_close(newData) # Close netcdf
    }
    ## Add or update selected slice to existing target netcdf
    summaryList = lapply(list(prcpNew, NgaugesNew), function(x) {
      summary(as.vector(x))
    })
    names(summaryList) = names(target_nc$var) # Give variables names
    # Write values to target netcdf file
    ncvar_put(target_nc, varid='prcp', vals=prcpNew, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
    ncvar_put(target_nc, varid='numStations', vals=NgaugesNew, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
  }
  
  if(what == "Monitoring (v6)"){ # monitoring netcdf update ----
    src_nc = paste0('https://opendata.dwd.de/climate_environment/GPCC/monitoring_v6/', yr, '/monitoring_v6_10_', yr, '_', mm, '.nc.gz')
    gz = download_and_check(src_nc)
    newData = nc_open(gz) # Open downloaded and unzipped netcdf
    # Read in the selected period and iterate to copy all variables into archive netcdf
    prcpNew = ncvar_get(newData, 'p') # Extract precipitation values as matrix
    prcpNew = prcpNew[ ,ncol(prcpNew):1]  # flip matrix to match netcdf format
    NgaugesNew = ncvar_get(newData, 's') # Extract gauges info as matrix
    NgaugesNew = NgaugesNew[ ,ncol(NgaugesNew):1] # flip
    solid_p = ncvar_get(newData, 'solid_p')
    solid_p = solid_p[ ,ncol(solid_p):1]
    liquid_p = ncvar_get(newData, 'liquid_p')
    liquid_p = liquid_p[ ,ncol(liquid_p):1]
    abs_gauge_err = ncvar_get(newData, 'abs_gauge_err')
    abs_gauge_err = abs_gauge_err[ ,ncol(abs_gauge_err):1]
    rel_gauge_err = ncvar_get(newData, 'rel_gauge_err')
    rel_gauge_err = rel_gauge_err[ ,ncol(rel_gauge_err):1]
    corr_fac = ncvar_get(newData, 'corr_fac')
    corr_fac = corr_fac[ ,ncol(corr_fac):1]
    nc_close(newData) # close netcdf
    ## Add or update selected slice to existing target netcdf
    summaryList = lapply(list(prcpNew, NgaugesNew, solid_p, liquid_p, rel_gauge_err, abs_gauge_err, corr_fac), 
                         function(x) {summary(as.vector(x))
                         })
    names(summaryList) = names(target_nc$var) # Give variables names
    # Write values to target netcdf file
    ncvar_put(target_nc, varid='prcp', vals=prcpNew, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
    ncvar_put(target_nc, varid='numStations', vals=NgaugesNew, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
    ncvar_put(target_nc, varid='proportionSolidPrecip', vals=solid_p, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
    ncvar_put(target_nc, varid='proportionLiquidPrecip', vals=liquid_p, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
    ncvar_put(target_nc, varid='absoluteError', vals=abs_gauge_err, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
    ncvar_put(target_nc, varid='relativeError', vals=rel_gauge_err, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
    ncvar_put(target_nc, varid='errorCorrection', vals=corr_fac, start=c(1,1,1,timeSlice), count=c(360,180,1,1))
  }
  
  ## Condition to check whether there are any missing months in between requested date, then updates time dimension
  if(postpone_message){
    ncvar_put(target_nc, varid='time', vals=c(rep(NA, monthSince - max(tm, na.rm=TRUE) -1), monthSince), 
              length(existingMonths)+1, timeSlice-length(existingMonths)) # Add month slice to time dimension
    firstMissing = as.Date(paste0(existingMonths[length(existingMonths)], '-28')) + 5 # Ensures the first missing date is taken from the last available
    warning('The selected month (', paste(yr,mm,sep='-'),') was updated, but the following months are missing: ', 
            paste(format(seq(firstMissing, by = "month", length.out = timeSlice - length(tm) - 1), '%Y-%m'), collapse=', '))
    uiMessage(paste('The selected month (', paste(yr,mm,sep='-'),') was updated, but the following months are missing: ', 
                    paste(format(seq(firstMissing, by = "month", length.out = timeSlice - length(tm) - 1), '%Y-%m'), collapse=', ')))
  } else {
    ncvar_put(target_nc, varid='time', vals=monthSince, timeSlice, 1) # Add month slice to time dimension
  }
  nc_close(target_nc)
  
  # print(summaryList)
  requestedMonth = format(as.Date(paste(requestedMonth,"-15",sep="")), '%B %Y') # Convert month string into plain text
  cat('Netcdf successfully updated in time slot number', timeSlice, ', corresponding to ', requestedMonth, '\r\n\r\n')
  return(gz) # Pass downloaded file name to application
}

## Function to write data into the database, via SQL queries
# vars: vector of variables to write/update to the database
# ID: grid ID as in the database
# yr, mm: year YYYY, month MM
# ch: database connection
# newData: dataframe of values to write to the database 
submit_query = function(vars, ID, yr, mm, ch, newData){
  # Add each variable to the oracle database 
  cat('Looping trough values.  \r\nThis may take a few minutes and freeze until finished.  \r\n')
  sqlstring = paste("SELECT G1D_ID FROM GRID_1DD_GPCC WHERE year = ", yr)
  ddarray = sqlQuery(ch, sqlstring)[ ,1]
  lgt = length(ID)
  for(x in 1:lgt){
    if(is.element(ID[x], ddarray)){
      valString = paste(paste(vars, mm, sep='_'), newData[x, ], sep='=', collapse=', ')
      updateString = paste('update GRID_1DD_GPCC set',  valString, ' where G1D_ID=', ID[x], ' and YEAR=', yr)
      sqlQuery(ch, updateString)
    } else {
      valString = paste(ID[x], yr, paste(newData[x, ], collapse=','), sep=',')
      insertString = paste('insert into GRID_1DD_GPCC (G1D_ID, YEAR,',
                           paste(vars, mm, collapse=',', sep='_'), ') values (', valString,')')
      sqlQuery(ch, insertString)
    }
    if(x %% 4000 == 0){
      cat(x,'records out of', lgt, 'processed.  \r\n')
    }
  }
}

## Function to update the database, manages the input data to write
# db: database name
# what: monitoring (v6) or first guess?
# yr, mm: year YYYY, month MM
# dwn: logical, token to download or not the data (depends on which functions were executed already)
db_update = function(db, what, yr, mm, username, password, dwn){
  if(!exists('oracleDriver')){ # Set Oracle driver depending on OS
    oracleDriver = ifelse(Sys.info()['sysname'] == 'Windows', '{Oracle in OraClient12Home1}', 
                          '/usr/lib64/oracle/12.1/client64/lib/libsqora.so.12.1')
  }
  cat('trying to connect to database...  \r\n')
  ch = tryCatch( # try to open database connection, if fails...
    odbcConnect(db, uid=username, pwd=password), error=function(e){stop(e)}, warning=function(w){})
  if(is.null(ch)){
    cat('still trying to connect, adding default driver specification: "{Oracle in OraClient12Home1}" ...  \r\n')
    ch = tryCatch( # ...try again with driver specification...
      odbcDriverConnect(paste0("Driver=",oracleDriver,";Dbq=",db,";Uid=",username,";Pwd=",password)),
      error=function(e){stop(e)}, warning=function(w){})
  }
  if(is.null(ch)){
    pth = '/dea.ies.jrc.it'
    cat('trying once more, adding driver specification and default service name: "',username,pth,'"...  \r\n', sep='')
    ch = tryCatch( # ...try once more adding the service name
      odbcDriverConnect(paste0("Driver=",oracleDriver,";Dbq=",db,pth,";Uid=",username,";Pwd=",password)),
      error=function(e){stop(e)}, warning=function(w){cat('Check Oracle driver, attempted with: ', oracleDriver); stop(w)})
  }
  cat('Successfully connected to database.  \r\n')
  
  ## Retrieve monthly data from GPCC and check whether it exists in target netcdf
  mm = ifelse(mm %in% 1:9, paste0('0', mm), mm) # Make sure month string has the zero ahead
  gm = ifelse(what=="First guess", "GUESS_RAIN_", "MON_RAIN_")
  tm = sqlQuery(ch, paste0("select SUM(", gm, mm, ") FROM GRID_1DD_GPCC where YEAR = ", yr))
  if(!is.na(tm)){
    warning("The requested month is present in target database already.")
    # Do not overwrite, default behaviour by automatic procedure
    conf = uiMessage(conf=FALSE,'The month you requested is present in target database already. \r\n\r\nWould you like to replace it?\r\n')
    if(!conf){
      close(ch) # close database connection
      return(FALSE) # required to keep the application running out of the function
    }
  }
  
  if(what=="First guess"){ # first guess db update ---- 
    library(R.utils)
    ## Download, unzip and extract data for selected month
    # Data before 2013 is in ascii format only, afterwards in netcdf too (the latter is preferred when available)
    strFile = ifelse(yr < 2013 | yr > 2018, 
                     paste0('/gpcc_first_guess_', mm, '_', yr, '.gz'), 
                     paste0('/first_guess_monthly_', yr, '_', mm, '.nc.gz'))
    src = paste0('https://opendata.dwd.de/climate_environment/GPCC/first_guess/', yr, strFile)
    if(dwn == FALSE){ # if data were not downloaded already, do it now
      dwn = download_and_check(src)
    }
    if(yr < 2013 | yr > 2018){
      newData = read.table(dwn, skip = 8, sep = "") # Read ascii removing header (first 8 rows)
      prcp = newData[ ,1]
      prcpNew = matrix(prcp, nc=180)
      prcpNew = prcpNew[ ,ncol(prcpNew):1] # flip matrix to match netcdf format
      Ngauges = newData[ ,2]
      NgaugesNew = matrix(Ngauges, nc=180)
      NgaugesNew = NgaugesNew[ ,ncol(NgaugesNew):1]
    } else {
      newData = nc_open(dwn) # open netcdf
      prcpNew = ncvar_get(newData, 'p') # extract precipitation data
      prcpNew = prcpNew[ ,ncol(prcpNew):1]  # flip matrix to match netcdf format
      NgaugesNew = ncvar_get(newData, 's') # extract gauges data
      NgaugesNew = NgaugesNew[ ,ncol(NgaugesNew):1] # flip
      nc_close(newData) # close netcdf
    }
    
    # Write/update data in tabular database using the identifier
    newData = cbind(as.vector(t(prcpNew)), as.vector(t(NgaugesNew)))
    keepThese = which(newData[ ,1] != -99999.99 & !is.na(newData[ ,1])) # Identify rows with valid rainfall data (including zero)
    #good_vs_NA = data.frame(Valid_Records=length(keepThese), Null_records=nrow(newData)-length(keepThese))
    newData = newData[keepThese, ] # Remove rows with NULL values in rainfall data
    ID = (1:(360*180))[keepThese] # Identifier to interact with Oracle database (it's sorted accordingly)
    vars = c('GUESS_RAIN','GUESS_GAUGES')
  }
  
  if(what == "Monitoring (v6)"){ # monitoring db update ----
    ## Download, check existence, unzip and extract data for selected month
    src_nc = paste0('https://opendata.dwd.de/climate_environment/GPCC/monitoring_v6/', yr, '/monitoring_v6_10_', yr, '_', mm, '.nc.gz')
    if(dwn == FALSE){ # if data were not downloaded already, do it now
      dwn = download_and_check(src_nc)
    }
    # Read in the selected period and iterate to copy all variables into archive db
    newData = nc_open(dwn) # Open downloaded and unzipped netcdf
    prcpNew = ncvar_get(newData, 'p') # Extract precipitation values as matrix
    prcpNew = prcpNew[ ,ncol(prcpNew):1]  # flip matrix to match netcdf format
    NgaugesNew = ncvar_get(newData, 's') # Extract gauges info as matrix
    NgaugesNew = NgaugesNew[ ,ncol(NgaugesNew):1] # flip
    solid_p = ncvar_get(newData, 'solid_p')
    solid_p = solid_p[ ,ncol(solid_p):1]
    liquid_p = ncvar_get(newData, 'liquid_p')
    liquid_p = liquid_p[ ,ncol(liquid_p):1]
    abs_gauge_err = ncvar_get(newData, 'abs_gauge_err')
    abs_gauge_err = abs_gauge_err[ ,ncol(abs_gauge_err):1]
    rel_gauge_err = ncvar_get(newData, 'rel_gauge_err')
    rel_gauge_err = rel_gauge_err[ ,ncol(rel_gauge_err):1]
    corr_fac = ncvar_get(newData, 'corr_fac')
    corr_fac = corr_fac[ ,ncol(corr_fac):1]
    nc_close(newData) # close netcdf
    
    # Write/update data in tabular database using the identifier
    newData = cbind(as.vector(t(prcpNew)), as.vector(t(NgaugesNew)), as.vector(t(solid_p)),
                    as.vector(t(liquid_p)), as.vector(t(abs_gauge_err)),
                    as.vector(t(rel_gauge_err)), as.vector(t(corr_fac)))
    keepThese = which(newData[ ,1] != -99999.99 & !is.na(newData[ ,1])) # Identify rows with valid rainfall data (including zero)
    #good_vs_NA = data.frame(Valid_Records=length(keepThese), Null_records=nrow(newData)-length(keepThese))
    newData = newData[keepThese, ] # Remove rows with NULL values in rainfall data
    ID = (1:(360*180))[keepThese] # Identifier to interact with Oracle database (it's sorted accordingly)
    vars = c('MON_RAIN','MON_GAUGES','MON_SOLID','MON_LIQUID','MON_GAUGE_ERROR','MON_GAUGE_PERC','MON_GAUGE_CORR')
  }
  submit_query(vars, ID, yr, mm, ch, newData) # write data to database
  close(ch) # close database connection
  cat('Database successfully updated.\r\n')
  return(dwn) # return token to signal whether to download or not, later in the script
}

## Function called by terminal commands, dispatches the arguments and execute the command chain
# yr, mm: year YYYY, month MM
# what: monitoring (v6) or first guess? Set as 'fg' or 'monit'
# chk: what data to update, netcdf or database, set as 'nc' or 'db'
# tnc: target netcdf path
# tdb, udb, pdb: target database name, user, password
GPCC_cmd_update = function(mm, yr, what, chk=NULL, tnc=NULL,tdb=NULL,udb=NULL,pdb=NULL){
  if(length(chk) == 0){
    stop("No item to update was specified (netcdf or database)")
  }
  if(all(chk %in% c('nc', 'db'))){
    if('nc' %in% chk){ # When netcdf is selected
      if(nchar(tnc) == 0){
        stop("The target netcdf to update is missing.")
      }
      library(ncdf4)
      # 'download_gpcc' variable avoids downloading the same data twice if database is selected too
      cat(as.character(Sys.time()),'\r\n')
      download_gpcc = netcdf_update(target=tnc, what, yr, mm)
    }
    if('db' %in% chk){ # When database is selected
      if(nchar(tdb) == 0 | nchar(pdb) == 0 | nchar(udb) == 0){
        try(file.remove(download_gpcc), silent = TRUE)
        stop("Database input, user and/or password are missing.")
      }
      library(RODBC)
      download_gpcc = ifelse(exists('download_gpcc'), download_gpcc, FALSE)
      download_gpcc = db_update(db=tdb, what, yr, mm,
                                username=udb, password=pdb, dwn=download_gpcc)
      cat(as.character(Sys.time()),'\r\n');
    }
  } else {
    stop("Wrong fourth argument, must be either 'db' or 'nc'.")
  }
  if(download_gpcc != FALSE){
    file.remove(download_gpcc) # unnecessary, as in temp
  }
  cat('---- Update of ', what, ' precipitation data completed.  \r\n')
}


save(.First,GPCC_updater,db_update,download_and_check,install_missing,uiMessage,
     fixVarName,netcdf_update,remove_unavailable,submit_query,GPCC_cmd_update, 
     file = "GPCC_application.RData") # Save as executable RData

###### END 