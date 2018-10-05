.First <- function(){
  ## Dario Masante - May 2017
  ## dario.masante@ext.ec.europa.eu (dario.masante@gmail.com)
  ## This set of functions starts a GUI to allow update of GPCC data.
  ## GPCC_updater
  ## Downloads GPCC data and updates netcdf files and database, via a user-friendly GUI (point and click), 
  ## powered by the gWidgets package for R and relying solely on R installation. 
  ## Provided that R is already installed and that .RData files are opened with it by default, 
  ## double clicking the RData file will start up the application. No code typing whatsoever is required.
  ## A function wrapping up the application is loaded and visible in the R global environment as well, 
  ## allowing the application to be started in an already open R session: GPCC_updater()
  
  # Version number:
  vers = 'v2.1'
  
  # Install packages if not installed yet, then load
  if(!require(ncdf4)) {utils::install.packages("ncdf4")}
  if(!require(RODBC)) {utils::install.packages("RODBC")}
  if(!require(gWidgets)) {utils::install.packages("gWidgets")}
  if(!require(gWidgetstcltk)) {utils::install.packages("gWidgetstcltk")}
  if(!require(R.utils)) {utils::install.packages("R.utils")}
  library(gWidgets)
  
  ## Declare functions 
  ## Function to download data for first guess
  # nc_link: url to the gpcc data (both netcdf or ascii)
  download_and_check = function(nc_link){
    gz = paste(tempdir(), basename(nc_link), sep='/')
    download.file(nc_link, gz)
    if(file.info(gz)$size < 1000){ # Check if file is smaller than 1000 byte: empty file
      remove_unavailable(gz)
    } else {
      gunzip(gz, overwrite=TRUE)
    }
    gz = gsub('.gz','',gz)
    fixVarName(gz)
    return( gz )
  }
  
  ## Fix GPCC bugged variable names
  fixVarName = function(gz){
    bugfix = nc_open(gz, write=TRUE)
    if(any(grepl('var',names(bugfix[['var']])))){
      bugfix = ncvar_rename(bugfix, names(bugfix[['var']])[1], 'p')
      bugfix = ncvar_rename(bugfix, names(bugfix[['var']])[2], 's')
      nc_close(bugfix)
    }
  }
  
  ## File remover when requested date is not available
  remove_unavailable = function(zipfile){
    file.remove(zipfile)
    gmessage("Selected month not available from GPCC (or just not for data version 6).\nPlease check available months at:  ftp://ftp.dwd.de/pub/data/gpcc/")
    stop('Selected month not available from GPCC (or just not for data version 6).\nPlease check available months at:  ftp://ftp.dwd.de/pub/data/gpcc/')
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
      conf = gconfirm("The month you requested is present in target netcdf already. \n\nWould you like to replace it?\n")
      if(conf){
        timeSlice = which(requestedMonth == existingMonths)  # Get index of month to overwrite
        monthSince = tm[timeSlice]
        postpone_message = FALSE
      } else {
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
      # Data before 2013 is in ascii format only, afterwards in netcdf too (the latter is preferred when available)
      if(yr < 2013){
        src_nc = paste0('ftp://ftp.dwd.de/pub/data/gpcc/first_guess/', yr, '/gpcc_first_guess_', mm, '_', yr, '.gz')
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
        src_nc = paste0('ftp://ftp.dwd.de/pub/data/gpcc/first_guess/', yr, '/first_guess_monthly_', yr, '_', mm, '.nc.gz')
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
      src_nc = paste0('ftp://ftp.dwd.de/pub/data/gpcc/monitoring_v6/', yr, '/monitoring_v6_10_', yr, '_', mm, '.nc.gz')
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
      ncvar_put(target_nc, varid='time', vals=c(rep(NA,monthSince - max(tm) -1),monthSince), 
                length(existingMonths)+1, timeSlice-length(existingMonths)) # Add month slice to time dimension
      firstMissing = as.Date(paste0(existingMonths[length(existingMonths)], '-28')) + 5 # Ensures the first missing date is taken from the last available
      warning('The selected month (', paste(yr,mm,sep='-'),') was updated, but the following months are missing: ', 
              paste(format(seq(firstMissing, by = "month", length.out = timeSlice - length(tm) - 1), '%Y-%m'), collapse=', '))
      gmessage(paste('The selected month (', paste(yr,mm,sep='-'),') was updated, but the following months are missing: ', 
               paste(format(seq(firstMissing, by = "month", length.out = timeSlice - length(tm) - 1), '%Y-%m'), collapse=', ')))
    } else {
      ncvar_put(target_nc, varid='time', vals=monthSince, timeSlice, 1) # Add month slice to time dimension
    }
    nc_close(target_nc)
    
    print(summaryList)
    requestedMonth = format(as.Date(paste(requestedMonth,"-15",sep="")), '%B %Y') # Convert month string into plain text
    cat('Netcdf successfully updated in time slot number', timeSlice, ', corresponding to ', requestedMonth, '\n\n')
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
    cat('Looping trough values.  \nThis may take a few minutes and freeze the GUI until finished.  \n')
    lgt = length(ID)
    for(x in 1:lgt){
      sqlstring = paste("select count(*) total from GRID_1DD_GPCC where YEAR =", yr, "and G1D_ID = ", ID[x])
      ddarray <- sqlQuery(ch, sqlstring)
      if(ddarray == 0){
        valString = paste(ID[x], yr, paste(newData[x, ], collapse=','), sep=',')
        insertString = paste('insert into GRID_1DD_GPCC (G1D_ID, YEAR,',
                             paste(vars, mm, collapse=',', sep='_'), ') values (', valString,')')
        #insertString = sprintf('insert into GRID_1DD_GPCC (G1D_ID, YEAR, %s) values ( %s)', 
        #                       paste(vars, mm, collapse=',', sep='_'), valString)
        sqlQuery(ch, insertString)
      } else {
        valString = paste(paste(vars, mm, sep='_'), newData[x, ], sep='=', collapse=', ')
        updateString = paste('update GRID_1DD_GPCC set',  valString, ' where G1D_ID=', ID[x], ' and YEAR=', yr)
        sqlQuery(ch, updateString)
      }
      if(x %% 2000 == 0){
        cat(x,'records out of', lgt, 'processed.  \n')
      }
    }
  }
  
  ## Function to update the database, manages the input data to write
  # db: database name
  # what: monitoring (v6) or first guess?
  # yr, mm: year YYYY, month MM
  # dwn: logical, token to download or not the data (depends on which functions were executed already)
  db_update = function(db, what, yr, mm, username, password, dwn){
    cat('trying to connect...  \n')
    ch = tryCatch( # try to open database connection, if fails...
      odbcConnect(db, uid=username, pwd=password), error=function(e){stop(e)}, warning=function(w){})
    if(is.null(ch)){
      cat('still trying to connect, adding default driver specification: "{Oracle in OraClient12Home1}" ...  \n')
      ch = tryCatch( # ...try again with driver specification...
        odbcDriverConnect(paste0("Driver={Oracle in OraClient12Home1};Dbq=",db,";Uid=",username,";Pwd=",password)),
          error=function(e){stop(e)}, warning=function(w){})
    }
    if(is.null(ch)){
      cat('trying once more, adding driver specification and default service name: "',username,'/dea.ies.jrc.it"...  \n', sep='')
      ch = tryCatch( # ...try once more adding the service name
        odbcDriverConnect(paste0("Driver={Oracle in OraClient12Home1};Dbq=",db,"/dea.ies.jrc.it;Uid=",username,";Pwd=",password)),
          error=function(e){stop(e)}, warning=function(w){stop(w)})
    }
    cat('Successfully connected to database.  \n')
    
    ## Retrieve monthly data from GPCC and check whether it exists in target netcdf
    mm = ifelse(mm %in% 1:9, paste0('0', mm), mm) # Make sure month string has the zero ahead
    gm = ifelse(what=="First guess", "GUESS_RAIN_", "MON_RAIN_")
    tm = sqlQuery(ch, paste0("select SUM(", gm, mm, ") FROM GRID_1DD_GPCC where YEAR = ", yr))
    if(!is.na(tm)){
      conf = gconfirm("The month you requested is present in target database already. \n\nWould you like to replace it?\n")
      warning("The requested month is present in target database already.")
      if(!conf){
        close(ch) # close database connection
        return(FALSE) # required to keep the application running out of the function
      }
    }
    
    if(what=="First guess"){ # first guess db update ----
      library(R.utils)
      ## Download, unzip and extract data for selected month
      # Data before 2013 is in ascii format only, afterwards in netcdf too (the latter is preferred when available)
      strFile = ifelse(yr < 2013, 
                       paste0('/gpcc_first_guess_', mm, '_', yr, '.gz'), 
                       paste0('/first_guess_monthly_', yr, '_', mm, '.nc.gz'))
      src = paste0('ftp://ftp.dwd.de/pub/data/gpcc/first_guess/', yr, strFile)
      if(dwn == FALSE){ # if data were not downloaded already, do it now
        dwn = download_and_check(src)
      }
      if(yr < 2013){
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
      src_nc = paste0('ftp://ftp.dwd.de/pub/data/gpcc/monitoring_v6/', yr, '/monitoring_v6_10_', yr, '_', mm, '.nc.gz')
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
    cat('\nDatabase successfully updated.\n')
    return(dwn) # return token to signal whether to download or not, later in the script
  }


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
      chk = svalue(chk_sel)
      tnc = svalue(target_netcdf)
      if(length(chk) == 0){
        gmessage("Please select at least one item to update (netcdf or database)")
        stop("No item to update was specified (netcdf or database)")
      }
      if(ticklist[1] %in% chk){ # When netcdf is selected
        if(nchar(tnc) == 0){
          gmessage("Please add the target netcdf to update.")
          stop("The target netcdf to update is missing.")
        }
        library(ncdf4)
        cat('Updating netcdf file...  \n')
        # 'download_gpcc' variable avoids downloading the same data twice if database is selected too
        download_gpcc = netcdf_update(target=tnc, what, yr, mm)
      }
      if(ticklist[2] %in% chk){ # When database is selected
        tdb = svalue(target_db)
        udb = svalue(user_db)
        pdb = svalue(pass_db)
        if(nchar(tdb) == 0 | nchar(pdb) == 0 | nchar(udb) == 0){
          tryCatch(file.remove(download_gpcc), error=function(e){})
          gmessage("Please specify database to update, user and password.")
          stop("Database input, user and/or password are missing.")
        }
        cat('Updating database:  \n')
        library(RODBC)
        download_gpcc = ifelse(exists('download_gpcc'), download_gpcc, FALSE)
        download_gpcc = db_update(db=tdb, what, yr, mm, 
                                  username=udb, password=pdb, dwn=download_gpcc)
      }
      if(download_gpcc != FALSE){
        if(length(svalue(interm_sel)) <= 0){
          keepNC = paste(dirname(tnc), basename(download_gpcc),sep='/')
          file.copy(download_gpcc, keepNC)
          cat('Downloaded netcdf was stored at: ', keepNC, '\n')
        }
        file.remove(download_gpcc) # unnecessary, as in temp
      }
      rm(download_gpcc) # unnecessary
      cat('\nExecution completed.  \n')
    }
    )
    
    glabel(" ", container = win)
    
    btnExit = gbutton("Cancel", handler = function(h,...) dispose(win), container=win)
    
    visible(win) = TRUE
    focus(win)
  }
  
  GUI()
  
}

GPCC_updater = .First # to make the function visible in the global environment as 'GPCC_updater'

save(.First, GPCC_updater, file = "GPCC_application.RData") # Save as executable RData

###### END