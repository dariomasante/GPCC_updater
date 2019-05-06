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

source('GPCC_update_functions.R')
save(.First,GPCC_updater,db_update,download_and_check,install_missing,uiMessage,
     fixVarName,netcdf_update,remove_unavailable,submit_query,GPCC_cmd_update, 
     file = "GPCC_application.RData") # Save as executable RData

###### END 