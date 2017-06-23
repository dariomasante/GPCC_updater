### Dario Masante - 06/2017 - dario.masante@ext.ec.europa.eu (or @gmail.com )

##################
### The function returns a vector of netcdf values sorted by ID number in the database.
### It assumes that IDs in the database are sorted in some way, so that ID number 1 is
### at any given corner of the map (even if actually missing from the database) and the 
### last ID number is at the opposite corner.

# nc_mtx -> a matrix with the values from the variable of interest to submit to database
# ALREADY EXTRACTED from the netcdf file using function 'ncvar_get' (package ncdf4).
# Netcdf files from different providers usually have more or less different structure and 
# this can't be wrapped inside a function, that's why the user must extract the data beforehand.

# db_byrow -> is the index in database attributed by row? If false then it's sorted by column

# db_topbottom -> is the index in database attributed from top to bottom (north to south)? 
# If false then it's ordered from bottom to top

# db_leftright -> is the index in database attributed from left to right? If false then it's 
# ordered from right to left

# EXAMPLE: if the ID for each cell in the database starts in the lower left corner
# and progress by column from left to rightmost column, the function arguments will be:
# sort_nc2db(netcdf_variable, db_byrow=FALSE, db_topbottom=FALSE, db_leftright=TRUE)

sort_nc2db = function(nc_mtx, db_byrow=TRUE, db_topbottom=TRUE, db_leftright=TRUE){
  nrows = ncol(nc_mtx)
  m = matrix(1:length(nc_mtx), nr=nrows, byrow=db_byrow) # make a matrix of db indexes
  if(db_topbottom==FALSE){
    m = m[nrows:1, ]
  } 
  if(db_leftright==FALSE){
    m = m[ ,ncol(m):1]
  }
  m = t(m)[ ,nrow(m):1] # flip to match netcdf frame
  as.vector(nc_mtx)[m] # order by database id and return
}

###################
## This function builds an SQL string and sends it to the database, to update or insert records
## It should be used with care, as it's quite limited and not tested thoroughly.

# nc_values -> a vector of values from the variable of interest in the netcdf, sorted according to 
#   the record id as in argument 'db_id_values'. Function sort_nc2db() sorts this out
# db, username, password -> database name, user and password
# db_var -> the name of variable of interest in the database 
# db_id_field -> the name of the column in the database containing the cell id , e.g. 'ID'
# db_id_values -> the id values of cells, sorted increasingly.
# db_table -> the name of the table in the database to add the values to. E.g. 'GRID_1DD_GPCC'
# db_filter_field -> if any, the name of columns to apply filter to e.g. 'YEAR'
# db_filter_value -> the values of db_filter_field to filter upon the table (e.g. '2014')
submit_query = function(nc_values, db, username, password, db_var, db_id_field, db_id_values, db_table, db_filter_field=NULL, db_filter_value=NULL){
#  ch = odbcConnect(db, uid=username, pwd=password)
  # Add each variable to the oracle database 
  cat('Looping trough values...\n')
  lgt = length(db_id_values)
  if(!is.null(db_filter_field)){
    db_str = paste(paste(db_filter_field, db_filter_value, sep=" = "), "and", collapse=' ')
  } else {
    db_str = ''
  }
  for(x in 1:lgt){ # loop through cells with IDs
    sqlstring = paste("select count(*) total from", db_table, "where", 
                      db_str, db_id_field, "=", db_id_values[x])
    ddarray = sqlQuery(ch, sqlstring)
    if(ddarray == 0){
      if(is.null(db_filter_field)){
        nmString = paste(db_id_field, db_var, sep=',') 
        valString = paste(db_id_values[x], nc_values[x], sep=',')
      } else {
        nmString = paste(db_id_field, db_filter_field, db_var, sep=',')
        valString = paste(db_id_values[x], db_filter_value, nc_values[x], sep=',')
      }
      insertString = paste('insert into ', db_table, ' (', nmString, ') values (', valString,')')
      sqlQuery(ch, insertString)
    } else {
      valString = paste(db_var, nc_values[x], sep=' = ')
      updateString = paste('update', db_table, 'set',  valString, ' where', db_str, db_id_field, '=', db_id_values[x])
      sqlQuery(ch, updateString)
    }
    if(x %% 2000 == 0){
      cat(x,'records out of', lgt, 'processed.\n')
    }
  }
}

