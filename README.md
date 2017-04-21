# GPCC_updater
Downloads GPCC data and updates netcdf files and database, via a user-friendly GUI (point and click), powered by the gWidgets package for R and relying solely on R installation. Provided that R is already installed and that .RData files are opened with it by default, double clicking the RData file will start up the application. No code typing whatsoever is required.

A function wrapping up the application is loaded and visible in the R global environment as well, so the application can be started in an open R session by loading the .RData file and typing the command *GPCC_updater()*
