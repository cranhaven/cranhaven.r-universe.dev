# ravedash 0.1.2

* Added presets to support importing `.nev` files with the most recent `raveio` updates
* Added shutdown function to allow users to shutdown instances from the interface, and added single-session mode to automatically shutdown the server once the session window is closed
* Fixed loader brain not updating issue when electrode table is empty
* Allowed run-analysis button to be placed on bottom-left with additional styles
* Added `error_notification` and `with_error_notification` to easily display error messages to the dashboard
* Allows modules to create session-based temporary files and options (experimental)
* Shorten response time needed to fire analysis event
* Moved `htmltools` to suggests
* Included free version of `fontawesome` into the build to avoid further changes
* Replaced `pickerInput` with native `selectInput` in condition builder
* Added `start_session` to start a new/existing session with one function
* Added stand-alone viewer support, allowing almost any outputs to be displayed in another session in full-screen, and synchronized with the main session
* Added `register_output` to register outputs, and `get_output_options` to obtain render details. This replaces the shiny output assign and facilitates the stand-alone viewer
* Added `output_gadget` and `output_gadget_container` to display built-in gadgets to outputs, allowing users to download outputs, and to display them in stand-alone viewers; needs little extra setups, must be used with `register_output`
* Added session-level (application) temporary directory and files (see `temp_dir` and `temp_file`), based on the application-level storage, created `session_setopt` and `session_getopt`

# ravedash 0.1.1

* Added a `NEWS.md` file to track changes to the package.
