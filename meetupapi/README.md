# meetupapi
## Using the Meetup API within R

This package allows flexible control of the meetup API from R.
Currently supports basic functionality.

If an API method is not supported please make a request on the GitHub or use the 
helper functions `meetupapi:::.construct_req()` and `meetupapi:::.meetup_api_GET()`.

Upcoming features:
* Handling date columns automatically
* Shiny interface for marking off attendance
* Additional API method functions


## Current functions
* `get_joined_meetups()`
* `get_meetup_events()`
* `get_meetup_members()`
* `get_event_rsvps()`
* `get_event_attendance()`
* `mark_event_attendance()`
