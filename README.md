# spock-electron
An Electron frontend with a Haskell-Spock back end with static resources compiled in.

There are 2 projects in this repo:
* frontend - A very simple Electron application.  The only logic in it is code to:
  * Spawn an instance of the backend when the frontend launches
  * Redirect the Electron page to the root of the backend server
  * Close the backend server when the frontend is shut down
* backend - A Haskell web server using the Spock library
  * Uses the Blaze library for Html composition
  * Uses the `wai-app-static` library to compile static resources (css, js) into the web server executable
  
The 2 projects are glued together using executable Shake build scripts, using the stack shebang.
