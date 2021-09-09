# Node Update Dependencies Efficiently (nude)
A simple tool for installing the latest version of multiple npm packages simultaneously.
## Usage
`nude <string>`

The string argument is a substring of all package names you want to update.
The program will find all dependecies in package.json that includes the provided substring
and install the latest version of each.

### Example
`nude capacitor` will install the latest version of each package which name contains "capacitor".
