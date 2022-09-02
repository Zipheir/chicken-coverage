# chicken-coverage

This is a very basic test-coverage tool for CHICKEN Scheme.
It extracts a list of exports from a CHICKEN module, then scans
a file for `test-group` forms with matching names.  When this
is complete, a summary is printed which tells you which forms
had test groups and which didn't.

This approach requires you to provide a test group for each
exported form.

# Usage

    chicken-coverage <module-file> <test-file>

# Building

Run `make`, then `make install` in the source directory.

# Author

Wolfgang Corcoran-Mathe
