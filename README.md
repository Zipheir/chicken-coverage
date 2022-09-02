# chicken-coverage

This is a very basic test-coverage tool for CHICKEN Scheme.
It extracts a list of exports from a CHICKEN module, then scans
a file for `test-group` forms with matching names.  When this
is complete, a summary is printed which tells you which forms
had test groups and which didn't.

This approach requires you to provide a test group for each
exported form.

Since `test-group` is provided by both SRFI 64 and the CHICKEN
`test` egg, this tool is compatible with both test libraries.

# Usage

    chicken-coverage <module-file> <test-file>

# Dependencies

The [slib-wt-tree](https://wiki.call-cc.org/eggref/5/slib-wt-tree)
egg is required.

# Building

Run `make`, then `make install` in the source directory.

# Caveats

This tool is currently very rough.  Many improvements are needed,
and the design may change at any moment.

`chicken-coverage` only tells you whether a form has a test group;
it tells you nothing about what that test group does.  It might
even be empty.

# Author

Wolfgang Corcoran-Mathe
