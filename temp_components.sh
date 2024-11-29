#!/usr/bin/env sh

# Temporarily comment lines in the user's list of Isabelle components to remove
# other instances of the ProcessComposition session while exporting code.

# Line pattern is anything ending in the session's name
# (In particular, this matches the relevant development repository's name)
PATTERN=".*ProcessComposition$"

# File location is found by asking the Isabelle executable
FILE="$(isabelle getenv -b ISABELLE_HOME_USER)/etc/components"

# Comment out matching lines
sed -i "s/^$PATTERN/#&/" $FILE

# Perform the export
make all

# Uncomment matching lines
sed -i "s/^#\($PATTERN\)/\1/" $FILE
