# Export verified code as specified in the code export theory.
#
# For reproducibility, we assume that the current directory contains a specific
# version of the required session ProcessComposition as a submodule in the
# directory "isa-ProcessComposition".
# For this to work, this session must not already be a component of Isabelle.
# (The script `temp_components.sh` wraps `make all` with temporarily commenting
# out certain patterns of components, satisfying this assumption in some
# configurations.)
#
# Further assumption is that the target language folders are in the current
# directory so that exporting from Isabelle into "." writes into them.
#
# Note that no target is an actual file name (so all are phony) and none depend
# on any files.
# Ideally the export would have the relevant theory files as dependencies, only
# re-exporting if they changed.
# However, drift in keeping that list up to date is likely to introduce errors,
# and this export is not intended to be run often so the cost is acceptable.
# Furthermore, keeping the actual code export specification in a separate
# session means that Isabelle itself will try to not rebuild unchanged files
# and will mitigate this issue.

.PHONY: all
all: exp

# Exporting assumes the destinations are clean and ensures they exist
mkdirs = haskell/isabelle/src/ProcessComposition/Isabelle\
				 haskell/factorio/src/ProcessComposition/Isabelle\
				 haskell/marking/src/ProcessComposition/Isabelle
dirs = -d isa-ProcessComposition -d theory
opts = -o quick_and_dirty=true -o document=false
.PHONY: exp
exp: clean
	mkdir -p $(mkdirs)
	isabelle export $(dirs) $(opts) -x "*:code/**" -O . -p 2 ProcessComposition_Code
	mv haskell/isabelle/src/ProcessComposition/Isabelle/Factorio haskell/factorio/src/ProcessComposition/Isabelle
	mv haskell/isabelle/src/ProcessComposition/Isabelle/Marking haskell/marking/src/ProcessComposition/Isabelle

# Clean export destinations
.PHONY: clean
clean:
	rm -rf haskell/isabelle/src/* haskell/factorio/src/* haskell/marking/src/*
