#!/usr/bin/sh

gleam deps download

# Copies include directories so the language server won't complain.
rsync -av --progress --mkpath build/packages/yamerl/include/ src/yamerl/include/ --exclude internal
# ...
