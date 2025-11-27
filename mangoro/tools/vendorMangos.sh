#!/bin/bash
# vendorMangos.sh: Vendors go.nanomsg.org/mangos/v3 into src/go for local builds
# Usage: bash vendorMangos.sh

set -xe
cd "$(dirname "$0")/../inst/go"

# Initialize go module if not present
if [ ! -f go.mod ]; then
    go mod init mangoro.local || exit 1
fi

go get go.nanomsg.org/mangos/v3@2c434adf4860dd26da9fe96329237fe5aabc6acc

go mod tidy

go mod vendor

echo "Vendoring complete. Mangos and dependencies are now in ./vendor."

# Move problematic long-path flatbuf files to tools/ directory
echo "Moving flatbuf files to avoid long path warnings..."
cd ../..
rm -rf tools/flatbuf
mkdir -p tools/flatbuf
cp -r inst/go/vendor/github.com/apache/arrow/go/v18/arrow/internal/flatbuf/* tools/flatbuf/
rm -rf inst/go/vendor/github.com/apache/arrow/go/v18/arrow/internal/flatbuf

echo "Flatbuf files moved to tools/flatbuf/"
