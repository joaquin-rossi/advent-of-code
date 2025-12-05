#!/usr/bin/env bash
set -euo pipefail

ffmpeg -f image2pipe -vcodec ppm -i - \
    -vf "scale=iw*2:ih*2:flags=neighbor" \
    -c:v libx264 -pix_fmt yuv420p -preset veryfast -crf 18 \
    -r "${2:-60}" \
    "$1"
