#!/usr/bin/env bash
set -euo pipefail

ffmpeg -f image2pipe -vcodec ppm -i - \
    -vf "scale=iw*2:ih*2:flags=neighbor,fps=${2:-60}" \
    -loop 0 \
    "$1.gif"
