#!/usr/bin/env bash
set -euo pipefail

mpv \
    --geometry=800x800 \
    --keep-open \
    --scale=nearest --dscale=nearest \
    "$1"
