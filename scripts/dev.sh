#!/usr/bin/env bash
cd "$(dirname "$0")/.."

# Recharge automatique en tapant "make."
swipl <<EOF
[grandgousier].
EOF
