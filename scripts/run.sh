#!/usr/bin/env bash

# Aller dans le répertoire du script pour être sûr qu'il trouve grandgousier.pl
cd "$(dirname "$0")/.."

# Lancer SWI-Prolog avec le fichier principal
swipl -q -s grandgousier.pl -g grandgousier -t halt
