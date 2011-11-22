#!/bin/sh
pnmtopng "$1" > "`dirname "$1"`/`basename "$1" .pnm`.png"
