#!/bin/bash

# ==============================================================================
# OSM Processing Script - AML Region
# Date: 2026-05-13
# Workflow:
#   1. Downloaded latest Portugal PBF from Geofabrik (portugal-latest.osm.pbf)
#   2. Renamed source to include download date (portugal-260512.osm.pbf)
#   3. This script clips the BBOX and filters for Transport, GTFS, and Bike Infra.
# ==============================================================================

# Variables for easy editing
INPUT_PBF="portugal-260512.osm.pbf"
CLIPPED_PBF="aml_20260513.osm.pbf"
FILTERED_PBF="aml_20260513_filtered.osm.pbf"
CONFIG="filter_config.txt"
BBOX="-9.541626,38.395492,-8.539124,39.179046"

echo "Step 1: Clipping BBOX from $INPUT_PBF..."
osmium extract -b $BBOX "$INPUT_PBF" -o "$CLIPPED_PBF" --overwrite

echo "Step 2: Filtering tags based on $CONFIG..."
osmium tags-filter "$CLIPPED_PBF" -e "$CONFIG" -o "$FILTERED_PBF" --overwrite

echo "Done! Final file: $FILTERED_PBF"
