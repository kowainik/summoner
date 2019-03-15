#!/bin/bash

set -e

nix-build -j1 -Q && nix-store -qR result | cachix push layer-3-cachix
