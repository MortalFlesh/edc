#!/bin/bash

function logSelenium() {
    # colors
    RED='\033[0;31m'
    YELLOW='\033[0;33m'
    PURPLE='\033[0;34m'
    BLUE='\033[0;36m'
    NC='\033[0m' # No Color

    echo -e "${PURPLE}$(date +"[%F %T]")${NC}: [${YELLOW}Steward${NC}] ${BLUE}$1${NC}"
}

function line() {
    echo " "
}
