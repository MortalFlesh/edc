#!/bin/bash

# see https://safe-stack.github.io/docs/template-appservice/

#==========#
# Defaults #
#==========#

LOCATION="westeurope"
PRICING_TIER="F1"
# F1 - Free Shared (60 CPU minutes / day) 1 GB Ram - Free
# D1 - Free Shared (240 CPU minutes / day) 1 GB 1.00 + Custom domains (without SSL) - 8 Eur / month
# B1 - Basic (A-series compute) 1.75 GB + Custom domains (+ SSL) + Up to 3 instances - 46.17 Eur / month

#=======#
# Azure #
#=======#

echo "Fill values first, then remove this line and the exit!"
exit

SUBSCRIPTION_ID="<fill>"
CLIENT_ID="<fill>"
TENANT_ID="<fill>"

PERSONAL_ID="<fill>" # for kv store, I dont know where to get it :D

MAIN_DOMAIN="www.mydomain.foo"
# Domain must have a `CNAME: $CONTEXT-$PURPOSE-ingress.azurefd.net`

#=========#
# Command #
#=========#

./fake.sh build --target appservice \
    -e subscriptionId="$SUBSCRIPTION_ID" \
    -e clientId="$CLIENT_ID" \
    -e tenantId="$TENANT_ID" \
    -e location="$LOCATION"       \
    -e pricingTier="$PRICING_TIER" \
    -e personalId="$PERSONAL_ID" \
    -e mainDomainHost="$MAIN_DOMAIN" \
    -e purpose="prod" \
    ;

#==========#
# Optional #
#==========#

# environment is an optional environment name that will be appended to all Azure resources created, which allows you to create entire dev / test environments quickly and easily. This defaults to a random GUID.
# -------------------------------
# -e environment="$ENVIRONMENT" \

# pricingTier is the pricing tier of the app service that hosts your SAFE app. This defaults to F1 (free); the full list can be viewed https://azure.microsoft.com/en-us/pricing/details/app-service/windows/.
# -------------------------------
