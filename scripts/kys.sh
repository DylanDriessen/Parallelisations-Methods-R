#!/bin/bash

kill $(ps -fauxw | grep $(whoami) | grep slave | tail -n+2 | awk '{ print $2 }')
