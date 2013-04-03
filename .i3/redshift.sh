#!/bin/bash

pidof redshift > /dev/null 2>&1 && {
    killall redshift
    exit
}

redshift
