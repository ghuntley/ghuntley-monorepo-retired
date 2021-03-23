#!/bin/bash
sudo ifconfig en0 ether 00-60-2F-$[RANDOM%10]$[RANDOM%10]-$[RANDOM%10]$[RANDOM%10]-$[RANDOM%10]$[RANDOM%10]
