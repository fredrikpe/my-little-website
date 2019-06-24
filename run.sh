#!/usr/bin/env bash

source venv/bin/activate

nohup python -u ./server.py >> server.log &
