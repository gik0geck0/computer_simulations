#!/bin/bash

# important note: This script will work with any version of python 3.x, but I have python 3.3, and getting that auto-detecting wouldn't be easy
echo '#!/bin/bash
python3.3 ./ladybugsim.py3 $1 $2 $3 $4' > ./SIM
chmod +x SIM
