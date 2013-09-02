#!/bin/bash

echo '#!/bin/bash
python3 ./ladybugsim.py3 $1 $2 $3 $4' > ./SIM
chmod +x SIM
