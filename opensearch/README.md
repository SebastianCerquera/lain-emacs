# Setup instructions

python3 -m venv org
source org/bin/activate
pip3 install -r requirements.txt

cd tests && pytest
