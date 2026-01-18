import subprocess
import sys
import os

def run_ingestion():
    # Get the directory of the current script (e2e_helper.py)
    script_dir = os.path.dirname(__file__)
    
    # Construct the path to ingest.py, assuming it's in the scripts directory
    ingest_script_path = os.path.join(script_dir, "..", "scripts", "ingest.py")
    
    # Define the sample_files directory relative to the project root
    sample_files_path = os.path.join(script_dir, "..", "sample_files")

    # Command to run ingest.py with sample_files as argument
    command = [sys.executable, ingest_script_path, sample_files_path]
    
    print(f"Running ingestion with command: {' '.join(command)}")
    try:
        result = subprocess.run(command, check=True, capture_output=True, text=True)
        print("Ingestion successful!")
        print("Stdout:", result.stdout)
        if result.stderr:
            print("Stderr:", result.stderr)
    except subprocess.CalledProcessError as e:
        print(f"Ingestion failed with error: {e}")
        print("Stdout:", e.stdout)
        print("Stderr:", e.stderr)
        sys.exit(1)

if __name__ == "__main__":
    run_ingestion()