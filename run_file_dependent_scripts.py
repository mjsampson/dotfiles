#!/usr/bin/env python3
# requires python3

from typing import Dict, Final
import hashlib
import os
import subprocess

# Expects a file of the format output by sha256sum (text mode)
CHECKSUM_FILE: Final = ".local_checksum"

# Global mapping between files/scripts
file_map = [
    {"script": "./run_once_check_brew.sh",
        "dependent_file": "dot_Brewfile"},
    {"script": "./run_once_terminal_settings.sh",
        "dependent_file": "dot_homebrew_theme.xml"},
]


def create_checksum_file() -> None:
    print("Creating checksum file")
    checksum_target_files = [d['dependent_file'] for d in file_map]
    sha_args = ["sha256sum"] + checksum_target_files
    checksum_file = open(CHECKSUM_FILE, mode="w")
    subprocess.run(sha_args, check=True, stdout=checksum_file)


def verify_checksums(existing_checksum_map: Dict[str, str]) -> bool:
    checksum_refresh = False

    for file in file_map:
        with open(file['dependent_file'], "rb") as dep_file:
            file_bytes = dep_file.read()
            hash = hashlib.sha256(file_bytes).hexdigest()
            if (file['dependent_file'] not in existing_checksum_map or
                    hash != existing_checksum_map[file['dependent_file']]):
                # Execute the script
                print(
                    "Hashes for " +
                    file['dependent_file'] +
                    " do not match. Executing " +
                    file['script'])
                subprocess.run(file['script'], check=True)
                checksum_refresh = True
            else:
                print("hashes for " + file['dependent_file'] +
                      " match, nothing to do.")
    return checksum_refresh


# main method
def main() -> None:
    result = subprocess.run(["chezmoi", "source-path"], check=True,
                            capture_output=True, encoding="utf-8")
    os.chdir(result.stdout.strip())
    existing_checksum_map = {}
    try:
        with open(CHECKSUM_FILE) as f:
            for line in f:
                tokenized = line.strip().split(maxsplit=1)
                existing_checksum_map[tokenized[1]] = tokenized[0]
    except FileNotFoundError:
        print("Checksum file not found. Will create at the end...")

    checksum_refresh = verify_checksums(existing_checksum_map)

    # Refresh checksums if needed
    if (checksum_refresh):
        create_checksum_file()


if __name__ == "__main__":
    main()
