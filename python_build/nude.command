#!/usr/bin/env python
# Imports
import os
import subprocess
import re

# Get all the lines in the dependencies object
# Returns a list of lines
def get_file(path):
    save_line = False
    lst = []
    with open(path, "r") as file:
        for line in file:
            if save_line:
                lst.append(line)
            if '"dependencies": {' in line:
                save_line = True
            if "}" in line:
                save_line = False
    return lst


# Splits the lines to the name and version (With extra characters)
# Returns a dict with name and current version
def split_strings(lst):
    dct = {}
    for s in lst:
        if len(s) > 10:
            s = s.split('"')
            dct[s[1]] = s[3]
    return dct


# Cleans up the version to only include numbers and punctuation e.g (1.2.45)
# Returns a clean string
def clean_v(version):
    return re.sub(r"^~|\^|[\\n b]|'|\r", '', version).replace("\n", "")


# Uses command "npm show <package_name> version" to get the latest version
# Converts the version to a string.
# Returns string representing the version
def get_latest(package):
    retval = subprocess.check_output(["npm", "show", package, "version"])
    return clean_v(str(retval))


# Uses command "npm install" to update all of the modules.
def install_latest():
    subprocess.check_output(["npm", "install"])
    print("\033[92mLatest installed\033[0m")


# Checks if the latest version is the current version.
def is_outdated(current, latest):
    if current == latest:
        return False
    return True


# Reads the package.json file and changes the version numbers to the latest.
def update_outdated(lst, path):
    new_line = False
    upd_count = 0
    replacement = ''
    with open(path, "r") as file:
        for line in file:
            if '"dependencies": {' in line:
                new_line = True
            if "}" in line:
                new_line = False
            for l in lst:
                if l[0] in line and new_line is True:
                    line = line.replace(l[1], l[2])
                    upd_count += 1
            replacement = replacement + line
        fout = open(path, "w")
        fout.write(replacement)
        fout.close()
        print("Updated \033[92m" + str(upd_count) + "\033[0m packages")


# Start program
print("Your current directory is: \033[1m" + os.getcwd()) + "\033[0m"
p = input("Enter the path to the file (between \" \"): ")
lst = get_file(os.getcwd() + p)
dct = split_strings(lst)
outdated = []
for x in dct:
    n_ = x
    c_ = clean_v(dct[x])
    l_ = get_latest(x)
    if is_outdated(c_, l_):
        outdated.append([n_, c_, l_])
        print("\033[1m{0}\033[0m, Current: \033[92m{1}\033[0m, Latest: \033[94m{2}\033[0m".format(n_, c_, l_))
inp = input("Do you want to update all? (Y/N)")
if  inp == 'Y':
    print('OK')
    update_outdated(outdated, os.getcwd() + p)
    os.chdir(os.getcwd() + p.replace("package.json", ""))
    install_latest()
