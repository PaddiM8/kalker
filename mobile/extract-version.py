#!/usr/bin/python3

import json

with open("package.json", "r") as f:
    version = json.load(f)["version"]

with open("version.txt", "w") as f:
    f.write("version: {}\n".format(version))
    versionCode = sum([int(x) if len(x) > 1 else int(x) * 10
                       for x in version.split(".")])
    f.write("versionCode: {}\n".format(versionCode))
