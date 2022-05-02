import glob
import os
import sys
from _path import ROOT_DIRECTORY


def main():
    try:
        os.chdir(f"{ROOT_DIRECTORY}/venom/data")
        executables = []
        for file in glob.glob("*.vnm"):
            executables.append(file)
        print('Select the index of the file you want to execute: \n')
        for executable in range(len(executables)):
            print(f"""[{executable}] {executables[executable]}""")
        filename = int(input())
        lexer_path = ROOT_DIRECTORY
        executable_path = f"{ROOT_DIRECTORY}/venom/data/{executables[filename]}"
        os.system(
            f"""swipl -s {ROOT_DIRECTORY}/venom/src/venom.pl -g 'venom("{lexer_path}", "{executable_path}").'""")

    except KeyboardInterrupt:
        try:
            sys.exit(0)
        except SystemExit:
            os._exit(0)


if __name__ == '__main__':
    main()
