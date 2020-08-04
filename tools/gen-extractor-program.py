#!/usr/bin/python3

import sys

def main() :
    if len(sys.argv) < 3:
        print("Usage: gen-extractor-py SRC_PATH_1:SRC_PATH_2:...:SRC_PATH_N OUTPUT_PRED [FILE]")
        exit(1)

    src_paths = sys.argv[1]
    output_pred = sys.argv[2]
    if len(sys.argv) >= 4:
        output_file = open(sys.argv[3], "w")
    else:
        output_file = sys.stdout

    lang = "java8"

    mdl_prog = "# Program generated using gen-extractor-program.py\n"
    prog_pred = output_pred[0].upper() + output_pred[1:]

    for src_path in src_paths.split(":"):
        mdl_prog += 'IMPORT(\'{}, "{}", "{}").\n'.format(prog_pred, src_path, lang)

    # Provide a type for the program relation
    mdl_prog += '{}(0, 0, 0, 0, "") :- NEQ(0, 0).\n'.format(prog_pred)

    mdl_prog += "OUTPUT('{}).\n".format(prog_pred)

    print(mdl_prog, file = output_file)

if __name__ == '__main__':
    main()
