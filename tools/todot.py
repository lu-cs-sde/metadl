#!/usr/bin/env python3
import sys
import csv
import itertools

def help() :
    print("USAGE: todot NODE.csv EDGE.csv")

def main():
    if len(sys.argv) != 3:
        print("Invalid number of arguments. Expecting 3.")
        help()
        exit(1)

    node_file_name = sys.argv[1]
    edge_file_name = sys.argv[2]

    print ("digraph G {")
    print ("node [shape=record];")

    with open(node_file_name) as node_file:
        for row in csv.reader(node_file, delimiter=","):
            node_id = row[0];

            r = "n" + node_id + " ["

            first_prop = True
            for i in range (1, len(row), 2):
                prop = row[i]
                val = row[i + 1]
                if prop:
                    if not first_prop:
                        r += ","
                    first_prop = False
                    r += "{}={}".format(prop, val)
            r += "] ;"

            print(r)


    with open(edge_file_name) as edge_file:
        for row in csv.reader(edge_file, delimiter=","):
            edge_from = row[0]
            edge_to = row[1]
            edge_kind = "->" # oriented

            r = "n{} {} n{}".format(edge_from, edge_kind, edge_to)
            r += " ["

            first_prop = True
            for i in range(2, len(row), 2):
                prop = row[i]
                val = row[i + 1]
                if prop:
                    if not first_prop:
                        r += ","
                    first_prop = False
                    r += "{}={}".format(prop, val)

            r += "] ;";
            print (r)

    print("}")


if __name__ == "__main__":
    main()
