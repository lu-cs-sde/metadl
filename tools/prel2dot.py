#!/usr/bin/python3

import csv
import sys
import itertools
from collections import defaultdict

class Node:
    def __init__(self, kind,  nid):
        self.kind = kind
        self.nid = nid
        self.children = dict()
        self.attr = dict()
        self.label = ""

    def addChild(self, i, c):
        self.children[i] = c

    def addAttr(self, a, n):
        self.attr[a] = n;

    def setLabel(self, label):
        self.label = label

    def toDotNode(self):
        s = "n" + str(self.nid) + " [label=\""
        s += "{<kind>" + self.kind + "[" + str(self.nid) + "] " + self.label + " | {"
        sorted_children = sorted(self.children.items(), key = lambda e : e[0])

        for idx, val in enumerate(sorted_children):
            if idx != 0:
                s += " | "
            s += "<c" + str(val[0]) + "> " + str(val[0])
        s += "}}\"]"

        return s

    def toDotEdges(self):
        s = ""
        first = True
        for idx, c in self.children.items():
            if not first:
                s += "; "
            s += "n" + str(self.nid) + ":c" + str(idx) + " -> n" + str(c) + ""
            first = False

        for attr, n in self.attr.items():
            if not first:
                s += "; ";
            s += "n" + str(self.nid) + " -> n" + str(n) + ' [constraint=false, color="{}", label="{}"]' \
                                                                    .format("red" if "type" in attr else "blue" if "decl" in attr else "green", attr)
            first = False
        return s

class Terminal:
    def __init__(self, nid, val):
        self.nid = nid
        self.val = val

    def toDotNode(self):
        s = "n" + str(self.nid) + " [label=\""
        s += "{<kind>Terminal" + str(self.nid) + " | " + self.val + "}\"]"
        return s

    def toDotEdges(self):
        return ""


def main():
    if len(sys.argv) != 4:
        print("Run as: prel2dot.py PR.csv ATTR.csv SRC.csv")
        exit(1)

    csv_file_name = sys.argv[1]
    attr_file_name = sys.argv[2]
    src_file_name = sys.argv[3]

    pr_csv_reader = csv.reader(open(csv_file_name), delimiter=',')
    src_csv_reader = csv.reader(open(src_file_name), delimiter=',')

    nodes = dict()
    attrs = []
    srcLocs = dict()

    node_to_file = dict()
    for row in src_csv_reader:
        node_to_file[row[0]] = row[5]


    # clusters= defaultdict(list)

    for row in pr_csv_reader:
        if row[0] in ["Token.ParseName", "ID"]:
            n = Terminal(row[1], row[4])
            nodes[row[1]] = n
        else:
            if row[1] in nodes:
                n = nodes[row[1]]
            else:
                n = Node(row[0], int(row[1]))
                nodes[row[1]] = n
            if int(row[2]) >= 0:
                n.addChild(int(row[2]), int(row[3]))



    # attrs_reader = csv.reader(open(attr_file_name), delimiter=',')
    # for attr, src, tgt in attrs_reader:
    #     if src in nodes:
    #         nodes[src].addAttr(attr, tgt)

    # for n, loc in srcLocs.items():
    #     if n in nodes and "Decl" in nodes[n].kind:
    #         nodes[n].setLabel(loc.replace("<", "_").replace(">", "_"))

    # print([n.toDotNode() for n in nodes.values()])

    # print([n.toDotEdges() for n in nodes.values()])

    print("digraph G {")
    print("node [shape=record];")
    print("compound = true;")

    for n in nodes.values():
        print(n.toDotNode(), ";")
    for n in nodes.values():
        s = n.toDotEdges()
        if len(s) != 0:
            print(s, ";")

    print("}")


if __name__ == "__main__":
    main()
