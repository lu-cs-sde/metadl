#!/usr/bin/python3

import csv
import sys

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
            s += "n" + str(self.nid) + " -> n" + str(n) + ' [color="{}", label="{}"]' \
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
    if len(sys.argv) != 2:
        print("Run as: prel2dot.py PROGRAM.csv")
        exit(1)

    csv_file_name = sys.argv[1]
    csvf = open(csv_file_name)

    csv_reader = csv.reader(csvf, delimiter=',')

    nodes = dict()
    attrs = []
    srcLocs = dict()

    for row in csv_reader:
        if row[0] != "SrcLocStart" and row[0] != "SrcLocEnd" \
           and row[0] != "Terminal" and not row[0].startswith("ATTR_") \
                         and row[0] != "REWRITE":
            if row[1] in nodes:
                n = nodes[row[1]]
            else:
                n = Node(row[0], int(row[1]))
                nodes[row[1]] = n
            if int(row[2]) >= 0:
                n.addChild(int(row[2]), int(row[3]))
        elif row[0] == "REWRITE" or row[0].startswith("ATTR_"):
            attrs.append((row[1], row[3], row[0]))
        elif row[0] == "SrcLocStart":
            srcLocs[row[1]] = row[4] + ":" + row[2] + "," + row[3];

        if row[0] == "Terminal":
            nodes[row[1]] = Terminal(row[1], row[4])


    for src, tgt, attr in attrs:
        if src in nodes:
            nodes[src].addAttr(attr, tgt)

    for n, loc in srcLocs.items():
        if n in nodes and "Decl" in nodes[n].kind:
            nodes[n].setLabel(loc.replace("<", "_").replace(">", "_"))

    # print([n.toDotNode() for n in nodes.values()])

    # print([n.toDotEdges() for n in nodes.values()])

    print("digraph G {")
    print("node [shape=record];")

    for n in nodes.values():
        print(n.toDotNode(), ";")
    for n in nodes.values():
        s = n.toDotEdges()
        if len(s) != 0:
            print(s, ";")

    print("}")


if __name__ == "__main__":
    main()
