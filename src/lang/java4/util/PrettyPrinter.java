/* Copyright (c) 2013-2015, Jesper Öqvist <jesper.oqvist@cs.lth.se>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.jastadd.util;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Stack;

/**
 * @author Jesper Öqvist <jesper.oqvist@cs.lth.se>
 */
public class PrettyPrinter {
  public interface Joiner {
    /** Prints a separator to join two PrettyPrintable objects. */
    void printSeparator(PrettyPrinter out);
  }

  private final String indentation;
  private final java.util.List<String> ind = new ArrayList<String>(32);
  private final Stack<Integer> indentStack = new Stack<Integer>();
  private int currentIndent = 0;

  private final PrintStream out;
  private boolean newline = true;

  {
    // Initialize indentation cache.
    ind.add("");
    indentStack.push(0);
  }

  /**
   * Creates a new pretty printer with System.out as target.
   * @param ind
   */
  public PrettyPrinter(String ind) {
    this.indentation = ind;
    this.out = System.out;
  }

  /**
   * Creates a new pretty printer with the given target stream.
   * @param ind
   * @param target
   */
  public PrettyPrinter(String ind, PrintStream target) {
    this.indentation = ind;
    this.out = target;
  }

  /**
   * @param level The level of indentation
   * @return The indentation string for the given indentation level
   */
  private final String getIndentation(int level) {
    while (ind.size() < (level + 1)) {
      ind.add(ind.get(ind.size() - 1) + indentation);
    }
    return ind.get(level);
  }


  /**
   * @param str
   */
  public void print(String str) {
    indentNewline();
    out.print(str);
  }

  /**
   *
   */
  public void println() {
    out.println();
    newline = true;
  }

  /**
   * @param node
   */
  public final void print(PrettyPrintable node) {
    pushIndentation();
    node.prettyPrint(this);
    popIndentation();
  }

  /**
   * Increase the indentation of the next printed object by the given number of
   * indentation levels. Only takes effect after calling pushIndentation().
   * @param level
   */
  public void indent(int level) {
    currentIndent = level;
  }

  /**
   * Store the current indentation level while printing something.
   */
  public void pushIndentation() {
    indentStack.push(currentIndent + indentStack.peek());
    currentIndent = 0;
  }

  /**
   * Restore the previous indentation level.
   */
  public void popIndentation() {
    currentIndent = indentStack.pop();
    currentIndent -= indentStack.peek();
  }

  protected void indentNewline() {
    if (newline) {
      out.print(getIndentation(indentStack.peek()));
      newline = false;
    }
  }

  public boolean isNewLine() {
    return newline;
  }

  /** Concatenate a list of pretty-printable items, without a separator. */
  public final void join(Iterable<? extends PrettyPrintable> list) {
    for (PrettyPrintable item : list) {
      print(item);
    }
  }

  /** Concatenate a list of pretty-printable items with a separator. */
  public final void join(Iterable<? extends PrettyPrintable> list, Joiner joiner) {
    boolean first = true;
    for (PrettyPrintable item : list) {
      if (!first) {
        joiner.printSeparator(this);
      }
      first = false;
      print(item);
    }
  }
}
